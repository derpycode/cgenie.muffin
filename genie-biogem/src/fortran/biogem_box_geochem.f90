! **********************************************************************************************************************************
! biogem_box_geochem.f90
! C-GOLDSTEIn/BioGeM
! Functions for redox and particle-dissolved transformations that are called directly from the main biogem function
! **********************************************************************************************************************************


MODULE biogem_box_geochem


  use gem_carbchem
  use gem_geochem
  USE biogem_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! NITROGEN
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE THE OXIDATION OF NH4
  SUBROUTINE sub_box_oxidize_NH4toNO3(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_O2,loc_NH4,loc_r15N
    real::loc_potO2cap
    real::loc_NH4_oxidation
    real,dimension(n_ocn,n_k)::loc_bio_remin
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! OXIDIZE NH4
    ! -------------------------------------------------------- !
    ! look for some NH4 and see if it can be oxidized (using O2; if there is any!)
    ! NH4+ + 2O2 -> NO3- + 2H+ + H2O
    DO k=n_k,dum_k1,-1
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_NH4 = ocn(io_NH4,dum_i,dum_j,k)
       if ((loc_O2 > const_real_nullsmall) .AND. (loc_NH4 > const_real_nullsmall)) then
          ! calculate potential NH4 oxidation
          SELECT CASE (opt_bio_remin_oxidize_NH4toNO3)
          CASE ('Fennel')
             ! from: Fennel et al. [2005]
             ! oxidation rate constant: 6 yr-1 (NOTE: corrected from 0.1666 in original model)
             ! oxidation half saturation for oxygen: 2.0E-05 mol kg-1
             loc_NH4_oxidation = dum_dtyr*6.0*loc_NH4*(loc_O2/(2.0E-05 + loc_O2))
          CASE ('FennelOLD')
             ! from: Fennel et al. [2005]
             loc_NH4_oxidation = dum_dtyr*0.16667*loc_NH4*(loc_O2/(2.0E-05 + loc_O2))
          CASE ('Ozaki')
             ! from: Ozaki et al. [EPSL ... ?]
             loc_NH4_oxidation = dum_dtyr*(18250.0/conv_m3_kg)*loc_NH4*loc_O2
          CASE ('Fanny')
	     ! Second order equation of enzyme kinetics which accounts for both O2 and NH4 limitations on nitrification
             loc_potO2cap = ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k)
             loc_NH4_oxidation = dum_dtyr*par_nitri_mu*loc_NH4*loc_potO2cap &
                  & /(par_nitri_c0_NH4*par_nitri_c0_O2 +par_nitri_c0_O2*loc_NH4 &
                  & +par_nitri_c0_NH4*loc_potO2cap +loc_NH4*loc_potO2cap) &
                  & *min(loc_NH4,loc_potO2cap*par_bio_red_POP_PON/(-par_bio_red_POP_PO2))
             If (loc_NH4_oxidation > min(loc_NH4,loc_potO2cap*par_bio_red_POP_PON/(-par_bio_red_POP_PO2))) then
                loc_NH4_oxidation = min(loc_NH4,loc_potO2cap*loc_potO2cap*par_bio_red_POP_PON/(-par_bio_red_POP_PO2))
             end if
          CASE ('NONE')
             loc_NH4_oxidation = 0.0
          case default
             loc_NH4_oxidation = min(0.5*loc_NH4,loc_O2)
          end select
          ! calculate isotopic ratio
          loc_r15N = ocn(io_NH4_15N,dum_i,dum_j,k)/ocn(io_NH4,dum_i,dum_j,k)
          if (loc_NH4_oxidation > loc_NH4) then
             ! complete NH4 oxidation (no N fractionation)
             loc_bio_remin(io_NH4,k) = -loc_NH4
             loc_bio_remin(io_NO3,k) = loc_NH4
             loc_bio_remin(io_O2,k)  = -2.0*loc_NH4
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k)
             loc_bio_remin(io_NH4_15N,k) = -loc_r15N*loc_NH4
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_NH4
          else
             ! partial NH4 oxidation (=> N isotope Rayleigh fractionation)
             loc_bio_remin(io_NH4,k) = -loc_NH4_oxidation
             loc_bio_remin(io_NO3,k) = loc_NH4_oxidation
             loc_bio_remin(io_O2,k)  = -2.0*loc_NH4_oxidation
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO N FRACTIONATION ########################################################## !
             loc_bio_remin(io_NH4_15N,k) = -loc_r15N*loc_NH4_oxidation
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_NH4_oxidation
             ! ################################################################################################################### !
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1) OLD
    diag_geochem_old(idiag_geochem_old_ammox_dNH4,dum_i,dum_j,:) = loc_bio_remin(io_NH4,:)
    diag_geochem_old(idiag_geochem_old_ammox_dNO3,dum_i,dum_j,:) = loc_bio_remin(io_NO3,:)
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_NH4toNO3_dNH4',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_NH4,:)
    id = fun_find_str_i('redox_NH4toNO3_dNO3',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_NO3,:)
    id = fun_find_str_i('redox_NH4toNO3_dO2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_O2,:)
    id = fun_find_str_i('redox_NH4toNO3_dALK',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_ALK,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_oxidize_NH4toNO3
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE THE OXIDATION OF NH4
  SUBROUTINE sub_box_oxidize_NH4toNO2(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k
    real::loc_O2,loc_NH4,loc_r15N
    real::loc_NH4_oxidation,loc_N2Ofrac
    real,dimension(n_ocn,n_k)::loc_bio_remin
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! OXIDIZE NH4
    ! -------------------------------------------------------- !
    ! look for some NH4 and see if it can be oxidized (using O2; if there is any!)
    ! 2NH4+ + 2O2 -> N2O + 2H+ + 3H2O
    ! (2NH4+ + 3O2 -> 2NO2- + 4H+ + 2H2O)
    DO k=n_k,dum_k1,-1
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_NH4 = ocn(io_NH4,dum_i,dum_j,k)
       if ((loc_O2 > const_real_nullsmall) .AND. (loc_NH4 > const_real_nullsmall)) then
          ! calculate potential NH4 oxidation
          loc_NH4_oxidation = dum_dtyr*par_bio_remin_kNH4toNO2*min(loc_NH4,loc_O2)* &
               & (loc_NH4/(loc_NH4 + par_bio_remin_cNH4_NH4toNO2))*(loc_O2/(loc_O2 + par_bio_remin_cO2_NH4toNO2))
          ! calculate isotopic ratio
          loc_r15N = ocn(io_NH4_15N,dum_i,dum_j,k)/ocn(io_NH4,dum_i,dum_j,k)
          ! calculate fraction to be transformed into N2O (if selected) rather than NO2
          if (ocn_select(io_N2O)) then
             loc_N2Ofrac = par_bio_remin_fracN2O
          else
             loc_N2Ofrac = 0.0
          end if
          if (loc_NH4_oxidation > loc_NH4) then
             ! complete NH4 oxidation (no N fractionation)
             loc_bio_remin(io_NH4,k) = -(1.0 - loc_N2Ofrac)*loc_NH4
             loc_bio_remin(io_NO2,k) = -loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_O2,k)  = (3.0/2.0)*loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO2,k)
             loc_bio_remin(io_NH4_15N,k) = loc_r15N*loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_NO2_15N,k) = loc_r15N*loc_bio_remin(io_NO2,k)
             if (ocn_select(io_N2O)) then
                loc_bio_remin(io_NH4,k) = loc_bio_remin(io_NH4,k) - loc_N2Ofrac*loc_NH4
                loc_bio_remin(io_N2O,k) = loc_bio_remin(io_NO2,k) + 0.5*loc_N2Ofrac*loc_NH4
                loc_bio_remin(io_O2,k)  = loc_bio_remin(io_O2,k) - loc_N2Ofrac*loc_NH4
                loc_bio_remin(io_ALK,k) = loc_bio_remin(io_ALK,k) - loc_N2Ofrac*loc_NH4
                loc_bio_remin(io_NH4_15N,k) = loc_bio_remin(io_NH4_15N,k) - loc_r15N*loc_N2Ofrac*loc_NH4
                loc_bio_remin(io_N2O_15N,k) = loc_bio_remin(io_N2O_15N,k) + loc_r15N*0.5*loc_N2Ofrac*loc_NH4
             end if
          else
             ! partial NH4 oxidation (=> N isotope Rayleigh fractionation)
             loc_bio_remin(io_NH4,k) = -(1.0 - loc_N2Ofrac)*loc_NH4_oxidation
             loc_bio_remin(io_NO2,k) = -loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_O2,k)  = (3.0/2.0)*loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO2,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO N FRACTIONATION ########################################################## !
             loc_bio_remin(io_NH4_15N,k) = loc_r15N*loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_NO2_15N,k) = loc_r15N*loc_bio_remin(io_NO2,k)
             ! ################################################################################################################### !
             if (ocn_select(io_N2O)) then
                loc_bio_remin(io_NH4,k) = loc_bio_remin(io_NH4,k) - loc_N2Ofrac*loc_NH4_oxidation
                loc_bio_remin(io_N2O,k) = loc_bio_remin(io_N2O,k) + 0.5*loc_N2Ofrac*loc_NH4_oxidation
                loc_bio_remin(io_O2,k)  = loc_bio_remin(io_O2,k) - loc_N2Ofrac*loc_NH4_oxidation
                loc_bio_remin(io_ALK,k) = loc_bio_remin(io_ALK,k) - loc_N2Ofrac*loc_NH4_oxidation
                ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO N FRACTIONATION ####################################################### !
                loc_bio_remin(io_NH4_15N,k) = loc_bio_remin(io_NH4_15N,k) - loc_r15N*loc_N2Ofrac*loc_NH4_oxidation
                loc_bio_remin(io_N2O_15N,k) = loc_bio_remin(io_N2O_15N,k) + loc_r15N*0.5*loc_N2Ofrac*loc_NH4_oxidation
                ! ################################################################################################################ !
             end if
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_oxidize_NH4toNO2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE THE OXIDATION OF NO2
  SUBROUTINE sub_box_oxidize_NO2(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k
    real::loc_O2,loc_NO2,loc_r15N
    real::loc_NO2_oxidation
    real,dimension(n_ocn,n_k)::loc_bio_remin
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! OXIDIZE NO2
    ! -------------------------------------------------------- !
    ! oxic conditions: 2NO2- + O2 -> 2N03-
    DO k=n_k,dum_k1,-1
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_NO2 = ocn(io_NO2,dum_i,dum_j,k)
       if ((loc_O2 > const_real_nullsmall) .AND. (loc_NO2 > const_real_nullsmall)) then
          ! calculate potential NH4 oxidation
          loc_NO2_oxidation = dum_dtyr*par_bio_remin_kNO2toNO3*min(loc_NO2,loc_O2)* &
               & (loc_NO2/(loc_NO2 + par_bio_remin_cNO2_NO2toNO3))*(loc_O2/(loc_O2 + par_bio_remin_cO2_NO2toNO3))
          ! calculate isotopic ratio
          loc_r15N = ocn(io_NO2_15N,dum_i,dum_j,k)/ocn(io_NO2,dum_i,dum_j,k)
          if (loc_NO2_oxidation > loc_NO2) then
             ! complete NO2 oxidation (no N fractionation)
             loc_bio_remin(io_NO2,k) = -loc_NO2
             loc_bio_remin(io_NO3,k) = loc_NO2
             loc_bio_remin(io_O2,k)  = -0.5*loc_NO2
             loc_bio_remin(io_ALK,k) = -loc_bio_remin(io_NO3,k)
             loc_bio_remin(io_NO2_15N,k) = -loc_r15N*loc_NO2
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_NO2
          else
             ! partial NO2 oxidation (=> N isotope Rayleigh fractionation)
             loc_bio_remin(io_NO2,k) = -loc_NO2_oxidation
             loc_bio_remin(io_NO3,k) = loc_NO2_oxidation
             loc_bio_remin(io_O2,k)  = -0.5*loc_NO2_oxidation
             loc_bio_remin(io_ALK,k) = -loc_bio_remin(io_NO3,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO N FRACTIONATION ########################################################## !
             loc_bio_remin(io_NO2_15N,k) = -loc_r15N*loc_NO2_oxidation
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_NO2_oxidation
             ! ################################################################################################################### !
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_oxidize_NO2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE THE REDUCTION OF NO2
  SUBROUTINE sub_box_reduce_NO2(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k
    real::loc_O2,loc_NO2,loc_r15N
    real::loc_NO2_reduction
    real,dimension(n_ocn,n_k)::loc_bio_remin
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! REDUCE NO2
    ! -------------------------------------------------------- !
    ! anoxic conditions: 2NO2- + 2H+ -> N2O + O2 + H2O
    DO k=n_k,dum_k1,-1
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_NO2 = ocn(io_NO2,dum_i,dum_j,k)
       if ((loc_O2 > const_real_nullsmall) .AND. (loc_NO2 > const_real_nullsmall)) then
          ! calculate potential NO2 reduction
          loc_NO2_reduction = dum_dtyr*par_bio_remin_kNO2toN2O*loc_NO2* &
               & (loc_NO2/(loc_NO2 + par_bio_remin_cNO2_NO2toN2O))*(1.0 - loc_O2/(loc_O2 + par_bio_remin_cO2_NO2toN2O))
          ! calculate isotopic ratio
          loc_r15N = ocn(io_NO2_15N,dum_i,dum_j,k)/ocn(io_NO2,dum_i,dum_j,k)
          if (loc_NO2_reduction > loc_NO2) then
             ! complete NO2 reduction (no N fractionation)
             loc_bio_remin(io_NO2,k) = -loc_NO2
             loc_bio_remin(io_N2O,k) = 0.5*loc_NO2
             loc_bio_remin(io_O2,k)  = 0.5*loc_NO2
             loc_bio_remin(io_ALK,k) = -loc_bio_remin(io_NO2,k)
             loc_bio_remin(io_NO2_15N,k) = -loc_r15N*loc_NO2
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_NO2
          else
             ! partial NO2 reduction (=> N isotope Rayleigh fractionation)
             loc_bio_remin(io_NO2,k) = -loc_NO2_reduction
             loc_bio_remin(io_N2O,k) = 0.5*loc_NO2_reduction
             loc_bio_remin(io_O2,k)  = 0.5*loc_NO2_reduction
             loc_bio_remin(io_ALK,k) = -loc_bio_remin(io_NO2,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO N FRACTIONATION ########################################################## !
             loc_bio_remin(io_NO2_15N,k) = -loc_r15N*loc_NO2_reduction
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_NO2_reduction
             ! ################################################################################################################### !
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_reduce_NO2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! IRON
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! OXIDATION OF reduced dissolved Fe2
  SUBROUTINE sub_box_oxidize_Fe2(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_O2,loc_Fe2,loc_r56Fe, loc_R_56Fe
    real::loc_Fe2_oxidation
    real,dimension(n_ocn,n_k)::loc_bio_remin
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! OXIDIZE Fe2
    ! -------------------------------------------------------- !
    ! look for some Fe2 and see if it can be instantaneously oxidized (using O2; if there is any!)
    ! Fe2 + 1/4*O2 + H -> Fe3 + 1/2H2O 
    ! NOTE: loc_Fe2_oxidation_const units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       loc_O2  = ocn(io_O2,dum_i,dum_j,k)
       loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
       if ( loc_O2>const_rns .AND. loc_Fe2>const_rns ) then
          ! calculate Fe2 oxidation
          ! NOTE: par_bio_remin_kFe2toFe units are (M-1 yr-1) (default from Millero et al., 1987)
          ! NOTE: the dependence on OH has been ignored because, well, Fe oxidizes 
          !       so fast it does not really matter anymore 
          loc_Fe2_oxidation = dum_dtyr*par_bio_remin_kFe2toFe*loc_Fe2*loc_O2
          ! cap at some fraction of maximum of available Fe, O2
          loc_Fe2_oxidation = min(loc_Fe2_oxidation,loc_f*loc_Fe2,loc_f*(4.0/1.0)*loc_O2)
          ! bulk tracer conversion
          loc_bio_remin(io_Fe2,k) = -loc_Fe2_oxidation
          loc_bio_remin(io_Fe,k)  = loc_Fe2_oxidation
          loc_bio_remin(io_O2,k)  = -1.0/4.0*loc_Fe2_oxidation
          ! isotopic fractionation
          ! NOTE: we already know that loc_Fe2 is non-zero
          if (ocn_select(io_Fe2_56Fe) .AND. ocn_select(io_Fe_56Fe)) then
             loc_r56Fe = ocn(io_Fe2_56Fe,dum_i,dum_j,k)/loc_Fe2
             loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)
             loc_bio_remin(io_Fe2_56Fe,k)  &
                  & = -par_d56Fe_Fe2ox_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fe2ox_alpha*loc_R_56Fe)*loc_Fe2_oxidation
             loc_bio_remin(io_Fe_56Fe,k)  &
                  & = par_d56Fe_Fe2ox_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fe2ox_alpha*loc_R_56Fe)*loc_Fe2_oxidation
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_Fe2toFe3_dFe2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_Fe2,:)
    id = fun_find_str_i('redox_Fe2toFe3_dFe',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_Fe,:)
    id = fun_find_str_i('redox_Fe2toFe3_dO2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_O2,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_oxidize_Fe2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Reduction of oxidised dissolved Fe3
  SUBROUTINE sub_box_reduce_Fe(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_H2S,loc_Fe,loc_r56Fe,loc_R_56Fe,loc_r34S,loc_R_34S
    real::loc_Fe_reduction,loc_H2S_oxidation
    real,dimension(n_ocn,n_k)::loc_bio_remin
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! REDUCE Fe3 (implicitly, FeOOH)
    ! -------------------------------------------------------- !
    ! Fe3 + 1/8*H2S + 1/2H2O -> Fe2 + 1/8*SO4 + 5/4*H+
    ! NOTE: loc_Fe_reduction_const units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       loc_Fe  = ocn(io_Fe,dum_i,dum_j,k)
       loc_H2S = ocn(io_H2S,dum_i,dum_j,k)
       ! look for some Fe3 and H2S
       if ( loc_H2S>const_rns .AND. loc_Fe>const_rns ) then
          ! calculate H2S oxidation
          ! NOTE: par_bio_remin_kH2StoSO4 units are (M-1 yr-1)
          ! NOTE: Reaction is taken from Poulton et al. (2004) - kinetic constant is assumed to be '2-line ferrihydrite'
          loc_Fe_reduction = dum_dtyr*par_bio_remin_kFetoFe2*loc_Fe*loc_H2S**(1.0/2.0)
          ! cap according to available Fe, H2S
          loc_Fe_reduction  = min(loc_Fe_reduction,loc_f*loc_Fe,loc_f*(8.0/1.0)*loc_H2S)
          loc_H2S_oxidation = (1.0/8.0)*loc_Fe_reduction
          ! bulk tracer conversion
          loc_bio_remin(io_Fe,k)  = -loc_Fe_reduction
          loc_bio_remin(io_Fe2,k) = loc_Fe_reduction
          loc_bio_remin(io_H2S,k) = -loc_H2S_oxidation
          loc_bio_remin(io_SO4,k) = loc_H2S_oxidation
          loc_bio_remin(io_ALK,k) = -2.0*loc_H2S_oxidation
          ! calculate isotopic fractionation -- 56Fe
          ! NOTE: we already know that loc_Fe2 is non-zero
          if (ocn_select(io_Fe2_56Fe) .AND. ocn_select(io_Fe_56Fe)) then
             loc_r56Fe = ocn(io_Fe_56Fe,dum_i,dum_j,k)/loc_Fe
             loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)
             loc_bio_remin(io_Fe_56Fe,k)  = &
                  & -par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_Fe_reduction
             loc_bio_remin(io_Fe2_56Fe,k) = &
                  & par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_Fe_reduction
          end if
          ! calculate isotopic fractionation -- 34S
          ! NOTE: we already know that loc_H2S is non-zero
          if (ocn_select(io_H2S_34S) .AND. ocn_select(io_SO4_34S)) then
             loc_r34S = ocn(io_H2S_34S,dum_i,dum_j,k)/loc_H2S
             loc_R_34S = loc_r34S/(1.0 - loc_r34S) 
             loc_bio_remin(io_H2S_34S,k) = &
                  & -par_d34S_Fered_alpha*loc_R_34S/(1.0 + par_d34S_Fered_alpha*loc_R_34S)*loc_H2S_oxidation
             loc_bio_remin(io_SO4_34S,k) = &
                  & par_d34S_Fered_alpha*loc_R_34S/(1.0 + par_d34S_Fered_alpha*loc_R_34S)*loc_H2S_oxidation
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_Fe3toFe2_dFe',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_Fe,:)
    id = fun_find_str_i('redox_Fe3toFe2_dFe2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_Fe2,:)
    id = fun_find_str_i('redox_Fe3toFe2_dH2S',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_H2S,:)
    id = fun_find_str_i('redox_Fe3toFe2_dSO4',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_SO4,:) 
    id = fun_find_str_i('redox_Fe3toFe2_dALK',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_ALK,:) 
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_reduce_Fe
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ABIOTIC FeOOH PRECIPITATION
  SUBROUTINE sub_calc_precip_FeOOH(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real,dimension(n_sed,n_k)::loc_bio_part
    real::loc_Fe
    real::loc_r56Fe
    real::loc_R_56Fe
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_uptake(io,:) = 0.0
    end do
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       loc_bio_part(is,:) = 0.0
    end DO
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! CALCULATE FeOOH precipitation
    ! -------------------------------------------------------- !
    ! water column loop
    ! look for existing Fe3 see if it can be instantaneously precipitated
    ! Fe3+ + 2*H2O -> FeOOH + 3*H+
    ! NOTE: Simple scheme: Fe3 > 1 nM precipitates - probably needs to be further developed
    DO k=n_k,dum_k1,-1
       ! set local concentrations
       loc_Fe = ocn(io_Fe,dum_i,dum_j,k)
       ! calculate FeOOH precipitation
       if (ctrl_bio_FeOOHprecip_explicit) then
          if (loc_Fe > par_FeOOH_Fethresh) then
             ! calculate FeOOH precip
             loc_bio_part(is_FeOOH,k) = loc_Fe - par_FeOOH_Fethresh
             ! cap according to available Fe, H2S
             loc_bio_part(is_FeOOH,k)  = min(loc_bio_part(is_FeOOH,k),loc_f*(loc_Fe - par_FeOOH_Fethresh))
             ! calculate Fe fractionation
             if (ocn_select(io_Fe_56Fe)) then
                loc_r56Fe  = ocn(io_Fe_56Fe,dum_i,dum_j,k)/ocn(io_Fe,dum_i,dum_j,k)
                loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)
                loc_bio_part(is_FeOOH_56Fe,k) = &
                     & par_d56Fe_FeOOH_alpha*loc_R_56Fe/(1.0 + par_d56Fe_FeOOH_alpha*loc_R_56Fe)*loc_bio_part(is_FeOOH,k)
            end if
          end if
       end if
       ! convert particulate sediment tracer indexed array concentrations to (dissolved) tracer indexed array
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_tot_i = conv_sed_ocn_i(0,is)
          do loc_i=1,loc_tot_i
             io = conv_sed_ocn_i(loc_i,is)
             loc_bio_uptake(io,k) = loc_bio_uptake(io,k) + conv_sed_ocn(io,is)*loc_bio_part(is,k)
          end do
       end DO
    end DO
    ! -------------------------------------------------------- !
    ! SET GLOBAL ARRAYS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! TRACER CONCENTRATIONS
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) - loc_bio_uptake(io,:)
    end do
    ! -------------------------------------------------------- ! PARTICULATE CONCENTRATIONS
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       bio_part(is,dum_i,dum_j,:) = bio_part(is,dum_i,dum_j,:) + loc_bio_part(is,:)
    end DO
    ! -------------------------------------------------------- ! MODIFY DET TRACER FLUX
    bio_part(is_det,dum_i,dum_j,:) = bio_part(is_det,dum_i,dum_j,:) + loc_bio_part(is_FeOOH,:)
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record geochem diagnostics (mol kg-1)
    diag_precip(idiag_precip_FeOOH_dFe,dum_i,dum_j,:) = -loc_bio_uptake(io_Fe,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_precip_FeOOH
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE AQUATIC IRONSULPHIDE FORMATION
  SUBROUTINE sub_calc_form_FeS(dum_i,dum_j,dum_k1)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k,l,io
    real,dimension(n_ocn,n_k)::loc_bio_remin
    real::loc_Fe2,loc_H2S,loc_FeS
    real::loc_r56Fe, loc_r34S
    real::loc_FeS_formation
    real,DIMENSION(2)::loc_roots
    real::loc_min,loc_max
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! CALCULATE FeS formation (depending on relative concentrations
    ! -------------------------------------------------------- !
    ! Fe2+ + H2S -> FeS + 2H+
    ! water column loop
    ! look for co-existing Fe and H2S and see if they are in equilibrium with dissolved FeS
    ! NOTE: par_bio_FeS2precip_k units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
       loc_H2S = ocn(io_H2S,dum_i,dum_j,k)
       loc_FeS = ocn(io_FeS,dum_i,dum_j,k)
       ! either Fe2 and H2S, or FeS have to be present
       if (((loc_Fe2 > 1.0e-11) .AND. (loc_H2S > 1.0e-11)) .OR. (loc_FeS > 1.0e-11)) then
          ! Avoid calculating with negative concentrations
          if (loc_FeS < const_rns) then
             loc_FeS = 0.0
          end if
          if (loc_Fe2 < const_rns) then
             loc_Fe2 = 0.0
          end if
          if (loc_H2S < const_rns) then 
             loc_H2S = 0.0
          end if
          ! Since FeS, we need to calculate the Fe-S speciation
          ! calculate solution for equilibrium:
          ! Fe2_eq*H2S_eq/FeS_eq = K
          ! Fe2_eq = Fe2_in - x; H2S_eq = H2S_in - x; FeS_eq = FeS_in + x
          ! (Fe2_in - x)(H2S_in - x) = K*FeS_in + K*x
          ! Fe_in*H2S_in - H2S_in*x - Fe_in*x + x^2 = K*FeS_in + K*x
          ! x^2 - (H2S_in + Fe_in + K) + (Fe_in*H2S_in - K*FeS_in) = 0
          ! solve for positive roots  
          loc_roots(:) = fun_quad_root( &
               & 1.0,-(loc_H2S + loc_Fe2 + par_bio_FeS_abioticohm_cte),(loc_Fe2*loc_H2S - par_bio_FeS_abioticohm_cte*loc_FeS) &
               & )
          loc_min = minval(loc_roots(:))
          loc_max = maxval(loc_roots(:))
          if ((loc_H2S - loc_max> const_rns) .AND. (loc_Fe2 - loc_max> const_rns).AND. (loc_FeS + loc_max> const_rns)) then
             loc_FeS_formation = loc_max
          elseif ((loc_H2S - loc_min> const_rns) .AND. (loc_Fe2 - loc_min> const_rns).AND. (loc_FeS + loc_min> const_rns)) then
             loc_FeS_formation = loc_min
          else
             loc_FeS_formation = 0.0
          end if
          loc_bio_remin(io_FeS,k) = loc_FeS_formation
          loc_bio_remin(io_Fe2,k) = -loc_FeS_formation
          loc_bio_remin(io_H2S,k) = -loc_FeS_formation
          ! deal with Fe and S isotopes? -> no fractionation of S during pyrite formation
          ! calculate isotopic ratio
          loc_r56Fe = ocn(io_Fe2_56Fe,dum_i,dum_j,k)/ocn(io_Fe2,dum_i,dum_j,k)
          loc_bio_remin(io_FeS_56Fe,k) = loc_r56Fe*loc_bio_remin(io_FeS,k)
          loc_bio_remin(io_Fe2_56Fe,k) = -loc_r56Fe*loc_bio_remin(io_FeS,k)
          loc_r34S = ocn(io_H2S_34S,dum_i,dum_j,k)/ocn(io_H2S,dum_i,dum_j,k)
          loc_bio_remin(io_FeS_34S,k) = loc_r34S*loc_bio_remin(io_FeS,k)
          loc_bio_remin(io_H2S_34S,k) = -loc_r34S*loc_bio_remin(io_FeS,k)   
       end if
       ! -------------------------------------------------------- !
       ! WRITE GLOBAL ARRAY DATA
       ! -------------------------------------------------------- !
       ! write ocean tracer remineralization field (global array)
       DO l=3,n_l_ocn
          io = conv_iselected_io(l)
          bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
       end do
    end do
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_form_FeS
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ABIOTIC PYRITE PRECIPITATION
  ! NOTE: const_rns == const_real_nullsmall
  SUBROUTINE sub_calc_precip_FeS2(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real,dimension(n_sed,n_k)::loc_bio_part
    real,dimension(1:3)::loc_Fe2spec
    real::loc_H2S,loc_SO4,loc_FeS,loc_FeSp,loc_Fe2
    real::loc_r56Fe,loc_r34S,loc_r34SO4
    real::loc_R_56Fe
    real::loc_FeS2_precipitation
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    loc_FeS2_precipitation = 0.0
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_uptake(io,:) = 0.0
    end do
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       loc_bio_part(is,:) = 0.0
    end DO
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! CALCULATE PYRITE PRECIPITATION
    ! -------------------------------------------------------- !
    ! water column loop
    ! look for co-existing Fe and H2S. Pyrite precipitates from FeS, so we first calculate how much of Fe2+ is in complexed form of
    ! FeS. unless FeS is calculated explicit (see above)
    ! FeS + 3/4H2S + 1/4SO4 + 1/2H -> FeS2 + H2O (where: Fe2+ + H2S -> FeS + 2H, calculated first)
    ! => NET: Fe2+ + 7/4H2S + 1/4SO4 -> FeS2
    ! NOTE: par_bio_FeS2precip_k units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       ! set local concentrations
       ! NOTE: Fe2 (not Fe(3))
       ! NOTE: cap diagnosed [loc_FeS] at [loc_Fe2]
       ! NOTE: FeS2 precipitates from FeS -> first calculate speciation of Fe and H2S
       if (ctrl_bio_FeS2precip_explicit) then
          loc_FeS = ocn(io_FeS,dum_i,dum_j,k)
          loc_H2S = ocn(io_H2S,dum_i,dum_j,k)
          loc_SO4 = ocn(io_SO4,dum_i,dum_j,k)
          loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
       else 
          loc_Fe2spec = fun_box_calc_spec_Fe2(ocn(io_Fe2,dum_i,dum_j,k),ocn(io_H2S,dum_i,dum_j,k),par_bio_FeS_abioticohm_cte)
          loc_FeS = loc_Fe2spec(3)
          loc_H2S = loc_Fe2spec(2)
          loc_SO4 = ocn(io_SO4,dum_i,dum_j,k)
          loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
          if (loc_FeS > loc_Fe2) then
             loc_FeS = loc_Fe2
          end if
       end if
       ! check for H2S and SO4 being greater than zero, 
       ! and loc_FeS greater than a critical threshold (par_bio_FeS_part_abioticohm_cte)
       if ( loc_FeS>par_bio_FeS_part_abioticohm_cte .AND. loc_H2S>const_rns .AND. loc_SO4>const_rns ) then
          ! calculate amount of FeS that could precipitate (as nanoparticulate precursor for pyrite)
          ! (loc_FeSp is the excess Fe available to precipitate as FeS2)
          ! Reaction and default kinetic constants are taken from Rickard (1997)
          loc_FeSp = loc_FeS - par_bio_FeS_part_abioticohm_cte
          loc_FeS2_precipitation = dum_dtyr*par_bio_FeS2precip_k*loc_FeSp*loc_H2S
          ! cap at maximum of available H2S, SO4, FeS
          loc_FeS2_precipitation = MIN(loc_FeS2_precipitation,loc_f*loc_FeSp,loc_f*(4.0/7.0)*loc_H2S,loc_f*(4.0/1.0)*loc_SO4)
          ! bulk tracer conversion
          loc_bio_part(is_FeS2,k) = loc_FeS2_precipitation
          ! calculate isotopic fractionation -- 56Fe
          ! NOTE: we already know that loc_FeSp is non-zero
          ! NOTE: loc_Fe2 may differ from ocn(io_Fe2,dum_i,dum_j,k) and hence the isotopic ratio of the Fe reservoir
          !       needs to be calculated w.r.t. the latter
          if (ocn_select(io_Fe2_56Fe)) then
             loc_r56Fe = ocn(io_Fe2_56Fe,dum_i,dum_j,k)/ocn(io_Fe2,dum_i,dum_j,k)
             loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)
             loc_bio_part(is_FeS2_56Fe,k) = &
                 & (par_d56Fe_FeS2_alpha*loc_R_56Fe/(1.0 + par_d56Fe_FeS2_alpha*loc_R_56Fe))*loc_bio_part(is_FeS2,k)
          end if
          ! calculate isotopic fractionation -- 34S
          ! NOTE: we already know that loc_H2S is non-zero
          ! NOTE: no 34S fractionation assumed in the formation of pyrite (fractionation takes place between SO4 and H2S)
          ! NOTE: loc_H2S may differ from ocn(io_H2S,dum_i,dum_j,k) and hence the isotopic ratio of the Fe reservoir
          !       needs to be calculated w.r.t. the latter
          if (ocn_select(io_H2S_34S) .AND. ocn_select(io_SO4_34S)) then
             loc_r34S   = ocn(io_H2S_34S,dum_i,dum_j,k)/ocn(io_H2S,dum_i,dum_j,k)
             loc_r34SO4 = ocn(io_SO4_34S,dum_i,dum_j,k)/ocn(io_SO4,dum_i,dum_j,k)
             
             loc_bio_part(is_FeS2_34S,k) = (7.0/8.0*loc_r34S + 1.0/8.0*loc_r34SO4)*loc_bio_part(is_FeS2,k)
          end if
          ! convert particulate sediment tracer indexed array concentrations to (dissolved) tracer indexed array
          loc_bio_uptake(io_Fe2,k) = loc_bio_uptake(io_Fe2,k) + loc_bio_part(is_FeS2,k)
          loc_bio_uptake(io_Fe2_56Fe,k) = loc_bio_uptake(io_Fe2_56Fe,k) + loc_bio_part(is_FeS2_56Fe,k)
          
          loc_bio_uptake(io_H2S,k) = loc_bio_uptake(io_H2S,k) + 7.0/4.0*loc_bio_part(is_FeS2,k)
          loc_bio_uptake(io_H2S_34S,k) = loc_bio_uptake(io_H2S_34S,k) + (7.0/4.0*loc_r34S*loc_bio_part(is_FeS2,k))
          
          loc_bio_uptake(io_SO4,k) = loc_bio_uptake(io_SO4,k) + 1.0/4.0*loc_bio_part(is_FeS2,k)
          loc_bio_uptake(io_SO4_34S,k) = loc_bio_uptake(io_SO4_34S,k) + (1.0/4.0*loc_r34SO4*loc_bio_part(is_FeS2,k))
          
          loc_bio_uptake(io_ALK,k) = loc_bio_uptake(io_ALK,k) - 2.0/4.0*loc_bio_part(is_FeS2,k)
          
          !DO l=1,n_l_sed
          !   is = conv_iselected_is(l)
          !   loc_tot_i = conv_sed_ocn_i(0,is)
          !   do loc_i=1,loc_tot_i
          !      io = conv_sed_ocn_i(loc_i,is)
          !      loc_bio_uptake(io,k) = loc_bio_uptake(io,k) + conv_sed_ocn(io,is)*loc_bio_part(is,k)
          !   end do
          !end DO
       end if
    end DO
    ! -------------------------------------------------------- !
    ! SET GLOBAL ARRAYS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! TRACER CONCENTRATIONS
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) - loc_bio_uptake(io,:)
    end do
    ! -------------------------------------------------------- ! PARTICULATE CONCENTRATIONS
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       bio_part(is,dum_i,dum_j,:) = bio_part(is,dum_i,dum_j,:) + loc_bio_part(is,:)
    end DO
    ! -------------------------------------------------------- ! MODIFY DET TRACER FLUX
    bio_part(is_det,dum_i,dum_j,:) = bio_part(is_det,dum_i,dum_j,:) + loc_bio_part(is_FeS2,:)
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record geochem diagnostics (mol kg-1)
    diag_precip(idiag_precip_FeS2_dFe,dum_i,dum_j,:)  = loc_bio_uptake(io_Fe2,:)
    diag_precip(idiag_precip_FeS2_dH2S,dum_i,dum_j,:) = loc_bio_uptake(io_H2S,:)
    diag_precip(idiag_precip_FeS2_dSO4,dum_i,dum_j,:) = loc_bio_uptake(io_SO4,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_precip_FeS2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ABIOTIC FeCO3 precipitation
  SUBROUTINE sub_calc_precip_FeCO3(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real,dimension(n_sed,n_k)::loc_bio_part
    real,dimension(1:3)::loc_Fe2spec
    real::loc_IAP
    real::loc_delta_FeCO3,loc_CO3,loc_Fe2,loc_OH,loc_H2S,loc_FeCO3_precipitation
    real::loc_alpha
    real::loc_R,loc_r56Fe,loc_R_56Fe
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_uptake(io,:) = 0.0
    end do
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       loc_bio_part(is,:) = 0.0
    end DO
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! CALCULATE SIDERITE PRECIPITATION
    ! -------------------------------------------------------- !
    ! Fe2+ + CO32- -> FeCO3
    ! NOTE: in selecting is_FeCO3, full water column carbonate chemsitry re-calculation has been automatically selected
    !       (ctrl_carbchemupdate_full = .true.)
    DO k=n_k,dum_k1,-1
       ! set local species concentrations
       loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
       loc_CO3 = carb(ic_conc_CO3,dum_i,dum_j,n_k)
       loc_OH  = 10.0**(-(14.0-carb(ic_pHsws,dum_i,dum_j,n_k)))
       loc_H2S = ocn(io_H2S,dum_i,dum_j,k)    
       ! modify local Fe2 avialability
       ! NOTE: if explicit FeS2 precip is not selected, Fe2 speciation must be calculated because FeCO3 precipitates
       !       from free Fe2+, so we need to find the available free Fe2+ pool
       if (.NOT. ctrl_bio_FeS2precip_explicit) then
          if ( loc_Fe2>const_rns .AND. loc_H2S>const_rns ) then
             loc_Fe2spec = fun_box_calc_spec_Fe2(loc_Fe2,loc_H2S,par_bio_FeS_abioticohm_cte)
             loc_Fe2     = min(loc_Fe2,loc_Fe2spec(1))
          end if
       end if
       ! calculate precipitation
       ! NOTE: assume loc_CO3 is always greater than zero
       if (loc_Fe2 > const_rns) then
          ! calculate Ion Activity Product (IAP)
          ! NOTE: gamma parameters are activity coefficients
          loc_IAP = (par_bio_remin_gammaCO2*loc_CO3)*(par_bio_remin_gammaFe2*loc_Fe2)*((par_bio_remin_gammaOH*loc_OH)**(1.0/2.0))
          ! calculate siderite precipitation based on IAP. Siderite precipitates at very high supersaturation (very unlikely to 
          ! occur in the ocean). REFERENCE: Jiang and Tosca, 2019, Earth and Planetary Science Letters.
          if (loc_IAP > const_rns) then
             loc_FeCO3_precipitation = dum_dtyr*par_bio_FeCO3precip_sf*exp(par_bio_FeCO3precip_exp*LOG10(loc_IAP))
          else
             loc_FeCO3_precipitation = 0.0 
          end if
          ! cap FeCO3 rpecip at maximum of available Fe2+, CO3
          ! NOTE: always allow all CO3 to be 'used' in a single time-step becasue the carbonate system is buffered
          !       (CO3 is always likely to be far in excess of Fe2)
          loc_FeCO3_precipitation = MIN(loc_FeCO3_precipitation,loc_f*loc_Fe2,loc_CO3)
          ! bulk tracer conversion
          loc_bio_part(is_FeCO3,k) = loc_FeCO3_precipitation
          ! calculate isotopic fractionation -- 56Fe
          ! NOTE: we already know that loc_Fe2 is non-zero, and to have got this far, io_Fe2 must also be non-zero
          ! NOTE: loc_Fe2 may differ from ocn(io_Fe2,dum_i,dum_j,k) and hence the isotopic ratio of the Fe reservoir
          !       needs to be calculated w.r.t. the latter
          if (sed_select(is_FeCO3_56Fe)) then
             loc_r56Fe = ocn(io_Fe2_56Fe,dum_i,dum_j,k)/ocn(io_Fe2,dum_i,dum_j,k)
             loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)
             loc_bio_part(is_FeCO3_56Fe,k) = &
                  & par_d56Fe_FeCO3_alpha*loc_R_56Fe/(1.0 + par_d56Fe_FeCO3_alpha*loc_R_56Fe)*loc_bio_part(is_FeCO3,k)
          end if
          if (sed_select(is_FeCO3_13C)) then
             ! calculate 13C/12C fractionation between DIC and FeCO3
             ! NOTE: T-dependent fractionation for calcite following Mook [1986]
             ! NOTE: FeCO3 fractionation w.r.t. CO3-
             loc_delta_FeCO3 = 15.10 - 4232.0/ocn(io_T,dum_i,dum_j,k)
             loc_alpha = 1.0 + loc_delta_FeCO3/1000.0
             loc_R = carbisor(ici_CO3_r13C,dum_i,dum_j,k)/(1.0 - carbisor(ici_CO3_r13C,dum_i,dum_j,k))
             loc_bio_part(is_FeCO3_13C,k) = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_bio_part(is_FeCO3,k)
          end if
       end if
       ! convert particulate sediment tracer indexed array concentrations to (dissolved) tracer indexed array
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_tot_i = conv_sed_ocn_i(0,is)
          do loc_i=1,loc_tot_i
             io = conv_sed_ocn_i(loc_i,is)
             loc_bio_uptake(io,k) = loc_bio_uptake(io,k) + conv_sed_ocn(io,is)*loc_bio_part(is,k)
          end do
       end DO
    end DO
    ! -------------------------------------------------------- !
    ! SET GLOBAL ARRAYS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! TRACER CONCENTRATIONS
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) - loc_bio_uptake(io,:)
    end do
    ! -------------------------------------------------------- ! PARTICULATE CONCENTRATIONS
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       bio_part(is,dum_i,dum_j,:) = bio_part(is,dum_i,dum_j,:) + loc_bio_part(is,:)
    end DO
    ! -------------------------------------------------------- ! MODIFY DET TRACER FLUX
    bio_part(is_det,dum_i,dum_j,:) = bio_part(is_det,dum_i,dum_j,:) + loc_bio_part(is_FeCO3,:)
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record geochem diagnostics (mol kg-1)
    diag_precip(idiag_precip_FeCO3_dFe,dum_i,dum_j,:)  = loc_bio_uptake(io_Fe2,:)
    diag_precip(idiag_precip_FeCO3_dDIC,dum_i,dum_j,:) = loc_bio_uptake(io_DIC,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_precip_FeCO3
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ABIOTIC greenalite precipitation
  SUBROUTINE sub_calc_precip_Fe3Si2O4(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real,dimension(n_sed,n_k)::loc_bio_part
    real,dimension(1:3)::loc_Fe2spec
    real::loc_IAP
    real::loc_Fe2,loc_SiO2,loc_H,loc_H2S,loc_Fe3Si2O4_precipitation
    real::loc_r56Fe,loc_R_56Fe,loc_r30Si
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_uptake(io,:) = 0.0
    end do
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       loc_bio_part(is,:) = 0.0
    end DO
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! CALCULATE Fe3Si2O4 PRECIPITATION
    ! -------------------------------------------------------- !
    ! 3Fe2+ + 2SiO2 + 5H2O -> Fe3Si2O5(OH)4 + 6H
    ! NOTE: in selecting is_FeCO3, full water column carbonate chemsitry re-calculation has been automatically selected
    !       (ctrl_carbchemupdate_full = .true.)
    DO k=n_k,dum_k1,-1
       ! set local species concentrations
       ! NOTE: loc_SiO2 now the ambient dissolved silica concentration 
       !       (previously it was hard-set via par_bio_Fe3Si2O4precip_cSi -- 'assumed SiO2 concentration in diatom-free ocean')
       loc_Fe2  = ocn(io_Fe2,dum_i,dum_j,k)   
       loc_SiO2 = ocn(io_SiO2,dum_i,dum_j,k)
       loc_H    = carb(ic_H,dum_i,dum_j,n_k)
       loc_H2S  = ocn(io_H2S,dum_i,dum_j,k)    
       ! set local Fe2 avialability
       ! NOTE: if explicit FeS2 precip is not selected, Fe2 speciation must be calculated because greenalite precipitates
       !       from free Fe2+, so we need to find the available free Fe2+ pool
       ! modify local Fe2 avialability
       if (.NOT. ctrl_bio_FeS2precip_explicit) then
          if ((loc_Fe2 > const_real_nullsmall) .AND. (loc_H2S > const_real_nullsmall)) then
             loc_Fe2spec = fun_box_calc_spec_Fe2(ocn(io_Fe2,dum_i,dum_j,k),ocn(io_H2S,dum_i,dum_j,k),par_bio_FeS_abioticohm_cte)
             loc_Fe2     = min(loc_Fe2,loc_Fe2spec(1))
          end if
       end if
       ! calculate precipitation
       if ((loc_Fe2 > const_rns) .AND. (loc_SiO2 > const_rns)) then
          ! calculate IAP
          ! NOTE: gamma parameters are activity coefficients
          ! NOTE: Calculate IAP according to Rasmussen et al., 2019, Geology
          loc_IAP = (((par_bio_remin_gammaSiO2*loc_SiO2)**2.0)*((par_bio_remin_gammaFe2*loc_Fe2)**3.0)) / &
               & ((par_bio_remin_gammaH*loc_H)**6.0)
          ! calculate Fe3Si2O4 precipitation based on IAP, following Rasmussen et al., 2019, Geology
          if (loc_IAP > const_rns) then
             loc_Fe3Si2O4_precipitation = dum_dtyr*par_bio_Fe3Si2O4precip_sf* &
                  & exp(par_bio_Fe3Si2O4precip_exp*LOG10(loc_IAP/par_bio_Fe3Si2O4precip_abioticohm_cte))
          else
             loc_Fe3Si2O4_precipitation = 0.0 
          end if
          ! cap Fe3Si2O4 precip at maximum of available Fe2, SiO2
          loc_Fe3Si2O4_precipitation = MIN(loc_Fe3Si2O4_precipitation,loc_f*loc_Fe2,loc_f*loc_SiO2)
          ! bulk tracer conversion
          loc_bio_part(is_Fe3Si2O4,k) = loc_Fe3Si2O4_precipitation
          ! calculate isotopic fractionation -- 56Fe
          ! NOTE: we already know that loc_Fe2 is non-zero, and to have got this far, io_Fe2 must also be non-zero
          ! NOTE: loc_Fe2 may differ from ocn(io_Fe2,dum_i,dum_j,k) and hence the isotopic ratio of the Fe reservoir
          !       needs to be calculated w.r.t. the latter
          if (sed_select(is_Fe3Si2O4_56Fe)) then
             loc_r56Fe = ocn(io_Fe2_56Fe,dum_i,dum_j,k)/ocn(io_Fe2,dum_i,dum_j,k)
             loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)
             loc_bio_part(is_Fe3Si2O4_56Fe,k) = &
                  & par_d56Fe_Fe3Si2O4_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fe3Si2O4_alpha*loc_R_56Fe)*loc_bio_part(is_Fe3Si2O4,k)
          end if
          ! calculate isotopic fractionation -- 30Si
          ! NOTE: we already know that loc_SiO2 is non-zero
          ! NOTE: assume no fractionation for now
          if (sed_select(is_Fe3Si2O4_30Si)) then
             loc_r30Si = ocn(io_SiO2_30Si,dum_i,dum_j,k)/ocn(io_SiO2,dum_i,dum_j,k)
             loc_bio_part(is_Fe3Si2O4_30Si,k) = loc_r30Si*loc_bio_part(is_Fe3Si2O4,k)
          end if          
       end if
       ! convert particulate sediment tracer indexed array concentrations to (dissolved) tracer indexed array
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_tot_i = conv_sed_ocn_i(0,is)
          do loc_i=1,loc_tot_i
             io = conv_sed_ocn_i(loc_i,is)
             loc_bio_uptake(io,k) = loc_bio_uptake(io,k) + conv_sed_ocn(io,is)*loc_bio_part(is,k)
          end do
       end DO
    end DO
    ! -------------------------------------------------------- !
    ! SET GLOBAL ARRAYS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! TRACER CONCENTRATIONS
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) - loc_bio_uptake(io,:)
    end do
    ! -------------------------------------------------------- ! PARTICULATE CONCENTRATIONS
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       bio_part(is,dum_i,dum_j,:) = bio_part(is,dum_i,dum_j,:) + loc_bio_part(is,:)
    end DO
    ! -------------------------------------------------------- ! MODIFY DET TRACER FLUX
    bio_part(is_det,dum_i,dum_j,:) = bio_part(is_det,dum_i,dum_j,:) + loc_bio_part(is_Fe3Si2O4,:)
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record geochem diagnostics (mol kg-1)
    diag_precip(idiag_precip_Fe3SiO4_dFe,dum_i,dum_j,:) = loc_bio_uptake(io_Fe2,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_precip_Fe3Si2O4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SULPHUR
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! OXIDATION OF HYDROGEN SULPHIDE
  SUBROUTINE sub_box_oxidize_H2S(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_O2,loc_H2S,loc_r34S
    real::loc_H2S_oxidation_const,loc_H2S_oxidation
    real,dimension(n_ocn,n_k)::loc_bio_remin
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! OXIDIZE H2S
    ! -------------------------------------------------------- !
    ! look for some H2S and see if it can be instantaneously oxidized (using O2; if there is any!)
    ! H2S + 2O2 -> SO4 + 2H
    ! NOTE: loc_H2S_oxidation_const units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       loc_O2  = ocn(io_O2,dum_i,dum_j,k)
       loc_H2S = ocn(io_H2S,dum_i,dum_j,k)
       if ( (loc_O2 > const_rns) .AND. (loc_H2S > const_rns) ) then
          ! calculate H2S oxidation, and cap value at H2S, O2 concentration if necessary
          SELECT CASE (opt_bio_remin_oxidize_H2StoSO4)
          CASE ('linear')
             ! NOTE: par_bio_remin_kH2StoSO4 units are (M-1 yr-1)
             loc_H2S_oxidation_const = par_bio_remin_kH2StoSO4
             loc_H2S_oxidation = dum_dtyr*loc_H2S_oxidation_const*loc_H2S*loc_O2
          CASE ('OLD')
             ! change units of H2S oxidation constant from mM-2 hr-1 to M-2 yr-1
             ! and convert from O2 consumption units to H2S units (i.e., divide by 2)
             loc_H2S_oxidation_const = 0.5*const_oxidation_coeff_H2S/conv_hr_yr/(conv_mmol_mol)**2.0
             loc_H2S_oxidation = dum_dtyr*loc_H2S_oxidation_const*loc_H2S*loc_O2**2.0
          case ('complete')
             loc_H2S_oxidation = min(loc_H2S,0.5*loc_O2)
          CASE ('OLDDEFAULT')
             ! entirely spurious ... but here for completness
             loc_H2S_oxidation = min(loc_H2S,0.5*loc_O2)
          case default
             loc_H2S_oxidation = 0.0
          end select
          ! cap H2S oxidation & O2 consumption
          loc_H2S_oxidation = min(loc_H2S_oxidation,loc_f*loc_H2S,loc_f*(1.0/2.0)*loc_O2)
          ! bulk tracer conversion
          loc_bio_remin(io_H2S,k) = -loc_H2S_oxidation
          loc_bio_remin(io_SO4,k) = loc_H2S_oxidation
          loc_bio_remin(io_O2,k)  = -2.0*loc_H2S_oxidation
          loc_bio_remin(io_ALK,k) = -2.0*loc_H2S_oxidation
          ! calculate isotopic fractionation -- 34S
          ! NOTE: we already know that loc_H2S is non-zero
          if (ocn_select(io_H2S_34S) .AND. ocn_select(io_SO4_34S)) then
             loc_r34S  = ocn(io_H2S_34S,dum_i,dum_j,k)/loc_H2S
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO S FRACTIONATION ########################################################## !
             loc_bio_remin(io_H2S_34S,k) = -loc_r34S*loc_H2S_oxidation
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*loc_H2S_oxidation
             ! ################################################################################################################### !
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1) OLD
    diag_geochem_old(idiag_geochem_old_dH2S,dum_i,dum_j,:) = loc_bio_remin(io_H2S,:)
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_H2StoSO4_dH2S',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_H2S,:)
    id = fun_find_str_i('redox_H2StoSO4_dSO4',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_SO4,:)
    id = fun_find_str_i('redox_H2StoSO4_dO2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_O2,:)
    id = fun_find_str_i('redox_H2StoSO4_dALK',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_ALK,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_oxidize_H2S
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! 'REDUCTION' OF SULPHATE [fix for negative oxygen!!!]
  SUBROUTINE sub_box_reduce_SO4(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_O2,loc_SO4,loc_r34S
    real::loc_SO4_reduction
    real,dimension(n_ocn,n_k)::loc_bio_remin
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! REDUCE SO4
    ! -------------------------------------------------------- !
    ! look for some negative O2 and positive SO4 and see if it can be 'reacted' ... :o)
    ! SO4 + 2H -> H2S + 2O2
    DO k=n_k,dum_k1,-1
       loc_O2  = ocn(io_O2,dum_i,dum_j,k)
       loc_SO4 = ocn(io_SO4,dum_i,dum_j,k)
       if ( (loc_O2 < -const_rns) .AND. (loc_SO4 > const_rns) ) then
          ! calculate SO4 'reduction'
          loc_SO4_reduction = (1.0/2.0)*abs(loc_O2)
          ! cap SO4 'reduction' & negative O2 consumption
          loc_SO4_reduction = min(loc_SO4_reduction,loc_f*loc_SO4,loc_f*(2.0/1.0)*abs(loc_O2))
          ! bulk tracer conversion
          loc_bio_remin(io_H2S,k) = loc_SO4_reduction
          loc_bio_remin(io_SO4,k) = -loc_SO4_reduction
          loc_bio_remin(io_O2,k)  = 2.0*loc_SO4_reduction
          loc_bio_remin(io_ALK,k) = 2.0*loc_SO4_reduction
          ! calculate isotopic fractionation -- 34S
          ! NOTE: we already know that loc_SO4 is non-zero
          if (ocn_select(io_H2S_34S) .AND. ocn_select(io_SO4_34S)) then
             loc_r34S  = ocn(io_SO4_34S,dum_i,dum_j,k)/loc_SO4
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO S FRACTIONATION ########################################################## !
             loc_bio_remin(io_H2S_34S,k) = loc_r34S*loc_SO4_reduction
             loc_bio_remin(io_SO4_34S,k) = -loc_r34S*loc_SO4_reduction
             ! ################################################################################################################### !
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    !id = fun_find_str_i('redox_H2StoSO4_dH2S',string_diag_redox)
    !diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_H2S,:)
    !id = fun_find_str_i('redox_H2StoSO4_dSO4',string_diag_redox)
    !diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_SO4,:)
    !id = fun_find_str_i('redox_H2StoSO4_dO2',string_diag_redox)
    !diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_O2,:)
    !id = fun_find_str_i('redox_H2StoSO4_dALK',string_diag_redox)
    !diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_ALK,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_reduce_SO4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! METHANE
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WATER COLUMN REMINERALIZATION OF METHANE - DEFAULT
  ! NOTE: old code -- no longer selectable as an option!!!
  SUBROUTINE sub_calc_bio_remin_oxidize_CH4(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_potO2cap
    real::loc_CH4
    real::loc_r13C,loc_r14C
    real::loc_frac
    real,dimension(n_ocn,n_k)::loc_bio_remin
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! OXIDIZE CH4
    ! -------------------------------------------------------- !
    ! look for some CH4 and see if it can be oxidized
    ! allow different rate constant depending on availability of O2 or not
    ! CH4 + 2O2 -> CO2 + 2H2O
    DO k=n_k,dum_k1,-1
       ! calculate potential oxidation capacity
       loc_potO2cap = ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k)
       if ((ocn(io_CH4,dum_i,dum_j,k) > const_real_nullsmall) .AND. (loc_potO2cap > const_real_nullsmall)) then
          ! calculate CH4 oxidation
          ! NOTE: units of par_bio_remin_CH4rate == per year
          loc_frac = dum_dtyr*par_bio_remin_CH4rate
          if (loc_frac > 1.0) loc_frac = 1.0
          loc_CH4 = loc_frac*ocn(io_CH4,dum_i,dum_j,k)
          ! calculate isotopic ratio
          loc_r13C = ocn(io_CH4_13C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          loc_r14C = ocn(io_CH4_14C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          if (loc_CH4 <= 0.5*loc_potO2cap) then
             ! complete CH4 oxidation (no C fractionation)
             loc_bio_remin(io_CH4,k) = -loc_CH4
             loc_bio_remin(io_DIC,k) = loc_CH4
             loc_bio_remin(io_O2,k)  = -2.0*loc_CH4
             loc_bio_remin(io_CH4_13C,k) = -loc_r13C*loc_CH4
             loc_bio_remin(io_CH4_14C,k) = -loc_r14C*loc_CH4
             loc_bio_remin(io_DIC_13C,k) = loc_r13C*loc_CH4
             loc_bio_remin(io_DIC_14C,k) = loc_r14C*loc_CH4
          else
             ! partial CH4 oxidation (=> C isotope Rayleigh fractionation)
             loc_bio_remin(io_CH4,k) = -0.5*loc_potO2cap
             loc_bio_remin(io_DIC,k) = 0.5*loc_potO2cap
             loc_bio_remin(io_O2,k)  = -loc_potO2cap
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO C FRACTIONATION ########################################################## !
             loc_bio_remin(io_CH4_13C,k) = -loc_r13C*0.5*loc_potO2cap
             loc_bio_remin(io_CH4_14C,k) = -loc_r14C*0.5*loc_potO2cap
             loc_bio_remin(io_DIC_13C,k) = loc_r13C*0.5*loc_potO2cap
             loc_bio_remin(io_DIC_14C,k) = loc_r14C*0.5*loc_potO2cap
             ! ################################################################################################################### !
          end if
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1) OLD
    diag_geochem_old(idiag_geochem_old_dCH4,dum_i,dum_j,:) = -loc_bio_remin(io_CH4,:)
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_CH4toDIC_dCH4',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_CH4,:)
    id = fun_find_str_i('redox_CH4toDIC_dDIC',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_DIC,:)
    id = fun_find_str_i('redox_CH4toDIC_dO2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_O2,:)
    id = fun_find_str_i('redox_CH4toDIC_dALK',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_ALK,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_bio_remin_oxidize_CH4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! AEROBIC WATER COLUMN REMINERALIZATION OF METHANE - new Michaelis-Menten scheme [CTR|2018]
  SUBROUTINE sub_calc_bio_remin_oxidize_CH4_AER(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_O2
    real::loc_CH4,loc_CO2
    real::loc_T,loc_TC,loc_kT
    real::loc_dG,loc_Ft,loc_Ft_min
    real::loc_MM,loc_AER
    real::loc_r13C,loc_r14C
    real,dimension(n_ocn,n_k)::loc_bio_remin
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! OXIDIZE CH4
    ! -------------------------------------------------------- !
    ! look for some CH4 and see if it can be oxidized
    ! allow different rate constant depending on availability of O2 or not
    ! CH4 + 2O2 -> CO2 + 2H2O
    DO k=n_k,dum_k1,-1
       ! pull relevant tracers
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_CH4 = ocn(io_CH4,dum_i,dum_j,k)
       if ((loc_CH4 > const_real_nullsmall) .AND. (loc_O2 > const_real_nullsmall)) then
          SELECT CASE (par_bio_remin_AER_thermo)
          CASE('off')
             ! thermo term disabled
             loc_Ft = 1.0
          CASE('on')
             ! estimate free energy available for aerobic methanotrophy
             loc_CO2 = carb(ic_conc_CO2,dum_i,dum_j,k)
             loc_T = ocn(io_T,dum_i,dum_j,k)
             loc_dG = par_bio_remin_AER_dG0 +                                                     &
                  & ( par_bio_remin_Rgas *                                                     &
                  &   loc_T *                                                                  &
                  &   LOG( (par_bio_remin_gammaCO2*loc_CO2) /                                  &
                  &        ((par_bio_remin_gammaO2*loc_O2)*(par_bio_remin_gammaCH4*loc_CH4)) ) &
                  & )
             ! calculate thermodynamic drive
             loc_Ft_min = 0
             loc_Ft = max(loc_Ft_min,1-exp((loc_dG+par_bio_remin_AER_BEQ)/(par_bio_remin_Rgas*loc_T)))
             if (loc_Ft > 1 .OR. loc_Ft < 0) then
                print*,' WARNING: AER thermodynamic drive out of bounds; DIC = ',ocn(io_DIC,dum_i,dum_j,k), &
                     &' ALK = ',ocn(io_ALK,dum_i,dum_j,k),' Ft = ',loc_Ft,'.'
             end if
          CASE default
             loc_Ft = 1.0
          END SELECT
          ! allow CH4 oxidation with O2 (units: mol CH4 kg-1)
          ! Michaelis-Menten term
          loc_MM = loc_O2/(loc_O2+par_bio_remin_AER_Km_O2)
          ! temperature term
          loc_TC = ocn(io_T,dum_i,dum_j,k) - const_zeroC
          loc_kT = par_bio_kT0*exp(loc_TC/par_bio_kT_eT)
          ! rate of aerobic methanotrophy 
          ! (first-order term for 'bloom' conditions, Michaelis-Menten kinetics, and temperature control)
          loc_AER = par_bio_remin_AER_kAER*loc_CH4*loc_MM*loc_kT*loc_Ft*dum_dtyr
          ! but don't oxidize too much CH4!
          loc_AER = min(loc_AER,loc_f*loc_CH4,loc_f*(1.0/2.0)*loc_O2)
          ! calculate isotopic ratios
          loc_r13C = ocn(io_CH4_13C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          loc_r14C = ocn(io_CH4_14C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          ! perform aerobic methanotrophy
          loc_bio_remin(io_CH4,k)     = -loc_AER
          loc_bio_remin(io_DIC,k)     =  loc_AER
          loc_bio_remin(io_O2,k)      = -2.0*loc_AER
          loc_bio_remin(io_CH4_13C,k) = -loc_r13C*loc_AER
          loc_bio_remin(io_CH4_14C,k) = -loc_r14C*loc_AER
          loc_bio_remin(io_DIC_13C,k) =  loc_r13C*loc_AER
          loc_bio_remin(io_DIC_14C,k) =  loc_r14C*loc_AER
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1) OLD
    diag_geochem_old(idiag_geochem_old_dCH4,dum_i,dum_j,:) = -loc_bio_remin(io_CH4,:)
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_CH4toDIC_dCH4',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_CH4,:)
    id = fun_find_str_i('redox_CH4toDIC_dDIC',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_DIC,:)
    id = fun_find_str_i('redox_CH4toDIC_dO2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_O2,:)
    id = fun_find_str_i('redox_CH4toDIC_dALK',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_ALK,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_bio_remin_oxidize_CH4_AER
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ANAEROBIC WATER COLUMN REMINERALIZATION OF METHANE [SLO|2015 CTR|2018]
  SUBROUTINE sub_calc_bio_remin_oxidize_CH4_AOM(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_O2
    real::loc_CH4,loc_HCO3
    real::loc_SO4,loc_H2S
    real::loc_T,loc_TC,loc_kT
    real::loc_dG,loc_Ft,loc_Ft_min
    real::loc_MM,loc_AOM
    real::loc_r13C,loc_r14C,loc_r34S
    real,dimension(n_ocn,n_k)::loc_bio_remin
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! OXIDIZE CH4 ANAEROBICALLY WITH SO4
    ! -------------------------------------------------------- !
    ! look for some CH4 and see if it can be oxidized with SO4
    ! CH4 + SO4 --> HCO3- + HS- + H2O
    DO k=n_k,dum_k1,-1
       ! pull relevant tracers, and check for the presence of O2
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_SO4 = ocn(io_SO4,dum_i,dum_j,k)
       loc_CH4 = ocn(io_CH4,dum_i,dum_j,k)
       if ((loc_O2 < const_real_nullsmall) .AND. (loc_SO4 > const_real_nullsmall) .AND. (loc_CH4 > const_real_nullsmall)) then
          SELECT CASE (par_bio_remin_AOM_thermo)
          CASE('off')
             ! thermo term disabled
             loc_Ft = 1.0
          CASE('on')
             ! estimate free energy available for anaerobic oxidation of methane           
             loc_H2S = ocn(io_H2S,dum_i,dum_j,k)
             loc_HCO3 = carb(ic_conc_HCO3,dum_i,dum_j,k)
             loc_T = ocn(io_T,dum_i,dum_j,k)
             loc_dG = par_bio_remin_AOM_dG0 +                                                        &
                  & ( par_bio_remin_Rgas *                                                        &
                  &   loc_T *                                                                     &
                  &   LOG( ((par_bio_remin_gammaHS*loc_H2S)*(par_bio_remin_gammaHCO3*loc_HCO3)) / &
                  &        ((par_bio_remin_gammaSO4*loc_SO4)*(par_bio_remin_gammaCH4*loc_CH4)) )  &
                  & )
             ! calculate thermodynamic drive
             loc_Ft_min = 0
             loc_Ft = max(loc_Ft_min,1 - exp((loc_dG+par_bio_remin_AOM_BEQ)/(par_bio_remin_Rgas*loc_T)))
             if (loc_Ft > 1 .OR. loc_Ft < 0) then
                print*,' WARNING: AOM thermodynamic drive out of bounds; DIC = ',ocn(io_DIC,dum_i,dum_j,k), &
                     &' ALK = ',ocn(io_ALK,dum_i,dum_j,k),' Ft = ',loc_Ft,'.'
             end if
          CASE default
             loc_Ft = 1.0
          END SELECT
          ! allow CH4 oxidation coupled to SO4 reduction (units: mol CH4 kg-1)
          ! Michaelis-Menten term
          loc_MM = loc_SO4/(loc_SO4*par_bio_remin_AOM_Km_SO4)
          ! temperature term
          loc_TC = ocn(io_T,dum_i,dum_j,k) - const_zeroC
          loc_kT = par_bio_kT0*exp(loc_TC/par_bio_kT_eT)
          ! rate of AOM 
          ! (first-order term for 'bloom' conditions, Michaelis-Menten kinetics, temperature, and thermodynamic control)
          loc_AOM = par_bio_remin_AOM_kAOM*loc_CH4*loc_MM*loc_kT*loc_Ft*dum_dtyr
          ! but don't oxidize too much CH4!
          loc_AOM = min(loc_AOM,loc_f*loc_CH4,loc_f*loc_SO4)
          ! calculate isotopic ratios
          loc_r13C = ocn(io_CH4_13C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          loc_r14C = ocn(io_CH4_14C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          loc_r34S = ocn(io_SO4_34S,dum_i,dum_j,k)/ocn(io_SO4,dum_i,dum_j,k)
          ! perform AOM
          loc_bio_remin(io_CH4,k)     = -loc_AOM
          loc_bio_remin(io_DIC,k)     =  loc_AOM
          loc_bio_remin(io_SO4,k)     = -loc_AOM
          loc_bio_remin(io_H2S,k)     =  loc_AOM
          loc_bio_remin(io_ALK,k)     =  2.0*loc_AOM
          loc_bio_remin(io_CH4_13C,k) = -loc_r13C*loc_AOM
          loc_bio_remin(io_CH4_14C,k) = -loc_r14C*loc_AOM
          loc_bio_remin(io_DIC_13C,k) =  loc_r13C*loc_AOM
          loc_bio_remin(io_DIC_14C,k) =  loc_r14C*loc_AOM
          loc_bio_remin(io_SO4_34S,k) = -loc_r34S*loc_AOM
          loc_bio_remin(io_H2S_34S,k) =  loc_r34S*loc_AOM
         
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1) OLD
    diag_geochem_old(idiag_geochem_old_dCH4_AOM,dum_i,dum_j,:) = -loc_bio_remin(io_CH4,:)
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_CH4toDICaom_dCH4',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_CH4,:)
    id = fun_find_str_i('redox_CH4toDICaom_dDIC',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_DIC,:)
    id = fun_find_str_i('redox_CH4toDICaom_dH2S',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_H2S,:)
    id = fun_find_str_i('redox_CH4toDICaom_dSO4',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_SO4,:)
    id = fun_find_str_i('redox_CH4toDICaom_dALK',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_ALK,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_bio_remin_oxidize_CH4_AOM
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! IODINE
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! OXIDATION OF IODIDE
  SUBROUTINE sub_calc_bio_remin_oxidize_I(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_O2,loc_I
    real::loc_I_oxidation
    real,dimension(n_ocn,n_k)::loc_bio_remin
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! OXIDIZE IODIDE
    ! -------------------------------------------------------- !
    ! look for some I and see if it can be instantaneously oxidized (using O2; if there is any!)
    ! 2I + 3O2 -> 2IO3
    ! NOTE: loc_I_oxidation_const units are (???)
    DO k=n_k,dum_k1,-1
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_I  = ocn(io_I,dum_i,dum_j,k)
       if ((loc_O2 > const_real_nullsmall) .AND. (loc_I > const_real_nullsmall)) then
          ! calculate I oxidation, and cap value at I concentration if necessary
          SELECT CASE (opt_bio_remin_oxidize_ItoIO3)
          CASE ('Fennel')
             ! from: Fennel et al. [2005]
             ! oxidation rate constant: 6 yr-1
             ! oxidation half saturation for oxygen: 2.0E-05 mol kg-1
             loc_I_oxidation = dum_dtyr*par_bio_remin_kItoIO3*loc_I*(loc_O2/(par_bio_remin_cO2_ItoIO3 + loc_O2))
          case ('lifetime')
             if (par_bio_remin_Ilifetime > dum_dtyr) then
                loc_I_oxidation = min((dum_dtyr/par_bio_remin_Ilifetime)*loc_I,(2.0/3.0)*loc_O2)
             else
                loc_I_oxidation = min(loc_I,(2.0/3.0)*loc_O2)
             end if
          case ('complete')
             loc_I_oxidation = min(loc_I,(2.0/3.0)*loc_O2)
          case default
             loc_I_oxidation = 0.0
          end select
          ! double-check on I removal ...
          if (loc_I_oxidation > loc_I) loc_I_oxidation = loc_I
          ! calculate tracer remin changes
          loc_bio_remin(io_I,k)   = -loc_I_oxidation
          loc_bio_remin(io_IO3,k) = loc_I_oxidation
          loc_bio_remin(io_O2,k)  = -(3.0/2.0)*loc_I_oxidation
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_ItoIO3_dI',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_I,:)
    id = fun_find_str_i('redox_ItoIO3_dIO3',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_IO3,:)
    id = fun_find_str_i('redox_ItoIO3_dO2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_O2,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_bio_remin_oxidize_I
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ### >>> TEMPORARY CODE ... ################################################################################################### !
  ! ****************************************************************************************************************************** !
  ! REDUCTION OF IODATE
  SUBROUTINE sub_calc_bio_remin_reduce_IO3(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k,id
    real::loc_O2,loc_IO3
    real::loc_IO3_reduction
    real,dimension(n_ocn,n_k)::loc_bio_remin
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! REDUCE IODATE
    ! -------------------------------------------------------- !
    ! look for some IO3 and see if it can be instantaneously reduced
    ! 2IO3 -> 2I + 3O2
    DO k=n_k,dum_k1,-1
       loc_O2   = ocn(io_O2,dum_i,dum_j,k)
       loc_IO3  = ocn(io_IO3,dum_i,dum_j,k)
       if (loc_IO3 > const_real_nullsmall) then
          ! calculate IO3 reduction
          SELECT CASE (opt_bio_remin_reduce_IO3toI)
          case ('inhibition')
             loc_IO3_reduction = dum_dtyr*par_bio_remin_kIO3toI*loc_IO3* &
                  & (loc_IO3/(loc_IO3 + par_bio_remin_cIO3_IO3toI))*(1.0 - loc_O2/(loc_O2 + par_bio_remin_cO2_IO3toI))
          case ('threshold')
             if (loc_O2 < par_bio_remin_cO2_IO3toI) then
                loc_IO3_reduction = loc_IO3
             else
                loc_IO3_reduction = 0.0
             endif
          case default
             loc_IO3_reduction = 0.0
          end select
          ! double-check on IO3 removal ...
          if (loc_IO3_reduction > loc_IO3) loc_IO3_reduction = loc_IO3
          ! calculate tracer remin changes
          loc_bio_remin(io_IO3,k) = -loc_IO3_reduction
          loc_bio_remin(io_I,k)   = loc_IO3_reduction
          loc_bio_remin(io_O2,k)  = (3.0/2.0)*loc_IO3_reduction
       end if
    end DO
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    id = fun_find_str_i('redox_IO3toI_dI',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_I,:)
    id = fun_find_str_i('redox_IO3toI_dIO3',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_IO3,:)
    id = fun_find_str_i('redox_IO3toI_dO2',string_diag_redox)
    diag_redox(id,dum_i,dum_j,:) = loc_bio_remin(io_O2,:)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_bio_remin_reduce_IO3
  ! ****************************************************************************************************************************** !
  ! ### <<< TEMPORARY CODE ... ################################################################################################### !
  ! ****************************************************************************************************************************** !


END MODULE biogem_box_geochem

