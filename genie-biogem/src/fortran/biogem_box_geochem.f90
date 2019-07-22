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
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! OXIDIZE Fe2
    ! -------------------------------------------------------- !
    ! look for some Fe2 and see if it can be instantaneously oxidized (using O2; if there is any!)
    ! Fe2 + 1/4*O2 -> Fe3 
    ! NOTE: loc_Fe2_oxidation_const units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
       if ((4.0/1.0*loc_O2 > const_rns) .AND. (loc_Fe2 > const_rns)) then
          ! calculate H2S oxidation, and cap value at H2S concentration if necessary
          ! NOTE: par_bio_remin_kH2StoSO4 units are (M-1 yr-1)

          loc_Fe2_oxidation = min(dum_dtyr*par_bio_remin_kFe2toFe*loc_Fe2*loc_O2,4.0/1.0*loc_O2)

          ! calculate isotopic ratio
          loc_r56Fe = ocn(io_Fe2_56Fe,dum_i,dum_j,k)/ocn(io_Fe2,dum_i,dum_j,k)

          if (loc_Fe2_oxidation > loc_Fe2) then
             ! complete Fe2 oxidation (no S fractionation)
             loc_Fe2_oxidation = loc_Fe2
             loc_bio_remin(io_Fe2,k) = -loc_Fe2
             loc_bio_remin(io_Fe,k)  = loc_Fe2
             loc_bio_remin(io_O2,k)  = -1.0/4.0*loc_Fe2
             !loc_bio_remin(io_ALK,k) = -2.0*loc_Fe2
             loc_bio_remin(io_Fe2_56Fe,k) = -loc_r56Fe*loc_Fe2
             loc_bio_remin(io_Fe_56Fe,k) = loc_r56Fe*loc_Fe2
          else
             ! partial Fe2 oxidation (=> Fe isotope Rayleigh fractionation)

             loc_bio_remin(io_Fe2,k) = -loc_Fe2_oxidation
             loc_bio_remin(io_Fe,k)  = loc_Fe2_oxidation
             loc_bio_remin(io_O2,k)  = -1.0/4.0*loc_Fe2_oxidation
             !loc_bio_remin(io_ALK,k) = -2.0*loc_Fe2_oxidation
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO S FRACTIONATION ########################################################## !
             !loc_bio_remin(io_Fe2_56Fe,k) = -loc_r56Fe*loc_Fe2_oxidation
             !loc_bio_remin(io_Fe_56Fe,k) = loc_r56Fe*loc_Fe2_oxidation
             
             loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)

             loc_bio_remin(io_Fe2_56Fe,k)  &
                  & = -par_d56Fe_Fe2ox_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fe2ox_alpha*loc_R_56Fe)*loc_Fe2_oxidation
             loc_bio_remin(io_Fe_56Fe,k)  &
                  & = par_d56Fe_Fe2ox_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fe2ox_alpha*loc_R_56Fe)*loc_Fe2_oxidation

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
  ! CALCULATE ABIOTIC PYRITE PRECIPITATION
  SUBROUTINE sub_calc_precip_FeOOH(dum_i,dum_j,dum_k1,dum_dt)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dt
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
    real::loc_alpha_56Fe
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
    ! -------------------------------------------------------- !
    ! CALCULATE FeOOH precipitation
    ! -------------------------------------------------------- !
    ! water column loop
    ! look for existing Fe3 see if it can be instantaneously precipitated
    ! Fe3+ + 2*H2O -> FeOOH + 3*H+
    ! NOTE: Simple scheme: Fe3 > 1 nM precipitates
    DO k=n_k,dum_k1,-1
       ! set local concentrations
       loc_Fe = ocn(io_Fe,dum_i,dum_j,k)
       ! calculate FeOOH precipitation
       if (ctrl_bio_FeOOHprecip_explicit) then
          if (loc_Fe > const_rns) then
             loc_bio_part(is_FeOOH,k) = loc_Fe - 1.0E-09
             loc_r56Fe  = ocn(io_Fe_56Fe,dum_i,dum_j,k)/ocn(io_Fe,dum_i,dum_j,k)
          else   
             loc_bio_part(is_FeOOH,k) = 0.0  
             loc_r56Fe = 0.0
          end if
       else   
          loc_bio_part(is_FeOOH,k) = 0.0  
          loc_r56Fe = 0.0
       end if
       ! calculate isotopic ratio
       !if (loc_Fe > 5.0e-11) then
          
       !else
       !   
       !end if
       loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)
       ! Potential Fe fractionation
       loc_bio_part(is_FeOOH_56Fe,k) = &
            & par_d56Fe_FeOOH_alpha*loc_R_56Fe/(1.0 + par_d56Fe_FeOOH_alpha*loc_R_56Fe)*loc_bio_part(is_FeOOH,k)
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
    real::loc_H2S,loc_Fe,loc_r56Fe, loc_R_56Fe, loc_r34S, loc_R_34S
    real::loc_Fe_reduction
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
    ! OXIDIZE Fe2
    ! -------------------------------------------------------- !
    ! look for some Fe2 and see if it can be instantaneously oxidized (using O2; if there is any!)
    ! Fe3 + 1/8*H2S -> Fe2 + 1/8*SO4 + 1/4*H+
    ! NOTE: loc_Fe_reduction_const units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       loc_H2S = ocn(io_H2S,dum_i,dum_j,k)
       loc_Fe = ocn(io_Fe,dum_i,dum_j,k)
       if ((8.0/1.0*loc_H2S > const_rns) .AND. (loc_Fe > 1.0e-10)) then
          ! calculate H2S oxidation, and cap value at H2S concentration if necessary
          ! NOTE: par_bio_remin_kH2StoSO4 units are (M-1 yr-1)

          loc_Fe_reduction = dum_dtyr*par_bio_remin_kFetoFe2*loc_Fe*loc_H2S

          ! calculate isotopic ratio
          loc_r56Fe = ocn(io_Fe_56Fe,dum_i,dum_j,k)/ocn(io_Fe,dum_i,dum_j,k)
          loc_r34S = ocn(io_H2S_34S,dum_i,dum_j,k)/ocn(io_H2S,dum_i,dum_j,k)

          if (loc_Fe_reduction > loc_Fe) then

             ! complete Fe reduction (no Fe fractionation)
             loc_Fe_reduction = loc_Fe
             loc_bio_remin(io_Fe,k)  = -loc_Fe
             loc_bio_remin(io_Fe2,k) = loc_Fe
             loc_bio_remin(io_H2S,k)  = -1.0/8.0*loc_Fe
             loc_bio_remin(io_SO4,k)  = 1.0/8.0*loc_Fe
             loc_bio_remin(io_ALK,k) = -1.0/4.0*loc_Fe
             loc_bio_remin(io_Fe_56Fe,k) = -loc_r56Fe*loc_Fe
             loc_bio_remin(io_Fe2_56Fe,k) = loc_r56Fe*loc_Fe

             ! partial H2S oxidation (S fractionation)

             loc_R_34S = loc_r34S/(1.0 - loc_r34S)

             loc_bio_remin(io_H2S_34S,k)  &
                  & = -par_d34S_Fered_alpha*loc_R_34S/(1.0 + par_d34S_Fered_alpha*loc_R_34S)*1.0/8.0*loc_Fe
             loc_bio_remin(io_SO4_34S,k)  &
                  & = par_d34S_Fered_alpha*loc_R_34S/(1.0 + par_d34S_Fered_alpha*loc_R_34S)*1.0/8.0*loc_Fe

          elseif (loc_Fe_reduction > 8.0/1.0*loc_H2S) then

             ! partial Fe oxidation (Fe fractionation)
             loc_Fe_reduction = loc_H2S
             loc_bio_remin(io_Fe,k)  = -loc_H2S
             loc_bio_remin(io_Fe2,k) = loc_H2S
             loc_bio_remin(io_H2S,k)  = -1.0/8.0*loc_H2S
             loc_bio_remin(io_SO4,k)  = 1.0/8.0*loc_H2S
             loc_bio_remin(io_ALK,k) = -1.0/4.0*loc_H2S
             
             !loc_bio_remin(io_Fe_56Fe,k) = -loc_r56Fe*loc_H2S
             !loc_bio_remin(io_Fe2_56Fe,k) = loc_r56Fe*loc_H2S
             
             loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)

             loc_bio_remin(io_Fe_56Fe,k) = -par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_H2S
             loc_bio_remin(io_Fe2_56Fe,k) = par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_H2S

             ! complete H2S oxidation (no S fractionation)
             loc_bio_remin(io_H2S_34S,k) = -loc_r34S*1.0/8.0*loc_H2S
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*1.0/8.0*loc_H2S

          else
             ! partial Fe reduction and S oxidation (=> Fe and S isotope Rayleigh fractionation)

             loc_bio_remin(io_Fe,k)  = -loc_Fe_reduction
             loc_bio_remin(io_Fe2,k) = loc_Fe_reduction
             loc_bio_remin(io_H2S,k)  = -1.0/8.0*loc_Fe_reduction
             loc_bio_remin(io_SO4,k)  = 1.0/8.0*loc_Fe_reduction
             loc_bio_remin(io_ALK,k) = -1.0/4.0*loc_Fe_reduction

             !loc_bio_remin(io_Fe_56Fe,k) = -loc_r56Fe*loc_Fe_reduction
             !loc_bio_remin(io_Fe2_56Fe,k) = loc_r56Fe*loc_Fe_reduction
             
             loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)

             loc_bio_remin(io_Fe_56Fe,k)  = &
                  & -par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_Fe_reduction
             loc_bio_remin(io_Fe2_56Fe,k) = &
                  & par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_Fe_reduction

             loc_R_34S = loc_r34S/(1.0 - loc_r34S) 

             loc_bio_remin(io_H2S_34S,k) = &
                  & -par_d34S_Fered_alpha*loc_R_34S/(1.0 + par_d34S_Fered_alpha*loc_R_34S)*1.0/8.0*loc_Fe_reduction
             loc_bio_remin(io_SO4_34S,k) = &
                  & par_d34S_Fered_alpha*loc_R_34S/(1.0 + par_d34S_Fered_alpha*loc_R_34S)*1.0/8.0*loc_Fe_reduction

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
  ! CALCULATE AQUATIC IRONSULPHIDE FORMATION
  SUBROUTINE sub_calc_form_FeS(dum_i,dum_j,dum_k1,dum_dt)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dt
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
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
    ! water column loop
    ! look for co-existing Fe and H2S and see if they are in equilibrium with FeS
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
  SUBROUTINE sub_calc_precip_FeS2(dum_i,dum_j,dum_k1,dum_dt)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dt
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real,dimension(n_sed,n_k)::loc_bio_part
    real,dimension(1:3)::loc_Fe2spec
    real::loc_H2S,loc_SO4,loc_FeS,loc_Fe2
    real::loc_r56Fe, loc_r34S, loc_r34S_SO4, loc_r34S_FeS
    real::loc_R_56Fe
    real::loc_alpha_56Fe
    real::loc_FeS2_precipitation
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
    ! -------------------------------------------------------- !
    ! CALCULATE PYRITE SPECIATION
    ! -------------------------------------------------------- !
    ! water column loop
    ! look for co-existing Fe and H2S and see if it can be instantaneously precipitated
    ! 4Fe + 7H2S + SO4 -> 4FeS2 + 6H
    ! NOTE: par_bio_FeS2precip_k units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       ! set local concentrations
       ! NOTE: Fe2 (not Fe(3))
       ! NOTE: cap diagnosed [loc_FeS] at [loc_Fe2]
       if (ctrl_bio_FeS2precip_explicit) then
          loc_FeS  = ocn(io_FeS,dum_i,dum_j,k)
          loc_H2S  = ocn(io_H2S,dum_i,dum_j,k)
          loc_SO4 = ocn(io_SO4,dum_i,dum_j,k)
          loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
       else 
          loc_Fe2spec = fun_box_calc_spec_Fe2(ocn(io_Fe2,dum_i,dum_j,k),ocn(io_H2S,dum_i,dum_j,k),par_bio_FeS_abioticohm_cte)
          loc_FeS  = loc_Fe2spec(3)
          loc_H2S  = loc_Fe2spec(2)
          loc_SO4 = ocn(io_SO4,dum_i,dum_j,k)
          loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
          if (loc_FeS > loc_Fe2) then
             loc_FeS = loc_Fe2
          end if
       end if
       ! calculate pyrite precipitation
       ! NOTE: const_rns == const_real_nullsmall
       if ( (loc_FeS > const_rns) .AND. (4.0/3.0*loc_H2S > const_rns) .AND. (4.0/1.0*loc_SO4 > const_rns) ) then
          ! loc_FeS2_precipitation = dum_dt*par_bio_FeS2precip_k*loc_FeS*loc_H2S
          loc_FeS2_precipitation = dum_dt*par_bio_FeS2precip_k*(loc_FeS/(K_lim_PYR + loc_FeS))*loc_FeS*loc_H2S
          ! calculate isotopic ratio
          if (loc_Fe2 > const_rns) then
             loc_r56Fe = ocn(io_Fe2_56Fe,dum_i,dum_j,k)/loc_Fe2
          else
             loc_r56Fe = 0.0
          end if
          loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)
          ! test for limitation by one of the reactants
          if (loc_FeS2_precipitation > MIN(4.0/3.0*loc_H2S,loc_FeS,4.0/1.0*loc_SO4)) then
             ! reactant limited
             loc_bio_part(is_FeS2,k) = MIN(4.0/3.0*loc_H2S,loc_FeS,4.0/1.0*loc_SO4)
             if (loc_FeS2_precipitation == loc_FeS) then
                loc_bio_part(is_FeS2_56Fe,k) = loc_r56Fe*loc_bio_part(is_FeS2,k)
             else 
                ! Potential Fe fractionation
                loc_bio_part(is_FeS2_56Fe,k) = &
                    & (par_d56Fe_FeS2_alpha*loc_R_56Fe/(1.0 + par_d56Fe_FeS2_alpha*loc_R_56Fe))*loc_bio_part(is_FeS2,k)
             end if
          else
             ! no limitation
             loc_bio_part(is_FeS2,k) = loc_FeS2_precipitation
             ! isotopes
             loc_bio_part(is_FeS2_56Fe,k) = &
                 & (par_d56Fe_FeS2_alpha*loc_R_56Fe/(1.0 + par_d56Fe_FeS2_alpha*loc_R_56Fe))*loc_bio_part(is_FeS2,k)	
          end if
          ! no fractionation of S during pyrite formation
          ! calculate isotopic ratio
          if (loc_H2S > const_rns) then
              loc_r34S = ocn(io_H2S_34S,dum_i,dum_j,k)/loc_H2S
          else
              loc_r34S = 0.0
          end if
          loc_bio_part(is_FeS2_34S,k) = loc_r34S*loc_bio_part(is_FeS2,k)
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
  SUBROUTINE sub_calc_precip_FeCO3(dum_i,dum_j,dum_k1,dum_dt)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dt
    ! local variables
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real,dimension(n_sed,n_k)::loc_bio_part
    real,dimension(1:3)::loc_Fe2spec
    real::loc_ohm
    real::loc_delta_FeCO3,loc_CO3,loc_Fe2,loc_Fe,loc_H2S,loc_O2,loc_FeCO3_precipitation
    real::loc_alpha
    real::loc_R, loc_r56Fe,loc_R_56Fe
    integer::loc_kmax

    ! *** INITIALIZE VARIABLES ***
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_uptake(io,:) = 0.0
    end do
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       loc_bio_part(is,:) = 0.0
    end DO

    ! *** CALCULATE FeCO3 PRECIPITATION ***
    DO k=n_k,dum_k1,-1
       ! re-calculate carbonate dissociation constants
       CALL sub_calc_carbconst(                 &
            & phys_ocn(ipo_Dmid,dum_i,dum_j,k), &
            & ocn(io_T,dum_i,dum_j,k),          &
            & ocn(io_S,dum_i,dum_j,k),          &
            & carbconst(:,dum_i,dum_j,k)        &
            & )
       ! adjust carbonate constants
       if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
          call sub_adj_carbconst(           &
               & ocn(io_Ca,dum_i,dum_j,k),  &
               & ocn(io_Mg,dum_i,dum_j,k),  &
               & carbconst(:,dum_i,dum_j,k) &
               & )
       end if
       ! re-estimate Ca and borate concentrations from salinity (if not selected and therefore explicitly treated)
       IF (.NOT. ocn_select(io_Ca))  ocn(io_Ca,dum_i,dum_j,k)  = fun_calc_Ca(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_B))   ocn(io_B,dum_i,dum_j,k)   = fun_calc_Btot(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_SO4)) ocn(io_SO4,dum_i,dum_j,k) = fun_calc_SO4tot(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_F))   ocn(io_F,dum_i,dum_j,k)   = fun_calc_Ftot(ocn(io_S,dum_i,dum_j,k))
       ! re-calculate surface ocean carbonate chemistry
       CALL sub_calc_carb(             &
            & ocn(io_DIC,dum_i,dum_j,k),  &
            & ocn(io_ALK,dum_i,dum_j,k),  &
            & ocn(io_Ca,dum_i,dum_j,k),   &
            & ocn(io_PO4,dum_i,dum_j,k),  &
            & ocn(io_SiO2,dum_i,dum_j,k), &
            & ocn(io_B,dum_i,dum_j,k),    &
            & ocn(io_SO4,dum_i,dum_j,k),  &
            & ocn(io_F,dum_i,dum_j,k),    &
            & ocn(io_H2S,dum_i,dum_j,k),  &
            & ocn(io_NH4,dum_i,dum_j,k),  &
            & carbconst(:,dum_i,dum_j,k), &
            & carb(:,dum_i,dum_j,k),      &
            & carbalk(:,dum_i,dum_j,k)    &
            & )

       loc_CO3    = carb(ic_conc_CO3,dum_i,dum_j,n_k)
       loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
       loc_H2S = ocn(io_H2S,dum_i,dum_j,k)

       loc_r56Fe  = ocn(io_Fe2_56Fe,dum_i,dum_j,k)/ocn(io_Fe2,dum_i,dum_j,k)
       loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe)       

       if (ctrl_bio_FeS2precip_explicit) then
          loc_Fe2    = ocn(io_Fe2,dum_i,dum_j,k)
       else 
          if ((loc_Fe2 > const_real_nullsmall) .AND. (loc_H2S > const_real_nullsmall)) then
             loc_Fe2spec = fun_box_calc_spec_Fe2(ocn(io_Fe2,dum_i,dum_j,k),ocn(io_H2S,dum_i,dum_j,k),par_bio_FeS_abioticohm_cte)
             loc_Fe2  = loc_Fe2spec(1) 
                if (loc_Fe2 > ocn(io_Fe2,dum_i,dum_j,k)) then
                    loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)   
                end if                    
          else
             loc_Fe2 = ocn(io_Fe2,dum_i,dum_j,k)
          end if
       end if

       if ((loc_Fe2 > const_rns) .AND. (loc_CO3 > const_rns)) then

          loc_ohm  = (loc_CO3*loc_Fe2)/par_bio_FeCO3precip_abioticohm_cte

          if (loc_ohm > par_bio_FeCO3precip_abioticohm_min) then
             loc_FeCO3_precipitation = &
                  & dum_dt*par_bio_FeCO3precip_sf*(loc_ohm - 1.0)**par_bio_FeCO3precip_exp

          else
             loc_FeCO3_precipitation      = 0.0 
             !loc_bio_part(is_FeCO3_56Fe,k) = loc_r56Fe*loc_FeCO3_prec
          end if
          if (loc_FeCO3_precipitation > MIN(loc_CO3,loc_Fe2)) then
             loc_FeCO3_precipitation = MIN(loc_CO3,loc_Fe2)
          end if
          loc_bio_part(is_FeCO3,k) = loc_FeCO3_precipitation
          loc_bio_part(is_FeCO3_56Fe,k) = &
               & par_d56Fe_FeCO3_alpha*loc_R_56Fe/(1.0 + par_d56Fe_FeCO3_alpha*loc_R_56Fe)*loc_bio_part(is_FeCO3,k)

          if (sed_select(is_FeCO3_13C)) then
             ! re-calculate carbonate system isotopic properties
             if (ocn_select(io_DIC_13C)) then
                call sub_calc_carb_r13C(           &
                     & ocn(io_T,dum_i,dum_j,k),       &
                     & ocn(io_DIC,dum_i,dum_j,k),     &
                     & ocn(io_DIC_13C,dum_i,dum_j,k), &
                     & carb(:,dum_i,dum_j,k),         &
                     & carbisor(:,dum_i,dum_j,k)      &
                     & )
             end IF
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
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! OXIDIZE H2S
    ! -------------------------------------------------------- !
    ! look for some H2S and see if it can be instantaneously oxidized (using O2; if there is any!)
    ! H2S + 2O2 -> SO4 + 2H
    ! NOTE: loc_H2S_oxidation_const units are (M-1 yr-1)
    DO k=n_k,dum_k1,-1
       loc_O2 = ocn(io_O2,dum_i,dum_j,k)
       loc_H2S = ocn(io_H2S,dum_i,dum_j,k)
       if ((loc_O2 > const_real_nullsmall) .AND. (loc_H2S > 1e-11)) then
          ! calculate H2S oxidation, and cap value at H2S concentration if necessary
          SELECT CASE (opt_bio_remin_oxidize_H2StoSO4)
          CASE ('linear')
             ! NOTE: par_bio_remin_kH2StoSO4 units are (M-1 yr-1)
             loc_H2S_oxidation_const = par_bio_remin_kH2StoSO4
             loc_H2S_oxidation = min(dum_dtyr*loc_H2S_oxidation_const*loc_H2S*loc_O2,0.5*loc_O2)
          CASE ('OLD')
             ! change units of H2S oxidation constant from mM-2 hr-1 to M-2 yr-1
             ! and convert from O2 consumption units to H2S units (i.e., divide by 2)
             loc_H2S_oxidation_const = 0.5*const_oxidation_coeff_H2S/conv_hr_yr/(conv_mmol_mol)**2
             loc_H2S_oxidation = dum_dtyr*loc_H2S_oxidation_const*loc_H2S*loc_O2**2
          case ('complete')
             loc_H2S_oxidation = min(loc_H2S,0.5*loc_O2)
          CASE ('OLDDEFAULT')
             ! entirely spurious ... but here for completness
             loc_H2S_oxidation = min(0.5*loc_H2S,loc_O2)
          case default
             loc_H2S_oxidation = 0.0
          end select
          ! calculate isotopic ratio
          loc_r34S = ocn(io_H2S_34S,dum_i,dum_j,k)/ocn(io_H2S,dum_i,dum_j,k)
          if (loc_H2S_oxidation > loc_H2S) then
             ! complete H2S oxidation (no S fractionation)
             loc_H2S_oxidation = loc_H2S
             loc_bio_remin(io_H2S,k) = -loc_H2S
             loc_bio_remin(io_SO4,k) = loc_H2S
             loc_bio_remin(io_O2,k)  = -2.0*loc_H2S
             loc_bio_remin(io_ALK,k) = -2.0*loc_H2S
             loc_bio_remin(io_H2S_34S,k) = -loc_r34S*loc_H2S
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*loc_H2S
          else
             ! partial H2S oxidation (=> S isotope Rayleigh fractionation)
             loc_bio_remin(io_H2S,k) = -loc_H2S_oxidation
             loc_bio_remin(io_SO4,k) = loc_H2S_oxidation
             loc_bio_remin(io_O2,k)  = -2.0*loc_H2S_oxidation
             loc_bio_remin(io_ALK,k) = -2.0*loc_H2S_oxidation
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
  ! METHANE
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WATER COLUMN REMINERALIZATION OF METHANE - DEFAULT
  SUBROUTINE sub_calc_bio_remin_oxidize_CH4(dum_i,dum_j,dum_k1,dum_dtyr)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! local variables
    integer::l,io,k,id
    real::loc_potO2cap
    real::loc_CH4
    real::loc_r13C,loc_r14C
    real::loc_frac
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** OXIDIZE CH4 ***
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
    id = fun_find_str_i('redox_CH4toDIC_dCO2',string_diag_redox)
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
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! local variables
    integer::l,io,k,id
    real::loc_O2
    real::loc_CH4,loc_CO2
    real::loc_T,loc_TC,loc_kT
    real::loc_dG,loc_Ft,loc_Ft_min
    real::loc_MM,loc_AER
    real::loc_r13C,loc_r14C
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** OXIDIZE CH4 ***
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
          loc_AER = min(loc_AER,loc_CH4,0.5*loc_O2)
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
    id = fun_find_str_i('redox_CH4toDIC_dCO2',string_diag_redox)
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
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! local variables
    integer::l,io,k,id
    real::loc_O2
    real::loc_CH4,loc_DIC,loc_HCO3
    real::loc_SO4,loc_H2S
    real::loc_T,loc_TC,loc_kT
    real::loc_dG,loc_Ft,loc_Ft_min
    real::loc_MM,loc_AOM
    real::loc_r13C,loc_r14C
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** OXIDIZE CH4 ANAEROBICALLY WITH SO4 ***
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
          loc_AOM = min(loc_AOM,loc_CH4,loc_SO4)
          ! calculate isotopic ratios
          loc_r13C = ocn(io_CH4_13C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          loc_r14C = ocn(io_CH4_14C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
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
    id = fun_find_str_i('redox_CH4toDICaom_dCO2',string_diag_redox)
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
    real::loc_I_oxidation_const,loc_I_oxidation
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
    real::loc_IO3_reduction_const,loc_IO3_reduction
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

