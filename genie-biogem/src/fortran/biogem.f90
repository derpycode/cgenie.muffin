
! ******************************************************************************************************************************** !
! BIOGEM LOOP SUBROUTINE
subroutine biogem(        &
     & dum_dts,           &
     & dum_genie_clock,   &
     & dum_sfcatm1,       &
     & dum_sfxatm1,       &
     & dum_sfcocn1,       &
     & dum_sfxocn1,       &
     & dum_sfcsed1,       &
     & dum_sfxsed1,       &
     & dum_sfxsumrok1     &
     & )
  use omp_lib
  use gem_carbchem
  USE biogem_lib
  USE biogem_box
  USE biogem_data
  USE biogem_data_ascii
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  REAL,INTENT(IN)::dum_dts                                       ! biogem time-step length (seconds)
  integer(kind=8),INTENT(IN)::dum_genie_clock                    ! genie clock (ms since start) NOTE: 8-byte integer
  real,intent(in),dimension(n_atm,n_i,n_j)::dum_sfcatm1          ! atmosphere-interface tracer composition; ocn grid
  real,intent(inout),dimension(n_atm,n_i,n_j)::dum_sfxatm1       ! atmosphere -> surface flux; ocn grid
  real,intent(out),dimension(n_ocn,n_i,n_j)::dum_sfcocn1         ! sediment-interface ocean tracer composition; ocn grid
  real,intent(inout),dimension(n_ocn,n_i,n_j)::dum_sfxocn1       ! sediment -> ocean flux; ocn grid
  real,intent(in),dimension(n_sed,n_i,n_j)::dum_sfcsed1          ! sediment-interface sediment composition; ocn grid
  real,intent(inout),dimension(n_sed,n_i,n_j)::dum_sfxsed1       ! ocean -> sediment flux; ocn grid
  real,intent(inout),dimension(n_ocn,n_i,n_j)::dum_sfxsumrok1    ! coastal (weathering) -> ocean flux; ocn grid

  ! LOCAL VARIABLES
  INTEGER::i,j,k,l,io,ia,is,n,id                                 ! counting indices
  integer::lo,ls
  integer::loc_k1,loc_k                                          ! local topography
  integer::loc_i,loc_j,loc_tot_i                                 !
  integer::loc_n_k_tot                                           !
  real::loc_k_tot_icefree,loc_k_icefree                          !
!!$  integer::loc_m,loc_tot_m                                       ! tracer array conversion indices
  real::loc_dts,loc_dtyr,loc_t,loc_yr                            ! local time and time step etc.
  real::loc_rdts,loc_rdtyr                                       ! time reciprocals
  logical::loc_debug_ij                                          !
  logical,DIMENSION(n_ocn)::locio_mask                           !
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::locijk_ocn                  ! local ocean tracer array
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::locijk_focn                 ! local ocean tracer flux array
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::locijk_fpart                ! local particulate tracer flux array
  REAL,DIMENSION(n_atm,n_i,n_j)::locij_fatm                      ! local atmosphere tracer flux array
  REAL,DIMENSION(n_atm)::loc_datm_restore                        !
  REAL::loc_dust_Fe                     !
  real::loc_ocn_tot_M,loc_ocn_rtot_M                             ! ocean mass and reciprocal
  real::loc_ocn_tot_V,loc_ocn_rtot_V                             ! ocean volume  and reciprocal
  real::loc_fS,loc_fT                                            !
  REAL,DIMENSION(n_ocn)::loc_force_restore_ocn_tmod              ! relaxation modifier
  REAL,DIMENSION(n_atm)::loc_force_restore_atm_tmod              ! relaxation modifier
  REAL,DIMENSION(n_atm,n_i,n_j)::locij_focnatm                   ! local ocn->atm flux (atm tracer currency), units (mol yr-1)
  REAL,DIMENSION(n_sed,n_i,n_j)::locij_focnsed                   ! local ocn->sed change (sed tracer currency), units (mol)
  REAL,DIMENSION(n_ocn,n_i,n_j)::locij_fsedocn                   ! local sed->ocean change (ocn tracer currency), units (mol)
  REAL,DIMENSION(n_ocn,n_i,n_j)::locij_frokocn                   ! local rok->ocean change (ocn tracer currency), units (mol)
  REAL,DIMENSION(n_ocn)::loc_ocnsed_audit                        ! temporary ocn->sed auditing array, units (mol)
  REAL,DIMENSION(n_ocn)::loc_fracdecay_ocn                       ! local reduction factor for decaying ocanic tracers
  REAL,DIMENSION(n_sed)::loc_fracdecay_sed                       ! local reduction factor for decaying sediment tracers
  real::loc_det_tot,loc_det_sol_tot,loc_det_Fe_sol_sf            !
  REAL,DIMENSION(n_ocn)::loc_fweather_tot,loc_fseddis_tot        ! local total weathering flux, dissolution flux
  REAL,DIMENSION(n_ocn)::loc_fsedpres_tot,loc_fsedsettle_tot     !
  real::loc_frac,loc_standard                                    !
  real::loc_delta_actual,loc_delta_target,loc_delta_source       !
  real::loc_force_flux
  real::loc_force_sign
  real::loc_force_target
  real::loc_force_actual
  real::loc_force_actual_d13C
  real::loc_force_actual_d44Ca
  real::loc_r18O
  real::loc_remin
  real,dimension(1:n_l_ocn)::loc_vocn                            !
  real,dimension(n_l_ocn,n_l_sed)::loc_conv_ls_lo                !
  CHARACTER(len=31)::loc_string     !
!!!integer::nthreads,thread_id

  loc_debug_ij = .FALSE.

  ! *** RESET GLOBAL ARRAYS ***
  ! reset remin array
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     bio_remin(io,:,:,:) = 0.0
  end do
  ! redox remin diag
  if (ctrl_bio_remin_redox_save) diag_redox(:,:,:,:) = 0.0

  ! *** INITIALIZE LOCAL ARRAYS ***
  ! NOTE: only bother initializing for selected tracers
  DO l=3,n_l_atm
     ia = conv_iselected_ia(l)
     locij_fatm(:,:,:)    = 0.0
     locij_focnatm(:,:,:) = 0.0
  end do
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     locijk_ocn(io,:,:,:)  = 0.0
     locijk_focn(io,:,:,:) = 0.0
     locij_fsedocn(io,:,:) = 0.0
     locij_frokocn(io,:,:) = 0.0
  end do
  DO l=1,n_l_sed
     is = conv_iselected_is(l)
     locij_focnsed(is,:,:)  = 0.0
     locijk_fpart(is,:,:,:) = 0.0
  end do
  ! initialize local tracer arrays
  loc_conv_ls_lo(:,:)   = 0.0

  ! *** CALCULATE GEM TIME ***
  ! update model time
  ! NOTE: par_misc_t_runtime is counted DOWN in years
  !       => for BIOGEM, the 'end of the world' occurs when time reaches zero
  loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
  loc_dts   = dum_dts
  loc_rdts  = 1.0/loc_dts
  loc_dtyr  = loc_dts/conv_yr_s
  loc_rdtyr = 1.0/loc_dtyr
  ! calculate actual year (counting years Before Present or otherwise)
  IF (ctrl_misc_t_BP) THEN
     loc_yr = loc_t + par_misc_t_end
  ELSE
     loc_yr = par_misc_t_end - loc_t
  END IF

  ! *** CALCULATE LOCAL CONSTANTS ***
  ! total ocean mass and recpirocal
  loc_ocn_tot_M = sum(phys_ocn(ipo_M,:,:,:))
  loc_ocn_rtot_M = 1.0/loc_ocn_tot_M
  ! total ocean volume and recpirocal
  loc_ocn_tot_V = sum(phys_ocn(ipo_V,:,:,:))
  loc_ocn_rtot_V = 1.0/loc_ocn_tot_V
  ! fractional reduction factors for decaying isotopes
  loc_fracdecay_ocn(:) = EXP(-loc_dtyr*const_lambda_ocn(:))
  loc_fracdecay_sed(:) = EXP(-loc_dtyr*const_lambda_sed(:))

  ! *** START-UP REPORTING ***
  if (loc_t > (par_misc_t_runtime - const_real_nullsmall)) then
     print*,' '
     print*,'>>> START BioGeM run-time diagnostics >>>'
     par_misc_t_echo_header = .TRUE.
  END IF

  ! ****************************** !
  ! *** UPDATE BIOGEOCHEMISTRY *** !
  ! ****************************** !
  ! main BioGeM loop - all updating of ocean-atmosphere-sediment biogeochemistry occurs within this,
  ! with ocean biogeochemistry is updated on a grid point by grid point ((i,j) loop) basis
  ! NOTE: update conditional on end of biogeochemical simulation not having been reached
  ! NOTE: tracer loops follow the GOLDSTEIn notation, with indices converted to their BioGeM equivalents
  if_go: IF (par_misc_t_go) THEN

     IF (ctrl_debug_lvl1) print*, '******************************'
     IF (ctrl_debug_lvl1) print*, '*** UPDATE BIOGEOCHEMISTRY ***'
     IF (ctrl_debug_lvl1) print*, '******************************'

     ! *** UPDATE RESTORING FORCING TIME-CONSTANTS ***
     IF (ctrl_debug_lvl1) print*, '*** UPDATE RESTORING FORCING TIME-CONSTANTS ***'
     ! recalculate time-varying restoring and flux forcings of the system & fractional relaxation
     ! NOTE: updating of restoring time-series position calculated seperately (sub: biogem_forcing)
     ! ATMOSPHERIC TRACERS (applied at the ocean-atmospere interface)
     DO l=3,n_l_atm
        ia = conv_iselected_ia(l)
        IF (force_restore_atm_select(ia)) THEN
           loc_force_restore_atm_tmod(ia) = 1.0 - EXP(-loc_dtyr/force_restore_atm_tconst(ia))
        END IF
     END DO
     ! OCEAN TRACERS
     DO l=1,n_l_ocn
        io = conv_iselected_io(l)
        IF (force_restore_ocn_select(io)) THEN
           loc_force_restore_ocn_tmod(io) = 1.0 - EXP(-loc_dtyr/force_restore_ocn_tconst(io))
        END IF
     END DO

     ! *** UPDATE DERIVED FORCING DATA ***
     loc_n_k_tot               = 0
     loc_k_tot_icefree         = 0.0
     loc_det_tot               = 0.0
     loc_det_sol_tot           = 0.0
     loc_fweather_tot(:)       = 0.0
     loc_fseddis_tot(:)        = 0.0
     loc_fsedpres_tot(:)       = 0.0
     loc_fsedsettle_tot(:)     = 0.0
     loc_force_actual          = 0.0
     loc_force_actual_d13C     = 0.0
     loc_force_actual_d44Ca    = 0.0
     !
     DO i=1,n_i
        DO j=1,n_j
           loc_k1 = goldstein_k1(i,j)
           IF (n_k >= loc_k1) THEN
              ! number of grid points(!)
              loc_n_k_tot = loc_n_k_tot + 1
              loc_k_tot_icefree = loc_k_tot_icefree + (1.0 - phys_ocnatm(ipoa_seaice,i,j))
           end IF
        end DO
     end DO
     ! DO
     DO i=1,n_i
        DO j=1,n_j
           loc_k1 = goldstein_k1(i,j)
           IF (n_k >= loc_k1) THEN
              loc_k_icefree = (1.0 - phys_ocnatm(ipoa_seaice,i,j))
              ! Aeolian solubilities
              ! NOTE: the flux used to derive the solubility, should be in units of  mass per unit area (or volume) (time can be ignored)
              !       on an equal area grid, it does not matter and hence old code was OK
              !       (old code was: phys_ocnatm(ipoa_solFe,i,j) = force_flux_sed(is_det,i,j)**(par_det_Fe_sol_exp - 1.0))
              if (force_flux_sed(is_det,i,j) > const_real_nullsmall) then
                 phys_ocnatm(ipoa_solFe,i,j) = (phys_ocn(ipo_rM,i,j,n_k)*force_flux_sed(is_det,i,j))**(par_det_Fe_sol_exp - 1.0)
                 loc_det_tot = loc_det_tot + force_flux_sed(is_det,i,j)
                 loc_det_sol_tot = loc_det_sol_tot + phys_ocnatm(ipoa_solFe,i,j)*force_flux_sed(is_det,i,j)
              else
                 phys_ocnatm(ipoa_solFe,i,j) = 0.0
              end if
              ! total global weathering (mol per time step)
              DO l=3,n_l_ocn
                 io = conv_iselected_io(l)
                 loc_fweather_tot(io) = loc_fweather_tot(io) + dum_sfxsumrok1(io,i,j)
              end DO
              ! sedimentary dissolution (mol per time step)
              DO l=3,n_l_ocn
                 io = conv_iselected_io(l)
                 loc_fseddis_tot(io) = loc_fseddis_tot(io) + loc_dts*phys_ocn(ipo_A,i,j,loc_k1)*dum_sfxocn1(io,i,j)
              end DO
              ! settling flux (mol per time step)
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 loc_tot_i = conv_sed_ocn_i(0,is)
                 do loc_i=1,loc_tot_i
                    io = conv_sed_ocn_i(loc_i,is)
                    loc_fsedsettle_tot(io) = loc_fsedsettle_tot(io) + conv_sed_ocn(io,is)*bio_settle(is,i,j,loc_k1)
                 end do
              end do
              ! set current mean ocean surface carbonate chemsitry
              ! NOTE: the 'red' tracer is used to set a time history of ocean surface pH
              !       the 'blue' tracer is used to set a time history of saturation state
              IF (force_restore_ocn_select(io_ALK) .AND. force_flux_ocn_select(io_ALK)) THEN
                 IF (force_restore_ocn_select(io_colr)) THEN
                    loc_force_actual = loc_force_actual + loc_k_icefree*carb(ic_H,i,j,n_k)/loc_k_tot_icefree
                 else
                    if (ctrl_force_ohmega_calcite) then
                       loc_force_actual = loc_force_actual + loc_k_icefree*carb(ic_ohm_cal,i,j,n_k)/loc_k_tot_icefree
                    else
                       loc_force_actual = loc_force_actual + loc_k_icefree*carb(ic_ohm_arg,i,j,n_k)/loc_k_tot_icefree
                    end if
                 end if
              elseif (force_flux_atm_select(ia_pCO2) .AND. force_flux_atm_select(ia_pCO2_13C)) THEN
                 IF (force_restore_ocn_select(io_colr)) THEN
                    loc_force_actual = loc_force_actual + loc_k_icefree*carb(ic_H,i,j,n_k)/loc_k_tot_icefree
                 end if
              end IF
              ! calc mean DIC (or DOC) d13C
              IF ( &
                   & (force_restore_ocn_select(io_DIC_13C) .OR. force_restore_ocn_select(io_DOM_C_13C)) &
                   &  .AND. &
                   & (force_flux_ocn_select(io_DIC_13C) .OR. force_flux_atm_select(ia_pCO2_13C)) &
                   & ) THEN
                 if (force_restore_ocn_select(io_DIC_13C)) then
                    loc_standard = const_standards(ocn_type(io_DIC_13C))
                    loc_force_actual_d13C = loc_force_actual_d13C + loc_k_icefree*&
                         & fun_calc_isotope_delta(ocn(io_DIC,i,j,n_k),ocn(io_DIC_13C,i,j,n_k),loc_standard,.FALSE.,const_real_null)/&
                         & loc_k_tot_icefree
                 elseif (force_restore_ocn_select(io_DOM_C_13C)) then
                    loc_standard = const_standards(ocn_type(io_DOM_C_13C))
                    loc_force_actual_d13C = loc_force_actual_d13C + loc_k_icefree*&
                         & fun_calc_isotope_delta(ocn(io_DOM_C,i,j,n_k),ocn(io_DOM_C_13C,i,j,n_k),loc_standard,.FALSE.,const_real_null)/&
                         & loc_k_tot_icefree
                 end if
              end IF
              ! calc mean Ca d44Ca
              IF ( &
                   & (force_restore_ocn_select(io_Ca) .AND. force_restore_ocn_select(io_Ca_44Ca)) &
                   &  .AND. &
                   & (force_flux_ocn_select(io_Ca) .AND. force_flux_ocn_select(io_Ca_44Ca)) &
                   & ) THEN
                 loc_standard = const_standards(ocn_type(io_Ca_44Ca))
                 loc_force_actual_d44Ca = loc_force_actual_d44Ca + loc_k_icefree*&
                      & fun_calc_isotope_delta(ocn(io_Ca,i,j,n_k),ocn(io_Ca_44Ca,i,j,n_k),loc_standard,.FALSE.,const_real_null)/&
                      & loc_k_tot_icefree
              end IF
           end IF
        end DO
     end DO
     ! calc mean O2 d18O
     IF (ocn_select(io_O2_18O)) THEN
        loc_r18O = SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_O2_18O,:,:,:))*loc_ocn_rtot_M / &
             & ( SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_O2,:,:,:))*loc_ocn_rtot_M )
     end IF
     ! sedimentary preservation (mol per time step)
     DO l=3,n_l_ocn
        io = conv_iselected_io(l)
        loc_fsedpres_tot(io) = loc_fsedsettle_tot(io) - loc_fseddis_tot(io)
     end DO
     ! re-scale Aeolian solubilities
     ! => calculate the scale factor required to match the requested mean global solubility (<par_det_Fe_sol>)
     if ((loc_det_tot > const_real_nullsmall) .AND. (par_det_Fe_sol > const_real_nullsmall)) then
        loc_det_Fe_sol_sf = par_det_Fe_sol/(loc_det_sol_tot/loc_det_tot)
     else
        loc_det_Fe_sol_sf = 0.0
     end if
     phys_ocnatm(ipoa_solFe,:,:) = loc_det_Fe_sol_sf*phys_ocnatm(ipoa_solFe,:,:)

     ! ****************************************************************************************************************************
     ! ****************************************************************************************************************************
     ! ****************************************************************************************************************************

     ! *** ORIGINAL CODE FRAGMENT *************************************************************************************************
     DO i=1,n_i
        DO j=1,n_j
           loc_k1 = goldstein_k1(i,j)
           IF (n_k >= loc_k1) THEN

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** AGE TRACERS ***'
              ! *** AGE TRACERS ***
              ! NOTE: red has unit concentraton input to the surface per year
              if (ctrl_force_ocn_age) then
                 bio_remin(io_colr,i,j,n_k) = bio_remin(io_colr,i,j,n_k) + &
                      & 1.0 - ocn(io_colr,i,j,n_k)
                 bio_remin(io_colb,i,j,n_k) = bio_remin(io_colb,i,j,n_k) + &
                      & loc_t*1.0 - ocn(io_colb,i,j,n_k)
              end if

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** DECAY RADIOACTIVE TRACERS ***'
              ! *** DECAY RADIOACTIVE TRACERS ***
              ! NOTE: decay of isotopes in atmosphere is implemented in ATCHEM
              ! NOTE: the decay of ocean tracers is implemented as a (-ve.) flux forcing
              !       because of ensuring mass conservation in the salinity-normalization tracer advection scheme later ...
              !       and to do this, units of mol kg-1 (tracer concentration) must be converted to mol yr-1 (flux)
              ! NOTE: particulate tracers can be adjusted directly
              ! OCEAN TRACERS
              DO l=3,n_l_ocn
                 io = conv_iselected_io(l)
                 IF (abs(const_lambda_ocn(io)).gt.const_real_nullsmall) THEN
                    DO k=loc_k1,n_k
                       locijk_focn(io,i,j,k) = locijk_focn(io,i,j,k) - &
                            & phys_ocn(ipo_M,i,j,k)*(1.0 - loc_fracdecay_ocn(io))*ocn(io,i,j,k)/loc_dtyr
                    END DO
                 end IF
              end do
              ! SEDIMENT TRACERS
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 IF (abs(const_lambda_sed(is)).gt.const_real_nullsmall) THEN
                    DO k=loc_k1,n_k
                       bio_part(is,i,j,k) = loc_fracdecay_sed(is)*bio_part(is,i,j,k)
                    END DO
                 end if
              END DO

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** AEROSOL DISSOLUTION INPUT ***'
              ! *** AEROSOL DISSOLUTION INPUT ***
              ! dissolved from Fe from dust
              ! NOTE: par_det_frac_Fe is the mass fraction of Fe in dust
              ! NOTE: first convert detrital concentration (mol kg-1) back to mass,
              !       and then from mass of dust to mass of iron in the dust ...
              ! total aeolian Fe input (mol yr-1)
              phys_ocnatm(ipoa_totFe,i,j) = &
                   & conv_Fe_g_mol*par_det_Fe_frac*conv_det_mol_g*phys_ocn(ipo_M,i,j,n_k)*bio_part(is_det,i,j,n_k)/loc_dtyr
              ! dissolved Fe input (mol yr-1)
              loc_dust_Fe = phys_ocnatm(ipoa_solFe,i,j)*phys_ocnatm(ipoa_totFe,i,j)
              ! update ocean flux array
              if (ocn_select(io_Fe)) then
                 locijk_focn(io_Fe,i,j,n_k) = locijk_focn(io_Fe,i,j,n_k) + loc_dust_Fe
              elseif (ocn_select(io_TDFe)) then
                 locijk_focn(io_TDFe,i,j,n_k) = locijk_focn(io_TDFe,i,j,n_k) + loc_dust_Fe
              else
                 !
              end if
              ! ### ADD CODE FOR ADDITIONAL AEOLIAN DISSOLUTION INPUTS ########################################################### !
              !
              ! ################################################################################################################## !

              ! *** SEDIMENT DISSOLUTION INPUT ***
              ! modify remineralization array according to dissolution input from sediments
              ! NOTE: <dum_sfxocn1> in units of (mol m-2 s-1) - needs to be converted to (mol per time step)
              ! NOTE: <dum_sfxsumrok1> in units of (mol) (per time-step)
              ! NOTE: if the model is configured as a 'closed' system, test for whether SEDGEM is coupled
              !       if so  => set a weathering flux equal to the preservation flux
              !       if not => set dissolution flux by scaling weathering flux to match preservation flux
              ! NOTE: if no weathering flux set (<loc_fweather_tot> = 0.0) do not attempt to balance burial (e.g. may be Fe)
              ! NOTE: if there is no preservation (or negative preservation, i.e. erosion), set weathering to zero
              ! NOTE: scale returned scavenged Fe according to value of par_scav_fremin
              ! NOTE: allow for (optional) return of Fe incorporated into organic matter (is_POFe)
              ! NOTE: make special case for 'scavenged' (POM-bound) S -- return to water column in a non-SEDGEM closed system
              if (ctrl_force_sed_closedsystem) then
                 If (flag_sedgem) then
                    ! set weathering flux equal to sediment preservation flux (globally averaged)
                    DO l=3,n_l_ocn
                       io = conv_iselected_io(l)
                       locij_fsedocn(io,i,j) = locij_fsedocn(io,i,j) + &
                            & loc_dts*phys_ocn(ipo_A,i,j,loc_k1)*dum_sfxocn1(io,i,j)
                       if (loc_fweather_tot(io) > const_real_nullsmall) &
                            & dum_sfxsumrok1(io,i,j) = (loc_fsedpres_tot(io)/loc_fweather_tot(io))*dum_sfxsumrok1(io,i,j)
                       if (abs(loc_fsedpres_tot(io)) < const_real_nullsmall) &
                            & dum_sfxsumrok1(io,i,j) = 0.0
                    end do
                 else
                    ! set dissolution flux equal to rain flux
                    ! NOTE: force return of S from POM-S in a closed system, despite it being a 'scavenged' type
                    DO l=1,n_l_ocn
                       loc_vocn(l) = ocn(l2io(l),i,j,loc_k1)
                    end DO
                    call sub_box_remin_redfield(loc_vocn,loc_conv_ls_lo(:,:))
                    DO ls=1,n_l_sed
                       loc_tot_i = conv_ls_lo_i(0,ls)
                       do loc_i=1,loc_tot_i
                          lo = conv_ls_lo_i(loc_i,ls)
                          if (lo > 0) then
                             if (sed_type(l2is(ls)) == par_sed_type_scavenged) then
                                if ((l2is(ls) == is_POM_S) .OR. (sed_dep(l2is(ls)) == is_POM_S)) then
                                   loc_remin = loc_conv_ls_lo(lo,ls)*bio_settle(l2is(ls),i,j,loc_k1)
                                else
                                   loc_remin = par_scav_fremin*loc_conv_ls_lo(lo,ls)*bio_settle(l2is(ls),i,j,loc_k1)
                                end if
                             else
                                loc_remin = loc_conv_ls_lo(lo,ls)*bio_settle(l2is(ls),i,j,loc_k1)
                             end if
                             locij_fsedocn(l2io(lo),i,j) = locij_fsedocn(l2io(lo),i,j) + loc_remin
                             if (ctrl_bio_remin_redox_save) then
                                loc_string = 'reminP_'//trim(string_sed(l2is(ls)))//'_d'//trim(string_ocn(l2io(lo)))
                                id = fun_find_str_i(trim(loc_string),string_diag_redox)
                                diag_redox(id,i,j,loc_k1) = diag_redox(id,i,j,loc_k1) + phys_ocn(ipo_rM,i,j,loc_k1)*loc_remin
                             end if
                          end if
                       end do
                    end DO
                 end If
                 ! prevent return of dissolved Fe?
                 if (ctrl_bio_NO_fsedFe) locij_fsedocn(io_Fe,i,j) = 0.0
              else
                 ! set dissolution fluxes
                 DO l=3,n_l_ocn
                    io = conv_iselected_io(l)
                    locij_fsedocn(io,i,j) = locij_fsedocn(io,i,j) + &
                         & loc_dts*phys_ocn(ipo_A,i,j,loc_k1)*dum_sfxocn1(io,i,j)
                 end do
                 if (ctrl_force_sed_closed_P) then
                    ! special case of partial closure -- calculate theoretical PO4 remin flux required for closure
                    ! set weathering equal to imbalance in P return
                    DO l=1,n_l_ocn
                       loc_vocn(l) = ocn(l2io(l),i,j,loc_k1)
                    end DO
                    call sub_box_remin_redfield(loc_vocn,loc_conv_ls_lo(:,:))
                    ls = is2l(is_POP)
                    loc_tot_i = conv_ls_lo_i(0,ls)
                    do loc_i=1,loc_tot_i
                       lo = conv_ls_lo_i(loc_i,ls)
                       if (lo == io2l(io_PO4)) then
                          loc_remin = loc_conv_ls_lo(lo,ls)*bio_settle(l2is(ls),i,j,loc_k1)
                          dum_sfxsumrok1(io_PO4,i,j) = dum_sfxsumrok1(io_PO4,i,j) + &
                               &  (loc_remin - locij_fsedocn(io_PO4,i,j))
                          if (ctrl_bio_red_ALKwithPOC) then
                             ! do nothing -- ALK with POC
                          else
                             dum_sfxsumrok1(io_ALK,i,j) = dum_sfxsumrok1(io_ALK,i,j) + &
                                  &  conv_sed_ocn(io_ALK,is_POP)*(loc_remin - locij_fsedocn(io_PO4,i,j))
                          end if
                       end if
                    end do
                 end if
              end if
              DO l=3,n_l_ocn
                 io = conv_iselected_io(l)
                 bio_remin(io,i,j,loc_k1) = bio_remin(io,i,j,loc_k1) + &
                      & phys_ocn(ipo_rM,i,j,loc_k1)*locij_fsedocn(io,i,j)
              end do

              ! *** ALT (OLD) CODE ************************************************************************************************

              ! *** WATER COLUMN REMINERALIZATION - DISSOLVED ORGANIC MATTER ***
              !call sub_calc_bio_remin_allDOM_tmp(i,j,loc_k1,loc_dtyr)

              ! *** WATER COLUMN REMINERALIZATION - PARTICULATE MATTER ***
              !call sub_calc_bio_remin(i,j,loc_k1,loc_dtyr)

              ! *******************************************************************************************************************

           end if
        end do
     end do
     ! ****************************************************************************************************************************

     !     !$omp parallel private(nthreads,thread_id)
     !     call omp_set_num_threads(4)

     !     nthreads = omp_get_max_threads()
     !     print*,'* # MAX THREADS: ',nthreads
     !     nthreads = omp_get_num_threads()
     !     print*,'* # ACTUAL THREADS: ',nthreads

     !    !$OMP SECTIONS
     !    !$OMP SECTION

     !  call sub_wasteCPUcycles1(ocn,'1',1)

     !  !$OMP SECTION

     !  call sub_wasteCPUcycles2(ocn,'2',1)

     !  !$OMP END SECTIONS
     !  !$omp end parallel

     ! *** TMP: make vectorized copy of <ocn> *************************************************************************************
     vocn(:) = fun_lib_conv_ocnTOvocn(ocn(:,:,:,:))
     vbio_part(:) = fun_lib_conv_sedTOvsed(bio_part(:,:,:,:))
     ! ****************************************************************************************************************************

     ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
     ! *** (v) GRID PT LOOP START *** !
     ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !

     !  ! Fork a team of threads giving them their own copies of variables
     !  ! *NB* No space between '!' and '$omp' below...
     !  !$omp parallel private(nthreads, tid)
     !  ! Obtain thread number
     !  tid = OMP_get_thread_num()

     do n=1,n_vocn

        ! *** WATER COLUMN REMINERALIZATION - DISSOLVED ORGANIC MATTER ***
        IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
             & '*** WATER COLUMN REMINERALIZATION - DISSOLVED ORGANIC MATTER ***'
        call sub_box_remin_DOM(vocn(n),vbio_remin(n),loc_dtyr)

        ! *** WATER COLUMN REMINERALIZATION - PARTICULATE MATTER ***
        IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
             & '*** WATER COLUMN REMINERALIZATION - PARTICULATE MATTER ***'
        call sub_box_remin_part(loc_dtyr,vocn(n),vphys_ocn(n),vbio_part(n),vbio_remin(n))

     end do

     ! [temp code in retaining primary use of <bio_remin>] ************************************************************************
     bio_part(:,:,:,:) = fun_lib_conv_vsedTOsed(vbio_part(:))
     bio_remin(:,:,:,:) = bio_remin(:,:,:,:) + fun_lib_conv_vocnTOocn(vbio_remin(:))
     ! ****************************************************************************************************************************

     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<< !
     ! *** (v) GRID PT LOOP END *** !
     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<< !

     !  ! All threads join master thread and disband
     !  !$omp end parallel

     ! ****************************************************************************************************************************
     ! ****************************************************************************************************************************
     ! ****************************************************************************************************************************

     ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
     ! *** (i,j) GRID PT LOOP START *** !
     ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !

     n = 0
     block_iloop: DO i=1,n_i
        block_jloop: DO j=1,n_j

           ! *** INITIALIZE LOOP VARIABLES ***
           ! set local depth loop limit
           loc_k1 = goldstein_k1(i,j)
           ! set local debug condition
           if (opt_misc(iopt_misc_debugij)) loc_debug_ij = .TRUE.
           if ((i == par_misc_debug_i) .AND. (j == par_misc_debug_j)) then
              loc_debug_ij = .TRUE.
           else
              loc_debug_ij = .FALSE.
           end if

           IF (ctrl_debug_lvl1 .AND. loc_debug_ij) &
                & print*, '*** >>> START (i,j) GRID POINT'

           ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
           ! *** WET GRID PT CONDITIONALITY START *** !
           ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !

           IF (n_k >= loc_k1) THEN
              n = n + 1

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** UPDATE CARBONATE CHEMSITRY ***'
              IF (opt_select(iopt_select_carbchem)) THEN
                 ! set local k loop limit
                 if (ctrl_carbchemupdate_full) then
                    loc_k = loc_k1
                 else
                    loc_k = n_k
                 end if
                 ! loop through k grid
                 DO k=loc_k,n_k
                    ! calculate carbonate dissociation constants
                    CALL sub_calc_carbconst(         &
                         & phys_ocn(ipo_Dmid,i,j,k), &
                         & ocn(io_T,i,j,k),          &
                         & ocn(io_S,i,j,k),          &
                         & carbconst(:,i,j,k)        &
                         & )
                    ! adjust carbonate constants
                    if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
                       call sub_adj_carbconst(   &
                            & ocn(io_Ca,i,j,k),  &
                            & ocn(io_Mg,i,j,k),  &
                            & carbconst(:,i,j,k) &
                            & )
                    end if
                    ! re-estimate Ca and borate concentrations from salinity (if not selected and therefore explicitly treated)
                    IF (.NOT. ocn_select(io_Ca))  ocn(io_Ca,i,j,k)  = fun_calc_Ca(ocn(io_S,i,j,k))
                    IF (.NOT. ocn_select(io_B))   ocn(io_B,i,j,k)   = fun_calc_Btot(ocn(io_S,i,j,k))
                    IF (.NOT. ocn_select(io_SO4)) ocn(io_SO4,i,j,k) = fun_calc_SO4tot(ocn(io_S,i,j,k))
                    IF (.NOT. ocn_select(io_F))   ocn(io_F,i,j,k)   = fun_calc_Ftot(ocn(io_S,i,j,k))
                    ! re-calculate surface ocean carbonate chemistry
                    CALL sub_calc_carb(        &
                         & ocn(io_DIC,i,j,k),  &
                         & ocn(io_ALK,i,j,k),  &
                         & ocn(io_Ca,i,j,k),   &
                         & ocn(io_PO4,i,j,k),  &
                         & ocn(io_SiO2,i,j,k), &
                         & ocn(io_B,i,j,k),    &
                         & ocn(io_SO4,i,j,k),  &
                         & ocn(io_F,i,j,k),    &
                         & ocn(io_H2S,i,j,k),  &
                         & ocn(io_NH4,i,j,k),  &
                         & carbconst(:,i,j,k), &
                         & carb(:,i,j,k),      &
                         & carbalk(:,i,j,k)    &
                         & )
                    ! re-calculate carbonate system isotopic properties
                    if (ocn_select(io_DIC_13C)) then
                       call sub_calc_carb_r13C(      &
                            & ocn(io_T,i,j,k),       &
                            & ocn(io_DIC,i,j,k),     &
                            & ocn(io_DIC_13C,i,j,k), &
                            & carb(:,i,j,k),         &
                            & carbisor(:,i,j,k)      &
                            & )
                    end IF
                    if (ocn_select(io_DIC_14C)) then
                       call sub_calc_carb_r14C(      &
                            & ocn(io_T,i,j,k),       &
                            & ocn(io_DIC,i,j,k),     &
                            & ocn(io_DIC_14C,i,j,k), &
                            & carb(:,i,j,k),         &
                            & carbisor(:,i,j,k)      &
                            & )
                    end IF
                 end do
                 ! estimate Revelle factor
                 ! NOTE: surface only property
                 CALL sub_calc_carb_RF0(      &
                      & ocn(io_DIC,i,j,n_k),  &
                      & ocn(io_ALK,i,j,n_k),  &
                      & ocn(io_PO4,i,j,n_k),  &
                      & ocn(io_SiO2,i,j,n_k), &
                      & ocn(io_B,i,j,n_k),    &
                      & ocn(io_SO4,i,j,n_k),  &
                      & ocn(io_F,i,j,n_k),    &
                      & ocn(io_H2S,i,j,n_k),  &
                      & ocn(io_NH4,i,j,n_k),  &
                      & carbconst(:,i,j,n_k), &
                      & carb(:,i,j,n_k)       &
                      & )
              end if

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** BRINE REJECTION ***'
              ! *** BRINE REJECTION ***
              ! calculate sea-ice brine rejection
              if (par_misc_brinerejection_frac > const_real_nullsmall) then
                 call sub_calc_misc_brinerejection(loc_dtyr,i,j,loc_fT,loc_fS)
                 locijk_focn(io_T,i,j,n_k)    = locijk_focn(io_T,i,j,n_k)    - loc_fT
                 locijk_focn(io_T,i,j,loc_k1) = locijk_focn(io_T,i,j,loc_k1) + loc_fT
                 locijk_focn(io_S,i,j,n_k)    = locijk_focn(io_S,i,j,n_k)    - loc_fS
                 locijk_focn(io_S,i,j,loc_k1) = locijk_focn(io_S,i,j,loc_k1) + loc_fS
              end if

              IF (opt_misc(iopt_misc_debugij)) print*,'(i,j); ',i,j

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** UPDATE AIR-SEA INTERFACE AQUEOUS SYSTEM ***'
              ! *** UPDATE AIR-SEA INTERFACE AQUEOUS SYSTEM ***
              ! re-calculate gas solubility coefficients
              call sub_calc_solconst(i,j)
              ! re-calculate piston velocities
              call sub_calc_pv(i,j)

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** CALCULATE RESTORING BOUNDARY CONDITIONS ***'
              ! *** CALCULATE RESTORING BOUNDARY CONDITIONS ***
              ! calculate tracer changes necessary to meet any imposed atmospheric restoring boundary conditions
              ! ATMOSPHERIC TRACERS
              ! NOTE: divide restoring atmospheric flux by <ocn_dt> to produce a value in units of mol yr-1,
              !       (since in updating the atmosphere subsequently, the flux is multiplied by the timestep
              !       to get the required increment)
              ! NOTE: use a crude conversion factor for partial pressure (atm) -> mol, BUT
              !       take into account that only 1/(imax x jmax) of the entire atmosphere is being forced under each grid point
              ! NOTE: do not force restore if time is outside the specified restoring interval
              !       (indicated by the time indices being identical)
              ! NOTE: do not calculate a restoring flux if there is also a flux forcing [special case]
              DO l=3,n_l_atm
                 ia = conv_iselected_ia(l)
                 IF (force_restore_atm_select(ia) .AND. (.NOT. force_flux_atm_select(ia))) THEN
                    IF (force_restore_atm_sig_i(ia,1) /= force_restore_atm_sig_i(ia,2)) THEN
                       ! catch missing restoring data (non-isotope tracer values < 0.0) => force restoring flux to zero
                       ! also catch 'null' isotopic values
                       SELECT CASE (atm_type(ia))
                       CASE (1)
                          if (force_restore_atm(ia,i,j) < 0.0) force_restore_atm(ia,i,j) = dum_sfcatm1(ia,i,j)
                       case default
                          if (force_restore_atm(ia,i,j) <= const_real_null) force_restore_atm(ia,i,j) = dum_sfcatm1(ia,i,j)
                       end select
                       ! set restoring flux
                       loc_datm_restore(ia) = (force_restore_atm(ia,i,j) - dum_sfcatm1(ia,i,j))*loc_force_restore_atm_tmod(ia)
                       locij_fatm(ia,i,j) = (1.0/real(n_i*n_j))*conv_atm_mol*loc_datm_restore(ia)*loc_rdtyr
                       ! record (atmopsheric) flux (diagnosed from restoring) forcing
                       diag_forcing(ia,i,j) = locij_fatm(ia,i,j)
                    END IF
                 end IF
              END DO
              ! OCEAN TRACERS #1
              ! hack to overwrite surface ocean restoring field and equilibrate with atmosphere if requested
              ! => this by-passes the need for explicit air-sea gas exchange
              ! NOTE: restoring time-constant (set in the tracer config file) will still apply
              ! NOTE: atmospheric inventory is not adjusted
              ! NOTE: assume one-to-one mapping between atm and ocn tracers
              !       => array need not be looped through to fine io corresponding to any chosen ia
              ! NOTE: if no correspondence, the NULL index of the ocean tracer array provides the dustbin
              DO l=3,n_l_atm
                 ia = conv_iselected_ia(l)
                 IF (atm_type(ia) == 1) THEN
                    if (ocnatm_airsea_eqm(ia)) then
                       loc_tot_i = conv_atm_ocn_i(0,ia)
                       do loc_i=1,loc_tot_i
                          io = conv_atm_ocn_i(loc_i,ia)
                          force_restore_locn(io2l(io),i,j,n_k) = ocnatm_airsea_solconst(ia,i,j)*dum_sfcatm1(ia,i,j)
                          ! ### INSERT CODE TO DEAL WITH RELATED ISOTOPE COMPOSITION BOUNDARY CONDITIONS ######################### !
                          !
                          ! ###################################################################################################### !
                       end do
                    end if
                 end IF
              end DO
              ! OCEAN TRACERS #2
              ! calculate tracer changes necessary to meet any imposed oceanic restoring boundary conditions
              ! NOTE: fractional sea ice-covered area is taken into account and used to modify restoring flux at the surface
              ! NOTE: do not force restore if time is outside the specified restoring interval
              !       (indicated by the time indices being identical)
              ! NOTE: do not calculate a restoring flux if there is also a flux forcing [special case]
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 IF (force_restore_ocn_select(io) .AND. (.NOT. force_flux_ocn_select(io))) THEN
                    IF (force_restore_ocn_sig_i(io,1) /= force_restore_ocn_sig_i(io,2)) THEN
                       DO k=force_restore_ocn_k1(io,i,j),n_k
                          ! catch missing restoring data (non-isotope tracer values < 0.0) => force restoring flux to zero
                          ! also catch 'null' isotopic values
                          SELECT CASE (ocn_type(io))
                          CASE (0,1)
                             if (force_restore_locn(l,i,j,k) < -const_real_nullsmall) force_restore_locn(l,i,j,k) = ocn(io,i,j,k)
                          CASE (n_itype_min:n_itype_max)
                             if ( &
                                  & abs(force_restore_locn(io2l(ocn_dep(io)),i,j,k) - ocn(ocn_dep(io),i,j,k)) &
                                  & < &
                                  & const_real_nullsmall) &
                                  & then
                                force_restore_locn(l,i,j,k) = ocn(io,i,j,k)
                             end if
                          case default
                          end select
                          ! set restoring flux
                          force_restore_docn_nuts(io) = (force_restore_locn(l,i,j,k) - ocn(io,i,j,k))*loc_force_restore_ocn_tmod(io)
                          locijk_focn(io,i,j,k) = force_restore_docn_nuts(io)*phys_ocn(ipo_M,i,j,k)*loc_rdtyr
                       END DO
                       ! modify surface layer restoring forcing according to fractional sea-ice cover
                       locijk_focn(io,i,j,n_k) = (1.0 - phys_ocnatm(ipoa_seaice,i,j))*locijk_focn(io,i,j,n_k)
                    end IF
                 END IF
              END DO
              ! OCEAN TRACERS #3
              ! if a nutrient-restoring biological productivity 'biological' option has been set,
              ! => do not directly change nutrient concentrations via <locijk_focn>
              !    (this will instead be done in sub_calc_bio_uptake)
              SELECT CASE (par_bio_prodopt)
              CASE ('1N1T_PO4restore','1N1T_PO4restoreLL')
                 locijk_focn(io_PO4,i,j,n_k) = 0.0
              CASE DEFAULT
                 ! ### INSERT CODE TO INCLUDE ADDITIONAL SPECIAL CASES OF OCEAN TRACER RESTORING FORCING ######################### !
                 !
                 ! ############################################################################################################### !
              end SELECT

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** EXTERNAL FLUX FORCING ***'
              ! *** EXTERNAL FLUX FORCING ***
              ! calculate value of applied flux forcings
              ! NOTE: <force_flux*> in units of (mol yr-1)
              ! ATMOSPHERIC TRACERS (applied at the ocean-atmosphere interface)
              DO l=3,n_l_atm
                 ia = conv_iselected_ia(l)
                 IF (force_flux_atm_select(ia)) THEN
                    locij_fatm(ia,i,j) = locij_fatm(ia,i,j) + force_flux_atm(ia,i,j)
                    ! record (atmopsheric) flux forcing
                    diag_forcing(ia,i,j) = force_flux_atm(ia,i,j)
                 END IF
              END DO
              ! OCEAN TRACERS
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 IF (force_flux_ocn_select(io)) THEN
                    DO k=loc_k1,n_k
                       locijk_focn(io,i,j,k) = locijk_focn(io,i,j,k) + force_flux_locn(l,i,j,k)
                    END DO
                 END IF
              END DO
              ! test for, and apply, additional salinity flux forcing
              if (.NOT. ctrl_force_ocn_age) then
                 IF (force_flux_ocn_select(io_S) .AND. force_flux_ocn_select(io_colb)) THEN
                    DO k=loc_k1,n_k
                       locijk_focn(io_S,i,j,k)    = locijk_focn(io_S,i,j,k) + locijk_focn(io_colb,i,j,k)
                       locijk_focn(io_colb,i,j,k) = 0.0
                    END DO
                 end if
              end if
              ! SEDIMENT TRACERS #1
              ! NOTE: currently, fluxes are valid at the ocean surface only
              ! NOTE: addition is made directly to particulate sedimentary tracer array (scaled by time-step and cell mass)
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 IF (force_flux_sed_select(is)) THEN
                    locijk_fpart(is,i,j,n_k) = locijk_fpart(is,i,j,n_k) + force_flux_sed(is,i,j)
                 END IF
              END DO
              ! SEDIMENT TRACERS #2
              ! if a prescribed export flux based biological productivity 'biological' option has been set,
              ! => do not directly change nutrient concentrations via <locijk_focn>
              !    (this will instead be done in sub_calc_bio_uptake)
              ! => hack the value of the nutrient restoring array
              ! NOTE: force_restore_docn_nuts(io_PO4) in units of mol kg-1 (converted from the flux forcing units of mol kg-1)
              ! NOTE: use BIOGEM array conversion of POC -> POP and POP -> PO4 for completeness
              ! NOTE: negative sign of force_restore_docn_nuts for particulate creation ...
              ! NOTE: locijk_fpart must be in units of mol yr-1 (per grid point)
              !       i.e. BIOGEM POC flux output must be unit-converted from mol m-2 yr-1, e.g. with a scaling factor
              SELECT CASE (par_bio_prodopt)
              CASE ('bio_POCflux')
                 force_restore_docn_nuts(io_PO4) = -phys_ocn(ipo_rM,i,j,n_k)*loc_dtyr* &
                      & conv_sed_ocn(io_PO4,is_POP)*bio_part_red(is_POC,is_POP,i,j)*locijk_fpart(is_POC,i,j,n_k)
                 locijk_fpart(is_POC,i,j,n_k) = 0.0
              CASE DEFAULT
                 ! ### INSERT CODE TO INCLUDE ADDITIONAL SPECIAL CASES OF SEDIMENT FLUX FORCING ################################## !
                 !
                 ! ############################################################################################################### !
              end SELECT
              !
              ! NOTE: create units equivalent of (degrees C yr-1) :o)
              !       i.e. W m-2 times seconds-in-a-year times area divided by heat capacity (J K-1 g-1) ...
              if (ctrl_force_Fgeothermal2D) then
                 locijk_focn(1,i,j,loc_k1) = locijk_focn(1,i,j,loc_k1) + &
                      & conv_yr_s*force_Fgeothermal2D(i,j)*phys_ocn(ipo_A,i,j,loc_k1)/(conv_kg_g*const_Cp)
              else
                 locijk_focn(1,i,j,loc_k1) = locijk_focn(1,i,j,loc_k1) + &
                      & conv_yr_s*par_Fgeothermal*phys_ocn(ipo_A,i,j,loc_k1)/(conv_kg_g*const_Cp)
              end if

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** INVERSIONS ***'
              IF (force_restore_ocn_select(io_ALK) .AND. force_flux_ocn_select(io_ALK)) THEN
                 ! ------------------------------------------- !
                 ! (1) ocean ALK adjustment INVERSIONS
                 ! ------------------------------------------- !
                 IF ( force_restore_atm_select(ia_pCO2) .AND. &
                      & ( &
                      & (.NOT. force_restore_ocn_select(io_colr)) .AND. &
                      & (.NOT. force_restore_ocn_select(io_colb)) .AND. &
                      & (par_force_invert_ohmega < const_real_nullsmall) &
                      & ) &
                      & ) THEN
                    ! (1a) ocean ALK [GEOENGINEEGING of pCO2]
                    ! NOTE: re-scale the 'target' in case the atm flux forcing has been scaled (same param applied to both)
                    loc_force_actual = dum_sfcatm1(ia_pCO2,i,j)
                    loc_force_target = force_restore_atm(ia_pCO2,i,j)/par_atm_force_scale_val(ia_pCO2)
                    ! calculate the sign of the ALK input
                    if (loc_force_target < loc_force_actual) then
                       loc_force_sign = 1.0
                    else
                       if (ctrl_force_invert_noneg) then
                          loc_force_sign = 0.0
                       else
                          loc_force_sign = -1.0
                       end if
                    end If
                    ! NOTE: surface ocean addition only
                    locijk_focn(io_ALK,i,j,n_k) = loc_force_sign*force_flux_locn(io2l(io_ALK),i,j,n_k)
                    IF (force_flux_ocn_select(io_DIC)) then
                       locijk_focn(io_DIC,i,j,n_k) = loc_force_sign*force_flux_locn(io2l(io_DIC),i,j,n_k)
                    end if
                    IF (force_flux_ocn_select(io_DIC_13C)) then
                       locijk_focn(io_DIC_13C,i,j,n_k) = loc_force_sign*force_flux_locn(io2l(io_DIC_13C),i,j,n_k)
                    end if
                    IF (force_flux_ocn_select(io_Ca)) then
                       locijk_focn(io_Ca,i,j,n_k) = loc_force_sign*force_flux_locn(io2l(io_Ca),i,j,n_k)
                    end if
                 else
                    ! (1b) ocean ALK [GEOENGINEEGING of SURFACE OCEAN CHEMISTRY]
                    ! NOTE: the 'red' tracer is used to set a time history of ocean surface pH
                    !       the 'blue' tracer is used to set a time history of saturation state
                    !       otherwise employ fixed target
                    IF (force_restore_ocn_select(io_colr)) THEN
                       loc_force_target = force_restore_locn(io2l(io_colr),i,j,n_k)
                       ! replace mean global pH by point value
                       if (par_force_point_i*par_force_point_j /= 0) then
                          loc_force_actual = carb(ic_H,par_force_point_i,par_force_point_j,n_k)
                       end if
                    elseif (force_restore_ocn_select(io_colb)) then
                       loc_force_target = force_restore_locn(io2l(io_colb),i,j,n_k)
                       ! replace mean global saturation by point value
                       if (par_force_point_i*par_force_point_j /= 0) then
                          loc_force_actual = carb(ic_ohm_cal,par_force_point_i,par_force_point_j,n_k)
                       end if
                    else
                       loc_force_target = par_force_invert_ohmega
                    endif
                    ! calculate the sign of the ALK (and DIC and Ca2+) input
                    ! NOTE: remember that higher pH requires *more* ALK ;)
                    IF (force_restore_ocn_select(io_colr)) THEN
                       If (loc_force_target > -log10(loc_force_actual)) then
                          loc_force_sign = 1.0
                       else
                          if (ctrl_force_invert_noneg) then
                             loc_force_sign = 0.0
                          else
                             loc_force_sign = -1.0
                          end if
                       end If
                    else
                       If (loc_force_target > loc_force_actual) then
                          loc_force_sign = 1.0
                       else
                          if (ctrl_force_invert_noneg) then
                             loc_force_sign = 0.0
                          else
                             loc_force_sign = -1.0
                          end if
                       end If
                    end IF
                    !
                    DO k=loc_k1,n_k
                       locijk_focn(io_ALK,i,j,k) = loc_force_sign*force_flux_locn(io2l(io_ALK),i,j,k)
                       IF (force_flux_ocn_select(io_DIC)) then
                          locijk_focn(io_DIC,i,j,k) = loc_force_sign*force_flux_locn(io2l(io_DIC),i,j,k)
                       end if
                       IF (force_flux_ocn_select(io_DIC_13C)) then
                          locijk_focn(io_DIC_13C,i,j,k) = loc_force_sign*force_flux_locn(io2l(io_DIC_13C),i,j,k)
                       end if
                       IF (force_flux_ocn_select(io_Ca)) then
                          locijk_focn(io_Ca,i,j,k) = loc_force_sign*force_flux_locn(io2l(io_Ca),i,j,k)
                       end if
                    END DO
                 end if
                 diag_misc_2D(idiag_misc_2D_FALK,i,j)     = sum(locijk_focn(io_ALK,i,j,:))
                 diag_misc_2D(idiag_misc_2D_FDIC,i,j)     = sum(locijk_focn(io_DIC,i,j,:))
                 diag_misc_2D(idiag_misc_2D_FDIC_13C,i,j) = sum(locijk_focn(io_DIC_13C,i,j,:))
                 diag_misc_2D(idiag_misc_2D_FCa,i,j)      = sum(locijk_focn(io_Ca,i,j,:))
              else
                 ! ------------------------------------------- !
                 ! (2) ATMOSPHERIC pCO2 INVERSIONS
                 ! ------------------------------------------- !
                 IF (force_restore_atm_select(ia_pCO2) .AND. force_flux_atm_select(ia_pCO2)) THEN
                    locij_fatm(ia_pCO2,i,j) = 0.0
                    IF (force_restore_atm_select(ia_pCO2_13C) .AND. force_flux_atm_select(ia_pCO2_13C)) THEN
                       locij_fatm(ia_pCO2_13C,i,j) = 0.0
                       ! calculate local variables
                       loc_standard = const_standards(atm_type(ia_pCO2_13C))
                       loc_delta_actual = fun_calc_isotope_delta( &
                            & dum_sfcatm1(ia_pCO2,i,j),dum_sfcatm1(ia_pCO2_13C,i,j),loc_standard,.FALSE.,const_real_null &
                            & )
                       loc_delta_target = fun_calc_isotope_delta( &
                            & force_restore_atm(ia_pCO2,i,j),force_restore_atm(ia_pCO2_13C,i,j), &
                            & loc_standard,.FALSE.,const_real_null &
                            & )
                       loc_delta_source = fun_calc_isotope_delta( &
                            & force_flux_atm(ia_pCO2,i,j),force_flux_atm(ia_pCO2_13C,i,j),loc_standard,.FALSE.,const_real_null &
                            & )
                       !
                       if (ctrl_force_invert_explicit) then
                          ! (2a) atmosphere pCO2-13C
                          ! NOTE: done in the same way as the 'double inversion'
                          loc_force_target = force_restore_atm(ia_pCO2,i,j)
                          loc_force_actual = dum_sfcatm1(ia_pCO2,i,j)
                          ! calculate the sign of the CO2 input
                          If (loc_force_target > loc_force_actual) then
                             loc_force_sign = 1.0
                          else
                             if (ctrl_force_invert_noneg) then
                                loc_force_sign = 0.0
                             else
                                loc_force_sign = -1.0
                             end if
                          end If
                          ! adjust d13C
                          ! calculate the sign of the input d13C (as a function of whether carbon is added or subtracted)
                          ! NOTE: simplify: don't invert sign of carbon source ... just assume zero if target d13C < actual
                          if (loc_force_sign > 0.0) then
                             ! (carbon added)
                             If (loc_delta_target > loc_delta_actual) loc_delta_source = 0.0
                          elseif (loc_force_sign < 0.0) then
                             ! (carbon removed)
                             If (loc_delta_target < loc_delta_actual) loc_delta_source = 0.0
                          else
                             loc_delta_source = 0.0
                          end if
                          ! recalculate loc_frac
                          loc_frac = fun_calc_isotope_fraction(loc_delta_source,loc_standard)
                          ! calculate flux of CO2 to atmosphere with specified d13C to approach atmospheric d13C target
                          ! NOTE: units of (mol yr-1)
                          locij_fatm(ia_pCO2,i,j)     = loc_force_sign*force_flux_atm(ia_pCO2,i,j)
                          locij_fatm(ia_pCO2_13C,i,j) = loc_frac*locij_fatm(ia_pCO2,i,j)
                          diag_misc_2D(idiag_misc_2D_FpCO2,i,j)     = locij_fatm(ia_pCO2,i,j)
                          diag_misc_2D(idiag_misc_2D_FpCO2_13C,i,j) = locij_fatm(ia_pCO2_13C,i,j)
                       else
                          ! (2b) atmosphere pCO2-13C
                          ! NOTE: this code has a similar effect of restoring forcing, except that it allows
                          !       isotopic properties of a target signal to be followed via input/loss of carbon with a prescribed d13C
                          loc_frac = force_flux_atm(ia_pCO2_13C,i,j)/force_flux_atm(ia_pCO2,i,j)
                          ! calculate the sign of the CO2 input
                          If (loc_delta_target > loc_delta_actual) then
                             if (loc_delta_source > loc_delta_actual) then
                                loc_force_sign = 1.0
                             else
                                loc_force_sign = -1.0
                                if (ctrl_force_invert_noneg) loc_force_sign = 0.0
                             end If
                          else
                             if (loc_delta_source > loc_delta_actual) then
                                loc_force_sign = -1.0
                                if (ctrl_force_invert_noneg) loc_force_sign = 0.0
                             else
                                loc_force_sign = 1.0
                             end If
                          end If
                          ! calculate flux of CO2 to atmosphere with specified d13C to approach atmospheric d13C target
                          ! NOTE: units of (mol yr-1)
                          locij_fatm(ia_pCO2,i,j)     = loc_force_sign*force_flux_atm(ia_pCO2,i,j)
                          locij_fatm(ia_pCO2_13C,i,j) = loc_frac*locij_fatm(ia_pCO2,i,j)
                          diag_misc_2D(idiag_misc_2D_FpCO2,i,j)     = locij_fatm(ia_pCO2,i,j)
                          diag_misc_2D(idiag_misc_2D_FpCO2_13C,i,j) = locij_fatm(ia_pCO2_13C,i,j)
                       end if
                    else
                       ! (2c) atmospheric pCO2
                       ! NOTE: this code has a similar effect of restoring forcing, except that it allows
                       !       the isotopic properties of the flux to be prescribed rather than just the final isotopic state
                       ! NOTE: the threshold is set via a prescribed restoring signal
                       If (dum_sfcatm1(ia_pCO2,i,j) > force_restore_atm(ia_pCO2,i,j)) then
                          locij_fatm(ia_pCO2,i,j)     = -force_flux_atm(ia_pCO2,i,j)
                          locij_fatm(ia_pCO2_13C,i,j) = -force_flux_atm(ia_pCO2_13C,i,j)
                       else
                          locij_fatm(ia_pCO2,i,j)     = force_flux_atm(ia_pCO2,i,j)
                          locij_fatm(ia_pCO2_13C,i,j) = force_flux_atm(ia_pCO2_13C,i,j)
                       end If
                    end if
                 elseif ( &
                      & (force_restore_ocn_select(io_DIC_13C) .OR. force_restore_ocn_select(io_DOM_C_13C)) &
                      & .AND. &
                      & (force_flux_ocn_select(io_DIC_13C) .OR. force_flux_atm_select(ia_pCO2_13C)) &
                      & ) THEN
                    ! (3) INVERSIONS: ocean DIC of DOC d13C [SURFACE ONLY] -- DIC *OR* pCO2 fluxes ...
                    if (force_restore_ocn_select(io_DIC_13C)) then
                       ! calculate local variables
                       loc_standard = const_standards(ocn_type(io_DIC_13C))
                       ! replace mean global surface DIC d13C by point value
                       if (par_force_point_i*par_force_point_j /= 0) then
                          loc_delta_actual = fun_calc_isotope_delta( &
                               & ocn(io_DIC,par_force_point_i,par_force_point_j,n_k), &
                               & ocn(io_DIC_13C,par_force_point_i,par_force_point_j,n_k), &
                               & loc_standard,.FALSE.,const_real_null &
                               & )
                       else
                          loc_delta_actual = loc_force_actual_d13C
                       end if
                       loc_delta_target = fun_calc_isotope_delta( &
                            & force_restore_locn(io2l(io_DIC),i,j,n_k),force_restore_locn(io2l(io_DIC_13C),i,j,n_k), &
                            & loc_standard,.FALSE.,const_real_null &
                            & )
                    elseif (force_restore_ocn_select(io_DOM_C_13C)) then
                       ! calculate local variables
                       loc_standard = const_standards(ocn_type(io_DOM_C_13C))
                       ! replace mean global surface DIC d13C by point value
                       if (par_force_point_i*par_force_point_j /= 0) then
                          loc_delta_actual = fun_calc_isotope_delta( &
                               & ocn(io_DOM_C,par_force_point_i,par_force_point_j,n_k), &
                               & ocn(io_DOM_C_13C,par_force_point_i,par_force_point_j,n_k), &
                               & loc_standard,.FALSE.,const_real_null &
                               & )
                          loc_delta_target = fun_calc_isotope_delta( &
                               & force_restore_locn(io2l(io_DOM_C),par_force_point_i,par_force_point_j,n_k), &
                               & force_restore_locn(io2l(io_DOM_C_13C),par_force_point_i,par_force_point_j,n_k), &
                               & loc_standard,.FALSE.,const_real_null &
                               & )
                       else
                          loc_delta_actual = loc_force_actual_d13C
                          loc_delta_target = fun_calc_isotope_delta( &
                               & force_restore_locn(io2l(io_DOM_C),i,j,n_k),force_restore_locn(io2l(io_DOM_C_13C),i,j,n_k), &
                               & loc_standard,.FALSE.,const_real_null &
                               & )
                       end if
                    end if
                    IF (force_flux_ocn_select(io_DIC_13C)) THEN
                       loc_frac = force_flux_locn(io2l(io_DIC_13C),i,j,n_k)/force_flux_locn(io2l(io_DIC),i,j,n_k)
                       loc_delta_source = fun_calc_isotope_delta( &
                            & force_flux_locn(io2l(io_DIC),i,j,n_k),force_flux_locn(io2l(io_DIC_13C),i,j,n_k), &
                            & loc_standard,.FALSE.,const_real_null &
                            & )
                    elseIF (force_flux_atm_select(ia_pCO2_13C)) then
                       loc_frac = force_flux_atm(ia_pCO2_13C,i,j)/force_flux_atm(ia_pCO2,i,j)
                       loc_delta_source = fun_calc_isotope_delta( &
                            & force_flux_atm(ia_pCO2,i,j),force_flux_atm(ia_pCO2_13C,i,j), &
                            & loc_standard,.FALSE.,const_real_null &
                            & )
                    else
                       loc_frac         = 0.0
                       loc_delta_source = 0.0
                    end IF
                    ! calculate the sign of the CO2 input
                    ! NOTE: becasue the target is DOC or DIC, it does not make sense to contrast source and target d13C
                    !       (and adjust the sign of teh force on this basis)
                    If (loc_delta_target < loc_delta_actual) then
                       loc_force_sign = 1.0
                    else
                       if (ctrl_force_invert_noneg) then
                          loc_force_sign = 0.0
                       else
                          loc_force_sign = -1.0
                       end if
                    end If
                    ! calculate flux of CO2 to ocean *OR* atmosphere with specified d13C to approach atmospheric d13C target
                    ! NOTE: units of (mol yr-1)
                    IF (force_flux_ocn_select(io_DIC_13C)) THEN
                       locijk_focn(io_DIC,i,j,n_k)     = loc_force_sign*force_flux_locn(io2l(io_DIC),i,j,n_k)
                       locijk_focn(io_DIC_13C,i,j,n_k) = loc_frac*locijk_focn(io_DIC,i,j,n_k)
                       diag_misc_2D(idiag_misc_2D_FDIC,i,j)     = locijk_focn(io_DIC,i,j,n_k)
                       diag_misc_2D(idiag_misc_2D_FDIC_13C,i,j) = locijk_focn(io_DIC_13C,i,j,n_k)
                       locij_fatm(ia_pCO2,i,j)     = 0.0
                       locij_fatm(ia_pCO2_13C,i,j) = 0.0
                       if (force_restore_ocn_select(io_DOM_C_13C)) then
                          locijk_focn(io_DOM_C,i,j,n_k)     = 0.0
                          locijk_focn(io_DOM_C_13C,i,j,n_k) = 0.0
                       end if
                    elseIF (force_flux_atm_select(ia_pCO2_13C)) then
                       locij_fatm(ia_pCO2,i,j)     = loc_force_sign*force_flux_atm(ia_pCO2,i,j)
                       locij_fatm(ia_pCO2_13C,i,j) = loc_frac*locij_fatm(ia_pCO2,i,j)
                       diag_misc_2D(idiag_misc_2D_FpCO2,i,j)     = locij_fatm(ia_pCO2,i,j)
                       diag_misc_2D(idiag_misc_2D_FpCO2_13C,i,j) = locij_fatm(ia_pCO2_13C,i,j)
                       if (force_restore_ocn_select(io_DIC_13C)) then
                          locijk_focn(io_DIC,i,j,n_k)     = 0.0
                          locijk_focn(io_DIC_13C,i,j,n_k) = 0.0
                       elseif (force_restore_ocn_select(io_DOM_C_13C)) then
                          locijk_focn(io_DOM_C,i,j,n_k)     = 0.0
                          locijk_focn(io_DOM_C_13C,i,j,n_k) = 0.0
                       end if
                    end IF
                 elseIF ( &
                      & (force_restore_ocn_select(io_Ca) .AND. force_restore_ocn_select(io_Ca_44Ca)) &
                      &  .AND. &
                      & (force_flux_ocn_select(io_Ca) .AND. force_flux_ocn_select(io_Ca_44Ca)) &
                      & ) THEN
                    ! (4) INVERSIONS: ocean d44Ca [SURFACE ONLY]
                    ! calculate local variables
                    loc_standard = const_standards(ocn_type(io_Ca_44Ca))
                    loc_delta_actual = loc_force_actual_d44Ca
                    loc_delta_target = fun_calc_isotope_delta( &
                         & force_restore_locn(io2l(io_Ca),i,j,n_k),force_restore_locn(io2l(io_Ca_44Ca),i,j,n_k), &
                         & loc_standard,.FALSE.,const_real_null &
                         & )
                    loc_frac = force_flux_locn(io2l(io_Ca_44Ca),i,j,n_k)/force_flux_locn(io2l(io_Ca),i,j,n_k)
                    loc_delta_source = fun_calc_isotope_delta( &
                         & force_flux_locn(io2l(io_Ca),i,j,n_k),force_flux_locn(io2l(io_Ca_44Ca),i,j,n_k), &
                         & loc_standard,.FALSE.,const_real_null &
                         & )
                    ! calculate the sign of the Ca input
                    If (loc_delta_target > loc_delta_actual) then
                       if (loc_delta_source > loc_delta_actual) then
                          loc_force_sign = 1.0
                       else
                          loc_force_sign = -1.0
                          if (ctrl_force_invert_noneg) loc_force_sign = 0.0
                       end If
                    else
                       if (loc_delta_source > loc_delta_actual) then
                          loc_force_sign = -1.0
                          if (ctrl_force_invert_noneg) loc_force_sign = 0.0
                       else
                          loc_force_sign = 1.0
                       end If
                    end If
                    ! calculate flux of Ca to ocean with specified d44Ca to approach atmospheric d44Ca target
                    ! NOTE: units of (mol yr-1)
                    ! NOTE: extend capabilities to full ocean depth not just surface
                    DO k=n_k,loc_k1,-1
                       locijk_focn(io_Ca,i,j,k)      = loc_force_sign*force_flux_locn(io2l(io_Ca),i,j,k)
                       locijk_focn(io_Ca_44Ca,i,j,k) = loc_frac*locijk_focn(io_Ca,i,j,k)
                       locijk_focn(io_ALK,i,j,k)     = loc_force_sign*force_flux_locn(io2l(io_ALK),i,j,k)
                       diag_misc_2D(idiag_misc_2D_FCa,i,j)      = diag_misc_2D(idiag_misc_2D_FCa,i,j) + locijk_focn(io_Ca,i,j,k)
                       diag_misc_2D(idiag_misc_2D_FCa_44Ca,i,j) = diag_misc_2D(idiag_misc_2D_FCa_44Ca,i,j) + locijk_focn(io_Ca_44Ca,i,j,k)
                       diag_misc_2D(idiag_misc_2D_FALK,i,j)     = diag_misc_2D(idiag_misc_2D_FALK,i,j) + locijk_focn(io_ALK,i,j,k)
                    end DO
                 end If
                 ! ------------------------------------------- !
                 ! pH--d13C inversion
                 ! ------------------------------------------- !
                 IF (force_restore_ocn_select(io_colr)) THEN
                    IF (force_flux_atm_select(ia_pCO2) .AND. force_flux_atm_select(ia_pCO2_13C)) THEN
                       ! (5) INVERSIONS: ocean pH
                       loc_force_target = force_restore_locn(io2l(io_colr),i,j,n_k)
                       ! replace mean global pH by point value
                       if (par_force_point_i*par_force_point_j /= 0) then
                          loc_force_actual = carb(ic_H,par_force_point_i,par_force_point_j,n_k)
                       end if
                       ! calculate the sign of the CO2 input
                       ! NOTE: remember that lower pH requires *more* CO2 ;)
                       If (loc_force_target < -log10(loc_force_actual)) then
                          loc_force_sign = 1.0
                       else
                          if (ctrl_force_invert_noneg) then
                             loc_force_sign = 0.0
                          else
                             loc_force_sign = -1.0
                          end if
                       end If
                       ! adjust d13C if d13C target is also selected
                       IF (force_restore_ocn_select(io_DIC) .AND. force_restore_ocn_select(io_DIC_13C)) THEN
                          loc_standard = const_standards(ocn_type(io_DIC_13C))
                          ! replace mean global surface DIC d13C by point value
                          if (par_force_point_i*par_force_point_j /= 0) then
                             loc_delta_actual = fun_calc_isotope_delta( &
                                  & ocn(io_DIC,par_force_point_i,par_force_point_j,n_k), &
                                  & ocn(io_DIC_13C,par_force_point_i,par_force_point_j,n_k), &
                                  & loc_standard,.FALSE.,const_real_null &
                                  & )
                          else
                             loc_delta_actual = loc_force_actual_d13C
                          end if
                          loc_delta_target = fun_calc_isotope_delta( &
                               & force_restore_locn(io2l(io_DIC),i,j,n_k),force_restore_locn(io2l(io_DIC_13C),i,j,n_k), &
                               & loc_standard,.FALSE.,const_real_null &
                               & )
                          loc_delta_source = fun_calc_isotope_delta( &
                               & force_flux_atm(ia_pCO2,i,j),force_flux_atm(ia_pCO2_13C,i,j), &
                               & loc_standard,.FALSE.,const_real_null &
                               & )
                          ! calculate the sign of the input d13C (as a function of whether carbon is added or subtracted)
                          ! NOTE: simplify: don't invert sign of carbon source ... just assume zero if target d13C < actual
                          if (loc_force_sign > 0.0) then
                             ! (carbon added)
                             If (loc_delta_target > loc_delta_actual) loc_delta_source = 0.0
                          elseif (loc_force_sign < 0.0) then
                             ! (carbon removed)
                             If (loc_delta_target < loc_delta_actual) loc_delta_source = 0.0
                          else
                             loc_delta_source = 0.0
                          end if
                          ! recalculate loc_frac
                          loc_standard = const_standards(atm_type(ia_pCO2_13C))
                          loc_frac = fun_calc_isotope_fraction(loc_delta_source,loc_standard)
                       else
                          loc_frac = force_flux_atm(ia_pCO2_13C,i,j)/force_flux_atm(ia_pCO2,i,j)
                       end IF
                       ! calculate flux of CO2 to atmosphere with specified d13C to approach atmospheric d13C target
                       ! NOTE: units of (mol yr-1)
                       locij_fatm(ia_pCO2,i,j)     = loc_force_sign*force_flux_atm(ia_pCO2,i,j)
                       locij_fatm(ia_pCO2_13C,i,j) = loc_frac*locij_fatm(ia_pCO2,i,j)
                       diag_misc_2D(idiag_misc_2D_FpCO2,i,j)     = locij_fatm(ia_pCO2,i,j)
                       diag_misc_2D(idiag_misc_2D_FpCO2_13C,i,j) = locij_fatm(ia_pCO2_13C,i,j)
                       IF (force_flux_atm_select(ia_pcolr) .AND. force_flux_atm_select(ia_pcolr_13C)) THEN
                          locij_fatm(ia_pCO2,i,j)      = locij_fatm(ia_pCO2,i,j) + locij_fatm(ia_pcolr,i,j)
                          locij_fatm(ia_pCO2_13C,i,j)  = locij_fatm(ia_pCO2_13C,i,j) + locij_fatm(ia_pcolr_13C,i,j)
                          locij_fatm(ia_pcolr,i,j)     = 0.0
                          locij_fatm(ia_pcolr_13C,i,j) = 0.0
                       end if
                    end if
                 end if
                 ! ------------------------------------------- !
                 ! calculate and add Corg burial if requested
                 ! ------------------------------------------- !
                 if (ctrl_force_invert_Corgburial) then
                    ! ---------------------------------------- ! determine sign of 13C mismatch
                    !                                            remove Corg only when actual d13C is too negative
                    If (loc_delta_target > loc_delta_actual) then
                       loc_force_sign = -1.0
                    else
                       loc_force_sign = 0.0
                    end If
                    ! ---------------------------------------- ! calculate 13C fractionation w.r.t. surface ocean DIC
                    loc_frac = &
                         & fun_Corg_Rfrac(ocn(io_T,i,j,n_k),carb(ic_conc_CO2,i,j,n_k), &
                         & carbisor(ici_CO2_r13C,i,j,n_k),par_d13C_DIC_Corg_ef_sp,.false.)
                    ! ---------------------------------------- ! update flux
                    IF (force_flux_ocn_select(io_DIC_13C)) THEN
                       loc_force_flux = loc_force_sign*force_flux_locn(io2l(io_DIC),i,j,n_k)
                    elseIF (force_flux_atm_select(ia_pCO2_13C)) then
                       loc_force_flux = loc_force_sign*force_flux_atm(ia_pCO2,i,j)
                    end IF
                    ! ---------------------------------------- ! scale C burial flux relative to emissions
                    loc_force_flux = par_force_invert_fCorgburial*loc_force_flux
                    ! ---------------------------------------- ! set and record C flux
                    locijk_focn(io_DIC,i,j,n_k)     = locijk_focn(io_DIC,i,j,n_k) + loc_force_flux
                    locijk_focn(io_DIC_13C,i,j,n_k) = locijk_focn(io_DIC_13C,i,j,n_k) + loc_frac*loc_force_flux
                    diag_misc_2D(idiag_misc_2D_FDIC,i,j)     = locijk_focn(io_DIC,i,j,n_k)
                    diag_misc_2D(idiag_misc_2D_FDIC_13C,i,j) = locijk_focn(io_DIC_13C,i,j,n_k)
                 end IF

              end IF

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** OCEAN-ATMOPSHERE EXCHANGE FLUXES ***'
              ! *** OCEAN-ATMOPSHERE EXCHANGE FLUXES ***
              ! calculate ocean-atmosphere exchange
              ! NOTE: a positive value represents net ocean to atmosphere transfer
              ! NOTE: units of (mol yr-1)
              ! NOTE: <locij_fatm> is an array used to update the atmospheric tracer inventory
              ! NOTE: <locij_focnatm> is an array used to store the ocean -> atm gas flux for results reporting;
              !                       => it is not used in the updating of mass balance anywhere
              locij_focnatm(:,i,j) = fun_calc_ocnatm_flux(i,j,dum_sfcatm1(:,i,j),loc_dtyr)
              ! set local flux arrays for the updating of ocean and atmosphere reservoirs
              DO l=3,n_l_atm
                 ia = conv_iselected_ia(l)
                 locij_fatm(ia,i,j) = locij_fatm(ia,i,j) + locij_focnatm(ia,i,j)
                 loc_tot_i = conv_atm_ocn_i(0,ia)
                 do loc_i=1,loc_tot_i
                    io = conv_atm_ocn_i(loc_i,ia)
                    locijk_focn(io,i,j,n_k) = locijk_focn(io,i,j,n_k) - conv_atm_ocn(io,ia)*locij_focnatm(ia,i,j)
                 end do
              end DO
              diag_airsea(:,i,j) = locij_focnatm(:,i,j)
              ! KMM: Add sulfate and negative alkalinity to surface ocean to compensate for H2S loss to atmosphere
              ! NOTE: H2S can escape to the atmosphere, and this code implicitly accounts for the return sulphuric acid rain
              ! NOTE: also account for O2 consumed in the atmosphere in the oxidation of H2S
              ! NOTE: only do anything if there is O2 in the atmosphere!!!
              if (ocn_select(io_H2S) .AND. atm_select(ia_pH2S)) then
                 select case (opt_ocnatmH2S_fix)
                 case ('KMM') ! default
                    IF (                                                       &
                         & (locij_focnatm(ia_pH2S,i,j) > const_real_nullsmall) &
                         &  .AND.                                              &
                         & (dum_sfcatm1(ia_pO2,i,j) > const_real_nullsmall)    &
                         & ) THEN
                       ! mass balance adjustments
                       locijk_focn(io_ALK,i,j,n_k) = locijk_focn(io_ALK,i,j,n_k) - 2.0*locij_focnatm(ia_pH2S,i,j)
                       locijk_focn(io_SO4,i,j,n_k) = locijk_focn(io_SO4,i,j,n_k) + locij_focnatm(ia_pH2S,i,j)
                       locij_fatm(ia_pO2,i,j)      = locij_fatm(ia_pO2,i,j)      - 2.0*locij_focnatm(ia_pH2S,i,j)
                       locij_fatm(ia_pH2S,i,j)     = 0.0
                       ! update flux reporting
                       locij_focnatm(ia_pO2,i,j)   = locij_focnatm(ia_pO2,i,j)   - 2.0*locij_focnatm(ia_pH2S,i,j)
                       locij_focnatm(ia_pH2S,i,j)  = 0.0
                       ! ### INSERT CODE FOR ISOTOPES ############################################################################### !
                       !
                       ! ############################################################################################################ !
                    end IF
                 case ('KMM_lowO2')
                    IF (                                                       &
                         & (locij_focnatm(ia_pH2S,i,j) > const_real_nullsmall) &
                         & ) THEN
                       ! mass balance adjustments
                       locijk_focn(io_ALK,i,j,n_k) = locijk_focn(io_ALK,i,j,n_k) - 2.0*locij_focnatm(ia_pH2S,i,j)
                       locijk_focn(io_SO4,i,j,n_k) = locijk_focn(io_SO4,i,j,n_k) + locij_focnatm(ia_pH2S,i,j)
                       locij_fatm(ia_pO2,i,j)      = locij_fatm(ia_pO2,i,j)      - 2.0*locij_focnatm(ia_pH2S,i,j)
                       locij_fatm(ia_pH2S,i,j)     = 0.0
                       ! update flux reporting
                       locij_focnatm(ia_pO2,i,j)   = locij_focnatm(ia_pO2,i,j)   - 2.0*locij_focnatm(ia_pH2S,i,j)
                       locij_focnatm(ia_pH2S,i,j)  = 0.0
                       ! ### INSERT CODE FOR ISOTOPES ############################################################################### !
                       !
                       ! ############################################################################################################ !
                    end IF
                 case ('noflux')
                    ! no flux to atmosphere
                    locij_fatm(ia_pH2S,i,j)    = 0.0
                    locij_focnatm(ia_pH2S,i,j) = 0.0
                    diag_airsea(ia_pH2S,i,j)   = 0.0
                 end select
              END IF
              IF (opt_select(iopt_select_carbchem)) THEN
                 ! record air-sea gas exchange coefficient for posterity
                 if ((1.0 - phys_ocnatm(ipoa_seaice,i,j)) > const_real_nullsmall) then
                    phys_ocnatm(ipoa_KCO2,i,j) = conv_umol_mol*phys_ocn(ipo_rA,i,j,n_k)* &
                         & (locij_focnatm(ia_pCO2,i,j)/(1.0 - phys_ocnatm(ipoa_seaice,i,j)))/ &
                         & ((carb(ic_conc_CO2,i,j,n_k)/ocnatm_airsea_solconst(ia_pCO2,i,j)) - dum_sfcatm1(ia_pCO2,i,j))
                 end if
              end IF

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** TERRESTRIAL WEATHERING INPUT ***'
              ! *** TERRESTRIAL WEATHERING INPUT ***
              ! modify remineralization array according to terrestrial weathering input
              ! NOTE: <dum_sfxsumrok1> in units of (mol) (per time-step)
              ! NOTE: no screening for a 'closed system' is made as substractions are made appropriatly from
              !       'SEDIMENT DISSOLUTION INPUT' if a weathering flux exists
              DO l=3,n_l_ocn
                 io = conv_iselected_io(l)
                 locij_frokocn(io,i,j) = locij_frokocn(io,i,j) + dum_sfxsumrok1(io,i,j)
              end do
              DO l=3,n_l_ocn
                 io = conv_iselected_io(l)
                 bio_remin(io,i,j,n_k) = bio_remin(io,i,j,n_k) + phys_ocn(ipo_rM,i,j,n_k)*locij_frokocn(io,i,j)
              end do

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** WATER COLUMN REMINERALIZATION - NH4 OXIDATION ***'
              ! *** WATER COLUMN REMINERALIZATION - NH4 OXIDATION ***
              if (ocn_select(io_O2) .AND. ocn_select(io_NO3) .AND. ocn_select(io_NH4)) then
                 if (ocn_select(io_NO2)) then
                    call sub_box_oxidize_NH4toNO2(i,j,loc_k1,loc_dtyr)
                 else
                    call sub_box_oxidize_NH4toNO3(i,j,loc_k1,loc_dtyr)
                 end if
              end If

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** WATER COLUMN GEOCHEMISTRY - H2S OXIDATION ***'
              ! *** WATER COLUMN GEOCHEMISTRY - H2S OXIDATION ***
              if (ocn_select(io_O2) .AND. ocn_select(io_SO4) .AND. ocn_select(io_H2S)) then
                 call sub_box_oxidize_H2S(i,j,loc_k1,loc_dtyr)
              end If

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** WATER COLUMN REMINERALIZATION - CH4 OXIDATION ***'
              ! *** WATER COLUMN REMINERALIZATION - CH4 OXIDATION ***
              select case (par_bio_remin_CH4ox)
              case ('default')
                 if (ocn_select(io_O2) .AND. ocn_select(io_CH4)) then
                    call sub_calc_bio_remin_oxidize_CH4(i,j,loc_k1,loc_dtyr)
                 end If
              case ('CH4ox_MM')
                 if (ocn_select(io_O2) .AND. ocn_select(io_CH4)) then
                    call sub_calc_bio_remin_oxidize_CH4_AER(i,j,loc_k1,loc_dtyr)
                 end If
              end select

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** WATER COLUMN REMINERALIZATION - ANAEROBIC CH4 OXIDATION ***'
              ! *** WATER COLUMN REMINERALIZATION - ANAEROBIC CH4 OXIDATION ***
              if (ocn_select(io_O2) .AND. ocn_select(io_SO4) .AND. ocn_select(io_CH4)) then
                 call sub_calc_bio_remin_oxidize_CH4_AOM(i,j,loc_k1,loc_dtyr)
              end If

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** WATER COLUMN REMINERALIZATION - I OXIDATION ***'
              ! *** WATER COLUMN REMINERALIZATION - I OXIDATION ***
              if (ocn_select(io_O2) .AND. ocn_select(io_I)) then
                 call sub_calc_bio_remin_oxidize_I(i,j,loc_k1,loc_dtyr)
              end If

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** SURFACE OCEAN BIOLOGICAL PRODUCTIVITY ***'
              ! *** SURFACE OCEAN BIOLOGICAL PRODUCTIVITY ***
              call sub_calc_bio(i,j,loc_k1,loc_dtyr)


              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** SET PREFORMED TRACERS ***'
              ! *** SET PREFORMED TRACERS ***
              call sub_calc_bio_preformed(i,j)

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** OCEAN ABIOTIC PRECIPITATION ***'
              ! *** OCEAN ABIOTIC PRECIPITATION ***
              if (ctrl_bio_CaCO3precip .AND. sed_select(is_CaCO3)) then
                 call sub_calc_bio_uptake_abio(i,j,loc_k1,loc_dtyr)
              end if

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** MISCELLANEOUS GEOCHEMICAL TRANSFORMATIONS ***'
              ! *** MISCELLANEOUS GEOCHEMICAL TRANSFORMATIONS ***
              call sub_box_misc_geochem(i,j,loc_k1,loc_dtyr)
              if (ocn_select(io_IO3)) then
                 call sub_calc_bio_remin_reduce_IO3(i,j,loc_k1,loc_dtyr)
              end If

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) &
                   & print*,'*** WATER COLUMN GEOCHEMISTRY - Fe SPECIATION ***'
              ! *** WATER COLUMN GEOCHEMISTRY - Fe SPECIATION ***
              ! NOTE: although <locijk_focn> Fe is added to the remin array within the sub_calc_geochem_Fe subroutine,
              !       the same flux is subtracted again after equilibrium has been calculated,
              !       hence <locijk_focn> Fe later can be added 'as normal' in updating the <ocn> array
              ! NOTE: the above of course makes no sense at all and newer schemes are called earier without this shit
              if (sed_select(is_det) .AND. ocn_select(io_Fe)) then
                 SELECT CASE (trim(opt_geochem_Fe))
                 CASE ('OLD')
                    call sub_calc_geochem_Fe(i,j,loc_k1,loc_dtyr*phys_ocn(ipo_rM,i,j,:)*locijk_focn(io_Fe,i,j,:))
                 end SELECT
              end If

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) print*, &
                   & '*** 18O hack ... ***'
              ! *** 18O hack!!! ***
              ! all ocean interior transformations involving the gain or loss of O2 are hence performed with no change in d18O
              ! NTOE: use global mean d18O value (to help prevent potential for numerical instability)
              if (ocn_select(io_O2_18O)) then
                 bio_remin(io_O2_18O,i,j,:) = loc_r18O*bio_remin(io_O2,i,j,:)
              end if

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) &
                   & print*, '*** INTERFACE ARRAY UPDATE ***'
              ! *** INTERFACE ARRAY UPDATE ***
              ! (1) set ocn->atm flux
              ! NOTE: convert units from (mol yr-1) to (mol m-2 s-1)
              ! NOTE: update external interface array with TOTAL flux to atmosphere
              !       (i.e., due to both air-sea exchange and any forcing of the atmosphere)
              DO l=3,n_l_atm
                 ia = conv_iselected_ia(l)
                 dum_sfxatm1(ia,i,j) = phys_ocnatm(ipoa_rA,i,j)*conv_s_yr*locij_fatm(ia,i,j)
              end do
              ! (2) set bottom-water tracers
              ! NOTE: estimate Ca and borate concentrations from salinity (if not selected and therefore not explicitly treated)
              ! NOTE: STRICTLY, THIS FIELD SHOULD BE UNMODIFIED 'OCN', ALTHOUGH GIVEN THE TIME AVERAGING, IT DOES NOT REALLY MATTER
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 dum_sfcocn1(io,i,j) = ocn(io,i,j,loc_k1) + &
                      & bio_remin(io,i,j,loc_k1) + loc_dtyr*phys_ocn(ipo_rM,i,j,loc_k1)*locijk_focn(io,i,j,loc_k1)
              end do
              IF (.NOT. ocn_select(io_Ca))    dum_sfcocn1(io_Ca,i,j) = fun_calc_Ca(dum_sfcocn1(io_S,i,j))
              IF (.NOT. ocn_select(io_B))     dum_sfcocn1(io_B,i,j)  = fun_calc_Btot(dum_sfcocn1(io_S,i,j))
              IF (.NOT. ocn_select(io_SO4)) dum_sfcocn1(io_SO4,i,j)  = fun_calc_SO4tot(dum_sfcocn1(io_S,i,j))
              IF (.NOT. ocn_select(io_F))     dum_sfcocn1(io_F,i,j)  = fun_calc_Ftot(dum_sfcocn1(io_S,i,j))
              ! (3) set ocn->sed flux
              ! NOTE: convert units from (mol per timestep) to (mol m-2 s-1)
              ! NOTE: if 'allow particulate flux to sediments' option in biogem_config is not selected,
              !       the value of the particulate flux to sediments local array <locij_ocnsed> is zero
              ! NOTE: for particulate fractions (type par_sed_type_frac) -- scale by time such that the fraction is preserved
              !       when passed through the ocean->sediment interface
              !       in the ocn->sed flux coupling, assumed units of (mol m-2 s-1) are converted to (mol m-2) and summed
              !       => convert here to pretend s-1 units (which is cancelled out in the call to cpl_flux_ocnsed)
              !       also add dummy conversion conv_m2_cm2 -- this is undone in sedgem (conv_cm2_m2*dum_sfxsumsed(:,i,j))
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 locij_focnsed(is,i,j) = bio_settle(is,i,j,loc_k1)
                 SELECT CASE (sed_type(is))
                 CASE (par_sed_type_frac)
                    dum_sfxsed1(is,i,j) = conv_m2_cm2*locij_focnsed(is,i,j)*loc_rdts
                 case default
                    dum_sfxsed1(is,i,j) = phys_ocn(ipo_rA,i,j,loc_k1)*locij_focnsed(is,i,j)*loc_rdts
                 end SELECT
              end do
              ! (4) set age tracers
              if (sed_select(is_CaCO3_age)) then
                 dum_sfxsed1(is_CaCO3_age,i,j) = loc_t*dum_sfxsed1(is_CaCO3,i,j)
              end if
              if (sed_select(is_det_age)) then
                 dum_sfxsed1(is_det_age,i,j) = loc_t*dum_sfxsed1(is_det,i,j)
              end if

              IF (ctrl_debug_lvl1 .AND. loc_debug_ij) &
                   & print*, '*** AUDIT FLUX UPDATE ***'
              ! *** AUDIT FLUX UPDATE ***
              IF (ctrl_audit) THEN
                 ! update net integrated fluxes into the ocean for tracer inventory auditing (if selected)
                 ! NOTE: take into account any sediment tracer flux forcing
                 loc_ocnsed_audit(:) = -locij_fsedocn(:,i,j) - locij_frokocn(:,i,j)
                 DO l=1,n_l_sed
                    is = conv_iselected_is(l)
                    loc_tot_i = conv_sed_ocn_i(0,is)
                    do loc_i=1,loc_tot_i
                       io = conv_sed_ocn_i(loc_i,is)
                       loc_ocnsed_audit(io) = loc_ocnsed_audit(io) + &
                            & conv_sed_ocn(io,is)*(locij_focnsed(is,i,j) - loc_dtyr*force_flux_sed(is,i,j))
                       locio_mask(io) = .FALSE.
                    end do
                 end DO
                 DO l=3,n_l_ocn
                    io = conv_iselected_io(l)
                    audit_ocn_delta(io) = audit_ocn_delta(io) + &
                         & loc_dtyr*SUM(locijk_focn(io,i,j,loc_k1:n_k)) - loc_ocnsed_audit(io)
                 END DO
              end IF

              ! WHAT IS THIS ... ?
              IF (force_restore_atm_select(ia_pCO2) .AND. force_flux_atm_select(ia_pCO2)) THEN
                 locij_fatm(ia_pCO2,i,j) = 0.0
                 if (force_restore_atm_select(ia_pCO2_13C)) locij_fatm(ia_pCO2_13C,i,j) = 0.0
                 if (force_restore_atm_select(ia_pCO2_14C)) locij_fatm(ia_pCO2_14C,i,j) = 0.0
              end if

           end if

           ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !
           ! *** WET GRID PT CONDITIONALITY END *** !
           ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !

           IF (ctrl_debug_lvl1 .AND. loc_debug_ij) &
                & print*, '*** <<< END (i,j) GRID POINT'

        END DO block_jloop
     END DO block_iloop

     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !
     ! *** (i,j) GRID PT LOOP END *** !
     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !

     ! *** CALCULATE TRACER ANOMOLY ***
     ! NOTE: also, for now, vectorize <ocn>
     IF (ctrl_debug_lvl1) print*, '*** CALCULATE TRACER ANOMOLY ***'
     do n=1,n_vocn
        loc_i = vocn(n)%i
        loc_j = vocn(n)%j
        loc_k1 = vocn(n)%k1
        DO k=n_k,loc_k1,-1
           DO l=1,n_l_ocn
              io = conv_iselected_io(l)
              vdocn(n)%mk(l,k) = bio_remin(io,loc_i,loc_j,k) + loc_dtyr*vphys_ocn(n)%mk(ipo_rM,k)*locijk_focn(io,loc_i,loc_j,k)
              vocn(n)%mk(l,k) = ocn(io,loc_i,loc_j,k)
           end do
           DO l=1,n_l_sed
              is = conv_iselected_is(l)
              vdbio_part(n)%mk(l,k) = loc_dtyr*vphys_ocn(n)%mk(ipo_rM,k)*locijk_fpart(is,loc_i,loc_j,k)
              vbio_part(n)%mk(l,k) = bio_part(is,loc_i,loc_j,k)
           end do
        end DO
     end do
!!$     DO i=1,n_i
!!$        DO j=1,n_j
!!$           DO k=goldstein_k1(i,j),n_k
!!$              !
!!$              DO l=1,n_l_ocn
!!$                 io = conv_iselected_io(l)
!!$                 docn(io,i,j,k) = loc_dtyr*phys_ocn(ipo_rM,i,j,k)*locijk_focn(io,i,j,k)
!!$              end DO
!!$              ! adjust particulate tracer concentrations
!!$              DO l=1,n_l_sed
!!$                 is = conv_iselected_is(l)
!!$                 dbio_part(is,i,j,k) = loc_dtyr*phys_ocn(ipo_rM,i,j,k)*locijk_fpart(is,i,j,k)
!!$              end do
!!$           end do
!!$        end do
!!$     end DO

  END IF if_go

  IF (ctrl_debug_lvl1) print*, '*** TEST FOR END-OF-RUN ***'
  ! *** TEST FOR END-OF-RUN ***
  ! if local (BioGeM) model time has surpassed the set model end time, then set the flag updating system biogeochemistry to false
  IF (loc_t < const_real_nullsmall) par_misc_t_go = .FALSE.
  If (error_stop) then
     PRINT*,'*** END BioGeM - ERROR ***'
     PRINT*,' '
     ! reset array values
     CALL sub_init_int_timeslice()
     call diag_biogem_timeslice( &
          dum_dts,               &
          dum_genie_clock,       &
          & dum_sfcatm1,         &
          & dum_sfxatm1,         &
          & dum_sfxocn1,         &
          & dum_sfcsed1,         &
          & dum_sfxsed1,         &
          & dum_sfxsumrok1,      &
          & .false.,             &
          & .true.               &
          & )
     CALL end_biogem()
     STOP
  end If
  If ((par_misc_t_start + real(dum_genie_clock)/(1000.0*conv_yr_s)) > par_misc_t_stop) then
     PRINT*,'*** END BioGeM - saving restart and terminating early ***'
     PRINT*,' '
     call biogem_save_restart(dum_genie_clock)
     STOP
  end if

  return

end subroutine biogem
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BIOGEM LOOP SUBROUTINE
! ******************************************************************************************************************************** !
! NOTE: <loc_vocn> is the array into which the GOLDSTEIn tracer field <ts> is going to be de-salinity normalized and placed
!       plus a copy of <ts> T,S for completeness
subroutine biogem_tracercoupling( &
     & dum_ts,                    &
     & dum_ts1,                   &
     & dum_genie_clock,        &
     & dum_egbg_sfcpart,          &
     & dum_egbg_sfcremin,         &
     & dum_egbg_sfcocn            &
     & )
  USE biogem_lib
  USE biogem_box
  USE genie_util, ONLY: check_iostat
  USE biogem_data_netCDF, ONLY: sub_save_netcdf_TM
  IMPLICIT NONE
  ! ---------------------------------------------------------- !
  ! DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  real,intent(inout),dimension(intrac_ocn,n_i,n_j,n_k)::dum_ts  ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
  real,intent(inout),dimension(intrac_ocn,n_i,n_j,n_k)::dum_ts1 ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
  real,intent(in),   dimension(n_sed,n_i,n_j,n_k)     ::dum_egbg_sfcpart  ! ecology-interface: particulate composition change; ocn grid
  real,intent(in),   dimension(n_ocn,n_i,n_j,n_k)     ::dum_egbg_sfcremin ! ecology-interface: ocean tracer composition change; ocn grid
  real,intent(out),  dimension(n_ocn,n_i,n_j,n_k)     ::dum_egbg_sfcocn   ! ecology-interface: ocean tracer composition; ocn grid
  integer(kind=8),INTENT(IN)::dum_genie_clock                    ! genie clock (ms since start) NOTE: 8-byte integer
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  INTEGER::k,l,n,io,is                                         ! counting indices
  integer::loc_k1                                              ! local topography
  integer::loc_i,loc_j                                         !
  real::loc_ocn_tot_M,loc_ocn_rtot_M                           ! ocean mass and reciprocal
  real::loc_ocn_tot_V,loc_ocn_rtot_V                           ! ocean volume  and reciprocal
  real::loc_ocn_mean_S_OLD,loc_ocn_rmean_S_OLD                 ! old mean ocean salinity and reciprocal
  real::loc_ocn_mean_S_NEW                                     ! new mean ocean salinity
  real::loc_Sratio,loc_rSratio                                 !
  REAL,DIMENSION(n_l_ocn)::loc_ocn_tot_OLD,loc_ocn_tot_NEW     !
  REAL,DIMENSION(n_l_ocn)::loc_ocn_rtot_NEW                    !
  type(fieldocn),DIMENSION(:),ALLOCATABLE::loc_vocn            !
  type(fieldocn),DIMENSION(:),ALLOCATABLE::loc_vts             !
  real,DIMENSION(:),ALLOCATABLE::loc_partialtot                !
  integer::matrix_tracer,nc_record_count ! matrix
  real::loc_t,loc_yr
  ! ---------------------------------------------------------- !
  ! INITIALIZE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! allocate arrays
  ALLOCATE(loc_vocn(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(loc_vts(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  do n=1,n_vocn
     allocate(loc_vocn(n)%mk(1:n_l_ocn,1:n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     allocate(loc_vts(n)%mk(1:n_l_ocn,1:n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
  end do
  ALLOCATE(loc_partialtot(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! ---------------------------------------------------------- ! initialize local arrays
  ! set grid information and zero tracer values
  loc_vocn(:) = fun_lib_init_vocn()
  loc_vts(:)  = fun_lib_conv_tsTOvocn(dum_ts(:,:,:,:))
  ! ---------------------------------------------------------- ! calculate local constants: total ocean mass and recpirocal
  loc_partialtot(:) = 0.0
  loc_ocn_tot_M = 0.0
  do n=1,n_vocn
     loc_k1 = vocn(n)%k1
     loc_partialtot(n) = sum(vphys_ocn(n)%mk(ipo_M,loc_k1:n_k))
  end do
  loc_ocn_tot_M = sum(loc_partialtot(:))
  loc_ocn_rtot_M = 1.0/loc_ocn_tot_M
  ! ---------------------------------------------------------- ! calculate local constants: total ocean volume and recpirocal
  loc_partialtot(:) = 0.0
  loc_ocn_tot_V = 0.0
  do n=1,n_vocn
     loc_k1 = vocn(n)%k1
     loc_partialtot(n) = sum(vphys_ocn(n)%mk(ipo_V,loc_k1:n_k))
  end do
  loc_ocn_tot_V = sum(loc_partialtot(:))
  loc_ocn_rtot_V = 1.0/loc_ocn_tot_V
  ! ---------------------------------------------------------- ! calculate gem time
  ! update model time
  ! NOTE: par_misc_t_runtime is counted DOWN in years
  !       => for BIOGEM, the 'end of the world' occurs when time reaches zero
  loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
  ! calculate actual year (counting years Before Present or otherwise)
  IF (ctrl_misc_t_BP) THEN
     loc_yr = loc_t + par_misc_t_end
  ELSE
     loc_yr = par_misc_t_end - loc_t
  END IF

  ! ---------------------------------------------------------- !
  ! MUFFIN MATRIX I
  ! ---------------------------------------------------------- !
  if(ctrl_data_diagnose_TM) then

  if(matrix_go.eq.1)then

    ! if matrix_k == 0 then we have finished diagnosing the matrix
    ! instead of recording results, write index and let run finish
    IF(matrix_k.lt.1)THEN
      ! write out an indexing file
      nc_record_count=1
      do n=1,n_vocn,1
        loc_k1 = loc_vts(n)%k1
        do k=n_k,loc_k1,-1
          call sub_save_netcdf_TM(0,nc_record_count,0.0,0,0,0,loc_vts(n)%i,loc_vts(n)%j,k)
          nc_record_count=nc_record_count+1
        end do
      end do
      print*, '>>> Transport matrix diagnosed'
      print*, '>>> Transport matrix index written to file'
      ctrl_data_diagnose_TM=.false. ! stop matrix being diagnosed

    else

    ! integrate dye experiment results array
    do l=1,6,1
      do n=1,n_vocn,1
        loc_k1=loc_vts(n)%k1
        do k=n_k,loc_k1,-1
          select case(l)
          case(1)
            matrix_exp(n)%mk(io2l(io_col0),k)=matrix_exp(n)%mk(io2l(io_col0),k)+loc_vts(n)%mk(io2l(io_col0),k)
          case(2)
            matrix_exp(n)%mk(io2l(io_col1),k)=matrix_exp(n)%mk(io2l(io_col1),k)+loc_vts(n)%mk(io2l(io_col1),k)
          case(3)
            matrix_exp(n)%mk(io2l(io_col2),k)=matrix_exp(n)%mk(io2l(io_col2),k)+loc_vts(n)%mk(io2l(io_col2),k)
          case(4)
            matrix_exp(n)%mk(io2l(io_col3),k)=matrix_exp(n)%mk(io2l(io_col3),k)+loc_vts(n)%mk(io2l(io_col3),k)
          case(5)
            matrix_exp(n)%mk(io2l(io_col4),k)=matrix_exp(n)%mk(io2l(io_col4),k)+loc_vts(n)%mk(io2l(io_col4),k)
          case(6)
            matrix_exp(n)%mk(io2l(io_col5),k)=matrix_exp(n)%mk(io2l(io_col5),k)+loc_vts(n)%mk(io2l(io_col5),k)
          end select
        end do
      end do
    end do

    matrix_avg_count=matrix_avg_count+1 ! keep track of number of steps integrated
    matrix_vocn_n=matrix_vocn_n+1  ! one full initialise/recover cycle complete so advance counter

    if(mod(real(matrix_vocn_n),(conv_kocn_ksedgem/par_data_TM_avg_n)).eq.0)then! if at set point, average results, write to file, advance some control counters, n.b. conv_kocn_ksedgem = n_timsteps!!

      call matrix_recover_exp(matrix_k)

      if(matrix_season.eq.par_data_TM_avg_n)then !
        matrix_season=1 ! need to reset season
      else
        matrix_season=matrix_season+1 ! otherwise advance season
      end if

      matrix_avg_count=0 ! since we have zeroed the matrix array, set the averaging count to 0
    end if ! end of experiment recovering call (mod(real...))

  end if ! end of store/write call (matrix_go)

  end if ! write index, matrix_k.lt.1

  end if ! ctrl_data_diagnose_TM


  ! ---------------------------------------------------------- !
  ! OCEAN TRACER UPDATE
  ! ---------------------------------------------------------- !
  ! NOTE: units of aqueous concentration (mol kg-1)
  If (.NOT. error_stop) then
     IF (ctrl_debug_lvl1) print*, '*** OCEAN TRACER UPDATE ***'
     ! ---------------------------------------------------- !
     ! (0) ORIGINAL TRACER INVENTORIES
     ! ---------------------------------------------------- !
     ! ---------------------------------------------------- ! calculate original (OLD) mean ocean salinity
     loc_partialtot(:) = 0.0
     loc_ocn_mean_S_OLD = 0.0
     do n=1,n_vocn
        loc_k1 = vocn(n)%k1
        loc_partialtot(n) = sum(vocn(n)%mk(2,loc_k1:n_k)*vphys_ocn(n)%mk(ipo_V,loc_k1:n_k))*loc_ocn_rtot_V
     end do
     loc_ocn_mean_S_OLD = sum(loc_partialtot(:))
     loc_ocn_rmean_S_OLD = 1.0/loc_ocn_mean_S_OLD
     ! ---------------------------------------------------- ! calculate original (OLD) BIOGEM tracer inventories
     loc_partialtot(:) = 0.0
     loc_ocn_tot_OLD(:) = 0.0
     DO l=3,n_l_ocn
        do n=1,n_vocn
           loc_k1 = vocn(n)%k1
           loc_partialtot(n) = sum(vocn(n)%mk(l,loc_k1:n_k)*vphys_ocn(n)%mk(ipo_M,loc_k1:n_k))
        end DO
        loc_ocn_tot_OLD(l) = sum(loc_partialtot(:))
     end do
     ! ---------------------------------------------------- !
     ! (1) SALINITY-ADJUST (CIRCULATION-UPDATED) GOLDSTEIN <loc_vts> TRACER ARRAY
     ! ---------------------------------------------------- !
     ! NOTE: copy T,S for completeness
     ! NOTE: no offset (array: <tstoocn_offset()>) required for biogeochem-only tracers
     do n=1,n_vocn
        loc_k1 = vocn(n)%k1
        DO k=n_k,loc_k1,-1
           loc_vocn(n)%mk(1:2,k)       = loc_vts(n)%mk(1:2,k) + tstoocn_offset(1:2)
           loc_vocn(n)%mk(3:n_l_ocn,k) = loc_vts(n)%mk(3:n_l_ocn,k)*vocn(n)%mk(2,k)*loc_ocn_rmean_S_OLD
        end DO
     end do
     ! ---------------------------------------------------- ! calculate new salinity-adjusted biogeochem tracer inventory
     ! NOTE: be sure to avoid potential divide-by-zero problems
     loc_partialtot(:) = 0.0
     loc_ocn_tot_NEW(:) = 0.0
     DO l=3,n_l_ocn
        do n=1,n_vocn
           loc_k1 = vocn(n)%k1
           loc_partialtot(n) = sum(loc_vocn(n)%mk(l,loc_k1:n_k)*vphys_ocn(n)%mk(ipo_M,loc_k1:n_k))
        end do
        loc_ocn_tot_NEW(l) = sum(loc_partialtot(:))
        if (abs(loc_ocn_tot_NEW(l)) < const_real_nullsmall) then
           loc_ocn_rtot_NEW(l) = const_real_zero
        else
           loc_ocn_rtot_NEW(l) = 1.0/loc_ocn_tot_NEW(l)
        end if
     end do
     ! ---------------------------------------------------- !
     ! (2) UPDATE <ocn> T,S
     ! ---------------------------------------------------- !
     do n=1,n_vocn
        loc_k1 = vocn(n)%k1
        DO k=n_k,loc_k1,-1
           DO l=1,2
              If (ctrl_force_GOLDSTEInTS) then
                 loc_vocn(n)%mk(l,k) = loc_vts(n)%mk(l,k) + tstoocn_offset(l) + vdocn(n)%mk(l,k)
                 vocn(n)%mk(l,k) = loc_vocn(n)%mk(l,k)
              else
                 vocn(n)%mk(l,k) = loc_vocn(n)%mk(l,k) + vdocn(n)%mk(l,k)
              end if
           end do
        end DO
     end do
     ! ---------------------------------------------------- ! re-calculate mean ocean salinity
     ! NOTE: calculate loc_ocn_mean_S_NEW with ocean volume, because it is assumed invarient
     loc_partialtot(:) = 0.0
     loc_ocn_mean_S_NEW = 0.0
     do n=1,n_vocn
        loc_k1 = vocn(n)%k1
        loc_partialtot(n) = sum(vocn(n)%mk(2,loc_k1:n_k)*vphys_ocn(n)%mk(ipo_V,loc_k1:n_k))*loc_ocn_rtot_V
     end do
     loc_ocn_mean_S_NEW = sum(loc_partialtot(:))
     ! ---------------------------------------------------- ! calc. relative change in global salinity
     loc_Sratio = loc_ocn_mean_S_NEW/loc_ocn_mean_S_OLD
     loc_rSratio = 1.0/loc_Sratio
     ! ---------------------------------------------------- !
     ! (3) ADJUST BIOGEOCHEM FIELDS
     ! ---------------------------------------------------- !
     ! (3a): adjust biogeochemal cell masses according to changes in global salinity inventory
     ! (3b): correct the <ocn> biogeochem tracer inventories and then update with biogeochem fluxes
     !       salinity normalize <ts> biogeochem tracers and copy to <ts1>
     ! NOTE: zero the global remin array after having added its contents to the ocean tracer array
     do n=1,n_vocn
        loc_i  = vocn(n)%i
        loc_j  = vocn(n)%j
        loc_k1 = vocn(n)%k1 ! index of deepest layer at i,j (surface = n_k)
        ! ------------------------------------------------- ! add ECOGEM remin and part arrays to dBIOGEM arrays
        If (flag_ecogem) then
           DO l=3,n_l_ocn
              io = conv_iselected_io(l)
              vdocn(n)%mk(l,loc_k1:n_k) = vdocn(n)%mk(l,loc_k1:n_k) + dum_egbg_sfcremin(io,loc_i,loc_j,loc_k1:n_k)
           end do
           DO l=1,n_l_sed
              is = conv_iselected_is(l)
              vdbio_part(n)%mk(l,loc_k1:n_k) = vdbio_part(n)%mk(l,loc_k1:n_k) + dum_egbg_sfcpart(is,loc_i,loc_j,loc_k1:n_k)
           end do
        end If
        ! ------------------------------------------------- !
        DO k=n_k,loc_k1,-1
           !
           vocn(n)%mk(3:n_l_ocn,k) = &
                & (loc_ocn_tot_OLD(3:n_l_ocn)*loc_ocn_rtot_NEW(3:n_l_ocn))*loc_vocn(n)%mk(3:n_l_ocn,k) + &
                & vdocn(n)%mk(3:n_l_ocn,k)
           vocn(n)%mk(3:n_l_ocn,k) = loc_Sratio*vocn(n)%mk(3:n_l_ocn,k)
           ! ---------------------------------------------- ! adjust particulate tracer concentrations
           vbio_part(n)%mk(1:n_l_sed,k) = vbio_part(n)%mk(1:n_l_sed,k) + vdbio_part(n)%mk(1:n_l_sed,k)
           vbio_part(n)%mk(1:n_l_sed,k) = loc_Sratio*vbio_part(n)%mk(1:n_l_sed,k)
           ! ---------------------------------------------- ! update GOLDSTEIn tracer arrays and salinity-normalize
           loc_vts(n)%mk(1:2,k)       = loc_vocn(n)%mk(1:2,k) - tstoocn_offset(1:2)
           loc_vts(n)%mk(3:n_l_ocn,k) = (loc_ocn_mean_S_NEW/vocn(n)%mk(2,k))*vocn(n)%mk(3:n_l_ocn,k)
        end DO
     end do
     do n=1,n_vocn
        loc_k1 = vocn(n)%k1
        vphys_ocn(n)%mk(ipo_M,loc_k1:n_k) = loc_rSratio*vphys_ocn(n)%mk(ipo_M,loc_k1:n_k)
        vphys_ocn(n)%mk(ipo_rM,loc_k1:n_k) = loc_Sratio*vphys_ocn(n)%mk(ipo_rM,loc_k1:n_k)
     end do
  ! ---------------------------------------------------------- !
  ! MUFFIN MATRIX II
  ! ---------------------------------------------------------- !
  ! initialise colour tracer in ts (going to goldstein)
  ! n.b. matrix_count & matrix_k are set in biogem_lib

  if(ctrl_data_diagnose_TM)THEN
    if(loc_yr.ge.par_data_TM_start)then

      if(matrix_vocn_n.eq.0)then ! catch issue when matrix_vocn_n=0 initally
        print*,'>>> Initialising transport matrix at depth level:',matrix_k
      end if

      if(mod(matrix_vocn_n,conv_kocn_ksedgem).eq.0 .and. matrix_vocn_n.ne.0)then ! once 96 steps have been completed, n.b. conv_kocn_ksedgem = n_timesteps!!
        matrix_k=matrix_k-1 ! decrement matrix_k for next time
        if(matrix_k.gt.0) print*,'>>> Initialising transport matrix at depth level:',matrix_k
      end if

      ! initialising grid_boxes with 1 mol kg-1 of colour tracer
      do n=1,n_vocn
        loc_k1=loc_vocn(n)%k1
        ! check k level for matrix is not in sediment
        if (matrix_k.ge.loc_k1) then
          matrix_tracer=mod(2*loc_vts(n)%j-1+mod(loc_vts(n)%i-1,6),6)+1 ! get tracer number for i j
          select case (matrix_tracer)
          CASE(1)
            loc_vts(n)%mk(io2l(io_col0),matrix_k)=1.0
          CASE(2)
            loc_vts(n)%mk(io2l(io_col1),matrix_k)=1.0
          CASE(3)
            loc_vts(n)%mk(io2l(io_col2),matrix_k)=1.0
          CASE(4)
            loc_vts(n)%mk(io2l(io_col3),matrix_k)=1.0
          CASE(5)
            loc_vts(n)%mk(io2l(io_col4),matrix_k)=1.0
          CASE(6)
            loc_vts(n)%mk(io2l(io_col5),matrix_k)=1.0
          end select
        end if
      end do


      if(matrix_go.eq.0)then
        matrix_go=1 ! flag for starting out of sync matrix loops
      end if

    end if ! par_data_TM_start
  end if ! ctrl_data_diagnose_TM

     ! ---------------------------------------------------- !
     ! (4) SET DUMMARY VARIABLE VALUES FOR RETURN
     ! ---------------------------------------------------- !
     ! ---------------------------------------------------- ! GOLDSTEIN arrays
     dum_ts(:,:,:,:) = fun_lib_conv_vocnTOts(loc_vts(:))
     dum_ts1(:,:,:,:) = dum_ts(:,:,:,:)
     ! ---------------------------------------------------- ! ECOGEM interface array
     do n=1,n_vocn
        loc_i  = vocn(n)%i
        loc_j  = vocn(n)%j
        loc_k1 = vocn(n)%k1
        DO l=1,n_l_ocn
           io = conv_iselected_io(l)
           dum_egbg_sfcocn(io,loc_i,loc_j,loc_k1:n_k) = vocn(n)%mk(l,loc_k1:n_k)
        end DO
     end do
     ! ### temporary v -> 3D conversion ########################################################################################## !
     ocn(:,:,:,:) = fun_lib_conv_vocnTOocn(vocn(:))
     bio_part(:,:,:,:) = fun_lib_conv_vsedTOsed(vbio_part(:))
     phys_ocn(ipo_M,:,:,:) = loc_rSratio*phys_ocn(ipo_M,:,:,:)
     phys_ocn(ipo_rM,:,:,:) = loc_Sratio*phys_ocn(ipo_rM,:,:,:)
     ! ########################################################################################################################### !
  end If
  ! ---------------------------------------------------------- !
  ! CLEAN UP
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! deallocate arrays
  DEALLOCATE(loc_vocn,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(loc_vts,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(loc_partialtot,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
end subroutine biogem_tracercoupling
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BIOGEM FORCINGS
subroutine biogem_forcing( &
     & dum_genie_clock     &
     & )
  USE biogem_lib
  USE biogem_box
  USE biogem_data
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  integer(kind=8),INTENT(IN)::dum_genie_clock                    ! genie clock (ms since start) NOTE: 8-byte integer
  ! LOCAL VARIABLES
  INTEGER::l,io,ia,is                                            ! counting indices
  real::loc_t                                                    ! local time

  ! *** CALCULATE GEM TIME ***
  ! update model time
  ! NOTE: par_misc_t_runtime is counted DOWN in years
  !       => for BIOGEM, the 'end of the world' occurs when time reaches zero
  loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
  ! *** UPDATE FORCING TIME SERIES DATA ***
  IF (ctrl_debug_lvl1) print*, '*** UPDATE FORCING TIME SERIES DATA ***'
  ! recalculate time-varying restoring and flux forcings of the system & fractional relaxation
  ! ATMOSPHERIC TRACERS (applied at the ocean-atmospere interface)
  DO l=3,n_l_atm
     ia = conv_iselected_ia(l)
     IF (force_restore_atm_select(ia)) THEN
        CALL sub_update_force_restore_atm(loc_t,ia)
     END IF
     IF (force_flux_atm_select(ia)) THEN
        CALL sub_update_force_flux_atm(loc_t,ia)
     END IF
  END DO
  ! OCEAN TRACERS
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     IF (force_restore_ocn_select(io)) THEN
        CALL sub_update_force_restore_ocn(loc_t,io)
     END IF
     IF (force_flux_ocn_select(io)) THEN
        CALL sub_update_force_flux_ocn(loc_t,io)
     END IF
  END DO
  ! SEDIMENT TRACERS (applied at the ocean surface)
  DO l=1,n_l_sed
     is = conv_iselected_is(l)
     IF (force_flux_sed_select(is)) THEN
        CALL sub_update_force_flux_sed(loc_t,is)
     END IF
  END DO
end subroutine biogem_forcing
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BIOGEM LOOP SUBROUTINE - CLIMATE STATE UPDATE
subroutine biogem_climate( &
     & dum_hght_sic,            &
     & dum_frac_sic,            &
     & dum_cost,                &
     & dum_solfor,              &
     & dum_fxsw,                &
     & dum_uvw,                 &
     & dum_tau,                 &
     & dum_psi,                 &
     & dum_uv,                  &
     & dum_usurf,               &
     & dum_mld,                 &
     & dum_evap,                &
     & dum_pptn,                &
     & dum_solconst,            &
     & dum_diffv,               &
     & dum_dzrho,               &
     & dum_rho_go               &
     & )

  USE biogem_lib
  USE biogem_box
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  real,dimension(n_i,n_j),INTENT(in)::dum_hght_sic               ! sea-ice height (2-D)
  real,dimension(n_i,n_j),INTENT(in)::dum_frac_sic               ! sea-ice fractional cover (2-D)
  real,dimension(n_i,n_j),INTENT(inout)::dum_cost                ! convective frequency something-or-other whatsit thingy (2-D)
  REAL,DIMENSION(n_j),INTENT(in)::dum_solfor                     ! potential solar radiation incident at Earth's surface
  REAL,DIMENSION(n_i,n_j),INTENT(in)::dum_fxsw                   ! actual solar radiation incident at Earth's surface
  REAL,DIMENSION(3,n_i,n_j,n_k),INTENT(in)::dum_uvw              ! GOLDSTEIN velocity field (3-D)
  REAL,DIMENSION(2,n_i,n_j),INTENT(in)::dum_tau                  ! GOLDSTEIN wind stress field (2-D)
  REAL,DIMENSION(0:n_i,0:n_j),INTENT(in)::dum_psi                ! GOLDSTEIN barotropic streamfunction (2-D)
  REAL,DIMENSION(2,n_i,n_j),INTENT(in)::dum_uv                   ! EMBM wind velocity field (2-D)
  REAL,DIMENSION(n_i,n_j),INTENT(in)::dum_usurf                  ! EMBM wind speed field (2-D)
  real,dimension(n_i,n_j),INTENT(in)::dum_mld                    ! mixed layer depth
  real,dimension(n_i,n_j),INTENT(in)::dum_evap                   ! evap
  real,dimension(n_i,n_j),INTENT(in)::dum_pptn                   ! precip
  REAL,INTENT(inout)::dum_solconst                               ! solar constant
  REAL,DIMENSION(n_i,n_j,n_k),INTENT(in)::dum_diffv              ! vertical diffusivity
  REAL,DIMENSION(n_i,n_j,n_k),INTENT(in)::dum_dzrho              ! density gradient
  REAL,DIMENSION(n_i,n_j,n_k),INTENT(in)::dum_rho_go             ! density from goldstein

  ! LOCAL VARIABLES
  INTEGER::i,j,k
  integer::loc_k1                                                ! local topography
  real::loc_tau_scale,loc_diff_scale,loc_dzrho_scale             ! dimensional scaling factors
  real,dimension(n_i,n_j)::locij_seaice_V_old                    ! sea-ice volume

  ! *** INITIALIZE ***
  ! calculate local variables
  loc_tau_scale = goldstein_rh0sc*goldstein_dsc*goldstein_usc*goldstein_fsc/goldstein_scf
  loc_diff_scale = (goldstein_usc*goldstein_dsc*goldstein_dsc)/const_rEarth
  loc_dzrho_scale = goldstein_rh0sc/goldstein_dsc
  ! calculate sea-ice volume at previous (BIOGEM) time-step
  DO i=1,n_i
     DO j=1,n_j
        loc_k1 = goldstein_k1(i,j)
        IF (n_k >= loc_k1) THEN
           locij_seaice_V_old(i,j) = phys_ocnatm(ipoa_seaice,i,j)*phys_ocnatm(ipoa_A,i,j)*phys_ocnatm(ipoa_seaice_th,i,j)
        end IF
     end DO
  end DO

  ! *** UPDATE CLIMATE PROPERTIES ***
  ! (i.e., just the physical properties of the ocean and ocean-atmosphere interface that BIOGEM needs to know about)
  DO i=1,n_i
     DO j=1,n_j
        loc_k1 = goldstein_k1(i,j)
        IF (n_k >= loc_k1) THEN
           ! wind stress
           phys_ocnatm(ipoa_tau_u,i,j) = loc_tau_scale*dum_tau(1,i,j)
           phys_ocnatm(ipoa_tau_v,i,j) = loc_tau_scale*dum_tau(2,i,j)
           ! convective 'cost'
           phys_ocnatm(ipoa_cost,i,j)  = dum_cost(i,j)
           ! MLD
           phys_ocnatm(ipoa_mld,i,j)   = dum_mld(i,j)
        end IF
        do k = loc_k1,n_k
           ! density
           phys_ocn(ipo_rho,i,j,k) = fun_calc_rho(ocn(io_T,i,j,k),ocn(io_S,i,j,k))
           ! velocity
           phys_ocn(ipo_gu,i,j,k) = dum_uvw(1,i,j,k)
           phys_ocn(ipo_gv,i,j,k) = dum_uvw(2,i,j,k)
           phys_ocn(ipo_gw,i,j,k) = dum_uvw(3,i,j,k)
           ! vertical diffusivity
           phys_ocn(ipo_diffv,i,j,k) = loc_diff_scale*dum_diffv(i,j,k)
           ! density gradient
           phys_ocn(ipo_dzrho,i,j,k) = loc_dzrho_scale*dum_dzrho(i,j,k)
           ! density from goldstein
           phys_ocn(ipo_rho_go,i,j,k) = goldstein_rh0sc*dum_rho_go(i,j,k)
        end do
        ! solar insolation
        phys_ocnatm(ipoa_solfor,i,j) = dum_solfor(j)
        phys_ocnatm(ipoa_fxsw,i,j)   = dum_fxsw(i,j)
        ! wind velocity
        phys_ocnatm(ipoa_u,i,j) = dum_uv(1,i,j)
        phys_ocnatm(ipoa_v,i,j) = dum_uv(2,i,j)
        ! wind speed
        phys_ocnatm(ipoa_usurf,i,j) = dum_usurf(i,j)
        ! evap and precip
        phys_ocnatm(ipoa_evap,i,j)  = dum_evap(i,j)
        phys_ocnatm(ipoa_pptn,i,j)  = dum_pptn(i,j)
     end DO
  end DO
  ! copy barotropic streamfunction
  diag_misc_psi(:,:) = dum_psi(:,:)
  ! force to prescribed fractional sea-ice cover if requested
  if (ctrl_force_seaice) then
     phys_ocnatm(ipoa_seaice,:,:) = par_phys_seaice(:,:)
  else
     phys_ocnatm(ipoa_seaice,:,:) = dum_frac_sic
  end if
  phys_ocnatm(ipoa_seaice_th,:,:) = dum_hght_sic
  ! calculate change in sea-ice volume
  DO i=1,n_i
     DO j=1,n_j
        loc_k1 = goldstein_k1(i,j)
        IF (n_k >= loc_k1) THEN
           phys_ocnatm(ipoa_seaice_dV,i,j) = &
                & phys_ocnatm(ipoa_seaice,i,j)*phys_ocnatm(ipoa_A,i,j)*phys_ocnatm(ipoa_seaice_th,i,j) - &
                & locij_seaice_V_old(i,j)
        end IF
     end DO
  end DO
  ! force to prescribed wind-speed if requested
  ! otherwise, calculate wind speed from wind stress
  if (ctrl_force_windspeed) then
     phys_ocnatm(ipoa_wspeed,:,:) = par_phys_windspeed(:,:)
  else
     phys_ocnatm(ipoa_wspeed,:,:) = fun_calc_u()
  end if
  ! replace solar constant(!)
  if (ctrl_force_solconst) dum_solconst = force_solconst_sig(2,par_data_save_sig_i)
  ! make internal copy of solar constant
  phys_solar_constant = dum_solconst
  ! reset cost function
  dum_cost(:,:) = 0.0

end subroutine biogem_climate
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BIOGEM LOOP SUBROUTINE - CLIMATE STATE UPDATE
subroutine biogem_climate_sol( &
     & dum_solfor,                  &
     & dum_fxsw,                    &
     & dum_solconst                 &
     & )
  USE biogem_lib
  USE biogem_box
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  REAL,DIMENSION(n_j),INTENT(in)::dum_solfor                     ! potential solar radiation incident at Earth's surface
  REAL,DIMENSION(n_i,n_j),INTENT(in)::dum_fxsw                   ! actual solar radiation incident at Earth's surface
  REAL,INTENT(inout)::dum_solconst                               ! solar constant

  ! LOCAL VARIABLES
  INTEGER::i,j

  ! *** UPDATE CLIMATE PROPERTIES ***
  ! (i.e., just the physical properties of the ocean and ocean-atmosphere interface that BIOGEM needs to know about)
  DO i=1,n_i
     DO j=1,n_j
        ! solar insolation
        phys_ocnatm(ipoa_solfor,i,j) = dum_solfor(j)
        phys_ocnatm(ipoa_fxsw,i,j)   = dum_fxsw(i,j)
     end DO
  end DO
  ! replace solar constant(!)
  if (ctrl_force_solconst) dum_solconst = force_solconst_sig(2,par_data_save_sig_i)
  ! make internal copy of solar constant
  phys_solar_constant = dum_solconst

end subroutine biogem_climate_sol
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BIOGEM pCO2 diagnostics (for GEMlite)
subroutine diag_biogem_pCO2( &
     & dum_sfcatm1,          &
     & dum_pCO2              &
     & )
  USE biogem_lib
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  REAL,DIMENSION(n_atm,n_i,n_j),INTENT(in)::dum_sfcatm1          !
  REAL,INTENT(inout)::dum_pCO2                                   !
  ! calculate pCO2
  dum_pCO2 = conv_mol_umol*SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia_pCO2,:,:))/sum(phys_ocnatm(ipoa_A,:,:))
end subroutine diag_biogem_pCO2
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BIOGEM ECOGEM diagnostics
subroutine diag_biogem_ecogem( &
     & dum_egbg_sfcpart,       &
     & dum_egbg_sfcremin       &
     & )
  USE biogem_lib
  USE biogem_box
  USE genie_util, ONLY: check_iostat
  IMPLICIT NONE
  ! ---------------------------------------------------------- !
  ! DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  real,intent(in),dimension(n_sed,n_i,n_j,n_k)::dum_egbg_sfcpart  ! ecology-interface: particulate composition change; ocn grid
  real,intent(in),dimension(n_ocn,n_i,n_j,n_k)::dum_egbg_sfcremin ! ecology-interface: ocean tracer composition change; ocn grid
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  integer::io,is,l
  integer::loc_m,loc_tot_m
  real::loc_tmp_M
  real::loc_ocn_M
  real,dimension(n_sed)::loc_DOM_M
  real,dimension(n_sed)::loc_POM_M
  ! ---------------------------------------------------------- !
  ! INITIALIZE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  loc_tmp_M = 0.0
  loc_ocn_M = sum(phys_ocn(ipo_M,:,:,n_k))
  loc_DOM_M(:) = 0.0
  loc_POM_M(:) = 0.0
  ! ---------------------------------------------------------- !
  ! CALCULATE GLOBAL FLUXES
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! DOM
  if (ocn_select(io_DOM_C)) then
     DO l=3,n_l_ocn
        io = conv_iselected_io(l)
        loc_tot_m = conv_DOM_POM_i(0,io)
        do loc_m=1,loc_tot_m
           loc_tmp_M = sum(phys_ocn(ipo_M,:,:,n_k)*dum_egbg_sfcremin(io,:,:,n_k))
           is = conv_DOM_POM_i(loc_m,io)
           loc_DOM_M(is) = loc_DOM_M(is) + conv_DOM_POM(is,io)*loc_tmp_M
        end do
     end do
  end if
  ! ---------------------------------------------------------- ! RDOM
  if (ocn_select(io_RDOM_C)) then
     DO l=3,n_l_ocn
        io = conv_iselected_io(l)
        loc_tot_m = conv_RDOM_POM_i(0,io)
        do loc_m=1,loc_tot_m
           loc_tmp_M = sum(phys_ocn(ipo_M,:,:,n_k)*dum_egbg_sfcremin(io,:,:,n_k))
           is = conv_DOM_POM_i(loc_m,io)
           loc_DOM_M(is) = loc_DOM_M(is) + conv_RDOM_POM_i(is,io)*loc_tmp_M
        end do
     end do
  end if
  ! ---------------------------------------------------------- ! POM
  DO l=1,n_l_sed
     is = conv_iselected_is(l)
     loc_POM_M(is) = sum(phys_ocn(ipo_M,:,:,n_k)*dum_egbg_sfcpart(is,:,:,n_k))
  end do
  ! ---------------------------------------------------------- ! SAVE DOM FRACTION
  int_fracdom(:) = 0.0
  DO l=1,n_l_sed
     is = conv_iselected_is(l)
     if (loc_DOM_M(is) > const_real_nullsmall) then
        int_fracdom(is) = loc_DOM_M(is)/(loc_POM_M(is)+loc_DOM_M(is))
     end if
  end do
  ! ---------------------------------------------------------- ! COPY 2D
  diag_ecogem_part(:,:,:)  = dum_egbg_sfcpart(:,:,:,n_k)
  diag_ecogem_remin(:,:,:) = dum_egbg_sfcremin(:,:,:,n_k)
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
end subroutine diag_biogem_ecogem
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! RESTART BioGeM (save data)
SUBROUTINE biogem_save_restart(dum_genie_clock)
  USE biogem_lib
  USE biogem_data_netCDF
  USE genie_util, ONLY:check_unit,check_iostat
  IMPLICIT NONE
  ! ---------------------------------------------------------- !
  ! DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  integer(kind=8),INTENT(IN)::dum_genie_clock                  ! genie clock (milliseconds since start) NOTE: 8-byte integer
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  integer::l,i,j,io,is                                         ! tracer counter
  integer::loc_iou
  CHARACTER(len=255)::loc_filename                             ! guess ...
  integer::ios                                                 ! file checks
  integer::loc_k1                                              ! local topography
  integer::loc_i,loc_tot_i                                     ! tracer array conversion indices
  real::loc_yr                                                 !
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::loc_docn                  ! local ocean tracer (change) array
  ! ---------------------------------------------------------- !
  ! ENSURE TRACER CONSERVATION
  ! ---------------------------------------------------------- !
  if (ctrl_force_sed_closedsystem) then
     ! 'flush' settling solid sediment interface array
     loc_docn(:,:,:,:) = 0.0
     DO i=1,n_i
        DO j=1,n_j
           loc_k1 = goldstein_k1(i,j)
           IF (n_k >= loc_k1) THEN
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 loc_tot_i = conv_sed_ocn_i(0,is)
                 do loc_i=1,loc_tot_i
                    ! convert settling solids at sediment interface to dissolved constituents
                    io = conv_sed_ocn_i(loc_i,is)
                    loc_docn(io,i,j,loc_k1) = loc_docn(io,i,j,loc_k1) + &
                         & phys_ocn(ipo_rM,i,j,loc_k1)*conv_sed_ocn(io,is)*bio_settle(is,i,j,loc_k1)
                    ! prevent return of dissolved Fe
                    loc_docn(io_Fe,i,j,loc_k1) = 0.0
                 end do
              end DO
           end if
        end DO
     end DO
     ! correct tracer inventory for restart save
     ocn(:,:,:,:) = ocn(:,:,:,:) + loc_docn(:,:,:,:)
     bio_settle(:,:,:,:) = 0.0
  end if
  ! ---------------------------------------------------------- ! calculate local time (years)
  loc_yr = real(dum_genie_clock)/(1000.0*conv_yr_s)
  ! ---------------------------------------------------------- ! test for restart format
  IF (ctrl_ncrst) THEN
     ! ------------------------------------------------------- !
     ! SAVE RESTART DATA: NETCDF FORMAT
     ! ------------------------------------------------------- !
     string_ncrst = TRIM(par_outdir_name)//trim(par_ncrst_name)
     ncrst_ntrec = 0
     call sub_data_netCDF_ncrstsave(trim(string_ncrst),loc_yr,loc_iou)
  else
     ! ------------------------------------------------------- !
     ! SAVE RESTART DATA: BINARY DUMP FORMAT
     ! ------------------------------------------------------- !
     ! binary dump data format
     ! NOTE: data is saved unformatted for minimal file size
     !       also means that arrays can be written directly to file without needing to loop thought data
     loc_filename = TRIM(par_outdir_name)//trim(par_outfile_name)
     call check_unit(out,__LINE__,__FILE__)
     OPEN(unit=out,status='replace',file=loc_filename,form='unformatted',action='write',iostat=ios)
     call check_iostat(ios,__LINE__,__FILE__)
     WRITE(unit=out,iostat=ios) &
          & n_l_ocn,                                           &
          & (conv_iselected_io(l),l=1,n_l_ocn),                &
          & (ocn(conv_iselected_io(l),:,:,:),l=1,n_l_ocn),     &
          & n_l_sed,                                           &
          & (conv_iselected_is(l),l=1,n_l_sed),                &
          & (bio_part(conv_iselected_is(l),:,:,:),l=1,n_l_sed)
     call check_iostat(ios,__LINE__,__FILE__)
     close(unit=out,iostat=ios)
     call check_iostat(ios,__LINE__,__FILE__)
  end if
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
end SUBROUTINE biogem_save_restart
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! ORBITAL DATA RECORDING
SUBROUTINE diag_biogem_rec_orb(dum_genie_clock,dum_sfcatm1)
  USE biogem_lib
  USE genie_util, ONLY:check_unit,check_iostat
  IMPLICIT NONE
  ! ---------------------------------------------------------- !
  ! DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  integer(kind=8),INTENT(IN)::dum_genie_clock                  ! genie clock (milliseconds since start) NOTE: 8-byte integer
  real,intent(in),dimension(n_atm,n_i,n_j)::dum_sfcatm1        ! atmosphere-interface tracer composition; ocn grid
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  real::loc_yr                                                 ! local time (yrs)
  integer::n
  integer::nvar,nloc
  integer::loc_i,loc_j,loc_k
  integer::loc_istr
  real::loc_tot,loc_frac,loc_standard
  ! ---------------------------------------------------------- !
  ! RECORD VARIABLES
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! calculate local time (years)
  loc_yr = real(dum_genie_clock)/(1000.0*conv_yr_s)
  ! ---------------------------------------------------------- !
  n_orb_pts = n_orb_pts+1
  if (n_orb_pts > n_orb_pts_nmax) then
     CALL sub_report_error( &
          & 'biogem','diag_biogem_rec_orb', &
          & 'Maximum temporary record storage exceeded -- parameter: bg_n_orb_pts_nmax', &
          & 'CONTINUING and overwriting last data', &
          & (/real(n_orb_pts_nmax)/),.false. &
          & )
     n_orb_pts = n_orb_pts_nmax
  end if
  orb_pts_time(n_orb_pts) = loc_yr
  ! ---------------------------------------------------------- ! START LOOP
  DO nloc=1,n_orb_pts_nloc
     ! extract (i,j)
     loc_i = orb_pts_loc(nloc,1)
     loc_j = orb_pts_loc(nloc,2)

     DO nvar=1,n_orb_pts_nvar

        ! (1) deduce k value
        ! zero local k layer value
        loc_k = 0
        ! search for surface selected
        loc_istr = INDEX(orb_pts_var(nvar)(:),'sur')
        IF (loc_istr /= 0) THEN
           loc_k = n_k
        END IF
        ! search for benthic selected
        loc_istr = INDEX(orb_pts_var(nvar)(:),'ben')
        IF (loc_istr /= 0) THEN
           loc_k = goldstein_k1(loc_i,loc_j)
        END IF
        ! catch other selection ...
        IF (loc_k == 0) THEN
           loc_k = n_k
        END IF
        ! (2) search through variable groups and deduce variable
        ! (2a) -- ocean tracers
        do n=1,n_ocn
           loc_istr = INDEX(orb_pts_var(nvar)(:),'ocn')
           if (loc_istr /= 0) then
              loc_istr = INDEX(orb_pts_var(nvar)(:),trim(string_ocn(n))//'.')
              if (loc_istr /= 0) then
                 loc_istr = fun_find_str_i(trim(string_ocn(n)),string_ocn)
                 SELECT CASE (ocn_type(loc_istr))
                 case (n_itype_min:n_itype_max)
                    loc_tot      = ocn(ocn_dep(loc_istr),loc_i,loc_j,loc_k)
                    loc_frac     = ocn(loc_istr,loc_i,loc_j,loc_k)
                    loc_standard = const_standards(ocn_type(loc_istr))
                    orb_pts(n_orb_pts,nloc,nvar) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
                 case default
                    If (loc_istr == io_T) then
                       orb_pts(n_orb_pts,nloc,nvar) = ocn(loc_istr,loc_i,loc_j,loc_k) - const_zeroC
                    else
                       orb_pts(n_orb_pts,nloc,nvar) = ocn(loc_istr,loc_i,loc_j,loc_k)
                    end if
                 END SELECT
                 exit
              end if
           end if
        end do
        ! (2b) -- carb chem
        do n=1,n_carb
           loc_istr = INDEX(orb_pts_var(nvar)(:),trim(string_carb(n))//'.')
           if (loc_istr /= 0) then
              loc_istr = fun_find_str_i(trim(string_carb(n)),string_carb)
              orb_pts(n_orb_pts,nloc,nvar) = carb(loc_istr,loc_i,loc_j,loc_k)
              exit
           end if
        end do
        ! (2c) -- sediment tracers (particulate fluxes)
        do n=1,n_sed
           loc_istr = INDEX(orb_pts_var(nvar)(:),'sed')
           if (loc_istr /= 0) then
              loc_istr = INDEX(orb_pts_var(nvar)(:),trim(string_sed(n))//'.')
              if (loc_istr /= 0) then
                 loc_istr = fun_find_str_i(trim(string_sed(n)),string_sed)
                 SELECT CASE (sed_type(loc_istr))
                 case (n_itype_min:n_itype_max)
                    loc_tot      = bio_settle(sed_dep(loc_istr),loc_i,loc_j,loc_k)
                    loc_frac     = bio_settle(loc_istr,loc_i,loc_j,loc_k)
                    loc_standard = const_standards(sed_type(loc_istr))
                    orb_pts(n_orb_pts,nloc,nvar) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
                 case default
                    orb_pts(n_orb_pts,nloc,nvar) = bio_settle(loc_istr,loc_i,loc_j,loc_k)
                 END SELECT
                 exit
              end if
           end if
        end do
!!!! (2d) -- ocean physics
!!!do n=1,n_phys_ocn
!!!   loc_istr = INDEX(orb_pts_var(nvar)(:),trim(string_phys_ocn(n))//'.')
!!!   if (loc_istr /= 0) then
!!!      loc_istr = fun_find_str_i(trim(string_phys_ocn(n)),string_phys_ocn)
!!!      orb_pts(n_orb_pts,nloc,nvar) = phys_ocn(loc_istr,loc_i,loc_j,loc_k)
!!!      exit
!!!   end if
!!!end do
        ! (2e) -- ocean-atmosphere interface 'physics'
        do n=1,n_phys_ocnatm
           loc_istr = INDEX(orb_pts_var(nvar)(:),trim(string_phys_ocnatm(n))//'.')
           if (loc_istr /= 0) then
              loc_istr = fun_find_str_i(trim(string_phys_ocnatm(n)),string_phys_ocnatm)
              orb_pts(n_orb_pts,nloc,nvar) = phys_ocnatm(loc_istr,loc_i,loc_j)
              exit
           end if
        end do
        ! (2f) -- atmosphere tracers
        do n=1,n_atm
           loc_istr = INDEX(orb_pts_var(nvar)(:),'atm')
           if (loc_istr /= 0) then
              loc_istr = INDEX(orb_pts_var(nvar)(:),trim(string_atm(n))//'.')
              if (loc_istr /= 0) then
                 loc_istr = fun_find_str_i(trim(string_atm(n)),string_atm)
                 SELECT CASE (atm_type(loc_istr))
                 case (n_itype_min:n_itype_max)
                    loc_tot      = dum_sfcatm1(atm_dep(loc_istr),loc_i,loc_j)
                    loc_frac     = dum_sfcatm1(loc_istr,loc_i,loc_j)
                    loc_standard = const_standards(atm_type(loc_istr))
                    orb_pts(n_orb_pts,nloc,nvar) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
                 case default
                    orb_pts(n_orb_pts,nloc,nvar) = dum_sfcatm1(loc_istr,loc_i,loc_j)
                 END SELECT
                 exit
              end if
           end if
        end do

     end do

  end do
  ! ---------------------------------------------------------- ! END LOOP
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
end SUBROUTINE diag_biogem_rec_orb
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BIOGEM DIAGNOSTICS
subroutine diag_biogem( &
     & dum_genie_clock, &
     & dum_sfcatm1,     &
     & dum_gemlite      &
     & )
  USE biogem_lib
  USE biogem_box
  USE biogem_data
  USE biogem_data_ascii
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  integer(kind=8),INTENT(IN)::dum_genie_clock                           ! genie clock (milliseconds since start) * 8-byte integer *
  real,intent(in),dimension(n_atm,n_i,n_j)::dum_sfcatm1                 ! atmosphere-surface tracer composition; ocn grid
  logical,intent(in)::dum_gemlite                                       ! in GEMlite phase of cycle?
  ! LOCAL VARIABLES
  real::loc_t,loc_yr                                                    ! local time and time step BLAH actual year
  REAL,DIMENSION(0:n_j,0:n_k)::loc_opsi                                 !
  REAL,DIMENSION(0:n_j,0:n_k)::loc_opsia,loc_opsip,loc_zpsi             !
  real::loc_opsi_scale                                                  !
  REAL,DIMENSION(2)::loc_opsia_minmax,loc_opsip_minmax                  !

  ! calculate local variables
  loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
  ! calculate actual year (counting years Before Present or otherwise)
  IF (ctrl_misc_t_BP) THEN
     loc_yr = loc_t + par_misc_t_end
  ELSE
     loc_yr = par_misc_t_end - loc_t
  END IF
  ! calculate local opsi conversion constant
  loc_opsi_scale = goldstein_dsc*goldstein_usc*const_rEarth*1.0E-6

  ! *** RUN-TIME REPORTING ***
  ! run-time reporting
  ! NOTE: carry out an updated tracer inventory audit at this time (if selected)
  ! NOTE: adjusted reporting frequency to every year
  if_report: if ( &
       & ((abs(loc_yr - real(INT(loc_yr))) < conv_s_yr) .AND. ctrl_debug_lvl0) &
       & .OR. &
       & (par_misc_t_echo_header) &
       & .OR. &
       & (error_stop) &
       & ) then
     IF (ctrl_debug_lvl1) print*, '*** RUN-TIME REPORTING ***'
     ! run-time data echo-ing
     ! ### UN-COMMENT TO PERIODICALLY RE-PRINT HEADER INFORMATION ############################################################### !
     ! if (mod(par_data_save_sig_i,50) == 0) par_misc_t_echo_header = .TRUE.
     ! ########################################################################################################################## !
     if (error_stop) par_misc_t_echo_header = .TRUE.
     CALL sub_calc_psi(phys_ocn(ipo_gu:ipo_gw,:,:,:),loc_opsi,loc_opsia,loc_opsip,loc_zpsi,loc_opsia_minmax,loc_opsip_minmax)
     call sub_echo_runtime(loc_yr,loc_opsi_scale,loc_opsia_minmax,dum_sfcatm1(:,:,:),dum_gemlite)
     ! carry out tracer audit + echo max,min ocean tracer values (and location)
     ! NOTE: use audit switch to turn on/off
     IF (ctrl_audit) THEN
        CALL sub_echo_maxmin()
        CALL sub_audit_update()
     END IF
  end IF if_report

end subroutine diag_biogem
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BIOGEM DIAGNOSTICS
subroutine diag_biogem_timesync( &
     & dum_tseries,              &
     & dum_tslice,               &
     & dum_intseries,            &
     & dum_intslice,             &
     & dum_endseries,            &
     & dum_endslice              &
     & )
  USE biogem_lib
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  real,INTENT(out)::dum_tseries                                         ! time-series time
  real,INTENT(out)::dum_tslice                                          ! time-slice time
  logical,intent(out)::dum_intseries                                    ! integrate time-series data?
  logical,intent(out)::dum_intslice                                     ! integrate time-slice data?
  logical,intent(out)::dum_endseries                                    ! end of time-series data integration interval?
  logical,intent(out)::dum_endslice                                     ! end of time-slice data integration interval?

  ! *** RETURN TIME INFORMATON ***
  !
  dum_tseries   = par_misc_t_tseries
  dum_tslice    = par_misc_t_tslice
  dum_intseries = par_misc_t_intseries
  dum_intslice  = par_misc_t_intslice
  dum_endseries = par_misc_t_endseries
  dum_endslice  = par_misc_t_endslice

end subroutine diag_biogem_timesync
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! biogem DIAGNOSTICS - TIME-SLICE
SUBROUTINE diag_biogem_timeslice( &
     & dum_dts,                   &
     & dum_genie_clock,           &
     & dum_sfcatm1,               &
     & dum_sfxatm1,               &
     & dum_sfxocn1,               &
     & dum_sfcsed1,               &
     & dum_sfxsed1,               &
     & dum_sfxsumrok1,            &
     & dum_save,                  &
     & dum_gemlite                &
     & )
  USE biogem_lib
  USE biogem_box
  USE biogem_data
  USE biogem_data_ascii
  USE genie_util, ONLY:check_unit,check_iostat
  ! dummy arguments
  REAL,INTENT(IN)::dum_dts                                       ! biogem time-step length (seconds)
  integer(kind=8),INTENT(IN)::dum_genie_clock                    ! genie clock (milliseconds since start) NOTE: 8-byte integer
  REAL,DIMENSION(n_atm,n_i,n_j),INTENT(in)::dum_sfcatm1          ! atmosphere composition interface array
  REAL,DIMENSION(n_atm,n_i,n_j),INTENT(in)::dum_sfxatm1          ! atmospheric flux interface array
  REAL,DIMENSION(n_ocn,n_i,n_j),INTENT(in)::dum_sfxocn1          ! sediment dissolution flux interface array
  REAL,DIMENSION(n_sed,n_i,n_j),INTENT(in)::dum_sfcsed1          ! sediment composition interface array
  REAL,DIMENSION(n_sed,n_i,n_j),INTENT(in)::dum_sfxsed1          ! sediment rain flux interface array
  real,dimension(n_ocn,n_i,n_j),intent(in)::dum_sfxsumrok1       ! coastal (weathering) -> ocean flux; ocn grid
  logical,INTENT(IN)::dum_save                                   ! average and save data?
  logical,intent(in)::dum_gemlite                                ! in GEMlite phase of cycle?
  ! local variables
  INTEGER::i,j,k,l,io,ia,is
  integer::loc_k1                                                !
  real::loc_t,loc_dts,loc_dtyr                                   !
  real::loc_yr_save                                              !
  REAL,DIMENSION(0:n_j,0:n_k)::loc_opsi,loc_zpsi,loc_opsia,loc_opsip !
  REAL,DIMENSION(n_atm,n_i,n_j)::locij_focnatm                   ! local ocn->atm flux (atm tracer currency) (mol yr-1)
  REAL,DIMENSION(n_sed,n_i,n_j)::locij_focnsed                   ! local ocn->sed change (sed tracer currency) (mol)
  REAL,DIMENSION(n_ocn,n_i,n_j)::locij_fsedocn                   ! local sed->ocean change (ocn tracer currency) (mol)
  REAL,DIMENSION(2)::loc_opsia_minmax,loc_opsip_minmax           !
  real,dimension(1:3)::loc_FeFELL                                !
  INTEGER::ios
  integer::n,nloc,nvar
  CHARACTER(len=255)::loc_filename                           ! filename string
  CHARACTER(len=6)::loc_locstr                               !

  ! *** TIME-SLICE DATA UPDATE ***
  IF (ctrl_debug_lvl1) print*, '*** TIME-SLICE DATA UPDATE ***'
  ! update time slice data
  ! NOTE: carried out only when the local (BioGeM) time falls between a selected time slice time plus integration time,
  !       and the time slice time itself
  ! NOTE: do not construct and save a time slice if <par_data_save_timeslice_i> == 0,
  !       i.e., no valid (in-range) time slices have been requested in the time slice input file 'biogem_save_timeslice.dat',
  !       or the final requested time slice has been made
  ! calculate local model time and time step length
  loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
  loc_dts  = dum_dts
  loc_dtyr = loc_dts/conv_yr_s
  ! update status of 'end'
  par_misc_t_endslice = .false.

  if_save1: if (par_data_save_timeslice_i > 0) then

     if_save2: IF ( &
          & ((loc_t - (par_data_save_timeslice(par_data_save_timeslice_i) + par_data_save_slice_dt/2.0)) < -conv_s_yr) &
          & .OR. &
          & (error_stop) &
          & ) THEN
        par_misc_t_intslice = .true.
        par_misc_t_tslice = par_misc_t_end - par_data_save_timeslice(par_data_save_timeslice_i)

        int_t_timeslice       = int_t_timeslice       + loc_dtyr
        int_t_timeslice_TOT   = int_t_timeslice_TOT   + loc_dtyr
        int_t_timeslice_count = int_t_timeslice_count + 1

        if_save3: if (dum_save) then

           ! reconstruct local interface fluxes and update whole-ocean carbonate equilibrium
           IF (opt_select(iopt_select_carbchem)) THEN
              DO i=1,n_i
                 DO j=1,n_j
                    loc_k1 = goldstein_k1(i,j)
                    IF (n_k >= loc_k1) THEN
                       ! ocn->atm
                       ! NOTE: convert units from (mol m-2 s-1) to (mol yr-1)
                       DO l=3,n_l_atm
                          ia = conv_iselected_ia(l)
                          locij_focnatm(ia,i,j) = conv_yr_s*phys_ocnatm(ipoa_A,i,j)*dum_sfxatm1(ia,i,j)
                       end do
                       ! ocn->sed
                       ! NOTE: convert units from (mol m-2 s-1) to (mol per timestep)
                       DO l=1,n_l_sed
                          is = conv_iselected_is(l)
                          locij_focnsed(is,i,j) = loc_dts*phys_ocn(ipo_A,i,j,loc_k1)*dum_sfxsed1(is,i,j)
                       end DO
                       ! sed->ocn
                       ! NOTE: convert units from (mol m-2 s-1) to (mol per timestep)
                       DO l=3,n_l_ocn
                          io = conv_iselected_io(l)
                          locij_fsedocn(io,i,j) = loc_dts*phys_ocn(ipo_A,i,j,loc_k1)*dum_sfxocn1(io,i,j)
                       end do
                    end IF
                    DO k=goldstein_k1(i,j),n_k
                       ! calculate carbonate dissociation constants
                       CALL sub_calc_carbconst(           &
                            & phys_ocn(ipo_Dmid,i,j,k), &
                            & ocn(io_T,i,j,k),          &
                            & ocn(io_S,i,j,k),          &
                            & carbconst(:,i,j,k)        &
                            & )
                       ! adjust carbonate constants
                       if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
                          call sub_adj_carbconst(   &
                               & ocn(io_Ca,i,j,k),  &
                               & ocn(io_Mg,i,j,k),  &
                               & carbconst(:,i,j,k) &
                               & )
                       end if
                       ! re-estimate Ca and borate concentrations from salinity (if not selected and therefore explicitly treated)
                       IF (.NOT. ocn_select(io_Ca))  ocn(io_Ca,i,j,k)  = fun_calc_Ca(ocn(io_S,i,j,k))
                       IF (.NOT. ocn_select(io_B))   ocn(io_B,i,j,k)   = fun_calc_Btot(ocn(io_S,i,j,k))
                       IF (.NOT. ocn_select(io_SO4)) ocn(io_SO4,i,j,k) = fun_calc_SO4tot(ocn(io_S,i,j,k))
                       IF (.NOT. ocn_select(io_F))   ocn(io_F,i,j,k)   = fun_calc_Ftot(ocn(io_S,i,j,k))
                       ! re-calculate surface ocean carbonate chemistry
                       CALL sub_calc_carb(        &
                            & ocn(io_DIC,i,j,k),  &
                            & ocn(io_ALK,i,j,k),  &
                            & ocn(io_Ca,i,j,k),   &
                            & ocn(io_PO4,i,j,k),  &
                            & ocn(io_SiO2,i,j,k), &
                            & ocn(io_B,i,j,k),    &
                            & ocn(io_SO4,i,j,k),  &
                            & ocn(io_F,i,j,k),    &
                            & ocn(io_H2S,i,j,k),  &
                            & ocn(io_NH4,i,j,k),  &
                            & carbconst(:,i,j,k), &
                            & carb(:,i,j,k),      &
                            & carbalk(:,i,j,k)    &
                            & )
                       ! estimate Revelle factor
                       CALL sub_calc_carb_RF0(      &
                            & ocn(io_DIC,i,j,n_k),  &
                            & ocn(io_ALK,i,j,n_k),  &
                            & ocn(io_PO4,i,j,n_k),  &
                            & ocn(io_SiO2,i,j,n_k), &
                            & ocn(io_B,i,j,n_k),    &
                            & ocn(io_SO4,i,j,n_k),  &
                            & ocn(io_F,i,j,n_k),    &
                            & ocn(io_H2S,i,j,n_k),  &
                            & ocn(io_NH4,i,j,n_k),  &
                            & carbconst(:,i,j,n_k), &
                            & carb(:,i,j,n_k)       &
                            & )
                       ! re-calculate carbonate system isotopic properties
                       if (ocn_select(io_DIC_13C)) then
                          call sub_calc_carb_r13C(      &
                               & ocn(io_T,i,j,k),       &
                               & ocn(io_DIC,i,j,k),     &
                               & ocn(io_DIC_13C,i,j,k), &
                               & carb(:,i,j,k),         &
                               & carbisor(:,i,j,k)      &
                               & )
                       end IF
                       if (ocn_select(io_DIC_14C)) then
                          call sub_calc_carb_r14C(      &
                               & ocn(io_T,i,j,k),       &
                               & ocn(io_DIC,i,j,k),     &
                               & ocn(io_DIC_14C,i,j,k), &
                               & carb(:,i,j,k),         &
                               & carbisor(:,i,j,k)      &
                               & )
                       end IF
                    end do
                 end DO
              end DO
           end IF

           ! reconstruct local interface fluxes and update whole-ocean carbonate equilibrium
           if (ocn_select(io_Fe) .OR. ocn_select(io_TDFe)) then
              SELECT CASE (trim(opt_geochem_Fe))
              CASE ('hybrid')
                 diag_Fe(idiag_Fe_TDFe,:,:,:) = ocn(io_TDFe,:,:,:)
                 diag_Fe(idiag_Fe_TL,:,:,:)   = ocn(io_TL,:,:,:)
                 DO i=1,n_i
                    DO j=1,n_j
                       loc_k1 = goldstein_k1(i,j)
                       IF (n_k >= loc_k1) THEN
                          DO k=loc_k1,n_k
                             loc_FeFeLL(:) = fun_box_calc_geochem_Fe( &
                                  & ocn(io_TDFe,i,j,k),ocn(io_TL,i,j,k) &
                                  & )
                             diag_Fe(idiag_Fe_Fe,i,j,k)  = loc_FeFeLL(1)
                             diag_Fe(idiag_Fe_FeL,i,j,k) = loc_FeFeLL(2)
                             diag_Fe(idiag_Fe_L,i,j,k)   = loc_FeFeLL(3)
                          end DO
                       end IF
                    end DO
                 end DO
              CASE ('lookup_4D')
                 diag_Fe(idiag_Fe_TDFe,:,:,:) = ocn(io_TDFe,:,:,:)
                 diag_Fe(idiag_Fe_TL,:,:,:)   = ocn(io_TL,:,:,:)
                 DO i=1,n_i
                    DO j=1,n_j
                       loc_k1 = goldstein_k1(i,j)
                       IF (n_k >= loc_k1) THEN
                          DO k=loc_k1,n_k
                             loc_FeFeLL(1) = fun_box_calc_lookup_Fe_4D_Fe3( &
                                  & (/ ocn(io_T,i,j,k), carb(ic_H,i,j,k),   &
                                  & ocn(io_TDFe,i,j,k), ocn(io_TL,i,j,k) /) &
                                  & )
                             diag_Fe(idiag_Fe_Fe3,i,j,k)  = loc_FeFeLL(1)
                             loc_FeFeLL(1) = fun_box_calc_lookup_Fe_4D_geo( &
                                  & (/ ocn(io_T,i,j,k), carb(ic_H,i,j,k),   &
                                  & ocn(io_TDFe,i,j,k), ocn(io_TL,i,j,k) /) &
                                  & )
                             diag_Fe(idiag_Fe_geo,i,j,k)  = loc_FeFeLL(1)
                          end DO
                       end IF
                    end DO
                 end DO
              CASE default
                 diag_Fe(idiag_Fe_Fe,:,:,:)   = ocn(io_Fe,:,:,:)
                 diag_Fe(idiag_Fe_FeL,:,:,:)  = ocn(io_FeL,:,:,:)
                 diag_Fe(idiag_Fe_L,:,:,:)    = ocn(io_L,:,:,:)
                 diag_Fe(idiag_Fe_TDFe,:,:,:) = ocn(io_Fe,:,:,:) + ocn(io_FeL,:,:,:)
                 diag_Fe(idiag_Fe_TL,:,:,:)   = ocn(io_L,:,:,:)  + ocn(io_FeL,:,:,:)
              end SELECT
           end IF

           ! update time slice data - ocean
           ! NOTE: do not time increment weight quantities such as <int_bio_remin_timeslice> or <int_bio_settle_timeslice>,
           !       because they represent discrete increments rather than a flux or weightable concentration value
           !       (also note that non-dimensional quantities such as par_sed_type_frac will have to be dealt with later ...
           !         except in this specific case, it is already weighted by the time-step so it will integrate properly)
           int_ocn_timeslice(:,:,:,:)        = int_ocn_timeslice(:,:,:,:)        + loc_dtyr*ocn(:,:,:,:)
           int_bio_part_timeslice(:,:,:,:)   = int_bio_part_timeslice(:,:,:,:)   + loc_dtyr*bio_part(:,:,:,:)
           int_bio_settle_timeslice(:,:,:,:) = int_bio_settle_timeslice(:,:,:,:) + bio_settle(:,:,:,:)
           int_bio_remin_timeslice(:,:,:,:)  = int_bio_remin_timeslice(:,:,:,:)  + bio_remin(:,:,:,:)
           int_phys_ocn_timeslice(:,:,:,:)   = int_phys_ocn_timeslice(:,:,:,:)   + loc_dtyr*phys_ocn(:,:,:,:)
           int_carb_timeslice(:,:,:,:)       = int_carb_timeslice(:,:,:,:)       + loc_dtyr*carb(:,:,:,:)
           int_carbconst_timeslice(:,:,:,:)  = int_carbconst_timeslice(:,:,:,:)  + loc_dtyr*carbconst(:,:,:,:)
           int_carbisor_timeslice(:,:,:,:)   = int_carbisor_timeslice(:,:,:,:)   + loc_dtyr*carbisor(:,:,:,:)
           ! update time slice data - ocean-atmosphere interface
           int_sfcatm1_timeslice(:,:,:)      = int_sfcatm1_timeslice(:,:,:)     + loc_dtyr*dum_sfcatm1(:,:,:)
           int_focnatm_timeslice(:,:,:)      = int_focnatm_timeslice(:,:,:)     + loc_dtyr*locij_focnatm(:,:,:)
           int_phys_ocnatm_timeslice(:,:,:)  = int_phys_ocnatm_timeslice(:,:,:) + loc_dtyr*phys_ocnatm(:,:,:)
           ! update time slice data - ocean-sediment interface
           int_sfcsed1_timeslice(:,:,:)  = int_sfcsed1_timeslice(:,:,:) + loc_dtyr*dum_sfcsed1(:,:,:)
           int_focnsed_timeslice(:,:,:)  = int_focnsed_timeslice(:,:,:) + locij_focnsed(:,:,:)
           int_fsedocn_timeslice(:,:,:)  = int_fsedocn_timeslice(:,:,:) + locij_fsedocn(:,:,:)
           ! update time-slice data - GOLDSTEIn
           CALL sub_calc_psi(phys_ocn(ipo_gu:ipo_gw,:,:,:),loc_opsi,loc_opsia,loc_opsip,loc_zpsi,loc_opsia_minmax,loc_opsip_minmax)
           int_opsi_timeslice(:,:)  = int_opsi_timeslice(:,:)  + loc_dtyr*loc_opsi(:,:)
           int_opsia_timeslice(:,:) = int_opsia_timeslice(:,:) + loc_dtyr*loc_opsia(:,:)
           int_opsip_timeslice(:,:) = int_opsip_timeslice(:,:) + loc_dtyr*loc_opsip(:,:)
           int_zpsi_timeslice(:,:)  = int_zpsi_timeslice(:,:)  + loc_dtyr*loc_zpsi(:,:)
           int_psi_timeslice(:,:)   = int_psi_timeslice(:,:)   + loc_dtyr*diag_misc_psi(:,:)
           int_u_timeslice(:,:,:,:) = int_u_timeslice(:,:,:,:) + loc_dtyr*phys_ocn(ipo_gu:ipo_gw,:,:,:)
           ! update time-slice data - diagnostics
           int_diag_bio_timeslice(:,:,:)       = int_diag_bio_timeslice(:,:,:)       + diag_bio(:,:,:)
           int_diag_geochem_timeslice(:,:,:,:) = int_diag_geochem_timeslice(:,:,:,:) + diag_geochem(:,:,:,:)
           int_diag_Fe_timeslice(:,:,:,:)      = int_diag_Fe_timeslice(:,:,:,:)      + loc_dtyr*diag_Fe(:,:,:,:)
           int_diag_redox_timeslice(:,:,:,:)   = int_diag_redox_timeslice(:,:,:,:)   + diag_redox(:,:,:,:)
           ! gemlite
           if (dum_gemlite) then
              int_diag_weather_timeslice(:,:,:)   = int_diag_weather_timeslice(:,:,:)   + loc_dtyr*dum_sfxsumrok1(:,:,:)
           else
              int_diag_weather_timeslice(:,:,:)   = int_diag_weather_timeslice(:,:,:)   + dum_sfxsumrok1(:,:,:)
           end if
           int_diag_airsea_timeslice(:,:,:)    = int_diag_airsea_timeslice(:,:,:)    + diag_airsea(:,:,:)
           ! eceogem
           if (flag_ecogem) then
              int_diag_ecogem_part(:,:,:)  = int_diag_ecogem_part(:,:,:)  + loc_dtyr*diag_ecogem_part(:,:,:)
              int_diag_ecogem_remin(:,:,:) = int_diag_ecogem_remin(:,:,:) + loc_dtyr*diag_ecogem_remin(:,:,:)
           end if

        end if if_save3

        ! write time-slice data and re-set integration
        if ( ((par_data_save_slice_dt - int_t_timeslice) < conv_s_yr) &
             & .OR.                                                   &
             & (error_stop)                                           &
             & .OR.                                                   &
             & (int_t_timeslice_count == par_data_save_slice_n)       &
             & ) then

           if_save3b: if (dum_save) then

              ! set save time
              if (int_t_timeslice_count == par_data_save_slice_n) then
                 if (ctrl_misc_t_BP) then
                    loc_yr_save = loc_t + int_t_timeslice/2.0 - par_misc_t_end
                 else
                    loc_yr_save = par_misc_t_end - loc_t - int_t_timeslice/2.0
                 end if
              elseif ((par_data_save_slice_dt - int_t_timeslice) < conv_s_yr) then
                 if (ctrl_misc_t_BP) then
                    loc_yr_save = loc_t + int_t_timeslice/2.0 - par_misc_t_end
                 else
                    loc_yr_save = par_misc_t_end - loc_t - int_t_timeslice/2.0
                 end if
              elseif (error_stop) then
                 if (ctrl_misc_t_BP) then
                    loc_yr_save = loc_t - par_misc_t_end
                 else
                    loc_yr_save = par_misc_t_end - loc_t
                 end if
              else
                 ! NOTHING
              end if
              ! clean up year
              ! NOTE: original 'rounding' equation resulted in integer time > 2 Myr ended up over the precision limit
              !             (-2147483.648 ended up being reported)
              loc_yr_save = &
                   & real(int(loc_yr_save)) + real(int(1000.0*(loc_yr_save - real(int(loc_yr_save)) + 0.0005)))/1000.0
              ! reporting
              if (int_t_timeslice_count == par_data_save_slice_n) then
                 WRITE(unit=6,fmt='(A57,f12.3)') &
                      & ' >>> SAVING BIOGEM MONTHLY DATA & year :                 ', &
                      & loc_yr_save
              elseif ((par_data_save_slice_dt - int_t_timeslice) < conv_s_yr) then
                 WRITE(unit=6,fmt='(A57,f12.3)') &
                      & ' >>> SAVING BIOGEM TIME-SLICE AVERAGE CENTERED @ year  : ', &
                      & loc_yr_save
              elseif (error_stop) then
                 WRITE(unit=6,fmt='(A57,f12.3)') &
                      & ' >>> SAVING FATAL ERROR DATA DUMP @ year :               ', &
                      & loc_yr_save
              else
                 ! NOTHING
              end if
              ! re-open netcdf file, update record number, close file -- 2D
              if (ctrl_data_save_2d) then
                 call sub_save_netcdf(loc_yr_save,2)
                 CALL sub_save_netcdf_2d()
                 ncout2d_ntrec = ncout2d_ntrec + 1
                 call sub_closefile(ncout2d_iou)
              end if
              ! re-open netcdf file, update record number, close file -- 3D
              if (ctrl_data_save_3d) then
                 call sub_save_netcdf(loc_yr_save,3)
                 CALL sub_save_netcdf_3d(loc_t)
                 ncout3d_ntrec = ncout3d_ntrec + 1
                 call sub_closefile(ncout3d_iou)
              end if
              ! save global diagnostics
              If (ctrl_data_save_GLOBAL) call sub_data_save_global_av()
              If (ctrl_data_save_GLOBAL .AND. ctrl_data_save_derived) call sub_data_save_global_snap(loc_t,dum_sfcatm1(:,:,:))

              ! save orbits data
              if ((n_orb_pts_nloc > 0) .and. (n_orb_pts > 0)) then
                 WRITE(unit=6,fmt='(A57,f12.3)') &
                      & ' >>> SAVING BIOGEM ORBITS DATA @ year                      : ', &
                      & real(dum_genie_clock)/(1000.0*conv_yr_s)
                 DO nloc=1,n_orb_pts_nloc
                    loc_locstr = 'i'//fun_conv_num_char_n(2,orb_pts_loc(nloc,1))//'j'//fun_conv_num_char_n(2,orb_pts_loc(nloc,2))
                    loc_filename=fun_data_timeseries_filename( &
                         & loc_t,par_outdir_name,trim(par_outfile_name)//'_orb',loc_locstr,string_results_ext)
                    call check_unit(out,__LINE__,__FILE__)
                    OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                    call check_iostat(ios,__LINE__,__FILE__)
                    DO n=1,n_orb_pts
                       WRITE(unit=out,fmt='(f12.3,999e14.6)',iostat=ios) &
                            & orb_pts_time(n), &
                            & (orb_pts(n,nloc,nvar),nvar=1,n_orb_pts_nvar)
                       call check_iostat(ios,__LINE__,__FILE__)
                    end do
                    CLOSE(unit=out,iostat=ios)
                    call check_iostat(ios,__LINE__,__FILE__)
                 end do
                 ! reset data counter, empty arrays
                 n_orb_pts       = 0
                 orb_pts(:,:,:)  = 0.0
                 orb_pts_time(:) = 0.0
              end if

           end if if_save3b

           ! reset array values
           ! NOTE: call subroutine here to always reset counter and integrated time (which otherwise accumulate in a GEMlite phase)
           CALL sub_init_int_timeslice()
           ! update time slice index at end of primary integration interval (not the end of monthly or seasonal averages)
           if ((par_data_save_slice_dt - int_t_timeslice_TOT) < conv_s_yr) then
              int_t_timeslice_TOT = 0.0
              par_data_save_timeslice_i = par_data_save_timeslice_i - 1
           end if
           ! update status of 'end'
           par_misc_t_endslice = .true.

        END IF

     else

        par_misc_t_intslice = .false.

     end if if_save2
  end if if_save1

end SUBROUTINE diag_biogem_timeslice
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! BioGeM DIAGNOSTICS - TIME-SERIES
SUBROUTINE diag_biogem_timeseries( &
     & dum_dts,                    &
     & dum_genie_clock,            &
     & dum_sfcatm1,                &
     & dum_sfxatm1,                &
     & dum_sfxocn1,                &
     & dum_sfcsed1,                &
     & dum_sfxsed1,                &
     & dum_sfxsumrok1,             &
     & dum_save,                   &
     & dum_forcesave,              &
     & dum_gemlite                 &
     & )
  USE biogem_lib
  USE biogem_box
  USE biogem_data
  USE biogem_data_ascii
  ! dummy arguments
  REAL,INTENT(IN)::dum_dts                                       ! biogem time-step length (seconds)
  integer(kind=8),INTENT(IN)::dum_genie_clock                    ! genie clock (milliseconds since start) NOTE: 8-byte integer
  REAL,DIMENSION(n_atm,n_i,n_j),INTENT(in)::dum_sfcatm1          ! atmosphere composition interface array
  REAL,DIMENSION(n_atm,n_i,n_j),INTENT(in)::dum_sfxatm1          ! atmospheric flux interface array
  REAL,DIMENSION(n_ocn,n_i,n_j),INTENT(in)::dum_sfxocn1          ! sediment dissolution flux interface array
  REAL,DIMENSION(n_sed,n_i,n_j),INTENT(in)::dum_sfcsed1          ! sediment composition interface array
  REAL,DIMENSION(n_sed,n_i,n_j),INTENT(in)::dum_sfxsed1          ! sediment rain flux interface array
  real,dimension(n_ocn,n_i,n_j),intent(in)::dum_sfxsumrok1       ! coastal (weathering) -> ocean flux; ocn grid
  logical,INTENT(IN)::dum_save                                   ! average and save data?
  logical,INTENT(IN)::dum_forcesave                              ! force data saving?
  logical,intent(in)::dum_gemlite                                ! in GEMlite phase of cycle?
  ! local variables
  INTEGER::i,j,k,l,io,ia,is,ic
  integer::ib,id,i2D                                             ! counting variables
  integer::loc_k1                                                !
  real::loc_t,loc_dts,loc_dtyr                                   !
  real::loc_yr,loc_yr_save                                       !
  REAL,DIMENSION(0:n_j,0:n_k)::loc_opsi,loc_zpsi                 !
  REAL,DIMENSION(0:n_j,0:n_k)::loc_opsia,loc_opsip               !
  REAL,DIMENSION(n_atm,n_i,n_j)::locij_focnatm                   ! local ocn->atm flux (atm tracer currency) (mol yr-1)
  REAL,DIMENSION(n_sed,n_i,n_j)::locij_focnsed                   ! local ocn->sed change (sed tracer currency) (mol)
  REAL,DIMENSION(n_ocn,n_i,n_j)::locij_fsedocn                   ! local sed->ocean change (ocn tracer currency) (mol)
  REAL,DIMENSION(n_ocn,n_i,n_j)::locij_ocn_ben                   ! local benthic ocean composition
  REAL,DIMENSION(n_i,n_j)::locij_mask_ben                        ! benthic save mask
  real::loc_ocn_tot_M                                            !
  real::loc_ocn_rtot_M                                           !
  real::loc_ocn_tot_A,loc_opn_tot_A,loc_ocnatm_tot_A             !
  real::loc_ocn_rtot_A,loc_opn_rtot_A,loc_ocnatm_rtot_A          !
  real::loc_ocnsed_tot_A,loc_ocnsed_tot_A_ben                    !
  real::loc_ocnsed_rtot_A,loc_ocnsed_rtot_A_ben                  !
  real::loc_tot_A                                                !
  real::loc_sig                                                  !
  REAL,DIMENSION(2)::loc_opsia_minmax,loc_opsip_minmax           !
  real::loc_opsi_scale                                           !

  ! *** TIME-SERIES DATA UPDATE ***
  IF (ctrl_debug_lvl1) print*, '*** RUN-TIME DATA UPDATE ***'

  ! update time slice data
  ! NOTE: carried out only when the local (BioGeM) time falls between a selected time slice time plus integration time,
  !       and the time slice time itself
  ! NOTE: do not construct and save a time slice if <par_data_save_timeslice_i> == 0,
  !       i.e., no valid (in-range) time slices have been requested in the time slice input file 'biogem_save_timeslice.dat',
  !       or the final requested time slice has been made
  ! calculate local model time and time step length
  loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
  loc_dts  = dum_dts
  loc_dtyr = loc_dts/conv_yr_s
  ! update status of 'end'
  par_misc_t_endseries = .false.

  if_save1: if(par_data_save_sig_i > 0) then
     if_save2: IF ( &
          & ((loc_t - (par_data_save_sig(par_data_save_sig_i) + par_data_save_sig_dt/2.0)) < -conv_s_yr) &
          & .OR. &
          &  (dum_forcesave) &
          & ) THEN

        par_misc_t_intseries = .true.
        par_misc_t_tseries = par_misc_t_end - par_data_save_sig(par_data_save_sig_i)

        if_save3: if (dum_save) then

           ! calculate local constants
           ! total ocean mass and recpirocal
           loc_ocn_tot_M = sum(phys_ocn(ipo_M,:,:,:))
           if (loc_ocn_tot_M > const_real_nullsmall) then
              loc_ocn_rtot_M = 1.0/loc_ocn_tot_M
           else
              loc_ocn_rtot_M = 0.0
           end if
           ! total ocean-atmosphere interface area
           loc_ocnatm_tot_A = sum(phys_ocnatm(ipoa_A,:,:))
           if (loc_ocnatm_tot_A > const_real_nullsmall) then
              loc_ocnatm_rtot_A = 1.0/loc_ocnatm_tot_A
           else
              loc_ocnatm_rtot_A = 0.0
           end if
           ! total ocean surface area
           loc_ocn_tot_A = sum(phys_ocn(ipo_A,:,:,n_k))
           if (loc_ocn_tot_A > const_real_nullsmall) then
              loc_ocn_rtot_A = 1.0/loc_ocn_tot_A
           else
              loc_ocn_rtot_A = 0.0
           end if
           ! total open ocean surface area (ice-free)
           loc_opn_tot_A = sum((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k))
           if (loc_opn_tot_A > const_real_nullsmall) then
              loc_opn_rtot_A = 1.0/loc_opn_tot_A
           else
              loc_opn_rtot_A = 0.0
           end if
           ! total ocean-sediment interface area
           loc_ocnsed_tot_A = sum(phys_ocn(ipo_A,:,:,n_k))
           if (loc_ocnsed_tot_A > const_real_nullsmall) then
              loc_ocnsed_rtot_A = 1.0/loc_ocnsed_tot_A
           else
              loc_ocnsed_rtot_A = 0.0
           end if
           ! reconstruct local interface fluxes
           ! also: benthic mask
           DO i=1,n_i
              DO j=1,n_j
                 loc_k1 = goldstein_k1(i,j)
                 IF (n_k >= loc_k1) THEN
                    ! ocn->atm
                    ! NOTE: convert units from (mol m-2 s-1) to (mol yr-1)
                    DO l=3,n_l_atm
                       ia = conv_iselected_ia(l)
                       locij_focnatm(ia,i,j) = conv_yr_s*phys_ocnatm(ipoa_A,i,j)*dum_sfxatm1(ia,i,j)
                    end do
                    ! ocn->sed
                    ! NOTE: convert units from (mol m-2 s-1) to (mol per timestep)
                    DO l=1,n_l_sed
                       is = conv_iselected_is(l)
                       locij_focnsed(is,i,j) = loc_dts*phys_ocn(ipo_A,i,j,loc_k1)*dum_sfxsed1(is,i,j)
                    end DO
                    ! sed->ocn
                    ! NOTE: convert units from (mol m-2 s-1) to (mol per timestep)
                    DO l=3,n_l_ocn
                       io = conv_iselected_io(l)
                       locij_fsedocn(io,i,j) = loc_dts*phys_ocn(ipo_A,i,j,loc_k1)*dum_sfxocn1(io,i,j)
                    end do
                    ! interface
                    if (phys_ocn(ipo_Dbot,i,j,loc_k1) > par_data_save_ben_Dmin) then
                       DO l=1,n_l_ocn
                          io = conv_iselected_io(l)
                          locij_ocn_ben(io,i,j) = ocn(io,i,j,loc_k1)
                       end do
                       locij_mask_ben(i,j) = 1.0
                    else
                       locij_mask_ben(i,j) = 0.0
                       locij_ocn_ben(:,i,j) = 0.0
                    end if
                 end IF
              end DO
           end DO
           ! benthic mask area
           loc_ocnsed_tot_A_ben = sum(locij_mask_ben(:,:)*phys_ocn(ipo_A,:,:,n_k))
           if (loc_ocnsed_tot_A_ben > const_real_nullsmall) then
              loc_ocnsed_rtot_A_ben = 1.0/loc_ocnsed_tot_A_ben
           else
              loc_ocnsed_rtot_A_ben = 0.0
           end if
           ! update signal-averaging ingetrated tracers
           ! NOTE: mass-weight ocean tracers, and surface area-weight atmospheric tracers
           ! NOTE: for sea surface (_sur) tracer values weight by fractional ice-free cover
           ! NOTE: for core-top sediment properties filter out grid points with no solid material from isotopic value averaging
           ! NOTE: for calculating mean of sedimentary core-top isotopic composition,
           !       additioanlly weight the integrated area used to normalize the sum by (wt%) CaCO3 abundance
           int_ocn_tot_M_sig     = int_ocn_tot_M_sig     + loc_dtyr*SUM(phys_ocn(ipo_M,:,:,:))
           int_ocn_tot_M_sur_sig = int_ocn_tot_M_sur_sig + loc_dtyr*SUM(phys_ocn(ipo_M,:,:,n_k))
           int_ocn_tot_V_sig     = int_ocn_tot_M_sig     + loc_dtyr*SUM(phys_ocn(ipo_V,:,:,:))
           ! main time-series
           IF (ctrl_data_save_sig_ocn) THEN
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 int_ocn_sig(io) = int_ocn_sig(io) + &
                      & loc_dtyr*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io,:,:,:))*loc_ocn_rtot_M
              END DO
              IF (ctrl_force_ocn_age) THEN
                 DO i=1,n_i
                    DO j=1,n_j
                       loc_k1 = goldstein_k1(i,j)
                       IF (n_k >= loc_k1) THEN
                          DO k=loc_k1,n_k
                             if (ocn(io_colr,i,j,k) > const_real_nullsmall) then
                                int_misc_age_sig = int_misc_age_sig + &
                                     & loc_dtyr*phys_ocn(ipo_M,i,j,k)*ocn(io_colb,i,j,k)/ocn(io_colr,i,j,k)*loc_ocn_rtot_M
                             end if
                          end DO
                       end IF
                    end DO
                 end DO
              END if
           end if
           IF (ctrl_data_save_sig_ocnatm) THEN
              DO l=1,n_l_atm
                 ia = conv_iselected_ia(l)
                 int_ocnatm_sig(ia) = int_ocnatm_sig(ia) + &
                      & loc_dtyr*SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia,:,:))*loc_ocnatm_rtot_A
              END DO
           end if
           IF (ctrl_data_save_sig_fexport) THEN
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 int_fexport_sig(is) = int_fexport_sig(is) + &
                      & SUM(bio_settle(is,:,:,n_k))
                 int_fracdom_sig(is) = int_fracdom_sig(is) + loc_dtyr*int_fracdom(is)
              END DO
           end if
           IF (ctrl_data_save_sig_focnatm) THEN
              DO l=3,n_l_atm
                 ia = conv_iselected_ia(l)
                 int_focnatm_sig(ia) = int_focnatm_sig(ia) + &
                      & loc_dtyr*SUM(locij_focnatm(ia,:,:))
              END DO
           end if
           IF (ctrl_data_save_sig_focnsed) THEN
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 int_focnsed_sig(is) = int_focnsed_sig(is) + &
                      & SUM(locij_focnsed(is,:,:))
              END DO
           end if
           IF (ctrl_data_save_sig_fsedocn) THEN
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 int_fsedocn_sig(io) = int_fsedocn_sig(io) + loc_dts*&
                      & SUM(phys_ocn(ipo_A,:,:,n_k)*dum_sfxocn1(io,:,:))
              END DO
           end if
           IF (ctrl_data_save_sig_ocn_sur) THEN
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 int_ocn_sur_sig(io) = int_ocn_sur_sig(io) + loc_dtyr*&
                      & SUM(phys_ocn(ipo_A,:,:,n_k)*ocn(io,:,:,n_k))*loc_ocn_rtot_A
                 int_ocn_opn_sig(io) = int_ocn_opn_sig(io) + loc_dtyr*&
                      & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io,:,:,n_k))*loc_opn_rtot_A
              END DO
              IF (ctrl_force_ocn_age) THEN
                 DO i=1,n_i
                    DO j=1,n_j
                       loc_k1 = goldstein_k1(i,j)
                       IF (n_k >= loc_k1) THEN
                          if (ocn(io_colr,i,j,n_k) > const_real_nullsmall) then
                             int_misc_age_sur_sig = int_misc_age_sur_sig + loc_dtyr*loc_ocn_rtot_A*&
                                  & phys_ocn(ipo_A,i,j,n_k)*ocn(io_colb,i,j,n_k)/ocn(io_colr,i,j,n_k)
                          end if
                       end IF
                    end DO
                 end DO
              END if
           end if
           IF (ctrl_data_save_sig_carb_sur) THEN
              DO ic=1,n_carb
                 int_carb_sur_sig(ic) = int_carb_sur_sig(ic) + loc_dtyr*&
                      & SUM(phys_ocn(ipo_A,:,:,n_k)*carb(ic,:,:,n_k))*loc_ocn_rtot_A
                 int_carb_opn_sig(ic) = int_carb_opn_sig(ic) + loc_dtyr*&
                      & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*carb(ic,:,:,n_k))*loc_opn_rtot_A
              END DO
           end if
           IF (ctrl_data_save_sig_ocn_sur) THEN
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 int_ocn_ben_sig(io) = int_ocn_ben_sig(io) + loc_dtyr*loc_ocnsed_rtot_A_ben*&
                      & SUM(locij_mask_ben(:,:)*phys_ocn(ipo_A,:,:,n_k)*locij_ocn_ben(io,:,:))
              END DO
              IF (ctrl_force_ocn_age) THEN
                 DO i=1,n_i
                    DO j=1,n_j
                       loc_k1 = goldstein_k1(i,j)
                       IF (n_k >= loc_k1) THEN
                          if (locij_ocn_ben(io_colr,i,j) > const_real_nullsmall) then
                             int_misc_age_ben_sig = int_misc_age_ben_sig + loc_dtyr*loc_ocnsed_rtot_A_ben*locij_mask_ben(i,j)*&
                                  & phys_ocn(ipo_A,i,j,n_k)*locij_ocn_ben(io_colb,i,j)/locij_ocn_ben(io_colr,i,j)
                          end if
                       end IF
                    end DO
                 end DO
              END if
           end if
           IF (ctrl_data_save_sig_misc) THEN
              ! record GEMlite phase
              if (dum_gemlite) int_misc_gemlite_sig = int_misc_gemlite_sig + loc_dtyr
              ! sea-ice
              int_misc_seaice_sig = int_misc_seaice_sig + &
                   & loc_dtyr*SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:))
              If (sum(phys_ocnatm(ipoa_seaice,:,:)) > const_real_nullsmall) then
                 int_misc_seaice_sig_th = int_misc_seaice_sig_th + &
                      & loc_dtyr* &
                      & SUM(phys_ocnatm(ipoa_seaice_th,:,:)*phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:))/ &
                      & SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:))
              end If
              int_misc_seaice_sig_vol = int_misc_seaice_sig_vol + &
                   & loc_dtyr*SUM(phys_ocnatm(ipoa_seaice_th,:,:)*phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:))
              ! overturning streamfunction
              CALL sub_calc_psi( &
                   & phys_ocn(ipo_gu:ipo_gw,:,:,:),loc_opsi,loc_opsia,loc_opsip,loc_zpsi,loc_opsia_minmax,loc_opsip_minmax &
                   & )
              int_misc_opsi_min_sig = int_misc_opsi_min_sig + loc_dtyr*minval(loc_opsi(:,:))
              int_misc_opsi_max_sig = int_misc_opsi_max_sig + loc_dtyr*maxval(loc_opsi(:,:))
              int_misc_opsia_min_sig = int_misc_opsia_min_sig + loc_dtyr*loc_opsia_minmax(1)
              int_misc_opsia_max_sig = int_misc_opsia_max_sig + loc_dtyr*loc_opsia_minmax(2)
              ! calculate current mean surface land (air) temperature SLT (degrees C)
              loc_sig = 0.0
              loc_tot_A = 0.0
              DO i=1,n_i
                 DO j=1,n_j
                    loc_k1 = goldstein_k1(i,j)
                    IF (n_k < loc_k1) THEN
                       loc_sig = loc_sig + phys_ocnatm(ipoa_A,i,j)*dum_sfcatm1(ia_T,i,j)
                       loc_tot_A = loc_tot_A + phys_ocnatm(ipoa_A,i,j)
                    end IF
                 end DO
              end DO
              if (loc_tot_A > const_real_nullsmall) then
                 int_misc_SLT_sig = int_misc_SLT_sig + loc_dtyr*loc_sig/loc_tot_A
              else
                 int_misc_SLT_sig = 0.0
              end if
              ! solar insolation (and orbitally-related information)
              ! NOTE: apply ocean mask (@ surface)
              ! (1) mean global properties
              int_misc_ocn_solfor_sig = int_misc_ocn_solfor_sig + &
                   & loc_dtyr*loc_ocn_rtot_A*sum(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_solfor,:,:))
              int_misc_opn_solfor_sig = int_misc_opn_solfor_sig + &
                   & loc_dtyr*loc_opn_rtot_A*sum(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_solfor,:,:))
              int_misc_ocn_fxsw_sig = int_misc_ocn_fxsw_sig + &
                   & loc_dtyr*loc_ocn_rtot_A*sum(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_fxsw,:,:))
              int_misc_opn_fxsw_sig = int_misc_opn_fxsw_sig + &
                   & loc_dtyr*loc_opn_rtot_A*sum(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_fxsw,:,:))
              ! (2) latitudinal/seasonal properties
              !     NOTE: done very crudely and taking values from a single specified time-step of the averaging only
              if (int_t_sig_count == par_t_sig_count_N) then
                 snap_misc_ocn_solfor_N_sig = &
                      & sum(phys_ocnatm(ipoa_A,:,par_sig_j_N)*phys_ocnatm(ipoa_solfor,:,par_sig_j_N)) / &
                      & sum(phys_ocnatm(ipoa_A,:,par_sig_j_N))
              end if
              if (int_t_sig_count == par_t_sig_count_S) then
                 snap_misc_ocn_solfor_S_sig = &
                      & sum(phys_ocnatm(ipoa_A,:,par_sig_j_S)*phys_ocnatm(ipoa_solfor,:,par_sig_j_S)) / &
                      & sum(phys_ocnatm(ipoa_A,:,par_sig_j_S))
              end if
           end if
           ! calcualte mean global sediement properties
           ! NOTE: for isotopic properties, set thresholds for including sediments in the global mean
           !       (to avoid possible NaNs or non-sensicle values associated with vanishigly small bulk abundances):
           !        0.1% for wt% POM
           !       10.0% for wt% CaCO3
           !        1.0% for wt% opal
           !        0.1% for wt% det
           IF (ctrl_data_save_sig_ocnsed) THEN
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 SELECT CASE (sed_type(is))
                 CASE (par_sed_type_bio,par_sed_type_abio, &
                      & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
                      & par_sed_type_scavenged)
                    loc_sig = SUM(phys_ocn(ipo_A,:,:,n_k)*dum_sfcsed1(is,:,:))
                    loc_tot_A = loc_ocnsed_tot_A
                 case (par_sed_type_age,n_itype_min:n_itype_max)
                    loc_sig = 0.0
                    loc_tot_A = 0.0
                    DO i=1,n_i
                       DO j=1,n_j
                          if ((sed_type(sed_dep(is)) == par_sed_type_POM) .OR. (sed_dep(is) == is_POC)) then
                             If (dum_sfcsed1(is_POC,i,j) > 0.1) then
                                loc_sig = loc_sig + phys_ocn(ipo_A,i,j,n_k)*dum_sfcsed1(sed_dep(is),i,j)*dum_sfcsed1(is,i,j)
                                loc_tot_A = loc_tot_A + phys_ocn(ipo_A,i,j,n_k)*dum_sfcsed1(sed_dep(is),i,j)
                             end if
                          end if
                          if ((sed_type(sed_dep(is)) == par_sed_type_CaCO3) .OR. (sed_dep(is) == is_CaCO3)) then
                             If (dum_sfcsed1(is_CaCO3,i,j) > 10.0) then
                                loc_sig = loc_sig + phys_ocn(ipo_A,i,j,n_k)*dum_sfcsed1(sed_dep(is),i,j)*dum_sfcsed1(is,i,j)
                                loc_tot_A = loc_tot_A + phys_ocn(ipo_A,i,j,n_k)*dum_sfcsed1(sed_dep(is),i,j)
                             end if
                          end if
                          if ((sed_type(sed_dep(is)) == par_sed_type_opal) .OR. (sed_dep(is) == is_opal)) then
                             If (dum_sfcsed1(is_opal,i,j) > 1.0) then
                                loc_sig = loc_sig + phys_ocn(ipo_A,i,j,n_k)*dum_sfcsed1(sed_dep(is),i,j)*dum_sfcsed1(is,i,j)
                                loc_tot_A = loc_tot_A + phys_ocn(ipo_A,i,j,n_k)*dum_sfcsed1(sed_dep(is),i,j)
                             end if
                          end if
                          if ((sed_type(sed_dep(is)) == par_sed_type_det) .OR. (sed_dep(is) == is_det)) then
                             If (dum_sfcsed1(is_det,i,j) > 0.1) then
                                loc_sig = loc_sig + phys_ocn(ipo_A,i,j,n_k)*dum_sfcsed1(sed_dep(is),i,j)*dum_sfcsed1(is,i,j)
                                loc_tot_A = loc_tot_A + phys_ocn(ipo_A,i,j,n_k)*dum_sfcsed1(sed_dep(is),i,j)
                             end if
                          end if
                       end DO
                    end DO
                 case default
                    loc_sig = 0.0
                    loc_tot_A = 0.0
                 end SELECT
                 if (loc_tot_A > const_real_nullsmall) then
                    int_ocnsed_sig(is) = int_ocnsed_sig(is) + loc_dtyr*loc_sig/loc_tot_A
                 else
                    int_ocnsed_sig(is) = 0.0
                 end if
              END DO
           end if
           ! aeolian Fe input and solubility etc
           int_misc_det_Fe_tot_sig = int_misc_det_Fe_tot_sig + loc_dtyr*SUM(phys_ocnatm(ipoa_totFe,:,:))
           int_misc_det_Fe_dis_sig = int_misc_det_Fe_dis_sig + loc_dtyr*SUM(phys_ocnatm(ipoa_solFe,:,:)*phys_ocnatm(ipoa_totFe,:,:))
           ! diagnostic diagnostics
           ! NOTE: the area used in calculating a mean global biological uptake rates must be total ocean surface area
           !       (i.e., including sea-ice covered area)
           ! NOTE: diag_bio variables have already been converted into yr-1
           loc_tot_A = sum(phys_ocn(ipo_A,:,:,n_k))
           IF (ctrl_data_save_sig_diag .OR. ctrl_data_save_sig_diag_geochem) THEN
              DO ib=1,n_diag_bio
                 int_diag_bio_sig(ib) = int_diag_bio_sig(ib) + &
                      & SUM(phys_ocn(ipo_A,:,:,n_k)*diag_bio(ib,:,:))/loc_tot_A
              END DO
              DO id=1,n_diag_geochem
                 int_diag_geochem_sig(id) = int_diag_geochem_sig(id) + &
                      & SUM(phys_ocn(ipo_M,:,:,:)*diag_geochem(id,:,:,:))*loc_ocn_rtot_M
              END DO
              DO id=1,n_diag_redox
                 int_diag_redox_sig(id) = int_diag_redox_sig(id) + &
                      & SUM(phys_ocn(ipo_M,:,:,:)*diag_redox(id,:,:,:))*loc_ocn_rtot_M
              END DO
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 if (dum_gemlite) then
                    int_diag_weather_sig(io) = int_diag_weather_sig(io) + loc_dtyr*SUM(dum_sfxsumrok1(io,:,:))
                 else
                    int_diag_weather_sig(io) = int_diag_weather_sig(io) + SUM(dum_sfxsumrok1(io,:,:))
                 end if
              END DO
              DO l=1,n_l_atm
                 ia = conv_iselected_ia(l)
                 int_diag_airsea_sig(ia) = int_diag_airsea_sig(ia) + loc_dtyr*SUM(diag_airsea(ia,:,:))
              END DO
              DO l=1,n_l_atm
                 ia = conv_iselected_ia(l)
                 int_diag_forcing_sig(ia) = int_diag_forcing_sig(ia) + loc_dtyr*SUM(diag_forcing(ia,:,:))
              END DO
           end if
           ! high resolution 3D tracer data
           if (ctrl_data_save_3d_sig) then
              DO l=1,n_l_ocn
                 io = conv_iselected_io(l)
                 int_misc_3D_sig(l,:,:,:) = int_misc_3D_sig(l,:,:,:) + loc_dtyr*ocn(io,:,:,:)
              end DO
           end if
           ! ### ADD ADDITIONAL TIME-SERIES UPDATES HERE ######################################################################### !
           !
           ! ##################################################################################################################### !

        end if if_save3

        ! update signal-averaging integrated time
        int_t_sig = int_t_sig + loc_dtyr
        int_t_sig_count = int_t_sig_count + 1

     else

        par_misc_t_intseries = .false.

     end IF if_save2

     if (dum_save) then
        ! special 'cases' here -- i.e. integrated fluxes that need to be updated every year
        ! misc - 2D diagnostics
        IF (ctrl_data_save_inversion) THEN
           do i2D = 1,n_diag_misc_2D
              int_diag_misc_2D_sig(i2D) = int_diag_misc_2D_sig(i2D) + loc_dtyr*SUM(diag_misc_2D(i2D,:,:))
           end do
        end IF
     end if

     ! set save time
     ! 28/12/2011: changed 'rounding' equation because integer time > 2 Myr ended up over the precision limit
     !             (-2147483.648 ended up being reported)
     !             => instead simply 'bumped' the year value to avoid the occurrence of e.g. x.999999 reported values
     if ((par_data_save_sig_dt - int_t_sig) < conv_s_yr) then
        if (ctrl_misc_t_BP) then
           loc_yr_save = loc_t + int_t_sig/2.0 - par_misc_t_end
        else
           loc_yr_save = par_misc_t_end - loc_t - int_t_sig/2.0
        end if
        ! clean up year
        ! NOTE: original 'rounding' equation resulted in integer time > 2 Myr ended up over the precision limit
        !             (-2147483.648 ended up being reported)
        loc_yr_save = &
             & real(int(loc_yr_save)) + real(int(1000.0*(loc_yr_save - real(int(loc_yr_save)) + 0.0005)))/1000.00

        if (dum_save) then

           WRITE(unit=6,fmt='(A57,f12.3)') &
                & '>>> SAVING BIOGEM TIME-SERIES AVERAGE CENTERED @ year : ', &
                & loc_yr_save
           ! check that there is no chance of dividing-by-zero ...
           IF (int_t_sig > const_real_nullsmall) then
              IF (ctrl_misc_t_BP) THEN
                 loc_yr = loc_t + par_misc_t_end
              ELSE
                 loc_yr = par_misc_t_end - loc_t
              END IF
              ! ### OPTIONAL CODE ################################################################################################ !
              ! NOTE: netCDF time-series saving is disabled by default, partly because updating has lagged behind the ASCII version
              !       (and partly because personally, I never used the data files ...)
              ! ################################################################################################################## !
              IF (ctrl_data_save_sig_ascii) then
                 CALL sub_data_save_runtime(loc_yr_save,loc_t)
              else
                 CALL sub_save_netcdf_runtime(loc_yr_save)
              end IF
              ! re-open netcdf file, update record number, close file -- high resolution 3D
              if (ctrl_data_save_3d_sig) then
                 call sub_save_netcdf(loc_yr_save,4)
                 CALL sub_save_netcdf_3d_sig()
                 ncout3dsig_ntrec = ncout3dsig_ntrec + 1
                 call sub_closefile(ncout3dsig_iou)
              end if
              if (.NOT. ctrl_debug_lvl0) then
                 loc_opsi_scale = goldstein_dsc*goldstein_usc*const_rEarth*1.0E-6
                 CALL sub_calc_psi( &
                      & phys_ocn(ipo_gu:ipo_gw,:,:,:),loc_opsi,loc_opsia,loc_opsip,loc_zpsi,loc_opsia_minmax,loc_opsip_minmax &
                      & )
                 call sub_echo_runtime(loc_yr,loc_opsi_scale,loc_opsia_minmax,dum_sfcatm1(:,:,:),dum_gemlite)
              endif
           end if

        else
           IF (ctrl_debug_lvl1) PRINT*,'>>> *SKIPPING* SAVING BIOGEM TIME-SERIES AVERAGE CENTERED @ year : ',loc_yr_save
        end if
        ! update save array index if not forced
        if (.NOT.dum_forcesave) par_data_save_sig_i = par_data_save_sig_i - 1
        ! reset array values
        CALL sub_init_int_timeseries()
        ! update status of 'end'
        par_misc_t_endseries = .true.
     END IF

  end IF if_save1
  ! ****************************************************************************************************************************** !

end SUBROUTINE diag_biogem_timeseries


! ******************************************************************************************************************************** !
subroutine matrix_recover_exp(&
& dum_matrix_k)

! +++ Divide By Cucumber Error. Please Reinstall Universe And Reboot +++ 12/03/15

use biogem_lib
use biogem_data_netCDF

implicit none

Integer,Intent(in)::dum_matrix_k ! depth level of dye experiment

integer::col_count,row_count,n,n2,k,k2 ! loop counters
integer:: m_j,m_i,m_i_plus_one,m_i_minus_one,m_j_plus_one,m_j_minus_one ! grid_indices
integer::matrix_tracer ! index for selecting colour tracer
integer::loc_k1,col_name!,gridboxes,tracer_n
real::loc_val

print*,">>> Recovering Matrix Information at k level:",dum_matrix_k,'@ averaging interval n:',matrix_season


! loop over boxes in vts
col_count=1  ! outer loop for matrix column	index
row_count=1  ! inner loop for matrix row index
do n=1,n_vocn,1
  loc_k1 = matrix_exp(n)%k1
   do k=n_k,loc_k1,-1
     if(k.eq.dum_matrix_k)then ! start looping over whole array for row indices and record results...

       ! find out which tracer was initialised
       matrix_tracer=mod(2*matrix_exp(n)%j-1+mod(matrix_exp(n)%i-1,6),6)+1 ! get tracer number for i j
       select CASE (matrix_tracer)
        case(1)
            col_name=io_col0
        case(2)
            col_name=io_col1
        case(3)
            col_name=io_col2
        case(4)
            col_name=io_col3
        case(5)
            col_name=io_col4
        case(6)
            col_name=io_col5
       end select

       ! calculate i+1, i-1, j+1, j-1
       m_i=matrix_exp(n)%i
       m_j=matrix_exp(n)%j
       m_i_plus_one=matrix_exp(n)%i+1
       m_i_minus_one=matrix_exp(n)%i-1
       m_j_plus_one=matrix_exp(n)%j+1
       m_j_minus_one=matrix_exp(n)%j-1

       ! account for longitude wraparound and off-grid
       if (m_i_plus_one.gt.36)then
         m_i_plus_one=1
       elseif(m_i_minus_one.lt.1)then
         m_i_minus_one=36
       end if

       ! loop over whole grid recording tracer where equals above i j's
       do n2=1,n_vocn,1
         loc_k1 = matrix_exp(n2)%k1
         do k2=n_k,loc_k1,-1

           ! catch grid-box in potential neighbouring boxes
           if(matrix_exp(n2)%i.eq.m_i .AND. matrix_exp(n2)%j.eq.m_j)then
             loc_val=matrix_exp(n2)%mk(io2l(col_name),k2)/(matrix_avg_count)
             matrix_exp(n2)%mk(io2l(col_name),k2)=0.0
           elseif(matrix_exp(n2)%i.eq.m_i_plus_one .AND. matrix_exp(n2)%j.eq.m_j)THEN
             loc_val=matrix_exp(n2)%mk(io2l(col_name),k2)/(matrix_avg_count)
             matrix_exp(n2)%mk(io2l(col_name),k2)=0.0
           elseif(matrix_exp(n2)%i.eq.m_i_minus_one .AND. matrix_exp(n2)%j.eq.m_j)THEN
             loc_val=matrix_exp(n2)%mk(io2l(col_name),k2)/(matrix_avg_count)
             matrix_exp(n2)%mk(io2l(col_name),k2)=0.0
           elseif(matrix_exp(n2)%i.eq.m_i .AND. matrix_exp(n2)%j.eq.m_j_plus_one)THEN
             loc_val=matrix_exp(n2)%mk(io2l(col_name),k2)/(matrix_avg_count)
             matrix_exp(n2)%mk(io2l(col_name),k2)=0.0
           elseif(matrix_exp(n2)%i.eq.m_i .AND. matrix_exp(n2)%j .eq.m_j_minus_one)THEN
             loc_val=matrix_exp(n2)%mk(io2l(col_name),k2)/(matrix_avg_count)
             matrix_exp(n2)%mk(io2l(col_name),k2)=0.0
           else
             loc_val=0.0
           end if

           ! record tracer if not zero
           if(abs(loc_val).gt.const_real_nullsmall)THEN
             call sub_save_netcdf_TM(1,1,loc_val,col_count,row_count,matrix_season,0,0,0)
           end if

           row_count=row_count+1

         end do
       end do
       row_count=1

     end if
     col_count=col_count+1
    end do
  end do

  end subroutine matrix_recover_exp
  ! ****************************************************************************************************************************** !
