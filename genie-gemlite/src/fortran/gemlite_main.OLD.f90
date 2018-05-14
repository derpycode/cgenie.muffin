! *************************************************************************************************
! gemlite_main.f90
! Geochemical Model Lite
! MAIN
! *************************************************************************************************



! *** SETUP GeMlite ***
SUBROUTINE setup_gemlite(dum_t0,dum_tsc)
  USE gemlite_data
  IMPLICIT NONE
  ! dummy arguments
  real,intent(in)::dum_t0
  real,intent(in)::dum_tsc
  ! local variables
  real::loc_t
  ! BLAH
  call sub_load_gemlite_config()
  ! calculate gemchemical model time and GeMlite trigger threshold
  loc_t = ABS(par_misc_t_end - par_misc_t_start) - (dum_tsc*dum_t0)/conv_yr_s - par_misc_t_err
  If (loc_t > par_gemlite_dtyr_nonacc) then
     par_gemlite_tyr_acc_start = loc_t - par_gemlite_dtyr_nonacc
  else
     par_gemlite_tyr_acc_start = 0.0
  end if
end SUBROUTINE setup_gemlite


! *** TIMESTEP GeMlite ***
SUBROUTINE tstep_gemlite(dum_t,dum_dt,dum_tsc, &
     & dum_t0, &
     & dum_ts, &
     & dum_ts1)
  USE gemlite_lib
  USE sedgem_lib
  USE sedgem_data
  USE biogem_lib
  USE biogem_box
  USE biogem_data
  IMPLICIT NONE
  ! dummy arguments
  real,intent(inout)::dum_t
  real,intent(in)::dum_dt
  real,intent(in)::dum_tsc
  real,intent(inout)::dum_t0
  REAL,DIMENSION(1:n_maxl,1:n_maxi,1:n_maxj,1:n_maxk),INTENT(inout)::dum_ts
  REAL,DIMENSION(1:n_maxl,1:n_maxi,1:n_maxj,1:n_maxk),INTENT(inout)::dum_ts1
  ! local variables
  INTEGER::i,j,io,ia,is
  integer::loc_isteplite
  integer::loc_count
  integer::loc_k1
  real::loc_t,loc_dts,loc_dtyr
  real::loc_int_t_sig
  real::loc_tot_ocn_M,loc_tot_atm_V
  real::loc_tot_pCO2,loc_tot_pCO2_INIT,loc_tot_pCO2_NEW,loc_tot_pCO2_OLD
  real,DIMENSION(0:n_ocn)::loc_dsystem
  real,DIMENSION(0:n_atm)::loc_dsystem_atm
  real,DIMENSION(0:n_ocn)::loc_dsystem_ocn
  real,DIMENSION(0:n_atm)::loc_force_atm
  real,DIMENSION(0:n_ocn)::loc_force_ocn
  real,DIMENSION(0:n_sed)::loc_interf_ocnsed
  real,DIMENSION(0:n_ocn)::loc_interf_sedocn
  REAL,DIMENSION(n_maxi,n_maxj)::loc_conv_atm_mol
!!$  REAL,DIMENSION(n_maxi,n_maxj)::loc_conv_mol_atm


  ! ***************************************
  ! *** ACCELERATED GEOCHEMISTRY UPDATE ***
  ! ***************************************

  ! *** CALCULATE GEM TIME ***
  ! calculate gemchemical model time
  ! NOTE: counted down in years
  ! calculate time step length
  ! NOTE: convert between time units of BioGeM (years) and GOLDSTEIn (use <tsc> scale factor to convert to seconds)
  loc_t       = ABS(par_misc_t_end - par_misc_t_start) - (dum_tsc*dum_t)/conv_yr_s - par_misc_t_err
  loc_dts     = dum_tsc*dum_dt
  loc_dtyr    = loc_dts/conv_yr_s

  ! check for the start of an acceleration period
  ! NOTE: loc_t counts DOWN in years (zero being the end of BioGeM integration)
  IF ((loc_t <= par_gemlite_tyr_acc_start) .AND. (loc_t > 0.0)) THEN

     ! *** INITIALIZE LOCAL VARIABLES ***
     loc_isteplite        = 0
     loc_force_atm(:)     = 0.0
     loc_force_ocn(:)     = 0.0
     loc_interf_ocnsed(:) = 0.0
     loc_interf_sedocn(:) = 0.0
     loc_dsystem(:)       = 0.0
     loc_int_t_sig        = 0.0

     ! *** SET LOCAL CONSTANTS ***
     ! total ocean mass and atmospheric volume
     loc_tot_ocn_M = sum(phys_ocn(ipo_M,:,:,:))
     loc_tot_atm_V = sum(phys_atm(ipa_V,:,:))
     ! convertion between partial pressure and molar quantity
     loc_conv_atm_mol(:,:) = phys_atm(ipa_V,:,:)/(conv_Pa_atm*const_R_SI*atm(ia_T,:,:))
!!$     loc_conv_mol_atm(:,:) = 1.0/loc_conv_atm_mol(:,:)
                       
     ! *** RECALCULATE CARBONATE CONSTANTS ***
     DO i=1,n_imax
        DO j=1,n_jmax
           IF (n_kmax >= goldstein_k1(i,j)) THEN
              CALL sub_calc_carbconst(              &
                   & phys_ocn(ipo_Dmid,i,j,n_kmax), &
                   & ocn(io_T,i,j,n_kmax),          &
                   & ocn(io_S,i,j,n_kmax),          &
                   & carbconst(:,i,j,n_kmax)        &
                   & )
           end if
        end do
     end do

     ! *** DIAGNOTICS ***
     print*,""
     print'(" * GeMlite ",A10,A6)', &
          & " isteplite","   MOD"
     PRINT'(" * GeMlite ",I10,I6,F12.2,F10.3,F10.3,F10.3)', &
          & loc_isteplite,MOD(loc_isteplite,par_gemlite_stp), &
          & loc_t + par_misc_t_err, &
          & conv_mol_umol*SUM(atm(ia_pCO2,:,:)*phys_atm(ipa_V,:,:)/loc_tot_atm_V), &
          & conv_mol_umol*SUM(ocn(io_CO2,:,:,:)*phys_ocn(ipo_M,:,:,:))/loc_tot_ocn_M, &
          & conv_mol_umol*SUM(ocn(io_ALK,:,:,:)*phys_ocn(ipo_M,:,:,:))/loc_tot_ocn_M

     ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
     ! *** Geochemistry lite LOOP START ***
     ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
     
     do while ((loc_t > (par_gemlite_tyr_acc_start - par_gemlite_dtyr_acc)) .AND. (loc_t > 0.0))

        ! *** UPDATE FORCING TIME SERIES DATA ***
        ! recalculate time-varying flux forcings of the system
        ! NOTE: code to excecute every <sigstp> iterations
        IF (MOD(loc_isteplite,par_gemlite_sigstp) == 0) THEN
           ! ATMOSPHERE
           do ia=1,n_atm
              IF (atm_select(ia) .AND. force_flux_atm_select(ia)) THEN
                 CALL sub_update_force_flux_atm(loc_t,ia)
              END IF
           END DO
           ! OCEAN
           do io=1,n_ocn
              IF (ocn_select(io) .AND. force_flux_ocn_select(io)) THEN
                 CALL sub_update_force_flux_ocn(loc_t,io)
              END IF
           END DO
        end IF

        ! *** CALCULATE OCEAN-ATMOSPHERE FLUX FORCING ***
        ! NOTE: <force_flux_atm> and <force_flux_ocn> in units of (mol yr-1)
        ! NOTE: <loc_force_atm> and <loc_ocnsed_flux> in units of (mol per time step)
        ! NOTE: <force_flux_ocn> includes fixed system forcing fluxes
        ! add prescribed fixed system flux forcing
        ! NOTE: in units of (mol yr-1)
        IF (MOD(loc_isteplite,par_gemlite_sigstp) == 0) THEN
           do ia=1,n_atm
              IF (atm_select(ia)) loc_force_atm(ia) = loc_dtyr*SUM(force_flux_atm(ia,:,:))
           end do
           do io=1,n_ocn
              IF (ocn_select(io)) loc_force_ocn(io) = loc_dtyr*SUM(force_flux_ocn(io,:,:,:))
           end do
           loc_force_ocn(:) = loc_force_ocn(:) + loc_dtyr*matmul(conv_sed_ocn(:,:),par_force_flux_weather(:))
        end if

        ! *** GEOCHEMICAL INPUTS TO OCEAN-ATMOSPHERE-SEDIMENT SYSTEM ***
        ! *********************
        ! *** <INSERT CODE> ***
        ! *********************

        ! *** CORAL REEF MODULE ***
        ! *********************
        ! *** <INSERT CODE> ***
        ! *********************

        ! *** CALCULATE NET OCEAN-SEDIMENT FLUX ***
        ! NOTE: <interf_sedocn_flux> and <interf_ocnsed_flux> in untis of (mol m-2 s-1)
        ! NOTE: <loc_sedocn_flux> and <loc_ocnsed_flux> in units of (mol per time step)
        IF (MOD(loc_isteplite,par_gemlite_sigstp) == 0) THEN
           do io=1,n_ocn
              IF (ocn_select(io)) loc_interf_sedocn(io) = loc_dts*SUM(interf_ocnsed_phys(ipo_A,:,:)*interf_sedocn_flux(io,:,:))
           end do
           do is=1,n_sed
              IF (sed_select(is)) loc_interf_ocnsed(is) = loc_dts*SUM(interf_ocnsed_phys(ipo_A,:,:)*interf_ocnsed_flux(is,:,:))
           end do
        end if

        ! *** UPDATE INTEGRATED SYSTEM FLUX ***
        ! update value of integrated system (ocean) flux
        ! NOTE: integrated system (ocean) flux in units of (mol per time step)
        ! NOTE: convert atmospheric fluxes to ocean tracer currency
        ! NOTE: convert ocn->sed fluxes to ocean tracer currency
        if (.NOT. opt_misc(iopt_misc_sed_closedsystem)) then
           loc_dsystem(:) = loc_dsystem(:) + &
                & loc_force_ocn(:) + &
                & matmul(conv_atm_ocn(:,:),loc_force_atm(:)) + &
                & loc_interf_sedocn(:) - &
                & matmul(conv_sed_ocn(:,:),loc_interf_ocnsed(:))
        end if

        ! *** INCREMENT LOCAL LOOP COUNTER ***
        loc_isteplite = loc_isteplite + 1
        
        ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
        ! *** START ATM-OCN-SED UPDATE ***
        ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

        if (MOD(loc_isteplite,par_gemlite_stp) == 0) then

           ! *** UPDATE SEDIMENTS ***
           ! couple ocean-sediment system
           call tstep_sedgem(dum_t,real(par_gemlite_stp)*dum_dt,dum_tsc)

           ! *** UPDATE OCEAN-ATMOSPHERE ***
           if (.NOT. opt_misc(iopt_misc_sed_closedsystem)) then

              ! *** INITIALIZE LOCAL VARIABLES ***
              loc_dsystem_atm(:) = 0.0
              loc_dsystem_ocn(:) = 0.0

              ! *** UPDATE OCEAN INVENTORY ***
              ! update ocean tracers and reset integrated system flux
              ! NOTE: in ocean tracer concentration units of (mol kg-1)
              do io=1,n_ocn
                 IF (ocn_select(io)) ocn(io,:,:,:) = ocn(io,:,:,:) + loc_dsystem(io)/loc_tot_ocn_M
              end do

              ! *** REPARTITION CO2 BETWEEN OCEAN AND ATMOSPEHRE ***
              ! NOTE: store initial atmospheric CO2 inventory in order to derive net air-sea flux later
              loc_tot_pCO2_INIT = SUM(loc_conv_atm_mol(:,:)*atm(ia_pCO2,:,:))
              loc_tot_pCO2_NEW = loc_tot_pCO2_INIT
              loc_count = 0
              Do
                 loc_tot_pCO2_OLD = loc_tot_pCO2_NEW
                 ! calculate weighted mean equilibrium pCO2
                 ! NOTE: only update pCO2 over wet grid points
                 loc_tot_pCO2 = 0.0
                 DO i=1,n_imax
                    DO j=1,n_jmax
                       IF (n_kmax >= goldstein_k1(i,j)) THEN
                          ! calculate surface ocean carbonate chemistry
                          CALL sub_calc_carb(             &
                               & ocn(io_CO2,i,j,n_kmax),  &
                               & ocn(io_ALK,i,j,n_kmax),  &
                               & ocn(io_PO4,i,j,n_kmax),  &
                               & ocn(io_Si,i,j,n_kmax),   &
                               & ocn(io_B,i,j,n_kmax),    &
                               & ocn(io_Ca,i,j,n_kmax),   &
                               & carbconst(:,i,j,n_kmax), &
                               & carb(:,i,j,n_kmax)       &
                               & )
                          ! update pseudo-equilibrium atmospheric CO2 inventory - wet grid points
                          ! NOTE: use a mixture of re-calculated 'equilibrium' pCO2 values and pre-existing values,
                          !       depending on sea ice extent
                          loc_tot_pCO2 = loc_tot_pCO2 + &
                               & (1.0 - phys_atm(ipa_seaice,i,j))*loc_conv_atm_mol(i,j)*carb(ic_fug_CO2,i,j,n_kmax) + &
                               & phys_atm(ipa_seaice,i,j)*loc_conv_atm_mol(i,j)*atm(ia_pCO2,i,j)
                       else
                          ! update pseudo-equilibrium atmospheric CO2 inventory - dry grid points
                          ! NOTE: use pre-existing pCO2 values
                          loc_tot_pCO2 = loc_tot_pCO2 + &
                               & loc_conv_atm_mol(i,j)*atm(ia_pCO2,i,j)
                       end if
                    end do
                 end do
                 ! test for convergence (of CO2 atmospheric inventory)
                 loc_tot_pCO2_NEW = loc_tot_pCO2
                 IF (ABS(1.0 - loc_tot_pCO2_NEW/loc_tot_pCO2_OLD) < 0.0001) THEN
                    loc_tot_pCO2_NEW = loc_tot_pCO2_OLD
                    EXIT
                 ELSE
                    loc_count = loc_count + 1
                    loc_tot_pCO2_NEW = (loc_tot_pCO2_NEW*loc_tot_pCO2_OLD)**0.5
                    ocn(io_CO2,:,:,:) = ocn(io_CO2,:,:,:) + &
                         & (loc_tot_pCO2_OLD - loc_tot_pCO2_NEW)/loc_tot_ocn_M
!!$                    ! [DIAGNOSTICS DEBUG]
!!$                    print*,loc_count, &
!!$                         & 1.0E+06*(loc_tot_pCO2_NEW/loc_tot_atm_V)*conv_Pa_atm*const_R_SI* &
!!$                         & atm(ia_T,par_misc_debug_i,par_misc_debug_j)
                 END IF
                 ! update atmosphere
                 ! NOTE: make homogeneous
                 atm(ia_pCO2,:,:) = (loc_tot_pCO2_NEW/loc_tot_atm_V)*conv_Pa_atm*const_R_SI*atm(ia_T,:,:)
              end Do

              ! *** UPDATE OCN-SED INTERFACE ***
              ! update ocn-sed interface arrays
              ! NOTE: only need to update tracer info
              ! NOTE: do not need to update the mean ocn->sed flux
              DO i=1,n_imax
                 DO j=1,n_jmax
                    loc_k1 = goldstein_k1(i,j)
                    IF (n_kmax >= loc_k1) interf_ocnsed_ocn(:,i,j) = ocn(:,i,j,loc_k1)
                 END DO
              END DO

              ! *** AUDIT FLUX UPDATE ***
              ! update (if selected) accumulated flux into and out of the ocean for tracer inventory auditing purposes
              ! NOTE: take into acocunt loss to the atmosphere
              ! also update effective atmospheric change
              IF (opt_misc(iopt_misc_audit)) THEN

                 loc_dsystem_ocn(:) = loc_dsystem(:)
                 loc_dsystem_atm(:) = 0.0

                 loc_dsystem_ocn(io_CO2)  = loc_dsystem_ocn(io_CO2) - (loc_tot_pCO2_NEW - loc_tot_pCO2_INIT)
                 loc_dsystem_atm(ia_pCO2) = loc_dsystem_atm(ia_pCO2) + (loc_tot_pCO2_NEW - loc_tot_pCO2_INIT)
                 do io=1,n_ocn
                    IF (ocn_select(io)) then
                       audit_ocn_delta(io) = audit_ocn_delta(io) + loc_dsystem_ocn(io)
                    end if
                 end do
                 do ia=1,n_atm
                    IF (atm_select(ia)) then
                       audit_atm_delta(ia) = audit_atm_delta(ia) + loc_dsystem_atm(ia)
                    end if
                 end do
              end if

              ! *** BLAH ***
              loc_dsystem(:) = 0.0

           end if

        end if
        
        ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
        ! *** END ATM-OCN-SED UPDATE ***
        ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

        ! *** UPDATE TIME ***
        ! GOLDSTEIn 'time'
        ! NOTE: update GOLDSTEIn t0 to pass back to effect a time 'acceleration'
        dum_t = dum_t + dum_dt
        dum_t0 = dum_t0 + dum_dt
        ! GeM 'time'
        loc_t = ABS(par_misc_t_end - par_misc_t_start) - (dum_tsc*dum_t)/conv_yr_s
        ! time-series integration time interval
        loc_int_t_sig = loc_int_t_sig + loc_dtyr

        ! *** DIAGNOTICS ***
        if (MOD(loc_isteplite,par_gemlite_diagstp) == 0) then
           PRINT'(" * GeMlite ",I10,I6,F12.2,F10.3,F10.3,F10.3)', &
                & loc_isteplite,MOD(loc_isteplite,par_gemlite_stp), &
                & loc_t + par_misc_t_err, &
                & conv_mol_umol*SUM(atm(ia_pCO2,:,:)*phys_atm(ipa_V,:,:)/loc_tot_atm_V), &
                & conv_mol_umol*SUM(ocn(io_CO2,:,:,:)*phys_ocn(ipo_M,:,:,:))/loc_tot_ocn_M, &
                & conv_mol_umol*SUM(ocn(io_ALK,:,:,:)*phys_ocn(ipo_M,:,:,:))/loc_tot_ocn_M
        end if
        
        ! *** DATA SAVE - TIME-SLICE ***
        ! save time slice data
        ! NOTE: do not save BioGeM time-slice data, as it will probably have been re-set to zero,
        !       and the spatial pattern will remain constant - only the inventory evolves in GeMlite
        IF ( &
             & (loc_t <= par_data_save_timeslice(par_data_save_timeslice_i)) &
             & .AND. &
             & (par_data_save_timeslice_i > 0) &
             & ) THEN
           ! save time-slice data
           CALL sub_data_save_timeslice()
           CALL sub_data_save_timeslice_sed()
           ! update time slice index
           par_data_save_timeslice_i = par_data_save_timeslice_i - 1
        END IF

        ! *** DATA SAVE - TIME-SERIES ***
        IF ( &
             & (loc_t <= par_data_save_sig(par_data_save_sig_i)) &
             & .AND. &
             & par_data_save_sig_i > 0 &
             & ) THEN
           IF (opt_data(iopt_data_save_sig_ocn)) THEN
              do io=1,n_ocn
                 IF (ocn_select(io)) then
                    int_ocn_sig(io) = SUM(ocn(io,:,:,:)*phys_ocn(ipo_M,:,:,:))
                 end if
              end do
           end if
           IF (opt_data(iopt_data_save_sig_atm)) THEN
              do ia=1,n_atm
                 IF (atm_select(ia)) then
                    int_atm_sig(ia) = SUM(loc_conv_atm_mol(:,:)*atm(ia,:,:))
                 end if
              end do
           end if
!!$           IF (opt_data(iopt_data_save_sig_atm_focnatm)) THEN
!!$              int_ocnatm_flux_sig(:) =
!!$           end if
           ! save data
           ! NOTE: set dummy int_t_sig signal-averaging ingetrated time value 
           int_t_sig = 1.0
           CALL sub_data_save_runtime(par_data_save_sig(par_data_save_sig_i) + loc_int_t_sig/2.0)
           IF (par_data_save_sig_i > 1) par_data_save_sig_i = par_data_save_sig_i - 1
           loc_int_t_sig = 0.0
        end if

        ! *** AUDIT ***
        ! carry out an updated tracer inventory audit
        ! NOTE: code to excecute every <auditstp> iterations
        IF (opt_misc(iopt_misc_audit) .AND. MOD(loc_isteplite,par_misc_auditstp) == 0) THEN
           CALL sub_audit_update()
        END IF

     end do

     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
     ! *** Geochemistry lite LOOP END ***
     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

     ! *** SAVE OCN-SED DAIGNOZTICS ***
     ! save sed diag
     if (opt_sed(iopt_sed_save_diag)) call sub_data_save_seddiag(real(par_gemlite_stp)*loc_dtyr)
!!$     if (MOD(loc_isteplite,par_gemlite_stp) /= 0) then
!!$        ! *** UPDATE SEDIMENTS ***
!!$        ! couple ocean-sediment system
!!$        call sub_sedgem_update(dum_t,real(MOD(loc_isteplite,par_gemlite_stp))*dum_dt,dum_tsc)
!!$        ! *** UPDATE OCEAN-ATMOSPHERE ***
!!$        call sub_gemlite_ocnatm_update(loc_dsystem(:),loc_count)
!!$        ! *** DIAGNOTICS ***
!!$        PRINT'(" * GeMlite ",I10I6,F12.2,F10.3,F10.3,F10.3)', &
!!$             & loc_isteplite,MOD(loc_isteplite,par_gemlite_stp), &
!!$             & loc_t, &
!!$             & conv_mol_umol*SUM(atm(ia_pCO2,:,:)*phys_atm(ipa_V,:,:)/loc_tot_atm_V), &
!!$             & conv_mol_umol*SUM(ocn(io_CO2,:,:,:)*phys_ocn(ipo_M,:,:,:))/loc_tot_ocn_M, &
!!$             & conv_mol_umol*SUM(ocn(io_ALK,:,:,:)*phys_ocn(ipo_M,:,:,:))/loc_tot_ocn_M
!!$        ! save sed diag
!!$        if (opt_sed(iopt_sed_save_diag)) call sub_data_save_seddiag(real(MOD(loc_isteplite,par_gemlite_stp))*loc_dtyr)
!!$     else
!!$        ! save sed diag
!!$        if (opt_sed(iopt_sed_save_diag)) call sub_data_save_seddiag(real(par_gemlite_stp)*loc_dtyr)
!!$     end if

     ! *** RE-INITIALIZE NEXT MODEL CONTINUATION PERIOD AND CLEAN UP ***
     ! set next start date of subsequent acceleration period
     ! NOTE: loc_t counts DOWN in years (zero being the end of BioGeM integration),
     !       such that par_gemlite_dtyr_nonacc must be subtracted from gem_t
     If (loc_t > par_gemlite_dtyr_nonacc) then
        par_gemlite_tyr_acc_start = loc_t - par_gemlite_dtyr_nonacc
     else
        par_gemlite_tyr_acc_start = 0.0
     end if
     ! update GOLDSTEIn <ts> and <ts1> arrays
     call sub_biogem_copy_ocntots(dum_ts)
     call sub_biogem_copy_ocntots(dum_ts1)
     ! re-init time-slice
     CALL sub_init_int_timeslice()
     ! re-init time-series
     CALL sub_init_int_timeseries()

     ! *** BLAH ***
     print*,""

  end if

  ! *** TEST FOR MODEL TERMINATION ***
  ! if local time has surppassed the set end time, then set the flag updating system biogeochemistry to false
  ! NOTE: halt entire (coupled ocean-biogeochemistry) model if that option has been set
  IF (loc_t <= 0.0) THEN
     par_misc_t_go = .FALSE.
     IF (opt_misc(iopt_misc_terminate_BioGeM)) THEN
        CALL end_biogem()
        STOP
     END IF
  END IF

END SUBROUTINE tstep_gemlite


! *** RESTART SedGeM (save data) ***
SUBROUTINE rest_gemlite()
  IMPLICIT NONE
  ! *********************
  ! *** <INSERT CODE> ***
  ! *********************
END SUBROUTINE rest_gemlite

  
! *** END GeMlite ***
SUBROUTINE end_gemlite()
  IMPLICIT NONE
  ! *********************
  ! *** <INSERT CODE> ***
  ! *********************
END SUBROUTINE end_gemlite




  !        *******
  !    ***************
  !  ********************
  ! **********************
  ! *** CODE GRAVEYARD ***
  ! **********************
  ! **********************
  ! **      **  **      **
  ! **  **  **  **  **  **
  ! **  **  **  **  **  **
  ! **  *  ***  **      **
  ! **  **  **  **  ******
  ! **  **  **  **  ******
  ! **  **  **  **  ******
  ! **********************
  ! **********************
  ! **********************
  ! **********************


!!$! *** copy and make available to GeM, variable values local to <gset.f> in GOLDSTEIn ***
!!$SUBROUTINE sub_gem_init(dum_imax,dum_jmax,dum_kmax,dum_lmax)
!!$  USE gem_lib
!!$  USE gem_data
!!$  IMPLICIT NONE
!!$  ! dummy arguments
!!$  INTEGER,INTENT(in)::dum_imax,dum_jmax,dum_kmax,dum_lmax
!!$  ! load GeM run-time options
!!$  CALL sub_load_gem_config()
!!$  ! *** initialize global (interface) arrays ***
!!$  interf_ocnsed_ocn(:,:,:)      = 0.0
!!$  interf_ocnsed_phys(:,:,:)     = 0.0
!!$  interf_ocnsed_flux(:,:,:)     = 0.0
!!$  interf_sedocn_flux(:,:,:)     = 0.0
!!$  interf_ocnsed_flux_int(:,:,:) = 0.0
!!$  interf_sedocn_flux_int(:,:,:) = 0.0
!!$  interf_ocnatm_flux(:,:,:)     = 0.0
!!$  ! *** misc ***
!!$  ! copy grid dimensions
!!$  n_imax = dum_imax
!!$  n_jmax = dum_jmax
!!$  n_kmax = dum_kmax
!!$  n_lmax = dum_lmax
!!$  ! NOTE: instead of using value of GODLSTEIn <lmax> take default value from <n_maxl>
!!$  !       (since it can be reset later if too small)
!!$  n_lmax = n_maxl
!!$  ! set (compositional) relational operator solid and particulate forms
!!$  ! convert solid species -> dissolved
!!$  ! NOTE: populate unused elements with zero
!!$  conv_sed_ocn(:,:) = 0.0
!!$  conv_sed_ocn(io_CO2,is_POC)   = 1.0
!!$  conv_sed_ocn(io_PO4,is_POP)   = 1.0
!!$  conv_sed_ocn(io_NO3,is_PON)   = 1.0
!!$  conv_sed_ocn(io_ALK,is_PON)   = -0.7
!!$  conv_sed_ocn(io_Fe,is_POFe)   = 1.0
!!$  conv_sed_ocn(io_CO2,is_CaCO3) = 1.0
!!$  conv_sed_ocn(io_ALK,is_CaCO3) = 2.0
!!$  conv_sed_ocn(io_Ca,is_CaCO3)  = 1.0
!!$  ! convert dissolved species -> solid
!!$!  ! NOTE: simply transpose and take reciprocal
!!$!  conv_ocn_sed(:,:) = 1.0/(transpose(conv_sed_ocn(:,:)))
!!$  conv_ocn_sed(:,:) = 0.0
!!$  conv_ocn_sed(is_POC,io_CO2)   = 1.0/conv_sed_ocn(io_CO2,is_POC)
!!$  conv_ocn_sed(is_POP,io_PO4)   = 1.0/conv_sed_ocn(io_PO4,is_POP)
!!$  conv_ocn_sed(is_PON,io_NO3)   = 1.0/conv_sed_ocn(io_NO3,is_PON)
!!$  conv_ocn_sed(is_PON,io_ALK)   = 1.0/conv_sed_ocn(io_ALK,is_PON)
!!$  conv_ocn_sed(is_POFe,io_Fe)   = 1.0/conv_sed_ocn(io_Fe,is_POFe)
!!$  conv_ocn_sed(is_CaCO3,io_CO2) = 1.0/conv_sed_ocn(io_CO2,is_CaCO3)
!!$  conv_ocn_sed(is_CaCO3,io_ALK) = 1.0/conv_sed_ocn(io_ALK,is_CaCO3)
!!$  conv_ocn_sed(is_CaCO3,io_Ca)  = 1.0/conv_sed_ocn(io_Ca,is_CaCO3)
!!$END SUBROUTINE sub_gem_init


!!$! *** update time ***
!!$SUBROUTINE sub_gem_t_update(dum_t,dum_dt,dum_tsc)
!!$  USE gem_lib
!!$  IMPLICIT NONE
!!$  ! dummy arguments
!!$  REAL,INTENT(in)::dum_t,dum_dt,dum_tsc
!!$  ! current model time and time step length
!!$  ! NOTE: convert between time units of BioGeM (years) and GOLDSTEIn (use <tsc> scale factor to convert to seconds)
!!$  gem_t = ABS(par_misc_t_end - par_misc_t_start) - (dum_tsc*dum_t)/conv_yr_s
!!$  gem_dt = (dum_tsc*dum_dt)/conv_yr_s
!!$end SUBROUTINE sub_gem_t_update


!!$! *** GeMlite sed update ***
!!$SUBROUTINE sub_gemlite_sed_update(dum_t,dum_dt,dum_tsc)
!!$  USE sedgem_lib
!!$  USE sedgem_box
!!$  IMPLICIT NONE
!!$  ! dummy arguments
!!$  REAL,INTENT(in)::dum_t,dum_dt,dum_tsc
!!$  ! local variables
!!$  integer::i,j
!!$  real::loc_t
!!$  real::loc_dtyr ! local time step in years
!!$  real::loc_dts  ! local time step in seconds
!!$
!!$  ! *** CALCULATE SEDGEM TIME ***
!!$  ! calculate sediment model time step length
!!$  ! NOTE: convert between time units of BioGeM (years) and GOLDSTEIn (use <tsc> scale factor to convert to seconds)
!!$  loc_t = ABS(par_misc_t_end - par_misc_t_start) - (dum_tsc*dum_t)/conv_yr_s
!!$  loc_dts  = dum_tsc*dum_dt
!!$  loc_dtyr = loc_dts/conv_yr_s
!!$
!!$  ! ************************
!!$  ! *** UPDATE SEDIMENTS ***
!!$  ! ************************
!!$
!!$  ! *** couple ocean-sediment system ***
!!$  DO i=1,n_imax
!!$     DO j=1,n_jmax
!!$        IF (sed_mask(i,j)) THEN
!!$           ! calculate sediment rain flux from sediment->ocean flux
!!$           ! NOTE: the cumulate mass flux is calculate within routine calc_bio_remin
!!$           ! NOTE: <interf_ocnsed_flux> in units of (mol m-2 s-1)
!!$           ! NOTE: <sed_fsed> in units of (mol cm-2)
!!$           sed_fsed(:,i,j) = loc_dts*interf_ocnsed_flux(:,i,j)/conv_m2_cm2
!!$           ! ammend sediment rain flux according to prescribed detrital input
!!$           ! NOTE: convert units from (g cm-2 kyr-1) to (mol cm-2)
!!$           sed_fsed(is_det,i,j) = sed_fsed(is_det,i,j) + conv_det_g_mol*par_sed_frefrac*(conv_yr_kyr*loc_dtyr)
!!$           ! update sediments
!!$           call sub_update_sed(                      &
!!$                & loc_t,loc_dtyr,i,j,                &
!!$                & interf_ocnsed_phys(ipo_Dbot,i,j),  &
!!$                & interf_ocnsed_ocn(io_T,i,j),       &
!!$                & interf_ocnsed_ocn(io_S,i,j),       &
!!$                & interf_ocnsed_ocn(io_CO2,i,j),     &
!!$                & interf_ocnsed_ocn(io_CO2_13C,i,j), &
!!$                & interf_ocnsed_ocn(io_ALK,i,j),     &
!!$                & interf_ocnsed_ocn(io_PO4,i,j),     &
!!$                & interf_ocnsed_ocn(io_Si,i,j),      &
!!$                & interf_ocnsed_ocn(io_O2,i,j),      &
!!$                & interf_ocnsed_ocn(io_O2_18O,i,j),  &
!!$                & interf_ocnsed_ocn(io_B,i,j),       &
!!$                & interf_ocnsed_ocn(io_Ca,i,j)       &
!!$                & )
!!$        end if
!!$     end do
!!$  end do
!!$
!!$
!!$  ! *** update sed->ocn interface ***
!!$  ! NOTE: <sed_fdis> in units of (mol cm-2)
!!$  ! NOTE: <interf_sedocn_flux> in units of (mol m-2 s-1)
!!$  ! NOTE: convert sediment tracer flux to dissolved tracer form
!!$  DO i=1,n_imax
!!$     DO j=1,n_jmax
!!$        IF (sed_mask(i,j)) THEN
!!$           interf_sedocn_flux(:,i,j) = conv_m2_cm2*matmul(conv_sed_ocn(:,:),sed_fdis(:,i,j))/loc_dts
!!$        end if
!!$     end do
!!$  end do
!!$  ! update sediment interface composition data
!!$  call sub_set_sed_coretop_data()
!!$end SUBROUTINE sub_gemlite_sed_update


!!$! *** GeMlite ocn-atm update ***
!!$SUBROUTINE sub_gemlite_ocnatm_update(dum_dsystem,dum_count)
!!$  USE biogem_lib
!!$  USE gemlite_lib
!!$  USE gem_box
!!$  IMPLICIT NONE
!!$  ! dummy arguments
!!$  real,DIMENSION(0:n_ocn),INTENT(inout)::dum_dsystem
!!$  integer,intent(out)::dum_count
!!$  ! local variables
!!$  integer::i,j,io,ia
!!$  integer::loc_count
!!$  integer::loc_k1
!!$  real::loc_tot_ocn_M,loc_tot_atm_V
!!$  real::loc_tot_pCO2,loc_tot_pCO2_INIT,loc_tot_pCO2_NEW,loc_tot_pCO2_OLD
!!$  REAL,DIMENSION(n_maxi,n_maxj)::loc_conv_atm_mol
!!$  real,DIMENSION(0:n_atm)::loc_dsystem_atm
!!$  real,DIMENSION(0:n_ocn)::loc_dsystem_ocn
!!$
!!$  ! *** INITIALIZE LOCAL VARIABLES ***
!!$  loc_dsystem_atm(:) = 0.0
!!$  loc_dsystem_ocn(:) = 0.0
!!$  
!!$  ! *** SET LOCAL CONSTANTS ***
!!$  ! total ocean mass and atmospheric volume
!!$  loc_tot_ocn_M = sum(phys_ocn(ipo_M,:,:,:))
!!$  loc_tot_atm_V = sum(phys_atm(ipa_V,:,:))
!!$  ! local constants for converting between partial pressure and molar quantity
!!$  loc_conv_atm_mol(:,:) = phys_atm(ipa_V,:,:)/(conv_Pa_atm*const_R_SI*atm(ia_T,:,:))
!!$
!!$  ! *******************************
!!$  ! *** UPDATE OCEAN-ATMOSPHERE ***
!!$  ! *******************************
!!$  
!!$  ! *** UPDATE OCEAN INVENTORY ***
!!$  ! update ocean tracers and reset integrated system flux
!!$  ! NOTE: in ocean tracer concentration units of (mol kg-1)
!!$  do io=1,n_ocn
!!$     IF (ocn_select(io)) ocn(io,:,:,:) = ocn(io,:,:,:) + dum_dsystem(io)/loc_tot_ocn_M
!!$  end do
!!$
!!$  ! *** REPARTITION CO2 BETWEEN OCEAN AND ATMOSPEHRE ***
!!$  ! NOTE: store initial atmospheric CO2 inventory in order to derive net air-sea flux later
!!$  loc_tot_pCO2_INIT = SUM(loc_conv_atm_mol(:,:)*atm(ia_pCO2,:,:))
!!$  loc_tot_pCO2_NEW = loc_tot_pCO2_INIT
!!$  loc_count = 0
!!$  Do
!!$     loc_tot_pCO2_OLD = loc_tot_pCO2_NEW
!!$     ! calculate weighted mean equilibrium pCO2
!!$     ! NOTE: only update pCO2 over wet grid points
!!$     loc_tot_pCO2 = 0.0
!!$     DO i=1,n_imax
!!$        DO j=1,n_jmax
!!$           IF (n_kmax >= goldstein_k1(i,j)) THEN
!!$              ! calculate surface ocean carbonate chemistry
!!$              CALL sub_calc_carb(             &
!!$                   & ocn(io_CO2,i,j,n_kmax),  &
!!$                   & ocn(io_ALK,i,j,n_kmax),  &
!!$                   & ocn(io_PO4,i,j,n_kmax),  &
!!$                   & ocn(io_Si,i,j,n_kmax),   &
!!$                   & ocn(io_B,i,j,n_kmax),    &
!!$                   & ocn(io_Ca,i,j,n_kmax),   &
!!$                   & carbconst(:,i,j,n_kmax), &
!!$                   & carb(:,i,j,n_kmax)       &
!!$                   & )
!!$              ! update pseudo-equilibrium atmospheric CO2 inventory - wet grid points
!!$              ! NOTE: use a mixture of re-calculated 'equilibrium' pCO2 values and pre-existing values,
!!$              !       depending on sea ice extent
!!$              loc_tot_pCO2 = loc_tot_pCO2 + &
!!$                   & (1.0 - phys_atm(ipa_seaice,i,j))*loc_conv_atm_mol(i,j)*carb(ic_fug_CO2,i,j,n_kmax) + &
!!$                   & phys_atm(ipa_seaice,i,j)*loc_conv_atm_mol(i,j)*atm(ia_pCO2,i,j)
!!$           else
!!$              ! update pseudo-equilibrium atmospheric CO2 inventory - dry grid points
!!$              ! NOTE: use pre-existing pCO2 values
!!$              loc_tot_pCO2 = loc_tot_pCO2 + &
!!$                   & loc_conv_atm_mol(i,j)*atm(ia_pCO2,i,j)
!!$           end if
!!$        end do
!!$     end do
!!$     ! test for convergence (of CO2 atmospheric inventory)
!!$     loc_tot_pCO2_NEW = loc_tot_pCO2
!!$     IF (ABS(1.0 - loc_tot_pCO2_NEW/loc_tot_pCO2_OLD) < 0.0001) THEN
!!$        loc_tot_pCO2_NEW = loc_tot_pCO2_OLD
!!$        EXIT
!!$     ELSE
!!$        loc_count = loc_count + 1
!!$        loc_tot_pCO2_NEW = (loc_tot_pCO2_NEW*loc_tot_pCO2_OLD)**0.5
!!$        ocn(io_CO2,:,:,:) = ocn(io_CO2,:,:,:) + &
!!$             & (loc_tot_pCO2_OLD - loc_tot_pCO2_NEW)/loc_tot_ocn_M
!!$        ! [DIAGNOSTICS DEBUG]
!!$        print*,loc_count, &
!!$             & 1.0E+06*(loc_tot_pCO2_NEW/loc_tot_atm_V)*conv_Pa_atm*const_R_SI*atm(ia_T,par_misc_debug_i,par_misc_debug_j)
!!$     END IF
!!$     ! update atmosphere
!!$     ! NOTE: make homogeneous
!!$     atm(ia_pCO2,:,:) = (loc_tot_pCO2_NEW/loc_tot_atm_V)*conv_Pa_atm*const_R_SI*atm(ia_T,:,:)
!!$  end Do
!!$
!!$  ! *** UPDATE OCN-SED INTERFACE ***
!!$  ! update ocn-sed interface arrays
!!$  ! NOTE: only need to update tracer info
!!$  ! NOTE: do not need to update the mean ocn->sed flux
!!$  DO i=1,n_imax
!!$     DO j=1,n_jmax
!!$        loc_k1 = goldstein_k1(i,j)
!!$        IF (n_kmax >= loc_k1) interf_ocnsed_ocn(:,i,j) = ocn(:,i,j,loc_k1)
!!$     END DO
!!$  END DO
!!$
!!$  ! *** AUDIT FLUX UPDATE ***
!!$  ! update (if selected) accumulated flux into and out of the ocean for tracer inventory auditing purposes
!!$  ! NOTE: take into acocunt loss to the atmosphere
!!$  ! also update effective atmospheric change
!!$  IF (opt_misc(iopt_misc_audit)) THEN
!!$
!!$     loc_dsystem_ocn(:) = dum_dsystem(:)
!!$     loc_dsystem_atm(:) = 0.0
!!$
!!$     loc_dsystem_ocn(io_CO2)  = dum_dsystem(io_CO2) - (loc_tot_pCO2_NEW - loc_tot_pCO2_INIT)
!!$     loc_dsystem_atm(ia_pCO2) = (loc_tot_pCO2_NEW - loc_tot_pCO2_INIT)
!!$     do io=1,n_ocn
!!$        IF (ocn_select(io)) then
!!$           audit_ocn_delta(io) = audit_ocn_delta(io) + loc_dsystem_ocn(io)
!!$        end if
!!$     end do
!!$     do ia=1,n_atm
!!$        IF (atm_select(ia)) then
!!$           audit_atm_delta(ia) = audit_atm_delta(ia) + loc_dsystem_atm(ia)
!!$        end if
!!$     end do
!!$  end if
!!$  
!!$  ! *** BLAH ***
!!$  dum_dsystem(:) = 0.0
!!$  
!!$end SUBROUTINE sub_gemlite_ocnatm_update
