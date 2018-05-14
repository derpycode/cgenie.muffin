
! ******************************************************************************************************************************** !
SUBROUTINE initialise_sedgem( &
     & dum_genie_timestep,    &
     & dum_sfxsumsed,         &
     & dum_sfcsumocn,         &
     & dum_sfcsed,            &
     & dum_sfxocn)
  USE sedgem_lib
  USE sedgem_data
  USE genie_util, ONLY: check_iostat
  IMPLICIT NONE
  ! dummy arguments
  REAL,INTENT(in)::dum_genie_timestep                          ! genie time-step (in seconds)
  real,dimension(n_sed,n_i,n_j),intent(inout)::dum_sfxsumsed
  real,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfcsumocn
  real,dimension(n_sed,n_i,n_j),intent(inout)::dum_sfcsed
  real,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxocn
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  integer::i,j                                                 ! local counting variables
  integer::loc_n_sed_stack_top                                 !
  real::loc_sed_stack_top_th                                   ! 
  real::loc_vol_top,loc_poros_top                            
  real::loc_vol,loc_poros                              

  print*,'======================================================='
  print*,' >>> Initialising SEDGEM sediment geochem. module ...'

  ! ---------------------------------------------------------- ! load goin information
  IF (ctrl_misc_debug2) print*, 'load goin information'
  call sub_load_goin_sedgem()
  ! ---------------------------------------------------------- ! initialize interface arrays 
  IF (ctrl_misc_debug2) print*, 'initialize interface arrays'
  dum_sfxsumsed(:,:,:) = 0.0   ! 
  dum_sfcsumocn(:,:,:) = 0.0   ! 
  dum_sfcsed(:,:,:)    = 0.0   ! 
  dum_sfxocn(:,:,:)    = 0.0   ! 
  ! ---------------------------------------------------------- ! allocate arrays
  IF (ctrl_misc_debug2) print*, 'allocate arrays'
  ! *** dimension the size of the 2-D sediment grid arrays ***
  ! NOTE: check for problems allocating array space
  ALLOCATE(phys_sed(n_phys_sed,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_mask(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_mask_reef(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_mask_muds(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed(n_sed,n_i,n_j,n_sed_tot),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_top(n_sed,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_top_h(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_top_INTdth(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_fsed(n_sed,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_fdis(n_sed,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sedocn_fnet(n_ocn,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_carb(n_carb,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_carbconst(n_carbconst,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_carbalk(n_carbalk,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_carbisor(n_carbisor,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_save_mask(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_fsed_OLD(n_sed,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_fdis_OLD(n_sed,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_Fsed_det(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_Fsed_caco3(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_Fsed_opal(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(sed_diag(n_diag_sed,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! ---------------------------------------------------------- ! initialize allocated arrays
  IF (ctrl_misc_debug2) print*, 'initialize allocated arrays'
  ! initialize dynamically-allocated arrays (those not done elsewhere)
  sed_fsed(:,:,:)      = 0.0   !
  sed_fdis(:,:,:)      = 0.0   !
  sedocn_fnet(:,:,:)   = 0.0   !
  sed_carb(:,:,:)      = 0.0   !
  sed_carbconst(:,:,:) = 0.0   !
  ! ---------------------------------------------------------- ! main initialization
  IF (ctrl_misc_debug2) print*, 'main initialization'
  ! setup SedGeM grid
  CALL sub_init_phys_sed()
  ! set meta-options and verify self-consistency of chosen parameters
  call sub_check_par_sedgem()
  ! initialize sediment sub-system
  call sub_init_sed()
  call sub_init_sed_layers_default()
  ! initialize sediment core location data save mask
  call sub_init_sedgem_save_sed_data()
  ! initialize sediment core environmental conditions saving
  if (ctrl_data_save_sedcorenv) call sub_sedgem_init_sedcoresenv()
  ! seed the aqueous carbonate system with an initial value of [H+]
  sed_carb(ic_H,:,:) = 1.0E-8
  ! initialize bioturbation profile
  IF (ctrl_sed_bioturb) THEN
     if (ctrl_sed_bioturb_Archer) then
        ALLOCATE(par_sed_mix_k(0:par_n_sed_mix),STAT=error)
     else
        ! load bioturbation profile data
        call sub_load_sed_mix_k()
     end if
  end IF
  ! set initial sediment age
  sed_age = par_misc_t_runtime - 0.5*conv_s_yr*(dum_genie_timestep*kocn_loop*conv_kocn_ksedgem)
  ! set ash event (to mark run start)
  par_sed_ashevent = .true.
  ! initialize sedcorenv time counters
  sed_time      = 0.0
  sed_time_save = 0.0
  ! ---------------------------------------------------------- ! INITIALIZE netCDF OUTPUT
  IF (ctrl_misc_debug2) print*, 'INITIALIZE netCDF OUTPUT'
  string_ncout2d   = TRIM(par_outdir_name)//'fields_sedgem_2d.nc'
  IF (ctrl_timeseries_output) THEN
     IF (ctrl_continuing.AND.ctrl_append_data) THEN
        OPEN(unit=in,status='old',file=TRIM(par_rstdir_name)//'netcdf_record_numbers',form='formatted',action='read')
        READ(unit=in,fmt='(i6)') ntrec_sout
        close(unit=in)
     ELSE
        ntrec_sout = 0
     ENDIF
     print*, 'netcdf record number: ',ntrec_sout
     print*,'par_outdir_name = par_rstdir_name:',par_outdir_name.eq.par_rstdir_name
  ELSE
     ntrec_sout = 0
  ENDIF
  ! ---------------------------------------------------------- ! INITIALIZE DATA SAVING
  IF (ctrl_misc_debug2) print*, 'INITIALIZE DATA SAVING'
  IF (ctrl_timeseries_output) THEN
     ! initialize timestep counter
     tstep_count = 0
     tsteps_per_year = conv_yr_s/(dum_genie_timestep*kocn_loop*conv_kocn_ksedgem)
     PRINT*,'timesteps per year                                  :',tsteps_per_year
     ! load in years for output generation
     CALL sub_data_output_years()
     year = min(output_years_0d(output_counter_0d),output_years_2d(output_counter_2d))
  ENDIF
  ! ---------------------------------------------------------- ! LOAD SEDIMENT RE-START
  IF (ctrl_misc_debug2) print*, 'LOAD SEDIMENT RE-START'
  IF (ctrl_continuing) then
     call sub_data_load_rst(dum_sfxsumsed,dum_sfxocn)
     ! modify sediment ages
     if (sed_select(is_CaCO3_age)) then
        sed(is_CaCO3_age,:,:,:)    = sed(is_CaCO3_age,:,:,:)    + par_misc_t_runtime*sed(is_CaCO3,:,:,:)
        sed_top(is_CaCO3_age,:,:)  = sed_top(is_CaCO3_age,:,:)  + par_misc_t_runtime*sed_top(is_CaCO3,:,:)
     end if
     if (sed_select(is_det_age)) then
        sed(is_det_age,:,:,:)    = sed(is_det_age,:,:,:)    + par_misc_t_runtime*sed(is_det,:,:,:)
        sed_top(is_det_age,:,:)  = sed_top(is_det_age,:,:)  + par_misc_t_runtime*sed_top(is_det,:,:)
     end if
     ! ------------------------------------------------------- ! modify numerical CaCO3 tracers
     if (ctrl_sed_dyerestart .AND. sed_select(is_CaCO3_red)) then
        DO i=1,n_i
           DO j=1,n_j
              IF (sed_mask(i,j)) THEN
                 ! set local (loop) cariables
                 loc_n_sed_stack_top  = INT(sed_top_h(i,j)) + 1
                 loc_sed_stack_top_th = sed_top_h(i,j) - REAL(loc_n_sed_stack_top - 1)
                 loc_vol_top = fun_calc_sed_vol(sed_top(:,i,j))
                 loc_poros_top = fun_calc_sed_poros_nsur(sed_top(is_CaCO3,i,j)/loc_vol_top,par_sed_top_th)
                 loc_vol = fun_calc_sed_vol(sed(:,i,j,loc_n_sed_stack_top))
                 loc_poros = fun_calc_sed_poros(sed(is_CaCO3,i,j,loc_n_sed_stack_top)/loc_vol)
                 ! tag core-top (normalize for porosity difference)
                 sed_top(is_CaCO3_red,i,j) = 1.0*sed_top(is_CaCO3,i,j)*(1.0 - loc_poros)/(1.0 - loc_poros_top)
                 sed_top(is_CaCO3_blue,i,j) = 0.0
                 ! tag red core segment
                 sed(is_CaCO3_red,i,j,(loc_n_sed_stack_top - par_sed_dyerestart_n + 2):loc_n_sed_stack_top) = &
                      & 1.0*sed(is_CaCO3,i,j,(loc_n_sed_stack_top - par_sed_dyerestart_n + 2):loc_n_sed_stack_top)
                 sed(is_CaCO3_blue,i,j,(loc_n_sed_stack_top - par_sed_dyerestart_n + 2):loc_n_sed_stack_top) = 0.0
                 ! tag base of red core sedgment (adjust to take into account incomplete core stack layer)
                 sed(is_CaCO3_red,i,j,loc_n_sed_stack_top - par_sed_dyerestart_n + 1) = &
                      & (1.0 - loc_sed_stack_top_th)*sed(is_CaCO3,i,j,loc_n_sed_stack_top - par_sed_dyerestart_n + 1)
                 sed(is_CaCO3_blue,i,j,loc_n_sed_stack_top - par_sed_dyerestart_n + 1) = &
                      & loc_sed_stack_top_th*sed(is_CaCO3,i,j,loc_n_sed_stack_top - par_sed_dyerestart_n + 1)
                 ! tag blue core segment
                 sed(is_CaCO3_red,i,j,1:(loc_n_sed_stack_top - par_sed_dyerestart_n)) = 0.0
                 sed(is_CaCO3_blue,i,j,1:(loc_n_sed_stack_top - par_sed_dyerestart_n)) = &
                      & 1.0*sed(is_CaCO3,i,j,1:(loc_n_sed_stack_top - par_sed_dyerestart_n))
              end if
           end DO
        end do
     end if
     ! update sediment interface composition data arrays with restart 
     dum_sfcsed(:,:,:) = fun_sed_coretop()
  end if
  ! ---------------------------------------------------------- ! INITIALIZE SEDCORES
  IF (ctrl_misc_debug2) print*, 'INITIALIZE SEDCORES'
  ! set number of sedcore array tracers
  n_sedcore_tracer = n_l_sed
  ! set number of sedcore array tracers
  ! NOTE: try and adapt the number of sedcore store layers allocated depending on runtime, e.g. 10 x kyr
  n_sedcore_tot = par_n_sedcore_tot_min + par_n_sedcore_tot_perky*int(par_misc_t_runtime/1000.0)
  ! initialize sedcores
  call sub_data_sedcore_init()

  print*,' <<< Initialisation complete'
  print*,'======================================================='

end SUBROUTINE initialise_sedgem
! ******************************************************************************************************************************** !
