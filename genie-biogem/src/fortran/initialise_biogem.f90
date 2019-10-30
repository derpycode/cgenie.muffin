
! ******************************************************************************************************************************** !
! SETUP BioGeM
! ******************************************************************************************************************************** !
SUBROUTINE initialise_biogem(                       &
     & dum_saln0,dum_rhoair,dum_cd,dum_ds,dum_dphi, &
     & dum_usc,dum_dsc,dum_fsc,dum_rh0sc,           &
     & dum_rhosc,dum_cpsc,dum_solconst,dum_scf,     &
     & dum_ips,dum_ipf,dum_ias,dum_iaf,dum_jsf,     &
     & dum_k1,                                      &
     & dum_dz,dum_dza,                              &
     & dum_c,dum_cv,dum_s,dum_sv,                   &
     & dum_ts,                                      &
     & dum_ts1,                                     &
     & dum_sfcatm1,                                 &
     & dum_sfxatm1,                                 &
     & dum_sfcocn1,                                 &
     & dum_sfxocn1,                                 &
     & dum_sfcsed1,                                 &
     & dum_sfxsed1                                  &
     & )
  USE biogem_lib
  USE biogem_data
  USE biogem_data_ascii
  USE genie_util, ONLY: check_iostat
  ! ---------------------------------------------------------- !
  ! DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  REAL,INTENT(in)::dum_saln0,dum_rhoair,dum_cd,dum_ds,dum_dphi   !
  real,INTENT(in)::dum_usc,dum_dsc,dum_fsc,dum_rh0sc             !
  real,INTENT(in)::dum_rhosc,dum_cpsc,dum_solconst,dum_scf       !
  INTEGER,INTENT(in),DIMENSION(n_j)::dum_ips,dum_ipf             !
  INTEGER,INTENT(in),DIMENSION(n_j)::dum_ias,dum_iaf             !
  INTEGER,INTENT(in)::dum_jsf                                    !
  integer,DIMENSION(n_i,n_j),INTENT(in)::dum_k1                  !
  REAL,DIMENSION(n_k),INTENT(in)::dum_dz,dum_dza                 !
  REAL,DIMENSION(0:n_j),INTENT(in)::dum_c,dum_cv,dum_s,dum_sv    !
  REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(inout)::dum_ts   ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
  REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(inout)::dum_ts1  ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
  real,intent(inout),dimension(n_atm,n_i,n_j)::dum_sfcatm1       ! atmosphere-surface tracer composition; occ grid
  real,intent(inout),dimension(n_atm,n_i,n_j)::dum_sfxatm1       ! atmosphere-surface fluxes; ocn grid
  real,intent(inout),dimension(n_ocn,n_i,n_j)::dum_sfcocn1       ! sediment-surface ocean tracer composition; ocn grid
  real,intent(inout),dimension(n_ocn,n_i,n_j)::dum_sfxocn1       ! sediment-surface (sed->ocn) fluxes; ocn grid
  real,intent(inout),dimension(n_sed,n_i,n_j)::dum_sfcsed1       ! sediment-surface sediment composition; ocn grid
  real,intent(inout),dimension(n_sed,n_i,n_j)::dum_sfxsed1       ! sediment-surface (ocn->sed) fluxes; ocn grid
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  integer::loc_iou
  integer::i,j,k,n
  integer::n_wet_grid,loc_k1
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  print*,'======================================================='
  print*,' >>> Initialising BIOGEM ocean biogeochem. module ...'
  ! ---------------------------------------------------------- ! load GOIN
  call sub_load_goin_biogem()
  ! ---------------------------------------------------------- ! set time
  ! NOTE: modify 'par_misc_t_start' according to the run-time accumulated in any requested restart,
  !       so that the time that BioGeM starts with is the same as the requested start time
  !       (BioGeM calculates its internal time as equal to par_misc_t_start + elapsed GOLDSTEIn time,
  !        BUT, elapsed GOLDSTEIn time will include any accumulated restart time,
  !        hence accumulated restart time is subtracted from par_misc_t_start)
  ! NOTE: I'm sure that this doesn't make any sense at all ... :(
  if (ctrl_misc_t_BP) then
     par_misc_t_end = par_misc_t_start - par_misc_t_runtime
  else
     par_misc_t_end = par_misc_t_start + par_misc_t_runtime
  end if
  ! ---------------------------------------------------------- !
  ! *** copy GOLDSTEIn parameters ***
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! copy dimensional scale factors for ocean
  goldstein_usc    = dum_usc
  goldstein_dsc    = dum_dsc
  goldstein_fsc    = dum_fsc
  goldstein_rh0sc  = dum_rh0sc
  goldstein_rhosc  = dum_rhosc
  goldstein_cpsc   = dum_cpsc
  ! ---------------------------------------------------------- ! copy miscellaneous constants
  ! NOTE: use identical value of pi to avoid possible GOLDETSIn/BioGeM mis-match in the calculation of ocean areas and volumes
  goldstein_saln0    = dum_saln0
  goldstein_rhoair   = dum_rhoair
  goldstein_cd       = dum_cd
  goldstein_ds       = dum_ds
  goldstein_dphi     = dum_dphi
  goldstein_scf      = dum_scf
  phys_solar_constant = dum_solconst
  ! ---------------------------------------------------------- ! copy ocean positions
  goldstein_jsf = dum_jsf
  goldstein_ips(:) = dum_ips(:)
  goldstein_ipf(:) = dum_ipf(:)
  goldstein_ias(:) = dum_ias(:)
  goldstein_iaf(:) = dum_iaf(:)
  ! ---------------------------------------------------------- ! copy grid information
  goldstein_k1(:,:) = dum_k1(:,:)
  goldstein_dz(:)  = dum_dz(:)
  goldstein_dza(:) = dum_dza(:)
  goldstein_c(:)   = dum_c(:)
  goldstein_cv(:)  = dum_cv(:)
  goldstein_s(:)   = dum_s(:)
  goldstein_sv(:)  = dum_sv(:)
  ! ---------------------------------------------------------- !
  ! *** define ts->ocn offset and copy <ts> array information ***
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! define ofset between GOLDSTEIn tracer units and BioGeM
  ! => temperature is degrees C in GOLDSTEIn, but K in BioGeM
  ! => salinity is as a relative deviation (o/oo) (from saln0) in GOLDSTEIn, but absolute (o/oo) in BioGeM
  tstoocn_offset(:) = 0.0
  tstoocn_offset(1) = const_zeroC
  tstoocn_offset(2) = goldstein_saln0
  ! ---------------------------------------------------------- !
  ! *** set-up biogem-sedgem and biogem-atchem interfacing ***
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! initialize interface arrays
  dum_sfcatm1(:,:,:) = 0.0
  dum_sfxatm1(:,:,:) = 0.0
  dum_sfcocn1(:,:,:) = 0.0
  dum_sfxocn1(:,:,:) = 0.0
  dum_sfcsed1(:,:,:) = 0.0
  dum_sfxsed1(:,:,:) = 0.0
  ! ---------------------------------------------------------- ! define relationships between tracers
  CALL sub_def_tracerrelationships()
  ! ---------------------------------------------------------- !
  !  INITIALIZE ARRAYS -- EXPLICIT GRID
  ! ---------------------------------------------------------- !
  ! NOTE: check for problems allocating array space
  !- --------------------------------------------------------- ! explicit grid: restoring forcing
  ALLOCATE(force_restore_locn(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(force_restore_locn_I(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(force_restore_locn_II(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  !- --------------------------------------------------------- ! flux grid: restoring forcing
  ALLOCATE(force_flux_locn(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(force_flux_locn_I(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(force_flux_locn_II(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  !- --------------------------------------------------------- ! data saving
  if (ctrl_data_save_3d_sig) then
     ALLOCATE(int_misc_3D_sig(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
  end if
  ! ---------------------------------------------------------- !
  !  INITIALIZE ARRAYS -- VECTORIZED -- 3D
  ! ---------------------------------------------------------- !
  ! NOTE: check for problems allocating array space
  ! ---------------------------------------------------------- ! calculate array grid point dimension
  ! count number of wet grid points
  n_vocn = 0
  DO i=1,n_i
     DO j=1,n_j
        IF (n_k >= goldstein_k1(i,j)) THEN
           n_vocn = n_vocn + 1
        end if
     end do
  end do
  ! ---------------------------------------------------------- ! allocate arrays: 3D
  ALLOCATE(vocn(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(vdocn(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(vbio_remin(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(vbio_part(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(vdbio_part(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(vphys_ocn(1:n_vocn),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  allocate(matrix_exp(1:n_vocn),stat=alloc_error)		! JDW: allocate matrix experiment storage array in one dimension
  call check_iostat(alloc_error,__LINE__,__FILE__)
  do n=1,n_vocn
     allocate(vocn(n)%mk(1:n_l_ocn,1:n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     allocate(vdocn(n)%mk(1:n_l_ocn,1:n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     allocate(vbio_remin(n)%mk(1:n_l_ocn,1:n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     allocate(vbio_part(n)%mk(1:n_l_sed,1:n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     allocate(vdbio_part(n)%mk(1:n_l_sed,1:n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     allocate(vphys_ocn(n)%mk(1:n_phys_ocn,1:n_k),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     allocate(matrix_exp(n)%mk(1:6,1:n_k),stat=alloc_error)		! JDW: allocate inner matrix array for *6* colour tracers
     call check_iostat(alloc_error,__LINE__,__FILE__)
  end do
  ! ---------------------------------------------------------- ! initialize arrays: 3D
  ! NOTE: explicit grid allocated arrays are initialized through the call to sub_init_force
  vocn(:)       = fun_lib_init_vocn()
  vdocn(:)      = fun_lib_init_vocn()
  vbio_remin(:) = fun_lib_init_vocn()
  vbio_part(:)  = fun_lib_init_vsed()
  vdbio_part(:) = fun_lib_init_vsed()
  vphys_ocn(:)  = fun_lib_init_vocn_n(n_phys_ocn)
  matrix_exp(:) = fun_lib_init_vocn()		! JDW: initialize matrix experiment array
  ! ---------------------------------------------------------- !
  !  INITIALIZE ARRAYS -- VECTORIZED -- 2D
  ! ---------------------------------------------------------- !
  ! NOTE: check for problems allocating array space
  ! ---------------------------------------------------------- ! calculate array grid point dimension
  n_vocnatm = 0
  DO i=1,n_i
     DO j=1,n_j
        n_vocnatm = n_vocnatm + 1
     end do
  end do
  ! ---------------------------------------------------------- ! allocate arrays: 2D
  ALLOCATE(vphys_ocnatm(1:n_vocnatm),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  do n=1,n_vocnatm
     allocate(vphys_ocnatm(n)%m(1:n_phys_ocnatm),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
  end do
  ! ---------------------------------------------------------- !
  !  INITIALIZE BIOGEM
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl2) print*, ' '
  IF (ctrl_debug_lvl2) print*, 'DEBUG LEVEL #2: initialize BioGeM'
  ! ---------------------------------------------------------- ! initialize arrays (1)
  IF (ctrl_debug_lvl2) print*, 'initialize arrays'
  CALL sub_init_force()
  CALL sub_init_diag()
  IF (ctrl_audit) CALL sub_init_audit()
  ! ---------------------------------------------------------- ! init ocean and ocean-atmosphere interfacing grids and physics
  IF (ctrl_debug_lvl2) print*, 'initialize ocean and ocean-atmosphere interfacing grids and physics'
  CALL sub_init_phys_ocn()
  CALL sub_init_phys_ocnatm()
  !>>> ALT VECTOR FORM
  CALL sub_data_init_phys_ocn()
  CALL sub_data_init_phys_ocnatm()
  !<<< ALT VECTOR FORM
  ! ---------------------------------------------------------- !
  ! load default values for ocean, atmosphere, and sediment tracers and and initialize tracer arrays and options
  IF (ctrl_debug_lvl2) print*, 'load default values for ocean, atmosphere, and sediment tracers'
  CALL sub_init_tracer_ocn_comp()
  CALL sub_init_tracer_forcing_atm()
  CALL sub_init_tracer_forcing_ocn()
  CALL sub_init_tracer_forcing_sed()
  call sub_biogem_copy_tstoocn(dum_ts)
  ! initialize biological scheme
  IF (ctrl_debug_lvl2) print*, 'initialize biological sub-system'
  call sub_init_bio()
  ! ?
  IF (trim(opt_misc_geoeng) /= 'NONE') THEN
     ! initialize geoengineering
     IF (ctrl_debug_lvl2) print*, 'initialize geoengineering'
     call sub_init_misc2D()
  end IF
  ! initialize geoghermal heat input
  if (ctrl_force_Fgeothermal2D) then
     call sub_init_force_Fgeothermal()
  end if
  ! calculate all the tracer relationship indices
  IF (ctrl_debug_lvl2) print*, 'calculate all the primary tracer relationship indices'
  call sub_calc_tracerrelationships_i()
  ! update relationships between tracers
  IF (ctrl_debug_lvl2) print*, 'update relationships between tracers'
  call sub_data_update_tracerrelationships
  ! data save meta config
  IF (ctrl_debug_lvl2) print*, 'set basic data save config'
  call sub_adj_par_save()
  ! set meta-options and verify self-consistency of chosen parameters
  IF (ctrl_debug_lvl2) print*, 'verify self-consistency of chosen parameters'
  call sub_check_par_biogem()
  ! initialize carbon cycle sub-systems
  IF (ctrl_debug_lvl2) print*, 'initialize carbon cycle sub-systems'
  CALL sub_init_carb()
  ! initialize gas solubility
  IF (ctrl_debug_lvl2) print*, 'initialize gas solubility'
  CALL sub_init_solconst()
  ! initialize geochem lookup tables
  IF (ctrl_debug_lvl2) print*, 'initialize geochem lookup tables'
  SELECT CASE (trim(opt_geochem_Fe))
  CASE ('lookup_4D')
     CALL sub_data_init_lookup_4D_Fe()
  end SELECT
  ! ---------------------------------------------------------- ! set up redox save/reporting
  CALL sub_init_redox()
  ! ---------------------------------------------------------- ! initialize arrays (2)
  IF (ctrl_debug_lvl2) print*, 'initialize data saving arrays'
  CALL sub_init_int_timeslice()
  CALL sub_init_int_timeseries()
  ! ---------------------------------------------------------- ! initialize the time scale save string
  IF (ctrl_debug_lvl2) print*, 'initialize the time scale save string'
  CALL sub_init_char()
  ! initialize basic data saving
  IF (ctrl_debug_lvl2) print*, 'initialize basic data saving'
  CALL sub_init_data_save()
  ! open units for runtime file I/O
  IF (ctrl_debug_lvl2) print*, 'open units for runtime file I/O'
  IF ((opt_append_data.eqv..FALSE.).OR.(ctrl_continuing.eqv..FALSE.)) THEN
     IF (ctrl_data_save_sig_ascii)  CALL sub_init_data_save_runtime()
  ENDIF
  ! initialize system forcings
  IF (ctrl_debug_lvl2) print*, 'initialize system forcings'
  CALL sub_init_force_restore_atm()
  CALL sub_init_force_flux_atm()
  CALL sub_init_force_restore_ocn()
  CALL sub_init_force_flux_ocn()
  !CALL sub_init_force_restore_sed()
  CALL sub_init_force_flux_sed()
  if (ctrl_force_solconst) then
     ! initialize time-dependent solar forcing
     IF (ctrl_debug_lvl2) print*, 'initialize time-dependent solar forcing'
     call sub_init_force_solconst()
  end if
  ! initialize orbital data saving
  IF (ctrl_debug_lvl2) print*, 'initialize orbital data saving'
  CALL sub_init_data_save_orb()

  ! *** INITILAIZE NETCDF  ***
  IF (ctrl_debug_lvl2) print*, 'initialize netCDF'
  ! set filenames
  string_ncout2d    = TRIM(par_outdir_name)//'fields_biogem_2d.nc'
  string_ncout3d    = TRIM(par_outdir_name)//'fields_biogem_3d.nc'
  string_nctsint    = TRIM(par_outdir_name)//'timeseries_biogem.nc'
  string_nctsi      = TRIM(par_outdir_name)//'ts_biogem_int.nc'
  string_nctsglob   = TRIM(par_outdir_name)//'ts_biogem_glob.nc'
  string_ncout3dsig = TRIM(par_outdir_name)//'fields_biogem_3dts.nc'
  ! initialise 2d and 3d netcdf files
  IF (ctrl_continuing.AND.opt_append_data) THEN
     OPEN(unit=in,status='old',file=TRIM(par_rstdir_name)//'netcdf_record_numbers',form='formatted',action='read')
     READ(unit=in,fmt='(i6)') ncout2d_ntrec,ncout3d_ntrec
     close(unit=in)
  ELSE
     call sub_init_netcdf(trim(string_ncout2d),loc_iou,2)
     ncout2d_iou = loc_iou
     ncout2d_ntrec = 0
     call sub_init_netcdf(trim(string_ncout3d),loc_iou,3)
     ncout3d_iou = loc_iou
     ncout3d_ntrec = 0
     if (ctrl_data_save_3d_sig) then
        call sub_init_netcdf(trim(string_ncout3dsig),loc_iou,4)
        ncout3dsig_iou = loc_iou
        ncout3dsig_ntrec = 0
     end if
  ENDIF

  ! JDW: initialise matrix netcdf output
  if(ctrl_data_diagnose_TM)THEN
    ! find number of wet grid-points
    n_wet_grid=0
    do n=1,n_vocn,1
      loc_k1 = vocn(n)%k1
        do k=n_k,loc_k1,-1
          n_wet_grid=n_wet_grid+1
        end do
    end do
    ! initialise matrix netcdf file
    call sub_init_netcdf_TM(n_wet_grid)
  end if

  ! *** load restart information ***
  IF (ctrl_continuing) then
     call sub_data_load_rst()
  end if

  ! *** initialize tracer auditing ***
  ! carry out initial tracer inventory audit
  IF (ctrl_audit) THEN
     IF (ctrl_debug_lvl2) print*, 'initialize tracer auditing'
     audit_ocn_init(:) = fun_calc_ocn_tot()
     audit_ocn_old(:)  = audit_ocn_init(:)
  end if

  ! *** write ocean tracer data to GOLDSTEIn arrays ***
  IF (ctrl_debug_lvl2) print*, 'initialize GOLDSTEIN biogeochem tracers'
  call sub_biogem_copy_ocntots(dum_ts,dum_ts1)
  if (ctrl_force_GOLDSTEInTS) call sub_biogem_copy_ocntotsTS(dum_ts,dum_ts1)

  ! ############################################################################################################################# !
!!! *** TESTING ***
  vocn(:)=fun_lib_conv_ocnTOvocn(ocn(:,:,:,:))
  ocn = 0.0
  ocn(:,:,:,:)=fun_lib_conv_vocnTOocn(vocn(:))
  ! ############################################################################################################################# !

  ! *** enable BioGeM ***
  par_misc_t_go = .TRUE.

  print*,' <<< Initialisation complete'
  print*,'======================================================='

  return

END SUBROUTINE initialise_biogem
! ******************************************************************************************************************************** !
