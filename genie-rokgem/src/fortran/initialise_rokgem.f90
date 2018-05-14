
! File: initialise_rokgem.f90
!
! Contains initialisation subroutine for RokGeM, as called in <initialise_genie.F> through <genie_ini_wrappers.f90>
!
! Subroutine: initialise_rokgem
!
! Initialisation subroutine for RokGeM, called in <initialise_genie.F> through <genie_ini_wrappers.f90>
!
! Uses:
!
!  - <rokgem_lib.f90>
!  - <rokgem_data.f90>
!  - <rokgem_box.f90>
!  - <rokgem_data_netCDF.f90>
!
! Calls:
!
! - <sub_load_goin_rokgem>
! - <sub_init_phys_rok>
! - <sub_init_netcdf_rg>
! - <sub_load_rokgem_restart>
! - <sub_init_netcdf_rg>
! - <sub_data_output_years>
! - <sub_ini_output>
! - <sub_load_data_ij>
! - <sub_land>
! - <sub_antarctica>
! - <define_river_array>
! - <sub_drainage>
! - <define_2D_arrays>
! - <sub_load_weath>
!
! Input/Output:
!
! dum_genie_timestep - number of seconds in a genie timestep
! dum_sfxrok - rocks-surface ocean tracer composition; rok grid
! dum_sfxsumrok1 - rocks-surface ocean fluxes; integrated, ocn grid

subroutine initialise_rokgem( &
     & dum_genie_timestep,    &
     & dum_sfxrok,              &
     & dum_sfxsumrok1        )
  
  use rokgem_lib
  use rokgem_data
  use rokgem_box
  USE rokgem_data_netCDF

! dummy arguments
  REAL,intent(inout)::dum_genie_timestep
  REAL,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxrok     ! rocks-surface tracer composition; rok grid
  REAL,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxsumrok1 ! rocks-surface fluxes; integrated, ocn grid

  ! local variables
  integer::loc_iou, i, j    
  integer::loc_len 

  print*,'======================================================='
  print*,' >>> Initialising rokgem weathering module ...'

  ! *** load goin information ***
  CALL sub_load_goin_rokgem()

  ! *** initialize rokgem ***
  CALL sub_init_phys_rok()

  ! *** setup for netcdf output  ***
  if (debug_init > 1) print*, 'initialize netCDF'
  string_ncout2d_rg  = TRIM(par_outdir_name)//'fields_rokgem_2d.nc' !note: this needs to be less than 100 characters
  if (debug_init > 1) print*, 'netcdf ouput file: ',TRIM(string_ncout2d_rg)
  ! initialise 2d netcdf files
  IF (ctrl_continuing.AND.opt_append_data) THEN
     call sub_load_rokgem_restart()
  ELSE   
     ncout2d_ntrec_rg = 0
     call sub_init_netcdf_rg(trim(string_ncout2d_rg),loc_iou)
  ENDIF
  if (debug_init > 1) print*, 'netcdf record number: ',ncout2d_ntrec_rg
  if (debug_init > 1) print*,'par_outdir_name = par_rstdir_name:',par_outdir_name.eq.par_rstdir_name
  !ncout2d_iou = loc_iou

 ! *** setup for netcdf output  ***
  if (debug_init > 1) print*, 'initialize netCDF'
  string_ncout2d_rg  = TRIM(par_outdir_name)//'fields_rokgem_2d.nc' !note: this needs to be less than 100 characters
  if (debug_init > 1) print*, 'netcdf ouput file: ',TRIM(string_ncout2d_rg)
  ! initialise 2d netcdf files
  IF (ctrl_continuing.AND.opt_append_data) THEN
     call sub_load_rokgem_restart()
  ELSE   
     ncout2d_ntrec_rg = 0
     call sub_init_netcdf_rg(trim(string_ncout2d_rg),loc_iou)
  ENDIF
  if (debug_init > 1) print*, 'netcdf record number: ',ncout2d_ntrec_rg
  if (debug_init > 1) print*,'par_outdir_name = par_rstdir_name:',par_outdir_name.eq.par_rstdir_name
  !ncout2d_iou = loc_iou

  ! *** initialize external interface arrays ***
  dum_sfxsumrok1(:,:,:) = 0.0
  dum_sfxrok(:,:,:)     = 0.0

  ! *** initialize timestep counter ***
  tstep_count = 0
  tsteps_per_year = conv_yr_s/(dum_genie_timestep*kocn_loop*conv_kocn_krokgem)
  if (debug_init > 1) PRINT*,'timesteps per year                                  :',tsteps_per_year
  ! *** load in years for output generation and initialise output ***
  CALL sub_data_output_years()
  year = min(output_years_0d(output_counter_0d),output_years_2d(output_counter_2d))
  CALL sub_ini_output()
  if (debug_init > 1) print*,'======================================================='

  ! -------------------------------------------------------- ! set legacy weathering options
    select case (opt_weather_CaSiO3)
    case ("Wallmann")
       opt_weather_CaSiO3="Wallmann"  
    case default
       IF (opt_weather_T_Ca) opt_weather_CaCO3="BLAG"
       if (opt_weather_C_Ca) opt_weather_CaCO3="WalkerKasting"
       IF (opt_weather_T_Si) opt_weather_CaSiO3="Brady_approx"
       IF (opt_weather_Talt_Si) opt_weather_CaSiO3="linear"
       IF (opt_weather_C_Si) opt_weather_CaSiO3="WalkerKasting"    
    end select

! ======= k_T constant ==================================================================!
        if (debug_init > 1) print*,'---  k_T constant ---'
        k_T=1000*par_E_a/(8.314472*((par_ref_T0+273.15)**2))
        if (debug_init > 1) print*,'k_T = ',k_T
        
! ======= RIVER ROUTING =================================================================!

        if (debug_init > 1) print*,'--- RIVER ROUTING ---'

  ! set alt dir path string length
  loc_len = LEN_TRIM(par_pindir_name)
! Read basic land run-off routing file into array runoff_drainage (k1 file)
       if (loc_len > 0) then
        CALL sub_load_data_ij(TRIM(par_pindir_name)//TRIM(topo),n_i+2,n_j+2,runoff_drainage)    
       else
        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(topo),n_i+2,n_j+2,runoff_drainage)
        endif

! Get landmask and number of landcells

        if (debug_init > 1) print*,'getting land mask from k1 file'
        CALL sub_land(runoff_drainage,landmask)
        if (debug_init > 1) print*,'number of land cells = ',nlandcells

! work out number of cells and rows in antarctica
          
          CALL sub_antarctica(landmask,ncells_antarctica,nrows_antarctica)

! Read detailed land run-off routing file (routing_new.dat) into array runoff_detail
          runoff_detail_i = 3*max_drain_cells
          runoff_detail_j = n_i*n_j
          CALL define_river_array()
          CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(routing)//'_'// &
                               & fun_conv_num_char_n(3,n_i)//'_'//fun_conv_num_char_n(3,n_j)//'.dat', &
                               & runoff_detail_i,runoff_detail_j,runoff_detail)

! Work out where on the coast to dump riverine solutes from each grid point 

          CALL sub_drainage(runoff_drainage,runoff_drainto,runoff_detail,runoff_coast)

! Note: sub_coastal_output is used to do the actual routing in the main loop

! ======= Calibration to data ==============================================================!

     
     if (debug_init > 1) print*,'--- CALIBRATION TO DATA ---'

     ! 0D calibration
     IF (opt_calibrate_T_0D) THEN
        calibrate_T_0D = ( par_data_T_0D + 273.15 ) / ( par_ref_T0 + 273.15 )
     ELSE
        calibrate_T_0D = 1.0
     ENDIF
     if (debug_init > 1) print*,'calibrate_T_0D = ',calibrate_T_0D
     
     IF (opt_calibrate_R_0D) THEN
        conv_GKWM_runoff = conv_GKWM_runoff * ( par_data_R_0D / par_ref_R0 )
        conv_GEM_CO2 = conv_GEM_CO2 * ( par_data_R_0D / par_ref_R0 )
        calibrate_R_0D = par_data_R_0D / par_ref_R0
     ELSE
        calibrate_R_0D=1.0
     ENDIF
     if (debug_init > 1) print*,'calibrate_R_0D = ',calibrate_R_0D

     if (debug_init > 1) print*,'conv_GKWM_runoff = ',conv_GKWM_runoff
     if (debug_init > 1) print*,'conv_GEM_CO2 = ',conv_GEM_CO2
     
     IF (opt_calibrate_P_0D) THEN
        calibrate_P_0D = par_data_P_0D / par_ref_P0
     ELSE
        calibrate_P_0D = 1.0
     ENDIF
     if (debug_init > 1) print*,'calibrate_P_0D = ',calibrate_P_0D

     ! 2D calibration
     IF (opt_calibrate_T_2D) THEN
        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_ref_T0_2D),n_i,n_j,ref_T0_2D)
        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_data_T_2D),n_i,n_j,data_T_2D)
        DO i=1,n_i
           DO j=1,n_j
              calibrate_T_2D(i,j) = ( data_T_2D(i,j) + 273.15 ) / ( ref_T0_2D(i,j) + 273.15 )
           END DO
        END DO
     ENDIF
     
if (debug_init > 2) then
                   print*, data_T_2D(:,1)
                   print*, ref_T0_2D(:,1)
                   print*, calibrate_T_2D(:,1)
end if
     
     IF (opt_calibrate_R_2D) THEN
        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_ref_R0_2D),n_i,n_j,ref_R0_2D)
        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_data_R_2D),n_i,n_j,data_R_2D)
        DO i=1,n_i
           DO j=1,n_j
              IF (ref_R0_2D(i,j).eq.0.0) THEN
                 calibrate_R_2D(i,j) = 1.0
              ELSE
                 calibrate_R_2D(i,j) = data_R_2D(i,j) / ref_R0_2D(i,j)
              ENDIF
           END DO
        END DO
     ENDIF
     
     IF (opt_calibrate_P_2D) THEN
        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_ref_P0_2D),n_i,n_j,ref_P0_2D)
        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_data_P_2D),n_i,n_j,data_P_2D)
        DO i=1,n_i
           DO j=1,n_j
              IF (ref_P0_2D(i,j).eq.0.0) THEN
                 calibrate_P_2D(i,j) = 1.0
              ELSE
                 calibrate_P_2D(i,j) = data_P_2D(i,j) / ref_P0_2D(i,j)
              ENDIF           
           END DO
        END DO
     ENDIF


! ======= 2D WEATHERING =================================================================!

  ! *** load in data for 2D weathering scheme if selected
  IF (par_weathopt.ne.'Global_avg') THEN
     if (debug_init > 1) print*,'--- 2D WEATHERING ---'
     gridcell_area = phys_rok(ipr_A,1,1)/1.0E6         ! factor of 1E6 to convert from m^2 to km^2
     if (debug_init > 1) print*,'gridcell area = ',gridcell_area
     conv_GKWM = 0.5*gridcell_area                     ! Formula is for Flux of bicarbonate produced, and we want riverine flux of Ca2+ 
                                                       ! (in line with global average formulation inputs), these are in ratio of 2:1 so factor of 0.5.
                                                       ! And fluxes are calculated per km^2 so multiply by gridcell_area
     conv_GKWM_runoff = 0.1 * conv_yr_s                ! Have separate constant for runoff as it is raised to a power in the formula;
                                                       ! runoff units are mm/s in EMBM but cm/yr in Gibbs' formula.         
                                                       ! normalise to annual average runoff used by Gibbs (41.8 cm/yr = 1.32E-05 mm/s)
                                                       !  - number is divided by annual average runoff during calculation in sub_GKWM

     conv_GEM_CO2 = 1.0E3 * conv_yr_s * gridcell_area  ! Runoff units are mm/s in EMBM but l*km^-2*s-1 in the GEM-CO2 formula. (factor 1E6).
                                                       ! Fluxes are calculated per km^2 so multiply by gridcell_area.
                                                       ! Formula is for Flux of CO2 consumed, and we want riverine flux of Ca2+ 
                                                       ! (in line with global average formulation inputs), these are in ratio of 1:1 so OK.
                                                       ! Factor of 10^-3 * conv_yr_s as formula calculates quantites in 10^-3 mol/s and we want mol/yr.

     SELECT case (par_weathopt)
         case ('GKWM')
         par_nliths = 6
         case ('GEM_CO2')
         par_nliths = 7
     end SELECT
     if (debug_init > 1) print*,'number of rock types (no. of files listed in x_lithologies.txt) = ',par_nliths
     CALL define_2D_arrays()
     CALL sub_load_weath(lithology_names,lithology)
          
     IF (opt_weath_regimes) THEN
        if (debug_init > 1) print*,'Erosion/transport limited weathering on; reading in orogeny landmask'
        ! Read basic landmask file with locations of different weathering regimes
        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(weath_regimes),n_i,n_j,orogeny)
     ENDIF
     
  ENDIF

  print*,' <<< Initialisation complete'
  print*,'======================================================='

end subroutine initialise_rokgem
! ******************************************************************************************************************************** !
