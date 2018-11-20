!
! File: rokgem_box.f90
! 
! Description: this module contains all the main subroutines for running rokgem
! Note: variables listed as Output in the subroutines are actually inout because they are defined in <rokgem.lib>
!
! Uses:
!
! - <rokgem_lib.f90>
! - <rokgem_data.f90>
! - <rokgem_data_netCDF>

MODULE rokgem_box

  USE rokgem_lib
  USE rokgem_data
  USE rokgem_data_netCDF
  IMPLICIT NONE
  SAVE


CONTAINS


  !========================================================================================!
  ! ========================== RIVER ROUTING ==============================================!
  !========================================================================================!

  ! ======= GET LAND MASK AND NUMBER OF LAND CELLS  =======================================!

  ! moved to rokgem_data.f90 for netcdf purpuses (so as not to create a loop of modules calling modules)

  ! ======= GET NUMBER OF CELLS IN ANTARCTIA ==============================================!

  ! Subroutine: sub_antarctica
  !
  ! Subroutine to work out the number of grid cells and rows taken up by Antarctica,
  ! for the purposes of subtracting Antarctica from output when river-routing (see <sub_drainage>) 
  ! scheme 3 is used as there is no detailed river-routing information for Antarctica.
  !
  ! Input:
  !
  ! dum_landmask(n_i,n_j) - this is an array with 1s for land and 0s for ocean
  !
  ! Output:
  !
  ! dum_ncells_antarctica - the number of grid cells taken up by Antarctica
  ! dum_nrows_antarctica - the number of grid rows taken up by Antarctica

  SUBROUTINE sub_antarctica(dum_landmask,dum_ncells_antarctica,dum_nrows_antarctica)

    INTEGER, INTENT(in)             :: dum_landmask(n_i,n_j)  
    INTEGER, INTENT(inout)          :: dum_ncells_antarctica
    INTEGER, INTENT(inout)          :: dum_nrows_antarctica             

    INTEGER                         :: i, j, n

    dum_ncells_antarctica = 0
    dum_nrows_antarctica = 0
    n = 0

    ! start at bottom and work way up until getting a clear lattitude (j) row without any land
    DO j=1,n_j
       dum_nrows_antarctica = j-2
       IF (n.EQ.36) EXIT
       n = 0
       DO i=1,n_i
          IF (dum_landmask(i,j).GT.0) THEN
             dum_ncells_antarctica = dum_ncells_antarctica + 1
          ELSE
             n = n + 1
          ENDIF
       END DO
    END DO

    if (debug_init > 2) PRINT*,'number of land cells in antarctica = ',dum_ncells_antarctica
    if (debug_init > 2) PRINT*,'number of lattitude rows taken up by antarctica = ',dum_nrows_antarctica

  END SUBROUTINE sub_antarctica

  !======= SUBROUTINE TO WORK OUT DRAINAGE ================================================!

  ! Subroutine: sub_drainage
  !
  ! Subroutine for working out the river routing from land cells to the ocean.
  ! Called during initialisation (see <intialise_rokgem.f90>)
  !
  ! Calls:
  !
  ! - <sub_save_data_ij>
  !
  ! Input:
  !
  ! dum_drainage - this is the *.k1 file
  !
  ! Output:
  !
  ! dum_dum_drainto_1 - array with coastal (lat,lon)s for routing fluxes (simple)
  ! dum_drainto_2 - detailed river routing file (lat,lon,amount)s - several for each cell
  ! dum_coast - detailed river routing file (lat,lon,amount)s - several for each cell

  SUBROUTINE sub_drainage(dum_drainage,dum_drainto_1,dum_drainto_2,dum_coast)

    IMPLICIT NONE
    INTEGER                         :: i, j, k, l, m, n, row, col, lon, lat
    REAL, INTENT(in)                :: dum_drainage(n_i+2,n_j+2)                        ! this is the *.k1 file
    INTEGER, INTENT(inout)          :: dum_drainto_1(n_i,n_j,2)         
    ! array with coastal (lat,lon)s for routing fluxes (simple)
    REAL, INTENT(inout)             :: dum_drainto_2(runoff_detail_i,runoff_detail_j)   
    ! detailed river routing file (lat,lon,amount)s - several for each cell 
    REAL, INTENT(inout)             :: dum_coast(n_i,n_j)

    ! *******
    ! ROUTING
    ! *******

    ! BASIC DRAINAGE (scheme 1) ---------------------------------------------------------------------
    ! Work out where on the coast to dump riverine solutes from each grid point 
    ! (create array 'runoff_drainto' of lon, lat (i,j) values for each grid cell) 
    ! using the fact that 91=East, 92=South, 93=West & 94=North in the *.k1 files.
    ! Note that the input array 'dum_drainage' is 38x38 for the 36x36 genie grid.

    IF ( routing_scheme.gt.0 ) THEN

       if (debug_init > 2) PRINT*,'Calculating costal drainage cells'
       DO i = 1, n_i
          DO j = 1, n_j
             l = i + 1
             m = j + 1
             DO n = 1,n_i*n_j
                IF ( dum_drainage(l,m).lt.91. ) THEN
                   IF (n.eq.1) THEN
                      l = 1
                      m = 1
                   ENDIF
                   EXIT
                ELSEIF ( dum_drainage(l,m).eq.91. ) THEN
                   IF (l.eq.n_i+1) THEN
                      l = 2
                   ELSE
                      l = l + 1
                   ENDIF
                   CYCLE
                ELSEIF ( dum_drainage(l,m).eq.92. ) THEN
                   m = m - 1
                   CYCLE
                ELSEIF ( dum_drainage(l,m).eq.93. ) THEN
                   IF (l.eq.2) THEN
                      l = n_i+1
                   ELSE
                      l = l - 1
                   ENDIF
                   CYCLE
                ELSEIF ( dum_drainage(l,m).eq.94. ) THEN
                   m = m + 1
                   CYCLE
                ENDIF
             END DO
             dum_drainto_1(i,j,1) = l - 1
             dum_drainto_1(i,j,2) = m - 1     
          END DO
       END DO

       ! Save output to files 'runoff_drainto_long.dat' and 'runoff_drainto_latt.dat' which give 
       ! the lattitude and longitude respectively of the costal grid point that run-off reaches from
       ! each inland grid point (ocean is denoted by 0s)
       if (debug_init > 2) PRINT*, &
            & 'saving lons. and lats. of costal cells to & runoff_drainto_long.dat and runoff_drainto_latt.dat files'
       ! NOTE: from gem_util
       CALL sub_save_data_ij( &
            & TRIM(par_outdir_name)//'runoff_drainto_long.dat',n_i,n_j,REAL(dum_drainto_1(:,:,1)) &
            & )
       ! NOTE: from gem_util
       CALL sub_save_data_ij( &
            & TRIM(par_outdir_name)//'runoff_drainto_latt.dat',n_i,n_j,REAL(dum_drainto_1(:,:,2)) &
            & )

    ENDIF

    ! INTERMEDIATE (level2) and DETAILED (level 3) ---------------------------------------------------
    ! use the array runoff_detail read in from file: contains n_i*n_j rows, each with a sucession of 
    ! (lat, long, fraction) data for each ocean cell corresponding to the land cell in question 
    ! (each row represents the landcell given by lat= floor(rownumber/n_j) lon=mod(rownumber,n_i)).
    IF ( routing_scheme.gt.1 ) THEN

       ! Adjust read in data so that 36 = highest northern latt. and 1 = lowest southern latt. rather than other way round
       DO i=1,n_i
          DO j=1,n_j
             row = (j-1)*n_j+i
             DO k=1,max_drain_cells
                col = 3*(k-1)
                lat = int(dum_drainto_2(col+1,row))
                IF (lat.GT.0.1) THEN
                   dum_drainto_2(col+1,row) = n_j + 1 - lat
                ENDIF
             END DO
          END DO
       END DO

       ! INTERMEDIATE (level 2)--------------------------------------------------------------------------
       ! With drainage that ends up on genie's land rather than in the ocean (about half!), use the basic 'roof' 
       ! routing provided by the k1 file to change the lats and lons contained in the detailed routing file appropriately.
       IF ( routing_scheme.eq.2 ) THEN
          DO i=1,n_i
             DO j=1,n_j
                row = (j-1)*n_j+i
                DO k=1,max_drain_cells
                   col = 3*(k-1)
                   lon = INT(dum_drainto_2(col+2,row))
                   lat = INT(dum_drainto_2(col+1,row))
                   IF (lat.GT.0.1) THEN
                      IF (landmask(lon,lat).EQ.1) THEN
                         dum_drainto_2(col+1,row) = dum_drainto_1(lon,lat,2)
                         dum_drainto_2(col+2,row) = dum_drainto_1(lon,lat,1)      
                      ENDIF
                   ENDIF
                END DO
             END DO
          END DO
       ENDIF

    ENDIF

    ! ****************
    ! COASTAL ENDPOINT
    ! ****************

    ! Produce map (runoff_coast) showing how many land grid cells' run-off is dumped into each
    ! costal grid cell

    if (debug_init > 2) PRINT*,'Producing map of costal drainage'

    ! initialise dum_coast, by setting all entries to 0.
    dum_coast(:,:) = 0.0

    ! BASIC (level 1)----------------------------------------------------------------------------------
    ! add 1 to each costal cell for every land cell that drains there
    IF ( routing_scheme.eq.1 ) THEN
       DO i = 1, n_i
          DO j = 1, n_j
             lon = dum_drainto_1(i,j,1)
             lat = dum_drainto_1(i,j,2)
             IF ((lon.NE.0).AND.(lat.NE.0)) THEN
                dum_coast(lon,lat) = dum_coast(lon,lat) + 1.0
             ENDIF
          END DO
       END DO
    ENDIF

    ! INTERMEDIATE (level2) and DETAILED (level 3)-----------------------------------------------------
    ! Ignore ocean cells denoted by 0 at start of line in detailed routing file; 
    ! also check that the cell is covered by the genie landmask (this is now done in Mathematica).
    ! Add up fractions and dump them into the coastal output array.
    IF ( routing_scheme.gt.1 ) THEN
       DO i=1,n_i
          DO j=1,n_j
             row = (j-1)*n_j+i
             DO k=1,max_drain_cells
                col = 3*(k-1)
                lon = INT(dum_drainto_2(col+2,row))
                lat = INT(dum_drainto_2(col+1,row))
                IF ((lon.NE.0).AND.(lat.NE.0)) THEN
                   dum_coast(lon,lat) = dum_coast(lon,lat) + dum_drainto_2(col+3,row)
                ENDIF
             END DO
          END DO
       END DO
       if (debug_init > 2) PRINT*,'sum(dum_coast(:,:)):',sum(dum_coast(:,:))
    ENDIF

    ! DETAILED (level 3)
    ! Take account of (approx. half) of flux that ends on land cells, by just taking ocean cells
    ! note: this means that some land cells won't be contributing much (if any) flux - including all of antarctica
    IF ( routing_scheme.eq.3 ) THEN
       dum_coast(:,:) = -1 * dum_coast(:,:) * (landmask(:,:)-1)
       if (debug_init > 2) PRINT*,'sum(dum_coast(:,:)):',sum(dum_coast(:,:))
    ENDIF

    ! INTERMEDIATE (level2) and DETAILED (level 3)-----------------------------------------------------
    ! scale the total up to the overall total (= no. of land cells)
    ! note: for scheme 3 subtract ncells_antarctica from nlandcells because of no Antarctica
    ! note: runoff_calib is overwritten in sub_coastal_output
    IF ( routing_scheme.gt.1 ) THEN
       runoff_calib = (nlandcells)/sum(dum_coast(:,:))
       IF ( routing_scheme.eq.3 ) THEN
          runoff_calib = (nlandcells-ncells_antarctica)/sum(dum_coast(:,:))
       ENDIF
       dum_coast(:,:) = dum_coast(:,:)*runoff_calib
       if (debug_init > 2) PRINT*,'calibration factor = ',runoff_calib
       if (debug_init > 2) PRINT*,'after calibration - sum(dum_coast(:,:)):',sum(dum_coast(:,:))
    ENDIF

    ! save output to 'runoff_coast_/routing_scheme/.dat'
    if (debug_init > 2) PRINT*,'saving map of coastal drainage to runoff_coast_'//fun_conv_num_char_n(1,routing_scheme)//'.dat'
    CALL sub_save_data_ij(TRIM(par_outdir_name)//'runoff_coast_'//      &
         &  fun_conv_num_char_n(1,routing_scheme)//'.dat',n_i,n_j,dum_coast(:,:))       ! from gem_util

  END SUBROUTINE sub_drainage


  ! ======= ROUTE WEATHERING FLUX TO COAST ================================================!

  ! Subroutine: sub_coastal_output
  !
  ! Subroutine for routing flux from the land surface to the coastal ocean.
  ! Called during time-stepping.
  !
  ! Input:
  !
  ! dum_input_array - the array with fluxes
  ! dum_drainto_1 - array with coastal (lat,lon)s for routing fluxes (simple)
  ! dum_dum_drainto_2 - detailed river routing array with (lat,lon,amount)s - several for each cell
  !
  ! Output:
  !
  ! dum_output_coast - array with fluxes in coastal ocean

  SUBROUTINE sub_coastal_output(dum_input_array,dum_drainto_1,dum_drainto_2,dum_output_coast)

    ! Dummy variables
    REAL, INTENT(in)                :: dum_input_array(n_i,n_j)                         !array with fluxes
    INTEGER, INTENT(in)             :: dum_drainto_1(n_i,n_j,2)          !array with coastal (lat,lon)s for routing fluxes (simple)
    REAL, INTENT(in)                :: dum_drainto_2(runoff_detail_i,runoff_detail_j)   !detailed river routing file (lat,lon,amount)s - several for each cell 
    REAL, INTENT(inout)             :: dum_output_coast(n_i,n_j)         !array with fluxes in coastal ocean

    ! Local counting variables
    INTEGER                         :: i, j, k, row, col, lon, lat
    REAL                            :: tot

    dum_output_coast(:,:) = 0.0
    tot = 0.0

    ! dump weathering flux to relevant point on coast

    ! BASIC (level 1)----------------------------------------------------------------------------------
    ! add 1 to each costal cell for every land cell that drains there
    IF ( routing_scheme.eq.1 ) THEN
       DO i = 1, n_i
          DO j = 1, n_j
             lon = dum_drainto_1(i,j,1)
             lat = dum_drainto_1(i,j,2)
             ! ignore ocean cells denoted by 0...
             IF ((lon.NE.0).AND.(lat.NE.0)) THEN
                dum_output_coast(lon,lat) = dum_output_coast(lon,lat) + dum_input_array(i,j)
             ENDIF
             ! ...unless the lithology map has land in the genie ocean - then dump weathering flux 
             ! straight into that ocean cell. Currently not used as land output is truncated to the genie landmask.
             ! if land output isn't truncated, get approx 1.5 times more flux.
             IF ((dum_input_array(i,j).NE.0).AND.(lon.EQ.0).AND.(lat.EQ.0)) THEN
                dum_output_coast(i,j) = dum_output_coast(i,j) + dum_input_array(i,j)  
             ENDIF
          END DO
       END DO
    ENDIF

    ! INTERMEDIATE (level2) and DETAILED (level 3)-----------------------------------------------------
    ! Ignore ocean cells denoted by 0 at start of line in detailed routing file; 
    ! also check that the cell is covered by the genie landmask (this is now done in Mathematica).
    ! Add up fractions (multiplied by the flux in the input array) and dump them into the coastal output array.
    IF ( routing_scheme.gt.1 ) THEN
       DO i=1,n_i
          DO j=1,n_j
             row = (j-1)*n_j+i
             DO k=1,max_drain_cells
                col = 3*(k-1)
                lon = INT(dum_drainto_2(col+2,row))
                lat = INT(dum_drainto_2(col+1,row))
                ! ignore ocean cells denoted by 0...
                IF ((lon.NE.0).AND.(lat.NE.0)) THEN
                   dum_output_coast(lon,lat) = dum_output_coast(lon,lat) + dum_input_array(n_i+1-i,j)*dum_drainto_2(col+3,row)
                ENDIF
                ! ...unless the lithology map has land in the genie ocean - then dump weathering flux 
                ! straight into that ocean cell. Currently not used as land output is truncated to the genie landmask.
                ! if land output isn't truncated, get approx 1.5 times more flux.
                IF ((k.EQ.1).AND.(dum_input_array(i,j).NE.0).AND.(landmask(i,j).EQ.0)) THEN
                   dum_output_coast(i,j) = dum_output_coast(i,j) + dum_input_array(i,j)
                ENDIF
             END DO
          END DO
       END DO
    ENDIF

    ! DETAILED (level 3)
    ! Take account of (approx. half) of flux that ends on land cells, by just taking ocean cells
    ! note: this means that some land cells won't be contributing much (if any) flux - including all of antarctica
    IF ( routing_scheme.eq.3 ) THEN
       dum_output_coast(:,:) = -1 * dum_output_coast(:,:) * (landmask(:,:)-1)
    ENDIF

    ! INTERMEDIATE (level2) and DETAILED (level 3)-----------------------------------------------------
    ! scale the ocean total up to the land total, taking into account there isn't a 1:1 mapping of land to ocean for these schemes
    ! note: for scheme 3 subtract ncells_antarctica from nlandcells because of no Antarctica
    IF ( routing_scheme.gt.1 ) THEN
       tot = sum(dum_output_coast(:,:))
       IF ( tot.lt.1.0E-19 ) THEN
          runoff_calib = 0.0
       ELSE
          IF ( routing_scheme.eq.2 ) THEN
             runoff_calib = sum(dum_input_array(:,:))/sum(dum_output_coast(:,:))
          ENDIF
          IF ( routing_scheme.eq.3 ) THEN
             runoff_calib = sum(dum_input_array(:,nrows_antarctica+1:n_j))/sum(dum_output_coast(:,:))
          ENDIF
       ENDIF
       dum_output_coast(:,:) = dum_output_coast(:,:)*runoff_calib
    ENDIF

  END SUBROUTINE sub_coastal_output


  !========================================================================================!
  !================================ WEATHERING  ===========================================!
  !========================================================================================!

  !======= GLOBAL AVERAGE WEATHERING (TAKEN FROM CBMS...BIOGEM) ===========================!

  ! Subroutine: sub_glob_avg_weath
  !
  ! Subroutine to calculate global average weathering.
  !
  ! Calls:
  !
  ! - <sub_init_phys_ocnrok>
  ! - <sub_coastal_output>
  ! - <sub_save_data_ij>
  !
  ! Input:
  !
  ! dum_sfcatm1 - atmosphere composition interface array (to get temperature and pCO2 from)
  ! dum_runoff(n_i,n_j) - run-off array (taken from EMBM)
  ! dum_photo(n_i,n_j) - photosynthesis array from land veg module (ENTS)
  ! dum_respveg(n_i,n_j) - vegetation respiration array from land veg module (ENTS)
  !
  ! Output:
  !
  ! dum_sfxrok - ocean flux interface array (same no of tracers as used in biogem ocean)
  ! dum_sfxatm1 - atmosphere flux interface array (same no of tracers as used in atchem atmosphere)

  SUBROUTINE sub_glob_avg_weath(dum_dts,dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)

    ! code originally from biogem.main.f90 (cbms_goldstein.v8.0.1) by AJR:

    ! dummy variables
    REAL,INTENT(in)::dum_dts        
    REAL,INTENT(in)                 :: dum_sfcatm1(n_atm,n_io,n_jo)                   ! atmosphere composition interface array
    REAL,INTENT(in)                 :: dum_runoff(n_i,n_j)                            ! run-off array (taken from EMBM)
    REAL,INTENT(in)                 :: dum_photo(n_i,n_j)                            ! photosythesis from land veg module (ENTS)
    REAL,INTENT(in)                 :: dum_respveg(n_i,n_j)            ! vegetation respiration from land veg module (ENTS)
    REAL,INTENT(inout)              :: dum_sfxrok(n_ocn,n_i,n_j)                                ! ocean flux interface array (same no of tracers as used in biogem ocean)
    REAL,INTENT(inout)              :: dum_sfxatm1(n_atm,n_io,n_jo)                             ! atmosphere flux interface array

    ! local variables
    INTEGER                         :: i, j, k
    REAL                            :: loc_avSLT
    REAL                            :: loc_SLT(n_i,n_j)
    REAL                            :: loc_SLT0
    REAL                            :: loc_maxSLT
    REAL                            :: loc_minSLT
    REAL                            :: loc_R
    REAL                            :: loc_runoff(n_i,n_j)
    REAL                            :: loc_R0
    REAL                            :: loc_maxR
    REAL                            :: loc_minR
    REAL                            :: loc_totR
    REAL                            :: loc_avP
    REAL                            :: loc_P(n_i,n_j)
    REAL                            :: loc_P0
    REAL                            :: loc_maxP
    REAL                            :: loc_minP
    REAL                            :: loc_CO2
    REAL                            :: loc_CO22(n_i,n_j)
    REAL                            :: loc_CO20
    REAL                            :: loc_maxCO2
    REAL                            :: loc_minCO2
    !REAL                            :: loc_A
    REAL                            :: loc_weather_ratio_CaSiO3
    REAL                            :: loc_weather_ratio_CaSiO3b
    REAL                            :: loc_weather_ratio_CaSiO3g
    REAL                            :: loc_weather_ratio_CaCO3
    REAL                            :: n, m

    REAL                            :: loc_force_flux_weather_a(n_atm)            ! total fluxes (atmosphere variables) 
    REAL                            :: loc_force_flux_weather_a_percell(n_ocn)                    ! flux per grid cell for even distribution (atmosphere variables)
    REAL                            :: loc_force_flux_weather_a_land(n_atm,n_i,n_j)                ! fluxes out of atmosphere (atmosphere variables)

    REAL                            :: loc_force_flux_weather_o(n_ocn)                    ! total fluxes (ocean variables)
    REAL                            :: loc_force_flux_weather_o_percell(n_ocn)                    ! flux per grid cell for even distribution (ocean variables)
    REAL                            :: loc_force_flux_weather_o_land(n_ocn,n_i,n_j)    ! fluxes shared over land (ocean variables)
    REAL                            :: loc_force_flux_weather_o_ocean(n_ocn,n_i,n_j)              ! fluxes into coastal positions in ocean (ocean variables) 

    real::loc_epsilon
    REAL                            :: loc_standard
    real::loc_d13C
    real::loc_87Sr,loc_88Sr
    REAL::loc_weather_o(n_ocn)

    CHARACTER(LEN=7),DIMENSION(n_ocn)       :: globtracer_names


    ! initialise tracer names       
    globtracer_names(io_ALK)                  = 'ALK    '
    globtracer_names(io_DIC)                  = 'DIC    '
    globtracer_names(io_Ca)                   = 'Ca     '
    globtracer_names(io_DIC_13C)              = 'DIC_13C'
    globtracer_names(io_DIC_14C)              = 'DIC_14C'

    ! initialise arrays
    loc_force_flux_weather_a(:)                 = 0.0                            
    loc_force_flux_weather_a_percell(:)         = 0.0              
    loc_force_flux_weather_a_land(:,:,:)        = 0.0 
    loc_force_flux_weather_o(:)                 = 0.0                            
    loc_force_flux_weather_o_percell(:)         = 0.0              
    loc_force_flux_weather_o_land(:,:,:)        = 0.0       
    loc_force_flux_weather_o_ocean(:,:,:)       = 0.0

    ! set reference surface land (air) temperature, runoff and productivity and CO2 level
    loc_SLT0 = par_ref_T0
    loc_R0 = par_ref_R0
    loc_P0 = par_ref_P0
    loc_CO20 = par_ref_CO20

    ! Initialise ocean array for temperature
    CALL sub_init_phys_ocnrok()                      !in rokgetime_series_namesm_data

    ! Put temp into local array
    ! extract temperature to local array this way to please intel compilers
    DO i=1,n_i
       DO j=1,n_j
          loc_SLT(i,j) = dum_sfcatm1(ia_T,i,j)
          ! do calibrations if requested
          IF (opt_calibrate_T_0D) THEN
             loc_SLT(i,j) = loc_SLT(i,j) + 273.15
             loc_SLT(i,j) = loc_SLT(i,j) * calibrate_T_0D
             loc_SLT(i,j) = loc_SLT(i,j) - 273.15
          ENDIF
          IF (opt_calibrate_T_2D) THEN
             loc_SLT(i,j) = loc_SLT(i,j) + 273.15
             loc_SLT(i,j) = loc_SLT(i,j) * calibrate_T_2D(i,j)
             loc_SLT(i,j) = loc_SLT(i,j) - 273.15
          ENDIF
       END DO
    END DO

    ! calculate current mean surface land (air) temperature SLT (degrees C)

    IF ((n_i.EQ.n_io).AND.(n_j.EQ.n_jo)) THEN

       ! for equal area grid:
       loc_avSLT = 0.0
       loc_maxSLT = -100.0
       loc_minSLT = 100.0
       DO i=1,n_i
          DO j=1,n_j
             m = landmask(i,j) * loc_SLT(i,j)
             loc_avSLT = loc_avSLT + m
             IF ((m.GT.loc_maxSLT).AND.(landmask(i,j).EQ.1)) THEN
                loc_maxSLT = m
             ENDIF
             IF ((m.LT.loc_minSLT).AND.(landmask(i,j).EQ.1)) THEN
                loc_minSLT = m
             ENDIF
          END DO
       END DO
       loc_avSLT = loc_avSLT/nlandcells
       !code below is alternative to that above, but causes compilation issues!
       !loc_avSLT = sum(landmask(:,:) * RESHAPE(dum_sfcatm1(ia_T,:,:),SHAPE=(/ n_i,n_j /)))/nlandcells

    ELSE
       ! from genie (accomodates non-equal-area grids):
       !       goldstein_k1(:,:) = go_k1(:,:)
       !       loc_A = 0.0
       !       DO i=1,n_io
       !          DO j=1,n_jo
       !             IF (n_ko < goldstein_k1(i,j)) THEN
       !                loc_avSLT = loc_avSLT + phys_ocnrok(ipoa_A,i,j)*loc_SLT(i,j)
       !                loc_A = loc_A + phys_ocnrok(ipoa_A,i,j)
       !             end IF
       !          end DO
       !       end DO
       !       loc_avSLT = loc_avSLT/loc_A

    ENDIF

    ! Put runoff into local array
    loc_runoff(:,:) = dum_runoff(:,:)

    ! Do calibrations if requested
    IF (opt_calibrate_R_0D) THEN
       DO i=1,n_i
          DO j=1,n_j
             loc_runoff(i,j) = loc_runoff(i,j) * calibrate_R_0D
          END DO
       END DO
    ENDIF
    IF (opt_calibrate_R_2D) THEN
       loc_runoff(:,:) = loc_runoff(:,:) * calibrate_R_2D(:,:)
    ENDIF

    ! calculate mean runoff (mm s-1)
    ! for equal area grid:
    loc_totR = 0.0
    loc_maxR = 0.0
    loc_minR = 0.0
    DO i=1,n_i
       DO j=1,n_j
          m = landmask(i,j) * dum_runoff(i,j)
          loc_totR = loc_totR + m
          IF ((m.GT.loc_maxR).AND.(landmask(i,j).EQ.1)) THEN
             loc_maxR = m
          ENDIF
          IF ((m.LT.loc_minR).AND.(landmask(i,j).EQ.1)) THEN
             loc_minR = m
          ENDIF
       END DO
    END DO
    loc_R = loc_totR/nlandcells

    ! convert atm pCO2 to ppm
    DO i=1,n_i
       DO j=1,n_j
          loc_CO22(i,j) = 1.0E+06*dum_sfcatm1(ia_PCO2,i,j)
       END DO
    END DO
    ! calculate mean co2 (ppm)
    ! for equal area grid:
    loc_CO2 = 0.0
    loc_maxCO2 = 0.0
    loc_minCO2 = 0.0
    DO i=1,n_i
       DO j=1,n_j
          m = landmask(i,j) * loc_CO22(i,j)
          loc_CO2 = loc_CO2 + m
          IF ((m.GT.loc_maxCO2).AND.(landmask(i,j).EQ.1)) THEN
             loc_maxCO2 = m
          ENDIF
          IF ((m.LT.loc_minCO2).AND.(landmask(i,j).EQ.1)) THEN
             loc_minCO2 = m
          ENDIF
       END DO
    END DO
    loc_CO2 = loc_CO2/nlandcells


    ! calculate mean surface productivity (kgC m-2 yr-1)

    SELECT case (par_prodopt)
       ! Global Primary Productivity
    case ('GPP')
       loc_P(:,:) = dum_photo(:,:)
       ! Net Primary Productivity
    case ('NPP')
       loc_P(:,:) = dum_photo(:,:) - dum_respveg(:,:)
    end SELECT

    ! Do calibrations if requested
    IF (opt_calibrate_P_0D) THEN
       DO i=1,n_i
          DO j=1,n_j
             loc_P(i,j) = loc_P(i,j) * calibrate_P_0D
          END DO
       END DO
    ENDIF
    IF (opt_calibrate_P_2D) THEN
       loc_P(:,:) = loc_P(:,:) * calibrate_P_2D(:,:)
    ENDIF

    ! for equal area grid:
    loc_avP = 0.0
    loc_maxP = 0.0
    loc_minP = 0.0
    DO i=1,n_i
       DO j=1,n_j
          m = landmask(i,j) * loc_P(i,j)
          loc_avP = loc_avP + m
          IF ((m.GT.loc_maxP).AND.(landmask(i,j).EQ.1)) THEN
             loc_maxP = m
          ENDIF
          IF ((m.LT.loc_minP).AND.(landmask(i,j).EQ.1)) THEN
             loc_minP = m
          ENDIF
       END DO
    END DO
    loc_avP = loc_avP/nlandcells

    ! -------------------------------------------------------- !
    ! GLOBAL MEAN CACO3 WEATHERING CALCULATIONS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! (1) base weathering rate modifier
    ! NOTE: make sure that weathering rates stay positive
    select case (opt_weather_CaCO3)
    case ("BLAG")
       n = 1.0 + par_k_Ca*(loc_avSLT - loc_SLT0) ! From BLAG (1983)
       IF (n < 0.0) n = 0.0
    case ("WalkerKasting")
       n = (loc_CO2/loc_CO20)**par_nCa ! From Walker and Kasting (1992)
       IF (n < 0.0) n = 0.0
    case default
       n = 1.0
    end select
    loc_weather_ratio_CaCO3 = n
    ! -------------------------------------------------------- ! (2) additional weathering rate modifiers
    ! NOTE: make sure that weathering modifiers stay positive
    ! NOTE: these modifiers are cumulative (in a product sense)
    IF (opt_weather_R_Ca) THEN
       IF (opt_weather_R_explicit) THEN
          n = (loc_R/loc_R0)
       ELSE
          n = (1.0 + par_k_run*(loc_avSLT - loc_SLT0)) ! From GEOCARB
       ENDIF
       IF (n < 0.0) n = 0.0
       loc_weather_ratio_CaCO3 = n*loc_weather_ratio_CaCO3
    ENDIF
    IF (opt_weather_P_Ca) THEN
       IF (opt_weather_P_explicit) THEN
          n = (loc_avP/loc_P0) ! From Lenton & Britton (2006)
       ELSE
          n = (2*(loc_CO2/loc_CO20)/(1+(loc_CO2/loc_CO20)))**0.4 ! From GEOCARB
       ENDIF
       IF (n < 0.0) n = 0.0
       loc_weather_ratio_CaCO3 = n*loc_weather_ratio_CaCO3
    ENDIF
    ! -------------------------------------------------------- ! cap maximum weathering rate modifier
    IF (loc_weather_ratio_CaCO3 > par_n_max_CaCO3) loc_weather_ratio_CaCO3 = par_n_max_CaCO3
    ! -------------------------------------------------------- ! enhance weathering for highly weatherable reservoir
    ! partially consistent with Mills et al. [2011] (and see thesis)
    ! the difference being that enhanced weathering is applied as a scalar to existing modifier rather than an absolute limit
    if (par_weather_fCaCO3_enh_nt > const_real_nullsmall) then
       ! SIMPLER: the enhancement of weathering is uniform until the 'reservoir' of easily weatherable material is depleted
       loc_weather_ratio_CaCO3 = par_weather_fCaCO3_enh_n*loc_weather_ratio_CaCO3
       par_weather_fCaCO3_enh_nt = par_weather_fCaCO3_enh_nt - conv_s_yr*dum_dts*par_weather_fCaCO3_enh_n
    end if
    ! -------------------------------------------------------- ! set actual weathering flux
    weather_fCaCO3  = loc_weather_ratio_CaCO3*par_weather_CaCO3
    ! -------------------------------------------------------- !

    ! -------------------------------------------------------- !
    ! GLOBAL MEAN CASIO3 WEATHERING CALCULATIONS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! (1) base weathering rate modifier
    ! NOTE: make sure that weathering rates stay positive
    select case (opt_weather_CaSiO3)
    case ("Brady_approx")
       ! NB: k_T=1000*par_E_a/(8.314472*((par_ref_T0+273.15)**2))
       n = exp(k_T*(loc_avSLT - loc_SLT0)) ! From Colbourn [2011] following Brady (1991)
       IF (n < 0.0) n = 0.0
    case ("Brady_FULL")
       n = exp(-(1000.0*par_E_a/const_R)*(1.0/(const_zeroC+loc_avSLT) - 1.0/(const_zeroC+loc_SLT0))) ! From Brady (1991)
       IF (n < 0.0) n = 0.0
    case ("WalkerKasting")
       n = (loc_CO2/loc_CO20)**par_nSi ! From Walker and Kasting (1992)
       IF (n < 0.0) n = 0.0
    case ("linear")
       n = 1.0 + par_k_Si*(loc_avSLT - loc_SLT0) ! linear
       IF (n < 0.0) n = 0.0
    case ("Wallmann")
       n = exp(par_k_Tb*(loc_avSLT - loc_SLT0))
       IF (n < 0.0) n = 0.0
       loc_weather_ratio_CaSiO3b = n  
       n = exp(par_k_Tg*(loc_avSLT - loc_SLT0))
       IF (n < 0.0) n = 0.0
       loc_weather_ratio_CaSiO3g = n 
       n = 0.0       
    case default
       n = 1.0
    end select
    loc_weather_ratio_CaSiO3 = n    
    ! -------------------------------------------------------- ! (2) additional weathering rate modifiers
    ! NOTE: make sure that weathering modifiers stay positive
    ! NOTE: these modifiers are cumulative (in a product sense)
    IF (opt_weather_R_Si) THEN
       IF (opt_weather_R_explicit) THEN
          n = (loc_R/loc_R0)**par_beta
       ELSE
          n = (1.0 + par_k_run*(loc_avSLT - loc_SLT0)) ! From GEOCARB
          IF (n < 0.0) n = 0.0
          n = n**par_beta ! From GEOCARB
       ENDIF
       IF (n < 0.0) n = 0.0
       IF (n > par_n_max_CaSiO3) n = par_n_max_CaSiO3
       loc_weather_ratio_CaSiO3 = n*loc_weather_ratio_CaSiO3
    ENDIF
    IF (opt_weather_P_Si) THEN
       IF (opt_weather_P_explicit) THEN
          n = (loc_avP/loc_P0)  ! From Lenton & Britton (2006)
       ELSE
          n = (2*(loc_CO2/loc_CO20)/(1+(loc_CO2/loc_CO20)))**0.4 ! From GEOCARB
       ENDIF
       IF (n < 0.0) n = 0.0
       IF (n > par_n_max_CaSiO3) n = par_n_max_CaSiO3
       loc_weather_ratio_CaSiO3 = n*loc_weather_ratio_CaSiO3
    ENDIF
    ! -------------------------------------------------------- ! cap maximum weathering rate modifier
    IF (loc_weather_ratio_CaSiO3 > par_n_max_CaSiO3) loc_weather_ratio_CaSiO3 = par_n_max_CaSiO3
    ! enhance weathering if highly weatherable reservoir available
    ! -------------------------------------------------------- ! enhance weathering for highly weatherable reservoir
    ! partially consistent with Mills et al. [2011] (and see thesis)
    ! the difference being that enhanced weathering is applied as a scalar to existing modifier rather than an absolute limit
    if (par_weather_fCaSiO3_enh_nt > const_real_nullsmall) then
       ! SIMPLER: the enhancement of weathering is uniform until the 'reservoir' of easily weatherable material is depleted
       loc_weather_ratio_CaSiO3 = par_weather_fCaSiO3_enh_n*loc_weather_ratio_CaSiO3
       par_weather_fCaSiO3_enh_nt = par_weather_fCaSiO3_enh_nt - conv_s_yr*dum_dts*loc_weather_ratio_CaSiO3
    end if
    ! -------------------------------------------------------- ! set actual weathering flux
    select case (opt_weather_CaSiO3)
    case ("Wallmann")
       weather_fCaSiO3b = loc_weather_ratio_CaSiO3b*par_weather_CaSiO3b
       weather_fCaSiO3g = loc_weather_ratio_CaSiO3g*par_weather_CaSiO3g 
       weather_fCaSiO3  = weather_fCaSiO3b + weather_fCaSiO3g
    case default
       weather_fCaSiO3  = loc_weather_ratio_CaSiO3*par_weather_CaSiO3
       weather_fCaSiO3b = weather_fCaSiO3/2.0
       weather_fCaSiO3g = weather_fCaSiO3/2.0
       par_weather_CaSiO3b_fracMg = par_weather_CaSiO3_fracMg
       par_weather_CaSiO3g_fracMg = par_weather_CaSiO3_fracMg
    end select
    ! -------------------------------------------------------- !

    ! -------------------------------------------------------- !
    ! CALCULATE DERIVED ELEMETAL (AND ISOTOPIC) FLUXES
    ! -------------------------------------------------------- !
    ! cations and alkalinity
    loc_force_flux_weather_o(io_Ca) = &
         & (1.0 - par_weather_CaSiO3b_fracMg)*weather_fCaSiO3b + (1.0 - par_weather_CaSiO3g_fracMg)*weather_fCaSiO3g + &
         & weather_fCaCO3
    loc_force_flux_weather_o(io_Mg) = &
         & par_weather_CaSiO3b_fracMg*weather_fCaSiO3b + par_weather_CaSiO3g_fracMg*weather_fCaSiO3g
    loc_force_flux_weather_o(io_ALK) = 2.0*weather_fCaSiO3 + 2.0*weather_fCaCO3

    ! bulk carbon
    IF (opt_short_circuit_atm.eqv..true.) THEN
       IF (opt_outgas_eq_Si.eqv..true.) THEN
          loc_force_flux_weather_o(io_DIC) = weather_fCaSiO3 + weather_fCaCO3
       ELSE
          loc_force_flux_weather_o(io_DIC) = par_outgas_CO2 + weather_fCaCO3
       ENDIF
    ELSE
       IF (opt_outgas_eq_Si.eqv..true.) THEN
          ! NOTE: '-' because coming out of atmosphere
          ! NOTE: not 2.0*weather_fCaSiO3 becasue outgassing is being assumed to balance net silicate weathering
          loc_force_flux_weather_a(ia_PCO2) = -(weather_fCaSiO3 + weather_fCaCO3)
       ELSE 
          ! NOTE: '-' because coming out of atmosphere
          ! NOTE: straight-forward -- outgassing minus CO2 consumed in weathering
          loc_force_flux_weather_a(ia_PCO2) = par_outgas_CO2 -(2.0*weather_fCaSiO3 + weather_fCaCO3)
       ENDIF
       loc_force_flux_weather_o(io_DIC) = 2.0*weather_fCaSiO3 + 2.0*weather_fCaCO3
    ENDIF

    ! d13C
    ! NOTE: does not matter how the standard is derived -- it is al the same standard! (13C)
    loc_standard = const_standards(atm_type(ia_pCO2_13C))
    loc_d13C = fun_calc_isotope_delta( &
         & dum_sfcatm1(ia_pCO2,n_io,n_jo),dum_sfcatm1(ia_pCO2_13C,n_io,n_jo),loc_standard,.FALSE.,const_nulliso &
         & )
    IF (opt_short_circuit_atm.eqv..true.) THEN
       IF (opt_outgas_eq_Si.eqv..true.) THEN
          loc_force_flux_weather_o(io_DIC_13C) =  &
               & fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*weather_fCaSiO3 + &
               & fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3
       ELSE
          loc_force_flux_weather_o(io_DIC_13C) =  &
               & fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*par_outgas_CO2 + &
               & fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3       
       ENDIF
    ELSE
       IF (opt_outgas_eq_Si.eqv..true.) THEN
          ! NOTE: '-' because coming out of atmosphere
          loc_force_flux_weather_a(ia_pCO2_13C) = &
               & -( &
               &   fun_calc_isotope_fraction(loc_d13C,loc_standard)*weather_fCaSiO3 + &
               &   fun_calc_isotope_fraction(loc_d13C,loc_standard)*weather_fCaCO3 &
               & ) 
       ELSE
          ! NOTE: '-' because coming out of atmosphere
          loc_force_flux_weather_a(ia_pCO2_13C) = &
               & fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*par_outgas_CO2 - & 
               & ( &
               & 2.0*fun_calc_isotope_fraction(loc_d13C,loc_standard)*weather_fCaSiO3 + &
               & fun_calc_isotope_fraction(loc_d13C,loc_standard)*weather_fCaCO3 &
               & ) 
       ENDIF
       loc_standard = const_standards(ocn_type(io_DIC_13C))
       loc_force_flux_weather_o(io_DIC_13C) = &
            & 2.0*fun_calc_isotope_fraction(loc_d13C,loc_standard)*weather_fCaSiO3 + &
            & fun_calc_isotope_fraction(loc_d13C,loc_standard)*weather_fCaCO3 + &
            & fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3
    ENDIF
    ! d14C
    ! NOTE assume completely radio carbon dead ...
    loc_force_flux_weather_o(io_DIC_14C) = 0.0

    ! ######################################################################################################################### !
    ! LITHIUM CODE
    ! bulk silicate Li weathering flux
    loc_force_flux_weather_o(io_Li) = loc_force_flux_weather_o(io_Li) + &
         & par_weather_CaSiO3_fracLi*weather_fCaSiO3
!!$    ! adjust dissolved load for clay formation
!!$    loc_R_flux = 1.0/(1.0 + exp(par_weather_Li_Rscale*(1.0 - loc_weather_ratio_CaSiO3 + par_weather_Li_Roffset/par_weather_Li_Rscale)))
!!$    loc_force_flux_weather_o(io_Li) = loc_R_flux*loc_force_flux_weather_o(io_Li)
!!$    ! calculate clay fractionation
!!$    IF (loc_weather_ratio_CaSiO3 > const_real_nullsmall) then
!!$       loc_epsilon = par_weather_CaSiO3_Li_d7Li + 1000.0*(loc_R_flux**((par_weather_Li_7Li_epsilon/1000.0 + 1.0) - 1.0) - 1.0)
!!$    else
!!$       loc_epsilon = par_weather_CaSiO3_Li_d7Li
!!$    end IF
    ! calculate net Li isotopic weathering signature
    loc_standard = const_standards(ocn_type(io_Li_7Li))
    loc_epsilon = par_weather_CaSiO3_Li_d7Li
    loc_force_flux_weather_o(io_Li_7Li) = loc_force_flux_weather_o(io_Li_7Li) + &
         & fun_calc_isotope_fraction(loc_epsilon,loc_standard)*loc_force_flux_weather_o(io_Li)
    ! bulk carbonate flux
    ! 
    ! *** DISCOUNT LI CONTENT OF CARBONATES ***
    ! 
    ! ######################################################################################################################### !

    ! ######################################################################################################################### !
    ! Ca ISOTOPES
    loc_standard = const_standards(ocn_type(io_Ca_44Ca))
    ! silicate 44Ca weathering flux
    loc_epsilon = par_weather_CaSiO3_d44Ca
    loc_force_flux_weather_o(io_Ca_44Ca) = loc_force_flux_weather_o(io_Ca_44Ca) + &
         & fun_calc_isotope_fraction(loc_epsilon,loc_standard)*(1.0 - par_weather_CaSiO3_fracMg)*weather_fCaSiO3
    ! carbonate 44Ca weathering flux
    loc_epsilon = par_weather_CaCO3_d44Ca
    loc_force_flux_weather_o(io_Ca_44Ca) = loc_force_flux_weather_o(io_Ca_44Ca) + &
         & fun_calc_isotope_fraction(loc_epsilon,loc_standard)*weather_fCaCO3
    ! ######################################################################################################################### !

    ! ######################################################################################################################### !
    ! STRONTIUM CODE
    ! (1) carbonate Sr weathering flux
    ! initialization -- populate array with bulk flux and isotopic delta values
    loc_weather_o(:) = 0.0
    loc_weather_o(io_Sr) = par_weather_CaCO3_fracSr*weather_fCaCO3
    loc_weather_o(io_Sr_87Sr) = 1000.0*(par_weather_CaCO3_r87Sr/const_standardsR(ocn_type(io_Sr_87Sr)) - 1.0)
    loc_weather_o(io_Sr_88Sr) = par_weather_CaCO3_d88Sr
    ! calculate Sr ISOTOPES -- 87Sr
    ! NOTE: do not update <loc_weather_o> yet as it is needed for the d88Sr calculation ...
    loc_87Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_weather_o(:),1)
    ! calculate Sr ISOTOPES -- 88Sr
    loc_88Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_weather_o(:),2)
    ! update flux array
    loc_weather_o(io_Sr_87Sr) = loc_87Sr
    loc_weather_o(io_Sr_88Sr) = loc_88Sr
    loc_force_flux_weather_o(:) = loc_force_flux_weather_o(:) + loc_weather_o(:)
    ! (2) silicate Sr weathering flux
    select case (opt_weather_CaSiO3)
    case ("Wallmann")
    ! (2a) basalt & granite
    ! initialization
    ! NOTE: populate array with bulk flux and isotopic delta values
    loc_weather_o(:) = 0.0
    loc_weather_o(io_Sr) = par_weather_CaSiO3b_fracSr*weather_fCaSiO3b
    loc_weather_o(io_Sr_87Sr) = 1000.0*(par_weather_CaSiO3b_r87Sr/const_standardsR(ocn_type(io_Sr_87Sr)) - 1.0)
    loc_weather_o(io_Sr_88Sr) = par_weather_CaSiO3b_d88Sr
    ! calculate Sr ISOTOPES -- 87Sr
    ! NOTE: do not update <loc_weather_o> yet as it is needed for the d88Sr calculation ...
    loc_87Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_weather_o(:),1)
    ! calculate Sr ISOTOPES -- 88Sr
    loc_88Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_weather_o(:),2)
    ! update flux array
    loc_weather_o(io_Sr_87Sr) = loc_87Sr
    loc_weather_o(io_Sr_88Sr) = loc_88Sr
    loc_force_flux_weather_o(:) = loc_force_flux_weather_o(:) + loc_weather_o(:)
    ! initialization
    ! NOTE: populate array with bulk flux and isotopic delta values
    loc_weather_o(:) = 0.0
    loc_weather_o(io_Sr) = par_weather_CaSiO3g_fracSr*weather_fCaSiO3g
    loc_weather_o(io_Sr_87Sr) = 1000.0*(par_weather_CaSiO3g_r87Sr/const_standardsR(ocn_type(io_Sr_87Sr)) - 1.0)
    loc_weather_o(io_Sr_88Sr) = par_weather_CaSiO3g_d88Sr
    ! calculate Sr ISOTOPES -- 87Sr
    ! NOTE: do not update <loc_weather_o> yet as it is needed for the d88Sr calculation ...
    loc_87Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_weather_o(:),1)
    ! calculate Sr ISOTOPES -- 88Sr
    loc_88Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_weather_o(:),2)
    ! update flux array
    loc_weather_o(io_Sr_87Sr) = loc_87Sr
    loc_weather_o(io_Sr_88Sr) = loc_88Sr
    loc_force_flux_weather_o(:) = loc_force_flux_weather_o(:) + loc_weather_o(:)  
    case default
    ! (2b) GLOBAL MEAN
    ! initialization
    ! NOTE: populate array with bulk flux and isotopic delta values
    loc_weather_o(:) = 0.0
    loc_weather_o(io_Sr) = par_weather_CaSiO3_fracSr*weather_fCaSiO3
    loc_weather_o(io_Sr_87Sr) = 1000.0*(par_weather_CaSiO3_r87Sr/const_standardsR(ocn_type(io_Sr_87Sr)) - 1.0)
    loc_weather_o(io_Sr_88Sr) = par_weather_CaSiO3_d88Sr
    ! calculate Sr ISOTOPES -- 87Sr
    ! NOTE: do not update <loc_weather_o> yet as it is needed for the d88Sr calculation ...
    loc_87Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_weather_o(:),1)
    ! calculate Sr ISOTOPES -- 88Sr
    loc_88Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_weather_o(:),2)
    ! update flux array
    loc_weather_o(io_Sr_87Sr) = loc_87Sr
    loc_weather_o(io_Sr_88Sr) = loc_88Sr
    loc_force_flux_weather_o(:) = loc_force_flux_weather_o(:) + loc_weather_o(:)
    end select
    ! ######################################################################################################################### !

    ! ######################################################################################################################### !
    ! ORGANIC MATTER / NUTRIENTS
    ! bulk silicate C (kerogen) flux (and isotopic signature)
    loc_force_flux_weather_o(io_DIC) = loc_force_flux_weather_o(io_DIC) + &
         & par_weather_CaSiO3_fracC*weather_fCaSiO3
    loc_standard = const_standards(ocn_type(io_DIC_13C))
    loc_force_flux_weather_o(io_DIC_13C) = loc_force_flux_weather_o(io_DIC_13C) + &
         & fun_calc_isotope_fraction(par_weather_CaSiO3_fracC_d13C,loc_standard)*par_weather_CaSiO3_fracC*weather_fCaSiO3
    ! bulk silicate P flux
    loc_force_flux_weather_o(io_PO4) = loc_force_flux_weather_o(io_PO4) + &
         & par_weather_CaSiO3_fracP*weather_fCaSiO3
    ! O2 consumption associated with Corg weathering (and conversion of C -> dissolved CO2)
    IF (opt_short_circuit_atm) THEN
       loc_force_flux_weather_o(io_O2) = loc_force_flux_weather_o(io_O2) - 1.0*par_weather_CaSiO3_fracC*weather_fCaSiO3
    ELSE
       ! NOTE: '-' because coming out of atmosphere
       loc_force_flux_weather_a(ia_pO2) = loc_force_flux_weather_a(ia_pO2) - 1.0*par_weather_CaSiO3_fracC*weather_fCaSiO3
    ENDIF
    ! ######################################################################################################################### !

    ! ######################################################################################################################### !
    ! PYRITE
    ! bulk silicate S and Fe fluxes (and isotopic signature) from pyrite (FeS2)
    ! NOTE: pyrite weathering should to balance stchiometry of pyrite formation
    !       2FeS2  + 7O2 +2H2O —–> 2Fe2+   H2SO4+ 2H+
    !       pyrite + oxygen + water —–> iron ions + sulphuric acid + hydrogen ions
    ! currently, in biogem_box: 4Fe + 7H2S + SO4 -> 4FeS2 + 6H
    ! NOTE: set Fe flux as Fe2 (not TDFe  for now / here)
    ! NOTE: remember he alkalinity associated with adding SO42- to the ocean ...
    ! S
    loc_force_flux_weather_o(io_H2S) = loc_force_flux_weather_o(io_H2S) + &
         & (7.0*par_weather_CaSiO3_fracFeS2*weather_fCaSiO3)/4.0
    loc_standard = const_standards(ocn_type(io_H2S_34S))
    loc_force_flux_weather_o(io_H2S_34S) = loc_force_flux_weather_o(io_H2S_34S) + &
         & fun_calc_isotope_fraction(par_weather_CaSiO3_fracFeS2_d34S,loc_standard)* &
         & (7.0*par_weather_CaSiO3_fracFeS2*weather_fCaSiO3)/4.0
    loc_force_flux_weather_o(io_SO4) = loc_force_flux_weather_o(io_SO4) + &
         & (1.0*par_weather_CaSiO3_fracFeS2*weather_fCaSiO3)/4.0
    loc_standard = const_standards(ocn_type(io_SO4_34S))
    loc_force_flux_weather_o(io_SO4_34S) = loc_force_flux_weather_o(io_SO4_34S) + &
         & fun_calc_isotope_fraction(par_weather_CaSiO3_fracFeS2_d34S,loc_standard)* &
         & (1.0*par_weather_CaSiO3_fracFeS2*weather_fCaSiO3)/4.0
    loc_force_flux_weather_o(io_ALK) = loc_force_flux_weather_o(io_ALK) + &
         & -2.0*(1.0*par_weather_CaSiO3_fracFeS2*weather_fCaSiO3)/4.0
    ! Fe
    loc_force_flux_weather_o(io_Fe2) = loc_force_flux_weather_o(io_Fe2) + &
         & par_weather_CaSiO3_fracFeS2*weather_fCaSiO3
    loc_standard = const_standards(ocn_type(io_Fe2_56Fe))
    loc_force_flux_weather_o(io_Fe2_56Fe) = loc_force_flux_weather_o(io_Fe2_56Fe) + &
         & fun_calc_isotope_fraction(par_weather_CaSiO3_fracFeS2_d56Fe,loc_standard)* &
         & par_weather_CaSiO3_fracFeS2*weather_fCaSiO3
    ! O2 consumption associated with pyrite weathering (and conversion of H2S -> H2SO4)
    ! NOTE: pyrite weathering should be the only source of H2S
    IF (.NOT. opt_short_circuit_atm) THEN
       loc_force_flux_weather_a(ia_pO2)     = loc_force_flux_weather_a(ia_pO2)     - 2.0*loc_force_flux_weather_o(io_H2S)
       loc_force_flux_weather_o(io_SO4)     = loc_force_flux_weather_o(io_SO4)     + loc_force_flux_weather_o(io_H2S)
       loc_force_flux_weather_o(io_SO4_34S) = loc_force_flux_weather_o(io_SO4_34S) + loc_force_flux_weather_o(io_H2S_34S)
       loc_force_flux_weather_o(io_ALK)     = loc_force_flux_weather_o(io_ALK)     - 2.0*loc_force_flux_weather_o(io_H2S)
       loc_force_flux_weather_o(io_H2S)     = 0.0
       loc_force_flux_weather_o(io_H2S_34S) = 0.0
    ENDIF
    ! ######################################################################################################################### !

    ! ######################################################################################################################### !
    ! GYPSUM
    ! NOTE: no net ALK input (Ca2+ PLUS SO42-)
    ! bulk carbonate/evapourite (gypsum) S flux (and isotopic signature)
    loc_force_flux_weather_o(io_SO4) = loc_force_flux_weather_o(io_SO4) + &
         & par_weather_CaCO3_fracCaSO4*weather_fCaCO3
    loc_standard = const_standards(ocn_type(io_H2S_34S))
    loc_force_flux_weather_o(io_SO4_34S) = loc_force_flux_weather_o(io_SO4_34S) + &
         & fun_calc_isotope_fraction(par_weather_CaCO3_fracCaSO4_d34S,loc_standard)* &
         & par_weather_CaCO3_fracCaSO4*weather_fCaCO3
    ! bulk carbonate/evapourite (gypsum) Ca flux (and isotopic signature)
    loc_force_flux_weather_o(io_Ca) = loc_force_flux_weather_o(io_Ca) + &
         & par_weather_CaCO3_fracCaSO4*weather_fCaCO3
    loc_standard = const_standards(ocn_type(io_Ca_44Ca))
    loc_force_flux_weather_o(io_Ca_44Ca) = loc_force_flux_weather_o(io_Ca_44Ca) + &
         & fun_calc_isotope_fraction(par_weather_CaCO3_fracCaSO4_d44Ca,loc_standard)* &
         & par_weather_CaCO3_fracCaSO4*weather_fCaCO3
    ! ######################################################################################################################### !

    ! ######################################################################################################################### !
    ! SIDERITE
    ! NOTE: set Fe flux as Fe2 (not TDFe  for now / here)
    ! NOTE: not net ALK input
    ! bulk siderite (FeCO3) -- Fe
    loc_force_flux_weather_o(io_Fe2) = loc_force_flux_weather_o(io_Fe2) + &
         & par_weather_CaCO3_fracFeCO3*weather_fCaCO3
    loc_standard = const_standards(ocn_type(io_Fe2_56Fe))
    loc_force_flux_weather_o(io_Fe2_56Fe) = loc_force_flux_weather_o(io_Fe2_56Fe) + &
         & fun_calc_isotope_fraction(par_weather_CaCO3_fracFeCO3_d56Fe,loc_standard)* &
         & par_weather_CaCO3_fracFeCO3*weather_fCaCO3
    ! bulk siderite (FeCO3) -- CO32-
    loc_force_flux_weather_o(io_DIC) = loc_force_flux_weather_o(io_DIC) + &
         & par_weather_CaCO3_fracFeCO3*weather_fCaCO3
    loc_standard = const_standards(ocn_type(io_DIC_13C))
    loc_force_flux_weather_o(io_DIC_13C) = loc_force_flux_weather_o(io_DIC_13C) + &
         & fun_calc_isotope_fraction(par_weather_CaCO3_fracFeCO3_d13C,loc_standard)* &
         & par_weather_CaCO3_fracFeCO3*weather_fCaCO3
    ! ######################################################################################################################### !

    ! ######################################################################################################################### !
    ! APATITE
    ! NOTE: include ALK
    ! bulk apatite (Ca5PO43)
    loc_force_flux_weather_o(io_PO4) = loc_force_flux_weather_o(io_PO4) + &
         & 3.0*par_weather_CaSiO3_fracCa5PO43*weather_fCaSiO3
    loc_force_flux_weather_o(io_Ca) = loc_force_flux_weather_o(io_Ca) + &
         & 5.0*par_weather_CaSiO3_fracCa5PO43*weather_fCaSiO3
    loc_force_flux_weather_o(io_ALK) = loc_force_flux_weather_o(io_ALK) + &
         & 2.0*5.0*par_weather_CaSiO3_fracCa5PO43*weather_fCaSiO3
    ! *** CALCIUM ISOTOPES ***
    loc_standard = const_standards(ocn_type(io_Ca_44Ca))
    ! apatite 44Ca weathering flux
    loc_epsilon = par_weather_CaSiO3_fracCa5PO43_d44Ca
    loc_force_flux_weather_o(io_Ca_44Ca) = loc_force_flux_weather_o(io_Ca_44Ca) + &
         & fun_calc_isotope_fraction(loc_epsilon,loc_standard)*5.0*par_weather_CaSiO3_fracCa5PO43*weather_fCaSiO3
    ! add simple/direct P input (no Ca assumed/included)
    loc_force_flux_weather_o(io_PO4) = loc_force_flux_weather_o(io_PO4) + &
         & par_weather_Ca0PO41
    ! ######################################################################################################################### !

    ! ######################################################################################################################### !
    ! SILICA
    ! bulk silicate Si flux (and isotopic signature)
    ! NOTE: despite the shorthand for silicates as 'CaSiO3', no simple 1:1 correspondence between Ca and Si should be assumed ...
    loc_force_flux_weather_o(io_SiO2) = loc_force_flux_weather_o(io_SiO2) + &
         & par_weather_CaSiO3_fracSi*weather_fCaSiO3
    loc_standard = const_standards(ocn_type(io_SiO2_30Si))
    loc_force_flux_weather_o(io_SiO2_30Si) = loc_force_flux_weather_o(io_SiO2_30Si) + &
         & fun_calc_isotope_fraction(par_weather_CaSiO3_fracSi_d30Si,loc_standard)* &
         & par_weather_CaSiO3_fracSi*weather_fCaSiO3
    ! quartz on its own ...
    loc_force_flux_weather_o(io_SiO2) = loc_force_flux_weather_o(io_SiO2) + &
         & par_weather_SiO2
    loc_standard = const_standards(ocn_type(io_SiO2_30Si))
    loc_force_flux_weather_o(io_SiO2_30Si) = loc_force_flux_weather_o(io_SiO2_30Si) + &
         & fun_calc_isotope_fraction(par_weather_SiO2_d30Si,loc_standard)* &
         & par_weather_SiO2
    ! ######################################################################################################################### !

    ! Spread out atmosphere variables' fluxes onto land
    DO k=3,n_atm
       loc_force_flux_weather_a_percell(k)  = loc_force_flux_weather_a(k)/nlandcells
       loc_force_flux_weather_a_land(k,:,:) = landmask(:,:) * loc_force_flux_weather_a_percell(k)
    END DO
    ! no need to route to the atmosphere - just take it straight from the cells above the land (assuming same grid)
    ! convert from Mol/yr to Mol/sec/m^2 and put it into passing array (only take variable altered here - pCO2)
    dum_sfxatm1(ia_PCO2,:,:)     =  loc_force_flux_weather_a_land(ia_PCO2,:,:)/(phys_rok(ipr_A,:,:)*conv_yr_s)
    dum_sfxatm1(ia_PCO2_13C,:,:) =  loc_force_flux_weather_a_land(ia_PCO2_13C,:,:)/(phys_rok(ipr_A,:,:)*conv_yr_s)
    ! Spread out ocean variables' fluxes onto land
    DO k=3,n_ocn
       IF (opt_weather_runoff) THEN
          loc_force_flux_weather_o_percell(k)  = loc_force_flux_weather_o(k)/loc_totR
          loc_force_flux_weather_o_land(k,:,:) = landmask(:,:) * dum_runoff(:,:) * loc_force_flux_weather_o_percell(k)
       else
          loc_force_flux_weather_o_percell(k)  = loc_force_flux_weather_o(k)/nlandcells
          loc_force_flux_weather_o_land(k,:,:) = landmask(:,:) * loc_force_flux_weather_o_percell(k)
       end if
    END DO
    ! route it into the coastal ocean cells (to pass to biogem in coupled model) and save the output to file
    DO k=3,n_ocn
       CALL sub_coastal_output(  loc_force_flux_weather_o_land(k,:,:),             &
            &  runoff_drainto(:,:,:),runoff_detail(:,:),                           &
            &  loc_force_flux_weather_o_ocean(k,:,:)                               )
    END DO
    ! convert from Mol/yr to Mol/sec and put it into passing array 
    dum_sfxrok(:,:,:) = loc_force_flux_weather_o_ocean(:,:,:)/conv_yr_s

    ! Output     

    IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN

       outputs = (/loc_avSLT,loc_maxSLT,loc_minSLT,loc_R*conv_yr_s,loc_maxR*conv_yr_s,loc_minR*conv_yr_s, &
            & loc_avP,loc_maxP,loc_minP,loc_CO2,loc_maxCO2,loc_minCO2, &
            & loc_weather_ratio_CaCO3,loc_weather_ratio_CaSiO3,weather_fCaCO3/1.0E12,weather_fCaSiO3/1.0E12, &
            & loc_force_flux_weather_a(ia_PCO2)/1.0E12, &
            & loc_force_flux_weather_o(io_ALK)/1.0E12,loc_force_flux_weather_o(io_DIC)/1.0E12, &
            & loc_force_flux_weather_o(io_Ca)/1.0E12,loc_force_flux_weather_o(io_DIC_13C)/1.0E12, &
            & sum(loc_force_flux_weather_o_land(io_ALK,:,:))/1.0E12,sum(loc_force_flux_weather_o_land(io_DIC,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_land(io_Ca,:,:))/1.0E12,sum(loc_force_flux_weather_o_land(io_DIC_13C,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_ocean(io_ALK,:,:))/1.0E12,sum(loc_force_flux_weather_o_ocean(io_DIC,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_ocean(io_Ca,:,:))/1.0E12,sum(loc_force_flux_weather_o_ocean(io_DIC_13C,:,:))/1.0E12/)

       call sub_output_0d(n_outputs,(/12,21,25/),outputs,output_descriptions,time_series_names)

    ENDIF

    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN

       IF (opt_2d_netcdf_output) THEN
          call rokgem_netcdf(loc_SLT,loc_CO22,dum_runoff,dum_photo,dum_respveg,loc_P,&
               & loc_force_flux_weather_a_land,loc_force_flux_weather_o_land,loc_force_flux_weather_o_ocean)
       ENDIF

       IF (opt_2d_ascii_output) THEN
          DO k=1,n_ocn
             IF ((k.EQ.io_ALK).OR.(k.EQ.io_DIC).OR.(k.EQ.io_Ca).OR.(k.EQ.io_DIC_13C).OR.(k.EQ.io_DIC_14C)) THEN
                CALL sub_save_data_ij( &
                     & TRIM(par_outdir_name)//'globavg_land_'//TRIM(globtracer_names(k))//'_year_'//TRIM(year_text)//'.dat', &
                     n_i,n_j,loc_force_flux_weather_o_land(k,:,:))                                   
                CALL sub_save_data_ij( &
                     & TRIM(par_outdir_name)//'globavg_ocean_'//TRIM(globtracer_names(k))//'_year_'//TRIM(year_text)//'.dat', &
                     n_i,n_j,loc_force_flux_weather_o_ocean(k,:,:))  
             ENDIF
          END DO
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'globavg_atm_PCO2_year_'//TRIM(year_text)//'.dat', &
               n_i,n_j,loc_force_flux_weather_a_land(ia_PCO2,:,:))                                   
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'temperature_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_SLT(:,:))
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'runoff_year_'//TRIM(year_text)//'.dat',n_i,n_j,dum_runoff(:,:)*conv_yr_s)
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'productivity_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_P(:,:))
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'CO2_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_CO22(:,:))
       ENDIF

    ENDIF

    ! for outputting calibrate 2D reference files
    ! should really only do at the end to save computation, i.e. export loc_SLT
    IF (opt_calibrate_T_2D) THEN
       ref_T0_2D(:,:) = loc_SLT(:,:)
    ENDIF
    IF (opt_calibrate_R_2D) THEN
       ref_R0_2D(:,:) = loc_runoff(:,:)*conv_yr_s
    ENDIF
    IF (opt_calibrate_P_2D) THEN
       ref_P0_2D(:,:) = loc_P(:,:)
    ENDIF

  END SUBROUTINE sub_glob_avg_weath


  !======= 2D LITHOLOGY DEPENDENT WEATHERING ==============================================!

  ! Subroutine: sub_GKWM
  !
  ! Subroutine for spatially-explicit weathering based on Gibbs et al. (1999)
  !
  ! Uses:
  !
  ! <genie_util>, ONLY: <check_unit>, <check_iostat>
  !
  !
  ! Calls:
  !
  ! - <sub_save_data_ij>
  !
  ! Input:
  !
  ! dum__runoff - run-off read in from exernal module (EMBM, or ENTS)
  ! dum_lithology - array containing proportions of each type of lithology in each land cell
  !
  ! Output:
  !
  ! dum_calcium_flux - array containing fluxes of calcium for each lithological type

  subroutine sub_GKWM       (                                  &
       & dum_runoff,                      &
       & dum_lithology,                   &
       & dum_calcium_flux                  )

    USE genie_util, ONLY: check_unit, check_iostat


    IMPLICIT NONE

    ! dummy variables
    REAL, intent(in)                :: dum_runoff(n_i,n_j)
    REAL, intent(in)                :: dum_lithology(par_nliths,n_i,n_j)
    REAL, intent(inout)             :: dum_calcium_flux(par_nliths,n_i,n_j)            ! F_HCO_3- is sum of this

    ! local variables
    REAL                            :: loc_runoff(n_i,n_j)
    REAL                            :: avg_runoff
    REAL                            :: r_avg_runoff
    REAL                            :: conv_factor(par_nliths)
    REAL                            :: rescale_runoff
    INTEGER                         :: i, j, k, ios

    ! Put runoff into local array
    loc_runoff(:,:) = dum_runoff(:,:)

    ! Do calibrations if requested
    IF (opt_calibrate_R_0D) THEN
       DO i=1,n_i
          DO j=1,n_j
             loc_runoff(i,j) = loc_runoff(i,j) * calibrate_R_0D
          END DO
       END DO
    ENDIF
    IF (opt_calibrate_R_2D) THEN
       loc_runoff(:,:) = loc_runoff(:,:) * calibrate_R_2D(:,:)
    ENDIF

    ! Calculate average runoff and reciprocal
    avg_runoff=sum(landmask(:,:) * loc_runoff(:,:))/nlandcells
    if (avg_runoff.eq.0.0) then
       r_avg_runoff = 1E12
    else
       r_avg_runoff = 1.0/avg_runoff
    endif

    ! Divide weathering up into transport and kinetic limited regimes if requested     
    IF (opt_weath_regimes) THEN

       DO i=1,n_i
          DO j=1,n_j
             IF (orogeny(i,j).eq.1) THEN
                IF (opt_calibrate_R_0D) THEN
                   regimes_calib(i,j) = par_data_R_0D
                ELSE
                   regimes_calib(i,j) = par_ref_R0
                ENDIF
             ENDIF
             IF (orogeny(i,j).eq.2) THEN
                regimes_calib(i,j) = avg_runoff
             ENDIF
          END DO
       END DO

       ! Speed up numerics by combining conversion factor, calibration with average runoff, and k
       do k = 1, par_nliths
          ! see initialise_rokgem.f90 for info on conversion factors conv_*
          conv_factor(k) = weath_consts(k,1) * conv_GKWM * ((conv_GKWM_runoff*r_avg_runoff) ** weath_consts(k,2)) 
       end do

       ! Calculate F_HCO_3- (calcium_flux)       
       DO k = 1, par_nliths
          DO i = 1, n_i
             DO j = 1, n_j
                dum_calcium_flux(k,i,j) = conv_factor(k) * regimes_calib(i,j)*&              
                     &                                dum_lithology(k,i,j) *               &
                     &                                (loc_runoff(i,j) ** weath_consts(k,2))
             END DO
          END DO
       END DO

    ELSE

       ! Set calibration factor if option is set
       ! If runoff feedback is off, then runoff is scaled to initially chosen reference point R0
       ! (R0 is set equal to the average runoff after each spin-up stage)
       ! This reference point is different if it is calibrated (to data) using opt_calibrate_R_0D.
       IF (opt_weather_R_Ca.or.opt_weather_R_Si) THEN
          rescale_runoff = avg_runoff
       ELSE
          IF (opt_calibrate_R_0D) THEN
             rescale_runoff = par_data_R_0D
          ELSE
             rescale_runoff = par_ref_R0
          ENDIF
       ENDIF

       ! Speed up numerics by combining conversion factor, calibration with average runoff, and k
       do k = 1, par_nliths
          ! see initialise_rokgem.f90 for info on conversion factors conv_*
          conv_factor(k) = weath_consts(k,1) * conv_GKWM * ((conv_GKWM_runoff*rescale_runoff*r_avg_runoff) ** weath_consts(k,2))
       end do

       ! Calculate F_HCO_3- (calcium_flux)       
       DO k = 1, par_nliths
          DO i = 1, n_i
             DO j = 1, n_j
                dum_calcium_flux(k,i,j) = conv_factor(k) *                   &              
                     &                                dum_lithology(k,i,j) *              &
                     &                                (loc_runoff(i,j) ** weath_consts(k,2))
             END DO
          END DO
       END DO

    ENDIF



    ! Save data to files calcium_lith.dat where 'lith' is the lithology
    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
       IF (opt_2d_ascii_output) THEN
          DO k = 1,par_nliths
             !          PRINT*,'Saving map of calcium flux for ',TRIM(lithology_names(k)),                            &
             !                &' to calcium_',lithology_names(k)(1:LEN(TRIM(lithology_names(k)))-4)//'.dat'
             call check_unit(17,__LINE__,__FILE__)
             OPEN(17,file=TRIM(par_outdir_name)// &  
                  & 'calcium_'//lithology_names(k)(1:LEN(TRIM(lithology_names(k)))-4)//'_'//       &
                  TRIM(year_text)//'.dat',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             DO j=n_j,1,-1
                WRITE(17,*,iostat=ios)(dum_calcium_flux(k,i,j),i=1,n_i)
                call check_iostat(ios,__LINE__,__FILE__)
             END DO
             CLOSE(17,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          END DO
       ENDIF
    ENDIF

  END SUBROUTINE sub_GKWM


  !========================================================================================!

  ! Subroutine: sub_GEM_CO2 
  !
  ! Subroutine for spatially-explicit weathering based on Amiotte-Suchet et al. (2003)
  !
  ! Uses:
  !
  ! <genie_util>, ONLY: <check_unit>, <check_iostat>
  !
  !
  ! Calls:
  !
  ! - <sub_save_data_ij>
  !
  ! Input:
  !
  ! dum__runoff - run-off read in from exernal module (EMBM, or ENTS)
  ! dum_lithology - array containing proportions of each type of lithology in each land cell
  !
  ! Output:
  !
  ! dum_calcium_flux - array containing fluxes of calcium for each lithological type

  SUBROUTINE sub_GEM_CO2     (                                  &
       & dum_runoff,                      &
       & dum_lithology,                   &
       & dum_calcium_flux                  )

    USE genie_util, ONLY: check_unit, check_iostat

    IMPLICIT NONE

    ! dummy variables
    REAL,INTENT(in)                 :: dum_runoff(n_i,n_j)
    REAL, INTENT(in)                :: dum_lithology(par_nliths,n_i,n_j)
    REAL, INTENT(inout)             :: dum_calcium_flux(par_nliths,n_i,n_j)            ! F_HCO_3- is sum of this

    ! local variables
    REAL                            :: loc_runoff(n_i,n_j)
    REAL                            :: avg_runoff
    REAL                            :: rescale_runoff
    REAL                            :: r_avg_runoff
    REAL                            :: conv_factor(par_nliths)
    INTEGER                         :: i, j, k, ios

    ! Put runoff into local array
    loc_runoff(:,:) = dum_runoff(:,:)

    ! Do calibrations if requested
    IF (opt_calibrate_R_0D) THEN
       DO i=1,n_i
          DO j=1,n_j
             loc_runoff(i,j) = loc_runoff(i,j) * calibrate_R_0D
          END DO
       END DO
    ENDIF
    IF (opt_calibrate_R_2D) THEN
       loc_runoff(:,:) = loc_runoff(:,:) * calibrate_R_2D(:,:)
    ENDIF

    ! Calculate average runoff and reciprocal
    avg_runoff=sum(landmask(:,:) * loc_runoff(:,:))/nlandcells
    if (avg_runoff.eq.0.0) then
       r_avg_runoff = 1E12
    else
       r_avg_runoff = 1.0/avg_runoff
    endif

    ! Set calibration factor if option is set
    ! If runoff feedback is off, then runoff is scaled to initially chosen reference point R0
    ! (R0 is set equal to the average runoff after each spin-up stage)
    ! This reference point is different if it is calibrated (to data) using opt_calibrate_R_0D.
    IF (opt_weather_R_Ca.or.opt_weather_R_Si) THEN
       rescale_runoff = avg_runoff
    ELSE
       IF (opt_calibrate_R_0D) THEN
          rescale_runoff = par_data_R_0D
       ELSE
          rescale_runoff = par_ref_R0
       ENDIF
    ENDIF

    ! Speed up numerics by combining conversion factor, calibration with average runoff, and k
    do k = 1, par_nliths
       ! see initialise_rokgem.f90 for info on conversion factors conv_GEM_CO2
       conv_factor(k) = weath_consts(k,1) * conv_GEM_CO2 * rescale_runoff * r_avg_runoff
    end do

    ! Calculate F_HCO_3- (calcium_flux)       
    DO k = 1, par_nliths
       DO i = 1, n_i
          DO j = 1, n_j
             dum_calcium_flux(k,i,j) = conv_factor(k) * dum_lithology(k,i,j) * loc_runoff(i,j)
          END DO
       END DO
    END DO

    ! Divide weathering up into transport and kinetic limited regimes if requested     
    IF (opt_weath_regimes) THEN  
       DO k = 1, par_nliths
          DO i=1,n_i
             DO j=1,n_j
                IF (orogeny(i,j).eq.1) THEN
                   dum_calcium_flux(k,i,j) = dum_calcium_flux(k,i,j) * r_avg_runoff
                ENDIF
             END DO
          END DO
       ENDDO
    ENDIF

    ! Save data to files calcium_lith.dat where 'lith' is the lithology
    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
       IF (opt_2d_ascii_output) THEN
          DO k = 1,par_nliths
             !          PRINT*,'Saving map of calcium flux for ',TRIM(lithology_names(k)),                            &
             !                &' to calcium_',lithology_names(k)(1:LEN(TRIM(lithology_names(k)))-4)//'.dat'
             call check_unit(17,__LINE__,__FILE__)
             OPEN(17,file=TRIM(par_outdir_name)// &
                  & 'calcium_'//lithology_names(k)(1:LEN(TRIM(lithology_names(k)))-4)//'_year_'// &
                  TRIM(year_text)//'.dat',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             DO j=n_j,1,-1
                WRITE(17,*,iostat=ios)(dum_calcium_flux(k,i,j),i=1,n_i)
                call check_iostat(ios,__LINE__,__FILE__)
             END DO
             CLOSE(17,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          END DO
       ENDIF
    ENDIF



  END SUBROUTINE sub_GEM_CO2


  ! ======= SUM UP THE WEATHERING FLUX ====================================================!

  ! Subroutine: sum_calcium_flux
  !
  ! Subroutine to sum up calcium fluxes.
  !
  ! Calls:
  !
  ! - <sub_save_data_ij>
  !
  ! Input:
  !
  ! dum_calcium_flux - array containing fluxes of calcium for each lithological type
  !
  ! Output:
  !
  ! dum_total_calcium_flux - array containing total fluxes of calcium from each grid cell

  SUBROUTINE sum_calcium_flux(dum_calcium_flux,dum_total_calcium_flux)

    REAL, INTENT(in)                :: dum_calcium_flux(par_nliths,n_i,n_j)            ! F_HCO_3- is sum of this
    REAL, INTENT(inout)             :: dum_total_calcium_flux(n_i,n_j)                 ! F_HCO_3-

    INTEGER                         :: i, j, k

    dum_total_calcium_flux(:,:) = 0.0

    DO k=1,par_nliths
       DO i=1,n_i
          DO j=1,n_j
             dum_total_calcium_flux(i,j) = dum_total_calcium_flux(i,j) +               &
                  &        dum_calcium_flux(k,i,j)
          END DO
       END DO
    END DO

    ! Save data to file calcium_total.dat
    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
       IF (opt_2d_ascii_output) THEN
          CALL sub_save_data_ij( &
               & TRIM(par_outdir_name)//'calcium_total_year_'//TRIM(year_text)//'.dat',n_i,n_j,dum_total_calcium_flux(:,:) &
               & )   
       ENDIF
    ENDIF

  END SUBROUTINE sum_calcium_flux


  ! ======= DIVIDE WEATHERING FLUX INTO CARBONATE AND SILICATE WEATHERING ==================!

  ! Subroutine: sum_calcium_flux_CaSi
  !
  ! Subroutine to divide up calcium fluxes between those resulting from Carbonate rocks, and those from Silicate rocks.
  !
  ! Calls:
  !
  ! - <sub_save_data_ij>
  !
  ! Input:
  !
  ! dum__calcium_flux - array containing fluxes of calcium for each lithological type
  !
  ! Output:
  !
  ! dum_total_calcium_flux_Ca - array containing total fluxes of calcium resulting from Carbonate rocks for each grid cell
  ! dum_total_calcium_flux_Si - array containing total fluxes of calcium resulting from Silicate rocks for each grid cell

  SUBROUTINE sum_calcium_flux_CaSi(dum_calcium_flux,dum_total_calcium_flux_Ca,dum_total_calcium_flux_Si)

    REAL, INTENT(in)                :: dum_calcium_flux(par_nliths,n_i,n_j)            ! F_HCO_3- is sum of this
    REAL, INTENT(inout)             :: dum_total_calcium_flux_Ca(n_i,n_j)              ! F_HCO_3- for Ca rocks
    REAL, INTENT(inout)             :: dum_total_calcium_flux_Si(n_i,n_j)              ! F_HCO_3- for Si rocks

    INTEGER                         :: i, j, k

    ! k = 1 is for carb rock type - corresponding to Carbonate rock weathering
    ! all other rock types are taken to be silicates (2 onwards); not sure if this is correct!
    ! Now have 0.125 of sand rock type as Carbonate weathering also - so have extra array terms for fCa and fSi in weath_consts array
    dum_total_calcium_flux_Ca(:,:) = 0.0
    DO k=1,par_nliths
       DO i=1,n_i
          DO j=1,n_j
             dum_total_calcium_flux_Ca(i,j) = dum_total_calcium_flux_Ca(i,j) +               &
                  &        dum_calcium_flux(k,i,j) * weath_consts(k,3)
          END DO
       END DO
    END DO

    dum_total_calcium_flux_Si(:,:) = 0.0
    DO k=1,par_nliths
       DO i=1,n_i
          DO j=1,n_j
             dum_total_calcium_flux_Si(i,j) = dum_total_calcium_flux_Si(i,j) +               &
                  &        dum_calcium_flux(k,i,j) * weath_consts(k,4)
          END DO
       END DO
    END DO

    ! Save data to file calcium_total.dat
    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
       IF (opt_2d_ascii_output) THEN
          CALL sub_save_data_ij( &
               & TRIM(par_outdir_name)//'calcium_total_Ca_year_'//TRIM(year_text)//'.dat',n_i,n_j,dum_total_calcium_flux_Ca(:,:) &
               & )   
          CALL sub_save_data_ij( &
               & TRIM(par_outdir_name)//'calcium_total_Si_year_'//TRIM(year_text)//'.dat',n_i,n_j,dum_total_calcium_flux_Si(:,:) &
               & )
       ENDIF
    ENDIF

  END SUBROUTINE sum_calcium_flux_CaSi


  !========================================================================================!

  ! Subroutine: sub_2D_weath
  !
  ! Subroutine to calculate spatially-explicit weathering
  !
  ! Calls:
  !
  ! - <sub_init_phys_ocnrok>
  ! - <calc_P>
  ! - <sub_coastal_output>
  ! - <sub_save_data_ij>
  !
  ! Input:
  !
  ! dum_sfcatm1 - atmosphere composition interface array (to get temperature from)
  ! dum__runoff - run-off read in from exernal module (EMBM, or ENTS)
  ! dum_photo(n_i,n_j) - photosynthesis array
  ! dum_respveg(n_i,n_j) - vegetation respiration array
  !
  ! Output:
  !
  ! dum_sfxrok - ocean flux interface array (same no of tracers as used in biogem ocean)
  ! dum_sfxatm1 - atmosphere flux interface array (same no of tracers as used in atchem atmosphere)

  SUBROUTINE sub_2D_weath(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)

    ! Based on SUBROUTINE sub_glob_avg_weath - see above

    ! dummy variables
    REAL,INTENT(in)                 :: dum_sfcatm1(n_atm,n_io,n_jo)      ! atmosphere composition interface array
    REAL,INTENT(in)                 :: dum_runoff(n_i,n_j)
    REAL,INTENT(in)                 :: dum_photo(n_i,n_j)                ! photosythesis from land veg module (ENTS)
    REAL,INTENT(in)                 :: dum_respveg(n_i,n_j)              ! vegetation respiration from land veg module (ENTS)
    REAL,INTENT(inout)              :: dum_sfxrok(n_ocn,n_i,n_j)                                ! ocean flux interface array (same no of tracers as used in biogem ocean)
    REAL,INTENT(inout)              :: dum_sfxatm1(n_atm,n_io,n_jo)      ! atmosphere flux interface array

    ! local variables
    INTEGER                         :: i, j, k
    REAL                            :: loc_SLT(n_i,n_j)
    REAL                            :: loc_SLT0
    REAL                            :: loc_runoff(n_i,n_j)
    REAL                            :: loc_P(n_i,n_j)
    REAL                            :: loc_P0
    REAL                            :: loc_CO2(n_i,n_j)
    REAL                            :: loc_CO20
    REAL                            :: loc_weather_ratio_CaSiO3(n_i,n_j)
    REAL                            :: loc_weather_ratio_CaCO3(n_i,n_j)
    REAL                            :: n
    REAL                            :: loc_standard

    REAL                            :: loc_force_flux_weather_a_land(n_atm,n_i,n_j) ! fluxes shared over land (atmosphere variables)
    REAL                            :: loc_force_flux_weather_o_land(n_ocn,n_i,n_j) ! fluxes shared over land (ocean variables)
    REAL                            :: loc_force_flux_weather_o_ocean(n_ocn,n_i,n_j)              ! fluxes into coastal positions in ocean (ocean variables)      

    CHARACTER(LEN=7),DIMENSION(n_ocn)       :: globtracer_names

    ! initialise tracer names       
    globtracer_names(io_ALK)                  = 'ALK    '
    globtracer_names(io_DIC)                  = 'DIC    '
    globtracer_names(io_Ca)                   = 'Ca     '
    globtracer_names(io_DIC_13C)              = 'DIC_13C'
    globtracer_names(io_DIC_14C)              = 'DIC_14C'

    ! initialise arrays   
    loc_force_flux_weather_a_land(:,:,:)        = 0.0           
    loc_force_flux_weather_o_land(:,:,:)        = 0.0       
    loc_force_flux_weather_o_ocean(:,:,:)       = 0.0

    ! set reference surface land (air) temperature and productivity
    loc_SLT0 = par_ref_T0
    loc_P0 = par_ref_P0
    loc_CO20 = par_ref_CO20

    ! Initialise ocean array for temperature
    CALL sub_init_phys_ocnrok()  

    ! Put runoff into local array
    loc_runoff(:,:) = dum_runoff(:,:)

    ! Do calibrations if requested
    IF (opt_calibrate_R_0D) THEN
       DO i=1,n_i
          DO j=1,n_j
             loc_runoff(i,j) = loc_runoff(i,j) * calibrate_R_0D
          END DO
       END DO
    ENDIF
    IF (opt_calibrate_R_2D) THEN
       loc_runoff(:,:) = loc_runoff(:,:) * calibrate_R_2D(:,:)
    ENDIF

    ! extract temperature to local array to please intel compilers
    !                     print*,"before calib", loc_SLT(:,1)
    DO i=1,n_i
       DO j=1,n_j
          loc_SLT(i,j) = dum_sfcatm1(ia_T,i,j)
          ! do calibrations if requested
          IF (opt_calibrate_T_0D) THEN
             loc_SLT(i,j) = loc_SLT(i,j) + 273.15
             loc_SLT(i,j) = loc_SLT(i,j) * calibrate_T_0D
             loc_SLT(i,j) = loc_SLT(i,j) - 273.15
          ENDIF
          IF (opt_calibrate_T_2D) THEN
             loc_SLT(i,j) = loc_SLT(i,j) + 273.15
             loc_SLT(i,j) = loc_SLT(i,j) * calibrate_T_2D(i,j)
             loc_SLT(i,j) = loc_SLT(i,j) - 273.15
          ENDIF
       END DO
    END DO
    ! extract temperature to local array to please intel compilers
    !                     print*,"after calib", loc_SLT(:,1)

    ! calculate mean surface productivity (kgC m-2 yr-1)
    SELECT case (par_prodopt)
       ! Global Primary Productivity
    case ('GPP')
       loc_P(:,:) = dum_photo(:,:)
       ! Net Primary Productivity
    case ('NPP')
       loc_P(:,:) = dum_photo(:,:) - dum_respveg(:,:)
    end SELECT

    ! Do calibrations if requested
    IF (opt_calibrate_P_0D) THEN
       DO i=1,n_i
          DO j=1,n_j
             loc_P(i,j) = loc_P(i,j) * calibrate_P_0D
          END DO
       END DO
    ENDIF
    ! calculate mean surface productivity (kgC m-2 yr-1)
    SELECT case (par_prodopt)
       ! Global Primary Productivity
    case ('GPP')
       loc_P(:,:) = dum_photo(:,:)
       ! Net Primary Productivity
    case ('NPP')
       loc_P(:,:) = dum_photo(:,:) - dum_respveg(:,:)
    end SELECT
    IF (opt_calibrate_P_2D) THEN
       loc_P(:,:) = loc_P(:,:) * calibrate_P_2D(:,:)
    ENDIF

    ! convert atm pCO2 to ppm
    DO i=1,n_i
       DO j=1,n_j
          loc_CO2(i,j) = 1.0E+06*dum_sfcatm1(ia_PCO2,i,j)
       END DO
    END DO

    ! Do calculations

    loc_weather_ratio_CaCO3(:,:) = 1.0
    IF (opt_weather_T_Ca) THEN
       DO i=1,n_i
          DO j=1,n_j
             ! make sure that numbers stay positive
             n = 1.0 + par_k_Ca*(dum_sfcatm1(ia_T,i,j) - loc_SLT0)
             IF (n.lt.0.0) THEN
                n = 0
             ENDIF
             loc_weather_ratio_CaCO3(i,j) = n
          END DO
       END DO
    ENDIF
    IF (opt_weather_P_Ca) THEN
       IF (opt_weather_P_explicit) THEN
          loc_weather_ratio_CaCO3(:,:) = (loc_P(:,:)/loc_P0)*loc_weather_ratio_CaCO3(:,:)  ! From Lenton & Britton (2006)
       ELSE
          loc_weather_ratio_CaCO3(:,:) = loc_weather_ratio_CaCO3(:,:)* &
               & (2*(loc_CO2(:,:)/loc_CO20)/(1+(loc_CO2(:,:)/loc_CO20)))**0.4 ! From GEOCARB
       ENDIF
    ENDIF

    loc_weather_ratio_CaSiO3(:,:) = 1.0
    IF (opt_weather_T_Si) THEN
       DO i=1,n_i
          DO j=1,n_j
             loc_weather_ratio_CaSiO3(i,j) = exp(k_T*(dum_sfcatm1(ia_T,i,j) - loc_SLT0)) 
          END DO
       END DO
    ENDIF
    IF (opt_weather_P_Si) THEN
       IF (opt_weather_P_explicit) THEN
          loc_weather_ratio_CaSiO3(:,:) = (loc_P(:,:)/loc_P0)*loc_weather_ratio_CaSiO3(:,:)  ! From Lenton & Britton (2006)
       ELSE
          loc_weather_ratio_CaSiO3(:,:) = loc_weather_ratio_CaSiO3(:,:)* &
               & (2*(loc_CO2(:,:)/loc_CO20)/(1+(loc_CO2(:,:)/loc_CO20)))**0.4 ! From GEOCARB
       ENDIF
    ENDIF

    ! Divide weathering up into transport and kinetic limited regimes if requested     
    IF (opt_weath_regimes) THEN       
       DO i=1,n_i
          DO j=1,n_j
             IF (orogeny(i,j).eq.1) THEN
                loc_weather_ratio_CaCO3(i,j) = 1.0
                loc_weather_ratio_CaSiO3(i,j) = 1.0
             ENDIF
          END DO
       END DO
    ENDIF

    ! calibrations
    IF (calibrate_weath) THEN
       SELECT case (par_weathopt)
       case ('GKWM')
          weather_fCaCO3_2D(:,:)  = loc_weather_ratio_CaCO3(:,:)*total_calcium_flux_Ca(:,:)*calibrate_weather_GKWM_CaCO3
          weather_fCaSiO3_2D(:,:) = loc_weather_ratio_CaSiO3(:,:)*total_calcium_flux_Si(:,:)*calibrate_weather_GKWM_CaSiO3
       case ('GEM_CO2')
          weather_fCaCO3_2D(:,:)  = loc_weather_ratio_CaCO3(:,:)*total_calcium_flux_Ca(:,:)*calibrate_weather_GEM_CO2_CaCO3
          weather_fCaSiO3_2D(:,:) = loc_weather_ratio_CaSiO3(:,:)*total_calcium_flux_Si(:,:)*calibrate_weather_GEM_CO2_CaSiO3
       end SELECT
    ELSE
       weather_fCaCO3_2D(:,:)  = loc_weather_ratio_CaCO3(:,:)*total_calcium_flux_Ca(:,:)
       weather_fCaSiO3_2D(:,:) = loc_weather_ratio_CaSiO3(:,:)*total_calcium_flux_Si(:,:)
    ENDIF

    loc_force_flux_weather_o_land(io_Ca,:,:) = weather_fCaSiO3_2D(:,:) + weather_fCaCO3_2D(:,:)
    loc_force_flux_weather_o_land(io_ALK,:,:) = 2.0*weather_fCaSiO3_2D(:,:) + 2.0*weather_fCaCO3_2D(:,:)
    IF (opt_short_circuit_atm.eqv..true.) THEN
       IF (opt_outgas_eq_Si.eqv..true.) THEN
          loc_force_flux_weather_o_land(io_DIC,:,:) = weather_fCaSiO3_2D(:,:) + weather_fCaCO3_2D(:,:)
       ELSE
          loc_force_flux_weather_o_land(io_DIC,:,:) = landmask(:,:)*par_outgas_CO2/nlandcells + weather_fCaCO3_2D(:,:)
       ENDIF
    ELSE
       IF (opt_outgas_eq_Si.eqv..true.) THEN
          loc_force_flux_weather_a_land(ia_PCO2,:,:) = & 
               & - 1.0*(weather_fCaSiO3_2D(:,:) + weather_fCaCO3_2D(:,:)) !'-' because coming out of atmosphere
       ELSE
          loc_force_flux_weather_a_land(ia_PCO2,:,:) = landmask(:,:)*par_outgas_CO2/nlandcells - &
               & 1.0*(2.0*weather_fCaSiO3_2D(:,:) + weather_fCaCO3_2D(:,:)) !'-' because coming out of atmosphere
       ENDIF
       loc_force_flux_weather_o_land(io_DIC,:,:) = 2.0*weather_fCaSiO3_2D(:,:) + 2.0*weather_fCaCO3_2D(:,:)
    ENDIF

    loc_standard = const_standards(ocn_type(io_DIC_13C))
    IF (opt_short_circuit_atm.eqv..true.) THEN
       IF (opt_outgas_eq_Si.eqv..true.) THEN
          loc_force_flux_weather_o_land(io_DIC_13C,:,:) =  &
               & fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*weather_fCaSiO3_2D(:,:) + &
               & fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3_2D(:,:)
       ELSE
          loc_force_flux_weather_o_land(io_DIC_13C,:,:) =  &
               & fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*landmask(:,:)*par_outgas_CO2/nlandcells + &
               & fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3_2D(:,:)
       ENDIF
    ELSE
       loc_standard = const_standards(atm_type(ia_pCO2_13C ))
       !'-' because coming out of atmosphere
       loc_force_flux_weather_a_land(ia_pCO2_13C,:,:) = &
            fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*landmask(:,:)*par_outgas_CO2/nlandcells - & 
            1.0*(2.0*fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*weather_fCaSiO3_2D(:,:) + &
            fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3_2D(:,:)) 
       IF (opt_outgas_eq_Si.eqv..true.) THEN
          loc_force_flux_weather_a_land(ia_pCO2_13C,:,:) = &
               - 1.0*(fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*weather_fCaSiO3_2D(:,:) + &
               fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3_2D(:,:)) 
       ELSE
          !'-' because coming out of atmosphere
          loc_force_flux_weather_a_land(ia_pCO2_13C,:,:) = &
               fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*landmask(:,:)*par_outgas_CO2/nlandcells - & 
               1.0*(2.0*fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*weather_fCaSiO3_2D(:,:) + &
               fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3_2D(:,:)) 
       ENDIF
       loc_standard = const_standards(ocn_type(io_DIC_13C))
       loc_force_flux_weather_o_land(io_DIC,:,:) = &
            2.0*fun_calc_isotope_fraction(par_outgas_CO2_d13C,loc_standard)*weather_fCaSiO3_2D(:,:) + &
            2.0*fun_calc_isotope_fraction(par_weather_CaCO3_d13C,loc_standard)*weather_fCaCO3_2D(:,:)
    ENDIF
    loc_force_flux_weather_o_land(io_DIC_14C,:,:) = 0.0

    ! route fluxes into the coastal ocean cells (to pass to biogem in coupled model) and save the output to file
    DO k=1,n_ocn
       IF((k.EQ.io_ALK).OR.(k.EQ.io_DIC).OR.(k.EQ.io_Ca).OR.(k.EQ.io_DIC_13C).OR.(k.EQ.io_DIC_14C)) THEN
          CALL sub_coastal_output( loc_force_flux_weather_o_land(k,:,:), &
               runoff_drainto(:,:,:),runoff_detail(:,:), &
               loc_force_flux_weather_o_ocean(k,:,:))
       ENDIF
    END DO

    dum_sfxatm1(ia_PCO2,:,:) =  loc_force_flux_weather_a_land(ia_PCO2,:,:)/(phys_rok(ipr_A,:,:)*conv_yr_s)
    dum_sfxatm1(ia_pCO2_13C,:,:) =  loc_force_flux_weather_a_land(ia_pCO2_13C,:,:)/(phys_rok(ipr_A,:,:)*conv_yr_s)

    dum_sfxrok(:,:,:) = loc_force_flux_weather_o_ocean(:,:,:)
    ! convert from Mol/yr to Mol/sec for passing out
    dum_sfxrok(:,:,:) = dum_sfxrok(:,:,:)/conv_yr_s

    ! Output

    IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN

       outputs = (/sum(landmask(:,:)*loc_SLT(:,:))/nlandcells,maxval(landmask(:,:)*loc_SLT(:,:)),minval(landmask(:,:)*loc_SLT(:,:)), &
            & sum(landmask(:,:)*loc_runoff(:,:))/nlandcells*conv_yr_s,maxval(landmask(:,:)*loc_runoff(:,:))*conv_yr_s,minval(landmask(:,:)*loc_runoff(:,:))*conv_yr_s, &
            & sum(landmask(:,:)*loc_P(:,:))/nlandcells,maxval(landmask(:,:)*loc_P(:,:)),minval(landmask(:,:)*loc_P(:,:)), &
            & sum(landmask(:,:)*loc_CO2(:,:))/nlandcells,maxval(landmask(:,:)*loc_CO2(:,:)),minval(landmask(:,:)*loc_CO2(:,:)), &
            & sum(loc_weather_ratio_CaCO3(:,:)*landmask(:,:))/nlandcells,sum(loc_weather_ratio_CaSiO3(:,:)*landmask(:,:))/nlandcells, &
            & sum(weather_fCaCO3_2D(:,:))/1.0E12,sum(weather_fCaSiO3_2D(:,:))/1.0E12, &
            & sum(loc_force_flux_weather_a_land(ia_PCO2,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_land(io_ALK,:,:))/1.0E12,sum(loc_force_flux_weather_o_land(io_DIC,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_land(io_Ca,:,:))/1.0E12,sum(loc_force_flux_weather_o_land(io_DIC_13C,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_land(io_ALK,:,:))/1.0E12,sum(loc_force_flux_weather_o_land(io_DIC,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_land(io_Ca,:,:))/1.0E12,sum(loc_force_flux_weather_o_land(io_DIC_13C,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_ocean(io_ALK,:,:))/1.0E12,sum(loc_force_flux_weather_o_ocean(io_DIC,:,:))/1.0E12, &
            & sum(loc_force_flux_weather_o_ocean(io_Ca,:,:))/1.0E12,sum(loc_force_flux_weather_o_ocean(io_DIC_13C,:,:))/1.0E12/)

       call sub_output_0d(n_outputs,(/12,21,25/),outputs,output_descriptions,time_series_names)

    ENDIF

    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN

       IF (opt_2d_netcdf_output) THEN
          call rokgem_netcdf(loc_SLT,loc_CO2,loc_runoff,dum_photo,dum_respveg,loc_P,&
               & loc_force_flux_weather_a_land,loc_force_flux_weather_o_land,loc_force_flux_weather_o_ocean)
       ENDIF

       IF (opt_2d_ascii_output) THEN
          DO k=1,n_ocn
             IF ((k.EQ.io_ALK).OR.(k.EQ.io_DIC).OR.(k.EQ.io_Ca).OR.(k.EQ.io_DIC_13C).OR.(k.EQ.io_DIC_14C)) THEN
                CALL sub_save_data_ij( &
                     & TRIM(par_outdir_name)//'spatial_land_'//TRIM(globtracer_names(k))//'_year_'//TRIM(year_text)//'.dat', &
                     & n_i,n_j,loc_force_flux_weather_o_land(k,:,:))                                 
                CALL sub_save_data_ij( &
                     & TRIM(par_outdir_name)//'spatial_ocean_'//TRIM(globtracer_names(k))//'_year_'//TRIM(year_text)//'.dat', &
                     & n_i,n_j,loc_force_flux_weather_o_ocean(k,:,:)) 
             ENDIF
          END DO
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'temperature_year_'//TRIM(year_text)//'.dat', &
               & n_i,n_j,loc_SLT)
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'runoff_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_runoff(:,:)*conv_yr_s) 
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'productivity_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_P(:,:))    
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'CO2_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_CO2(:,:))
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'loc_weather_ratio_CaSiO3_year_'//TRIM(year_text)//'.dat', &
               n_i,n_j,loc_weather_ratio_CaSiO3(:,:))                                         
          CALL sub_save_data_ij(TRIM(par_outdir_name)//'loc_weather_ratio_CaCO3_year_'//TRIM(year_text)//'.dat', &
               n_i,n_j,loc_weather_ratio_CaCO3(:,:))       
       ENDIF

    ENDIF

    ! for outputting calibrate 2D reference files
    ! should really only do at the end to save computation, i.e. export loc_SLT
    IF (opt_calibrate_T_2D) THEN
       ref_T0_2D(:,:) = loc_SLT(:,:)
    ENDIF
    IF (opt_calibrate_R_2D) THEN
       ref_R0_2D(:,:) = loc_runoff(:,:)*conv_yr_s
    ENDIF
    IF (opt_calibrate_P_2D) THEN
       ref_P0_2D(:,:) = loc_P(:,:)
    ENDIF

  END SUBROUTINE sub_2D_weath

  !========================================================================================!

END MODULE rokgem_box
