! File: rokgem_data.f90
!
! Description: data loading, reporting, saving and initialisation subroutines
!
! Uses:
!
! - <rokgem_lib.f90>

MODULE rokgem_data


  USE rokgem_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! LOAD rokgem 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_rokgem()

    ! Subroutine: sub_load_goin_rokgem
    !
    ! Subroutine to read in and report namelist parameters
    !
    ! Uses:
    !
    ! <genie_util>, ONLY: <check_unit>, <check_iostat>


    USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    integer::ios
    ! read data_rokgem file
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file='data_ROKGEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open rokgem initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_rokgem file
    read(UNIT=in,NML=ini_rokgem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read rokgem namelist'
       stop
    else
       close(unit=in,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    ! set and report namelist data
    par_indir_name = trim(par_indir_name)//'/'
    par_outdir_name = trim(par_outdir_name)//'/'
    par_rstdir_name = trim(par_rstdir_name)//'/'
    if (ctrl_debug_init > 0) then
       ! --- RUN CONTROL ------------------------------------------------------------------------------------------------------------ !
       print*,'--- RUN CONTROL ---'
       print*,'Continuing run?                                     : ',ctrl_continuing
       print*,'Simulation start year                               : ',start_year
       ! --- I/O DIRECTORY DEFINITIONS ---------------------------------------------------------------------------------------------- !
       print*,'--- I/O DEFINITIONS ---'
       print*,'(Paleo config) input dir. name                      : ',trim(par_pindir_name)
       print*,'Input dir. name                                     : ',trim(par_indir_name)
       print*,'Output dir. name                                    : ',trim(par_outdir_name)
       print*,'Restart (input) dir. name                           : ',trim(par_rstdir_name)
       print*,'Filename for restart input                          : ',trim(par_infile_name)
       print*,'Filename for restart output                         : ',trim(par_outfile_name)
       print*,'Output to screen                                    : ',opt_screen_output
       print*,'file containing years for 0D output to be generated : ',trim(par_output_years_file_0d)
       print*,'file containing years for 2D output to be generated : ',trim(par_output_years_file_2d)
       print*,'output 2d fields to .dat files                      : ',opt_2d_ascii_output
       print*,'output 2d fields to netcdf                          : ',opt_2d_netcdf_output
       print*,'append data to output files on restart              : ',opt_append_data
       ! --- RIVER ROUTING PARAMETERS ---------------------------------------------------------------------------------------------- !
       print*,'--- RIVER ROUTING PARAMETERS ---'
       print*,'routing scheme to use: '
       print*,'1 = roof routing using k1 file;' 
       print*,'2 = intermediate using detailed map, but roof for'
       print*,'    stuff that ends up on genie land grid (~1/2)'
       print*,'3 = detailed scheme, scaling up coastal ocean flux'
       print*,'    to match total number of genie land cells       : ',routing_scheme
       print*,'file containing basic (roof) river routing to read'
       print*,'    (k1 file)                                       : ',trim(topo)
       print*,'prefix of file containing detailed river routing to '
       print*,'    read (based on detailed topographic data) -'
       print*,'    suffix is grid dimensions (n_i_n_j.dat)         : ',trim(routing)
       print*,'maximum number of ocean cells a single land cell'
       print*,'    routes to                                       : ',max_drain_cells
       !--- WEATHERING PARAMETERS -------------------------------------------------------------------------------------------------- !
       print*,'--- WEATHERING PARAMETERS ---'
       print*,'short circuit atmosphere                            : ',opt_short_circuit_atm   
       print*,'scale (global) weathering with runoff               : ',opt_weather_runoff                   
       print*,'weathering scheme ID string                         : ',trim(par_weathopt)               
       print*,'global CaCO3 weathering scheme ID string            : ',trim(opt_weather_CaCO3)       
       print*,'global CaSiO3 weathering scheme ID string           : ',trim(opt_weather_CaSiO3)
       print*,'CaCO3 weathering - temperature feedback             : ',opt_weather_T_Ca    
       print*,'CaSiO3 weathering - temperature feedback            : ',opt_weather_T_Si   
       print*,'alt CaSiO3 weathering - temperature feedback        : ',opt_weather_Talt_Si
       print*,'explicit runoff feedback (or T dependence)          : ',opt_weather_R_explicit
       print*,'CaCO3 weathering - runoff feedback                  : ',opt_weather_R_Ca
       print*,'CaSiO3 weathering - runoff feedback                 : ',opt_weather_R_Si
       print*,'explicit productivity feedback (or RCO2 dependence) : ',opt_weather_P_explicit
       print*,'CaCO3 weathering - productivity feedback            : ',opt_weather_P_Ca
       print*,'CaSiO3 weathering - productivity feedback           : ',opt_weather_P_Si
       print*,'CaCO3 weathering - CO2 feedback?                    : ',opt_weather_C_Ca
       print*,'CaSiO3 weathering - CO2 feedback?                   : ',opt_weather_C_Si
       print*,'Distinguish between basalt and granite weathering?  : ',opt_weather_C_Si_bg
       print*,'exponent for CaCO3 CO2-weathering feedback          : ',par_nCa
       print*,'exponent for CaSiO3 CO2-weathering feedback         : ',par_nSi
       print*,'prodictivity to use ("GPP" or "NPP")                : ',trim(par_prodopt)
       print*,'constant for temperature-carbonate weath. feedback  : ',par_k_Ca
       print*,'constant for temperature-silicate weath. feedback   : ',par_k_Si
       print*,'Activation energy for silicate weathering (kJ/Mol)  : ',par_E_a
       print*,'Exponent for basalt weathering                      : ',par_k_tb
       print*,'Exponent for granite weathering                     : ',par_k_Tg
       print*,'constant for temperature-runoff linear correlation  : ',par_k_run
       print*,'frac. power of explicit weathering-runoff dependence: ',par_beta
       print*,'weathering ref. mean global land surface temp. (C)  : ',par_ref_T0
       print*,'weathering ref. mean runoff (mm/yr)                 : ',par_ref_R0
       print*,'weathering ref. mean runoff (mm/s)                  : ',par_ref_R0 / conv_yr_s
       print*,'weathering ref. mean global prod. (kgC m-2 yr-1)    : ',par_ref_P0
       print*,'weathering ref. mean global atm pCO2 (ppm)          : ',par_ref_CO20
       print*,'set volcanic outgassing equal to Si weathering      : ',opt_outgas_eq_Si
       print*,'CO2 outgassing rate (mol C yr-1)                    : ',par_outgas_CO2
       print*,'CO2 outgassing rate (mol C yr-1)                    : ',par_outgas_CO2
       print*,'mean volcanic/metamorphic d13C (o/oo)               : ',par_outgas_CO2_d13C
       ! ------------------- GLOBAL AVERAGE WEATHERING PARAMETERS ------------------------------------------------------------------ !
       print*,'--- GLOBAL AVERAGE WEATHERING PARAMETERS ---'
       print*,'global silicate weathering rate (mol Ca2+ yr-1)     : ',par_weather_CaSiO3
       print*,'basaltic silicate weathering rate (mol Ca2+ yr-1)   : ',par_weather_CaSiO3b
       print*,'granitic silicate weathering rate (mol Ca2+ yr-1)   : ',par_weather_CaSiO3g
       print*,'global (Ca silicate) Mg abundance                   : ',par_weather_CaSiO3_fracMg
       print*,'basaltic (Ca silicate) Mg abundance                 : ',par_weather_CaSiO3b_fracMg
       print*,'granitic (Ca silicate) Mg abundance                 : ',par_weather_CaSiO3g_fracMg
       print*,'global (silicate) kerogen abundance                 : ',par_weather_CaSiO3_fracC
       print*,'global (silicate) kerogen d13C (o/oo)               : ',par_weather_CaSiO3_fracC_d13C
       print*,'global (silicate) phosphate abundance               : ',par_weather_CaSiO3_fracP
       print*,'global carbonate weathering rate (mol Ca2+ yr-1)    : ',par_weather_CaCO3
       print*,'mean carbonate d13C (o/oo)                          : ',par_weather_CaCO3_d13C
       print*,'global silicate Si abundance                        : ',par_weather_CaSiO3_fracSi
       print*,'global silicate d30Si (o/oo)                        : ',par_weather_CaSiO3_fracSi_d30Si
       print*,'global silicate pyrite abundance                    : ',par_weather_CaSiO3_fracFeS2
       print*,'global pyrite d34S (o/oo)                           : ',par_weather_CaSiO3_fracFeS2_d34S
       print*,'global pyrite d56Fe (o/oo)                          : ',par_weather_CaSiO3_fracFeS2_d34S
       print*,'global carbonate CaSO4 abundance                    : ',par_weather_CaCO3_fracCaSO4
       print*,'global gypsum d34S (o/oo)                           : ',par_weather_CaCO3_fracCaSO4_d34S
       print*,'global gypsum d44Ca (o/oo)                          : ',par_weather_CaCO3_fracCaSO4_d44Ca
       print*,'global carbonate FeCO3 abundance                    : ',par_weather_CaCO3_fracFeCO3
       print*,'global siderite d56Fe (o/oo)                        : ',par_weather_CaCO3_fracFeCO3_d56Fe
       print*,'global siderite d13C (o/oo)                         : ',par_weather_CaCO3_fracFeCO3_d13C      
       print*,'global silicate Li abundance                        : ',par_weather_CaSiO3_fracLi
       print*,'global silicate Li weathering scaling               : ',par_weather_Li_Rscale
       print*,'global silicate Li weathering offset                : ',par_weather_Li_Roffset
       print*,'global silicate (bulk Earth) d7Li (o/oo)            : ',par_weather_CaSiO3_Li_d7Li
       print*,'secondary clay fractionation (o/oo)                 : ',par_weather_Li_7Li_epsilon
       print*,'global silicate d44Ca (o/oo)                        : ',par_weather_CaSiO3_d44Ca
       print*,'global carbonate d44Ca (o/oo)                       : ',par_weather_CaCO3_d44Ca
       print*,'global silicate Sr bundance                         : ',par_weather_CaSiO3_fracSr
       print*,'basaltic silicate Sr bundance                       : ',par_weather_CaSiO3b_fracSr
       print*,'granitic silicate Sr bundance                       : ',par_weather_CaSiO3g_fracSr
       print*,'global carbonate Sr abundance                       : ',par_weather_CaCO3_fracSr
       print*,'global silicate d87Sr (o/oo)                        : ',par_weather_CaSiO3_r87Sr
       print*,'basaltic silicate d87Sr (o/oo)                      : ',par_weather_CaSiO3b_r87Sr
       print*,'granitic silicate d87Sr (o/oo)                      : ',par_weather_CaSiO3g_r87Sr
       print*,'global carbonate d87Sr (o/oo)                       : ',par_weather_CaCO3_r87Sr
       print*,'global silicate d88Sr (o/oo)                        : ',par_weather_CaSiO3_d88Sr
       print*,'basaltic silicate d88Sr (o/oo)                      : ',par_weather_CaSiO3b_d88Sr
       print*,'granitic silicate d88Sr (o/oo)                      : ',par_weather_CaSiO3g_d88Sr
       print*,'global carbonate d88r (o/oo)                        : ',par_weather_CaCO3_d88Sr    
       print*,'global silicate apatite abundance                   : ',par_weather_CaSiO3_fracCa5PO43
       print*,'global apatite d44Ca (o/oo)                         : ',par_weather_CaSiO3_fracCa5PO43_d44Ca
       print*,'global apatite weathering rate (mol PO4 yr-1)       : ',par_weather_Ca0PO41
       print*,'global quartz weathering rate (mol Si yr-1)         : ',par_weather_SiO2
       print*,'global quartz d30Si (o/oo)                          : ',par_weather_SiO2_d30Si
       print*,'calibrate temperature fields to global average data : ',opt_calibrate_T_0D
       print*,'calibrate runoff fields to global average data      : ',opt_calibrate_R_0D
       print*,'calibrate productivity fields to global average data: ',opt_calibrate_P_0D
       print*,'mean global land surface temp (C) to calibrate to   : ',par_data_T_0D
       print*,'mean global runoff (mm/yr) to calibrate to          : ',par_data_R_0D
       print*,'mean global runoff (mm/s) to calibrate to           : ',par_data_R_0D / conv_yr_s
       print*,'mean global land prod. (kgC m-2 yr-1) to calib. to  : ',par_data_P_0D
       print*,'imposed maximum carbonate weathering enhancement    : ',par_n_max_CaCO3
       print*,'imposed maximum silicate weathering enhancement     : ',par_n_max_CaSiO3
       print*,'enhanced weathering scale factor                    : ',par_weather_fCaCO3_enh_n
       print*,'enhanced weathering scale factor                    : ',par_weather_fCaSiO3_enh_n
       print*,'enhanced weathering total inventory                 : ',par_weather_fCaCO3_enh_nt
       print*,'enhanced weathering total inventory                 : ',par_weather_fCaSiO3_enh_nt
       ! ------------------- 2D WEATHERING PARAMETERS --------------------------------------------------------------------------------!
       print*,'--- 2D WEATHERING PARAMETERS ---'
       print*,'name of lithological data set (part 1)              : ',par_lith_data
       print*,'name of lithological data set (part 2)              : ',par_lith_data2
       print*,'name of lithological data set (part 3)              : ',par_lith_data3
       print*,'truncate lithological maps to genie land-mask       : ',truncate_to_land
       print*,'scale lithological maps to genie land-mask area     : ',scale_to_landarea
       print*,'calibrate 2D weathering - if .true. use values below: ',calibrate_weath
       print*,'calibration value for CaCO3 weath in GKWM scheme    : ',calibrate_weather_GKWM_CaCO3
       print*,'calibration value for CaCO3 weath in GEM_CO2 scheme : ',calibrate_weather_GEM_CO2_CaCO3
       print*,'calibration value for CaSiO3 weath in GKWM scheme   : ',calibrate_weather_GKWM_CaSiO3
       print*,'calibration value for CaSiO3 weath in GEM_CO2 scheme: ',calibrate_weather_GEM_CO2_CaSiO3
       print*,'calibrate temperature fields to data                : ',opt_calibrate_T_2D
       print*,'calibrate runoff fields to data                     : ',opt_calibrate_R_2D
       print*,'calibrate productivity fields to data               : ',opt_calibrate_P_2D 
       print*,'land surface temp (C) reference scaling field       : ',par_ref_T0_2D
       print*,'land surface runoff (mm/yr) reference scaling field : ',par_ref_R0_2D
       print*,'land surface prod (kgC m-2 yr-1) ref. scaling field : ',par_ref_P0_2D
       print*,'land surface temperature (C) calibration datafile   : ',par_data_T_2D
       print*,'runoff (mm/s) calibration datafile                  : ',par_data_R_2D
       print*,'land productivity (kgC m-2 yr-1) calib. datafile    : ',par_data_P_2D
       print*,'separate kinetic and transport limited regimes?     : ',opt_weath_regimes
       print*,'orogeny landmask file                               : ',weath_regimes
       ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ############################################################################# !
       !
       ! ############################################################################################################################ !
       print*,'======================================================='
    end if

    ! *** adjust units ***
    ! convert par_weather_R0 to mm/s
    par_ref_R0 = par_ref_R0 / conv_yr_s
    ! convert par_data_R0 to mm/s
    par_data_R_0D = par_data_R_0D / conv_yr_s
    
  END SUBROUTINE sub_load_goin_rokgem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Subroutine: sub_load_rokgem_restart
  !
  ! Subroutine to read in restart info - just the netcdf record number for appending data at the moment

  SUBROUTINE sub_load_rokgem_restart()
    IMPLICIT NONE
    ! local variables
    integer::ios                                    ! local counting variables
    CHARACTER(len=255)::loc_filename                ! filename string

    ! retrieve restart data
    loc_filename = TRIM(par_rstdir_name)//trim(par_infile_name)
    OPEN(unit=in,status='old',file=loc_filename,form='formatted',action='read',iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
    READ(unit=in,fmt='(i6)') ncout2d_ntrec_rg                             
    close(unit=in)
  end SUBROUTINE sub_load_rokgem_restart
  ! ****************************************************************************************************************************** !

  ! ======= GET LAND MASK AND NUMBER OF LAND CELLS  =======================================!

  ! Subroutine: sub_land
  !
  ! Subroutine for working out a landmask from the input *.k1 file
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
  ! dum_landmask - this is an array with 1s for land and 0s for ocean

  SUBROUTINE sub_land(dum_drainage,dum_landmask)

    REAL, INTENT(in)                :: dum_drainage(n_i+2,n_j+2)            ! this is the *.k1 file
    INTEGER, INTENT(inout)          :: dum_landmask(n_i,n_j)              

    INTEGER                         :: i, j

    ! calc land mask (1 for land, 0 for ocean)
    DO i=1,n_i
       DO j=1,n_j
          IF (dum_drainage(i+1,j+1).GT.90) THEN
             dum_landmask(i,j) = 1
          ELSE
             dum_landmask(i,j) = 0
          ENDIF
       END DO
    END DO

    ! Save data to file landmask.dat
    if (ctrl_debug_init > 1) PRINT*,'Saving landmask (1 for land, 0 for ocean)'
    CALL sub_save_data_ij(TRIM(par_outdir_name)//'landmask.dat',n_i,n_j,REAL(dum_landmask(:,:)))       ! from gem_util

    ! calculate number of land cells
    nlandcells = SUM(dum_landmask(:,:))

  END SUBROUTINE sub_land



  ! ****************************************************************************************************************************** !
  ! Subroutine: sub_init_phys_rok 
  !
  ! Initialises the weathering array

  SUBROUTINE sub_init_phys_rok()
    ! local variables
    INTEGER::i,j
    real::loc_th0,loc_th1,loc_s0,loc_s1,loc_ds
    real,dimension(0:n_j)::loc_s,loc_sv
    ! zero array
    phys_rok(:,:,:) = 0.0
    ! calculate local constants
    loc_th0 = -const_pi/2 
    loc_th1 = const_pi/2 
    loc_s0 = sin(loc_th0) ! =1   
    loc_s1 = sin(loc_th1) ! =-1
    loc_ds = (loc_s1-loc_s0)/real(n_j) ! = -1/18 [for 36x36 grid]
    DO j=0,n_j
       loc_sv(j) = loc_s0 + real(j)*loc_ds ! array going from 1 to -1 in steps of 1/18 [for 36x36 grid]
       loc_s(j) = loc_sv(j) - 0.5*loc_ds ! same array, offset by -1/36 [for 36x36 grid]
    end do
    ! initialize array values
    DO i=1,n_i
       DO j=1,n_j
          phys_rok(ipr_lat,i,j)  = (180.0/const_pi)*ASIN(loc_s(j)) ! lattitudes (degrees; +=N, -=S) of mid-points of grid cells
          phys_rok(ipr_latn,i,j)  = (180.0/const_pi)*ASIN(loc_sv(j)) ! lattitudes (degrees) of the northerly edges of grid cells
          phys_rok(ipr_lon,i,j)  = (360.0/real(n_i))*(real(i)-0.5) + par_grid_lon_offset ! longitudes (degrees) of mid-points of grid cells
          phys_rok(ipr_lone,i,j)  = (360.0/real(n_i))*(real(i)) + par_grid_lon_offset ! longitudes (degrees) of easterly edges of grid cells
          phys_rok(ipr_dlat,i,j) = (180.0/const_pi)*(ASIN(loc_sv(j)) - ASIN(loc_sv(j-1))) ! heights of grid cells in degrees latitude
          phys_rok(ipr_dlon,i,j) = (360.0/real(n_i)) ! widths of grid cells in degreees longitude
          phys_rok(ipr_A,i,j)    = 2.0*const_pi*(const_rEarth**2)*(1.0/real(n_i))*(loc_sv(j) - loc_sv(j-1)) ! areas of grid cells
          phys_rok(ipr_rA,i,j)   = 1.0/phys_rok(ipr_A,i,j) ! reciprocals of areas
       END DO
    END DO
  END SUBROUTINE sub_init_phys_rok
  ! ****************************************************************************************************************************** !

  ! taken from biogem_data (where is is sub_init_phys_ocnatm):
  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'PHYSICS' - OCEAN-ROKGEM INTERFACE
  SUBROUTINE sub_init_phys_ocnrok()
    ! local variables
    INTEGER::i,j
    !    CHARACTER(len=255)::loc_filename
    ! zero array
    !   phys_ocnrok(:,:,:) = 0.0
    ! initialize array values
    DO i=1,n_i
       DO j=1,n_j
          !          phys_ocnrok(ipor_lat,i,j)  = (180.0/const_pi)*ASIN(goldstein_s(j))
          !          phys_ocnrok(ipor_lon,i,j)  = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
          !          phys_ocnrok(ipor_dlat,i,j) = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
          !          phys_ocnrok(ipor_dlon,i,j) = (360.0/n_i)
          !          phys_ocnrok(ipor_A,i,j)    = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
          !          phys_ocnrok(ipor_rA,i,j)   = 1.0/ phys_ocnrok(ipor_A,i,j)
          !          IF (n_k >= goldstein_k1(i,j)) THEN
          !             phys_ocnrok(ipor_seaice,i,j) = 0.0
          !             phys_ocnrok(ipor_u,i,j)      = 0.0
          !             phys_ocnrok(ipor_mask_ocn,i,j) = 1.0
          !          END IF
       END DO
    END DO
    ! load prescribed sea-ice cover (if requested)
    ! NOTE: convert from %cover to fractional cover
    !    if (ctrl_force_seaice) then
    !       loc_filename = TRIM(par_indir_name)//'biogem_force_seaice'//TRIM(string_data_ext)
    !       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_seaice(:,:))
    !       par_phys_seaice(:,:) = par_phys_seaice(:,:)/100.0
    !    end if
    ! load prescribed wind-speed (if requested)
    ! NOTE: (m s-1)
    !    if (ctrl_force_windspeed) then
    !       loc_filename = TRIM(par_indir_name)//'biogem_force_windspeed'//TRIM(string_data_ext)
    !       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_windspeed(:,:))
    !    end if
  END SUBROUTINE sub_init_phys_ocnrok
  ! ****************************************************************************************************************************** !     

  !======= SUBROUTINE TO READ IN OUTPUT YEARS ========================================================!

  ! Subroutine: sub_data_output_years
  !
  ! Reads in years to output data from file.
  !
  ! Uses:
  !
  ! <genie_util>, ONLY: <check_unit>, <check_iostat>

  SUBROUTINE sub_data_output_years()

    USE genie_util, ONLY: check_unit, check_iostat

    IMPLICIT NONE

    !local variables
    INTEGER:: i, n_years, n_output_years, ios, alloc_stat
    REAL:: year

    ! For 0d
    call check_unit(in,__LINE__,__FILE__)
    open(in,file=TRIM(par_indir_name)//TRIM(par_output_years_file_0d),action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    n_years = 0
    n_output_years=0
    DO
       READ(in,*,iostat=ios) year
       !call check_iostat(ios,__LINE__,__FILE__)
       IF (ios.lt.0) EXIT
       n_years = n_years + 1
       IF (year.gt.start_year) THEN
          n_output_years = n_output_years + 1
       ENDIF
    END DO

    if (ctrl_debug_init > 1) PRINT*,'number of output years (left) in '//TRIM(par_output_years_file_0d)//': ',n_output_years
    rewind(unit=in)

    ALLOCATE(output_years_0d(n_output_years),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    ALLOCATE(output_tsteps_0d(n_output_years),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)

    i = 1
    DO
       READ(in,*,iostat=ios) year
       !call check_iostat(ios,__LINE__,__FILE__)
       IF (ios.lt.0) EXIT
       IF (year.gt.start_year) THEN
          output_years_0d(i) = year
          i = i + 1
       ENDIF
    END DO
    close(unit=in)

    !PRINT*,'output years_0d:'
    !write(6,fmt='(f14.1)'),output_years_0d
    output_tsteps_0d = int(tsteps_per_year*(output_years_0d-start_year))
    output_counter_0d = 1

    ! For 2d
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file=TRIM(par_indir_name)//TRIM(par_output_years_file_2d),action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    n_years = 0
    n_output_years=0
    DO
       READ(in,*,iostat=ios) year
       !call check_iostat(ios,__LINE__,__FILE__)
       IF (ios.lt.0) EXIT
       n_years = n_years + 1
       IF (year.gt.start_year) THEN
          n_output_years = n_output_years + 1
       ENDIF
    END DO

    if (ctrl_debug_init > 1) PRINT*,'number of output years (left) in '//TRIM(par_output_years_file_2d)//': ',n_output_years
    rewind(in)

    ALLOCATE(output_years_2d(n_output_years),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    ALLOCATE(output_tsteps_2d(n_output_years),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)

    i = 1
    DO
       READ(in,*,iostat=ios) year
       !call check_iostat(ios,__LINE__,__FILE__)
       IF (ios.lt.0) EXIT
       IF (year.gt.start_year) THEN
          output_years_2d(i) = year
          i = i + 1
       ENDIF
    END DO
    close(in)

    !PRINT*,'output years_2d:'
    !write(6,fmt='(f14.1)'),output_years_2d
    output_tsteps_2d = int(tsteps_per_year*(output_years_2d-start_year))
    output_counter_2d = 1

  END SUBROUTINE sub_data_output_years


  !======= SUBROUTINE TO CHANGE OUTPUT YEAR  ==================================================!

  ! Subroutine: sub_output_year
  !
  ! year is read from list of output years depending on whether 0D or 2D output is due

  SUBROUTINE sub_output_year()

    IMPLICIT NONE

    IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN
       year = output_years_0d(output_counter_0d)
    ENDIF

    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN 
       year = output_years_2d(output_counter_2d)
    ENDIF

    !print*,tstep_count,output_counter_0d,output_counter_2d,year

    year_int = int(year)
    year_remainder = int(1000*(year - real(year_int)))
    year_text = fun_conv_num_char_n(8,year_int)//'_'//fun_conv_num_char_n(3,year_remainder)

  END SUBROUTINE sub_output_year

  !======= SUBROUTINE TO INCREMENT OUTPUT COUNTERS  ==================================================!

  ! Subroutine: sub_output_counters
  !
  ! output_counters go up by 1 after each output

  SUBROUTINE sub_output_counters()

    IMPLICIT NONE

    IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN
       output_counter_0d = output_counter_0d + 1
    ENDIF

    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN 
       output_counter_2d = output_counter_2d + 1 
    ENDIF

  END SUBROUTINE sub_output_counters

  !======= SUBROUTINE TO INITIALISE DATA OUTPUT  ====================================================!

  ! Subroutine: sub_ini_output
  !
  ! Set strings for outputting to file

  SUBROUTINE sub_ini_output()

    USE genie_util, ONLY: check_unit, check_iostat

    IMPLICIT NONE

    INTEGER                                 :: alloc_stat, i, ios

    n_outputs = 29
    ALLOCATE(time_series_names(n_outputs),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    ALLOCATE(output_descriptions(n_outputs),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)

    time_series_names =   (/'SLT_av                                            ', &
         & 'SLT_max                                           ', &
         & 'SLT_min                                           ', &
         & 'R_av                                              ', &
         & 'R_max                                             ', &
         & 'R_min                                             ', &
         & 'P_av                                              ', &
         & 'P_max                                             ', &
         & 'P_min                                             ', &
         & 'CO2_av                                            ', &
         & 'CO2_max                                           ', &
         & 'CO2_min                                           ', &
         & 'weather_ratio_CaCO3                               ', &
         & 'weather_ratio_CaSiO3                              ', &
         & 'weather_fCaCO3                                    ', &
         & 'weather_fCaSiO3                                   ', &
         & 'CO2_flux_coast                                    ', &
         & 'ALK_flux_coast                                    ', &
         & 'DIC_flux_coast                                    ', &
         & 'Ca_flux_coast                                     ', &
         & 'DIC_13C_flux                                      ', &
         & 'ALK_flux_land                                     ', &
         & 'DIC_flux_land                                     ', &
         & 'Ca_flux_land                                      ', &
         & 'DIC_13C_flux_land                                 ', &
         & 'ALK_flux_ocean                                    ', &
         & 'DIC_flux_ocean                                    ', &
         & 'Ca_flux_ocean                                     ', &
         & 'DIC_13C_flux_ocean                                ' /)

    output_descriptions = (/                                                       &
                                !'---------------------------- inputs -----------------------------'
         & 'global average land surface temperature (deg C)  ', &
         & 'max land surface temperature (deg C)             ', &
         & 'min land surface temperature (deg C)             ', &
         & 'global average land surface runoff (mm/yr)       ', &
         & 'max land surface runoff (mm/yr)                  ', &
         & 'min land surface runoff (mm/yr)                  ', &
         & 'global av land surf productivity (kgC m-2 yr-1)  ', &
         & 'max land surface productivity (kgC m-2 yr-1)     ', &
         & 'min land surface productivity (kgC m-2 yr-1)     ', &
         & 'global av atmospheric pCO2 (ppm)                 ', &
         & 'max atmospheric pCO2 (ppm)                       ', &
         & 'min atmospheric pCO2 (ppm)                       ', &
                                !'---------------------------- outputs -----------------------------'
         & 'loc_weather_ratio_CaCO3                          ', &
         & 'loc_weather_ratio_CaSiO3                         ', &
         & 'weather_fCaCO3 (Tmol yr-1)                       ', &
         & 'weather_fCaSiO3 (Tmol yr-1)                      ', &
         & 'CO2 weathering flux (Tmol yr-1)                  ', & ! taken out of atmosphere - is 0 if short-circuiting is off
         & 'ALK weathering flux (Tmol yr-1)                  ', & ! note: these fluxes are spread onto land in 'Globavg',
         & 'DIC weathering flux (Tmol yr-1)                  ', & ! but as they are already spread over land for 2D schemes
         & 'Ca weathering flux (Tmol yr-1)                   ', & ! they are just set to the exact same fluxes as the ones below
         & 'DIC_13C weathering flux (Tmol yr-1)              ', & !
                                !'                            * land *                              '
         & 'ALK weathering flux (Tmol yr-1)                  ', &
         & 'DIC weathering flux (Tmol yr-1)                  ', &
         & 'Ca weathering flux (Tmol yr-1)                   ', &
         & 'DIC_13C weathering flux (Tmol yr-1)              ', &
                                ! '                            * ocean *                            '
         & 'ALK weathering flux (Tmol yr-1)                  ', &
         & 'DIC weathering flux (Tmol yr-1)                  ', &
         & 'Ca weathering flux (Tmol yr-1)                   ', &
         & 'DIC_13C weathering flux (Tmol yr-1)              ' /)

    ALLOCATE(outputs(n_outputs),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)

    DO i=1,n_outputs    
       IF ((output_counter_0d.eq.1).AND.(((opt_append_data.eqv..FALSE.).OR.(ctrl_continuing.eqv..FALSE.)))) THEN
          call check_unit(20,__LINE__,__FILE__)
          open(20,file=TRIM(par_outdir_name)//'rokgem_series_'//TRIM(time_series_names(i))//string_results_ext, &
               & action='write',status='replace',form='formatted',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(20,fmt='(A16,A52)',iostat=ios)TRIM('%     year    / '),output_descriptions(i)
          call check_iostat(ios,__LINE__,__FILE__) 
          close(20,iostat=ios)  
          call check_iostat(ios,__LINE__,__FILE__)
       END IF
    END DO

  END SUBROUTINE sub_ini_output

  !======= SUBROUTINE TO READ IN LITHOLOGIES ========================================================!

  ! Subroutine: sub_data_input_3D 
  !
  ! Reads a set of files (each containing the fraction of land in each grid cell that is 
  ! a particular lithology) from the data input directory into a single array.
  !
  ! Uses:
  !
  ! <genie_util>, ONLY: <check_unit>, <check_iostat>
  !
  ! Calls:
  !
  ! - <sub_load_data_ij>
  ! - <sub_save_data_ij>
  !
  ! Input:
  !
  ! dum_nfiles - number of files to read in (equal to the namelist parameter par_nliths)
  ! dum_input_dir - the input directory
  ! dum_filenames_file - the file containing the list of filenames to read in
  ! dum_filenames - array containing list of filenames
  ! dum_i, dum_j - dimensions of the lithology arrays
  !
  ! Output:
  !
  ! dum_array_3D - array containing proportions of each rock type in each grid cell

  SUBROUTINE sub_data_input_3D            (                                                 &
       & dum_input_dir,                                  &
       & dum_filenames_file,                             &
       & dum_filenames,                                  &
       & dum_nfiles,                                     &
       & dum_i,dum_j,                                    &
       & dum_array_3D                                    )

    USE genie_util, ONLY: check_unit, check_iostat

    ! dummy variables
    IMPLICIT NONE
    INTEGER, INTENT(in)                    :: dum_nfiles
    CHARACTER(LEN=*), INTENT(in)           :: dum_input_dir
    CHARACTER(LEN=*), INTENT(in)           :: dum_filenames_file
    CHARACTER(LEN=50), INTENT(inout)          :: dum_filenames(dum_nfiles)
    INTEGER, INTENT(in)                    :: dum_i, dum_j
    REAL, INTENT(inout)                    :: dum_array_3D(dum_nfiles,dum_i,dum_j)

    ! local variables
    INTEGER                                :: i, j, k, ios, n
    !REAL                                   :: loc_array_3D_offset(dum_nfiles,dum_i,dum_j)
    REAL                                   :: fracs(dum_nfiles)
    REAL                                   :: scaling

    if (ctrl_debug_init > 1) print*,'reading in lithological data'
    call check_unit(in,__LINE__,__FILE__)
    OPEN(in,file=dum_filenames_file,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (ctrl_debug_init > 1) print*,dum_filenames_file,' being read in'
    read(in,*,iostat=ios)(dum_filenames(k),k=1,dum_nfiles)
    call check_iostat(ios,__LINE__,__FILE__)
    CLOSE(in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (ctrl_debug_init > 1) print*,'lithologies: '
    if (ctrl_debug_init > 1) print*,'file name | land cells occupied | land cells covered | fractional cover'

    DO k=1,dum_nfiles
       CALL sub_load_data_ij(TRIM(dum_input_dir)//TRIM(dum_filenames(k)),                    &
            & dum_i,dum_j,dum_array_3D(k,:,:)) 
    END DO

    DO k=1,dum_nfiles
       ! count number of land cells in each lithological map   
       n = 0
       DO i=1,n_i
          DO j=1,n_j
             IF (dum_array_3D(k,i,j).GT.0) THEN
                n = n + 1
             ENDIF
          END DO
       END DO
       ! calculate fractional coverage
       fracs(k) = SUM(dum_array_3D(k,:,:))/nlandcells
       PRINT*,TRIM(dum_filenames(k)),' | ',n,' | ',SUM(dum_array_3D(k,:,:)),' | ',fracs(k)
    END DO
    if (ctrl_debug_init > 1) print*,'total coverage (should be 1): ',SUM(fracs(:))
    if (ctrl_debug_init > 1) print*,'total land cells covered: ',SUM(dum_array_3D(:,:,:))

    ! if option is set, truncate to genie landmask
    ! note: if option isn't set, flux from land in the genie ocean is placed directly into the ocean at that point.
    IF (truncate_to_land) THEN
       if (ctrl_debug_init > 1) print*,'truncating lithological maps to genie landmask'
       DO k=1,dum_nfiles
          DO i=1,n_i
             DO j=1,n_j
                dum_array_3D(k,i,j) = dum_array_3D(k,i,j) * landmask(i,j)
             END DO
          END DO
       END DO
       if (ctrl_debug_init > 1) print*,'total land cells covered: ',SUM(dum_array_3D(:,:,:))
       scaling = REAL(nlandcells) / SUM(dum_array_3D(:,:,:))
    ELSE
       scaling = 1
    END IF
    ! take into account no Antarctica for river-routing scheme 3.
    IF ( routing_scheme.eq.3 ) THEN
       if (ctrl_debug_init > 1) print*,'Using river-routing scheme 3, therefore Antarctica not included (-38 land cells)'
       scaling = REAL(nlandcells-ncells_antarctica) / SUM(dum_array_3D(:,:,:))
    ENDIF
    IF (scale_to_landarea) THEN
       if (ctrl_debug_init > 1) print*,'scaling up to land surface area'
       dum_array_3D(:,:,:) = dum_array_3D(:,:,:) * scaling
       if (ctrl_debug_init > 1) print*,'total land cells covered: ',SUM(dum_array_3D(:,:,:))
    ELSE 
       if (ctrl_debug_init > 1) print*,'not scaling up to land surface area'
    ENDIF

    !save modified lithological maps
    PRINT*,'saving modified lithological maps'
    DO k=1,dum_nfiles
       CALL sub_save_data_ij(TRIM(par_outdir_name)//TRIM(dum_filenames(k)),n_i,n_j,dum_array_3D(k,:,:))
    END DO

  END SUBROUTINE sub_data_input_3D


  !======= SUBROUTINE TO READ IN DATA FOR 2D WEATHERING SCHEME ======================================!
  !
  ! Subroutine: sub_weath
  !
  ! Subroutine to load in constants for weathering scheme, and then call <sub_data_input_3D> to read in lithological data
  !
  ! Uses:
  !
  ! <genie_util>, ONLY: <check_unit>, <check_iostat>
  !
  ! Calls:
  !
  ! - <sub_data_input_3D>
  !
  ! Input/Output:
  !
  ! dum_lithology_names(par_nliths) - array containing list of lithology filenames
  ! dum_lithology(par_nliths,n_i,n_j) - array containing proportions of each rock type in each grid cell

  SUBROUTINE sub_load_weath         ( dum_lithology_names,dum_lithology )

    USE genie_util, ONLY: check_unit, check_iostat

    INTEGER                                :: i, j, ios
    CHARACTER(LEN=50), INTENT(inout)       :: dum_lithology_names(par_nliths)
    REAL, INTENT(inout)                    :: dum_lithology(par_nliths,n_i,n_j)

    ! read in k and f constants and fCa and fSi fractions

    if (ctrl_debug_init > 1) print*,'Reading in weathering constants'
    call check_unit(in,__LINE__,__FILE__)
    OPEN(in,file=TRIM(par_indir_name)//TRIM(par_weathopt)//'_consts.dat',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    DO i = 1,par_nliths
       read(in,*,iostat=ios)(weath_consts(i,j),j=1,4)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    CLOSE(in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    write(6,fmt='(f14.3)') weath_consts


    ! read in lithological data

    CALL sub_data_input_3D                (                                                 &
         & TRIM(par_indir_name)//'lithologies'//'_'//      &
         & TRIM(par_lith_data)//                           &
         & TRIM(par_lith_data2)//                          &
         & TRIM(par_lith_data3)//'_'//                     &
         & fun_conv_num_char_n(3,n_i)//'_'//fun_conv_num_char_n(3,n_j)//'/', &
         & TRIM(par_indir_name)//TRIM(par_weathopt)//'_lithologies.txt',     &
         & dum_lithology_names,                            &
         & par_nliths,                                     &
         & n_i,n_j,                                        &
         & dum_lithology                                   )

  END SUBROUTINE sub_load_weath


  !======= SUBROUTINE TO OUTPUT DATA ================================================================!
  !
  ! Subroutine: sub_output
  !
  ! Outputs global data to screen and file at specified years, and outputs time series
  !
  ! Uses:
  !
  ! <genie_util>, ONLY: <check_unit>, <check_iostat>
  ! 
  ! Input:
  !
  ! dum_n_outputs
  ! dum_outputs
  ! dum_output_divisions
  ! dum_output_descriptions
  ! dum_time_series_names

  SUBROUTINE sub_output_0d(dum_n_outputs,dum_output_divisions,dum_outputs,dum_output_descriptions,dum_time_series_names)

    USE genie_util, ONLY: check_unit, check_iostat

    ! dummy variables
    INTEGER, INTENT(in)             :: dum_n_outputs
    INTEGER, INTENT(in)             :: dum_output_divisions(3)
    REAL, INTENT(in)                :: dum_outputs(dum_n_outputs)
    CHARACTER(LEN=50), INTENT(in)   :: dum_output_descriptions(dum_n_outputs)
    CHARACTER(LEN=50), INTENT(in)   :: dum_time_series_names(dum_n_outputs)

    ! local variable
    INTEGER                         :: i, j, k, l, ios, out(2)
    CHARACTER(LEN=6)                :: loc_num_format      

    ! time series
    DO i=1,dum_n_outputs    
       loc_num_format = 'f14.6'

       call check_unit(20,__LINE__,__FILE__)
       open(20,file=TRIM(par_outdir_name)//'rokgem_series_'//TRIM(dum_time_series_names(i))//string_results_ext, &
            & action='write',POSITION='APPEND',form='formatted',iostat=ios)
       write(20,fmt='(f14.6,'//TRIM(loc_num_format)//')',iostat=ios) year,dum_outputs(i)
       call check_iostat(ios,__LINE__,__FILE__)
       close(20,iostat=ios)  
       call check_iostat(ios,__LINE__,__FILE__) 
    END DO

    !global data report for each time

    call check_unit(20,__LINE__,__FILE__)
    open(20,file=TRIM(par_outdir_name)//'RokGeM_DATA_GLOBAL_year_'//TRIM(year_text)//string_results_ext &
         & ,status='replace',form='formatted',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    out = (/6,20/) !6 = screen; 20 = file

    IF (opt_screen_output) THEN
       l = 1
    ELSE
       l = 2
    ENDIF

    DO j=l,2
       write(out(j),*) ''
       write(out(j),fmt='(A38,f14.1,A13)') '=========== Global Weathering @ year ',year,' ==========='
       write(out(j),*) ''
       write(out(j),*) '--------------------------- inputs -----------------------------'
       write(out(j),*) ''
       DO k = 1,dum_output_divisions(1)
          write(out(j),fmt='(A50,A1,f14.6)') dum_output_descriptions(k),':',dum_outputs(k)
       END DO
       write(out(j),*) ''
       write(out(j),*) '--------------------------- outputs ----------------------------'
       write(out(j),*) ''
       DO k = dum_output_divisions(1)+1,dum_output_divisions(2)
          write(out(j),fmt='(A50,A1,f14.6)') dum_output_descriptions(k),':',dum_outputs(k)
       END DO
       write(out(j),*) ''
       write(out(j),*) '                           * land * '
       DO k = dum_output_divisions(2)+1,dum_output_divisions(3)
          write(out(j),fmt='(A50,A1,f14.6)') dum_output_descriptions(k),':',dum_outputs(k)
       END DO
       write(out(j),*) '                           * ocean * '
       DO k = dum_output_divisions(3)+1,dum_n_outputs
          write(out(j),fmt='(A50,A1,f14.6)') dum_output_descriptions(k),':',dum_outputs(k)
       END DO
       write(out(j),*) ''
       write(out(j),*) ''
    END DO
    close(20) 

  END SUBROUTINE sub_output_0d


END MODULE rokgem_data
