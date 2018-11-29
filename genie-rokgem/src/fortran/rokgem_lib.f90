!
! File: rokgem_lib.f90
! 
! Description: this is the library module contains all the variable allocations for RokGeM
!
! Uses:
!
! - <genie_control>
! - <gem_cmn>
! - <gem_util>
! - <gem_carbchem>

MODULE rokgem_lib


  use genie_control
  use gem_cmn
  use gem_util
  use gem_carbchem
  IMPLICIT NONE
  SAVE

  ! ****************************************************************************************************************************** !
  ! *** NAMELIST DEFINITIONS ***************************************************************************************************** !
  ! ****************************************************************************************************************************** !


  ! ### EDIT ADD AND/OR EXTEND NAME-LIST PARAMETER AND CONTROL OPTIONS ########################################################### !
  ! ------------------- RUN CONTROL ---------------------------------------------------------------------------------------------- !
  logical::ctrl_continuing                                                               ! continuing run?
  NAMELIST /ini_rokgem_nml/ctrl_continuing
  REAL::start_year                                                                       ! Simulation start year [real]
  NAMELIST /ini_rokgem_nml/start_year
  ! ------------------- I/O DEFINITIONS ------------------------------------------------------------------------------------------ !
  CHARACTER(len=255)::par_indir_name,par_outdir_name,par_rstdir_name,par_pindir_name                     ! 
  NAMELIST /ini_rokgem_nml/par_indir_name,par_outdir_name,par_rstdir_name,par_pindir_name
  CHARACTER(len=63)::par_infile_name,par_outfile_name                                    ! 
  NAMELIST /ini_rokgem_nml/par_infile_name,par_outfile_name
  logical::opt_screen_output                                                             ! output to screen
  NAMELIST /ini_rokgem_nml/opt_screen_output
  CHARACTER(len=63)::par_output_years_file_0d                                            ! file containing years for 0D output to be generated (to screen and file)
  CHARACTER(len=63)::par_output_years_file_2d                                            ! file containing years for 2D output to be generated (to file)  
  NAMELIST /ini_rokgem_nml/par_output_years_file_0d,par_output_years_file_2d
  logical::opt_2d_ascii_output                                                           ! output 2d fields to .dat files
  logical::opt_2d_netcdf_output                                                          ! output 2d fields to netcdf
  logical::opt_append_data                                                               ! append data to output files on restart
  NAMELIST /ini_rokgem_nml/opt_2d_ascii_output,opt_2d_netcdf_output,opt_append_data
  ! --- RIVER ROUTING PARAMETERS ------------------------------------------------------------------------------------------------- !
  INTEGER::routing_scheme                                                                ! routing scheme to use: 1 = 'roof' routing using k1 file; 
  NAMELIST /ini_rokgem_nml/routing_scheme                                                ! 2 = intermediate using detailed map, but roof for stuff that ends up on genie land grid (about half)
                                                                                         ! 3 = detailed scheme, scaling up coastal ocean flux to match total number of genie land cells
  CHARACTER(len=63)::topo                                                                ! file containing basic (roof) river routing to read (k1 file)
  NAMELIST /ini_rokgem_nml/topo
  CHARACTER(len=63)::routing                                                             ! prefix of file containing detailed river routing to read (based on detailed topographic data)
                                                                                         ! - suffix is grid dimensions (n_i_n_j.dat)
  NAMELIST /ini_rokgem_nml/routing
  INTEGER::max_drain_cells                                                               ! maximum number of ocean cells a single land cell routes to
  NAMELIST /ini_rokgem_nml/max_drain_cells
  !--- WEATHERING PARAMETERS ----------------------------------------------------------------------------------------------------- !
  LOGICAL:: opt_short_circuit_atm                                                        ! short circuit atmosphere by not taking CO2 directly, instead have less DIC put into ocean.
  NAMELIST /ini_rokgem_nml/opt_short_circuit_atm
  LOGICAL:: opt_weather_runoff                                                           ! scale (global) weathering with runoff
  NAMELIST /ini_rokgem_nml/opt_weather_runoff
  CHARACTER(len=63)::par_weathopt                                                        ! weathering scheme ID string ('Global_avg','GKWM',or 'GEM_CO2')
  NAMELIST /ini_rokgem_nml/par_weathopt
  CHARACTER(len=63)::opt_weather_CaCO3                                                   ! global CaCO3 weathering scheme ID
  CHARACTER(len=63)::opt_weather_CaSiO3                                                  ! global CaSIO3 weathering scheme ID
  NAMELIST /ini_rokgem_nml/opt_weather_CaCO3,opt_weather_CaSiO3
  LOGICAL:: opt_weather_T_Ca                                                             ! CaCO3 weathering-temperature feedback
  LOGICAL:: opt_weather_T_Si                                                             ! CaSiO3 weathering-temperature feedback
  LOGICAL:: opt_weather_Talt_Si                                                             ! CaSiO3 weathering-temperature feedback
  LOGICAL:: opt_weather_R_explicit                                                       ! if true then R/R_0 is used rather than the 1 + 0.045(T-T_0) parameterisation from GEOCARB.
  LOGICAL:: opt_weather_R_Ca                                                             ! CaCO3 weathering-runoff feedback
  LOGICAL:: opt_weather_R_Si                                                             ! CaSiO3 weathering-runoff feedback
  NAMELIST /ini_rokgem_nml/opt_weather_T_Ca,opt_weather_T_Si,opt_weather_Talt_Si
  NAMELIST /ini_rokgem_nml/opt_weather_R_explicit,opt_weather_R_Ca,opt_weather_R_Si
  LOGICAL:: opt_weather_C_Ca                                                             ! CaCO3 weathering-CO2 feedback  
  LOGICAL:: opt_weather_C_Si                                                             ! CaSiO3 weathering-CO2 feedback
  LOGICAL:: opt_weather_C_Si_bg                                                          ! CaSiO3 weathering-CO2 feedback -- distinguish between basalt and granite
  NAMELIST /ini_rokgem_nml/opt_weather_C_Ca,opt_weather_C_Si,opt_weather_C_Si_bg
  REAL:: par_nCa                                                                         ! exponent to control strength of CO2-weathering feedback for CaCO3
  REAL:: par_nSi                                                                         ! exponent to control strength of CO2-weathering feedback for CaSiO3
  NAMELIST /ini_rokgem_nml/par_nCa,par_nSi
  LOGICAL:: opt_weather_P_explicit                                                       ! if true then P/P_0 is used rather than the [2RCO2/(1+RCO2)]^0.4 parameterisation from GEOCARB
  LOGICAL:: opt_weather_P_Ca                                                             ! CaCO3 weathering-productivity feedback
  LOGICAL:: opt_weather_P_Si                                                             ! CaSiO3 weathering-productivity feedback
  NAMELIST /ini_rokgem_nml/opt_weather_P_explicit,opt_weather_P_Ca,opt_weather_P_Si
  CHARACTER(len=63)::par_prodopt                                                         ! prodictivity to use ("GPP" or "NPP")
  NAMELIST /ini_rokgem_nml/par_prodopt
  REAL:: par_k_Ca                                                                        ! constant for temperature-silicate weathering feedback
  REAL:: par_k_Si                                                                        ! constant for temperature-carbonate weathering feedback
  REAL:: par_E_a                                                                         ! Activation energy for silicate weathering (kJ/Mol)
  NAMELIST /ini_rokgem_nml/par_k_Ca,par_k_Si,par_E_a  
  REAL:: par_k_Tb                                                                          ! Exponent for basalt weathering
  REAL:: par_k_Tg                                                                          ! Exponent for granite weathering
  NAMELIST /ini_rokgem_nml/par_k_Tb,par_k_Tg
  REAL:: par_k_run                                                                       ! constant for temperature-runoff linear correlation
  REAL:: par_beta                                                                        ! frac. power of explicit weathering-runoff dependence  NAMELIST /ini_rokgem_nml/par_k_Ca,par_E_a,par_k_run
  NAMELIST /ini_rokgem_nml/par_k_run,par_beta
  REAL:: par_ref_T0                                                                      ! weathering reference mean global land surface temperature (C)
  REAL:: par_ref_R0                                                                      ! weathering reference mean global runoff (mm/yr)  
  REAL:: par_ref_P0                                                                      ! weathering reference mean global land productivity (kgC m-2 yr-1)
  REAL:: par_ref_CO20                                                                    ! weathering reference mean global land atmospheric CO2 level (ppm)  
  NAMELIST /ini_rokgem_nml/par_ref_T0,par_ref_R0,par_ref_P0,par_ref_CO20 
  LOGICAL:: opt_outgas_eq_Si
  NAMELIST /ini_rokgem_nml/opt_outgas_eq_Si                                              ! set volcanic outgassing equal to silicate weathering
  REAL:: par_outgas_CO2                                                                  ! CO2 outgassing rate (mol C yr-1)
  REAL:: par_outgas_CO2_d13C                                                             ! mean volcanic/metamorphic d13C (o/oo)
  NAMELIST /ini_rokgem_nml/par_outgas_CO2,par_outgas_CO2_d13C          
  ! ------------------- GLOBAL AVERAGE WEATHERING PARAMETERS --------------------------------------------------------------------- !
  REAL:: par_weather_CaSiO3                                                              ! global silicate weathering rate (mol ALK yr-1)
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3
  REAL:: par_weather_CaSiO3b                                                             ! basaltic weathering rate (mol ALK yr-1)
  REAL:: par_weather_CaSiO3g                                                             ! granitic weathering rate (mol ALK yr-1)
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3b,par_weather_CaSiO3g
  REAL:: par_weather_CaSiO3_fracMg                                                       ! global (Ca silicate) Mg relative abundance
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracMg
  REAL:: par_weather_CaSiO3b_fracMg                                                      ! basaltic (Ca silicate) Mg relative abundance
  REAL:: par_weather_CaSiO3g_fracMg                                                      ! granitic (Ca silicate) Mg relative abundance
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3b_fracMg,par_weather_CaSiO3g_fracMg
  REAL:: par_weather_CaSiO3_fracC                                                        ! global (silicate) kerogen relativeabundance
  REAL:: par_weather_CaSiO3_fracC_d13C                                                   ! global (silicate) kerogen d13C
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracC,par_weather_CaSiO3_fracC_d13C
  REAL:: par_weather_CaSiO3_fracP                                                        ! global (silicate) phosphate relativeabundance
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracP
  REAL:: par_weather_CaCO3                                                               ! global carbonate weathering rate (mol ALK yr-1)
  REAL:: par_weather_CaCO3_d13C                                                          ! mean carbonate d13C (o/oo)
  NAMELIST /ini_rokgem_nml/par_weather_CaCO3,par_weather_CaCO3_d13C
  real:: par_weather_CaSiO3_fracSi                                                       ! global (silicate) Si relative abundance
  REAL:: par_weather_CaSiO3_fracSi_d30Si                                                 ! global silicate d30Si
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracSi,par_weather_CaSiO3_fracSi_d30Si
  real:: par_weather_CaSiO3_fracFeS2                                                     ! global (silicate) pyrite relative abundance
  REAL:: par_weather_CaSiO3_fracFeS2_d34S                                                ! global pyrite d34S
  REAL:: par_weather_CaSiO3_fracFeS2_d56Fe                                               ! global pyrite d56Fe
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracFeS2,par_weather_CaSiO3_fracFeS2_d34S,par_weather_CaSiO3_fracFeS2_d56Fe
  real:: par_weather_CaCO3_fracCaSO4                                                     ! global (carbonate) gypsum relative abundance
  REAL:: par_weather_CaCO3_fracCaSO4_d34S                                                ! global gypsum d34S
  REAL:: par_weather_CaCO3_fracCaSO4_d44Ca                                               ! global gypsum d44Ca
  NAMELIST /ini_rokgem_nml/par_weather_CaCO3_fracCaSO4,par_weather_CaCO3_fracCaSO4_d34S,par_weather_CaCO3_fracCaSO4_d44Ca  
  real:: par_weather_CaCO3_fracFeCO3                                                     ! global (carbonate) siderite relative abundance
  REAL:: par_weather_CaCO3_fracFeCO3_d56Fe                                               ! global siderite d56Fe
  REAL:: par_weather_CaCO3_fracFeCO3_d13C                                                ! global siderite d13C
  NAMELIST /ini_rokgem_nml/par_weather_CaCO3_fracFeCO3,par_weather_CaCO3_fracFeCO3_d56Fe,par_weather_CaCO3_fracFeCO3_d13C
  real:: par_weather_CaSiO3_fracLi                                                       ! global silicate Li relativeabundance
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracLi
  real:: par_weather_Li_Rscale                                                           ! global silicate Li weathering scaling 
  real:: par_weather_Li_Roffset                                                          ! global silicate Li weathering offset
  NAMELIST /ini_rokgem_nml/par_weather_Li_Rscale,par_weather_Li_Roffset
  real:: par_weather_CaSiO3_Li_d7Li                                                      ! global silicate (bulk Earth) d7Li (o/oo)
  real::par_weather_Li_7Li_epsilon                                                       ! secondary clay fractionation (o/oo)
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_Li_d7Li,par_weather_Li_7Li_epsilon
  REAL:: par_weather_CaSiO3_d44Ca                                                        ! 
  REAL:: par_weather_CaCO3_d44Ca                                                         ! 
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_d44Ca,par_weather_CaCO3_d44Ca
  REAL::par_weather_CaSiO3_fracSr                                                        ! global silicate Sr abundance
  REAL::par_weather_CaCO3_fracSr                                                         ! global carbonate Sr abundance     
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracSr,par_weather_CaCO3_fracSr
  REAL::par_weather_CaSiO3b_fracSr                                                       ! basaltic silicate Sr abundance
  REAL::par_weather_CaSiO3g_fracSr                                                       ! granitic silicate Sr abundance    
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3b_fracSr,par_weather_CaSiO3g_fracSr
  REAL::par_weather_CaSiO3_r87Sr                                                         ! global silicate r87Sr (87/86)
  REAL::par_weather_CaCO3_r87Sr                                                          ! global carbonate r87Sr (87/86)
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_r87Sr,par_weather_CaCO3_r87Sr
  REAL::par_weather_CaSiO3b_r87Sr                                                         ! basaltic r87Sr (87/86)
  REAL::par_weather_CaSiO3g_r87Sr                                                        ! granitic r87Sr (87/86)
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3b_r87Sr,par_weather_CaSiO3g_r87Sr
  REAL::par_weather_CaSiO3_d88Sr                                                         ! global silicate d88Sr (o/oo)
  REAL::par_weather_CaCO3_d88Sr                                                          ! global carbonate d88r (o/oo) 
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_d88Sr,par_weather_CaCO3_d88Sr
  REAL::par_weather_CaSiO3b_d88Sr                                                         ! basaltic d88Sr (o/oo)
  REAL::par_weather_CaSiO3g_d88Sr                                                        ! granitic d88r (o/oo) 
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3b_d88Sr,par_weather_CaSiO3g_d88Sr
  real:: par_weather_CaSiO3_fracCa5PO43                                                  ! global silicate apatite relative abundance
  REAL:: par_weather_CaSiO3_fracCa5PO43_d44Ca                                            ! global apatite d44Ca
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracCa5PO43,par_weather_CaSiO3_fracCa5PO43_d44Ca
  REAL::par_weather_Ca0PO41                                                              ! global apatite weathering rate (mol PO4 yr-1)
  NAMELIST /ini_rokgem_nml/par_weather_Ca0PO41  
  REAL::par_weather_SiO2                                                                 ! global quartz weathering rate (mol Si yr-1)
  REAL::par_weather_SiO2_d30Si                                                           ! global quartz d30Si (o/oo)
  NAMELIST /ini_rokgem_nml/par_weather_SiO2,par_weather_SiO2_d30Si  
  LOGICAL:: opt_calibrate_T_0D                                                           ! calibrate temperature fields to global average data
  LOGICAL:: opt_calibrate_R_0D                                                           ! calibrate runoff fields to global average data
  LOGICAL:: opt_calibrate_P_0D                                                           ! calibrate productivity fields to global average data
  NAMELIST /ini_rokgem_nml/opt_calibrate_T_0D,opt_calibrate_R_0D,opt_calibrate_P_0D
  REAL:: par_data_T_0D                                                                   ! mean global land surface temperature (C) to calibrate to
  REAL:: par_data_R_0D                                                                   ! mean global runoff (mm/yr) to calibrate to
  REAL:: par_data_P_0D                                                                   ! mean global land productivity (kgC m-2 yr-1) to calibrate to
  NAMELIST /ini_rokgem_nml/par_data_T_0D,par_data_R_0D,par_data_P_0D
  real::par_n_max_CaCO3                                        ! imposed maximum carbonate weathering enhancement
  real::par_n_max_CaSiO3                                       ! imposed maximum silicate weathering enhancement
  NAMELIST /ini_rokgem_nml/par_n_max_CaCO3,par_n_max_CaSiO3
  real::par_weather_fCaCO3_enh_n                               ! enhanced weathering scale factor
  real::par_weather_fCaSiO3_enh_n                              ! enhanced weathering scale factor
  NAMELIST /ini_rokgem_nml/par_weather_fCaCO3_enh_n,par_weather_fCaSiO3_enh_n
  real::par_weather_fCaCO3_enh_nt                              ! enhanced weathering total inventory
  real::par_weather_fCaSiO3_enh_nt                             ! enhanced weathering total inventory
  NAMELIST /ini_rokgem_nml/par_weather_fCaCO3_enh_nt,par_weather_fCaSiO3_enh_nt
  ! ------------------- 2D WEATHERING PARAMETERS --------------------------------------------------------------------------------- !
  CHARACTER(len=63)::par_lith_data 
  CHARACTER(len=63)::par_lith_data2 
  CHARACTER(len=63)::par_lith_data3
  NAMELIST /ini_rokgem_nml/par_lith_data,par_lith_data2,par_lith_data3                   ! name of lithological data set - corresponding to directory genie-rokgem/data/input/lithologies_par_lith_datapar_lith_data2par_lith_data3_036_036
  LOGICAL:: truncate_to_land                                                             ! truncate lithological maps to genie land-mask - if option is set to false than flux from land in genie ocean, goes direct to ocean
  LOGICAL:: scale_to_landarea                                                            ! scale lithological areas to total the land surface area (switch off if altering lithological areas e.g. for enhanced weathering)
  NAMELIST /ini_rokgem_nml/truncate_to_land,scale_to_landarea
  LOGICAL:: calibrate_weath                                                              ! calibrate 2D weathering - if .true. use values below
  NAMELIST /ini_rokgem_nml/calibrate_weath  
  REAL:: calibrate_weather_GKWM_CaCO3                                                    ! calibration values for 2D CaCO3 weathering - to avoid drift, set equal to (half of CaCO3 sediment burrial flux)/(original uncorrected flux) 
  REAL:: calibrate_weather_GEM_CO2_CaCO3                                                 ! (e.g. 1.5754 for Gi, 1.0505 for Am)
  NAMELIST /ini_rokgem_nml/calibrate_weather_GKWM_CaCO3,calibrate_weather_GEM_CO2_CaCO3
  REAL:: calibrate_weather_GKWM_CaSiO3                                                   ! calibration values for 2D CaSiO3 weathering - to avoid drift, set equal to (half of CaCO3 sediment burrial flux)/(original uncorrected flux) 
  REAL:: calibrate_weather_GEM_CO2_CaSiO3                                                ! (e.g. 0.8510 for Gi, 0.7917 for Am) or leave as 1.0 for uncalibrated.
  NAMELIST /ini_rokgem_nml/calibrate_weather_GKWM_CaSiO3,calibrate_weather_GEM_CO2_CaSiO3
  LOGICAL:: opt_calibrate_T_2D                                                           ! calibrate temperature fields to data (recreate real world patterns)
  LOGICAL:: opt_calibrate_R_2D                                                           ! calibrate runoff fields to data (recreate real world patterns)
  LOGICAL:: opt_calibrate_P_2D                                                           ! calibrate productivity fields to data (recreate real world patterns)
  NAMELIST /ini_rokgem_nml/opt_calibrate_T_2D,opt_calibrate_R_2D,opt_calibrate_P_2D
  CHARACTER(len=63)::par_ref_T0_2D                                                       ! land surface temperature (C) calibration file containing base pattern for 
                                                                                         ! (model field is scaled by rg_par_data_T_2D divided by this if rg_opt_calibrate_T_2D=.true.)
  CHARACTER(len=63)::par_ref_R0_2D                                                       ! land surface runoff (mm/yr) calibration file containing base pattern for calibration
                                                                                         ! (model field is scaled by rg_par_data_R_2D divided by this if rg_opt_calibrate_R_2D=.true.)
  CHARACTER(len=63)::par_ref_P0_2D                                                       ! land surface productivity (kgC m-2 yr-1) calibration file containing base pattern for calibration 
                                                                                         ! (model field is scaled by rg_par_data_P_2D divided by this if rg_opt_calibrate_P_2D=.true.)
  NAMELIST /ini_rokgem_nml/par_ref_T0_2D,par_ref_R0_2D,par_ref_P0_2D
  CHARACTER(len=63)::par_data_T_2D                                                       ! file containing weathering reference land surface temperature (C) data field to calibrate to
  CHARACTER(len=63)::par_data_R_2D                                                       ! file containing weathering reference land surface runoff (mm/yr) data field to calibrate to
  CHARACTER(len=63)::par_data_P_2D                                                       ! file containing weathering reference land surface productivity (kgC m-2 yr-1) data field to calibrate to
  NAMELIST /ini_rokgem_nml/par_data_T_2D,par_data_R_2D,par_data_P_2D
  LOGICAL:: opt_weath_regimes                                                            ! separate kinetic and transport limited regimes?
  CHARACTER(len=63)::weath_regimes                                                       ! orogeny landmask file
  NAMELIST /ini_rokgem_nml/opt_weath_regimes,weath_regimes
  ! ############################################################################################################################## !


  ! *************************************
  ! *** MODEL CONFIGURATION CONSTANTS ***
  ! *************************************

  ! *** array dimensions ***
  ! grid dimensions
  INTEGER,PARAMETER::n_i                                  = ilon1_rok ! 
  INTEGER,PARAMETER::n_j                                  = ilat1_rok ! 
  ! grid properties array dimensions 
  INTEGER,PARAMETER::n_phys_rok                           = 08    ! number of grid properties descriptors
  INTEGER,PARAMETER::n_phys_ocnrok                        = 06    ! number of grid properties descriptors

  ! *** PRIMARY rokgem ARRAYS ***
  real,dimension(n_phys_rok,n_i,n_j)::phys_rok                                         ! 'physical' array info - see below (lats, lons, areas of grid cells)

  ! *** array index values ***
  ! weathering 'physics' properties array indices
  INTEGER,PARAMETER::ipr_lat                              = 01    ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipr_latn                             = 02    ! latitude (degrees) [northerly point]
  INTEGER,PARAMETER::ipr_lon                              = 03    ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipr_lone                             = 04    ! longitude (degrees) [easterly point]
  INTEGER,PARAMETER::ipr_dlat                             = 05    ! latitude (degrees) [width]
  INTEGER,PARAMETER::ipr_dlon                             = 06    ! longitude (degrees) [width]
  INTEGER,PARAMETER::ipr_A                                = 07    ! area (m2)
  INTEGER,PARAMETER::ipr_rA                               = 08    ! reciprocal area (to speed up numerics)

  ! *** array index names ***
  ! atmosphere interface 'physics'
  CHARACTER(len=16),DIMENSION(n_phys_rok),PARAMETER::string_phys_rok = (/ &
       & 'lat             ', &
       & 'latn            ', &
       & 'lon             ', &
       & 'lone            ', &
       & 'dlat            ', &
       & 'dlon            ', &
       & 'A               ', &
       & 'rA              '/)

  ! ocean/atm interface stuff: grids, depth and location of oceans, and tracers
  INTEGER,PARAMETER                              ::n_io = ilon1_rok
  INTEGER,PARAMETER                              ::n_jo = ilat1_rok
  INTEGER,PARAMETER                              ::n_ko = inl1_ocn                                 ! no. of depth levels in ocean
  REAL,DIMENSION(n_phys_ocnrok,n_io,n_jo)        ::phys_ocnrok                                     ! 'physical' array info for ocean-atmosphere - see above (lats, lons, areas of grid cells)
  INTEGER,DIMENSION(ilon1_ocn,ilat1_ocn)         ::goldstein_k1                                    ! taken from goldstein (put this in somewhere: goldstein_k1(:,:) = go_k1(:,:))  
 
 ! ocean-atmosphere interface 'physics' properties array indices
  INTEGER,PARAMETER::ipoa_lat                    = 01                                              ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipoa_lon                    = 02                                              ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipoa_dlat                   = 03                                              ! latitude (degrees) [width]
  INTEGER,PARAMETER::ipoa_dlon                   = 04                                              ! longitude (degrees) [width]
  INTEGER,PARAMETER::ipoa_A                      = 05                                              ! area (m2)
  INTEGER,PARAMETER::ipoa_rA                     = 06                                              ! reciprocal area (to speed up numerics)

  ! *** miscellaneous ***
  ! should really get from somewhere in global GENIE
  REAL                                           :: gridcell_area                                  ! in km^2:
  INTEGER                                        :: nlandcells
  INTEGER                                        :: ncells_antarctica                              ! number of grid cells taken up by Antarctica
  INTEGER                                        :: nrows_antarctica                               ! number of lattitude rows taken up by Antarctica

  ! Output
  INTEGER                                        :: tstep_count
  REAL                                           :: tsteps_per_year
  INTEGER,PARAMETER                              :: n_output_years_max = 10000
  REAL, DIMENSION(:), ALLOCATABLE                :: output_years_0d
  REAL, DIMENSION(:), ALLOCATABLE                :: output_years_2d
  INTEGER , DIMENSION(:), ALLOCATABLE            :: output_tsteps_0d
  INTEGER , DIMENSION(:), ALLOCATABLE            :: output_tsteps_2d
  INTEGER                                        :: output_counter_0d
  INTEGER                                        :: output_counter_2d
  REAL                                           :: year
  INTEGER                                        :: year_int, year_remainder
  CHARACTER(LEN=11)                              :: year_text
  INTEGER,PARAMETER                              :: nglobtracers = 5                               ! number of output 'tracers'
  INTEGER                                        :: n_outputs
  REAL, DIMENSION(:), ALLOCATABLE                :: outputs
  CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE   :: time_series_names
  CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE   :: output_descriptions 
  CHARACTER(len=7)                               :: string_ncrunid_rg                              ! netCDF runID
  CHARACTER(len=254)                             :: string_ncout2d_rg                              ! netCDF output filename
  INTEGER                                        :: ncout2d_ntrec_rg                               ! count for netcdf datasets
  INTEGER                                        :: ncout2d_iou_rg                                 ! io for netcdf datasets

  ! *** global average weathering variables ***
  REAL                                           :: weather_fCaCO3
  REAL                                           :: weather_fCaSiO3
  REAL                                           :: weather_fCaSiO3b
  REAL                                           :: weather_fCaSiO3g
          
  ! *** landmask and runoff routing arrays ***
  INTEGER                                        :: landmask(n_i,n_j)
  REAL                                           :: runoff_drainage(n_i+2,n_j+2)                   !'+2' comes from fact that *.k1 file is 38x38
  INTEGER                                        :: runoff_detail_i   
  INTEGER                                        :: runoff_detail_j
  REAL, DIMENSION(:,:), ALLOCATABLE              :: runoff_detail                                  ! contains n_i*n_j rows, each with a sucession of (lat, long, fraction) data 
                                                                                                   ! for each ocean cell corresponding to the land cell in question (each row represents the land
                                                                                                   ! cell given by lat= floor(rownumber/n_i) lon=mod(rownumber,n_i).
  INTEGER                                        :: runoff_drainto(n_i,n_j,2)                      !'+2' comes from fact that *.k1 file is 38x38
  REAL                                           :: runoff_coast(n_i,n_j)
  REAL                                           :: runoff_calib                                   ! calibration faction for routing schemes 2 and 3

  ! 2D basic weathering variables
  INTEGER                                        :: par_nliths                                     ! number of rock types (no. of files listed in x_lithologies.txt) - 6 for Gibbs, 7 for Amiotte            
  REAL, DIMENSION(:,:), ALLOCATABLE              :: weath_consts                               
  CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE   :: lithology_names
  REAL, DIMENSION(:,:,:), ALLOCATABLE            :: lithology
  REAL, DIMENSION(:,:,:), ALLOCATABLE            :: calcium_flux
  REAL                                           :: total_calcium_flux(n_i,n_j)                    ! Ca2+ weathering fluxes
  REAL                                           :: total_calcium_flux_Ca(n_i,n_j)
  REAL                                           :: total_calcium_flux_Si(n_i,n_j)  
  REAL                                           :: weather_fCaCO3_2D(n_i,n_j)                     ! weathering fluxes after T & P feedbacks
  REAL                                           :: weather_fCaSiO3_2D(n_i,n_j)
  REAL                                           :: orogeny(n_i,n_j)                               ! Orogeny Landmask to divide weathering into kinetic and transport limited regimes. Used if opt_weath_regimes=.true.
  REAL                                           :: regimes_calib(n_i,n_j)                         ! Array for use in calculations involving different weathering regimes.
  
  ! Calibration constants and arrays
  REAL                                           :: calibrate_T_0D
  REAL                                           :: calibrate_R_0D
  REAL                                           :: calibrate_P_0D
  REAL                                           :: ref_T0_2D(n_i,n_j)
  REAL                                           :: ref_R0_2D(n_i,n_j)
  REAL                                           :: ref_P0_2D(n_i,n_j)
  REAL                                           :: data_T_2D(n_i,n_j)
  REAL                                           :: data_R_2D(n_i,n_j)
  REAL                                           :: data_P_2D(n_i,n_j)
  REAL                                           :: calibrate_T_2D(n_i,n_j)
  REAL                                           :: calibrate_R_2D(n_i,n_j)
  REAL                                           :: calibrate_P_2D(n_i,n_j)

  ! conversion factors to speed up numerics
  REAL                                           :: conv_GKWM
  REAL                                           :: conv_GKWM_runoff 
  REAL                                           :: conv_GEM_CO2
  REAL                                           :: k_T                                            ! constant factor for temperature-silicate weathering feedback = 1000*E_a/(R*T_0^2)

  contains
    
! Subroutine: define_river_array
!
! dynamic memory allocation
!
! Uses:
!
!  - <genie_util.f90>
!
! Calls:
!
! - <check_iostat>

subroutine define_river_array()
      
      USE genie_util, ONLY : check_iostat
      
      implicit none
      
      ! locals
      integer :: alloc_stat
      
      ALLOCATE(runoff_detail(runoff_detail_i,runoff_detail_j),stat=alloc_stat)                       
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      
    end subroutine define_river_array



    subroutine define_2D_arrays()
      
      USE genie_util, ONLY : check_iostat
      
      implicit none
      
      ! locals
      integer :: alloc_stat
      
      ALLOCATE(weath_consts(par_nliths,4),stat=alloc_stat)                                                ! coeffs in weathering equations
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      ALLOCATE(lithology_names(par_nliths),stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      ALLOCATE(lithology(par_nliths,n_i,n_j),stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      ALLOCATE(calcium_flux(par_nliths,n_i,n_j),stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      
    end subroutine define_2D_arrays


! Subroutine: define_river_array
!
! dynamic memory cleanup
!
! Uses:
!
!  - <genie_util.f90>
!
! Calls:
!
! - <check_iostat>
    
    subroutine deallocate_arrays()
      
      USE genie_util, ONLY : check_iostat
      
      implicit none
      
      ! locals
      integer :: alloc_stat
            
      if (ALLOCATED(runoff_detail)) DEALLOCATE(runoff_detail,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      if (ALLOCATED(weath_consts)) DEALLOCATE(weath_consts,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      if (ALLOCATED(lithology_names)) DEALLOCATE(lithology_names,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      if (ALLOCATED(lithology)) DEALLOCATE(lithology,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      if (ALLOCATED(calcium_flux)) DEALLOCATE(calcium_flux,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      
    end subroutine deallocate_arrays

END MODULE rokgem_lib

