! ******************************************************************************************************************************** !
! ecogem_lib.f90
!
! LIBRARY MODULE
! ******************************************************************************************************************************** !


MODULE ecogem_lib

  use genie_control
  USE gem_cmn
  USE gem_util
  use gem_carbchem


  IMPLICIT NONE
  SAVE

  !-----------------------------------------------------------------------------------------

  ! ****************************************************************************************************************************** !
  ! *** NAMELIST DEFINITIONS ***************************************************************************************************** !
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! *** ECOSYSTEM MODEL CONFIGURATION PARAMETERS ********************************************************************************* !
  ! ****************************************************************************************************************************** !
  integer :: n_keco   ! number of model layers with living plankton
  namelist/ini_ecogem_nml/n_keco
  integer :: komax   ! number of organic matter classes
  namelist/ini_ecogem_nml/komax
  logical :: useDIC  ! using DIC
  logical :: useNO3  ! using Nitrate
  logical :: useNO2  ! using Nitrite
  logical :: useNH4  ! using Ammonium
  logical :: usePO4  ! using Phosphate
  logical :: useFe   ! using Iron
  logical :: useSiO2 ! using Silicate
  namelist/ini_ecogem_nml/useDIC,useNO3,useNO2,useNH4,usePO4,useFe,useSiO2
  logical :: useDIC_13C !ckc added by kc, using carbon-13 DIC
  namelist/ini_ecogem_nml/useDIC_13C
  logical :: nquota   ! dynamic N quota
  logical :: pquota   ! dynamic P quota
  logical :: fquota   ! dynamic Fe quota
  logical :: squota   ! dynamic Si quota
  logical :: chlquota ! dynamic chlorophyll
  namelist/ini_ecogem_nml/nquota,pquota,fquota,squota,chlquota
  logical :: fundamental ! fundamental niche experiment
  namelist/ini_ecogem_nml/fundamental
  logical :: eco_export_verbose ! output of size-class POC export
  namelist/ini_ecogem_nml/eco_export_verbose
  logical :: eco_uptake_fluxes ! output of size-class POC export
  namelist/ini_ecogem_nml/eco_uptake_fluxes
  logical :: c13trace !ckc tracing for carbon 13
  namelist/ini_ecogem_nml/c13trace

  !ckc need to add either a logical or other to choose which c13 scheme to use

  ! ****************************************************************************************************************************** !
  ! *** ECOSYSTEM MODEL RATE and RELATIONSHIP PARAMETERS ************************************************************************* !
  ! ****************************************************************************************************************************** !
  ! Nitrogen parameters
  real :: qminN_a,   qminN_b        ! a/b: minimum quota
  real :: qmaxN_a,   qmaxN_b        ! a/b: maximum quota
  real :: kexcN_a,   kexcN_b        ! a/b: excretion rate
  real :: amminhib                  ! ammonium inhibition of NO3 and NO2 uptake or growth
  ! Nitrate
  real :: vmaxNO3_a, vmaxNO3_b      ! a/b: maximum nitrate uptake rate    (Michaelis-Menten)
  real ::affinNO3_a    ,affinNO3_b  ! a/b: half-saturation nitrate uptake (Michaelis-Menten)
  ! Nitrite
  real :: vmaxNO2_a, vmaxNO2_b      ! a/b: maximum nitrite uptake rate    (Michaelis-Menten)
  real ::affinNO2_a,affinNO2_b      ! a/b: half-saturation nitrite uptake (Michaelis-Menten)
  ! Ammonium
  real :: vmaxNH4_a, vmaxNH4_b      ! a/b: maximum ammonium uptake rate    (Michaelis-Menten)
  real ::affinNH4_a,affinNH4_b      ! a/b: half-saturation ammonium uptake (Michaelis-Menten)
  namelist/ini_ecogem_nml/qminN_a,qmaxN_a,vmaxNO3_a,affinNO3_a,vmaxNO2_a,affinNO2_a,vmaxNH4_a,affinNH4_a,kexcN_a,amminhib
  namelist/ini_ecogem_nml/qminN_b,qmaxN_b,vmaxNO3_b,affinNO3_b,vmaxNO2_b,affinNO2_b,vmaxNH4_b,affinNH4_b,kexcN_b
  ! Phosphorus parameters
  real ::   qminP_a,   qminP_b      ! a/b: minimum quota
  real ::   qmaxP_a,   qmaxP_b      ! a/b: maximum quota
  real :: vmaxPO4_a, vmaxPO4_b      ! a/b: maximum uptake rate       (Michaelis-Menten)
  real ::affinPO4_a,affinPO4_b      ! a/b: half-saturation uptake    (Michaelis-Menten)
  real ::   kexcP_a,   kexcP_b      ! a/b: excretion rate
  namelist/ini_ecogem_nml/qminP_a,qmaxP_a,vmaxPO4_a,affinPO4_a,kexcP_a
  namelist/ini_ecogem_nml/qminP_b,qmaxP_b,vmaxPO4_b,affinPO4_b,kexcP_b
  ! Iron parameters
  real :: qminFe_a,qminFe_b         ! a/b: minimum quota
  real :: qmaxFe_a,qmaxFe_b         ! a/b: maximum quota
  real :: vmaxFe_a,vmaxFe_b         ! a/b: maximum uptake rate    (Michaelis-Menten)
  real ::affinFe_a,affinFe_b        ! a/b: half-saturation uptake (Michaelis-Menten)
  real :: kexcFe_a,kexcFe_b         ! a/b: excretion rate
  namelist/ini_ecogem_nml/qminFe_a,qmaxFe_a,vmaxFe_a,affinFe_a,kexcFe_a
  namelist/ini_ecogem_nml/qminFe_b,qmaxFe_b,vmaxFe_b,affinFe_b,kexcFe_b
  ! Silicon parameters
  real ::   qminSi_a,  qminSi_b     ! a/b: minimum quota
  real ::   qmaxSi_a,  qmaxSi_b     ! a/b: maximum quota
  real :: vmaxSiO2_a,vmaxSiO2_b     ! a/b: maximum uptake rate    (Michaelis-Menten)
  real ::affinSiO2_a,affinSiO2_b    ! a/b: half-saturation uptake (Michaelis-Menten)
  real ::   kexcSi_a,  kexcSi_b     ! a/b: excretion rate
  namelist/ini_ecogem_nml/qminSi_a,qmaxSi_a,vmaxSiO2_a,affinSiO2_a,kexcSi_a
  namelist/ini_ecogem_nml/qminSi_b,qmaxSi_b,vmaxSiO2_b,affinSiO2_b,kexcSi_b
  ! Carbon quota and Photosynthesis parameters
  real :: vmaxDIC_a,vmaxDIC_b,vmaxDIC_c       ! a/b/c: maximum photosynthetic rate
  real :: qcarbon_a,qcarbon_b       ! a/b: carbon per cell
  real :: alphachl_a,alphachl_b     ! a/b: initial slope of PI curve
  real :: PARfrac                   ! PAR fraction of solar radiation
  real :: chl2nmax                  !      maximum chlorophyll to nitrogen ratio
  real :: biosynth                  !      cost of biosynthesis
  real :: k_w                       !      light attenuation by water
  real :: k_chl                     !      light attenuation by chlorophyll a
  namelist/ini_ecogem_nml/vmaxDIC_a,vmaxDIC_b,vmaxDIC_c
  namelist/ini_ecogem_nml/qcarbon_a,alphachl_a,PARfrac,chl2nmax,biosynth,k_w,k_chl
  namelist/ini_ecogem_nml/qcarbon_b,alphachl_b
  logical::ctrl_restrict_mld    ! restrict MLD
  NAMELIST /ini_ecogem_nml/ctrl_restrict_mld
  logical::ctrl_PARseaicelimit    ! PAR attenutation by sea-ice cover?
  NAMELIST /ini_ecogem_nml/ctrl_PARseaicelimit
  ! Grazing parameters
  real :: ass_eff                   !      maximum assimilation efficiency
  integer :: ns                     !      prey switching exponent
  real :: hill                      !      hill number for grazing assimilation
  real :: Lambda                    !      grazing refuge parameter
  real ::  graz_a,  graz_b          ! a/b: maximum grazing rate
  real ::    kg_a,    kg_b          ! a/b: half-saturation concentration for grazing
  real ::pp_opt_a,pp_opt_b          ! a/b: optimal predator:prey length ratio
  real ::pp_sig_a,pp_sig_b          ! a/b: width of grazing kernel
  namelist/ini_ecogem_nml/ass_eff,ns,hill,lambda,graz_a,kg_a,pp_opt_a,pp_sig_a
  namelist/ini_ecogem_nml/                       graz_b,kg_b,pp_opt_b,pp_sig_b
  ! Other loss parameters
  real ::    respir_a,   respir_b   ! a/b: carbon respiration rate
  real ::   biosink_a,  biosink_b   ! a/b: biomass sinking rate
  real ::      mort_a,     mort_b   ! a/b: basal mortality
  real :: beta_graz_a,beta_graz_b,beta_graz_c   ! a/b/c: fraction messy feeding to dissolved
  real :: beta_mort_a,beta_mort_b,beta_mort_c   ! a/b/c: fraction mortality to dissolved
  real :: par_bio_remin_POC_frac2,par_bio_remin_CaCO3_frac2
  namelist/ini_ecogem_nml/respir_a,biosink_a,mort_a
  namelist/ini_ecogem_nml/respir_b,biosink_b,mort_b
  namelist/ini_ecogem_nml/beta_graz_a,beta_graz_b,beta_graz_c,beta_mort_a,beta_mort_b,beta_mort_c
  namelist/ini_ecogem_nml/par_bio_remin_POC_frac2,par_bio_remin_CaCO3_frac2
  ! Mixotrophy parameters
  real :: trophic_tradeoff
  namelist/ini_ecogem_nml/trophic_tradeoff
  ! foramecogenie parameters (11)
  logical::ctrl_foramecogenie_bleach, ctrl_foramecogenie_oa, ctrl_use_foramecogenie
  real :: foram_auto_cost_sn, foram_auto_cost_ss, foram_hetero_cost_sn, foram_hetero_cost_ss
  real :: foram_grazing_scale_bs, foram_grazing_scale_ss, foram_symbiont_esd_scale
  real :: foramecogenie_bleach_temp
  namelist/ini_ecogem_nml/foram_auto_cost_sn,foram_auto_cost_ss,foram_hetero_cost_sn,foram_hetero_cost_ss
  namelist/ini_ecogem_nml/foram_grazing_scale_bs, foram_grazing_scale_ss, foram_symbiont_esd_scale
  namelist/ini_ecogem_nml/ctrl_foramecogenie_bleach, ctrl_foramecogenie_oa,ctrl_use_foramecogenie
  namelist/ini_ecogem_nml/foramecogenie_bleach_temp
  ! Temperature dependence
  real ::  temp_A,temp_P,temp_K,temp_T0   !
  namelist/ini_ecogem_nml/temp_A,temp_P,temp_K,temp_T0
  ! maximum temperature
  real ::  temp_max
  namelist/ini_ecogem_nml/temp_max
  ! CaCO3 production
  real ::  par_bio_red_POC_CaCO3,par_bio_red_POC_CaCO3_pP
  namelist/ini_ecogem_nml/par_bio_red_POC_CaCO3,par_bio_red_POC_CaCO3_pP
  ! logical control variables
  logical::ctrl_debug_eco_init    ! debug ecogem nml input
  NAMELIST /ini_ecogem_nml/ctrl_debug_eco_init
  ! character strings
  character(10)::ctrl_tdep_form           ! form of temperature limitation function
  character(10)::ctrl_photosynth_form     ! form of temperature limitation function
  NAMELIST /ini_ecogem_nml/ctrl_tdep_form,ctrl_photosynth_form
  ! other stuff
  integer :: nsubtime ! Number of ecogem timesteps per biogem timestep
  NAMELIST /ini_ecogem_nml/nsubtime!
  CHARACTER(len=127)::par_ecogem_plankton_file
  NAMELIST /ini_ecogem_nml/par_ecogem_plankton_file
  CHARACTER(len=127)::par_ecogem_timeseries_file
  NAMELIST /ini_ecogem_nml/par_ecogem_timeseries_file
  ! JDW force T fields
  logical::ctrl_force_T
  namelist /ini_ecogem_nml/ctrl_force_T
  character(LEN=127)::par_ecogem_force_T_file
  namelist /ini_ecogem_nml/par_ecogem_force_T_file
  ! explicit grazing parameters
  logical::ctrl_grazing_explicit
  namelist /ini_ecogem_nml/ctrl_grazing_explicit
  logical::gkernel_cap
  namelist /ini_ecogem_nml/gkernel_cap
  character(len=127)::par_ecogem_grazing_file
  namelist /ini_ecogem_nml/par_ecogem_grazing_file
  ! ------------------- ISOTOPIC FRACTIONATION ----------------------------------------------------------------------------------- !
  CHARACTER(len=63)::opt_d13C_DIC_Corg                           ! Corg 13C fractionation scheme ID string
  NAMELIST /ini_ecogem_nml/opt_d13C_DIC_Corg
  real::par_d13C_DIC_Corg_b                                      ! b value for Popp et al. fractionation
  NAMELIST /ini_ecogem_nml/par_d13C_DIC_Corg_b
  real::par_d13C_DIC_Corg_ef                                     ! frac for intercellular C fix
  NAMELIST /ini_ecogem_nml/par_d13C_DIC_Corg_ef
  ! ------------------- RUN CONTROL ---------------------------------------------------------------------------------------------- !
  logical::ctrl_continuing                                     ! continuing run?
  NAMELIST /ini_ecogem_nml/ctrl_continuing
  REAL::par_misc_t_start                                       ! start time (years)
  REAL::par_misc_t_runtime                                     ! run time (years)
  NAMELIST /ini_ecogem_nml/par_misc_t_start,par_misc_t_runtime
  LOGICAL::ctrl_misc_t_BP                                      ! years before present?
  NAMELIST /ini_ecogem_nml/ctrl_misc_t_BP
  ! ------------------- I/O DIRECTORY DEFINITIONS -------------------------------------------------------------------------------- !
  CHARACTER(len=255)::par_indir_name                           !
  CHARACTER(len=255)::par_outdir_name                          !
  CHARACTER(len=255)::par_rstdir_name                          !
  NAMELIST /ini_ecogem_nml/par_indir_name,par_outdir_name,par_rstdir_name
  CHARACTER(len=127)::par_infile_name,par_outfile_name         !
  NAMELIST /ini_ecogem_nml/par_infile_name,par_outfile_name
  ! ------------------- DATA SAVING: MISC ---------------------------------------------------------------------------------------- !
  LOGICAL::ctrl_ncrst                                          ! restart as netCDF format?
  NAMELIST /ini_ecogem_nml/ctrl_ncrst
  CHARACTER(len=127)::par_ncrst_name                           !
  NAMELIST /ini_ecogem_nml/par_ncrst_name
  REAL :: par_data_save_sig_dt,par_data_save_slice_dt          ! Integration interval (years)
  NAMELIST /ini_ecogem_nml/par_data_save_sig_dt,par_data_save_slice_dt
  INTEGER :: par_data_save_slice_n
  NAMELIST /ini_ecogem_nml/par_data_save_slice_n
  CHARACTER(len=127)::par_infile_sig_name
  NAMELIST /ini_ecogem_nml/par_infile_sig_name
  LOGICAL::ctrl_data_save_sig_autoend,ctrl_data_save_3d_sig
  NAMELIST /ini_ecogem_nml/ctrl_data_save_sig_autoend,ctrl_data_save_3d_sig
  ! ------------------- DATA SAVING: TIME-SERIES --------------------------------------------------------------------------------- !
  LOGICAL::ctrl_data_save_timeseries                           ! time-series data save: e.g. JGOFS
  NAMELIST /ini_ecogem_nml/ ctrl_data_save_timeseries
  ! ------------------- MISC ----------------------------------------------------------------------------------------------------- !
  logical::ctrl_limit_neg_biomass
  NAMELIST /ini_ecogem_nml/ctrl_limit_neg_biomass
  real::par_beta_POCtoDOC
  namelist/ini_ecogem_nml/par_beta_POCtoDOC
  logical::ctrl_Tdep_POCtoDOC
  NAMELIST /ini_ecogem_nml/ctrl_Tdep_POCtoDOC
  ! ------------------- TEST" !--------------------------------------------------------------------------------------------------- !
  logical::ctrl_hello_world                                             ! hello world!
  NAMELIST /ini_ecogem_nml/ctrl_hello_world



  ! ****************************************************************************************************************************** !
  ! *** MODEL CONFIGURATION CONSTANTS - ARRAY DIMENSIONS ************************************************************************* !
  ! ****************************************************************************************************************************** !

  ! *** array dimensions ***
  ! grid dimensions
  INTEGER,PARAMETER::n_i = ilon1_ocn                                    !
  INTEGER,PARAMETER::n_j = ilat1_ocn                                    !
  INTEGER,PARAMETER::n_k = inl1_ocn                                     !

  ! indices for ecological arrays
  INTEGER :: npmax
  INTEGER :: iDIC,iNO3,iNO2,iNH4,iPO4,iFe,iSiO2,iimax
  INTEGER :: iCarb,iNitr,iPhos,iIron,iSili,iChlo,iomax
  INTEGER :: iChl,iSil
  INTEGER :: inutrient,iplankton,iorgmat,nDarwin
  INTEGER :: iDIC_13C,iimaxiso  !ckc isotope in nuts
  INTEGER :: iCarb13C,iomaxiso  !ckc isotopes in plank
  INTEGER :: inutiso, iplankiso, iorgmatiso !ckc isotopes


  ! ****************************************************************************************************************************** !
  ! *** GLOBAL VARIABLES AND RUN-TIME SET PARAMETERS ***************************************************************************** !
  ! *****************************************************
  ! thickness of ocean layers
  REAL                           ::goldstein_dsc !
  REAL   ,DIMENSION(n_k)         ::goldstein_dz  !
  REAL   ,DIMENSION(n_k)         ::goldstein_dza !
  INTEGER,DIMENSION(n_i,n_j)     ::goldstein_k1  !
  REAL   ,DIMENSION(0:n_j)       ::goldstein_sv  !
  REAL   ,DIMENSION(n_k)         ::loc_grid_dz   !
  REAL   ,DIMENSION(n_i,n_j,n_k) ::ocn_grid_vol  !
  INTEGER,DIMENSION(n_i,n_j)     ::wet_mask_ij   !
  INTEGER,DIMENSION(n_i,n_j,n_k) ::wet_mask_ijk  !

  ! Ecosystem arrays
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)  ::nutrient      ! nutrient array (iimax,i,j,k)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:)::plankton      ! plankton biomass array (npmax,iomax,i,j,k)
  !ckc isotope arrays for water (like nutrient) and plankton groups (like plankton)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)  ::nutiso        !ckc isotopes of nutrient (iimaxiso, i,j,k)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:)::plankiso      !ckc isotopes in plankton (npmax,iomaxiso,i,j,k)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:)::uptake_flux   ! inorganic nutrient uptake flux for each plankton (iomax,npmax,i,j,k)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:)::export_flux   ! surface export flux for each plankton (iomax,npmax,i,j,k)    Fanny/Maria - Aug19
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:)::AP_flux   ! surface autotrophic flux for each plankton (iomax,npmax,i,j,k)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:)::HP_flux   ! surface heterotrophic flux for each plankton (iomax,npmax,i,j,k)
  !ckc isotope uptake flux array, to trace full food web interaction
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:)::up_flux_iso   !ckc rate of upstake isotopes (iimaxiso,npmax,i,j,k)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)  ::eco_carb      ! carbonate chemistry variables
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)  ::eco_carbconst ! carbonate chemistry constants
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)  ::eco_carbalk   ! carbonate chemistry alkalinity
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)  ::eco_carbisor  ! carbonate (carbon) isotopic properties array
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:)::phys_limit    ! growth limitation factors
!BAW: zoolimit should be optional  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)  ::zoo_limit     ! zoo food limitation factors
  ! Size-dependent parameters (npmax)
  character(len=16),ALLOCATABLE,DIMENSION(:)    ::pft                                      ! Plankton functional type
  character(len=3) ,ALLOCATABLE,DIMENSION(:)    ::quotastrng                               ! Plankton biomass quota labels
  character(len=16),ALLOCATABLE,DIMENSION(:)    ::quotaunits                               ! Plankton biomass quota labels
  character(len=5) ,ALLOCATABLE,DIMENSION(:)    ::rsrcstrng                                ! Inorganic resource labels
  INTEGER          ,ALLOCATABLE,DIMENSION(:)    ::random_n                                 ! n population replicates
  INTEGER          ,ALLOCATABLE,DIMENSION(:)    ::nut2quota                                ! match nutrients to quotas
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::volume,diameter,logvol,logesd           ! Size parameters
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::autotrophy,heterotrophy                  ! Trophic strategy
  LOGICAL          ,ALLOCATABLE,DIMENSION(:)    ::herbivory,carnivory                      ! Feeding behavior - Added by Grigoratou, Nov18
  real             ,ALLOCATABLE,DIMENSION(:)    ::pp_opt_a_array,pp_sig_a_array,ns_array   ! grazing parameters as arrays
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::prey_refuge                              ! PFT dependent traits - Added by Grigoratou, Dec18
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::mort_protect                             ! PFT dependent traits - Added by Grigoratou, Dec18
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::growthcost_factor                        ! PFT dependent traits - Added by Grigoratou, Dec18
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::palatability                             ! Lower value for defence strategy
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::NO3up,Nfix,calcify,silicify              ! PFT dependent traits
  REAL             ,ALLOCATABLE,DIMENSION(:,:)  ::qmin,qmax,vmax,affinity,kexc             ! Nutrient quota parameters
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::qcarbon,alphachl                         ! Carbon quota and Photosynthesis parameters
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::graz,kg,pp_opt,pp_sig                    ! Grazing parameters
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::respir,biosink,mort,beta_graz,beta_mort  ! Other loss parameters
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::symbiont_auto_cost, symbiont_hetero_cost ! RY, foramecogenie symbiont cost parameter
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::auto_volume, hetero_volume, grazing_esd_scale, symbiont_esd_scale           ! RY, foramecogenie size parameter
  REAL             ,ALLOCATABLE,DIMENSION(:)    ::respir_cost,kg_scale, hetero_diameter                    ! RY, foramecogenie grazing parameter
  
  ! Grazing kernel
  REAL,ALLOCATABLE,DIMENSION(:,:)::gkernel,gkernelT
  ! netCDF and netCDF restart parameters
  CHARACTER(len=31)::string_rstid                              !
  CHARACTER(len=7)::string_ncrunid                             !
  CHARACTER(len=254)::string_ncout2d                           !
  CHARACTER(len=254)::string_ncout3d                           !
  CHARACTER(len=254)::string_ncrst                             !
  CHARACTER(len=254) ::string_nctsi                            !
  CHARACTER(len=254) ::string_nctsint                          !
  integer,ALLOCATABLE,DIMENSION(:)::ncout1d_ntrec              ! count for netcdf datasets
  integer::ncout2d_ntrec                                       ! count for netcdf datasets
  integer::ncout3d_ntrec                                       ! count for netcdf datasets
  integer,ALLOCATABLE,DIMENSION(:)::ncout1d_iou                ! io for netcdf datasets
  integer::ncout2d_iou                                         ! io for netcdf datasets
  integer::ncout3d_iou                                         ! io for netcdf datasets
  integer::ncrst_ntrec                                         ! count for netcdf datasets
  integer::ncrst_iou                                           ! io for netcdf restart
  ! *** I/O ***
  ! integrated values storage arrays
  REAL::int_t_sig                                                ! integrated time for run-time (signal) save (years)
  REAL::int_t_timeslice                                          ! integrated time for time-slice save (years)
  REAL::int_t_timeslice_TOT = 0.0                                ! integrated time for time-slice save (TOTAL) (years)
  integer::int_t_sig_count                                       !
  integer::int_t_timeslice_count                                 !
  ! *** time control ***
  REAL::par_misc_t_end                                           !
  real::par_misc_t_err                                           !
  LOGICAL::par_misc_t_go = .FALSE.                               !
  LOGICAL::par_misc_t_echo_header = .TRUE.                       !
  real::par_misc_t_tseries = 0.0
  real::par_misc_t_tslice  = 0.0
  logical::par_misc_t_intseries = .FALSE.
  logical::par_misc_t_intslice  = .FALSE.

 ! JDW: force temperature array
  real,allocatable,dimension(:,:)::T_input! (i,j,k) array for forced temperature JDW

  ! ############################################################################################################################## !
  ! ### ADD ADDITIONAL TIME-SERIES ARRAY DEFINITIONS HERE ######################################################################## !
  INTEGER :: n_tser
  character(len=16),ALLOCATABLE,DIMENSION(:)    ::tser_name       ! Time series name
  INTEGER          ,ALLOCATABLE,DIMENSION(:)    ::tser_i,tser_j   ! Time series coordinates
  ! Time-series storage arrays
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:)  ::nutrient_tser ! nutrient array (iimax,nsite,48)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)::plankton_tser ! plankton biomass array (npmax,iomax,nsite,48)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)::uptake_tser   ! inorganic nutrient uptake flux for each plankton (iomax,npmax,nsite,48)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)::gamma_tser    ! limiting factors (iomax,npmax,nsite,48)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)::export_tser    ! limiting factors (iomax,npmax,nsite,48)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)::autotrophic_tser    ! limiting factors (iomax,npmax,nsite,48)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)::heterotrophic_tser    ! limiting factors (iomax,npmax,nsite,48)
  
  REAL             ,ALLOCATABLE,DIMENSION(:)      ::time_tser     ! timestep # (48)

  ! ############################################################################################################################## !

  ! *** integrated (time-averaged) time-slice arrays ***
  ! integrated time slice storage arrays - ocean
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::int_plankton_timeslice !
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::int_uptake_timeslice   !
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::int_gamma_timeslice    !
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)   ::int_nutrient_timeslice !
!BAW: zoolimit should be optional  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:)   ::int_zoogamma_timeslice !
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::int_export_timeslice   ! Surface export flux for each plankton (iomax,npmax,i,j,k)  Fanny/Maria - Aug19
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::int_AP_timeslice   ! Surface autotrophic uptake flux for each plankton (iomax,npmax,i,j,k)
  REAL             ,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::int_HP_timeslice   ! Surface heterotrophic uptake flux for each plankton (iomax,npmax,i,j,k)

  ! ### ADD ADDITIONAL TIME-SLICE ARRAY DEFINITIONS HERE ######################################################################### !

END MODULE ecogem_lib
