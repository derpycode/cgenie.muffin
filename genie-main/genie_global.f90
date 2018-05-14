!--------------------------------------------------------
!
! File: genie_global.f90
!
! Description:
!
! This Fortran90 module contains the top-level variables
! and routines for the GENIE framework. 
!--------------------------------------------------------

MODULE genie_global

  use genie_control

  implicit none
  
  !     STANDARD VARAIBLE NAMES
  !     surf_*_??? = surface propoerties defined over the entire global surface
  !     land_*_??? = land-only surface properties
  !     ocean_*_??? = ocean-only (including seaice, i.e. non-land) surface properties
  !     seaice_*_??? = seaice-only surface propoerties
  !     landice_*_??? = landice-only surface properties
  !     atmos_*_??? = atmospheric properties over the entire global surface
  !
  !     *_*_atm = variables on the atmospheric grid
  !     *_*_ocn = variables on the ocean grid
  !     *_*_sic = variables on the seaice grid
  !     *_*_lic = variables on the landice grid
  !     *_*_lnd = variables on the land grid

  !     ============================================================
  !      Declarations
  !     ============================================================
  
  !
  integer::nthreads,thread_id

  !     Useful temporary variables for writing data to netcdf format
  !     files
  real,allocatable,dimension(:,:,:) :: ncwritedata
  ! 3d array in which to store multiple 2d data variables to output.
  character(len=BUFSIZ),allocatable,dimension(:) :: ncwritevarnames
  ! names of data arrays to appear in NetCDF file.

  !
  !     These set up the spatial and temporal grids for interpolation
  !     between modules, and for the timestepping
  !
  !     AY (22/09/04) : added istep_gsurf for GOLDOCN-GOLDSIC surflux

  ! var: genie_vesion
  ! Version number for the model
  integer :: genie_version
  character, parameter :: TAB = char(9) 

  ! This is the overall timestep counter
  integer(kind=8) :: koverall 
  integer::katm

  integer :: istep_atm    
  integer :: istep_sic    
  integer :: istep_ocn    
  integer :: istep_lic    
  integer :: istep_gsurf  
  integer :: istep_che    
  integer :: katmos       
  integer :: kgem   
  integer :: istep_gem
  integer :: istep_tot
  integer :: gem_yr
  integer :: gem_yr_min
  integer :: gem_yr_max
  integer :: gem_notyr
  integer :: gem_notyr_min
  integer :: gem_notyr_max
  integer :: gem_dyr
  integer :: gem_status
  integer :: gem_switch
  logical :: gem_adapt_auto
  logical :: gem_adapt_auto_unlimitedGEM
  real :: gem_adapt_dpCO2dt
  real :: gem_adapt_DpCO2
  real :: gem_pCO2
  real :: gem_pCO2_INIT
  real :: gem_pCO2_OLD
  logical :: gem_adapt_diag_biogem_full
  ! global variables for synchronizing data saving with BIOGEM
  real::genie_tseries = -1.0
  real::genie_tslice  = -1.0
  logical::genie_intseries = .FALSE.
  logical::genie_intslice  = .FALSE.
  logical::genie_endseries = .FALSE.
  logical::genie_endslice  = .FALSE.
  !
  ! SG Variables added to pass into biogem. Maybe removed later
  ! SG when the code is cleaned up.
  !
  integer :: istep_bgm
  integer::go_npstp,go_iwstp,go_itstp,go_ianav
  !
  real, dimension(ilon1_atm) :: alon1_atm
  real, dimension(ilat1_atm) :: alat1_atm
  real, dimension(ilon1_ocn) :: alon1_ocn
  real, dimension(ilat1_ocn) :: alat1_ocn
  real, dimension(ilon1_sic) :: alon1_sic
  real, dimension(ilat1_sic) :: alat1_sic
  real, dimension(ilon1_atm+1) :: aboxedge1_lon_atm
  real, dimension(ilat1_atm+1) :: aboxedge1_lat_atm
  real, dimension(ilon1_ocn+1) :: aboxedge1_lon_ocn
  real, dimension(ilat1_ocn+1) :: aboxedge1_lat_ocn
  real, dimension(ilon1_sic+1) :: aboxedge1_lon_sic
  real, dimension(ilat1_sic+1) :: aboxedge1_lat_sic
  real, dimension(ilon2_atm) :: alon2_atm
  real, dimension(ilat2_atm) :: alat2_atm
  real, dimension(ilon2_ocn) :: alon2_ocn
  real, dimension(ilat2_ocn) :: alat2_ocn
  real, dimension(ilon2_sic) :: alon2_sic
  real, dimension(ilat2_sic) :: alat2_sic
  real, dimension(ilon2_atm+1) :: aboxedge2_lon_atm
  real, dimension(ilat2_atm+1) :: aboxedge2_lat_atm
  real, dimension(ilon2_ocn+1) :: aboxedge2_lon_ocn
  real, dimension(ilat2_ocn+1) :: aboxedge2_lat_ocn
  real, dimension(ilon2_sic+1) :: aboxedge2_lon_sic
  real, dimension(ilat2_sic+1) :: aboxedge2_lat_sic
  real, dimension(ilon3_atm) :: alon3_atm
  real, dimension(ilat3_atm) :: alat3_atm
  real, dimension(ilon3_ocn) :: alon3_ocn
  real, dimension(ilat3_ocn) :: alat3_ocn
  real, dimension(ilon3_sic) :: alon3_sic
  real, dimension(ilat3_sic) :: alat3_sic
  real, dimension(ilon3_atm+1) :: aboxedge3_lon_atm
  real, dimension(ilat3_atm+1) :: aboxedge3_lat_atm
  real, dimension(ilon3_ocn+1) :: aboxedge3_lon_ocn
  real, dimension(ilat3_ocn+1) :: aboxedge3_lat_ocn
  real, dimension(ilon3_sic+1) :: aboxedge3_lon_sic
  real, dimension(ilat3_sic+1) :: aboxedge3_lat_sic
  real, dimension(inl1_ocn) :: depth1_ocn
  real, dimension(inl2_ocn) :: depth2_ocn
  !
  integer, dimension(ilon1_atm,ilat1_atm) :: ilandmask1_atm
  integer, dimension(ilon2_atm,ilat2_atm) :: ilandmask2_atm
  integer, dimension(ilon3_atm,ilat3_atm) :: ilandmask3_atm
  integer, dimension(ilon1_ocn,ilat1_ocn) :: ilandmask1_ocn
  integer, dimension(ilon2_ocn,ilat2_ocn) :: ilandmask2_ocn
  integer, dimension(ilon3_ocn,ilat3_ocn) :: ilandmask3_ocn
  integer, dimension(ilon1_sic,ilat1_sic) :: ilandmask1_sic
  integer, dimension(ilon2_sic,ilat2_sic) :: ilandmask2_sic
  integer, dimension(ilon3_sic,ilat3_sic) :: ilandmask3_sic
  !
  integer, dimension(ilon1_atm,ilat1_atm) :: iwork1_atm
  integer, dimension(ilon2_atm,ilat2_atm) :: iwork2_atm
  integer, dimension(ilon3_atm,ilat3_atm) :: iwork3_atm
  integer, dimension(ilon1_ocn,ilat1_ocn) :: iwork1_ocn
  integer, dimension(ilon2_ocn,ilat2_ocn) :: iwork2_ocn
  integer, dimension(ilon3_ocn,ilat3_ocn) :: iwork3_ocn
  !
  !     These are the fluxes which come out of the atmosphere, *_atm
  !     and those which go into goldstein after interpolation onto the
  !     goldstein spatial grid, *_ocn.
  !
  !     AY (18/02/04)
  !     Extra flux fields are added below this list for ocean,
  !     atmosphere and sea-ice fluxes required for c-GOLDSTEIN
  !     
  real, dimension (ilon1_atm,ilat1_atm) :: tstar_atm
  real, dimension (ilon1_ocn,ilat1_ocn) :: tstar_ocn
  real, dimension (ilon1_ocn,ilat1_ocn) :: latent_ocn
  real, dimension (ilon1_ocn,ilat1_ocn) :: sensible_ocn
  real, dimension (ilon1_atm,ilat1_atm) :: netsolar_atm
  real, dimension (ilon1_ocn,ilat1_ocn) :: netsolar_ocn
  real, dimension (ilon1_atm,ilat1_atm) :: netlong_atm
  real, dimension (ilon1_ocn,ilat1_ocn) :: netlong_ocn

  real, dimension (ilon1_atm,ilat1_atm) :: netheat_atm
  real, dimension (ilon1_sic,ilat1_sic) :: netheat_sic
  real, dimension (ilon1_atm,ilat1_atm) :: insolar_atm
  real, dimension (ilon1_sic,ilat1_sic) :: insolar_sic
  real, dimension (ilon1_atm,ilat1_atm) :: inlong_atm
  real, dimension (ilon1_sic,ilat1_sic) :: inlong_sic
  real, dimension (ilon1_atm,ilat1_atm) :: surft_atm
  real, dimension (ilon1_sic,ilat1_sic) :: surft_atm_sic
  real, dimension (ilon1_atm,ilat1_atm) :: surfq_atm
  real, dimension (ilon1_sic,ilat1_sic) :: surfq_atm_sic
  real, dimension (ilon1_atm,ilat1_atm) :: surfp_atm
  real, dimension (ilon1_sic,ilat1_sic) :: surfp_atm_sic
  !
  real, dimension(ilon1_atm,ilat1_atm) :: surf_latent_atm
  real, dimension(ilon1_atm,ilat1_atm) :: surf_sensible_atm
!plasim coefficients
  real, dimension(ilon1_atm,ilat1_atm) :: surf_latent_coeff_atm
  real, dimension(ilon1_atm,ilat1_atm) :: latent_coeff_atm
  real, dimension(ilon1_atm,ilat1_atm) :: surf_sensible_coeff_atm
  real, dimension(ilon1_atm,ilat1_atm) :: sensible_coeff_atm

  real, dimension(ilon1_atm,ilat1_atm) :: surf_stressx_atm
  real, dimension(ilon1_atm,ilat1_atm) :: surf_stressy_atm
  real, dimension(ilon2_atm,ilat2_atm) :: surf_stressx2_atm
  real, dimension(ilon2_atm,ilat2_atm) :: surf_stressy2_atm
  real, dimension(ilon3_atm,ilat3_atm) :: surf_stressx3_atm
  real, dimension(ilon3_atm,ilat3_atm) :: surf_stressy3_atm
  real, dimension(ilon3_atm,ilat3_atm) :: surf_windspeed_atm
  real, dimension(ilon1_atm,ilat1_atm) :: surf_evap_atm
  !
  real, dimension(ilon1_ocn,ilat1_ocn) :: trest_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: srest_ocn

  !
  real, dimension(ilon1_atm,ilat1_atm) :: land_latent_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_sensible_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_stressx_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_stressy_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_evap_atm = 0.0
  !
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_latent_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_sensible_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_stressx_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_stressy_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_evap_atm = 0.0
  !
  !     BEWARE!!! I HAVE CHANGED THIS A BIT....
  !       WHY WAS RUNOFF_OCN ON THE ATM GRID??!!
  !
  !    AY (22/07/04) : introducing new "flat" fields for wind speed and
  !                 wind stress (i.e. separate x and y components at
  !                 u (=2) and v (=3) positions)
  !
  !     surf_orog_atm = orography [m]  
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_lowestlu2_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_lowestlu3_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_lowestlv2_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_lowestlv3_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_stressx2_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_stressx3_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_stressy2_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_stressy3_ocn
  real, dimension(ilon1_sic,ilat1_sic) :: surf_windspeed_sic
  !     ***********************
  !     These 2 are just being kept temporarily until my and 
  !       Andrew's codes are merged....& 
  real, dimension(2,ilon1_ocn,ilat1_ocn) :: stressx_ocn
  real, dimension(2,ilon1_ocn,ilat1_ocn) :: stressy_ocn

  !     ***********************

  real, dimension(ilon1_atm,ilat1_atm) :: surf_orog_atm
  real, dimension(ilon1_atm,ilat1_atm) :: zonwind_atm
  real, dimension(ilon1_atm,ilat1_atm) :: merwind_atm
  real, dimension(ilon1_ocn,ilat1_ocn) :: runoff_ocn
  ! GHC 29/10/09 added runoff_land which is runoff on land 
  ! before routing to ocean, for use in rokgem
  real, dimension(ilon1_ocn,ilat1_ocn) :: runoff_land
  real, dimension(ilon1_atm,ilat1_atm) :: runoff_sic
  !
  !     For the seaice on the goldstein grid....
  real, dimension(ilon1_ocn,ilat1_ocn) :: seaicefrac_ocn
  !
  !     This variable is so that the stressx_atm and stressy_atm
  !     for goldstein can be passed sensibly.
  !     The atmos one is a dummy variable for returning the temp and albedo
  real, dimension(ilon1_ocn,ilat1_ocn) :: dummy_ocn
  real, dimension(ilon1_atm,ilat1_atm) :: dummy_atm
  !
  !     Need a variable for the interp mask
  real, dimension(ilon1_atm,ilat1_atm) :: interpmask_atm
  real, dimension(ilon1_ocn,ilat1_ocn) :: interpmask_ocn
  !
  real :: weighttot_atm
  real :: weighttot_ocn
  !
  !     Extra fluxes for new c-GOLDSTEIN modules
  !       
  real, dimension(ilon1_ocn,ilat1_ocn) :: evap_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: precip_ocn
  real, dimension(ilon1_atm,ilat1_atm) :: evap_atm
  real, dimension(ilon1_atm,ilat1_atm) :: evap_sic
  real, dimension(ilon1_atm,ilat1_atm) :: precip_atm
  real, dimension(ilon1_atm,ilat1_atm) :: precip_sic
  real, dimension(ilon1_ocn,ilat1_ocn) :: ustar_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: vstar_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: sstar_ocn
  !
  !     Extra fields for c-GOLDSTEIN sea-ice module
  !
  real, dimension(ilon1_sic,ilat1_sic) :: hght_sic
  real, dimension(ilon1_sic,ilat1_sic) :: frac_sic
  real, dimension(ilon1_sic,ilat1_sic) :: temp_sic
  real, dimension(ilon1_sic,ilat1_sic) :: albd_sic
  real, dimension(ilon1_sic,ilat1_sic) :: dhght_sic
  real, dimension(ilon1_sic,ilat1_sic) :: dfrac_sic
  real, dimension(ilon1_ocn,ilat1_ocn) :: waterflux_ocn
  real, dimension(ilon1_atm,ilat1_atm) :: waterflux_atm
  real, dimension(ilon1_sic,ilat1_sic) :: waterflux_sic
!for PLASIM coupling   
  real, dimension(4,ilon1_sic,ilat1_sic) :: delta_flux
  !
  !     CO2 concentration field for c-GOLDSTEIN surface flux routine - 
  !     will be unnecessary in final version of genie.F (when AtCheM is a
  !     genie.F level routine), but necessary at present
  !     Added methane and N2O.
  !     Also now essential for use with the igcm.
  !     With the igcm, these gases are given by genie-fixedchem.
  real, dimension(ilon1_atm,ilat1_atm) :: co2_atm
  real, dimension(ilon1_atm,ilat1_atm) :: n2o_atm
  real, dimension(ilon1_atm,ilat1_atm) :: ch4_atm
  !     
  !     14co2 used in igcm and ichem modules
  !
  real, dimension(ilon1_atm,ilat1_atm,inl1_atm) :: mass14co2
  real, dimension(ilon1_atm,ilat1_atm,inl1_atm) :: ddtmass14co2
  real, dimension(ilon1_atm,ilat1_atm,inl1_atm) :: massair
  !
  !     Parameters storing the information about the ocean basins
  !     that needs to be passed from GOLDSTEIN to the EMBM
  !
  integer, dimension(ilat1_ocn) :: ips_out
  integer, dimension(ilat1_ocn) :: ipf_out
  integer, dimension(ilat1_ocn) :: ias_out
  integer, dimension(ilat1_ocn) :: iaf_out
  integer jsf_out
  
  !
  !    AY (21/09/04) : extra fields of "average" ocean cell temperature
  !                 and roughness - not used at present, but will be
  !
  real, dimension(ilon1_ocn,ilat1_ocn) :: tavg_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: rough_ocn
  !     
  !     These are the fluxes out of the atmosphere once they have been 
  !     averaged onto the seaice (*_atm_meanseaice) or ocean 
  !     (*_atm_meanocn) timestep.
  !     
  real, dimension(ilon1_atm,ilat1_atm) :: latent_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: latent_atm_meansic
  real, dimension(ilon1_atm,ilat1_atm) :: sensible_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: sensible_atm_meansic
  real, dimension(ilon1_atm,ilat1_atm) :: netsolar_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: netsolar_atm_meansic
  real, dimension(ilon1_atm,ilat1_atm) :: netlong_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: netlong_atm_meansic
  real, dimension(ilon1_atm,ilat1_atm) :: stressx_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: stressx_atm_meansic
  real, dimension(ilon1_atm,ilat1_atm) :: stressy_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: stressy_atm_meansic
  real, dimension(ilon1_atm,ilat1_atm) :: precip_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: precip_atm_meansic
  real, dimension(ilon1_atm,ilat1_atm) :: evap_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: evap_atm_meansic
  !
  !     AY (21/09/04) : extra meanocn fields for GOLDOCN-GOLDSIC surflux
  real, dimension(ilon1_atm,ilat1_atm) :: lowestlt_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: lowestlq_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: lowestlp_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: lowestlh_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: lowestlu_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: lowestlv_atm_meanocn
  !
  !     EXTRA FIELDS:
  !
  real, dimension(ilon1_atm,ilat1_atm) :: runoff_atm_meanocn
  !
  !     These are the fluxes out of the seaice once they have been 
  !     averaged onto the ocean 
  !     (*_atm_meanocn) timestep.
  !
  real, dimension(ilon1_atm,ilat1_atm) :: seaicefrac_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: conductflux_atm_meanocn
  real, dimension(ilon1_atm,ilat1_atm) :: waterflux_atm_meanocn
  !     
  !     This is the sea-ice fraction (=1 or 0 fo the slab case)
  !     Should really be renamed seaicefrac_seaice seen as it's on the 
  !       seaice grid (this just happens to be the same as the atmos grid
  !       at the moment)
  !
  real, dimension(ilon1_atm,ilat1_atm) :: seaicefrac_atm
  real, dimension(ilon1_atm,ilat1_atm) :: conductflux_atm
  !     
  !     These are the carry-over from ocean to seaice (*_ocn_seaice)
  !     or from seaice to ocean (*_seaice_ocn)
  !     
  real, dimension(ilon1_atm,ilat1_atm) :: dtcarry_ocn_sic
  real, dimension(ilon1_atm,ilat1_atm) :: energycarry_ocn_sic
  real, dimension(ilon1_atm,ilat1_atm) :: energycarry_sic_ocn
  !     
  !     This is the albedo.  In the slab and fixed case, it is calculated
  !     in the ocean or seaice modules.  It could be calculated by the 
  !     atmosphere instead, in which case it would not need to be in 
  !     genie.f.
  !     
  real, dimension(ilon1_atm,ilat1_atm) :: albedo_atm
  real, dimension(ilon1_ocn,ilat1_ocn) :: albedo_ocn
  !     
  !     This is the temperture of the uppermost layer of the ocean,
  !       and its thickness.  This is output from the ocean, and 
  !       used in the seaice module to calculate the ocean-seaice heat flux.
  !
  real, dimension(ilon1_atm,ilat1_atm) :: temptop_atm
  real, dimension(ilon1_atm,ilat1_atm) :: thicktop_atm
  
  !     Extra files
  real, dimension (ilon1_sic,ilat1_sic) :: conductflux_sic
  real, dimension (ilon1_ocn,ilat1_ocn) :: conductflux_ocn
  real, dimension (ilon1_sic,ilat1_sic) :: seaicefrac_sic
  !
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_lowestlu_atm
  real, dimension(ilon2_atm,ilat2_atm) :: atmos_lowestlu2_atm
  real, dimension(ilon3_atm,ilat3_atm) :: atmos_lowestlv3_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_lowestlv_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_lowestlq_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_lowestlt_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_lowestlp_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_lowestlh_atm
  !
  real, dimension(ilon1_atm,ilat1_atm) :: land_lowestlu_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_lowestlv_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_lowestlq_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_lowestlt_atm = 0.0
  !
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_lowestlu_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_lowestlv_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_lowestlq_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_lowestlt_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_lowestlp_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_lowestlh_atm = 0.0
  !
  real :: surfsigma
  real :: surfdsigma
  real, dimension(inl1_atm) :: psigma
  !
  !     For the fixedatmos grid.  1=igcm, 2=goldstein
  !
  integer :: grid_type_fixedatmos
  
  integer :: i,j
  !
  !     AY (23/09/04) : more extra fields
  !
  !     Inputs to c-GOLDSTEIN surflux routines
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_lowestlt_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_lowestlq_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_lowestlp_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_lowestlh_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_atm_netsolar_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_atm_netlong_ocn
  !
  !     Outputs from c-GOLDSTEIN surflux routines
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_latent_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_sensible_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_netsolar_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_netlong_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_evap_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_precip_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: ocean_runoff_ocn
  !
  real, dimension(ilon1_ocn,ilat1_ocn) :: atmos_latent_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: atmos_sensible_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: atmos_netsolar_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: atmos_netlong_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: atmos_evap_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: atmos_precip_ocn
  !
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_latent_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_sensible_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_netsolar_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_netlong_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_evap_atm
  real, dimension(ilon1_atm,ilat1_atm) :: atmos_precip_atm
  !
  !     AY (10/09/04) : GOLDOCN-GOLDSIC surflux requires information about
  !                 average ocean cell temperature and albedo.  Also,
  !                 temporary variables needed after goldstein.F call 
  !                 to feed either ocean-only or ocean+sea-ice average
  !                 fields of temperature and albedo out (i.e. slab
  !                 sea-ice vs. GOLDSTEIN sea-ice)
  !
  real, dimension(ilon1_ocn,ilat1_ocn) :: tmpavg_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: albavg_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: dumtmp_ocn
  real, dimension(ilon1_ocn,ilat1_ocn) :: dumalb_ocn
  !
  !     ****************************
  !     For the energy and water checking routines.....
  !
  real :: tmp_check
  
  real :: escn_netsolar_atm_meanocn
  real :: escn_netsolar_atm_meansi
  real :: escn_netsolar_atm_meansurf
  real :: escn_netsolar_ocn
  !
  real :: escn_netlong_atm_meanocn
  real :: escn_netlong_atm_meansi
  real :: escn_netlong_atm_meansurf
  real :: escn_netlong_ocn
  !
  real :: escn_sensible_atm_meanocn
  real :: escn_sensible_atm_meansi
  real :: escn_sensible_atm_meansurf
  real :: escn_sensible_ocn
  !
  real :: escn_latent_atm_meanocn
  real :: escn_latent_atm_meansi
  real :: escn_latent_atm_meansurf
  real :: escn_latent_ocn
  !
  real :: escn_conductflux_atm_meanocn
  real :: escn_conductflux_ocn
  !
  real :: wscn_precip_atm_meanocn
  real :: wscn_precip_atm_meansurf
  real :: wscn_precip_atm_meansurf_o
  real :: wscn_precip_atm_meansurf_l
  real :: wscn_precip_ocn
  !
  real :: wscn_evap_atm_meanocn
  real :: wscn_evap_atm_meansurf
  real :: wscn_evap_atm_meansurf_o
  real :: wscn_evap_atm_meansurf_l
  real :: wscn_evap_ocn
  !
  real :: wscn_waterflux_atm_meanocn
  real :: wscn_waterflux_atm_meansurf
  real :: wscn_waterflux_ocn
  !
  real :: wscn_runoff_atm_meanocn
  real :: wscn_runoff_atm_meansurf
  real :: wscn_runoff_ocn
  !
  real, dimension(ilon1_atm,ilat1_atm) :: weight_atm
  real, dimension(ilon1_ocn,ilat1_ocn) :: weight_ocn
  real :: weightcheck
  real :: pi
  parameter(pi=3.141592654)
  !
  real :: enet_atm
  real :: enetnoconduct_atm
  real :: enet_ocn
  !
  real :: wnet_atm
  real :: wnet_atm_o
  real :: wnet_atm_l
  real :: wnet_ocn
  !
  real :: plumin
  !
  !     ****************************
  !
  !     FOR THE NEW SPLIT BLSURF:
  !     This is the loop over the igcm landsurface scheme.
  !
  integer :: surf_iter_tim
  integer :: land_niter_tim
  integer :: ocean_niter_tim
  !
  real, dimension(ilon1_atm,ilat1_atm) :: land_latentinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_sensibleinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_stressxinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_stressyinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_evapinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_tstarinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_rough_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_qstar_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_salb_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: land_runoff_atm = 0.0
  !
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_latentinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_sensibleinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_stressxinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_stressyinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_evapinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_tstarinst_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_rough_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_qstar_atm = 0.0
  real, dimension(ilon1_atm,ilat1_atm) :: ocean_salb_atm = 0.0
  !
  real, dimension(ilon1_atm,ilat1_atm) :: surf_tstarinst_atm
  real, dimension(ilon1_atm,ilat1_atm) :: surf_rough_atm
  real, dimension(ilon1_atm,ilat1_atm) :: surf_qstar_atm
  real, dimension(ilon1_atm,ilat1_atm) :: surf_salb_atm
  !
  real :: atmos_dt_tim
  !
  real :: test_energy_seaice 
  real :: test_energy_ocean
  real :: test_water_seaice 
  real :: test_water_ocean 
  real :: test_water_land 

  real, dimension(ilon1_atm,ilat1_atm) :: test_energy_radseaice
  real, dimension(ilon1_atm,ilat1_atm) :: test_energy_radocean
  real, dimension(ilon1_atm,ilat1_atm) :: test_energy_radsurf
  real, dimension(ilon1_atm,ilat1_atm) :: radn_energy
  real, dimension(ilon1_atm,ilat1_atm) :: temp_energy
  !
  real :: error_cal!
  !
  !     for the fixedicesheet testing....
  !
  real, dimension(ilon1_atm,ilat1_atm) :: landicealbedo_atm 
  real, dimension(ilon1_atm,ilat1_atm) :: landicefrac_atm
  real, dimension(ilon1_atm,ilat1_atm) :: landsnowicefrac_atm
  real, dimension(ilon1_atm,ilat1_atm) :: landsnowvegfrac_atm
  real, dimension(ilon1_atm,ilat1_atm) :: landsnowdepth_atm
  !
  !
  !     Parameter type variable needed by glimmer
  !
  logical glim_flag
  logical glim_snow_model
  logical glim_icets
  ! var: glim_timestep
  ! The length of the calling timestep in hours 
  integer glim_timestep 
  ! var: ratio_sd_dense
  ! Ratio of water and snow densities.
  ! This is needed because the igcm land scheme uses
  ! actual snow depth, whereas glimmer
  ! returns snow depth in m of water equivalent.
  real,parameter :: ratio_sd_dense=10.0/3.0 
  ! var: glim_orog
  ! Holds orography output by glimmer
  real, dimension(ilon1_atm,ilat1_atm) :: glim_orog
  real, dimension(ilon1_atm,ilat1_atm) :: glim_covmap
  real, dimension(ilon1_atm,ilat1_atm) :: glim_covmap_orog
  real, dimension(ilon1_atm,ilat1_atm) :: glim_albedo
  real, dimension(ilon1_atm,ilat1_atm) :: glim_icefrac
  real, dimension(ilon1_atm,ilat1_atm) :: glim_vegfrac
  real, dimension(ilon1_atm,ilat1_atm) :: glim_snowicefrac
  real, dimension(ilon1_atm,ilat1_atm) :: glim_snowvegfrac
  real, dimension(ilon1_atm,ilat1_atm) :: glim_snowdepth
  real, dimension(ilon1_atm,ilat1_atm) :: glim_waterin
  real, dimension(ilon1_atm,ilat1_atm) :: glim_waterout
  real, dimension(ilon1_atm,ilat1_atm) :: glim_prcp_adj
  real, dimension(ilon1_atm,ilat1_atm) :: glim_runoff_adj
  logical,dimension(ilon1_atm,ilat1_atm) :: glim_tempmask
  !
  !     Other variables and arrays needed for glimmer coupling
  !
  logical :: flag_glim_t2m_force
  !     Temporary genie-land variable
  !
  REAL,DIMENSION(ilon1_atm,ilat1_atm) :: tstar_gb_land,albedo_land  !For atmos
  REAL,DIMENSION(ilon1_atm,ilat1_atm) :: evap_land,fx_le_land,fx_sen_land
  REAL,DIMENSION(ilon1_atm,ilat1_atm) :: fx_momx_land,fx_momy_land   !For atmos
  REAL,DIMENSION(ilon1_atm,ilat1_atm) :: land_fxco2_atm,ice_icefrac_atm
  REAL,DIMENSION(ilon1_atm,ilat1_atm) :: land_tice_ice,land_albice_ice  !For ice-sheet
  !
  !      This is a dodgy parameter for the icesheet to pass to the 
  !        igcm the fact that it has changed.....
  !      iconv_ice comes out of the icesheet as 1 
  !        if the orography has been updated.
  !      When the igcm has recalculated the spectral coefficients, it 
  !        resets iconv_ice to 0.
  !
  integer :: iconv_ice = 0
  integer :: iconv4lnd  ! Land also needs this dodgy parameter...
  ! There should really be a generic icesheet model variable
  ! for this(a la glim_flag) and not modified by other model
  ! components
  integer :: iconv_che = 0 ! This one is to indicate to the igcm that the gases have 
  !been updates 
  !
  !     *********************************************
  !     for temporary water-fix.  To be removed.....      
  integer ifirst
  data ifirst/1/
  real, dimension(ilon1_ocn,ilat1_ocn) :: evap_save1
  real, dimension(ilon1_ocn,ilat1_ocn) :: evap_save2
  real, dimension(ilon1_ocn,ilat1_ocn) :: late_save1
  real, dimension(ilon1_ocn,ilat1_ocn) :: late_save2
  real, dimension(ilon1_ocn,ilat1_ocn) :: sens_save1
  real, dimension(ilon1_ocn,ilat1_ocn) :: sens_save2
  !     *********************************************
  
  !     For seaice writing variables - not yet implemented.
  real, dimension(ilon1_sic,ilat1_sic) :: netsolar_sic
  real, dimension(ilon1_sic,ilat1_sic) :: netlong_sic
  real, dimension(ilon1_sic,ilat1_sic) :: sensible_sic
  real, dimension(ilon1_sic,ilat1_sic) :: latent_sic
  real, dimension(ilon1_sic,ilat1_sic) :: stressx_sic
  real, dimension(ilon1_sic,ilat1_sic) :: stressy_sic
  real, dimension(ilon1_sic,ilat1_sic) :: tstar_sic
  real, dimension(ilon1_sic,ilat1_sic) :: albedo_sic
  
  !     for water-transport diagnosti!
  real, dimension(ilon1_atm,ilat1_atm) :: at_mask
  real :: at1 = 0.0
  real :: at2 = 0.0
  real :: at3 = 0.0
  real :: at1_tmp = 0.0
  real :: at2_tmp = 0.0
  real :: at3_tmp = 0.0
  
  !       here are the boundary-layer variables:
  real, dimension(ilon1_atm,ilat1_atm) :: u10m_atm
  real, dimension(ilon1_atm,ilat1_atm) :: v10m_atm
  real, dimension(ilon1_atm,ilat1_atm) :: t2m_atm
  real, dimension(ilon1_atm,ilat1_atm) :: q2m_atm
  real, dimension(ilon1_atm,ilat1_atm) :: rh2m_atm

  ! This is for the standard genie timestep.
  ! It is the length in seconds of one timestep
  real :: genie_timestep

  ! Running total of elapsed time (SECONDS)
  ! An 8-byte integer can count up to 9.2^18
  integer(kind=8) :: genie_clock

  ! Verbosity level for logging and messages to screen
  ! 0: silent running, 1: basic, 2: chatty, 3: debug
  ! Value is set in genie_control_nml
  integer :: verbosity

  ! solar constant
  real :: genie_solar_constant

! *********************************

! genie-biogem variables:
! This will not be hard-wired soon!!!!!!
! Also, to be renamed!!
  real :: go_dt
  real :: go_saln0
  real :: go_rhoair = 1.25
  real :: go_cd = 1.3e-3
  real, dimension(ilat1_ocn) :: go_ds
  real :: go_dphi
  real :: go_usc
!  parameter(go_tsc=go_rsc/go_usc)
  real :: go_dsc = 5e3
  real :: go_fsc = 2*7.2921e-5
  real :: go_rh0sc = 1e3
  real :: go_rhosc, go_cpsc, go_solconst
  real :: go_scf
  integer,dimension(ilat1_ocn)::go_ips
  integer,dimension(ilat1_ocn)::go_ipf
  integer,dimension(ilat1_ocn)::go_ias
  integer,dimension(ilat1_ocn)::go_iaf
  integer::go_jsf
  integer,dimension(ilon1_ocn,ilat1_ocn)::go_k1
  real,dimension(1:inl1_ocn)::go_dz
  real,dimension(1:inl1_ocn)::go_dza
  real,dimension(0:ilat1_ocn)::go_c
  real,dimension(0:ilat1_ocn)::go_cv
  real,dimension(0:ilat1_ocn)::go_s
  real,dimension(0:ilat1_ocn)::go_sv
  real,dimension(intrac_ocn,ilon1_ocn,ilat1_ocn,inl1_ocn)::go_ts
  real,dimension(intrac_ocn,ilon1_ocn,ilat1_ocn,inl1_ocn)::go_ts1
  real,dimension(ilon1_ocn,ilat1_ocn)::go_cost
  real,dimension(1:3,ilon1_ocn,ilat1_ocn,1:inl1_ocn)::go_uvw
  real,dimension(1:2,ilon1_ocn,ilat1_ocn)::go_tau
  real,dimension(1:2,ilon1_ocn,ilat1_ocn)::eb_uv
  real,dimension(ilon1_ocn,ilat1_ocn)::eb_usurf
  REAL,DIMENSION(ilat1_ocn)::go_solfor
  REAL,DIMENSION(ilat1_atm)::solfor_atm
  REAL,DIMENSION(ilon1_ocn,ilat1_ocn)::go_fxsw
  REAL,DIMENSION(ilon1_ocn,ilat1_ocn)::go_mldta
  REAL,DIMENSION(0:ilon1_ocn,0:ilat1_ocn)::go_psi
  real,dimension(ilon1_ocn,ilat1_ocn,inl1_ocn)::go_diffv
  real,dimension(ilon1_ocn,ilat1_ocn,inl1_ocn)::go_dzrho

! *********************************

! genie-ents variables
! SG Variables declared in goldstein and embm
! that need to be made available to ents. The variable
! prefix reflects where they are originally defined, i.e.
! eb_ for embm and go_ for goldstein.

 integer go_istep0
 real :: go_rsc
 real :: go_syr
 integer :: go_nyear
 real :: eb_dphi
 real :: eb_rmax
 real :: eb_rdtdim
! real,dimension(ilat1_ocn)::eb_ds
 real,dimension(ilon1_ocn,ilat1_ocn)::eb_ca
! real,dimension(2,ilon1_ocn,ilat1_ocn)::eb_tq
! real,dimension(ilon1_ocn,ilat1_ocn) :: eb_co2
 character go_lin*13
 real ea_co2(ilon1_ocn,ilat1_ocn)
 real ea_fxplw(ilon1_ocn,ilat1_ocn)
 real,dimension(4)::go_ec
 real,dimension(ilon1_ocn,ilat1_ocn,inl1_ocn)::go_rho

! Variables for surflux_ents

 integer en_ntimes_max
 parameter (en_ntimes_max=50) 
 integer en_t_orog,en_norog,en_orogsteps
 integer en_t_lice,en_nlice,en_licesteps
 real en_orog_vect(ilon1_ocn,ilat1_ocn,en_ntimes_max)
 real en_lice_vect(ilon1_ocn,ilat1_ocn,en_ntimes_max)
 real eb_fx0a(ilon1_ocn,ilat1_ocn)
 real eb_fx0o(ilon1_ocn,ilat1_ocn)
 real eb_fxsen(ilon1_ocn,ilat1_ocn)
 real eb_fxlw(ilon1_ocn,ilat1_ocn)
 real eb_evap(ilon1_ocn,ilat1_ocn)
 real eb_pptn(ilon1_ocn,ilat1_ocn)
 real eb_relh(ilon1_ocn,ilat1_ocn)
 real,dimension(ilon1_atm,ilat1_atm)::torog_atm
 real,dimension(ilon1_lic,ilat1_lic)::landice_slicemask_lic
 real,dimension(ilon1_atm,ilat1_atm)::albs_atm
 real,dimension(ilon1_lnd,ilat1_lnd)::land_albs_snow_lnd
 real,dimension(ilon1_lnd,ilat1_lnd)::land_albs_nosnow_lnd
 real,dimension(ilon1_lnd,ilat1_lnd)::land_snow_lnd
 real,dimension(ilon1_lnd,ilat1_lnd)::land_bcap_lnd
 real,dimension(ilon1_lnd,ilat1_lnd)::land_z0_lnd
 real,dimension(ilon1_lnd,ilat1_lnd)::land_temp_lnd
 real,dimension(ilon1_lnd,ilat1_lnd)::land_moisture_lnd

! carbon variables from var_ents (for rokgem)
! prefix denotes module name - ENTS Land
  real el_leaf(ilon1_ocn,ilat1_ocn)         ! leaf litter (kgC/m2/yr) 
  real el_respveg(ilon1_ocn,ilat1_ocn)      ! vegetation respiration (kgC/m2/yr)
  real el_respsoil(ilon1_ocn,ilat1_ocn)     ! soil respiration (kgC/m2/yr) 
  real el_photo(ilon1_ocn,ilat1_ocn)         ! photosynthesis (kgC/m2/yr)

! *********************************

  ! ocean-atmosphere tracer interface arrays
  real,dimension(intrac_atm_max,ilon1_atm,ilat1_atm) :: genie_sfcatm    ! atmosphere-surface tracer composition; atm grid
  real,dimension(intrac_atm_max,ilon1_atm,ilat1_atm) :: genie_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
  real,dimension(intrac_atm_max,ilon1_ocn,ilat1_ocn) :: genie_sfcatm1   ! atmosphere-surface tracer composition; ocn grid
  real,dimension(intrac_atm_max,ilon1_lnd,ilat1_lnd) :: genie_sfcatm_lnd! atmosphere-surface tracer composition; lnd grid
  real,dimension(intrac_atm_max,ilon1_ocn,ilat1_ocn) :: genie_sfxatm1   ! atmosphere-surface fluxes; ocn grid
  real,dimension(intrac_atm_max,ilon1_lnd,ilat1_lnd) :: genie_sfxatm_lnd! land-atmosphere fluxes; lnd grid
  ! ocean-rock tracer interface arrays (for purposes of getting weathering flux from rockgem grid into biogem)
  real,dimension(intrac_ocn_max,ilon1_rok,ilat1_rok) :: genie_sfxrok    ! rock-surface(coastal ocean) fluxes; rok grid
  real,dimension(intrac_ocn_max,ilon1_ocn,ilat1_ocn) :: genie_sfxsumrok1! rock-surface(coastal ocean) fluxes; integrated ocn grid
  real,dimension(intrac_ocn_max,ilon1_ocn,ilat1_ocn) :: genie_sfxsumrok1_gem ! (version of above for GEMlite)
  real,dimension(intrac_atm_max,ilon1_ocn,ilat1_ocn) :: genie_sfxsumatm1_gem ! 
  ! atmosphere-rock tracer interface arrays (for purposes of getting temp and runoff into rokgem)
  ! oecan-sediment tracer interface arrays
  real,dimension(intrac_sed_max,ilon1_sed,ilat1_sed) :: genie_sfcsed    ! sediment-surface sediment composition; sed grid
  real,dimension(intrac_sed_max,ilon1_sed,ilat1_sed) :: genie_sfxsumsed ! sediment-surface (ocn->sed) fluxes; integrated, sed grid
  real,dimension(intrac_sed_max,ilon1_ocn,ilat1_ocn) :: genie_sfxsumsed1! sediment-surface (ocn->sed) fluxes; integrated, ocn grid
  real,dimension(intrac_ocn_max,ilon1_sed,ilat1_sed) :: genie_sfcsumocn ! sediment-surface ocean tracer comp; integrated, sed grid
  real,dimension(intrac_ocn_max,ilon1_sed,ilat1_sed) :: genie_sfxocn    ! sediment-surface (sed->ocn) fluxes; sed grid
  real,dimension(intrac_ocn_max,ilon1_ocn,ilat1_ocn) :: genie_sfxocn1   ! sediment-surface (sed->ocn) fluxes; ocn grid
  real,dimension(intrac_ocn_max,ilon1_ocn,ilat1_ocn) :: genie_sfcocn1   ! sediment-surface ocean tracer composition; ocn grid
  real,dimension(intrac_sed_max,ilon1_ocn,ilat1_ocn) :: genie_sfcsed1   ! sediment-surface sediment composition; ocn grid
  real,dimension(intrac_sed_max,ilon1_ocn,ilat1_ocn) :: genie_sfxsed1   ! sediment-surface (ocn->sed) fluxes; ocn grid
  ! ocean-ecology tracer interface arrays
  ! ecology-interface arrays
  real,dimension(intrac_sed_max,ilon1_ocn,ilat1_ocn,inl1_ocn) :: egbg_sfcpart  ! ecology-interface: particulate composition change; ocn grid
  real,dimension(intrac_ocn_max,ilon1_ocn,ilat1_ocn,inl1_ocn) :: egbg_sfcremin ! ecology-interface: ocean tracer composition change; ocn grid
  real,dimension(intrac_ocn_max,ilon1_ocn,ilat1_ocn,inl1_ocn) :: egbg_sfcocn   ! ecology-interface: ocean tracer composition; ocn grid
  ! temporary tracer arrays (for passing to/from GEMlite)
  real,dimension(intrac_atm_max,ilon1_atm,ilat1_atm)::genie_atm1 = 0.0   ! atmosphere tracers; ocn grid
  real,dimension(intrac_ocn_max,ilon1_ocn,ilat1_ocn,inl1_ocn)::genie_ocn = 0.0 ! ocean tracers; ocn grid
  integer::n_intrac_col = 0
! *********************************

! these are for whether we have a restart run or not
  logical lrestart_genie


! this is for the high-res orography field between igcm and glimmer
#ifndef IGCMHRLONS
#define IGCMHRLONS 320
#endif
#ifndef IGCMHRLATS
#define IGCMHRLATS 160
#endif

  integer nhrlon,nhrlat
  parameter(nhrlon=IGCMHRLONS,nhrlat=IGCMHRLATS)
  real hrlons_atm(nhrlon),hrlats_atm(nhrlat)
  real hrlonsedge_atm(nhrlon+1),hrlatsedge_atm(nhrlat+1)

#ifndef REV
#define REV 0
#endif

  parameter(genie_version=REV)

contains
  
  ! subroutine: increment_genie_clock
  ! The GENIE clock keeps track of elapsed model time
  ! (in s) which is available to all components.
  ! This routine increments the accumulated time by
  ! a 'GENIE timestep' 
  subroutine increment_genie_clock
    implicit none
    ! genie_timestep is in MILISECONDS
	! NOTE: this is to minimize numerical error (drift) when using time-steps that are not integer number of seconds
	!       e.g. 100 (ocean) time-steps per year (remainder 0.2s)
	! NOTE: this requires BIOGEM to each time divide this f*cker by 1000.0 ... :(
    genie_clock = genie_clock + nint(1000.0*genie_timestep)
  end subroutine increment_genie_clock

  ! getversion()
  ! Return the version of the model
  integer function getversion()
    implicit none
    getversion = genie_version
  end function getversion


END MODULE genie_global

