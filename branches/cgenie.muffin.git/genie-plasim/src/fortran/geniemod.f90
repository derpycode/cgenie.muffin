      module geniemod
      implicit none

      integer NLAT_ATM,NLEV_ATM,NPRO_ATM

      parameter(NLAT_ATM = 32)
      parameter(NLEV_ATM = 10)
      parameter(NPRO_ATM = 1)

! coupling inputs from genie
      real :: genie_sst(NLAT_ATM*NLAT_ATM*2)
      real :: genie_icet(NLAT_ATM*NLAT_ATM*2)
      real :: genie_hght_sic(NLAT_ATM*NLAT_ATM*2)
      real :: genie_frac_sic(NLAT_ATM*NLAT_ATM*2)
      real :: genie_alb_sic(NLAT_ATM*NLAT_ATM*2)
      real :: genie_dflux(4,NLAT_ATM*NLAT_ATM*2) !not used
      real :: genie_co2

! daily averages for use in AO gearing
      real :: g_latent_coeff_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_sensible_coeff_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_netsolar_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_solfor_plas(NLAT_ATM,360)
      real :: g_insolar_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_inlong_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_sat_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_spec_hum_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_pressure_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_evap_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_precip_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_runoff_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_stressx2_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_stressy2_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_stressx3_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_stressy3_plas(NLAT_ATM*2,NLAT_ATM,360)
      real :: g_windspeed_plas(NLAT_ATM*2,NLAT_ATM,360)

! ENTS carbon flux output
      real :: sfxatm_lnd(NLAT_ATM*2,NLAT_ATM)


! useful stuff (plasim grid with genie indexing)
      real :: genie_area(NLAT_ATM*2,NLAT_ATM)
      real :: genie_mask(NLAT_ATM*2,NLAT_ATM)
 
! IO Directories
      character :: indir_name*200
      character :: outdir_name*200
      character :: rstdir_name*200
      character :: runtime_root*200

! flux correction (GENIE lon-lat grid)
      real :: genie_apm(NLAT_ATM*2,NLAT_ATM)
      real :: scale_apm

!geared coupling
      integer :: ngear,ngear_years_plasim,ngear_multiple

!set whether carbon fluxes passed are back to genie
      logical :: atchem_couple

!time series of emissions GtC/year
      integer :: ntco2e !are emissions applied?
      real :: co2emissions(100000)

!time series of non-CO2 forcing
      integer :: nradfor !are emissions applied?
      real :: radfor(100000)


      end module geniemod
