! ******************************************************************************************************************************** !
! biogem_lib.f90
! BioGeM
! LIBRARY MODULE
! ******************************************************************************************************************************** !


MODULE biogem_lib


  use genie_control
  USE gem_util
  use gem_carbchem
  IMPLICIT NONE
  SAVE


  ! ****************************************************************************************************************************** !
  ! *** NAMELIST DEFINITIONS ***************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ### EDIT ADD AND/OR EXTEND NAME-LIST PARAMETER AND CONTROL OPTIONS ########################################################### !
  ! ------------------- TRACER INITIALIZATION ------------------------------------------------------------------------------------ !
  REAL,DIMENSION(n_ocn)::ocn_init                                       ! ocean tracer array initial values
  NAMELIST /ini_biogem_nml/ocn_init
  REAL,DIMENSION(n_ocn)::ocn_dinit                                      ! ocean tracer array restart perturbation values
  NAMELIST /ini_biogem_nml/ocn_dinit
  logical::ctrl_ocn_dinit                                               ! absolute (not relative) tracer re-start adjustment?
  NAMELIST /ini_biogem_nml/ctrl_ocn_dinit
  ! ------------------- RUN CONTROL ---------------------------------------------------------------------------------------------- !
  logical::ctrl_continuing                                              ! continuing run?
  NAMELIST /ini_biogem_nml/ctrl_continuing
  REAL::par_misc_t_start                                                ! start time (years)
  REAL::par_misc_t_runtime                                              ! run time (years)
  NAMELIST /ini_biogem_nml/par_misc_t_start,par_misc_t_runtime
  LOGICAL::ctrl_misc_t_BP                                               ! years before present?
  NAMELIST /ini_biogem_nml/ctrl_misc_t_BP
  REAL::par_misc_t_stop                                                ! stop time (years)
  NAMELIST /ini_biogem_nml/par_misc_t_stop
  ! ------------------- MISC CONTROL --------------------------------------------------------------------------------------------- !
  logical::ctrl_misc_Snorm                                              !
  logical::ctrl_misc_noSnorm                                            !
  logical::ctrl_misc_nobioupdate                                        !
  NAMELIST /ini_biogem_nml/ctrl_misc_Snorm,ctrl_misc_noSnorm,ctrl_misc_nobioupdate
  REAL::par_misc_brinerejection_frac                                    ! sea-ice brine rejection fraction
  integer::par_misc_brinerejection_jmax                                 ! max j for sea-ice brine rejection
  logical::ctrl_misc_brinerejection_bgc                                 ! include biogeochem in Sea-ice brine rejection?
  NAMELIST /ini_biogem_nml/par_misc_brinerejection_frac,par_misc_brinerejection_jmax,ctrl_misc_brinerejection_bgc
  CHARACTER(len=63)::opt_misc_geoeng                                    ! geoengineering scheme ID string (default: 'NONE')
  NAMELIST /ini_biogem_nml/opt_misc_geoeng
  CHARACTER(len=127)::par_misc_2D_file                                  ! filename of generic 2D field
  REAL::par_misc_2D_scale                                               ! scalar of generic 2D field
  NAMELIST /ini_biogem_nml/par_misc_2D_file,par_misc_2D_scale
  integer::par_misc_kmin_pipe                                           ! Min k for geoengineering ocean pipes!
  NAMELIST /ini_biogem_nml/par_misc_kmin_pipe
  logical::ctrl_misc_geoeng_noDIC                                       ! exclude DIC
  NAMELIST /ini_biogem_nml/ctrl_misc_geoeng_noDIC
  logical::ctrl_ocn_rst_reset_T                                         ! Overwrite restart temperatures?
  NAMELIST /ini_biogem_nml/ctrl_ocn_rst_reset_T
  logical::ctrl_carbchemupdate_full                                     ! Full (entire grid) carbonate chem update?
  NAMELIST /ini_biogem_nml/ctrl_carbchemupdate_full
  ! ------------------- BOUNDARY CONDITIONS -------------------------------------------------------------------------------------- !
  logical::ctrl_force_sed_closedsystem                                  ! Set dissolution flux = rain flux to close system?
  NAMELIST /ini_biogem_nml/ctrl_force_sed_closedsystem
  logical::ctrl_force_sed_closed_P                                      ! Balance the P cycle (with weathering)?
  NAMELIST /ini_biogem_nml/ctrl_force_sed_closed_P
  logical::ctrl_force_GOLDSTEInTS                                       ! Allow temperature / salinity forcing of climate?
  logical::ctrl_force_GOLDSTEInTSonly                                   ! Allow ONLY temperature / salinity forcing of climate?
  logical::ctrl_force_seaice                                            ! Replace internal fractional sea-ice cover field?
  logical::ctrl_force_windspeed                                         ! Replace internal wind-speed field?
  NAMELIST /ini_biogem_nml/ctrl_force_GOLDSTEInTS,ctrl_force_GOLDSTEInTSonly,ctrl_force_seaice,ctrl_force_windspeed
  logical::ctrl_force_CaCO3toPOCrainratio                               ! Replace internal CaCO3:POC export rain ratio?
  NAMELIST /ini_biogem_nml/ctrl_force_CaCO3toPOCrainratio
  logical::ctrl_force_POCdtoPOCrainratio                                ! Replace internal POCd:POC export rain ratio?
  NAMELIST /ini_biogem_nml/ctrl_force_POCdtoPOCrainratio
  logical::ctrl_force_Cd_alpha                                          ! Replace internal [Cd/P]POM/[Cd/P]SW alpha?
  NAMELIST /ini_biogem_nml/ctrl_force_Cd_alpha
  logical::ctrl_force_CaCO3ballastcoeff                                 ! Set spatial ballast coefficient distribution (CaCO3)
  logical::ctrl_force_opalballastcoeff                                  ! Set spatial ballast coefficient distribution (opal)
  logical::ctrl_force_detballastcoeff                                  ! Set spatial ballast coefficient distribution (det)
  NAMELIST /ini_biogem_nml/ctrl_force_CaCO3ballastcoeff,ctrl_force_opalballastcoeff,ctrl_force_detballastcoeff
  logical::ctrl_force_scav_fpart_POC                                    ! Replace internal POC flux for isotope scavenging
  logical::ctrl_force_scav_fpart_CaCO3                                  ! Replace internal CaCO3 flux for isotope scavenging
  logical::ctrl_force_scav_fpart_opal                                   ! Replace internal opal flux for isotope scavenging
  logical::ctrl_force_scav_fpart_det                                    ! Replace internal det flux for isotope scavenging
  NAMELIST /ini_biogem_nml/ctrl_force_scav_fpart_POC,ctrl_force_scav_fpart_CaCO3
  NAMELIST /ini_biogem_nml/ctrl_force_scav_fpart_opal,ctrl_force_scav_fpart_det
  REAL::par_gastransfer_a                                               ! Value of Wanninkhof [1992] gas transfer coeff (a)
  NAMELIST /ini_biogem_nml/par_gastransfer_a
  CHARACTER(len=127)::par_seaice_file                                   !
  CHARACTER(len=127)::par_windspeed_file                                !
  NAMELIST /ini_biogem_nml/par_seaice_file,par_windspeed_file
  CHARACTER(len=127)::par_CaCO3toPOCrainratio_file                      !
  CHARACTER(len=127)::par_POCdtoPOCrainratio_file                       !
  CHARACTER(len=127)::par_Cd_alpha_file                                 !
  NAMELIST /ini_biogem_nml/par_CaCO3toPOCrainratio_file,par_POCdtoPOCrainratio_file,par_Cd_alpha_file
  CHARACTER(len=127)::par_CaCO3ballastcoeff_file                        !
  CHARACTER(len=127)::par_opalballastcoeff_file                         !
  CHARACTER(len=127)::par_detballastcoeff_file                          !
  NAMELIST /ini_biogem_nml/par_CaCO3ballastcoeff_file,par_opalballastcoeff_file,par_detballastcoeff_file
  CHARACTER(len=127)::par_scav_fpart_POC_file                           !
  CHARACTER(len=127)::par_scav_fpart_CaCO3_file                         !
  CHARACTER(len=127)::par_scav_fpart_opal_file                          !
  CHARACTER(len=127)::par_scav_fpart_det_file                           !
  NAMELIST /ini_biogem_nml/par_scav_fpart_POC_file,par_scav_fpart_CaCO3_file,par_scav_fpart_opal_file,par_scav_fpart_det_file
  logical::ctrl_force_solconst                                          ! Replace solar constant?
  NAMELIST /ini_biogem_nml/ctrl_force_solconst
  logical::ctrl_force_oldformat                                         ! Use old tracer forcing file format?
  NAMELIST /ini_biogem_nml/ctrl_force_oldformat
  CHARACTER(len=127)::par_forcing_name                                  !
  NAMELIST /ini_biogem_nml/par_forcing_name
  CHARACTER(len=127)::opt_ocnatmH2S_fix
  NAMELIST /ini_biogem_nml/opt_ocnatmH2S_fix
  real::par_Fgeothermal                                        ! geothermal heat flux (W m-2)
  NAMELIST /ini_biogem_nml/par_Fgeothermal
  logical::ctrl_force_Fgeothermal2D                                    ! Use 2D geothermal heat input field?
  NAMELIST /ini_biogem_nml/ctrl_force_Fgeothermal2D
  CHARACTER(len=127)::par_force_Fgeothermal2D_file                          ! Filename for 2D geothermal heat input field
  NAMELIST /ini_biogem_nml/par_force_Fgeothermal2D_file
  ! ------------------- BIOLOGICAL NEW PRODUCTION -------------------------------------------------------------------------------- !
  CHARACTER(len=63)::par_bio_prodopt                             ! biological scheme ID string (e.g., 1N1T_PO4MM, 1N1T_PO4MM_Cd)
  NAMELIST /ini_biogem_nml/par_bio_prodopt
  real::par_bio_k0_PO4                                           ! base [PO4] uptake rate (mol kg-1 yr-1)
  real::par_bio_k0_NO3                                           ! base [NO3] uptake rate (mol kg-1 yr-1)
  NAMELIST /ini_biogem_nml/par_bio_k0_PO4,par_bio_k0_NO3
  real::par_bio_c0_PO4                                           ! [PO4] M-M half-sat value (mol kg-1)
  real::par_bio_c0_PO4_sp                                        ! [PO4] M-M half-sat value [SP]
  real::par_bio_c0_PO4_nsp                                       ! [PO4] M-M half-sat value [NSP]
  real::par_bio_c0_NO3                                           ! [NO3] M-M half-sat value (mol kg-1)
  real::par_bio_c0_N                                             ! [NO3]+[NH4] M-M half-sat value (mol kg-1)
  real::par_bio_c0_Fe                                            ! [Fe] M-M half-sat value (mol kg-1)
  real::par_bio_c0_Fe_Diaz                                       ! [Fe] M-M half-sat value for diazotrophs (mol kg-1)
  real::par_bio_c0_Fe_sp                                         ! [Fe] M-M half-sat value [SP]
  real::par_bio_c0_Fe_nsp                                        ! [Fe] M-M half-sat value [NSP]
  real::par_bio_c0_SiO2                                          ! [H4SiO4] M-M half-sat value (mol kg-1)
  real::par_bio_c0_SiO2_sp                                       ! [H4SiO4] M-M half-sat value [SP]
  real::par_bio_c0_SiO2_nsp                                      ! [H4SiO4] M-M half-sat value [NSP]
  NAMELIST /ini_biogem_nml/par_bio_c0_PO4,par_bio_c0_PO4_sp,par_bio_c0_PO4_nsp
  NAMELIST /ini_biogem_nml/par_bio_c0_NO3,par_bio_c0_N
  NAMELIST /ini_biogem_nml/par_bio_c0_Fe,par_bio_c0_Fe_sp,par_bio_c0_Fe_nsp,par_bio_c0_Fe_Diaz
  NAMELIST /ini_biogem_nml/par_bio_c0_SiO2,par_bio_c0_SiO2_sp,par_bio_c0_SiO2_nsp
  real::par_bio_tau                                              ! biological production time-scale (days) (OCMIP-2)
  real::par_bio_tau_sp                                           ! biological production time-scale -- SP
  real::par_bio_tau_nsp                                          ! biological production time-scale -- NSP
  real::par_bio_relprod_sp                                       ! uptake rate modification factor for siliceous phytoplankton
  NAMELIST /ini_biogem_nml/par_bio_tau,par_bio_tau_sp,par_bio_tau_nsp,par_bio_relprod_sp
  real::par_bio_zc                                               ! biological production zone depth (m) (OCMIP-2)
  real::par_bio_I_eL                                             ! light e-folding depth (m) (OCMIP-2)
  real::par_bio_kT0                                              ! coefficient for temperature-dependent uptake rate modifier
  real::par_bio_kT_eT                                            ! e-folding temp (K) for temp-dependent uptake rate modifier
  NAMELIST /ini_biogem_nml/par_bio_zc,par_bio_I_eL,par_bio_kT0,par_bio_kT_eT
  ! ------------------- ORGANIC MATTER EXPORT RATIOS ----------------------------------------------------------------------------- !
  real::par_bio_red_POP_PON                                             ! N/P organic matter Redfield ratio
  real::par_bio_red_POP_POC                                             ! C/P organic matter Redfield ratio
  real::par_bio_red_POP_PO2                                             ! O2/P organic matter pseudo-Redfield ratio
  real::par_bio_red_PON_ALK                                             ! ALK/N alkalinty correction factor
  NAMELIST /ini_biogem_nml/par_bio_red_POP_PON,par_bio_red_POP_POC,par_bio_red_POP_PO2,par_bio_red_PON_ALK
  integer::par_bio_red_PC_flex                                          ! C/P organic matter flexible stoichiometry activation
  real::par_bio_red_PC_alpha1                                           ! scaling of C/P flexible stoichiometry
  real::par_bio_red_PC_alpha2                                           ! offset of C/P flexible stoichiometry
  NAMELIST /ini_biogem_nml/par_bio_red_PC_flex,par_bio_red_PC_alpha1,par_bio_red_PC_alpha2
  real::par_bio_red_PC_max                                              ! maximum C/P
  NAMELIST /ini_biogem_nml/par_bio_red_PC_max
  real::par_bio_red_DOMfrac                                             ! production fraction of dissolved organic matter
  NAMELIST /ini_biogem_nml/par_bio_red_DOMfrac
  real::par_bio_red_RDOMfrac                                            ! production fraction of R-dissolved organic matter
  NAMELIST /ini_biogem_nml/par_bio_red_RDOMfrac
  real::par_bio_red_rP_POM_DOM                                          ! P:C fractionation during POM->DOM production
  real::par_bio_red_rN_POM_DOM                                          ! N:C fractionation during POM->DOM production
  NAMELIST /ini_biogem_nml/par_bio_red_rP_POM_DOM,par_bio_red_rN_POM_DOM
  real::par_bio_red_rP_POM_RDOM                                         ! P:C fractionation during POM->RDOM production
  real::par_bio_red_rN_POM_RDOM                                         ! N:C fractionation during POM->RDOM production
  NAMELIST /ini_biogem_nml/par_bio_red_rP_POM_RDOM,par_bio_red_rN_POM_RDOM
  LOGICAL::ctrl_bio_red_ALKwithPOC                                      ! tie ALK with POC (rather than POP)
  NAMELIST /ini_biogem_nml/ctrl_bio_red_ALKwithPOC
  LOGICAL::ctrl_bio_red_O2withPOC                                       ! tie O2 with POC (rather than POP)
  NAMELIST /ini_biogem_nml/ctrl_bio_red_O2withPOC
  ! ------------------- INORGANIC MATTER EXPORT RATIOS --------------------------------------------------------------------------- !
  CHARACTER(len=63)::opt_bio_CaCO3toPOCrainratio                        ! CaCO3:POC rain ratio option ID (e.g. 'fixed')
  NAMELIST /ini_biogem_nml/opt_bio_CaCO3toPOCrainratio
  real::par_bio_red_POC_CaCO3_pP                                        ! exponent for modifier of CaCO3:POC export ratio
  real::par_bio_red_POC_CaCO3                                           ! base CaCO3:POC export ratio
  NAMELIST /ini_biogem_nml/par_bio_red_POC_CaCO3,par_bio_red_POC_CaCO3_pP
  real::par_bio_red_POC_CaCO3_Kmax                                      ! ohmega half-sat constant [Gehlen et al., 2007]
  real::par_bio_red_POC_CaCO3_CO2aqREF                                  ! Heinze [2004] CO2aq reference concentration (umol kg-1)
  real::par_bio_red_POC_CaCO3_CO3REF                                    ! Barker et al. [2003] CO3 reference conc (umol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_red_POC_CaCO3_Kmax,par_bio_red_POC_CaCO3_CO2aqREF,par_bio_red_POC_CaCO3_CO3REF
  real::par_bio_red_POC_opal                                            !
  NAMELIST /ini_biogem_nml/par_bio_red_POC_opal
  real::par_part_red_opal_FeTKSp                                        ! Ridgwell [2001] -- opal:POC KSp for FeT
  real::par_part_red_opal_FeToff                                        ! Ridgwell [2001] -- opal:POC offset for FeT
  NAMELIST /ini_biogem_nml/par_part_red_opal_FeTKSp,par_part_red_opal_FeToff
  CHARACTER(len=63)::opt_bio_red_SitoC                                  ! opal:POC rain ratio option ID (e.g. 'fixed' == default)
  NAMELIST /ini_biogem_nml/opt_bio_red_SitoC
  ! ------------------- REMINERALIZATION ----------------------------------------------------------------------------------------- !
  real::par_bio_remin_RDOMfrac                                          ! fraction of POM remin concverted to RDOM
  NAMELIST /ini_biogem_nml/par_bio_remin_RDOMfrac
  real::par_bio_remin_DOMlifetime                                       ! DOC lifetime (yrs)
  NAMELIST /ini_biogem_nml/par_bio_remin_DOMlifetime
  real::par_bio_remin_RDOMlifetime                                      ! RDOC lifetime (yrs)
  NAMELIST /ini_biogem_nml/par_bio_remin_RDOMlifetime
  LOGICAL::ctrl_bio_remin_RDOM_photolysis                               ! RDOM degradation by (surface) photolysis only?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_RDOM_photolysis
  LOGICAL::ctrl_bio_remin_POC_fixed                              ! fixed-profile POM remineralization
  LOGICAL::ctrl_bio_remin_CaCO3_fixed                            ! fixed-profile CaCO3 remineralization
  LOGICAL::ctrl_bio_remin_opal_fixed                             ! fixed-profile opal remineralization
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_POC_fixed,ctrl_bio_remin_CaCO3_fixed,ctrl_bio_remin_opal_fixed
  LOGICAL::ctrl_bio_remin_POC_kinetic
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_POC_kinetic
  CHARACTER(len=63)::par_bio_remin_fun                           ! Remineralization functional form'
  NAMELIST /ini_biogem_nml/par_bio_remin_fun
  LOGICAL::ctrl_bio_remin_POC_ballast                            ! ballasting parameterization?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_POC_ballast
  real::par_bio_remin_POC_frac2                                  ! initial fractional abundance of POC component (#2)
  real::par_bio_remin_CaCO3_frac2                                ! initial fractional abundance of CaCO3 component (#2)
  real::par_bio_remin_opal_frac2                                 ! initial fractional abundance of opal component (#2)
  NAMELIST /ini_biogem_nml/par_bio_remin_POC_frac2,par_bio_remin_CaCO3_frac2,par_bio_remin_opal_frac2
  real::par_bio_remin_POC_dfrac2                                 ! range of fractional abundance of POC component (#2)
  real::par_bio_remin_POC_c0frac2                                ! fractional abundance of POC #2 half-sat
  NAMELIST /ini_biogem_nml/par_bio_remin_POC_dfrac2,par_bio_remin_POC_c0frac2
  real::par_bio_remin_POC_eL1                                    ! remineralization length #1 for POC
  real::par_bio_remin_POC_eL2                                    ! remineralization length #2 for POC
  real::par_bio_remin_CaCO3_eL1                                  ! remineralization length #1 for CaCO3
  real::par_bio_remin_CaCO3_eL2                                  ! remineralization length #2 for CaCO3
  real::par_bio_remin_opal_eL1                                   ! remineralization length #1 for opal
  real::par_bio_remin_opal_eL2                                   ! remineralization length #2 for opal
  NAMELIST /ini_biogem_nml/par_bio_remin_POC_eL1,par_bio_remin_POC_eL2
  NAMELIST /ini_biogem_nml/par_bio_remin_CaCO3_eL1,par_bio_remin_CaCO3_eL2
  NAMELIST /ini_biogem_nml/par_bio_remin_opal_eL1,par_bio_remin_opal_eL2
  real::par_bio_remin_martin_b                                   ! Power law power for POC
  real::par_bio_remin_z0                                         ! Power law z0 for POC
  NAMELIST /ini_biogem_nml/par_bio_remin_martin_b,par_bio_remin_z0
  real::par_bio_remin_POC_K1,par_bio_remin_POC_K2
  real::par_bio_remin_POC_Ea1,par_bio_remin_POC_Ea2
  NAMELIST /ini_biogem_nml/par_bio_remin_POC_K1,par_bio_remin_POC_K2
  NAMELIST /ini_biogem_nml/par_bio_remin_POC_Ea1,par_bio_remin_POC_Ea2
  real::par_bio_remin_sinkingrate                                ! prescribed particle sinking rate (m d-1)
  real::par_bio_remin_sinkingrate_scav                           ! sinking rate (for calculating scavenging) (m d-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_sinkingrate,par_bio_remin_sinkingrate_scav
  real::par_bio_remin_sinkingrate_physical                       ! Explicit physical particle sinking rate (m d-1)
  real::par_bio_remin_sinkingrate_reaction                       ! Implicit sinking rate for geochem reactions (m d-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_sinkingrate_physical,par_bio_remin_sinkingrate_reaction
  real::par_bio_remin_ballast_kc                                 ! organic matter carrying capacity of CaCO3
  real::par_bio_remin_ballast_ko                                 ! organic matter carrying capacity of opal
  real::par_bio_remin_ballast_kl                                 ! organic matter carrying capacity of detital (lithogenic)
  NAMELIST /ini_biogem_nml/par_bio_remin_ballast_kc,par_bio_remin_ballast_ko,par_bio_remin_ballast_kl
  LOGICAL::ctrl_bio_remin_ONtoNH4                                ! Aerobic remineralization of ON -> NH4 (not NO3)?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_ONtoNH4
  REAL::par_bio_remin_denitrO2thresh                             ! Denitrification [O2] threshold (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_denitrO2thresh
  LOGICAL::ctrl_bio_remin_thresh                                 ! Apply a hard tracer oxidant remin threshold?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_thresh
  real::par_bio_remin_cthresh_O2                                 ! Hard threshold for oxic remin (mol kg-1)
  real::par_bio_remin_cthresh_NO3                                ! Hard threshold for denitrification (mol kg-1)
  real::par_bio_remin_cthresh_SO4                                ! Hard threshold for sulphate reduction (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_cthresh_O2,par_bio_remin_cthresh_NO3,par_bio_remin_cthresh_SO4
  LOGICAL::ctrl_bio_remin_reminfix                               ! Catch mis-behaving rapidly-oxidizing species going < 0.0?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_reminfix
  CHARACTER(len=63)::opt_bio_remin_oxidize_NH4toNO3              ! NH4 -> NO3 oxidation option
  CHARACTER(len=63)::opt_bio_remin_oxidize_H2StoSO4              ! H2S -> SO4 oxidation option
  NAMELIST /ini_biogem_nml/opt_bio_remin_oxidize_NH4toNO3,opt_bio_remin_oxidize_H2StoSO4
  CHARACTER(len=63)::opt_bio_remin_scavenge_H2StoPOMS
  NAMELIST /ini_biogem_nml/opt_bio_remin_scavenge_H2StoPOMS      ! H2S -> POMS
  CHARACTER(len=63)::par_bio_remin_CH4ox                         ! aerobic methanotrophy scheme ID string
  NAMELIST /ini_biogem_nml/par_bio_remin_CH4ox
  real::par_bio_remin_CH4rate                                    ! specific CH4 oxidation rate (d-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_CH4rate
  real::par_bio_remin_AER_kAER                                   ! AER rate constant (yr-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_AER_kAER
  real::par_bio_remin_AER_Km_O2                                  ! AER O2 half sat constant (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_AER_Km_O2
  CHARACTER(len=63)::par_bio_remin_AER_thermo                    ! AER thermodynamic drive ID string
  NAMELIST /ini_biogem_nml/par_bio_remin_AER_thermo
  real::par_bio_remin_AER_dG0                                    ! Std Gibbs free energy of AOM (kJ mol-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_AER_dG0
  real::par_bio_remin_AER_BEQ                                    ! AER biological energy quantum (kJ mol-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_AER_BEQ
  real::par_bio_remin_AOM_kAOM                                   ! AOM rate constant (yr-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_AOM_kAOM
  real::par_bio_remin_AOM_Km_SO4                                 ! AOM SO4 half sat constant (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_AOM_Km_SO4
  CHARACTER(len=63)::par_bio_remin_AOM_thermo                    ! AOM thermodynamic drive ID string
  NAMELIST /ini_biogem_nml/par_bio_remin_AOM_thermo
  real::par_bio_remin_AOM_dG0                                    ! Std Gibbs free energy of AOM (kJ mol-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_AOM_dG0
  real::par_bio_remin_AOM_BEQ                                    ! AOM biological energy quantum (kJ mol-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_AOM_BEQ
  real::par_bio_remin_Rgas                                       ! Gas constant for thermo calculations (kJ K-1 mol-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_Rgas
  real::par_bio_remin_gammaO2                                    ! Activity coefficient for dissolved O2
  NAMELIST /ini_biogem_nml/par_bio_remin_gammaO2
  real::par_bio_remin_gammaCO2                                   ! Activity coefficient for aqueous CO2
  NAMELIST /ini_biogem_nml/par_bio_remin_gammaCO2
  real::par_bio_remin_gammaHS                                    ! Activity coefficient for dissolved HS
  NAMELIST /ini_biogem_nml/par_bio_remin_gammaHS
  real::par_bio_remin_gammaHCO3                                  ! Activity coefficient for dissolved HCO3
  NAMELIST /ini_biogem_nml/par_bio_remin_gammaHCO3
  real::par_bio_remin_gammaSO4                                   ! Activity coefficient for dissolved SO4
  NAMELIST /ini_biogem_nml/par_bio_remin_gammaSO4
  real::par_bio_remin_gammaCH4                                   ! Activity coefficient for aqueous CH4
  NAMELIST /ini_biogem_nml/par_bio_remin_gammaCH4
  ! JDW size-dependent remin
  real::par_bio_remin_POC_eL0                                    ! e-folding depth of smallest ecogem size class (m)
  real::par_bio_remin_POC_size0                                  ! diameter of smallest ecogem size class (um)
  real::par_bio_remin_POC_eta                                    ! exponent linking sinking speed and size (Stemmann et al., 2004)
  NAMELIST / ini_biogem_nml / par_bio_remin_POC_eL0,par_bio_remin_POC_size0,par_bio_remin_POC_eta
  ! kinetics
  real::par_bio_remin_k_O2
  real::par_bio_remin_k_NO3
  real::par_bio_remin_k_SO4
  real::par_bio_remin_k_meth
  NAMELIST /ini_biogem_nml/par_bio_remin_k_O2,par_bio_remin_k_NO3,par_bio_remin_k_SO4,par_bio_remin_k_meth
  real::par_bio_remin_c0_O2
  real::par_bio_remin_c0_NO3
  real::par_bio_remin_c0_SO4
  NAMELIST /ini_biogem_nml/par_bio_remin_c0_O2,par_bio_remin_c0_NO3,par_bio_remin_c0_SO4
  real::par_bio_remin_ci_O2
  real::par_bio_remin_ci_NO3
  real::par_bio_remin_ci_SO4
  NAMELIST /ini_biogem_nml/par_bio_remin_ci_O2,par_bio_remin_ci_NO3,par_bio_remin_ci_SO4
  ! S
  real::par_bio_remin_kH2StoSO4
  real::par_bio_remin_kH2StoPOMS
  NAMELIST /ini_biogem_nml/par_bio_remin_kH2StoSO4,par_bio_remin_kH2StoPOMS
  ! N
  real::par_bio_remin_kNH4toNO2
  real::par_bio_remin_kNO2toNO3
  real::par_bio_remin_kNO2toN2O
  NAMELIST /ini_biogem_nml/par_bio_remin_kNH4toNO2,par_bio_remin_kNO2toNO3,par_bio_remin_kNO2toN2O
  real::par_bio_remin_cNH4_NH4toNO2
  real::par_bio_remin_cO2_NH4toNO2
  NAMELIST /ini_biogem_nml/par_bio_remin_cNH4_NH4toNO2,par_bio_remin_cO2_NH4toNO2
  real::par_bio_remin_cNO2_NO2toNO3
  real::par_bio_remin_cO2_NO2toNO3
  NAMELIST /ini_biogem_nml/par_bio_remin_cNO2_NO2toNO3,par_bio_remin_cO2_NO2toNO3
  real::par_bio_remin_cNO2_NO2toN2O
  real::par_bio_remin_cO2_NO2toN2O
  NAMELIST /ini_biogem_nml/par_bio_remin_cNO2_NO2toN2O,par_bio_remin_cO2_NO2toN2O
  real::par_bio_remin_fracN2O
  NAMELIST /ini_biogem_nml/par_bio_remin_fracN2O
  ! I
  CHARACTER(len=63)::opt_bio_remin_oxidize_ItoIO3              ! I -> IO3 oxidation option
  CHARACTER(len=63)::opt_bio_remin_reduce_IO3toI              ! IO3 -> I reduction option
  NAMELIST /ini_biogem_nml/opt_bio_remin_oxidize_ItoIO3,opt_bio_remin_reduce_IO3toI
  real::par_bio_remin_Ilifetime
  NAMELIST /ini_biogem_nml/par_bio_remin_Ilifetime
  real::par_bio_remin_kItoIO3
  real::par_bio_remin_kIO3toI
  NAMELIST /ini_biogem_nml/par_bio_remin_kItoIO3,par_bio_remin_kIO3toI
  real::par_bio_remin_cO2_IO3toI
  real::par_bio_remin_cO2_ItoIO3
  real::par_bio_remin_cIO3_IO3toI
  NAMELIST /ini_biogem_nml/par_bio_remin_cO2_IO3toI,par_bio_remin_cO2_ItoIO3,par_bio_remin_cIO3_IO3toI
  ! ------------------- ISOTOPIC FRACTIONATION ----------------------------------------------------------------------------------- !
  CHARACTER(len=63)::opt_d13C_DIC_Corg                           ! Corg 13C fractionation scheme ID string
  NAMELIST /ini_biogem_nml/opt_d13C_DIC_Corg
  CHARACTER(len=63)::opt_d44Ca_Ca_CaCO3                          ! CaCO3 44Ca fractionation scheme ID string
  NAMELIST /ini_biogem_nml/opt_d44Ca_Ca_CaCO3
  real::par_d13C_DIC_Corg_b                                      ! b value for Popp et al. fractionation
  NAMELIST /ini_biogem_nml/par_d13C_DIC_Corg_b
  real::par_d13C_DIC_Corg_ef                                     ! frac for intercellular C fix
  real::par_d13C_DIC_Corg_ef_sp                                  ! frac for intercellular C fix of siliceous phytoplankton
  real::par_d13C_DIC_Corg_ef_nsp                                 ! frac for intercellular C fix of non-siliceous phytoplankton
  NAMELIST /ini_biogem_nml/par_d13C_DIC_Corg_ef,par_d13C_DIC_Corg_ef_sp,par_d13C_DIC_Corg_ef_nsp
  real::par_d30Si_opal_epsilon                                   ! fractionation of 30Si during opal formation by diatoms
  NAMELIST /ini_biogem_nml/par_d30Si_opal_epsilon
  real::par_d114Cd_POCd_epsilon                                  ! *** d114Cd = 1.0006 ***
  namelist /ini_biogem_nml/par_d114Cd_POCd_epsilon
  real::par_d114Cd_CdCO3_epsilon                                 ! 114/???Cd fractionation between Cd and CdCO3
  namelist /ini_biogem_nml/par_d114Cd_CdCO3_epsilon
  real::par_d7Li_LiCO3_epsilon                                   ! 7/6Li fractionation between Li and LiCO3
  namelist /ini_biogem_nml/par_d7Li_LiCO3_epsilon
  CHARACTER(len=63)::opt_bio_foram_p_13C_delta                   ! planktic foram tracer 13C fractionation scheme
  NAMELIST /ini_biogem_nml/opt_bio_foram_p_13C_delta
  real::par_d44Ca_CaCO3_epsilon                                  ! 44/40Ca fractionation between Ca and CaCO3
  namelist /ini_biogem_nml/par_d44Ca_CaCO3_epsilon
  real::par_d88Sr_SrCO3_epsilon                                  ! 88/86Sr fractionation between Sr and SrCO3
  namelist /ini_biogem_nml/par_d88Sr_SrCO3_epsilon
  real::par_d13C_Corg_CH4_epsilon                                ! 'methanogenesis fractionation
  namelist /ini_biogem_nml/par_d13C_Corg_CH4_epsilon
  ! ------------------- IRON CYCLING --------------------------------------------------------------------------------------------- !
  real::par_det_Fe_sol                                           ! fractional solubility of Fe in dust
  real::par_det_Fe_sol_exp                                       ! exponent for aeolian Fe solubility
  NAMELIST /ini_biogem_nml/par_det_Fe_sol,par_det_Fe_sol_exp
  LOGICAL::ctrl_bio_red_fixedFetoC                               ! fixed cellular Fe:C ratio?
  NAMELIST /ini_biogem_nml/ctrl_bio_red_fixedFetoC
  real::par_bio_red_POFe_POC                                     ! Fe:C 'Redfield' ratio
  NAMELIST /ini_biogem_nml/par_bio_red_POFe_POC
  LOGICAL::ctrl_bio_Fe_fixedKscav                                ! Fixed scavening rate (if not: Parekh scheme)?
  NAMELIST /ini_biogem_nml/ctrl_bio_Fe_fixedKscav
  real::par_scav_Fe_Ks                                           ! Fixed Fe scavenging rate (d-1)
  NAMELIST /ini_biogem_nml/par_scav_Fe_Ks
  real::par_scav_Fe_sf_POC                                       ! Parekh Fe scavenging rate scale factor - POC
  real::par_scav_Fe_sf_CaCO3                                     ! Parekh Fe scavenging rate scale factor - CaCO3
  real::par_scav_Fe_sf_opal                                      ! Parekh Fe scavenging rate scale factor - opal
  real::par_scav_Fe_sf_det                                       ! Parekh Fe scavenging rate scale factor - det
  NAMELIST /ini_biogem_nml/par_scav_Fe_sf_POC,par_scav_Fe_sf_CaCO3,par_scav_Fe_sf_opal,par_scav_Fe_sf_det
  real::par_scav_fremin                                          ! Fraction of scavenged Fe that can be remineralized
  logical::ctrl_bio_NO_fsedFe                                    ! Prevent return of Fe from the sediments?
  NAMELIST /ini_biogem_nml/par_scav_fremin,ctrl_bio_NO_fsedFe
  real::par_K_FeL_pP
  NAMELIST /ini_biogem_nml/par_K_FeL_pP                          ! log10 of Fe ligand stability constant K'(FeL)
  real::par_bio_FetoC_pP                                         ! [FeT] dependent Fe:C ratio [Ridgwell, 2001] -- power
  real::par_bio_FetoC_K                                          ! [FeT] dependent Fe:C ratio [Ridgwell, 2001] -- scaling
  real::par_bio_FetoC_C                                          ! [FeT] dependent Fe:C ratio [Ridgwell, 2001] -- constant
  NAMELIST /ini_biogem_nml/par_bio_FetoC_pP,par_bio_FetoC_K,par_bio_FetoC_C
  logical::ctrl_bio_red_Ridgwell2001FetoC                        ! Use Ridgwell [2001] sp and nsp Fe:C functions?
  NAMELIST /ini_biogem_nml/ctrl_bio_red_Ridgwell2001FetoC
  CHARACTER(len=127)::opt_geochem_Fe
  NAMELIST /ini_biogem_nml/opt_geochem_Fe
  CHARACTER(len=127)::par_lookup_Fe3_file
  CHARACTER(len=127)::par_lookup_geo_file
  NAMELIST /ini_biogem_nml/par_lookup_Fe3_file,par_lookup_geo_file
  CHARACTER(len=127)::par_lookup_Fe_file_1
  CHARACTER(len=127)::par_lookup_Fe_file_2
  CHARACTER(len=127)::par_lookup_Fe_file_3
  CHARACTER(len=127)::par_lookup_Fe_file_4
  NAMELIST /ini_biogem_nml/par_lookup_Fe_file_1,par_lookup_Fe_file_2,par_lookup_Fe_file_3,par_lookup_Fe_file_4
  ! ------------------- SILICIC ACID CYCLING ------------------------------------------------------------------------------------- !
  real::par_bio_remin_opal_K                                     ! opal particulate base dissolution rate (d-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_opal_K
  ! ------------------- NITROGEN CYCLING ----------------------------------------------------------------------------------------- !
  real::par_bio_mu1                                              ! mu-1 maximum rate of export production (yr-1)
  real::par_bio_mu2                                              ! mu-2 maximum rate of export production from N2-fixation (yr-1)
  real::par_bio_N2fixthresh                                      ! threshold NO3+NH4 to encourage N2 fixation (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_mu1,par_bio_mu2,par_bio_N2fixthresh
  real::par_bio_NPdiaz                                           ! N:P elemental ratio of diazotrophs (mol:mol)
  real::par_bio_N2fixdyn                                         ! constant for dynamical threshold to encourage N2 fixation
  NAMELIST /ini_biogem_nml/par_bio_NPdiaz,par_bio_N2fixdyn
  real::par_bio_Nstar_offset                                     ! N* offset (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_Nstar_offset
  real::par_nitri_mu                                             ! NH4 oxidation rate constant (yr-1)
  real::par_nitri_c0_NH4                                         ! NH4 half-saturatation constant for NH4 oxidation (mol kg-1)
  real::par_nitri_c0_O2                                          ! O2 half-saturatation constant for NH4 oxidation (mol kg-1)
  NAMELIST /ini_biogem_nml/par_nitri_mu,par_nitri_c0_NH4,par_nitri_c0_O2
  ! ------------------- TRACE METAL/ELEMENT CYCLING ------------------------------------------------------------------------------ !
  real::par_bio_red_POC_POCd                                     ! Default cellular C:Cd (Cd/C) ratio
  real::par_bio_red_POC_POCd_alpha                               ! [Cd/P]POM/[Cd/P]SW partition coefficient (alpha)
  NAMELIST /ini_biogem_nml/par_bio_red_POC_POCd,par_bio_red_POC_POCd_alpha
  LOGICAL::ctrl_bio_red_CdtoC_Felim                              ! Fe-limitation dependent Cd:C 'Redfield' uptake ratio?
  NAMELIST /ini_biogem_nml/ctrl_bio_red_CdtoC_Felim
  real::par_bio_red_CdtoC_Felim_min                              ! minimum (Fe replete) Cd:C 'Redfield' uptake ratio
  real::par_bio_red_CdtoC_Felim_max                              ! maximum (Fe limited) Cd:C 'Redfield' uptake ratio
  NAMELIST /ini_biogem_nml/par_bio_red_CdtoC_Felim_min,par_bio_red_CdtoC_Felim_max
  real::par_bio_red_CaCO3_LiCO3                                     ! Default CaCO3 Li/Ca ratio
  real::par_bio_red_CaCO3_LiCO3_alpha                               ! partition coefficient (alpha)
  NAMELIST /ini_biogem_nml/par_bio_red_CaCO3_LiCO3,par_bio_red_CaCO3_LiCO3_alpha
  real::par_bio_red_CaCO3_CdCO3                                     ! Default CaCO3 Cd/Ca ratio
  real::par_bio_red_CaCO3_CdCO3_alpha                               ! partition coefficient (alpha)
  NAMELIST /ini_biogem_nml/par_bio_red_CaCO3_CdCO3,par_bio_red_CaCO3_CdCO3_alpha
  real::par_bio_red_CaCO3_SrCO3                                     ! Default CaCO3 Sr/Ca ratio
  real::par_bio_red_CaCO3_SrCO3_alpha                               ! partition coefficient (alpha)
  NAMELIST /ini_biogem_nml/par_bio_red_CaCO3_SrCO3,par_bio_red_CaCO3_SrCO3_alpha
  real::par_bio_red_POC_POI                                     ! Default cellular C:I (I/C) ratio
  real::par_bio_red_POC_POI_C0                                  ! Reference [IO3-] value @ default C:I ratio
  NAMELIST /ini_biogem_nml/par_bio_red_POC_POI,par_bio_red_POC_POI_C0
  ! ------------------- 230Th AND 231Pa CYCLING ---------------------------------------------------------------------------------- !
  CHARACTER(len=63)::par_scav_230Th_scavopt                     ! scavenging scheme ID string (e.g., 'equilibrium') for 230Th
  CHARACTER(len=63)::par_scav_231Pa_scavopt                     ! scavenging scheme ID string (e.g., 'equilibrium') for 231Pa
  NAMELIST /ini_biogem_nml/par_scav_230Th_scavopt,par_scav_231Pa_scavopt
  real::par_scav_230Th_KPOC                                     ! eqm scavenging coefficient for POC associated 230Th
  real::par_scav_230Th_KCaCO3                                   ! eqm scavenging coefficient for calciate associated 230Th
  real::par_scav_230Th_Kopal                                    ! eqm scavenging coefficient for opal associated 230Th
  real::par_scav_230Th_Kdet                                     ! eqm scavenging coefficient for detrital matter associated 230Th
  NAMELIST /ini_biogem_nml/par_scav_230Th_KPOC,par_scav_230Th_KCaCO3,par_scav_230Th_Kopal,par_scav_230Th_Kdet
  real::par_scav_231Pa_KPOC                                     ! eqm scavenging coefficient for POC associated 231Pa
  real::par_scav_231Pa_KCaCO3                                   ! eqm scavenging coefficient for calciate associated 231Pa
  real::par_scav_231Pa_Kopal                                    ! eqm scavenging coefficient for opal associated 231Pa
  real::par_scav_231Pa_Kdet                                     ! eqm scavenging coefficient for detrital matter associated 231Pa
  NAMELIST /ini_biogem_nml/par_scav_231Pa_KPOC,par_scav_231Pa_KCaCO3,par_scav_231Pa_Kopal,par_scav_231Pa_Kdet
  real::par_scav_230Th_indepsinkingvel
  real::par_scav_231Pa_indepsinkingvel
  NAMELIST /ini_biogem_nml/par_scav_230Th_indepsinkingvel,par_scav_231Pa_indepsinkingvel
  ! ------------------- ABIOTIC PRECIPITATION ------------------------------------------------------------------------------------ !
  real::par_bio_CaCO3precip_sf                                   ! Scale factor for CaCO3 precipitation
  real::par_bio_CaCO3precip_exp                                  ! Rate law power for CaCO3 precipitation
  NAMELIST /ini_biogem_nml/par_bio_CaCO3precip_sf,par_bio_CaCO3precip_exp
  LOGICAL::ctrl_bio_CaCO3precip                                  ! Allow abiotic CaCO3 precipitation?
  LOGICAL::ctrl_bio_CaCO3precip_sur                              ! Restrict precipitation to surface layer?
  NAMELIST /ini_biogem_nml/ctrl_bio_CaCO3precip,ctrl_bio_CaCO3precip_sur
  logical::par_bio_CaCO3precip_calcite                           ! Precipitate as calcite (otherwise aragonite)
  real::par_bio_CaCO3precip_abioticohm_min                       ! Minimum ohmega threshold for precip
  NAMELIST /ini_biogem_nml/par_bio_CaCO3precip_calcite,par_bio_CaCO3precip_abioticohm_min
  ! ------------------- I/O DIRECTORY DEFINITIONS -------------------------------------------------------------------------------- !
  CHARACTER(len=255)::par_pindir_name                             !
  CHARACTER(len=255)::par_indir_name                             !
  CHARACTER(len=255)::par_outdir_name                            !
  CHARACTER(len=255)::par_rstdir_name                            !
  CHARACTER(len=255)::par_fordir_name                            !
  NAMELIST /ini_biogem_nml/par_indir_name,par_outdir_name,par_rstdir_name,par_fordir_name,par_pindir_name
  CHARACTER(len=127)::par_infile_name,par_outfile_name           !
  NAMELIST /ini_biogem_nml/par_infile_name,par_outfile_name
  ! ------------------- DATA SAVING: TIME-SLICES --------------------------------------------------------------------------------- !
  LOGICAL::ctrl_data_save_slice_ocnatm                           ! time-slice data save: Atmospheric (interface) composition (2D)?
  LOGICAL::ctrl_data_save_slice_ocn                              ! time-slice data save: Ocean composition (3D)?
  LOGICAL::ctrl_data_save_slice_ocnsed                           ! time-slice data save: Sediment (interface) composition (2D)?
  LOGICAL::ctrl_data_save_slice_fairsea                          ! time-slice data save: Air-sea gas exchange (2D)?
  LOGICAL::ctrl_data_save_slice_focnatm                          ! time-slice data save: Ocean-atmosphere flux (2D)?
  LOGICAL::ctrl_data_save_slice_focnsed                          ! time-slice data save: Ocean-sediment flux (2D)?
  LOGICAL::ctrl_data_save_slice_fsedocn                          ! time-slice data save: Sediment-ocean flux (2D)?
  LOGICAL::ctrl_data_save_slice_bio                              ! time-slice data save: Biological fluxes (3D)?
  LOGICAL::ctrl_data_save_slice_carb                             ! time-slice data save: Aqueous carbonate system properties (3D)?
  LOGICAL::ctrl_data_save_slice_carbconst                        ! time-slice data save: Aqueous carbonate system constants (3D)?
  LOGICAL::ctrl_data_save_slice_phys_atm                         ! time-slice data save: Atmospheric physical properties (2D)?
  LOGICAL::ctrl_data_save_slice_phys_ocn                         ! time-slice data save: Ocean physical properties (3D)?
  LOGICAL::ctrl_data_save_slice_misc                             ! time-slice data save: Miscellaneous properties (-)?
  LOGICAL::ctrl_data_save_slice_diag                             ! time-slice data save: biogeochemical diagnostics?
  LOGICAL::ctrl_data_save_slice_diag_redox_old                   ! redox back-compatability
  LOGICAL::ctrl_data_save_slice_sur                              ! time-slice data save: surface properties
  NAMELIST /ini_biogem_nml/ &
       & ctrl_data_save_slice_ocnatm,ctrl_data_save_slice_ocn,ctrl_data_save_slice_ocnsed, &
       & ctrl_data_save_slice_fairsea, &
       & ctrl_data_save_slice_focnatm,ctrl_data_save_slice_focnsed,ctrl_data_save_slice_fsedocn, &
       & ctrl_data_save_slice_bio,ctrl_data_save_slice_carb,ctrl_data_save_slice_carbconst, &
       & ctrl_data_save_slice_phys_atm,ctrl_data_save_slice_phys_ocn,ctrl_data_save_slice_misc,ctrl_data_save_slice_diag, &
       & ctrl_data_save_slice_diag_redox_old, &
       & ctrl_data_save_slice_sur
  real::par_data_save_slice_dt                                   ! Integration interval (yr)
  NAMELIST /ini_biogem_nml/par_data_save_slice_dt
  CHARACTER(len=127)::par_infile_slice_name                      !
  NAMELIST /ini_biogem_nml/par_infile_slice_name
  integer::par_data_save_slice_n                                 ! number of timesteps in sub-inteval (e.g., monthly) saving
  NAMELIST /ini_biogem_nml/par_data_save_slice_n
  LOGICAL::ctrl_data_save_slice_autoend                          ! auto save at run end?
  NAMELIST /ini_biogem_nml/ctrl_data_save_slice_autoend
  LOGICAL::ctrl_data_save_slice_cdrmip                           ! save cdrmip data (only)?
  NAMELIST /ini_biogem_nml/ctrl_data_save_slice_cdrmip
  ! ------------------- DATA SAVING: TIME-SERIES --------------------------------------------------------------------------------- !
  LOGICAL::ctrl_data_save_sig_ocnatm                             ! time-series data save: Atmospheric (interface) composition?
  LOGICAL::ctrl_data_save_sig_ocn                                ! time-series data save: Oceanic composition?
  LOGICAL::ctrl_data_save_sig_fexport                            ! time-series data save: Export flux?
  LOGICAL::ctrl_data_save_sig_fairsea                            ! time-series data save: Air-sea gas exchange?
  LOGICAL::ctrl_data_save_sig_ocnsed                             ! time-series data save: Sediment (interface) composition?
  LOGICAL::ctrl_data_save_sig_focnatm                            ! time-series data save: Ocean->atmosphere flux?
  LOGICAL::ctrl_data_save_sig_focnsed                            ! time-series data save: Ocean->sediment flux?
  LOGICAL::ctrl_data_save_sig_fsedocn                            ! time-series data save: Sediment->ocean flux?
  LOGICAL::ctrl_data_save_sig_ocn_sur                            ! time-series data save: Ocean surface tracers?
  LOGICAL::ctrl_data_save_sig_carb_sur                           ! time-series data save: Ocean surface carbonate chemistry?
  LOGICAL::ctrl_data_save_sig_misc                               ! time-series data save: Miscellaneous properties?
  LOGICAL::ctrl_data_save_sig_diag                               ! time-series data save: biogeochemical diagnostics?
  LOGICAL::ctrl_data_save_sig_diag_redox_old                     ! redox back-compatability
  NAMELIST /ini_biogem_nml/ &
       & ctrl_data_save_sig_ocnatm,ctrl_data_save_sig_ocn,ctrl_data_save_sig_fexport,ctrl_data_save_sig_ocnsed, &
       & ctrl_data_save_sig_fairsea, &
       & ctrl_data_save_sig_focnatm,ctrl_data_save_sig_focnsed,ctrl_data_save_sig_fsedocn, &
       & ctrl_data_save_sig_ocn_sur,ctrl_data_save_sig_carb_sur,ctrl_data_save_sig_misc,ctrl_data_save_sig_diag, &
       & ctrl_data_save_sig_diag_redox_old
  real::par_data_save_sig_dt                                     ! Integration interval (yr)
  NAMELIST /ini_biogem_nml/par_data_save_sig_dt
  CHARACTER(len=127)::par_infile_sig_name                        !
  NAMELIST /ini_biogem_nml/par_infile_sig_name
  LOGICAL::ctrl_data_save_sig_autoend                            ! auto save at run end?
  NAMELIST /ini_biogem_nml/ctrl_data_save_sig_autoend
  LOGICAL::ctrl_data_save_3d_sig                                 ! save high resolution 3D data (@ time-series frequency)?
  NAMELIST /ini_biogem_nml/ctrl_data_save_3d_sig
  LOGICAL::ctrl_data_save_ocn_3D_ij                              ! save 3D data for a single ij location?
  NAMELIST /ini_biogem_nml/ctrl_data_save_ocn_3D_ij
  ! ------------------- DATA SAVING: MISC ---------------------------------------------------------------------------------------- !
  LOGICAL::ctrl_bio_preformed                                  ! Create pre-formed tracers? (requires #x numerical tracers)
  NAMELIST /ini_biogem_nml/ctrl_bio_preformed
  LOGICAL::ctrl_bio_remin_redox_save                             ! Create redox/remin data for saving?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_redox_save
  ! ------------------- DATA SAVING: MISC ---------------------------------------------------------------------------------------- !
  integer::par_data_save_level                                 ! Degree of comprehensivity of data saving
  NAMELIST /ini_biogem_nml/par_data_save_level
  LOGICAL::ctrl_data_save_derived                                ! save 'derived' data (e.g., salinity-normalized ocean tracers)?
  LOGICAL::ctrl_data_save_GLOBAL                                 ! save global diagnostics (at time-slice intervals)?
  NAMELIST /ini_biogem_nml/ctrl_data_save_derived,ctrl_data_save_GLOBAL
  LOGICAL::ctrl_data_save_slice_ascii                            ! Save time-slice data in ASCII format?
  LOGICAL::ctrl_data_save_sig_ascii                              ! Save time-series data in ASCII format?
  NAMELIST /ini_biogem_nml/ctrl_data_save_slice_ascii,ctrl_data_save_sig_ascii
  logical::opt_append_data                                       ! append data to output files on restart
  NAMELIST /ini_biogem_nml/opt_append_data
  real::par_data_save_ben_Dmin                                   ! minimum depth for 'benthic' average
  NAMELIST /ini_biogem_nml/par_data_save_ben_Dmin
  integer::par_t_sig_count_N,par_t_sig_count_S                   ! time-step point for snap-shot (time-series) solar forcing data
  NAMELIST /ini_biogem_nml/par_t_sig_count_N,par_t_sig_count_S
  integer::par_sig_j_N,par_sig_j_S                               ! generic N and S, j values (for time-series data saving)
  NAMELIST /ini_biogem_nml/par_sig_j_N,par_sig_j_S
  LOGICAL::ctrl_ncrst                                            ! restart as netCDF format?
  NAMELIST /ini_biogem_nml/ctrl_ncrst
  CHARACTER(len=127)::par_ncrst_name                             !
  NAMELIST /ini_biogem_nml/par_ncrst_name
  LOGICAL::ctrl_data_save_2d                                     ! save 2D netCDF data?
  LOGICAL::ctrl_data_save_3d                                     ! save 3D netCDF data?
  NAMELIST /ini_biogem_nml/ctrl_data_save_2d,ctrl_data_save_3d
  integer::par_misc_save_i
  integer::par_misc_save_j
  NAMELIST /ini_biogem_nml/par_misc_save_i,par_misc_save_j
  integer::n_orb_pts_nmax                                            !
  CHARACTER(len=127)::par_infile_orb_pts_loc_name                        !
  CHARACTER(len=127)::par_infile_orb_pts_var_name                        !
  NAMELIST /ini_biogem_nml/n_orb_pts_nmax,par_infile_orb_pts_loc_name,par_infile_orb_pts_var_name
  ! ------------------- TRACER AUDITING AND DEBUGGING OPTIONS -------------------------------------------------------------------- !
  LOGICAL::ctrl_audit                                            ! audit tracer inventory?
  LOGICAL::ctrl_audit_fatal                                      ! halt on audit fail?
  NAMELIST /ini_biogem_nml/ctrl_audit,ctrl_audit_fatal
  real::par_misc_audit_relerr                                    ! threshold of relative inventory change to trigger audit error
  NAMELIST /ini_biogem_nml/par_misc_audit_relerr
  LOGICAL::ctrl_debug_reportwarnings                             ! report all run-time warnings?
  NAMELIST /ini_biogem_nml/ctrl_debug_reportwarnings
  LOGICAL::ctrl_debug_lvl0                                       ! report 'level #0' debug?
  LOGICAL::ctrl_debug_lvl1                                       ! report 'level #1' debug?
  LOGICAL::ctrl_debug_lvl2                                       ! report 'level #2' debug?
  NAMELIST /ini_biogem_nml/ctrl_debug_lvl0,ctrl_debug_lvl1,ctrl_debug_lvl2
  ! ------------------- TRACER FORCING ------------------------------------------------------------------------------------------- !
  REAL,DIMENSION(n_atm)::par_atm_force_scale_time                ! scale tracer forcing time points
  REAL,DIMENSION(n_atm)::par_atm_force_scale_val                 ! scale tracer forcing value
  NAMELIST /ini_biogem_nml/par_atm_force_scale_time,par_atm_force_scale_val
  REAL,DIMENSION(n_ocn)::par_ocn_force_scale_time                ! scale tracer forcing time points
  REAL,DIMENSION(n_ocn)::par_ocn_force_scale_val                 ! scale tracer forcing value
  NAMELIST /ini_biogem_nml/par_ocn_force_scale_time,par_ocn_force_scale_val
  integer::par_force_point_i                                     ! 'i' coordinate of point forcing
  integer::par_force_point_j                                     ! 'j' coordinate of point forcing
  integer::par_force_point_k                                     ! 'k' coordinate of point forcing
  NAMELIST /ini_biogem_nml/par_force_point_i,par_force_point_j,par_force_point_k
  REAL::par_force_invert_ohmega                                ! surface ocean saturation state target
  NAMELIST /ini_biogem_nml/par_force_invert_ohmega
  logical::ctrl_force_invert_noneg                               ! prevent negative inversion fluxes (i.e. no removal)
  NAMELIST /ini_biogem_nml/ctrl_force_invert_noneg
  logical::ctrl_force_ohmega_calcite                             ! Calcite saturation as the saturation target?
  NAMELIST /ini_biogem_nml/ctrl_force_ohmega_calcite
  logical::ctrl_force_invert_Corgburial                        ! Allow carbon removal via Corg?
  NAMELIST /ini_biogem_nml/ctrl_force_invert_Corgburial
  real::par_force_invert_fCorgburial                           ! Scaling C burial flux relative to emissions
  NAMELIST /ini_biogem_nml/par_force_invert_fCorgburial
  logical::ctrl_force_invert_explicit                        ! Force explicit inversion?
  NAMELIST /ini_biogem_nml/ctrl_force_invert_explicit
  logical::ctrl_force_ocn_age                                   ! automatic ocean age tracer
  NAMELIST /ini_biogem_nml/ctrl_force_ocn_age
  ! ---------------- TRANSPORT MATRIX---------------------------!
  LOGICAL::ctrl_data_diagnose_TM !                              ! diagnose matrix in run?
  NAMELIST /ini_biogem_nml/ctrl_data_diagnose_TM
  REAL::par_data_TM_avg_n                                       ! number of intervals to diagnose average matrix in 1 year
  REAL::par_data_TM_start                                       ! year to start diagnosing matrix
  NAMELIST /ini_biogem_nml/par_data_TM_avg_n,par_data_TM_start
  ! ############################################################################################################################## !
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! *** *NON* NAMELIST DEFINITIONS *********************************************************************************************** !
  ! ****************************************************************************************************************************** !
  LOGICAL::ctrl_data_save_slice_diag_bio
  LOGICAL::ctrl_data_save_slice_diag_geochem
  LOGICAL::ctrl_data_save_slice_diag_proxy
  LOGICAL::ctrl_data_save_slice_diag_tracer
  LOGICAL::ctrl_data_save_sig_diag_bio
  LOGICAL::ctrl_data_save_sig_diag_geochem
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! MODEL CONFIGURATION CONSTANTS - ARRAY DIMENSIONS
  ! ****************************************************************************************************************************** !

  ! *** array dimensions ***
  ! grid dimensions
  INTEGER,PARAMETER::n_i = ilon1_ocn                           !
  INTEGER,PARAMETER::n_j = ilat1_ocn                           !
  INTEGER,PARAMETER::n_k = inl1_ocn                            !
  ! misc arrays dimensions
  INTEGER,PARAMETER::n_phys_ocn                           = 24 ! number of ocean box physical descriptors
  INTEGER,PARAMETER::n_phys_ocnatm                        = 25 ! number of ocean-atmosphere interface physical descriptors
  INTEGER,PARAMETER::n_data_max     = 32767                    ! (maximum) number of (time series) data points (2^15 - 1)
  ! options arrays dimensions
  INTEGER,PARAMETER::n_opt_misc                           = 14 ! miscellaneous
  INTEGER,PARAMETER::n_opt_atm                            = 01 ! atmosphere
  INTEGER,PARAMETER::n_opt_bio                            = 06 ! biogeochemical cycling
  INTEGER,PARAMETER::n_opt_force                          = 08 ! forcings
  INTEGER,PARAMETER::n_opt_data                           = 30 ! data (I/O)
  INTEGER,PARAMETER::n_opt_select                         = 05 ! (tracer) selections
  INTEGER,PARAMETER::n_diag_bio                           = 21 !
  INTEGER,PARAMETER::n_diag_geochem                       = 10 !
  INTEGER,PARAMETER::n_diag_Fe                            = 07 !
  INTEGER,PARAMETER::n_diag_misc_2D                       = 07 !
  INTEGER::n_diag_redox                                   =  0 !


  ! ****************************************************************************************************************************** !
  ! MODEL CONFIGURATION CONSTANTS - ARRAY INDICES NAMES
  ! ****************************************************************************************************************************** !

  ! *** array index values ***
  ! ocean 'physics' properties array indices
  INTEGER,PARAMETER::ipo_lat                              = 01   ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipo_lon                              = 02   ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipo_dlat                             = 03   ! latitude (degrees) [width]
  INTEGER,PARAMETER::ipo_dlon                             = 04   ! longitude (degrees) [width]
  INTEGER,PARAMETER::ipo_latn                             = 05   ! latitude (degrees) [north edge]
  INTEGER,PARAMETER::ipo_lone                             = 06   ! longitude (degrees) [east edge]
  INTEGER,PARAMETER::ipo_Dmid                             = 07   ! depth (m) [mid-point]
  INTEGER,PARAMETER::ipo_dD                               = 08   ! depth (m) [thickness]
  INTEGER,PARAMETER::ipo_Dbot                             = 09   ! depth (m) [bottom]
  INTEGER,PARAMETER::ipo_Dtop                             = 10   ! depth (m) [top]
  INTEGER,PARAMETER::ipo_A                                = 11   ! area (m2)
  INTEGER,PARAMETER::ipo_rA                               = 12   ! reciprocal area (to speed up numerics)
  INTEGER,PARAMETER::ipo_V                                = 13   ! ocean box volume (m3)
  INTEGER,PARAMETER::ipo_rV                               = 14   ! reciprocal volume (to speed up numerics)
  INTEGER,PARAMETER::ipo_M                                = 15   ! ocean box water mass (kg)
  INTEGER,PARAMETER::ipo_rM                               = 16   ! reciprocal mass (to speed up numerics)
  INTEGER,PARAMETER::ipo_mask_ocn                         = 17   ! wet grid point mask (wet = 1.0)
  INTEGER,PARAMETER::ipo_rho                              = 18   ! density
  INTEGER,PARAMETER::ipo_gu                               = 19   ! ocean velocity - u component
  INTEGER,PARAMETER::ipo_gv                               = 20   ! ocean velocity - v component
  INTEGER,PARAMETER::ipo_gw                               = 21   ! ocean velocity - w component
  INTEGER,PARAMETER::ipo_dzrho                            = 22   ! density gradient
  INTEGER,PARAMETER::ipo_diffv                            = 23   ! vertical diffusivity
  INTEGER,PARAMETER::ipo_rho_go                           = 24   ! goldstein density
  ! ocean-atmosphere interface 'physics' properties array indices
  INTEGER,PARAMETER::ipoa_lat                             = 01   ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipoa_lon                             = 02   ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipoa_dlat                            = 03   ! latitude (degrees) [width]
  INTEGER,PARAMETER::ipoa_dlon                            = 04   ! longitude (degrees) [width]
  INTEGER,PARAMETER::ipoa_A                               = 05   ! area (m2)
  INTEGER,PARAMETER::ipoa_rA                              = 06   ! reciprocal area (to speed up numerics)
  INTEGER,PARAMETER::ipoa_mask_ocn                        = 07   ! wet grid point mask (wet = 1.0)
  INTEGER,PARAMETER::ipoa_wspeed                          = 08   ! surface wind speed (m s-1)
  INTEGER,PARAMETER::ipoa_seaice                          = 09   ! fractional seaice cover
  INTEGER,PARAMETER::ipoa_seaice_th                       = 10   ! fractional seaice cover
  INTEGER,PARAMETER::ipoa_solfor                          = 11   ! solar forcing (W m-2)
  INTEGER,PARAMETER::ipoa_fxsw                            = 12   ! sw incident at surface (W m-2)
  INTEGER,PARAMETER::ipoa_tau_u                           = 13   ! wind stress - u component
  INTEGER,PARAMETER::ipoa_tau_v                           = 14   ! wind stress - v component
  INTEGER,PARAMETER::ipoa_u                               = 23   ! winds - u component
  INTEGER,PARAMETER::ipoa_v                               = 24   ! winds - v component
  INTEGER,PARAMETER::ipoa_usurf                           = 25   ! winds - speed
  INTEGER,PARAMETER::ipoa_cost                            = 15   ! cost
  INTEGER,PARAMETER::ipoa_KCO2                            = 16   ! CO2 air-sea gas exchange coefficient (mol m-2 yr-1 uatm-1)
  INTEGER,PARAMETER::ipoa_totFe                           = 17   ! total aeolian Fe input (mol yr-1)
  INTEGER,PARAMETER::ipoa_solFe                           = 18   ! aeolian Fe solubility (fraction)
  INTEGER,PARAMETER::ipoa_mld                             = 19   ! MDL (m below surface)
  INTEGER,PARAMETER::ipoa_evap                            = 20   ! evap
  INTEGER,PARAMETER::ipoa_pptn                            = 21   ! pptn
  INTEGER,PARAMETER::ipoa_seaice_dV                       = 22   ! change in sea-ice volumn
  ! options - misc
  integer,parameter::iopt_misc_O2_equil                   = 07   ! force O2 equilibrium of ocean with atmosphere
  integer,parameter::iopt_misc_debugij                    = 10   ! debug - explicit reporting of (i,j) location in main loop
  integer,parameter::iopt_misc_debugwarn                  = 11   ! debug - report all warningsst?
  ! options - force
  INTEGER,PARAMETER::iopt_force_GOLDSTEIn_CO2             = 01   ! use BioGeM CO2 to force to C-GOLDSTEIn energy balance calc?
  INTEGER,PARAMETER::iopt_force_freshwater                = 06   ! modify surface tracer concentrations by fresh water dilution?
  ! options - save
  INTEGER,PARAMETER::iopt_data_save_timeslice_fnint       = 23   ! construct time slice filename with integer year only?
  INTEGER,PARAMETER::iopt_data_save_config                = 24   ! save copies of biogem config files?
  ! tracer selection combination options
  INTEGER,PARAMETER::iopt_select_carbchem                 = 01   !
  INTEGER,PARAMETER::iopt_select_ocnatm_CO2               = 02   !
  INTEGER,PARAMETER::iopt_select_ocnatm_O2                = 03   !
  INTEGER,PARAMETER::iopt_select_ocnatm_N2                = 04   !
  INTEGER,PARAMETER::iopt_select_ocnatm_HC                = 05   !
  ! diagnostics - biology
  INTEGER,PARAMETER::idiag_bio_dPO4                      = 01    !
  INTEGER,PARAMETER::idiag_bio_dPO4_1                    = 02    !
  INTEGER,PARAMETER::idiag_bio_dPO4_2                    = 03    !
  INTEGER,PARAMETER::idiag_bio_N2fixation                = 04    !
  INTEGER,PARAMETER::idiag_bio_NH4assim                  = 05    !
  INTEGER,PARAMETER::idiag_bio_kT                        = 06    !
  INTEGER,PARAMETER::idiag_bio_kI                        = 07    !
  INTEGER,PARAMETER::idiag_bio_knut                      = 08    !
  INTEGER,PARAMETER::idiag_bio_DOMfrac                   = 09    !
  INTEGER,PARAMETER::idiag_bio_kPO4                      = 10    !
  INTEGER,PARAMETER::idiag_bio_kFe                       = 11    !
  INTEGER,PARAMETER::idiag_bio_kSiO2                     = 12    !
  INTEGER,PARAMETER::idiag_bio_kPO4_nsp                  = 13    !
  INTEGER,PARAMETER::idiag_bio_kFe_nsp                   = 14    !
  INTEGER,PARAMETER::idiag_bio_kSiO2_nsp                 = 15    !
  INTEGER,PARAMETER::idiag_bio_kPO4_sp                   = 16    !
  INTEGER,PARAMETER::idiag_bio_kFe_sp                    = 17    !
  INTEGER,PARAMETER::idiag_bio_kSiO2_sp                  = 18    !
  INTEGER,PARAMETER::idiag_bio_CaCO3toPOC_nsp            = 19    !
  INTEGER,PARAMETER::idiag_bio_opaltoPOC_sp              = 20    !
  INTEGER,PARAMETER::idiag_bio_fspPOC                    = 21    !
  ! diagnostics - geochemistry
  INTEGER,PARAMETER::idiag_geochem_ammox_dNO3            = 01    !
  INTEGER,PARAMETER::idiag_geochem_ammox_dNH4            = 02    !
  INTEGER,PARAMETER::idiag_geochem_Nredct_dN2            = 03    !
  INTEGER,PARAMETER::idiag_geochem_Nredct_dNH4           = 04    !
  INTEGER,PARAMETER::idiag_geochem_Sredct_dH2S           = 05    !
  INTEGER,PARAMETER::idiag_geochem_Sredct_dNH4           = 06    !
  INTEGER,PARAMETER::idiag_geochem_dH2S                  = 07    !
  INTEGER,PARAMETER::idiag_geochem_dCH4                  = 08    !
  INTEGER,PARAMETER::idiag_geochem_dCH4_AOM              = 09    !
  INTEGER,PARAMETER::idiag_geochem_dH2S_POMS             = 10    !
  ! diagnostics - geochemistry -- Fe
  INTEGER,PARAMETER::idiag_Fe_Fe                         = 01    !
  INTEGER,PARAMETER::idiag_Fe_FeL                        = 02    !
  INTEGER,PARAMETER::idiag_Fe_L                          = 03    !
  INTEGER,PARAMETER::idiag_Fe_TDFe                       = 04    !
  INTEGER,PARAMETER::idiag_Fe_TL                         = 05    !
  INTEGER,PARAMETER::idiag_Fe_Fe3                        = 06    !
  INTEGER,PARAMETER::idiag_Fe_geo                        = 07    !
  ! diagnostics - misc - 2D
  INTEGER,PARAMETER::idiag_misc_2D_FpCO2                 = 01    !
  INTEGER,PARAMETER::idiag_misc_2D_FpCO2_13C             = 02    !
  INTEGER,PARAMETER::idiag_misc_2D_FDIC                  = 03    !
  INTEGER,PARAMETER::idiag_misc_2D_FDIC_13C              = 04    !
  INTEGER,PARAMETER::idiag_misc_2D_FALK                  = 05    !
  INTEGER,PARAMETER::idiag_misc_2D_FCa                   = 06    !
  INTEGER,PARAMETER::idiag_misc_2D_FCa_44Ca              = 07    !

  ! *** array index names ***
  ! ocean 'physics'
  CHARACTER(len=16),DIMENSION(n_phys_ocn),PARAMETER::string_phys_ocn = (/ &
       & 'lat             ', &
       & 'lon             ', &
       & 'dlat            ', &
       & 'dlon            ', &
       & 'latn            ', &
       & 'lone            ', &
       & 'Dmid            ', &
       & 'dD              ', &
       & 'Dbot            ', &
       & 'Dtop            ', &
       & 'A               ', &
       & 'rA              ', &
       & 'V               ', &
       & 'rV              ', &
       & 'M               ', &
       & 'rM              ', &
       & 'mask_ocn        ', &
       & 'rho             ', &
       & 'u               ', &
       & 'v               ', &
       & 'w               ', &
       & 'dzrho           ', &
       & 'diffv           ', &
       & 'rho_go          ' /)
  ! ocean-atmosphere interface 'physics'
  CHARACTER(len=16),DIMENSION(n_phys_ocnatm),PARAMETER::string_phys_ocnatm = (/ &
       & 'lat             ', &
       & 'lon             ', &
       & 'dlat            ', &
       & 'dlon            ', &
       & 'A               ', &
       & 'rA              ', &
       & 'mask_ocn        ', &
       & 'wspeed          ', &
       & 'seaice          ', &
       & 'seaice_th       ', &
       & 'solfor          ', &
       & 'fxws            ', &
       & 'tau_u           ', &
       & 'tau_v           ', &
       & 'cost            ', &
       & 'KCO2            ', &
       & 'totFe           ', &
       & 'solFe           ', &
       & 'MLD             ', &
       & 'evap            ', &
       & 'precip          ', &
       & 'seaice_dV       ', &
       & 'u               ', &
       & 'v               ', &
       & 'usurf           ' /)
  ! diagnostics - biology
  CHARACTER(len=14),DIMENSION(n_diag_bio),PARAMETER::string_diag_bio = (/ &
       & 'dPO4          ', &
       & 'dPO4_1        ', &
       & 'dPO4_2        ', &
       & 'N2fixation    ', &
       & 'NH4assim      ', &
       & 'k_temp        ', &
       & 'k_light       ', &
       & 'k_nutrients   ', &
       & 'DOMfrac       ', &
       & 'k_PO4         ', &
       & 'k_Fe          ', &
       & 'k_SiO2        ', &
       & 'k_PO4_nsp     ', &
       & 'k_Fe_nsp      ', &
       & 'k_SiO2_nsp    ', &
       & 'k_PO4_sp      ', &
       & 'k_Fe_sp       ', &
       & 'k_SiO2_sp     ', &
       & 'CaCO3toPOC_nsp', &
       & 'opaltoPOC_sp  ', &
       & 'fspPOC        ' /)
  ! diagnostics - geochemistry
  CHARACTER(len=14),DIMENSION(n_diag_geochem),PARAMETER::string_diag_geochem = (/ &
       & 'NH4_oxid_dNO3 ', &
       & 'NH4_oxid_dNH4 ', &
       & 'NO3_redct_dN2 ', &
       & 'NO3_redct_dNH4', &
       & 'SO4_redct_dH2S', &
       & 'SO4_redct_dNH4', &
       & 'dH2S          ', &
       & 'dCH4          ', &
       & 'dCH4_AOM      ', &
       & 'H2StoPOMS_dH2S' /)
  ! diagnostics - geochemistry -- Fe
  CHARACTER(len=14),DIMENSION(n_diag_Fe),PARAMETER::string_diag_Fe = (/ &
       & 'Fe            ', &
       & 'FeL           ', &
       & 'L             ', &
       & 'TDFe          ', &
       & 'TL            ', &
       & 'Fe3           ', &
       & 'geo           ' /)
  ! diagnostics - misc - 2D
  CHARACTER(len=14),DIMENSION(n_diag_misc_2D),PARAMETER::string_diag_misc_2D = (/ &
       & 'FpCO2         ', &
       & 'FpCO2_13C     ', &
       & 'FDIC          ', &
       & 'FDIC_13C      ', &
       & 'FALK          ', &
       & 'FCa           ', &
       & 'FCa_44Ca      ' /)
  ! diagnostics - redox
  CHARACTER(len=31),DIMENSION(:),ALLOCATABLE::string_diag_redox        !

  ! *** miscellaneous ***
  ! changes in T or S required to trigger re-calculation of carbonate dissociation constants and Schmidt number
  REAL,parameter::par_carb_dT = 0.1                              ! UNITS: (K)
  REAL,parameter::par_carb_dS = 0.1                              ! UNITS: (o/oo)
  ! parameter determining the maximum flux between surface ocean and atmosphere,
  ! relative to the disequilibrium between ocean and atmosphere
  ! (i)   a value of 1.0 will allow a surface ocean cell to no more than equilibriate during a time-step
  ! (ii)  a value of 2.0 will allow the air-sea difference in partial pressure to be reversed
  ! (iii) a very large value will place no restrictions on air-sea gas exchange
  real,parameter::par_airsea_r_dflux_deqm_max = 1.00


  ! ****************************************************************************************************************************** !
  ! GLOBAL VARIABLE AND RUN-TIME SET PARAMETER ARRAYS
  ! ****************************************************************************************************************************** !

  ! *** Miscellanenous ***
  integer::par_misc_debug_i                                      ! 'i' index value for spatially-explicit debugging
  integer::par_misc_debug_j                                      ! 'j' index value for spatially-explicit debugging
  ! strings
  CHARACTER(len=6) ::string_runid                                !
  CHARACTER(len=31)::string_restartid                            !
  CHARACTER(len=7) ::string_ncrunid                              !
  CHARACTER(len=254) ::string_nctsi                              !
  CHARACTER(len=254) ::string_nctsint                            !
  CHARACTER(len=254) ::string_nctsglob                           !
  CHARACTER(len=254) ::string_ncout2d                            !
  CHARACTER(len=254) ::string_ncout3d                            !
  CHARACTER(len=254) ::string_ncout3dsig                         !
  CHARACTER(len=254) ::string_ncrst                              !
  integer::ncout2d_ntrec                                         ! count for netcdf datasets
  integer::ncout3d_ntrec                                         ! count for netcdf datasets
  integer::ncout3dsig_ntrec                                      ! count for netcdf datasets
  integer::ncout2d_iou                                           ! io for netcdf datasets
  integer::ncout3d_iou                                           ! io for netcdf datasets
  integer::ncout3dsig_iou                                        ! io for netcdf datasets
  integer::ncrst_ntrec                                         ! count for netcdf datasets
  integer::ncrst_iou                                           ! io for netcdf restart

  ! *** Miscellanenous run-time control options ***
  LOGICAL,DIMENSION(n_opt_misc)::opt_misc                        !

  ! *** time control ***
  REAL::par_misc_t_end                                           !
  real::par_misc_t_err                                           !
  LOGICAL::par_misc_t_go = .FALSE.                               !
  LOGICAL::par_misc_t_echo_header = .TRUE.                       !
  !
  real::par_misc_t_tseries = 0.0
  real::par_misc_t_tslice  = 0.0
  logical::par_misc_t_intseries = .FALSE.
  logical::par_misc_t_intslice  = .FALSE.
  logical::par_misc_t_endseries = .FALSE.
  logical::par_misc_t_endslice  = .FALSE.

  !*** transport matrix ***
  INTEGER::matrix_vocn_n = 0
  INTEGER::matrix_k = n_k
  INTEGER::matrix_go = 0
  INTEGER::matrix_season = 1
  INTEGER::matrix_avg_count = 0

  ! ocean tracer array
  !
  integer::n_vocn
  !
  type fieldocn
     integer::i
     integer::j
     integer::k1
     real,allocatable,DIMENSION(:,:)::mk
  end type fieldocn
  !
  type(fieldocn),DIMENSION(:),ALLOCATABLE::vocn                  !
  type(fieldocn),DIMENSION(:),ALLOCATABLE::vdocn                 ! tracer anomoly
  type(fieldocn),DIMENSION(:),ALLOCATABLE::vbio_remin            !
  type(fieldocn),DIMENSION(:),ALLOCATABLE::vphys_ocn             !
  type(fieldocn),DIMENSION(:),ALLOCATABLE::vbio_part             !
  type(fieldocn),DIMENSION(:),ALLOCATABLE::vdbio_part            !
  type(fieldocn),DIMENSION(:),ALLOCATABLE::matrix_exp            ! matrix dye output
  !
  ! atmosphere tracer array
  !
  integer::n_vocnatm
  !
  type fieldatm
     integer::i
     integer::j
     real,allocatable,DIMENSION(:)::m
  end type fieldatm
  !
  type(fieldatm),DIMENSION(:),ALLOCATABLE::vphys_ocnatm          !

!!$! *** TMP ***
!!$  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::docn                        ! ***********
!!$  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::dbio_part                   ! ***********
!!$! ***********

  ! *** GOLDSTEIN interface with BioGeM ***
  real,dimension(n_ocn)::tstoocn_offset                          ! tracer units offset (GOLDSTEIN <-> BIOGEM conversion)
  ! ocean
  ! NOTE: ocean tracers (dissolved and particulate) are stored as concentrations (mol kg-1)
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::ocn                         ! ocean tracer array
  ! atmosphere
  logical,DIMENSION(n_atm)::ocnatm_airsea_eqm                    !
  real,DIMENSION(n_atm,n_i,n_j)::ocnatm_airsea_pv                !
  real,DIMENSION(n_atm,n_i,n_j)::ocnatm_airsea_solconst          !
  ! 'biology'
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::bio_part                    ! ocean tracer particle field (NOTE: <n_sed> tracers)
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::bio_remin                   ! ocean tracer particle remin. field (NOTE: <n_ocn> tracers)
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::bio_settle                  ! ocean tracer particle settling field (NOTE: <n_sed> tracers)
  REAL,DIMENSION(n_sed,n_sed,n_i,n_j)::bio_part_red              ! 'Redfield' ratios
  ! 'physics'
  REAL,DIMENSION(n_phys_ocn,n_i,n_j,n_k)::phys_ocn               !
  REAL,DIMENSION(n_phys_ocnatm,n_i,n_j)::phys_ocnatm             !
  ! aqueous carbonate system
  REAL,DIMENSION(n_carb,n_i,n_j,n_k)::carb                       !
  REAL,DIMENSION(n_carbconst,n_i,n_j,n_k)::carbconst             !
  REAL,DIMENSION(n_carbalk,n_i,n_j,n_k)::carbalk                 !
  REAL,DIMENSION(n_carbisor,n_i,n_j,n_k)::carbisor               ! carbonate (carbon) isotopic properties array
  REAL,DIMENSION(3,n_i,n_j,n_k)::carb_TSn                        !
  ! diagnostics
  REAL,DIMENSION(n_diag_bio,n_i,n_j)::diag_bio                   ! biology diagnostics
  REAL,DIMENSION(n_diag_geochem,n_i,n_j,n_k)::diag_geochem       ! geochemistry diagnostics
  REAL,DIMENSION(n_diag_Fe,n_i,n_j,n_k)::diag_Fe                 ! geochemistry (Fe) diagnostics
!!!  REAL,DIMENSION(n_ocn,n_i,n_j)::diag_weather                    ! weathering diagnostics
  REAL,DIMENSION(n_atm,n_i,n_j)::diag_airsea                     ! air-sea gas exchange diagnostics
  REAL,DIMENSION(n_atm,n_i,n_j)::diag_forcing                    ! atmospheric forcing diagnostics
  REAL,DIMENSION(n_diag_misc_2D,n_i,n_j)::diag_misc_2D           !
  REAL,DIMENSION(0:n_i,0:n_j)::diag_misc_psi                     !
  real,DIMENSION(:,:,:,:),ALLOCATABLE::diag_redox                ! redox diagnostics
  REAL,DIMENSION(n_sed,n_i,n_j)::diag_ecogem_part                !
  REAL,DIMENSION(n_ocn,n_i,n_j)::diag_ecogem_remin               !

  ! *** integrated (time-averaged) time-series storage scalars and vectors ***
  !
  REAL::int_misc_gemlite_sig                                     !
  REAL::int_ocn_tot_M_sig                                        !
  REAL::int_ocn_tot_M_sur_sig                                    !
  REAL::int_ocn_tot_V_sig                                        !
  REAL,DIMENSION(n_ocn)::int_ocn_sig                             !
  REAL,DIMENSION(n_atm)::int_ocnatm_sig                          !
  REAL,DIMENSION(n_sed)::int_fexport_sig                         !
  REAL,DIMENSION(n_sed)::int_fracdom_sig                         !
  REAL,DIMENSION(n_atm)::int_focnatm_sig                         !
  REAL,DIMENSION(n_sed)::int_focnsed_sig                         !
  REAL,DIMENSION(n_ocn)::int_fsedocn_sig                         !
  REAL,DIMENSION(n_ocn)::int_ocn_sur_sig                         !
  REAL,DIMENSION(n_ocn)::int_ocn_opn_sig                         !
  REAL,DIMENSION(n_ocn)::int_ocn_ben_sig                         !
  REAL,DIMENSION(n_carb)::int_carb_sur_sig                       !
  REAL,DIMENSION(n_carb)::int_carb_opn_sig                       !
  REAL,DIMENSION(n_carb)::int_carb_ben_sig                       !
  REAL::int_misc_age_sig                                         !
  real::int_misc_age_sur_sig,int_misc_age_ben_sig                !
  REAL::int_misc_seaice_sig                                      !
  real::int_misc_seaice_sig_th,int_misc_seaice_sig_vol           !
  real::int_misc_opsi_min_sig,int_misc_opsi_max_sig              !
  real::int_misc_opsia_min_sig,int_misc_opsia_max_sig            !
  real::int_misc_SLT_sig                                         !
  real::int_misc_det_Fe_tot_sig,int_misc_det_Fe_dis_sig          !
  REAL,DIMENSION(n_sed)::int_ocnsed_sig                          !
  REAL,DIMENSION(n_diag_bio)::int_diag_bio_sig                   ! biology diagnostics
  REAL,DIMENSION(n_diag_geochem)::int_diag_geochem_sig           ! geochemistry diagnostics
  REAL,DIMENSION(n_ocn)::int_diag_weather_sig                    ! weathering diagnostics
  REAL,DIMENSION(n_atm)::int_diag_airsea_sig                     ! air-sea gas exchange diagnostics
  REAL,DIMENSION(n_atm)::int_diag_forcing_sig                    ! forcing diagnostics
  REAL,DIMENSION(n_diag_misc_2D)::int_diag_misc_2D_sig           !
  ! misc
  real::int_misc_ocn_solfor_sig,int_misc_opn_solfor_sig          !
  real::int_misc_ocn_fxsw_sig,int_misc_opn_fxsw_sig              !
  ! 'snap-shot' time-series arrays
  real::snap_misc_ocn_solfor_N_sig                               !
  real::snap_misc_ocn_solfor_S_sig                               !
  ! high resolution 3D!
  real,DIMENSION(:,:,:,:),ALLOCATABLE::int_misc_3D_sig           !
  ! redox
  real,DIMENSION(:),ALLOCATABLE::int_diag_redox_sig              ! redox diagnostics time-series
  ! ### ADD ADDITIONAL TIME-SERIES ARRAY DEFINITIONS HERE ######################################################################## !
  !
  ! ############################################################################################################################## !

  ! *** integrated (time-averaged) time-slice arrays ***
  ! integrated time slice storage arrays - ocean
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::int_ocn_timeslice           !
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::int_bio_part_timeslice      !
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::int_bio_settle_timeslice    !
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::int_bio_remin_timeslice     !
  REAL,DIMENSION(n_phys_ocn,n_i,n_j,n_k)::int_phys_ocn_timeslice !
  REAL,DIMENSION(n_phys_ocnatm,n_i,n_j)::int_phys_ocnatm_timeslice   !
  REAL,DIMENSION(n_carb,n_i,n_j,n_k)::int_carb_timeslice         !
  REAL,DIMENSION(n_carbconst,n_i,n_j,n_k)::int_carbconst_timeslice   !
  REAL,DIMENSION(n_carbisor,n_i,n_j,n_k)::int_carbisor_timeslice !
  !  integrated time slice storage arrays - ocean-atmosphere interface
  REAL,DIMENSION(n_atm,n_i,n_j)::int_sfcatm1_timeslice           !
  REAL,DIMENSION(n_atm,n_i,n_j)::int_focnatm_timeslice           !
  !  integrated time slice storage arrays - ocean-sediment interface
  REAL,DIMENSION(n_sed,n_i,n_j)::int_sfcsed1_timeslice           !
  REAL,DIMENSION(n_sed,n_i,n_j)::int_focnsed_timeslice           !
  REAL,DIMENSION(n_ocn,n_i,n_j)::int_fsedocn_timeslice           !
  !  integrated time slice storage arrays - GOLDSTEIn
  REAL,DIMENSION(0:n_j,0:n_k)::int_opsi_timeslice                !
  REAL,DIMENSION(0:n_j,0:n_k)::int_opsia_timeslice               !
  REAL,DIMENSION(0:n_j,0:n_k)::int_opsip_timeslice               !
  REAL,DIMENSION(0:n_j,0:n_k)::int_zpsi_timeslice                !
  REAL,DIMENSION(3,n_i,n_j,n_k)::int_u_timeslice                 !
  REAL,DIMENSION(0:n_i,0:n_j)::int_psi_timeslice                 !
  REAL,DIMENSION(n_diag_bio,n_i,n_j)::int_diag_bio_timeslice             ! biology diagnostics
  REAL,DIMENSION(n_diag_geochem,n_i,n_j,n_k)::int_diag_geochem_timeslice ! geochemistry diagnostics
  REAL,DIMENSION(n_diag_Fe,n_i,n_j,n_k)::int_diag_Fe_timeslice ! geochemistry (Fe) diagnostics
  REAL,DIMENSION(n_ocn,n_i,n_j)::int_diag_weather_timeslice      ! weathering diagnostics
  REAL,DIMENSION(n_atm,n_i,n_j)::int_diag_airsea_timeslice       ! air-sea gas exchange diagnostics
  ! redox
  real,DIMENSION(:,:,:,:),ALLOCATABLE::int_diag_redox_timeslice  ! redox diagnostics 3D time-slice
  ! ecogem
  REAL,DIMENSION(n_sed,n_i,n_j)::int_diag_ecogem_part                !
  REAL,DIMENSION(n_ocn,n_i,n_j)::int_diag_ecogem_remin               !
  ! ### ADD ADDITIONAL TIME-SLICE ARRAY DEFINITIONS HERE ######################################################################### !
  !
  ! ############################################################################################################################## !

  ! *** misc ***
  ! audit arrays
  REAL,DIMENSION(n_ocn)::audit_ocn_init                           !
  REAL,DIMENSION(n_ocn)::audit_ocn_old                            !
  REAL,DIMENSION(n_ocn)::audit_ocn_new                            !
  REAL,DIMENSION(n_ocn)::audit_ocn_delta                          !
  ! options arrays
  LOGICAL,DIMENSION(n_opt_atm)::opt_atm                           !
  LOGICAL,DIMENSION(n_opt_bio)::opt_bio                           !
  LOGICAL,DIMENSION(n_opt_force)::opt_force = .FALSE.             !
  LOGICAL,DIMENSION(n_opt_data)::opt_data                         !
  LOGICAL,DIMENSION(n_opt_select)::opt_select                     !
  ! integrated time series arrays
  REAL,DIMENSION(n_data_max)::par_data_save_sig                   !
  REAL,DIMENSION(n_data_max)::par_data_save_timeslice             !
  ! lookup table arrays
  real,DIMENSION(:,:,:,:),ALLOCATABLE::lookup_Fe_4D_Fe3      ! 4D Fe3 lookuptable
  real,DIMENSION(:,:,:,:),ALLOCATABLE::lookup_Fe_4D_geo      ! 4D geo lookuptable
  real,DIMENSION(:),ALLOCATABLE::lookup_Fe_1D_1       !
  real,DIMENSION(:),ALLOCATABLE::lookup_Fe_1D_2       !
  real,DIMENSION(:),ALLOCATABLE::lookup_Fe_1D_3       !
  real,DIMENSION(:),ALLOCATABLE::lookup_Fe_1D_4       !
  integer::par_lookup_Fe_n_1
  integer::par_lookup_Fe_n_2
  integer::par_lookup_Fe_n_3
  integer::par_lookup_Fe_n_4
  ! orbital-scale data saving
  integer::n_orb_pts = 0                                         ! current orbital data point number
  integer::n_orb_pts_nloc                                        ! number of orbital data point locations
  integer::n_orb_pts_nvar                                        ! number of orbital data point variables
  integer,DIMENSION(:,:),ALLOCATABLE::orb_pts_loc                ! orbital point location data
  CHARACTER(len=31),DIMENSION(:),ALLOCATABLE::orb_pts_var              ! orbital point variable data
  real,DIMENSION(:,:,:),ALLOCATABLE::orb_pts                     ! saved orbital point data
  real,DIMENSION(:),ALLOCATABLE::orb_pts_time                ! orbital point time
  ! global means
  REAL,DIMENSION(n_sed)::int_fracdom                         !

  ! *** forcing ***
  ! forcing - restoring
  real,DIMENSION(:,:,:,:),ALLOCATABLE::force_restore_locn          !
  real,DIMENSION(:,:,:,:),ALLOCATABLE::force_restore_locn_I        !
  real,DIMENSION(:,:,:,:),ALLOCATABLE::force_restore_locn_II       !
  REAL,DIMENSION(n_ocn,2,n_data_max)::force_restore_ocn_sig       !
  REAL,DIMENSION(n_ocn)::force_restore_ocn_sig_x                  !
  REAL,DIMENSION(n_ocn)::force_restore_ocn_tconst                 !
  INTEGER,DIMENSION(n_ocn,2)::force_restore_ocn_sig_i             !
  LOGICAL,DIMENSION(n_ocn)::force_restore_ocn_select              !
  LOGICAL,DIMENSION(n_ocn)::force_restore_ocn_sur                 !
  INTEGER,DIMENSION(n_ocn,n_i,n_j)::force_restore_ocn_k1          !
  REAL,DIMENSION(n_atm,n_i,n_j)::force_restore_atm                !
  REAL,DIMENSION(n_atm,n_i,n_j)::force_restore_atm_I              !
  REAL,DIMENSION(n_atm,n_i,n_j)::force_restore_atm_II             !
  REAL,DIMENSION(n_atm,2,n_data_max)::force_restore_atm_sig       !
  REAL,DIMENSION(n_atm)::force_restore_atm_sig_x                  !
  REAL,DIMENSION(n_atm)::force_restore_atm_tconst                 !
  INTEGER,DIMENSION(n_atm,2)::force_restore_atm_sig_i             !
  LOGICAL,DIMENSION(n_atm)::force_restore_atm_select              !
!!$  REAL,DIMENSION(n_sed,n_i,n_j)::force_restore_sed
!!$  REAL,DIMENSION(n_sed,n_i,n_j)::force_restore_sed_I
!!$  REAL,DIMENSION(n_sed,n_i,n_j)::force_restore_sed_II
!!$  REAL,DIMENSION(n_sed,2,n_data_max)::force_restore_sed_sig
!!$  REAL,DIMENSION(n_sed)::force_restore_sed_sig_x
!!$  REAL,DIMENSION(n_sed)::force_restore_sed_tconst
!!$  INTEGER,DIMENSION(n_sed,2)::force_restore_sed_sig_i
!!$  LOGICAL,DIMENSION(n_sed)::force_restore_sed_select
  ! forcing - flux
  real,DIMENSION(:,:,:,:),ALLOCATABLE::force_flux_locn          !
  real,DIMENSION(:,:,:,:),ALLOCATABLE::force_flux_locn_I        !
  real,DIMENSION(:,:,:,:),ALLOCATABLE::force_flux_locn_II       !
  REAL,DIMENSION(n_ocn,2,n_data_max)::force_flux_ocn_sig         !
  REAL,DIMENSION(n_ocn)::force_flux_ocn_sig_x                    !
  INTEGER,DIMENSION(n_ocn,2)::force_flux_ocn_sig_i               !
  LOGICAL,DIMENSION(n_ocn)::force_flux_ocn_select                !
  LOGICAL,DIMENSION(n_ocn)::force_flux_ocn_scale                 !
  INTEGER,DIMENSION(n_ocn,n_i,n_j)::force_flux_ocn_k1            !
  REAL,DIMENSION(n_atm,n_i,n_j)::force_flux_atm                  !
  REAL,DIMENSION(n_atm,n_i,n_j)::force_flux_atm_I                !
  REAL,DIMENSION(n_atm,n_i,n_j)::force_flux_atm_II               !
  REAL,DIMENSION(n_atm,2,n_data_max)::force_flux_atm_sig         !
  REAL,DIMENSION(n_atm)::force_flux_atm_sig_x                    !
  INTEGER,DIMENSION(n_atm,2)::force_flux_atm_sig_i               !
  LOGICAL,DIMENSION(n_atm)::force_flux_atm_select                !
  LOGICAL,DIMENSION(n_atm)::force_flux_atm_scale                 !
  REAL,DIMENSION(n_sed,n_i,n_j)::force_flux_sed                  !
  REAL,DIMENSION(n_sed,n_i,n_j)::force_flux_sed_I                !
  REAL,DIMENSION(n_sed,n_i,n_j)::force_flux_sed_II               !
  REAL,DIMENSION(n_sed,2,n_data_max)::force_flux_sed_sig         !
  REAL,DIMENSION(n_sed)::force_flux_sed_sig_x                    !
  INTEGER,DIMENSION(n_sed,2)::force_flux_sed_sig_i               !
  LOGICAL,DIMENSION(n_sed)::force_flux_sed_select                !
  LOGICAL,DIMENSION(n_sed)::force_flux_sed_scale                 !
  ! forcing - misc
  REAL,DIMENSION(2,n_data_max)::force_solconst_sig               !
  real,DIMENSION(n_ocn)::force_restore_docn_nuts                 !
  integer,DIMENSION(n_atm)::force_atm_uniform                    !
  integer,DIMENSION(n_ocn)::force_ocn_uniform                    !
  integer,DIMENSION(n_sed)::force_sed_uniform                    !
  integer,DIMENSION(n_atm)::force_atm_point_i                    !
  integer,DIMENSION(n_ocn)::force_ocn_point_i                    !
  integer,DIMENSION(n_sed)::force_sed_point_i                    !
  integer,DIMENSION(n_atm)::force_atm_point_j                    !
  integer,DIMENSION(n_ocn)::force_ocn_point_j                    !
  integer,DIMENSION(n_sed)::force_sed_point_j                    !
  integer,DIMENSION(n_ocn)::force_ocn_point_k                    !
  ! ### ADD ADDITIONAL FORCINGS ARRAY DEFINITIONS HERE ########################################################################### !
  !
  ! ############################################################################################################################## !
  ! misc
  REAL,DIMENSION(n_i,n_j)::par_phys_seaice                       !
  REAL,DIMENSION(n_i,n_j)::par_phys_windspeed                    !
  REAL,DIMENSION(n_i,n_j)::par_bio_CaCO3toPOCrainratio           !
  REAL,DIMENSION(n_i,n_j)::par_bio_Cd_alpha                      !
  REAL,DIMENSION(n_i,n_j)::par_bio_POCdtoPOCrainratio            !
  REAL,DIMENSION(n_i,n_j)::par_bio_remin_kc                      !
  REAL,DIMENSION(n_i,n_j)::par_bio_remin_ko                      !
  REAL,DIMENSION(n_i,n_j)::par_bio_remin_kl                      !
  REAL,DIMENSION(n_i,n_j,n_k)::par_scav_fpart_POC                !
  REAL,DIMENSION(n_i,n_j,n_k)::par_scav_fpart_CaCO3              !
  REAL,DIMENSION(n_i,n_j,n_k)::par_scav_fpart_opal               !
  REAL,DIMENSION(n_i,n_j,n_k)::par_scav_fpart_det                !
  REAL,DIMENSION(n_i,n_j)::par_bio_remin_b                       !
  REAL,DIMENSION(n_i,n_j)::par_misc_2D                           !
  REAL,DIMENSION(n_i,n_j)::force_Fgeothermal2D                   !

  ! ****************************************************************************************************************************** !
  ! *** GLOBAL VARIABLES AND RUN-TIME SET PARAMETERS ***************************************************************************** !
  ! ****************************************************************************************************************************** !

  ! *** copies of GOLDSTEIn variables ***
  ! dimensional scale values for the ocean
  REAL::goldstein_usc                                            !
  REAL::goldstein_dsc                                            !
  REAL::goldstein_fsc                                            !
  REAL::goldstein_rh0sc                                          !
  REAL::goldstein_rhosc                                          !
  REAL::goldstein_cpsc                                           !
  ! miscellaneous constants
  REAL::goldstein_saln0                                          ! EMBM reference salinity
  REAL::goldstein_rhoair                                         ! air density
  REAL::goldstein_cd                                             ! drag coefficient for wind stress calc
  REAL::goldstein_ds                                             ! grid spacing; sin(lat)
  REAL::goldstein_dphi                                           ! grid spacing; long
  real::goldstein_scf                                            !
  real::phys_solar_constant = 0.0                                !
  ! depth and location of oceans
  INTEGER,DIMENSION(n_i,n_j)::goldstein_k1                       !
  INTEGER::goldstein_jsf                                         !
  INTEGER,DIMENSION(n_j)::goldstein_ips                          !
  INTEGER,DIMENSION(n_j)::goldstein_ipf                          !
  INTEGER,DIMENSION(n_j)::goldstein_ias                          !
  INTEGER,DIMENSION(n_j)::goldstein_iaf                          !
  ! miscellaneous
  REAL,DIMENSION(n_k)::goldstein_dz                              !
  REAL,DIMENSION(n_k)::goldstein_dza                             !
  REAL,DIMENSION(0:n_j)::goldstein_c                             !
  REAL,DIMENSION(0:n_j)::goldstein_cv                            !
  REAL,DIMENSION(0:n_j)::goldstein_s                             !
  REAL,DIMENSION(0:n_j)::goldstein_sv                            !

  ! *** I/O ***
  ! string formation associated variables
  INTEGER::n_char_years                                          !
  INTEGER::n_char_years_fractional                               !
  ! integrated values storage arrays
  REAL::int_t_sig                                                ! integrated time for run-time (signal) save (years)
  REAL::int_t_timeslice                                          ! integrated time for time-slice save (years)
  REAL::int_t_timeslice_TOT = 0.0                                ! integrated time for time-slice save (TOTAL) (years)
  integer::int_t_sig_count                                       !
  integer::int_t_timeslice_count                                 !
  ! time series arrays - data save
  INTEGER::par_data_save_sig_i                                   !
  INTEGER::par_data_save_timeslice_i                             !
  !
  logical::ctrl_data_save_inversion

  ! *** MISC ***
  real::par_bio_c0_I                                             !
  real::par_bio_c0_Cd                                            !
  real::par_det_Fe_frac                                          ! mass abundance of Fe in dust
  real::par_K_FeL                                                !
  real::par_scav_Fe_exp                                          ! see: Parekh et al. [2005]
  real::par_scav_Fe_k0                                           ! Parekh et al. [2006] initial scavenging rate
  real::par_part_red_FeTmin                                      !
  real::par_part_red_FetoCmax                                    !
  real::par_bio_red_O2_H2SO4                                     ! pseudo 'Redfield ratio' to convert O2 deficit to sulphate O2
  real::par_bio_red_O2_NO3                                       ! pseudo 'Redfield ratio' to convert O2 deficit to nitrate O2


CONTAINS


  ! ****************************************************************************************************************************** !
  ! TRACER ARRAY CONVERSION
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_init_vocn()
    ! result variable
    type(fieldocn),DIMENSION(:),ALLOCATABLE::fun_lib_init_vocn   !
    ! local variables
    integer::i,j,n
    integer::loc_n,loc_k1
    ! allocate result variable size
    ALLOCATE(fun_lib_init_vocn(1:n_vocn))
    do n=1,n_vocn
       allocate(fun_lib_init_vocn(n)%mk(1:n_l_ocn,1:n_k))
    end do
    ! set result variable
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_k1 = goldstein_k1(i,j)
          IF (n_k >= loc_k1) THEN
             loc_n = loc_n + 1
             fun_lib_init_vocn(loc_n)%i  = i
             fun_lib_init_vocn(loc_n)%j  = j
             fun_lib_init_vocn(loc_n)%k1 = loc_k1
             fun_lib_init_vocn(loc_n)%mk(:,:) = 0.0
          end if
       end do
    end do
  END function fun_lib_init_vocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_init_vsed()
    ! result variable
    type(fieldocn),DIMENSION(:),ALLOCATABLE::fun_lib_init_vsed   !
    ! local variables
    integer::i,j,n
    integer::loc_n,loc_k1
    ! allocate result variable size
    ALLOCATE(fun_lib_init_vsed(1:n_vocn))
    do n=1,n_vocn
       allocate(fun_lib_init_vsed(n)%mk(1:n_l_sed,1:n_k))
    end do
    ! set result variable
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_k1 = goldstein_k1(i,j)
          IF (n_k >= loc_k1) THEN
             loc_n = loc_n + 1
             fun_lib_init_vsed(loc_n)%i  = i
             fun_lib_init_vsed(loc_n)%j  = j
             fun_lib_init_vsed(loc_n)%k1 = loc_k1
             fun_lib_init_vsed(loc_n)%mk(:,:) = 0.0
          end if
       end do
    end do
  END function fun_lib_init_vsed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_init_vocn_n(dum_n)
    ! dummy valiables
    integer,INTENT(in)::dum_n                                   !
    ! result variable
    type(fieldocn),DIMENSION(:),ALLOCATABLE::fun_lib_init_vocn_n   !
    ! local variables
    integer::i,j,n
    integer::loc_n,loc_k1
    ! allocate result variable size
    ALLOCATE(fun_lib_init_vocn_n(1:n_vocn))
    do n=1,n_vocn
       allocate(fun_lib_init_vocn_n(n)%mk(1:dum_n,1:n_k))
    end do
    ! set result variable
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_k1 = goldstein_k1(i,j)
          IF (n_k >= loc_k1) THEN
             loc_n = loc_n + 1
             fun_lib_init_vocn_n(loc_n)%i  = i
             fun_lib_init_vocn_n(loc_n)%j  = j
             fun_lib_init_vocn_n(loc_n)%k1 = loc_k1
             fun_lib_init_vocn_n(loc_n)%mk(:,:) = 0.0
          end if
       end do
    end do
  END function fun_lib_init_vocn_n
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_conv_ocnTOvocn(dum_ocn)
    ! dummy valiables
    REAL,DIMENSION(n_ocn,n_i,n_j,n_k),INTENT(in)::dum_ocn        !
    ! result variable
    type(fieldocn),DIMENSION(:),ALLOCATABLE::fun_lib_conv_ocnTOvocn !
    ! local variables
    integer::i,j,k,n
    integer::l,io
    integer::loc_n,loc_k1
    ! allocate result variable size
    ALLOCATE(fun_lib_conv_ocnTOvocn(1:n_vocn))
    do n=1,n_vocn
       allocate(fun_lib_conv_ocnTOvocn(n)%mk(1:n_l_ocn,1:n_k))
    end do
    !
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_k1 = goldstein_k1(i,j)
          IF (n_k >= loc_k1) THEN
             loc_n = loc_n + 1
             fun_lib_conv_ocnTOvocn(loc_n)%i = i
             fun_lib_conv_ocnTOvocn(loc_n)%j = j
             fun_lib_conv_ocnTOvocn(loc_n)%k1 = loc_k1
             ! initialize, because not all 'k' depths are valid
             fun_lib_conv_ocnTOvocn(loc_n)%mk(:,:) = 0.0
             DO k=n_k,loc_k1,-1
                DO l=1,n_l_ocn
                   io = conv_iselected_io(l)
                   fun_lib_conv_ocnTOvocn(loc_n)%mk(l,k) = dum_ocn(io,i,j,k)
                end DO
             end DO
          end if
       end do
    end do
  END function fun_lib_conv_ocnTOvocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_conv_sedTOvsed(dum_sed)
    ! dummy valiables
    REAL,DIMENSION(n_sed,n_i,n_j,n_k),INTENT(in)::dum_sed           !
    ! result variable
    type(fieldocn),DIMENSION(:),ALLOCATABLE::fun_lib_conv_sedTOvsed !
    ! local variables
    integer::i,j,k,n
    integer::l,is
    integer::loc_n,loc_k1
    ! allocate result variable size
    ALLOCATE(fun_lib_conv_sedTOvsed(1:n_vocn))
    do n=1,n_vocn
       allocate(fun_lib_conv_sedTOvsed(n)%mk(1:n_l_sed,1:n_k))
    end do
    !
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_k1 = goldstein_k1(i,j)
          IF (n_k >= loc_k1) THEN
             loc_n = loc_n + 1
             fun_lib_conv_sedTOvsed(loc_n)%i = i
             fun_lib_conv_sedTOvsed(loc_n)%j = j
             fun_lib_conv_sedTOvsed(loc_n)%k1 = loc_k1
             ! initialize, because not all 'k' depths are valid
             fun_lib_conv_sedTOvsed(loc_n)%mk(:,:) = 0.0
             DO k=n_k,loc_k1,-1
                DO l=1,n_l_sed
                   is = conv_iselected_is(l)
                   fun_lib_conv_sedTOvsed(loc_n)%mk(l,k) = dum_sed(is,i,j,k)
                end DO
             end DO
          end if
       end do
    end do
  END function fun_lib_conv_sedTOvsed
  ! ****************************************************************************************************************************** !


!!$  ! ****************************************************************************************************************************** !
!!$  !
!!$  function fun_lib_conv_vnocnTOijocn(dum_vnocn)
!!$    ! dummy valiables
!!$    type(fieldocn)::dum_vnocn                                    !
!!$    ! result variable
!!$    REAL,DIMENSION(n_ocn,n_k)::fun_lib_conv_vnocnTOijocn         !
!!$    ! local variables
!!$    integer::k
!!$    integer::l,io
!!$    integer::loc_k1
!!$    ! initialize, because not all grid points or 'k' depths are valid
!!$    fun_lib_conv_vnocnTOijocn(:,:) = 0.0
!!$    !
!!$    loc_k1 = dum_vnocn%k1
!!$    DO k=n_k,loc_k1,-1
!!$       DO l=1,n_l_ocn
!!$          io = conv_iselected_io(l)
!!$          fun_lib_conv_vnocnTOijocn(io,k) = dum_vnocn%mk(l,k)
!!$       end DO
!!$    end DO
!!$  END function fun_lib_conv_vnocnTOijocn
!!$  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_conv_vocnTOocn(dum_vocn)
    ! dummy valiables
    type(fieldocn),DIMENSION(:)::dum_vocn                        !
    ! result variable
    REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::fun_lib_conv_vocnTOocn    !
    ! local variables
    integer::loc_i,loc_j,k,n
    integer::l,io
    integer::loc_k1
    ! initialize results variable, becasue not all grid points or 'k' depths are valid
    fun_lib_conv_vocnTOocn(:,:,:,:) = 0.0
    !
    do n=1,n_vocn
       loc_i = dum_vocn(n)%i
       loc_j = dum_vocn(n)%j
       loc_k1 = dum_vocn(n)%k1
       DO k=n_k,loc_k1,-1
          DO l=1,n_l_ocn
             io = conv_iselected_io(l)
             fun_lib_conv_vocnTOocn(io,loc_i,loc_j,k) = dum_vocn(n)%mk(l,k)
          end DO
       end DO
    end do
  END function fun_lib_conv_vocnTOocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_conv_vsedTOsed(dum_vsed)
    ! dummy valiables
    type(fieldocn),DIMENSION(:)::dum_vsed                        !
    ! result variable
    REAL,DIMENSION(n_sed,n_i,n_j,n_k)::fun_lib_conv_vsedTOsed    !
    ! local variables
    integer::loc_i,loc_j,k,n
    integer::l,is
    integer::loc_k1
    ! initialize results variable, becasue not all grid points or 'k' depths are valid
    fun_lib_conv_vsedTOsed(:,:,:,:) = 0.0
    !
    do n=1,n_vocn
       loc_i = dum_vsed(n)%i
       loc_j = dum_vsed(n)%j
       loc_k1 = dum_vsed(n)%k1
       DO k=n_k,loc_k1,-1
          DO l=1,n_l_sed
             is = conv_iselected_is(l)
             fun_lib_conv_vsedTOsed(is,loc_i,loc_j,k) = dum_vsed(n)%mk(l,k)
          end DO
       end DO
    end do
  END function fun_lib_conv_vsedTOsed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_conv_tsTOvocn(dum_ts)
    ! dummy valiables
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(in)::dum_ts    !
    ! result variable
    type(fieldocn),DIMENSION(:),ALLOCATABLE::fun_lib_conv_tsTOvocn !
    ! local variables
    integer::i,j,k,n
    integer::loc_n,loc_k1
    ! allocate result variable size
    ALLOCATE(fun_lib_conv_tsTOvocn(1:n_vocn))
    do n=1,n_vocn
       allocate(fun_lib_conv_tsTOvocn(n)%mk(1:n_l_ocn,1:n_k))
    end do
    !
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_k1 = goldstein_k1(i,j)
          IF (n_k >= loc_k1) THEN
             loc_n = loc_n + 1
             fun_lib_conv_tsTOvocn(loc_n)%i = i
             fun_lib_conv_tsTOvocn(loc_n)%j = j
             fun_lib_conv_tsTOvocn(loc_n)%k1 = loc_k1
             ! initialize, becasue not all 'k' depths are valid
             fun_lib_conv_tsTOvocn(loc_n)%mk(:,:) = 0.0
             DO k=n_k,loc_k1,-1
                fun_lib_conv_tsTOvocn(loc_n)%mk(:,k) = dum_ts(:,i,j,k)
             end DO
          end if
       end do
    end do
  END function fun_lib_conv_tsTOvocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_lib_conv_vocnTOts(dum_vocn)
    ! dummy valiables
    type(fieldocn),DIMENSION(:),INTENT(in)::dum_vocn             !
    ! result variable
    REAL,DIMENSION(n_l_ocn,n_i,n_j,n_k)::fun_lib_conv_vocnTOts   !
    ! local variables
    integer::loc_i,loc_j,k,n
    integer::l
    integer::loc_k1
    ! initialize, becasue not all grid points or 'k' depths are valid
    fun_lib_conv_vocnTOts(:,:,:,:) = 0.0
    !
    do n=1,n_vocn
       loc_i = dum_vocn(n)%i
       loc_j = dum_vocn(n)%j
       loc_k1 = dum_vocn(n)%k1
       DO k=n_k,loc_k1,-1
          DO l=1,n_l_ocn
             fun_lib_conv_vocnTOts(l,loc_i,loc_j,k) = dum_vocn(n)%mk(l,k)
          end DO
       end DO
    end do
  END function fun_lib_conv_vocnTOts
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! I/O ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD TIME-SERIES DATA (2 VARIABLES)
  SUBROUTINE sub_load_data_t2(dum_filename,dum_data_scale,dum_data,dum_n_elements)
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename
    REAL,INTENT(in),DIMENSION(2)::dum_data_scale
    REAL,INTENT(inout),DIMENSION(2,n_data_max)::dum_data
    INTEGER,INTENT(inout)::dum_n_elements
    ! local variables
    INTEGER::n
    INTEGER::loc_n_elements,loc_n_start
    REAL,DIMENSION(2,n_data_max)::loc_data
    ! initialize local variables
    loc_data(:,:) = 0.0
    ! check file format
    CALL sub_check_fileformat(TRIM(dum_filename),loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=TRIM(dum_filename),action='read')
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! read in forcing function data
    DO n = 1,loc_n_elements
       READ(unit=in,fmt=*) loc_data(1,n),loc_data(2,n)
    END DO
    CLOSE(in)
    ! re-scale data
    loc_data(1,:) = dum_data_scale(1)*loc_data(1,:)
    loc_data(2,:) = dum_data_scale(2)*loc_data(2,:)
    !
    IF (loc_n_elements > n_data_max) THEN
       CALL sub_report_error( &
            & 'biogem_lib','load_data_t2','loc_n_elements > n_data_max', &
            & 'STOPPING', &
            & (/REAL(loc_n_elements),REAL(n_data_max)/),.TRUE. &
            & )
    ELSE if (loc_n_elements > 0) THEN
       IF (ctrl_misc_t_BP .AND. (loc_data(1,loc_n_elements) >= loc_data(1,1))) THEN
          dum_data(1,:) = loc_data(1,:) - par_misc_t_end
          dum_data(2,:) = loc_data(2,:)
       END IF
       IF (ctrl_misc_t_BP .AND. (loc_data(1,loc_n_elements) < loc_data(1,1))) THEN
          DO n = 1,loc_n_elements
             dum_data(1,n) = loc_data(1,loc_n_elements - n + 1) - par_misc_t_end
             dum_data(2,n) = loc_data(2,loc_n_elements - n + 1)
          END DO
       END IF
       IF (.NOT.(ctrl_misc_t_BP) .AND. (loc_data(1,loc_n_elements) <= loc_data(1,1))) THEN
          dum_data(1,:) = par_misc_t_end - loc_data(1,:)
          dum_data(2,:) = loc_data(2,:)
       END IF
       IF (.NOT.(ctrl_misc_t_BP) .AND. (loc_data(1,loc_n_elements) > loc_data(1,1))) THEN
          DO n = 1,loc_n_elements
             dum_data(1,n) = par_misc_t_end - loc_data(1,loc_n_elements - n + 1)
             dum_data(2,n) = loc_data(2,loc_n_elements - n + 1)
          END DO
       END IF
       dum_n_elements = loc_n_elements
    else
       dum_n_elements = 0
    END IF
  END SUBROUTINE sub_load_data_t2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD TIME-SERIES DATA (1 VARIABLE)
  SUBROUTINE sub_load_data_t1(dum_filename,dum_data_scale,dum_data,dum_n_elements)
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename
    REAL,INTENT(in)::dum_data_scale
    REAL,INTENT(inout),DIMENSION(n_data_max)::dum_data
    INTEGER,INTENT(inout)::dum_n_elements
    ! local variables
    INTEGER::n
    INTEGER::loc_n_elements,loc_n_start
    REAL,DIMENSION(n_data_max)::loc_data
    ! initialize local variables
    loc_data(:) = 0.0
    ! check file format
    CALL sub_check_fileformat(TRIM(dum_filename),loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=TRIM(dum_filename),action='read')
    ! goto start-of-file tag
    loc_data = 0.0
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! read in forcing function data
    DO n = 1,loc_n_elements
       READ(unit=in,fmt=*) loc_data(n)
    END DO
    CLOSE(in)
    ! re-scale data
    loc_data(:) = dum_data_scale*loc_data(:)
    !
    IF (loc_n_elements > n_data_max) THEN
       CALL sub_report_error( &
            & 'biogem_lib','load_data_t1','loc_n_elements > n_data_max', &
            & 'STOPPING', &
            & (/REAL(loc_n_elements),REAL(n_data_max)/),.TRUE. &
            & )
    ELSE if (loc_n_elements > 0) THEN
       IF (ctrl_misc_t_BP .AND. (loc_data(loc_n_elements) >= loc_data(1))) THEN
          dum_data(1:loc_n_elements) = loc_data(1:loc_n_elements) - par_misc_t_end
       END IF
       IF (ctrl_misc_t_BP .AND. (loc_data(loc_n_elements) < loc_data(1))) THEN
          DO n = 1,loc_n_elements
             dum_data(n) = loc_data(loc_n_elements - n + 1) - par_misc_t_end
          END DO
       END IF
       IF (.NOT.(ctrl_misc_t_BP) .AND. (loc_data(loc_n_elements) <= loc_data(1))) THEN
          dum_data(1:loc_n_elements) = par_misc_t_end - loc_data(1:loc_n_elements)
       END IF
       IF (.NOT.(ctrl_misc_t_BP) .AND. (loc_data(loc_n_elements) > loc_data(1))) THEN
          DO n = 1,loc_n_elements
             dum_data(n) = par_misc_t_end - loc_data(loc_n_elements - n + 1)
          END DO
       END IF
       dum_n_elements = loc_n_elements
    else
       dum_n_elements = 0
    END IF
  END SUBROUTINE sub_load_data_t1
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE THE TIME SCALE SAVE STRING
  SUBROUTINE sub_init_char()
    ! local variables
    INTEGER::n,loc_digit
    REAL::loc_t
    ! find the length (in number of digits) of the longest run-time date
    ! NOTE: add par_misc_t_err to the real number before integer conversion to ensure that the integer part is correctly extracted
    n_char_years = 0
    loc_t = MAX(par_misc_t_start,par_misc_t_end) + par_misc_t_err
    DO n=99,1,-1
       loc_digit = INT(loc_t*10.0**(-(n-1)) + par_misc_t_err)
       IF (loc_digit > 0) THEN
          n_char_years = n
          EXIT
       END IF
    END DO
    ! set number of decimal places (in years) that save time is stored to
    n_char_years_fractional = 3
  END SUBROUTINE sub_init_char
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FORM THE TIME-SLICE FILENAME
  FUNCTION fun_data_timeslice_filename(dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext)
    ! result variable
    CHARACTER(len=255)::fun_data_timeslice_filename
    ! dummy arguments
    CHARACTER(len=*),INTENT(in)::dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_t_int,loc_t_int_fractional
    REAL::loc_t,loc_t_fractional
    CHARACTER(len=n_char_years)::loc_char_years
    CHARACTER(len=n_char_years_fractional)::loc_char_years_fractional
    ! form filename
    IF (ctrl_misc_t_BP) THEN
       loc_t = par_data_save_timeslice(par_data_save_timeslice_i) + par_misc_t_end
    ELSE
       loc_t = par_misc_t_end - par_data_save_timeslice(par_data_save_timeslice_i)
    END IF
    !
    loc_t_int = INT(loc_t)
    loc_char_years = fun_conv_num_char_n(n_char_years,loc_t_int)
    IF (opt_data(iopt_data_save_timeslice_fnint)) THEN
       loc_filename = &
            & TRIM(dum_string_dir)// &
            & TRIM(dum_string_runid)//'_'//loc_char_years//'_'//TRIM(dum_string_name)// &
            & TRIM(dum_string_ext)
    ELSE
       IF (loc_t > 0.0) THEN
          loc_t_fractional = loc_t - real(loc_t_int)
       ELSE
          loc_t_fractional = 0.0
       END IF
       loc_t_int_fractional = INT(loc_t_fractional*10**n_char_years_fractional)
       loc_char_years_fractional = fun_conv_num_char_n(n_char_years_fractional,loc_t_int_fractional)
       loc_filename = &
            & TRIM(dum_string_dir)// &
            & TRIM(dum_string_runid)//'_'//loc_char_years//'_'//loc_char_years_fractional//'_'//TRIM(dum_string_name)// &
            & TRIM(dum_string_ext)
    END IF
    ! return function value
    fun_data_timeslice_filename = loc_filename
  END FUNCTION fun_data_timeslice_filename
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FORM THE TIME-SERIES FILENAME
  FUNCTION fun_data_timeseries_filename(dum_t,dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext)
    ! result variable
    CHARACTER(len=255)::fun_data_timeseries_filename
    ! dummy arguments
    real,intent(in)::dum_t
    CHARACTER(len=*),INTENT(in)::dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_t_int,loc_t_int_fractional
    REAL::loc_t,loc_t_fractional
    CHARACTER(len=n_char_years)::loc_char_years
    CHARACTER(len=n_char_years_fractional)::loc_char_years_fractional
    ! form filename
    IF (ctrl_misc_t_BP) THEN
       loc_t = dum_t + par_misc_t_end
    ELSE
       loc_t = par_misc_t_end - dum_t
    END IF
    !
    loc_t_int = INT(loc_t)
    loc_char_years = fun_conv_num_char_n(n_char_years,loc_t_int)
    IF (loc_t > 0.0) THEN
       loc_t_fractional = loc_t - real(loc_t_int)
    ELSE
       loc_t_fractional = 0.0
    END IF
    loc_t_int_fractional = INT(loc_t_fractional*10**n_char_years_fractional)
    loc_char_years_fractional = fun_conv_num_char_n(n_char_years_fractional,loc_t_int_fractional)
    loc_filename = &
         & TRIM(dum_string_dir)// &
         & TRIM(dum_string_runid)//'_'//TRIM(dum_string_name)// &
         & TRIM(dum_string_ext)
    ! return function value
    fun_data_timeseries_filename = loc_filename
  END FUNCTION fun_data_timeseries_filename
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FORM THE TIME-SERIES FILENAME
  FUNCTION fun_data_timesnap_filename(dum_t,dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext)
    ! result variable
    CHARACTER(len=255)::fun_data_timesnap_filename
    ! dummy arguments
    real,intent(in)::dum_t
    CHARACTER(len=*),INTENT(in)::dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_t_int,loc_t_int_fractional
    REAL::loc_t,loc_t_fractional
    CHARACTER(len=n_char_years)::loc_char_years
    CHARACTER(len=n_char_years_fractional)::loc_char_years_fractional
    ! form filename
    IF (ctrl_misc_t_BP) THEN
       loc_t = dum_t + par_misc_t_end
    ELSE
       loc_t = par_misc_t_end - dum_t
    END IF
    !
    loc_t_int = INT(loc_t)
    loc_char_years = fun_conv_num_char_n(n_char_years,loc_t_int)
    IF (opt_data(iopt_data_save_timeslice_fnint)) THEN
       loc_filename = &
            & TRIM(dum_string_dir)// &
            & TRIM(dum_string_runid)//'_'//loc_char_years//'_'//TRIM(dum_string_name)// &
            & TRIM(dum_string_ext)
    ELSE
       IF (loc_t > 0.0) THEN
          loc_t_fractional = loc_t - real(loc_t_int)
       ELSE
          loc_t_fractional = 0.0
       END IF
       loc_t_int_fractional = INT(loc_t_fractional*10**n_char_years_fractional)
       loc_char_years_fractional = fun_conv_num_char_n(n_char_years_fractional,loc_t_int_fractional)
       loc_filename = &
            & TRIM(dum_string_dir)// &
            & TRIM(dum_string_runid)//'_'//loc_char_years//'_'//loc_char_years_fractional//'_'//TRIM(dum_string_name)// &
            & TRIM(dum_string_ext)
    END IF
    ! return function value
    fun_data_timesnap_filename = loc_filename
  END FUNCTION fun_data_timesnap_filename
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! MISC
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  subroutine sub_wasteCPUcycles1(dum_ocn,dum_string,dum_n)
    ! dummy valiables
    REAL,DIMENSION(n_ocn,n_i,n_j,n_k),INTENT(in)::dum_ocn        !
    CHARACTER(len=*),INTENT(in)::dum_string                      !
    INTEGER,INTENT(in)::dum_n                                    !
    ! local variables
    REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::loc_ocn                   !
    integer::n
    ! init
    loc_ocn(:,:,:,:) = 0.0
    print*,'@@@ START: ',dum_string
    ! waste CPU cycles!!!
    DO n=1,dum_n
       loc_ocn(:,:,:,:) = loc_ocn(:,:,:,:) + abs(dum_ocn(:,:,:,:))**0.5
    end DO
    print*,'@@@ END: ',dum_string
  END subroutine sub_wasteCPUcycles1
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  subroutine sub_wasteCPUcycles2(dum_ocn,dum_string,dum_n)
    ! dummy valiables
    REAL,DIMENSION(n_ocn,n_i,n_j,n_k),INTENT(in)::dum_ocn        !
    CHARACTER(len=*),INTENT(in)::dum_string                      !
    INTEGER,INTENT(in)::dum_n                                    !
    ! local variables
    REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::loc_ocn                   !
    integer::n
    ! init
    loc_ocn(:,:,:,:) = 0.0
    print*,'@@@ START: ',dum_string
    ! waste CPU cycles!!!
    DO n=1,dum_n
       loc_ocn(:,:,:,:) = loc_ocn(:,:,:,:) + abs(dum_ocn(:,:,:,:))**0.5
    end DO
    print*,'@@@ END: ',dum_string
  END subroutine sub_wasteCPUcycles2
  ! ****************************************************************************************************************************** !


END MODULE biogem_lib
