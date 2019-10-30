! ******************************************************************************************************************************** !
! biogem_data.f90
! BioGeM
! DATA LOADING/SAVING ROUTINES
! ******************************************************************************************************************************** !


MODULE biogem_data


  USE biogem_lib
  USE biogem_box
  USE biogem_data_netCDF
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! DATA LOADING ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD BioGeM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_biogem()
    USE genie_util, ONLY: check_unit,check_iostat
    ! local variables
    integer::l,io,ia                                                    ! tracer counter
    integer::ios                                                        !
    ! read data_BIOGEM file
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file='data_BIOGEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open BIOGEM initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_BIOGEM file
    read(UNIT=in,NML=ini_biogem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read BIOGEM namelist'
       stop
    else
       close(unit=in,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    ! set and report namelist data
    par_indir_name = trim(par_indir_name)//'/'
    par_outdir_name = trim(par_outdir_name)//'/'
    par_rstdir_name = trim(par_rstdir_name)//'/'
    par_fordir_name = trim(par_fordir_name)//'/'
    ! *************************************************************
    ! *** HARD SET RESTART NAME BECAUSE NAMELIST IS BEING PANTS ***
    ! *************************************************************
    par_ncrst_name = '_restart.nc'
    ! *************************************************************
    if (ctrl_debug_init > 0) then
       ! --- TRACER INITIALIZATION  ---------------------------------------------------------------------------------------------- !
       print*,'--- INITIALIZATION ---------------------------------'
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          print*,'ocn tracer initial value: ',trim(string_ocn(io)),' = ',ocn_init(io)
          print*,'ocn tracer perturbation : ',trim(string_ocn(io)),' = ',ocn_dinit(io)
       end do
       print*,'Absolute (not relative) tracer re-start adjustment? : ',ctrl_ocn_dinit
       ! --- RUN CONTROL --------------------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM TIME CONTROL ----------------------------'
       print*,'Continuing run?                                     : ',ctrl_continuing
       print*,'Simulation start year                               : ',par_misc_t_start
       print*,'Simulation run length (yr)                          : ',par_misc_t_runtime
       print*,'Time as Years Before Present?                       : ',ctrl_misc_t_BP
       print*,'Simulation stop year                                : ',par_misc_t_stop
       ! --- MISC CONTROL -------------------------------------------------------------------------------------------------------- !
       print*,'--- MISC CONTROL -----------------------------------'
       print*,'Salanity normalization?                             : ',ctrl_misc_Snorm
       print*,'No salanity normalization?                          : ',ctrl_misc_noSnorm
       print*,'No biological update (and transformations)?         : ',ctrl_misc_nobioupdate
       print*,'Sea-ice brine rejection fraction                    : ',par_misc_brinerejection_frac
       print*,'Max j for sea-ice brine rejection                   : ',par_misc_brinerejection_jmax
       print*,'Include biogeochem in sea-ice brine rejection?      : ',ctrl_misc_brinerejection_bgc
       print*,'Geoengineering scheme ID string                     : ',trim(opt_misc_geoeng)
       print*,'Filename for generic 2D field                       : ',trim(par_misc_2D_file)
       print*,'scalar for generic misc 2D field                    : ',par_misc_2D_scale
       print*,'Min k for geoengineering ocean pipes!               : ',par_misc_kmin_pipe
       print*,'Exclude DIC from geoenginering?                     : ',ctrl_misc_geoeng_noDIC
       print*,'Overwrite restart temperatures?                     : ',ctrl_ocn_rst_reset_T
       print*,'Full (entire grid) carbonate chem update?           : ',ctrl_carbchemupdate_full
       ! --- BOUNDARY CONDITIONS ------------------------------------------------------------------------------------------------- !
       print*,'--- BOUNDARY CONDITIONS ----------------------------'
       print*,'Set dissolution flux = rain flux to close system?   : ',ctrl_force_sed_closedsystem
       print*,'Balance the P cycle (with weathering)?              : ',ctrl_force_sed_closed_P
       print*,'Allow temperature / salinity forcing of climate?    : ',ctrl_force_GOLDSTEInTS
       print*,'Allow ONLY temperature / salinity forcing?          : ',ctrl_force_GOLDSTEInTSonly
       print*,'Replace internal fractional sea-ice cover field?    : ',ctrl_force_seaice
       print*,'Replace internal wind-speed field?                  : ',ctrl_force_windspeed
       print*,'Replace internal CaCO3:POC export rain ratio?       : ',ctrl_force_CaCO3toPOCrainratio
       print*,'Replace internal POCd:POC export rain ratio?        : ',ctrl_force_POCdtoPOCrainratio
       print*,'Replace internal [Cd/P]POM/[Cd/P]SW alpha?          : ',ctrl_force_Cd_alpha
       print*,'Replace uniform CaCO3 scavenging coefficient?       : ',ctrl_force_CaCO3ballastcoeff
       print*,'Replace uniform opal scavenging coefficient?        : ',ctrl_force_opalballastcoeff
       print*,'Replace uniform det scavenging coefficient?         : ',ctrl_force_detballastcoeff
       print*,'Replace internal POC flux for 230Th/231Pa scav.     : ',ctrl_force_scav_fpart_POC
       print*,'Replace internal CaCO3 flux for 230Th/231Pa scav.   : ',ctrl_force_scav_fpart_CaCO3
       print*,'Replace internal opal flux for 230Th/231Pa scav.    : ',ctrl_force_scav_fpart_opal
       print*,'Replace internal det flux for 230Th/231Pa scav.     : ',ctrl_force_scav_fpart_det
       print*,'Value of Wanninkhof [1992] gas transfer coeff (a)   : ',par_gastransfer_a
       print*,'Filename for imposed seaice                         : ',trim(par_seaice_file)
       print*,'Filename for imposed windspeed                      : ',trim(par_windspeed_file)
       print*,'Filename for imposed CaCO3toPOCrainratio_file       : ',trim(par_CaCO3toPOCrainratio_file)
       print*,'Filename for imposed POCdtoPOCrainratio             : ',trim(par_POCdtoPOCrainratio_file)
       print*,'Filename for imposed Cd_alpha                       : ',trim(par_Cd_alpha_file)
       print*,'Filename for CaCO3 ballast coefficient field        : ',trim(par_CaCO3ballastcoeff_file)
       print*,'Filename for opal ballast coefficient field         : ',trim(par_opalballastcoeff_file)
       print*,'Filename for det ballast coefficient field          : ',trim(par_detballastcoeff_file)
       print*,'Filename for imposed scavenging POC flux            : ',trim(par_scav_fpart_POC_file)
       print*,'Filename for imposed scavenging CaCO3 flux          : ',trim(par_scav_fpart_CaCO3_file)
       print*,'Filename for imposed scavenging opal flux           : ',trim(par_scav_fpart_opal_file)
       print*,'Filename for imposed scavenging det flux            : ',trim(par_scav_fpart_det_file)
       print*,'Replace solar constant?                             : ',ctrl_force_solconst
       print*,'Use old tracer forcing file format?                 : ',ctrl_force_oldformat
       print*,'Forcings name                                       : ',trim(par_forcing_name)
       print*,'Air-sea H2S flux fix ...                            : ',trim(opt_ocnatmH2S_fix)
       print*,'Geothermal heat flux (W m-2)                        : ',par_Fgeothermal
       print*,'Use 2D geothermal heat input field?                 : ',ctrl_force_Fgeothermal2D
       print*,'Filename for 2D geothermal heat input field         : ',trim(par_force_Fgeothermal2D_file)
       ! --- BIOLOGICAL NEW PRODUCTION ------------------------------------------------------------------------------------------- !
       print*,'--- BIOLOGICAL NEW PRODUCTION ----------------------'
       print*,'Biological scheme ID string                         : ',par_bio_prodopt
       print*,'Base [PO4] uptake rate (mol kg-1 yr-1)              : ',par_bio_k0_PO4
       print*,'Base [NO3] uptake rate (mol kg-1 yr-1)              : ',par_bio_k0_NO3
       print*,'[PO4] M-M half-sat value (mol kg-1)                 : ',par_bio_c0_PO4
       print*,'[PO4] M-M half-sat value [SP]                       : ',par_bio_c0_PO4_sp
       print*,'[PO4] M-M half-sat value [NSP]                      : ',par_bio_c0_PO4_nsp
       print*,'[NO3] M-M half-sat value (mol kg-1)                 : ',par_bio_c0_NO3
       print*,'[NO3]+[NH4] M-M half-sat value (mol kg-1)           : ',par_bio_c0_N
       print*,'[Fe] M-M half-sat value (mol kg-1)                  : ',par_bio_c0_Fe
       print*,'[Fe] M-M half-sat value for diazotrophs (mol kg-1)  : ',par_bio_c0_Fe_Diaz
       print*,'[Fe] M-M half-sat value [SP]                        : ',par_bio_c0_Fe_sp
       print*,'[Fe] M-M half-sat value [NSP]                       : ',par_bio_c0_Fe_nsp
       print*,'[H4SiO4] M-M half-sat value (mol kg-1)              : ',par_bio_c0_SiO2
       print*,'[H4SiO4] M-M half-sat value [SP]                    : ',par_bio_c0_SiO2_sp
       print*,'[H4SiO4] M-M half-sat value [NSP]                   : ',par_bio_c0_SiO2_nsp
       print*,'Biological production zone depth (m) (OCMIP-2)      : ',par_bio_zc
       print*,'Biological production time-scale (days) (OCMIP-2)   : ',par_bio_tau
       print*,'Biological production time-scale -- siliceous plank : ',par_bio_tau_sp
       print*,'Biological production time-scale -- non-siliceous   : ',par_bio_tau_nsp
       print*,'Fract. prod. of si. phytop. in Si/Fe-replete cond.  : ',par_bio_relprod_sp
       print*,'Light e-folding depth (m) (OCMIP-2)                 : ',par_bio_I_eL
       print*,'Coefficient for T-dep. uptake rate modifier         : ',par_bio_kT0
       print*,'e-folding temp. (K) for T-dep. uptake rate modifier : ',par_bio_kT_eT
       ! --- ORGANIC MATTER EXPORT RATIOS ---------------------------------------------------------------------------------------- !
       print*,'--- ORGANIC MATTER EXPORT RATIOS -------------------'
       print*,'N/P organic matter Redfield ratio                   : ',par_bio_red_POP_PON
       print*,'C/P organic matter Redfield ratio                   : ',par_bio_red_POP_POC
       print*,'C/P organic matter set to flexible? ( > 0 = yes)    : ',par_bio_red_PC_flex
       print*,'PC_alpha1, scaling of flexible C/P ratio            : ',par_bio_red_PC_alpha1
       print*,'PC_alpha2, offset of flexible C/P ratio             : ',par_bio_red_PC_alpha2
       print*,'maximum C/P                                         : ',par_bio_red_PC_max
       print*,'O2/P organic matter pseudo-Redfield ratio           : ',par_bio_red_POP_PO2
       print*,'ALK/N alkalinty correction factor                   : ',par_bio_red_PON_ALK
       print*,'Production fraction of dissolved organic matter     : ',par_bio_red_DOMfrac
       print*,'Production fraction of R-dissolved organic matter   : ',par_bio_red_RDOMfrac
       print*,'P:C fractionation during POM->DOM production        : ',par_bio_red_rP_POM_DOM
       print*,'N:C fractionation during POM->DOM production        : ',par_bio_red_rN_POM_DOM
       print*,'P:C fractionation during POM->RDOM production       : ',par_bio_red_rP_POM_RDOM
       print*,'N:C fractionation during POM->RDOM production       : ',par_bio_red_rN_POM_RDOM
       print*,'Tie ALK with POC (rather than POP)?                 : ',ctrl_bio_red_ALKwithPOC
       print*,'Tie O2 consumption with POC (rather than POP)?      : ',ctrl_bio_red_O2withPOC
       ! --- INORGANIC MATTER EXPORT RATIOS -------------------------------------------------------------------------------------- !
       print*,'--- INORGANIC MATTER EXPORT RATIOS -----------------'
       print*,'CaCO3:POC rain ratio option ID string               : ',opt_bio_CaCO3toPOCrainratio
       print*,'Base CaCO3:POC export ratio                         : ',par_bio_red_POC_CaCO3
       print*,'Exponent for modifier of CaCO3:POC export ratio     : ',par_bio_red_POC_CaCO3_pP
       print*,'Ohmega half-sat constant [Gehlen et al., 2007]      : ',par_bio_red_POC_CaCO3_Kmax
       print*,'Heinze [2004] CO2aq reference conc (umol kg-1)      : ',par_bio_red_POC_CaCO3_CO2aqREF
       print*,'Barker et al. [2003] CO3 reference conc (umol kg-1) : ',par_bio_red_POC_CaCO3_CO3REF
       print*,'Base opal:POC export ratio                          : ',par_bio_red_POC_opal
       print*,'Ridgwell [2001] -- opal:POC KSp for FeT (mol kg-1)  : ',par_part_red_opal_FeTKSp
       print*,'Ridgwell [2001] -- opal:POC offset, FeT (mol kg-1)  : ',par_part_red_opal_FeToff
       print*,'opal:POC rain ratio option ID string                : ',opt_bio_red_SitoC
       ! --- REMINERALIZATION ---------------------------------------------------------------------------------------------------- !
       print*,'--- REMINERALIZATION -------------------------------'
       print*,'Fraction of POM remin concverted to RDOM            : ',par_bio_remin_RDOMfrac
       print*,'DOM lifetime (yrs)                                  : ',par_bio_remin_DOMlifetime
       print*,'RDOM lifetime (yrs)                                 : ',par_bio_remin_RDOMlifetime
       print*,'RDOM degradation by (surface) photolysis only?      : ',ctrl_bio_remin_RDOM_photolysis
       print*,'Apply fixed-profile for POM remineralization?       : ',ctrl_bio_remin_POC_fixed
       print*,'Kinetic-based POM remineralization?                 : ',ctrl_bio_remin_POC_kinetic
       print*,'Remineralization functional form                    : ',par_bio_remin_fun
       print*,'Ballasting parameterization?                        : ',ctrl_bio_remin_POC_ballast
       print*,'Initial fractional abundance of POC component #2    : ',par_bio_remin_POC_frac2
       print*,'Remineralization length #1 for POC                  : ',par_bio_remin_POC_eL1
       print*,'Remineralization length #2 for POC                  : ',par_bio_remin_POC_eL2
       print*,'Power law power                                     : ',par_bio_remin_martin_b
       print*,'Power law z0                                        : ',par_bio_remin_z0
       print*,'Degradation rate constant #1 for POC                : ',par_bio_remin_POC_K1
       print*,'Degradation rate constant #2 for POC                : ',par_bio_remin_POC_K2
       print*,'Activation energy #1 for POC                        : ',par_bio_remin_POC_Ea1
       print*,'Activation energy #2 for POC                        : ',par_bio_remin_POC_Ea2
       print*,'Range of fractional abundance of POC component #2   : ',par_bio_remin_POC_dfrac2
       print*,'Fractional abundance of POC #2 half sat             : ',par_bio_remin_POC_c0frac2
       print*,'Apply fixed-profile for CaCO3 remineralization?     : ',ctrl_bio_remin_CaCO3_fixed
       print*,'Initial fractional abundance of CaCO3 component #2  : ',par_bio_remin_CaCO3_frac2
       print*,'Remineralization length #1 for CaCO3                : ',par_bio_remin_CaCO3_eL1
       print*,'Remineralization length #2 for CaCO3                : ',par_bio_remin_CaCO3_eL2
       print*,'Apply fixed-profile for opal remineralization?      : ',ctrl_bio_remin_opal_fixed
       print*,'Initial fractional abundance of opal component #2   : ',par_bio_remin_opal_frac2
       print*,'Remineralization length #1 for opal                 : ',par_bio_remin_opal_eL1
       print*,'Remineralization length #2 for opal                 : ',par_bio_remin_opal_eL2
       print*,'Prescribed particle sinking rate (m d-1)            : ',par_bio_remin_sinkingrate
       print*,'Prescribed scavenging sinking rate (m d-1)          : ',par_bio_remin_sinkingrate_scav
       print*,'Explicit physical particle sinking rate (m d-1)     : ',par_bio_remin_sinkingrate_physical
       print*,'Implicit sinking rate for geochem reactions (m d-1) : ',par_bio_remin_sinkingrate_reaction
       print*,'Organic matter carrying capacity of CaCO3           : ',par_bio_remin_ballast_kc
       print*,'Organic matter carrying capacity of opal            : ',par_bio_remin_ballast_ko
       print*,'Organic matter carrying capacity of lithogenics     : ',par_bio_remin_ballast_kl
       print*,'Aerobic remineralization of OM -> NH4 (not NO3)?    : ',ctrl_bio_remin_ONtoNH4
       print*,'Denitrification [O2] threshold (mol kg-1)           : ',par_bio_remin_denitrO2thresh
       print*,'Apply a hard tracer oxidant remin threshold?        : ',ctrl_bio_remin_thresh
       print*,'Hard threshold for oxic remin (mol kg-1)            : ',par_bio_remin_cthresh_O2
       print*,'Hard threshold for denitrification (mol kg-1)       : ',par_bio_remin_cthresh_NO3
       print*,'Hard threshold for sulphate reduction (mol kg-1)    : ',par_bio_remin_cthresh_SO4
       print*,'Catch rapidly-oxidizing species going < 0.0?        : ',ctrl_bio_remin_reminfix
       print*,'NH4 -> NO3 odixation option                         : ',trim(opt_bio_remin_oxidize_NH4toNO3)
       print*,'H2S -> SO4 oxidation option                         : ',trim(opt_bio_remin_oxidize_H2StoSO4)
       print*,'H2S -> POCS scavenging option                       : ',trim(opt_bio_remin_scavenge_H2StoPOMS)
       print*,'Remin rate -- oxic (yr-1)                           : ',par_bio_remin_k_O2
       print*,'Remin rate -- denitrification (yr-1)                : ',par_bio_remin_k_NO3
       print*,'Remin rate -- sulphate reduction (yr-1)             : ',par_bio_remin_k_SO4
       print*,'Remin rate -- methanogenesis (yr-1)                 : ',par_bio_remin_k_meth
       print*,'Half-saturation for oxic remin (mol kg-1)           : ',par_bio_remin_c0_O2
       print*,'Half-saturation for denitrification (mol kg-1)      : ',par_bio_remin_c0_NO3
       print*,'Half-saturation for sulphate reduction (mol kg-1)   : ',par_bio_remin_c0_SO4
       print*,'Inhibition constant by oxygen (mol kg-1)            : ',par_bio_remin_ci_O2
       print*,'Inhibition constant by nitrate (mol kg-1)           : ',par_bio_remin_ci_NO3
       print*,'Inhibition constant by sulphate (mol kg-1)          : ',par_bio_remin_ci_SO4
       print*,'Oxidation rate constant for H2S -> SO4              : ',par_bio_remin_kH2StoSO4
       print*,'Oxidation rate constant for NH4 -> NO2              : ',par_bio_remin_kNH4toNO2
       print*,'Oxidation rate constant for NO2 -> NO3              : ',par_bio_remin_kNO2toNO3
       print*,'Oxidation rate constant for NO2 -> N2O              : ',par_bio_remin_kNO2toN2O
       print*,'NH4 half-saturation for for NH4 -> NO2              : ',par_bio_remin_cNH4_NH4toNO2
       print*,'O2 half-saturation for for NH4 -> NO2               : ',par_bio_remin_cO2_NH4toNO2
       print*,'NO2 half-saturation for for NO2 -> NO3              : ',par_bio_remin_cNO2_NO2toNO3
       print*,'O2 half-saturation for for NO2 -> NO3               : ',par_bio_remin_cO2_NO2toNO3
       print*,'NO2 half-saturation for for NO2 -> N2O              : ',par_bio_remin_cNO2_NO2toN2O
       print*,'O2 half-saturation for for NO2 -> N2O               : ',par_bio_remin_cO2_NO2toN2O
       print*,'Fraction of NH4 oxidation -> N2O (rather than NO2)  : ',par_bio_remin_fracN2O
       print*,'Reaction rate constant for H2S -> POMS              : ',par_bio_remin_kH2StoPOMS
       print*,'Aerobic methanotrophy scheme ID string              : ',par_bio_remin_CH4ox
       print*,'Specific CH4 oxidation rate (d-1)                   : ',par_bio_remin_CH4rate
       print*,'AER rate constant (yr-1)                            : ',par_bio_remin_AER_kAER
       print*,'AER O2 half sat constant (mol kg-1)                 : ',par_bio_remin_AER_Km_O2
       print*,'Thermodynamic drive ID string for AER               : ',par_bio_remin_AER_thermo
       print*,'Std Gibbs free energy of AER (kJ mol-1)             : ',par_bio_remin_AER_dG0
       print*,'Biological energy quantum (BEQ) for AER (kJ mol-1)  : ',par_bio_remin_AER_BEQ
       print*,'AOM rate constant (yr-1)                            : ',par_bio_remin_AOM_kAOM
       print*,'AOM SO4 half sat constant (mol kg-1)                : ',par_bio_remin_AOM_Km_SO4
       print*,'Thermodynamic drive ID string for AOM               : ',par_bio_remin_AOM_thermo
       print*,'Std Gibbs free energy of AOM (kJ mol-1)             : ',par_bio_remin_AOM_dG0
       print*,'Biological energy quantum (BEQ) for AOM (kJ mol-1)  : ',par_bio_remin_AOM_BEQ
       print*,'Gas constant for thermo calculations (kJ K-1 mol-1) : ',par_bio_remin_Rgas
       print*,'Activity coefficient for O2                         : ',par_bio_remin_gammaO2
       print*,'Activity coefficient for CO2                        : ',par_bio_remin_gammaCO2
       print*,'Activity coefficient for HS                         : ',par_bio_remin_gammaHS
       print*,'Activity coefficient for HCO3                       : ',par_bio_remin_gammaHCO3
       print*,'Activity coefficient for SO4                        : ',par_bio_remin_gammaSO4
       print*,'Activity coefficient for CH4                        : ',par_bio_remin_gammaCH4
       print*,'I -> IO3 oxidation option                           : ',trim(opt_bio_remin_oxidize_ItoIO3)
       print*,'IO3 -> I reduction option                           : ',trim(opt_bio_remin_reduce_IO3toI)
       print*,'(oxidation) lifetime for I (yrs)                    : ',par_bio_remin_Ilifetime
       print*,'Oxidation rate constant for I -> IO3                : ',par_bio_remin_kItoIO3
       print*,'Reduction rate constant for IO3 -> I                : ',par_bio_remin_kIO3toI
       print*,'O2 half-saturation for for I -> IO3                 : ',par_bio_remin_cO2_ItoIO3
       print*,'O2 half-saturation for IO3 -> I                     : ',par_bio_remin_cO2_IO3toI
       print*,'IO3 half-saturation for IO3 -> I                    : ',par_bio_remin_cIO3_IO3toI
       ! ------------------- ISOTOPIC FRACTIONATION ------------------------------------------------------------------------------ !
       print*,'Corg 13C fractionation scheme ID string             : ',trim(opt_d13C_DIC_Corg)
       print*,'CaCO3 44Ca fractionation scheme ID string           : ',trim(opt_d44Ca_Ca_CaCO3)
       print*,'b value for Popp et al. fractionation               : ',par_d13C_DIC_Corg_b
       print*,'fractionation for intercellular C fixation          : ',par_d13C_DIC_Corg_ef
       print*,'fract. for intercell. C fixation of si. phytop.     : ',par_d13C_DIC_Corg_ef_sp
       print*,'fract. for intercell. C fixation of non-si. phytop. : ',par_d13C_DIC_Corg_ef_nsp
       print*,'30/28Si fractionation between H4SiO4 and opal       : ',par_d30Si_opal_epsilon
       print*,'*** d114Cd = 1.0006 ***                             : ',par_d114Cd_POCd_epsilon
       print*,'114/???Cd fractionation between Cd and CdCO3        : ',par_d114Cd_CdCO3_epsilon
       print*,'7/6Li fractionation between Li and LiCO3            : ',par_d7Li_LiCO3_epsilon
       print*,'Planktic foram 13C fractionation scheme ID string   : ',opt_bio_foram_p_13C_delta
       print*,'44/40Ca fractionation between Ca and CaCO3          : ',par_d44Ca_CaCO3_epsilon
       print*,'88/86Sr fractionation between Sr and SrCO3          : ',par_d88Sr_SrCO3_epsilon
       print*,'methanogenesis fractionation                        : ',par_d13C_Corg_CH4_epsilon
       ! --- IRON CYCLING -------------------------------------------------------------------------------------------------------- !
       print*,'--- IRON CYCLING -----------------------------------'
       print*,'Aeolian Fe solubility                               : ',par_det_Fe_sol
       print*,'Exponent for aeolian Fe solubility                  : ',par_det_Fe_sol_exp
       print*,'Fixed cellular Fe:C ratio?                          : ',ctrl_bio_red_fixedFetoC
       print*,'C/Fe organic matter ratio                           : ',par_bio_red_POFe_POC
       print*,'Fixed scavening rate (if not: Parekh scheme)?       : ',ctrl_bio_Fe_fixedKscav
       print*,'Fixed Fe scavenging rate (d-1)                      : ',par_scav_Fe_Ks
       print*,'Parekh Fe scavenging rate scale factor: POC         : ',par_scav_Fe_sf_POC
       print*,'Parekh Fe scavenging rate scale factor: CaCO3       : ',par_scav_Fe_sf_CaCO3
       print*,'Parekh Fe scavenging rate scale factor: opal        : ',par_scav_Fe_sf_opal
       print*,'Parekh Fe scavenging rate scale factor: det         : ',par_scav_Fe_sf_det
       print*,'Fraction of scavenged Fe that can be remineralized  : ',par_scav_fremin
       print*,'Prevent return of Fe from the sediments?            : ',ctrl_bio_NO_fsedFe
       print*,'log10 of Fe ligand stability constant (K`(FeL))     : ',par_K_FeL_pP
       print*,'[FeT] dependent Fe:C ratio -- power                 : ',par_bio_FetoC_pP
       print*,'[FeT] dependent Fe:C ratio -- scaling               : ',par_bio_FetoC_K
       print*,'[FeT] dependent Fe:C ratio -- constant              : ',par_bio_FetoC_C
       print*,'Use Ridgwell [2001] sp and nsp Fe:C functions?      : ',ctrl_bio_red_Ridgwell2001FetoC
       print*,'              : ',opt_geochem_Fe
       print*,'              : ',par_lookup_Fe3_file
       print*,'              : ',par_lookup_geo_file
       print*,'              : ',par_lookup_Fe_file_1
       print*,'              : ',par_lookup_Fe_file_2
       print*,'              : ',par_lookup_Fe_file_3
       print*,'              : ',par_lookup_Fe_file_4
       ! --- SILICA CYCLING ------------------------------------------------------------------------------------------------------ !
       print*,'--- SILICA CYCLING ---------------------------------'
       print*,'opal particulate base dissolution rate (d-1)        : ',par_bio_remin_opal_K
       ! --- NITROGEN CYCLING ---------------------------------------------------------------------------------------------------- !
       print*,'--- NITROGEN CYCLING -------------------------------'
       print*,'mu-1 max rate of export production (yr-1)           : ',par_bio_mu1
       print*,'mu-2 max rate of export from N2-fixation (yr-1)     : ',par_bio_mu2
       print*,'threshold NO3+NH4 for N2 fixation (mol kg-1)        : ',par_bio_N2fixthresh
       print*,'N:P ratio of diazotrophs                            : ',par_bio_NPdiaz
       print*,'constant for dynamical threshold for N2 fixation    : ',par_bio_N2fixdyn
       print*,'N* offset (mol kg-1)                                : ',par_bio_Nstar_offset
       print*,'par_nitri_mu                                        : ',par_nitri_mu
       print*,'par_nitri_c0_NH4                                    : ',par_nitri_c0_NH4
       print*,'par_nitri_c0_O2                                     : ',par_nitri_c0_O2
       ! --- TRACE METAL/ELEMENT CYCLING ----------------------------------------------------------------------------------------- !
       print*,'--- TRACE METAL/ELEMENT CYCLING --------------------'
       print*,'Default cellular C:Cd (Cd/C) ratio                  : ',par_bio_red_POC_POCd
       print*,'[Cd/P]POM/[Cd/P]SW partition coefficient (alpha)    : ',par_bio_red_POC_POCd_alpha
       print*,'Fe-limitation dependent Cd:C uptake ratio?          : ',ctrl_bio_red_CdtoC_Felim
       print*,'Minimum (Fe replete) Cd:C uptake ratio              : ',par_bio_red_CdtoC_Felim_min
       print*,'Maximum (Fe limited) Cd:C uptake ratio              : ',par_bio_red_CdtoC_Felim_max
       print*,'Default CaCO3 Ca:Li ratio                           : ',par_bio_red_CaCO3_LiCO3
       print*,'partition coefficient (alpha)                       : ',par_bio_red_CaCO3_LiCO3_alpha
       print*,'Default CaCO3 Ca:Cd ratio                           : ',par_bio_red_CaCO3_CdCO3
       print*,'partition coefficient (alpha)                       : ',par_bio_red_CaCO3_CdCO3_alpha
       print*,'Default CaCO3 Sr:Cd ratio                           : ',par_bio_red_CaCO3_SrCO3
       print*,'partition coefficient (alpha)                       : ',par_bio_red_CaCO3_SrCO3_alpha
       print*,'Default cellular C:I (I/C) ratio                    : ',par_bio_red_POC_POI
       print*,'Reference [IO3-] value @ default C:I ratio          : ',par_bio_red_POC_POI_C0
       ! --- ABIOTIC PRECIPITATION ----------------------------------------------------------------------------------------------- !
       print*,'--- ABIOTIC PRECIPITATION --------------------------'
       print*,'Allow abiotic CaCO3 precipitation?                  : ',ctrl_bio_CaCO3precip
       print*,'Restrict precipitation to surface layer?            : ',ctrl_bio_CaCO3precip_sur
       print*,'Precipitate as calcite (otherwise aragonite)        : ',par_bio_CaCO3precip_calcite
       print*,'Minimum ohmega threshold for precip                 : ',par_bio_CaCO3precip_abioticohm_min
       print*,'Scale factor for CaCO3 precipitation                : ',par_bio_CaCO3precip_sf
       print*,'Rate law power for CaCO3 precipitation              : ',par_bio_CaCO3precip_exp
       ! --- I/O DIRECTORY DEFINITIONS ------------------------------------------------------------------------------------------- !
       print*,'--- I/O DIRECTORY DEFINITIONS ----------------------'
       print*,'(Paleo config) input dir. name                      : ',trim(par_pindir_name)
       print*,'Input dir. name                                     : ',trim(par_indir_name)
       print*,'Output dir. name                                    : ',trim(par_outdir_name)
       print*,'Restart (input) dir. name                           : ',trim(par_rstdir_name)
       print*,'Forcings (input) dir. name                          : ',trim(par_fordir_name)
       print*,'Filename for restart input                          : ',trim(par_infile_name)
       print*,'Filename for restart output                         : ',trim(par_outfile_name)
       ! --- DATA SAVING: TIME-SLICES -------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM DATA SAVING: TIME-SLICES ----------------'
       print*,'Atmospheric (interface) composition (2D)?           : ',ctrl_data_save_slice_ocnatm
       print*,'Ocean composition (3D)?                             : ',ctrl_data_save_slice_ocn
       print*,'Sediment (interface) composition (2D)?              : ',ctrl_data_save_slice_ocnsed
       print*,'Export flux?                                        : ',ctrl_data_save_sig_fexport
       print*,'Air-sea gas exchange flux (2D)?                     : ',ctrl_data_save_slice_fairsea
       print*,'Ocean-sediment flux (2D)?                           : ',ctrl_data_save_slice_focnsed
       print*,'Sediment-ocean flux (2D)?                           : ',ctrl_data_save_slice_fsedocn
       print*,'Biological fluxes (3D)?                             : ',ctrl_data_save_slice_bio
       print*,'Aqueous carbonate system properties (3D)?           : ',ctrl_data_save_slice_carb
       print*,'Aqueous carbonate system constants (3D)?            : ',ctrl_data_save_slice_carbconst
       print*,'Atmospheric physical properties (2D)?               : ',ctrl_data_save_slice_phys_atm
       print*,'Ocean physical properties (3D)?                     : ',ctrl_data_save_slice_phys_ocn
       print*,'Miscellaneous properties (-)?                       : ',ctrl_data_save_slice_misc
       print*,'Biogeochemical diagnostics (3D)?                    : ',ctrl_data_save_slice_diag
       print*,'redox back-compatability                            : ',ctrl_data_save_slice_diag_redox_old
       print*,'Surface fields?                                     : ',ctrl_data_save_slice_sur
       print*,'Integration interval (yr)                           : ',par_data_save_slice_dt
       print*,'Filename for time-slice definition input            : ',trim(par_infile_slice_name)
       print*,'Number of timesteps in sub-inteval saving           : ',par_data_save_slice_n
       print*,'Auto save at run end?                               : ',ctrl_data_save_slice_autoend
       print*,'Save cdrmip data (only)?                            : ',ctrl_data_save_slice_cdrmip
       ! --- DATA SAVING: TIME-SERIES -------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM DATA SAVING: TIME-SERIES ----------------'
       print*,'Atmospheric (interface) composition?                : ',ctrl_data_save_sig_ocnatm
       print*,'Oceanic composition?                                : ',ctrl_data_save_sig_ocn
       print*,'Export flux?                                        : ',ctrl_data_save_sig_fexport
       print*,'Air-sea gas exchange flux ?                         : ',ctrl_data_save_sig_fairsea
       print*,'Sediment (interface) composition?                   : ',ctrl_data_save_sig_ocnsed
       print*,'Ocean->atmosphere flux?                             : ',ctrl_data_save_sig_focnatm
       print*,'Ocean->sediment flux?                               : ',ctrl_data_save_sig_focnsed
       print*,'Sediment->ocean flux/                               : ',ctrl_data_save_sig_fsedocn
       print*,'Ocean surface tracers?                              : ',ctrl_data_save_sig_ocn_sur
       print*,'Ocean surface carbonate chemistry?                  : ',ctrl_data_save_sig_carb_sur
       print*,'Miscellaneous properties?                           : ',ctrl_data_save_sig_misc
       print*,'Biogeochemical diagnostics?                         : ',ctrl_data_save_sig_diag
       print*,'redox back-compatability                            : ',ctrl_data_save_sig_diag_redox_old
       print*,'Integration interval (yr)                           : ',par_data_save_sig_dt
       print*,'Filename for time-series definition input           : ',trim(par_infile_sig_name)
       print*,'Auto save at run end?                               : ',ctrl_data_save_sig_autoend
       print*,'Save high res 3D data (@ time-series frequency)?    : ',ctrl_data_save_3d_sig
       print*,'Save 3D data at a particular ij location?           : ',ctrl_data_save_ocn_3D_ij
       ! --- DATA SAVING: DIAGNOSTICS -------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM DATA SAVING: DIAGNOSTICS ----------------'
       print*,'Create pre-formed tracers?                          : ',ctrl_bio_preformed
       print*,'Create redox/remin data for saving?                 : ',ctrl_bio_remin_redox_save
       ! --- DATA SAVING: MISC --------------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM DATA SAVING: MISC -----------------------'
       print*,'Degree of comprehensivity of data saving            : ',par_data_save_level
       print*,'Save derived data (e.g., S-normalized tracers)?     : ',ctrl_data_save_derived
       print*,'Save global diagnostics (at time-slice intervals)?  : ',ctrl_data_save_GLOBAL
       print*,'Save time-slice data in ASCII format?               : ',ctrl_data_save_slice_ascii
       print*,'Save time-series data in ASCII format?              : ',ctrl_data_save_sig_ascii
       print*,'append data to output files on restart              : ',opt_append_data
       print*,'Minimum depth for benthic average (m)               : ',par_data_save_ben_Dmin
       print*,'Time-step for snap-shot saving (N)                  : ',par_t_sig_count_N
       print*,'Time-step for snap-shot saving (S)                  : ',par_t_sig_count_S
       print*,'Generic N j value (for time-series data saving)     : ',par_sig_j_N
       print*,'Generic S j value (for time-series data saving)     : ',par_sig_j_S
       print*,'Restart in netCDF format?                           : ',ctrl_ncrst
       print*,'netCDF restart file name                            : ',trim(par_ncrst_name)
       print*,'Save 2D netCDF data?                                : ',ctrl_data_save_2d
       print*,'Save 3D netCDF data?                                : ',ctrl_data_save_3d
       print*,'i coordinate for saving water column results        : ',par_misc_save_i
       print*,'j coordinate for saving water column results        : ',par_misc_save_j
       print*,'                                                    : ',n_orb_pts_nmax
       print*,'                                                    : ',trim(par_infile_orb_pts_loc_name)
       print*,'                                                    : ',trim(par_infile_orb_pts_var_name)
       ! --- TRACER AUDITING AND DEBUGGING OPTIONS ------------------------------------------------------------------------------- !
       print*,'--- TRACER AUDITING AND DEBUGGING OPTIONS ----------'
       print*,'Audit tracer inventory?                             : ',ctrl_audit
       print*,'Halt on audit fail?                                 : ',ctrl_audit_fatal
       print*,'Max allowed relative tracer inventory change        : ',par_misc_audit_relerr
       print*,'Report all run-time warnings?                       : ',ctrl_debug_reportwarnings
       print*,'Report level #0 debug?                              : ',ctrl_debug_lvl0
       print*,'Report level #1 debug?                              : ',ctrl_debug_lvl1
       print*,'Report level #2 debug?                              : ',ctrl_debug_lvl2
       ! --- TRACER FORCING ------------------------------------------------------------------------------------------------------ !
       print*,'--- TRACER FORCING ---------------------------------'
       DO l=1,n_l_atm
          ia = conv_iselected_ia(l)
          print*,'atm tracer forcing time scale factor  : ',trim(string_atm(ia)),' = ',par_atm_force_scale_time(ia)
          print*,'atm tracer forcing value scale factor : ',trim(string_atm(ia)),' = ',par_atm_force_scale_val(ia)
       end do
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          print*,'ocn tracer forcing time scale factor  : ',trim(string_ocn(io)),' = ',par_ocn_force_scale_time(io)
          print*,'ocn tracer forcing value scale factor : ',trim(string_ocn(io)),' = ',par_ocn_force_scale_val(io)
       end do
       print*,'i coordinate of point forcing (0 = DISABLED)        : ',par_force_point_i
       print*,'j coordinate of point forcing (0 = DISABLED)        : ',par_force_point_j
       print*,'k coordinate of point forcing (0 = DISABLED)        : ',par_force_point_k
       print*,'Surface ocean saturation state target               : ',par_force_invert_ohmega
       print*,'Prevent negative inversion fluxes                   : ',ctrl_force_invert_noneg
       print*,'Calcite saturation as the saturation target?        : ',ctrl_force_ohmega_calcite
       print*,'Allow carbon removal via Corg?                      : ',ctrl_force_invert_Corgburial
       print*,'Scaling C burial flux relative to emissions         : ',par_force_invert_fCorgburial
       print*,'Force explicit inversion?                           : ',ctrl_force_invert_explicit
       print*,'Automatic ocean age tracer?                         : ',ctrl_force_ocn_age
       ! --- TRANSPORT MATRIX ------------------------------------------------------------------------------------------------------ !
       print*,'Diagnose transport matrix during run?		: ',ctrl_data_diagnose_TM
       print*,'Year to start diagnosing transport matrix	: ',par_data_TM_start
       print*,'Number of intervals within a year to diagnose transport matrix		: ',par_data_TM_avg_n
       ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ########################################################################## !
       !
       ! ######################################################################################################################### !
    end if
    ! ### TO BE CONVERTED TO NAMELIST ITEMS ###################################################################################### !
    par_misc_t_err = 3600.0*1.0/conv_yr_s ! time-stepping error == 1hr
    opt_data(iopt_data_save_timeslice_fnint) = .FALSE.
    opt_data(iopt_data_save_config) = .FALSE.
    opt_misc(iopt_misc_debugij) = .FALSE.
    par_misc_debug_i = 1
    par_misc_debug_j = 3
    opt_force(iopt_force_freshwater) = .FALSE.
    par_bio_c0_I = 20.0 ! half saturatin value for light (W m-2) [Doney et al., 2006] (30.0 in Parekth et al. [2005])
    par_det_Fe_frac = 0.035 ! mass fraction of Fe in dust
    par_K_FeL = 10**par_K_FeL_pP ! conditional stability constant of ligand-bound Fe [Parekth et al., 2005]
    par_scav_Fe_exp = 0.58 ! (see: Parekth et al. [2005])
    par_scav_Fe_k0  = 0.079 ! (see: Parekth et al. [2005])
    par_scav_Fe_k0 = par_scav_Fe_k0/conv_d_yr ! adjust units of scavening rate constant (d-1 -> yr-1)
    par_part_red_FeTmin = 0.125E-9 ! (see: Ridgwell [2001])
    par_part_red_FetoCmax = 250000.0 !
    ! ############################################################################################################################ !
    ! -------------------------------------------------------- !
    ! backwards compatability
    ! -------------------------------------------------------- !
    ! filter CaCO3:POC rain ratio options for backwards compatability
    if (ctrl_force_CaCO3toPOCrainratio) opt_bio_CaCO3toPOCrainratio = 'prescribed'
    ! sinking -- if par_bio_remin_sinkingrate_physical not set, then re-use values specific by original parameter names
    if (par_bio_remin_sinkingrate_physical < const_real_nullsmall) then
       par_bio_remin_sinkingrate_physical = par_bio_remin_sinkingrate
       par_bio_remin_sinkingrate_reaction = par_bio_remin_sinkingrate_scav
    end if
    ! -------------------------------------------------------- !
    ! adjust units
    ! -------------------------------------------------------- !
    ! adjust units of nutrient update time-scale from days to years
    par_bio_tau     = conv_d_yr*par_bio_tau
    par_bio_tau_sp  = conv_d_yr*par_bio_tau_sp
    par_bio_tau_nsp = conv_d_yr*par_bio_tau_nsp
    ! adjust units of scavening rate constant (d-1 -> yr-1)
    par_scav_Fe_ks = par_scav_Fe_ks/conv_d_yr
    ! adjust units of prescribed particulates sinking rate (m d-1 -> m yr-1)
    par_bio_remin_sinkingrate_physical = par_bio_remin_sinkingrate_physical/conv_d_yr
    par_bio_remin_sinkingrate_reaction = par_bio_remin_sinkingrate_reaction/conv_d_yr
    ! adjust units of CH4 oxidation (d-1 -> yr-1)
    par_bio_remin_CH4rate = par_bio_remin_CH4rate/conv_d_yr
    ! opal dissolution
    par_bio_remin_opal_K = par_bio_remin_opal_K/conv_d_yr ! opal particulate base dissolution rate (d-1 -> yr-1) [Ridgwell, 2001]
    ! ballast coefficients (g POC m-2 yr-1 (g ballast m-2 yr-1)-1 -> mol POC m-2 yr-1 (mol ballast m-2 yr-1)-1)
    par_bio_remin_ballast_kc = (conv_POC_cm3_mol*conv_POC_g_cm3/(conv_cal_cm3_mol*conv_cal_g_cm3))*par_bio_remin_ballast_kc
    par_bio_remin_ballast_ko = (conv_POC_cm3_mol*conv_POC_g_cm3/(conv_opal_cm3_mol*conv_opal_g_cm3))*par_bio_remin_ballast_ko
    par_bio_remin_ballast_kl = (conv_POC_cm3_mol*conv_POC_g_cm3/(conv_det_cm3_mol*conv_det_g_cm3))*par_bio_remin_ballast_kl
    ! -------------------------------------------------------- !
    ! MISC
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! complete path
    par_fordir_name = trim(par_fordir_name)//trim(par_forcing_name)//'/'
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_load_goin_biogem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD BioGeM RESTART DATA
  SUBROUTINE sub_data_load_rst()
    USE biogem_lib
    use gem_netcdf
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,is,iv                                        ! local counting variables
    integer::ios                                               !
    integer::loc_ncid                                          !
    CHARACTER(len=255)::loc_filename                           ! filename string
    integer::loc_n_l_ocn,loc_n_l_sed                           ! number of selected tracers in the re-start file
    integer,DIMENSION(n_ocn)::loc_conv_iselected_io            ! number of selected ocean tracers in restart
    integer,DIMENSION(n_sed)::loc_conv_iselected_is            !
    real,dimension(n_i,n_j,n_k)::loc_ocn,loc_part              !
    integer::loc_ndims,loc_nvars
    integer,ALLOCATABLE,dimension(:)::loc_dimlen
    integer,ALLOCATABLE,dimension(:,:)::loc_varlen
    integer,ALLOCATABLE,dimension(:)::loc_vdims
    character(20),ALLOCATABLE,dimension(:)::loc_varname
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! set filename
    IF (ctrl_ncrst) THEN
       loc_filename = TRIM(par_rstdir_name)//par_ncrst_name
    else
       loc_filename = TRIM(par_rstdir_name)//trim(par_infile_name)
    endif
    ! -------------------------------------------------------- ! check file status
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
    close(unit=in)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'biogem_data','sub_data_load_restart', &
            & 'You have requested a CONTINUING run, but restart file <'//trim(loc_filename)//'> does not exist', &
            & 'SKIPPING - using default initial values', &
            & (/const_real_null/),.false. &
            & )
    else
       ! -------------------------------------------------------- !
       ! LOAD RESTART
       ! -------------------------------------------------------- !
       IF (ctrl_ncrst) THEN
          call sub_openfile(loc_filename,loc_ncid)
          ! -------------------------------------------------------- ! determine number of variables
          call sub_inqdims (loc_filename,loc_ncid,loc_ndims,loc_nvars)
          ! -------------------------------------------------------- ! allocate arrays
          ALLOCATE(loc_dimlen(loc_ndims),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varlen(2,loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_vdims(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varname(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! get variable names
          call sub_inqvars(loc_ncid,loc_ndims,loc_nvars,loc_dimlen,loc_varname,loc_vdims,loc_varlen)
          ! -------------------------------------------------------- ! load and apply only tracers that are selected
          IF (ctrl_debug_init == 1) print*,' * Loading ocean restart fields (dissolved tracers): '
          DO iv=1,loc_nvars
             DO l=1,n_l_ocn
                io = conv_iselected_io(l)
                if ('ocn_'//trim(string_ocn(io)) == trim(loc_varname(iv))) then
                   IF (ctrl_debug_init == 1) print*,'   ',trim(loc_varname(iv))
                   loc_ocn = 0.0
                   call sub_getvarijk(loc_ncid,'ocn_'//trim(string_ocn(io)),n_i,n_j,n_k,loc_ocn(:,:,:))
                   ocn(io,:,:,:) = loc_ocn(:,:,:)
                endif
             end do
          end DO
          IF (ctrl_debug_init == 1) print*,' * Loading ocean restart fields (particulate tracers): '
          DO iv=1,loc_nvars
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                if ('bio_part_'//trim(string_sed(is)) == trim(loc_varname(iv))) then
                   IF (ctrl_debug_init == 1) print*,'   ',trim(loc_varname(iv))
                   loc_part = 0.0
                   call sub_getvarijk(loc_ncid,'bio_part_'//trim(string_sed(is)),n_i,n_j,n_k,loc_part(:,:,:))
                   bio_part(is,:,:,:) = loc_part(:,:,:)
                endif
             end do
          end DO
          ! -------------------------------------------------------- ! deallocate arrays
          deALLOCATE(loc_dimlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_vdims,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varname,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! close file
          call sub_closefile(loc_ncid)
       else
          OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
          read(unit=in,iostat=ios)                                          &
               & loc_n_l_ocn,                                               &
               & (loc_conv_iselected_io(l),l=1,loc_n_l_ocn),                &
               & (ocn(loc_conv_iselected_io(l),:,:,:),l=1,loc_n_l_ocn),     &
               & loc_n_l_sed,                                               &
               & (loc_conv_iselected_is(l),l=1,loc_n_l_sed),                &
               & (bio_part(loc_conv_iselected_is(l),:,:,:),l=1,loc_n_l_sed)
          call check_iostat(ios,__LINE__,__FILE__)
          close(unit=in,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       endif
       ! -------------------------------------------------------- ! adjust restart data
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          if (ctrl_ocn_dinit) then
             ocn(io,:,:,:) = ocn(io,:,:,:) + ocn_dinit(io)*phys_ocn(ipo_mask_ocn,:,:,:)
          else
             ocn(io,:,:,:) = (1.0 + ocn_dinit(io))*ocn(io,:,:,:)
          end if
       end do
       if (ctrl_ocn_rst_reset_T) then
          ocn(io_T,:,:,:) = ocn_init(io_T)
       end if
       if (ctrl_force_ocn_age) then
          ocn(io_colb,:,:,:) = ocn(io_colb,:,:,:) + par_misc_t_runtime*ocn(io_colr,:,:,:)
       end if
    end If
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_data_load_rst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DATA INITIALIZATION ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'BIOLOGICAL' PARAMETERS AND VARIABLES
  SUBROUTINE sub_init_bio()
    ! local variables
    integer::i,j              !malod: for flexible stoichiometry
    CHARACTER(len=255)::loc_filename

    ! *** initialize global arrays ***
    bio_part(:,:,:,:)     = 0.0
    bio_remin(:,:,:,:)    = 0.0
    bio_settle(:,:,:,:)   = 0.0
    bio_part_red(:,:,:,:) = 0.0

    ! *** set default 'Redfield' ratios ***
    ! trivial self-relationships(!)
    bio_part_red(is_POP,is_POP,:,:)     = 1.0
    bio_part_red(is_POC,is_POC,:,:)     = 1.0
    bio_part_red(is_PON,is_PON,:,:)     = 1.0
    bio_part_red(is_CaCO3,is_CaCO3,:,:) = 1.0
    bio_part_red(is_opal,is_opal,:,:)   = 1.0
    bio_part_red(is_POCd,is_POCd,:,:)   = 1.0
    bio_part_red(is_POFe,is_POFe,:,:)   = 1.0
    ! set values and derived values
    ! NOTE: relate everything to carbon units where it is not already
    ! NOTE2: Variable P:C ratio coded by malod
    IF (abs(par_bio_red_POP_POC) > const_real_nullsmall) then
       ! par_bio_red_PC_flex = 1 --> activate variable stoichiometry
       ! par_bio_red_PC_flex = 2 --> activate variable stoichiometry with limit at high PO4
       ! par_bio_red_PC_flex = 0 --> default fixed Redfield stoichiometry
       if (par_bio_red_PC_flex > 0) then
          ! default par_bio_red_PC_alpha1 = 1.0
          ! default par_bio_red_PC_alpha2 = 1.0
          ! to achieve average P:C more similar to fixed Redfield modern run, use  PC_alpha1 = 1.1643
          ! an alternative is to scale only PC_alpha2 = 1.16
          DO i = 1,n_i
             do j = 1,n_j
                bio_part_red(is_POC,is_POP,i,j) = par_bio_red_PC_alpha1 * (6.9e-3 * ocn(io_PO4,i,j,n_k)*1.0e6 + par_bio_red_PC_alpha2*6.0e-3)
                if (par_bio_red_PC_flex > 1) then  ! limit C:P for high PO4 (no data for PO4 > 1.7 uM in Galbraith & Martiny, 2015)
                   if (bio_part_red(is_POC,is_POP,i,j) > par_bio_red_PC_max) then
                      bio_part_red(is_POC,is_POP,i,j) = par_bio_red_PC_max
                   end if
                end if
                bio_part_red(is_POP,is_POC,i,j) = 1.0/bio_part_red(is_POC,is_POP,i,j)
             end do
          end DO
       elseif (par_bio_red_PC_flex == -1) then
          bio_part_red(is_POC,is_POP,:,:) = par_bio_red_PC_max
          bio_part_red(is_POP,is_POC,:,:) = 1.0/bio_part_red(is_POC,is_POP,:,:)
       else
          bio_part_red(is_POP,is_POC,:,:) = par_bio_red_POP_POC
          bio_part_red(is_POC,is_POP,:,:) = 1.0/bio_part_red(is_POP,is_POC,:,:)
       end if
    end if
    IF (abs(par_bio_red_POP_PON) > const_real_nullsmall) then
       bio_part_red(is_POP,is_PON,:,:) = par_bio_red_POP_PON
       bio_part_red(is_POC,is_PON,:,:) = bio_part_red(is_POC,is_POP,:,:)*bio_part_red(is_POP,is_PON,:,:)
       bio_part_red(is_PON,is_POC,:,:) = 1.0/bio_part_red(is_POC,is_PON,:,:)
    end if
    if (abs(par_bio_red_POC_CaCO3) > const_real_nullsmall) then
       bio_part_red(is_POC,is_CaCO3,:,:) = par_bio_red_POC_CaCO3
       bio_part_red(is_CaCO3,is_POC,:,:) = 1.0/bio_part_red(is_POC,is_CaCO3,:,:)
    end if
    if (abs(par_bio_red_POC_opal) > const_real_nullsmall) then
       bio_part_red(is_POC,is_opal,:,:) = par_bio_red_POC_opal
       bio_part_red(is_opal,is_POC,:,:) = 1.0/bio_part_red(is_POC,is_opal,:,:)
    end if
    IF (abs(par_bio_red_POC_POCd) > const_real_nullsmall) then
       bio_part_red(is_POC,is_POCd,:,:) = par_bio_red_POC_POCd
       bio_part_red(is_POCd,is_POC,:,:) = 1.0/bio_part_red(is_POC,is_POCd,:,:)
    end if
    IF (abs(par_bio_red_POFe_POC) > const_real_nullsmall) then
       bio_part_red(is_POFe,is_POC,:,:) = par_bio_red_POFe_POC
       bio_part_red(is_POC,is_POFe,:,:) = 1.0/bio_part_red(is_POFe,is_POC,:,:)
    end if
    ! denifrification and sulphate reduction
    if (par_bio_red_POP_PO2 == -138.0 ) then
       par_bio_red_O2_H2SO4 = 53.0/(-par_bio_red_POP_PO2)
       par_bio_red_O2_NO3 = 84.8/(-par_bio_red_POP_PO2)
    elseif (par_bio_red_POP_PO2 == -150.0 ) then
       par_bio_red_O2_H2SO4 = 59.0/(-par_bio_red_POP_PO2)
       par_bio_red_O2_NO3 = 104.0/(-par_bio_red_POP_PO2)
    else
       par_bio_red_O2_H2SO4 = 0.0
       par_bio_red_O2_NO3 = 0.0
    end if

    ! *** load prescribed CaCO3:POC field (if requested) ***
    if (ctrl_force_CaCO3toPOCrainratio) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_CaCO3toPOCrainratio_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_CaCO3toPOCrainratio(:,:))
    end if

    ! *** load prescribed POCd:POC field (if requested) ***
    if (ctrl_force_POCdtoPOCrainratio) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_POCdtoPOCrainratio_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_POCdtoPOCrainratio(:,:))
    end if

    ! *** load prescribed [Cd/P]POM/[Cd/P]SW partition coefficient field (if requested) ***
    if (ctrl_force_Cd_alpha) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_Cd_alpha_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_Cd_alpha(:,:))
    end if

    ! *** load prescribed CaCO3 ballasting field (if requested) ***
    if (ctrl_force_CaCO3ballastcoeff) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_CaCO3ballastcoeff_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_remin_kc(:,:))
    else
       par_bio_remin_kc(:,:) = par_bio_remin_ballast_kc
    end if

    ! *** load prescribed opal ballasting field (if requested) ***
    if (ctrl_force_opalballastcoeff) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_opalballastcoeff_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_remin_ko(:,:))
    else
       par_bio_remin_ko(:,:) = par_bio_remin_ballast_ko
    end if

    ! *** load prescribed detrital ballasting field (if requested) ***
    if (ctrl_force_detballastcoeff) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_detballastcoeff_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_remin_kl(:,:))
    else
       par_bio_remin_kl(:,:) = par_bio_remin_ballast_kl
    end if

    ! *** load prescribed POC scavenging coefficient field (if requested) ***
    if (ctrl_force_scav_fpart_POC) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_scav_fpart_POC_file)
       CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,par_scav_fpart_POC(:,:,:))
    end if

    ! *** load prescribed CaCO3 scavenging coefficient field (if requested) ***
    if (ctrl_force_scav_fpart_CaCO3) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_scav_fpart_CaCO3_file)
       CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,par_scav_fpart_CaCO3(:,:,:))
    end if

    ! *** load prescribed opal scavenging coefficient field (if requested) ***
    if (ctrl_force_scav_fpart_opal) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_scav_fpart_opal_file)
       CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,par_scav_fpart_opal(:,:,:))
    end if

    ! *** load prescribed det scavenging coefficient field (if requested) ***
    if (ctrl_force_scav_fpart_det) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_scav_fpart_det_file)
       CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,par_scav_fpart_det(:,:,:))
    end if

  END SUBROUTINE sub_init_bio
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  SUBROUTINE sub_init_force_Fgeothermal()
    USE biogem_lib
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    CHARACTER(len=255)::loc_filename                           ! filename string
    ! -------------------------------------------------------- !
    ! LOAD 2D FIELD
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! load geothermal 2D field
    loc_filename = TRIM(par_indir_name)//TRIM(par_force_Fgeothermal2D_file)
    CALL sub_load_data_ij(loc_filename,n_i,n_j,force_Fgeothermal2D(:,:))
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_init_force_Fgeothermal
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  SUBROUTINE sub_init_misc2D()
    USE biogem_lib
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    CHARACTER(len=255)::loc_filename                           ! filename string
    ! -------------------------------------------------------- !
    ! LOAD 2D FIELD
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! load miscellaneous 2D field
    loc_filename = TRIM(par_indir_name)//TRIM(par_misc_2D_file)
    CALL sub_load_data_ij(loc_filename,n_i,n_j,par_misc_2D(:,:))
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_init_misc2D
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  SUBROUTINE sub_init_redox()
    USE biogem_lib
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::n
    CHARACTER(len=31),DIMENSION(:),ALLOCATABLE::loc_string     !
    integer::lo,ls
    integer::loc_m,loc_tot_m
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    n = 0
    allocate(loc_string(100),STAT=alloc_error)
    ! -------------------------------------------------------- !
    ! COUNT POTENTIAL REDOX REACTIONS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! (1) dissolved phase
    if (ocn_select(io_H2S)) then
       if (ocn_select(io_O2)) then
          n = n+1
          loc_string(n) = 'H2StoSO4_dH2S'
          n = n+1
          loc_string(n) = 'H2StoSO4_dSO4'
          n = n+1
          loc_string(n) = 'H2StoSO4_dO2'
          n = n+1
          loc_string(n) = 'H2StoSO4_dALK'
       end if
    end if
    if (ocn_select(io_NH4)) then
       if (ocn_select(io_O2)) then
          n = n+1
          loc_string(n) = 'NH4toNO3_dNH4'
          n = n+1
          loc_string(n) = 'NH4toNO3_dNO3'
          n = n+1
          loc_string(n) = 'NH4toNO3_dO2'
          n = n+1
          loc_string(n) = 'NH4toNO3_dALK'
       end if
    end if
    if (ocn_select(io_CH4)) then
       if (ocn_select(io_O2)) then
          n = n+1
          loc_string(n) = 'CH4toDIC_dCH4'
          n = n+1
          loc_string(n) = 'CH4toDIC_dCO2'
          n = n+1
          loc_string(n) = 'CH4toDIC_dO2'
          n = n+1
          loc_string(n) = 'CH4toDIC_dALK'
       end if
       if (ocn_select(io_SO4)) then
          n = n+1
          loc_string(n) = 'CH4toDICaom_dCH4'
          n = n+1
          loc_string(n) = 'CH4toDICaom_dDIC'
          n = n+1
          loc_string(n) = 'CH4toDICaom_dH2S'
          n = n+1
          loc_string(n) = 'CH4toDICaom_dSO4'
          n = n+1
          loc_string(n) = 'CH4toDICaom_dALK'
       end if
    end if
    if (ocn_select(io_I) .AND. ocn_select(io_I)) then
       if (ocn_select(io_O2)) then
          n = n+1
          loc_string(n) = 'ItoIO3_dI'
          n = n+1
          loc_string(n) = 'ItoIO3_dIO3'
          n = n+1
          loc_string(n) = 'ItoIO3_dO2'
          n = n+1
          loc_string(n) = 'IO3toI_dI'
          n = n+1
          loc_string(n) = 'IO3toI_dIO3'
          n = n+1
          loc_string(n) = 'IO3toI_dO2'
       end if
    end if
    ! -------------------------------------------------------- ! (2) solid -> dissolved
    !                                                                NOTE: repeat loop to add dissolved redox transformations
    !                                                                      (as if a 2nd set of particulates)
    if (ctrl_bio_remin_redox_save) then
       DO ls=1,n_l_sed
          loc_tot_m = conv_ls_lo_i(0,ls)
          do loc_m=1,loc_tot_m
             lo = conv_ls_lo_i(loc_m,ls)
             if (lo > 0) then
                n = n+1
                loc_string(n) = 'reminP_'//trim(string_sed(l2is(ls)))//'_d'//trim(string_ocn(l2io(lo)))
             end if
          end do
       end DO
       DO ls=1,n_l_sed
          loc_tot_m = conv_ls_lo_i(0,ls)
          do loc_m=1,loc_tot_m
             lo = conv_ls_lo_i(loc_m,ls)
             if (lo > 0) then
                n = n+1
                loc_string(n) = 'reminD_'//trim(string_sed(l2is(ls)))//'_d'//trim(string_ocn(l2io(lo)))
             end if
          end do
       end DO
    end if
    ! -------------------------------------------------------- ! record total
    n_diag_redox = n
    ! -------------------------------------------------------- !
    ! ALLOCATE ARRAYS
    ! -------------------------------------------------------- !
    allocate(string_diag_redox(n_diag_redox),STAT=alloc_error)
    allocate(diag_redox(n_diag_redox,n_i,n_j,n_k),STAT=alloc_error)
    allocate(int_diag_redox_timeslice(n_diag_redox,n_i,n_j,n_k),STAT=alloc_error)
    allocate(int_diag_redox_sig(n_diag_redox),STAT=alloc_error)
    ! -------------------------------------------------------- !
    ! INITIALIZE ARRAYS
    ! -------------------------------------------------------- !
    DO n=1,n_diag_redox
       string_diag_redox(n) = loc_string(n)
    end do
    diag_redox(:,:,:,:)               = 0.0
    int_diag_redox_timeslice(:,:,:,:) = 0.0
    int_diag_redox_sig(:)             = 0.0
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
    DEALLOCATE(loc_string,STAT=alloc_error)
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_init_redox
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! UPDATE RELATIONSHIPS BETWEEN TRACERS
  ! NOTE: the reverse transformation array <conv_ocn_sed> was never used and hence is no longer updated here
  ! NOTE: update the basic oxic transformation (<conv_sed_ocn>) first:
  !       this is used to create particulate matter (e.g. biological uptake) as well as in the tracer auditing calculations
  SUBROUTINE sub_data_update_tracerrelationships()
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real,dimension(1:n_ocn,1:n_sed)::loc_conv_sed_ocn          !
    real::loc_alpha
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_conv_sed_ocn(:,:) = 0.0
    ! -------------------------------------------------------- !
    ! UPDATE REDFIELD RELATIONSHIPS
    ! -------------------------------------------------------- !
    ! N (in POM)
    ! NOTE: the default assumption is that the assimilation (and uptake) of N to form PON, is as NO3 (and not N2 or NH4)
    if (ocn_select(io_NO3)) then
       conv_sed_ocn(io_NO3,is_PON) = 1.0
       conv_sed_ocn(io_NH4,is_PON) = 0.0
       conv_sed_ocn(io_N2,is_PON)  = 0.0
    else
       conv_sed_ocn(io_NO3,is_PON) = 0.0
       conv_sed_ocn(io_NH4,is_PON) = 0.0
       conv_sed_ocn(io_N2,is_PON)  = 0.0
    end if
    ! ALK
    ! if NO3 is employed: calculate alkalnity corrections associated with the formation and destruction of organic matter from NO3
    ! otherwise: convert PO4 units to NO3 via the P:N Redfield ratio and then calculate the ALK correction from NO3
    ! NOTE: ensure that both corrections are mutually exclusive (i.e., make sure that there can be no double ALK correction)
    ! NOTE: catch any incidence of Redfield ratios (par_bio_red_xxx) set to 0.0
    if (ocn_select(io_NO3)) then
       conv_sed_ocn(io_ALK,is_PON) = par_bio_red_PON_ALK
       conv_sed_ocn(io_ALK,is_POP) = 0.0
       conv_sed_ocn(io_ALK,is_POC) = 0.0
    else
       conv_sed_ocn(io_ALK,is_PON) = 0.0
       if (abs(par_bio_red_POP_POC) > const_real_nullsmall) then
          if (ctrl_bio_red_ALKwithPOC) then
             conv_sed_ocn(io_ALK,is_POC) = (1.0/par_bio_red_POP_POC)*par_bio_red_POP_PON*par_bio_red_PON_ALK
             conv_sed_ocn(io_ALK,is_POP) = 0.0
          else
             conv_sed_ocn(io_ALK,is_POC) = 0.0
             conv_sed_ocn(io_ALK,is_POP) = par_bio_red_POP_PON*par_bio_red_PON_ALK
          end if
       else
          conv_sed_ocn(io_ALK,is_POC) = 0.0
          conv_sed_ocn(io_ALK,is_POP) = 0.0
       end if
    end if
    ! O2 (of P, N, C)
    ! update O2 demand associated with organic matter (taken as the carbon component)
    ! reduce O2 demand associated with C (and H) oxidation => treat N and P explicitly
    ! NOTE: set no PON O2 demand if NO3 tracer not selected (and increase POC O2 demand)
    ! NOTE: NO3 uptake assumed as: 2H+ + 2NO3- -> 2PON + (5/2)O2 + H2O
    !       (and as implemented, per mol N, this ends up as (5/2)/2 = 5.0/4.0
    if (ocn_select(io_NO3)) then
       conv_sed_ocn(io_O2,is_PON) = -(5.0/4.0)
    else
       conv_sed_ocn(io_O2,is_PON) = 0.0
    end if
    if (ctrl_bio_red_O2withPOC) then
       conv_sed_ocn(io_O2,is_POP) = 0.0
       conv_sed_ocn(io_O2,is_PON) = 0.0
    else
       conv_sed_ocn(io_O2,is_POP) = -4.0/2.0
    end if
    if (abs(par_bio_red_POP_POC*par_bio_red_POP_PO2) > const_real_nullsmall) then
       conv_sed_ocn(io_O2,is_POC) = par_bio_red_POP_PO2/par_bio_red_POP_POC - &
            & conv_sed_ocn(io_O2,is_POP)/par_bio_red_POP_POC - &
            & conv_sed_ocn(io_O2,is_PON)*par_bio_red_POP_PON/par_bio_red_POP_POC
    else
       conv_sed_ocn(io_O2,is_POP) = 0.0
       conv_sed_ocn(io_O2,is_PON) = 0.0
       conv_sed_ocn(io_O2,is_POC) = 0.0
    end if
    ! -------------------------------------------------------- !
    ! UPDATE ALT REDOX SED->OCN RELATIONSHIPS
    ! -------------------------------------------------------- !
    ! NOTE: arrays are only one-way (i.e. there is no equivalent ocn --> sed transformation)
    ! NOTE: remember that conv_sed_ocn(io_O2,is_POC) is *negative*
    ! -------------------------------------------------------- ! Modify for oxic conditions(!)
    ! NOTE: the only modifications needed relate to the remin of N in POM
    ! NOTE: NO3 uptake assumed as: 2H+ + 2NO3- -> 2PON + (5/2)O2 + H2O
    !       (and as implemented, per mol N, this ends up as (5/2)/2 = 5.0/4.0
    ! NOTE: to balance the uptake of NO3 into organic matter
    !       [2H+ + 2NO3- -> 2PON + (5/2)O2 + H2O]
    !       with the release of N as ammonium and subsequent oxidation to NO3 ...
    !       [NH4+ + 2O2 -> NO3- + 2H+ + H2O]
    !       the remin of PON needs to be adjusted in order that everything is conserved:
    !       2PON + 3H2O + 2H+ --> 2NH4+ + (3/2)O2
    !       and per N:
    !       PON + (3/2)H2O + H+ --> NH4+ + (3/4)O2
    if (ocn_select(io_O2)) then
       conv_sed_ocn_O(:,:)  = conv_sed_ocn(:,:)
       ! N
       if (ocn_select(io_NH4)) then
          conv_sed_ocn_O(io_NO3,is_PON) = 0.0
          conv_sed_ocn_O(io_NH4,is_PON) = 1.0
          conv_sed_ocn_O(io_N2,is_PON)  = 0.0
       end if
       ! ALK
       if (ocn_select(io_NH4)) then
          conv_sed_ocn_O(io_ALK,is_PON) = conv_sed_ocn_O(io_NH4,is_PON)
       end if
       ! O2 (of P, N, C)
       if (ocn_select(io_NH4)) then
          conv_sed_ocn_O(io_O2,is_PON) = (3.0/4.0)
       end if
    end if
    ! -------------------------------------------------------- ! Modify for N-reducing conditions
    ! NOTE: to balance the uptake of NO3 into organic matter
    !       [2H+ + 2NO3- -> 2PON + (5/2)O2 + H2O]
    !       with the release of N as ammonium and subsequent oxidation to NO3 ...
    !       [NH4+ + 2O2 -> NO3- + 2H+ + H2O]
    !       the remin of PON needs to be adjusted in order that everything is conserved:
    !       2PON + 3H2O + 2H+ --> 2NH4+ + (3/2)O2
    !       and per N:
    !       PON + (3/2)H2O + H+ --> NH4+ + (3/4)O2
    ! NOTE: oxidation equivalence assumption (NH4): NO3- + H2O + 2H+ <-> 2O2 + NH4+
    !                                           or: O2 == (1/2)NO3- + (1/2)H2O + H+ - (1/2)NH4+
    !       e.g. P + NO3- + H2O + 2H+ -> PO4 + NH4+ (ignoring charges associated with P)
    ! NOTE: oxidation equivalence assumption (N2):  2NO3- + 2H+ <-> (5/2)O2 + N2 + H2O
    !                                           or: O2 == (4/5)NO3- + (4/5)H+ - (2/5)N2 - (2/5)H2O
    !       e.g. P + (8/5)NO3- + (8/5)H+ -> PO4 + (4/5)N2 + (4/5)H2O
    ! NOTE: assumption for NO2: 2NO3- <-> O2 + 2NO2-
    !                       or: O2 == 2NO3- - 2NO2-
    if (ocn_select(io_NO3)) then
       conv_sed_ocn_N(:,:)  = conv_sed_ocn(:,:)
       ! N
       if (ocn_select(io_NH4)) then
          conv_sed_ocn_N(io_NO3,is_PON) = 0.0
          conv_sed_ocn_N(io_NH4,is_PON) = 1.0
          conv_sed_ocn_N(io_ALK,is_PON) = conv_sed_ocn_N(io_NH4,is_PON)
          conv_sed_ocn_N(io_O2,is_PON)  = (3.0/4.0)
       elseif (ocn_select(io_N2)) then
          conv_sed_ocn_N(io_NO3,is_PON) = 0.0
          conv_sed_ocn_N(io_N2,is_PON)  = 0.5
          conv_sed_ocn_N(io_ALK,is_PON) = 0.0
          conv_sed_ocn_N(io_O2,is_PON)  = 0.0
       else
          ! [DEFAULT, oxic remin relationship]
       endif
       ! P,C
       if (ocn_select(io_NO2)) then
          conv_sed_ocn_N(io_NO3,is_POP) = -4.0
          conv_sed_ocn_N(io_NO2,is_POP) = 4.0
          conv_sed_ocn_N(io_ALK,is_POP) = 0.0
          conv_sed_ocn_N(io_O2,is_POP)  = 0.0
          conv_sed_ocn_N(io_NO3,is_POC) = 2.0*conv_sed_ocn(io_O2,is_POC)
          conv_sed_ocn_N(io_NO2,is_POC) = -2.0*conv_sed_ocn(io_O2,is_POC)
          conv_sed_ocn_N(io_ALK,is_POC) = 0.0
          conv_sed_ocn_N(io_O2,is_POC)  = 0.0
       elseif (ocn_select(io_N2)) then
          conv_sed_ocn_N(io_NO3,is_POP) = -(8.0/5.0)
          conv_sed_ocn_N(io_N2,is_POP)  = -0.5*conv_sed_ocn_N(io_NO3,is_POP)
          conv_sed_ocn_N(io_ALK,is_POP) = -conv_sed_ocn_N(io_NO3,is_POP)
          conv_sed_ocn_N(io_O2,is_POP)  = 0.0
          conv_sed_ocn_N(io_NO3,is_POC) = (4.0/5.0)*conv_sed_ocn(io_O2,is_POC)
          conv_sed_ocn_N(io_N2,is_POC)  = -0.5*conv_sed_ocn_N(io_NO3,is_POC)
          conv_sed_ocn_N(io_ALK,is_POC) = -conv_sed_ocn_N(io_NO3,is_POC)
          conv_sed_ocn_N(io_O2,is_POC)  = 0.0
       else
          ! [DEFAULT, oxic remin relationship]
       endif
    else
       conv_sed_ocn_N(:,:) = 0.0
    end if
    ! -------------------------------------------------------- ! Modify for S-reducing conditions
    if (ocn_select(io_SO4)) then
       conv_sed_ocn_S(:,:) = conv_sed_ocn(:,:)
       ! N
       if (ocn_select(io_NH4)) then
          conv_sed_ocn_S(io_NO3,is_PON) = 0.0
          conv_sed_ocn_S(io_NH4,is_PON) = 1.0
          conv_sed_ocn_S(io_ALK,is_PON) = conv_sed_ocn_N(io_NH4,is_PON)
          conv_sed_ocn_S(io_O2,is_PON)  = 0.0
       elseif (ocn_select(io_N2)) then
          conv_sed_ocn_S(io_NO3,is_PON) = 0.0
          conv_sed_ocn_S(io_N2,is_PON)  = 0.5
          conv_sed_ocn_S(io_ALK,is_PON) = 0.0
          conv_sed_ocn_S(io_O2,is_PON)  = 0.0
       else
          ! [DEFAULT, oxic remin relationship]
       endif
       ! P,C
       conv_sed_ocn_S(io_SO4,is_POP) = 0.5*conv_sed_ocn_S(io_O2,is_POP)
       conv_sed_ocn_S(io_H2S,is_POP) = -0.5*conv_sed_ocn_S(io_O2,is_POP)
       conv_sed_ocn_S(io_O2,is_POP)  = 0.0
       conv_sed_ocn_S(io_SO4,is_POC) = 0.5*conv_sed_ocn(io_O2,is_POC)
       conv_sed_ocn_S(io_H2S,is_POC) = -0.5*conv_sed_ocn(io_O2,is_POC)
       conv_sed_ocn_S(io_O2,is_POC)  = 0.0
       if (ctrl_bio_red_ALKwithPOC) then
          conv_sed_ocn_S(io_ALK,is_POP) = -2.0*conv_sed_ocn_S(io_SO4,is_POP)
          conv_sed_ocn_S(io_ALK,is_POC) = -2.0*conv_sed_ocn_S(io_SO4,is_POC) + conv_sed_ocn(io_ALK,is_POC)
       else
          conv_sed_ocn_S(io_ALK,is_POP) = -2.0*conv_sed_ocn_S(io_SO4,is_POP) + conv_sed_ocn(io_ALK,is_POP)
          conv_sed_ocn_S(io_ALK,is_POC) = -2.0*conv_sed_ocn_S(io_SO4,is_POC)
       end if
    else
       conv_sed_ocn_S(:,:) = 0.0
    end if
    ! -------------------------------------------------------- ! Modify for methanogenesis
    ! NOTE: methanogenesis is acetoclastic - CH2O -> 0.5CO2 + 0.5CH4
    ! NOTE: O2 released by the creation of organic matter:
    !       conv_sed_ocn(io_O2,is_POC) + conv_sed_ocn(io_O2,is_POP) + conv_sed_ocn(io_O2,is_PON)
    !       == par_bio_red_POP_PO2/par_bio_red_POP_POC
    !       => this has to be balanced by O2 consumded by CH4 oxidation
    ! NOTE: in the alternative N transformations -- *no* O2 is involved
    ! NOTE: assume that in POP --> PO4, the O2 comes from 'elsewhere' (O in organic matter, or H2O)
    !       (and don't explicitly account for O2 changing hands)
    if (ocn_select(io_CH4)) then
       conv_sed_ocn_meth(:,:) = conv_sed_ocn(:,:)
       loc_alpha = 1.0 + par_d13C_Corg_CH4_epsilon/1000.0
       ! N
       if (ocn_select(io_NH4)) then
          conv_sed_ocn_meth(io_NO3,is_PON) = 0.0
          conv_sed_ocn_meth(io_NH4,is_PON) = 1.0
          conv_sed_ocn_meth(io_ALK,is_PON) = conv_sed_ocn_N(io_NH4,is_PON)
          conv_sed_ocn_meth(io_O2,is_PON)  = 0.0
       elseif (ocn_select(io_N2)) then
          conv_sed_ocn_meth(io_NO3,is_PON) = 0.0
          conv_sed_ocn_meth(io_N2,is_PON)  = 0.5
          conv_sed_ocn_meth(io_ALK,is_PON) = 0.0
          conv_sed_ocn_meth(io_O2,is_PON)  = 0.0
       else
          ! [DEFAULT, oxic remin relationship]
       endif
       ! P,C
       conv_sed_ocn_meth(io_O2,is_POP)  = 0.0
       conv_sed_ocn_meth(io_CH4,is_POC) = -0.5*par_bio_red_POP_PO2/par_bio_red_POP_POC
       conv_sed_ocn_meth(io_DIC,is_POC) = 1.0 - conv_sed_ocn_meth(io_CH4,is_POC)
       conv_sed_ocn_meth(io_O2,is_POC)  = 0.0
       conv_sed_ocn_meth(io_CH4_13C,is_POC_13C) = loc_alpha*conv_sed_ocn_meth(io_CH4,is_POC)
       conv_sed_ocn_meth(io_DIC_13C,is_POC_13C) = 1.0 - conv_sed_ocn_meth(io_CH4_13C,is_POC_13C)
    else
       conv_sed_ocn_meth(:,:) = 0.0
    end if
    ! -------------------------------------------------------- ! Set local remin array reflecting 'mix' of redox possibilities
    ! NOTE: this is the 'redox tree' of all enabled posibilities
    !       (possibilities of not having O2 but having a different oxidant are omitted, as are O2 + Fe without SO4)
    if (ocn_select(io_O2))  loc_conv_sed_ocn(:,:) = loc_conv_sed_ocn(:,:) + abs(conv_sed_ocn_O)
    if (ocn_select(io_NO3)) loc_conv_sed_ocn(:,:) = loc_conv_sed_ocn(:,:) + abs(conv_sed_ocn_N)
    if (ocn_select(io_SO4)) loc_conv_sed_ocn(:,:) = loc_conv_sed_ocn(:,:) + abs(conv_sed_ocn_S)
    if (ocn_select(io_CH4)) loc_conv_sed_ocn(:,:) = loc_conv_sed_ocn(:,:) + abs(conv_sed_ocn_meth)
    ! -------------------------------------------------------- !  indexing array (basic oxic-only)
    conv_sed_ocn_i(:,:) = fun_recalc_tracerrelationships_i(conv_sed_ocn(:,:))
    ! -------------------------------------------------------- !
    ! CREATE COMPACT TRACER INDEX FORMAT ARRAY EQUIVALENTS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! sed -> ocn
    if (ocn_select(io_O2))  conv_ls_lo(:,:)      =  fun_conv_sedocn2lslo(conv_sed_ocn(:,:))
    if (ocn_select(io_O2))  conv_ls_lo_O(:,:)    =  fun_conv_sedocn2lslo(conv_sed_ocn_O(:,:))
    if (ocn_select(io_NO3)) conv_ls_lo_N(:,:)    =  fun_conv_sedocn2lslo(conv_sed_ocn_N(:,:))
    if (ocn_select(io_SO4)) conv_ls_lo_S(:,:)    =  fun_conv_sedocn2lslo(conv_sed_ocn_S(:,:))
    if (ocn_select(io_CH4)) conv_ls_lo_meth(:,:) =  fun_conv_sedocn2lslo(conv_sed_ocn_meth(:,:))
    ! -------------------------------------------------------- !  indexing array (all possible)
    conv_ls_lo_i(:,:) =  fun_conv_sedocn2lslo_i(fun_recalc_tracerrelationships_i(loc_conv_sed_ocn(:,:)))
    ! -------------------------------------------------------- ! POM -> DOM
    conv_lP_lD(:,:)   =  fun_conv_sedocn2lslo(conv_POM_DOM(:,:))
    conv_lP_lD_i(:,:) =  fun_conv_sedocn2lslo_i(conv_POM_DOM_i(:,:))
    ! -------------------------------------------------------- ! DOM -> POM
    conv_lD_lP(:,:)   =  fun_conv_ocnsed2lols(conv_DOM_POM(:,:))
    conv_lD_lP_i(:,:) =  fun_conv_ocnsed2lols_i(conv_DOM_POM_i(:,:))
    ! -------------------------------------------------------- ! POM -> RDOM
    conv_lP_lRD(:,:)   =  fun_conv_sedocn2lslo(conv_POM_RDOM(:,:))
    conv_lP_lRD_i(:,:) =  fun_conv_sedocn2lslo_i(conv_POM_RDOM_i(:,:))
    ! -------------------------------------------------------- ! RDOM -> POM
    conv_lRD_lP(:,:)   =  fun_conv_ocnsed2lols(conv_RDOM_POM(:,:))
    conv_lRD_lP_i(:,:) =  fun_conv_ocnsed2lols_i(conv_RDOM_POM_i(:,:))
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_update_tracerrelationships
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE INTEGRATED TIME-SLICE VALUE ARRAYS
  SUBROUTINE sub_init_int_timeslice()
    ! initialize integrated time
    int_t_timeslice = 0.0
    int_t_timeslice_count = 0
    ! initialize time-slice data - ocn
    int_ocn_timeslice(:,:,:,:)        = 0.0
    int_bio_part_timeslice(:,:,:,:)   = 0.0
    int_bio_settle_timeslice(:,:,:,:) = 0.0
    int_bio_remin_timeslice(:,:,:,:)  = 0.0
    int_phys_ocn_timeslice(:,:,:,:)   = 0.0
    int_carb_timeslice(:,:,:,:)       = 0.0
    int_carbconst_timeslice(:,:,:,:)  = 0.0
    int_carbisor_timeslice(:,:,:,:)   = 0.0
    ! initialize time-slice data - ocn-atm
    int_sfcatm1_timeslice(:,:,:)     = 0.0
    int_focnatm_timeslice(:,:,:)     = 0.0
    int_phys_ocnatm_timeslice(:,:,:) = 0.0
    ! initialize time-slice data - ocn-sed
    int_sfcsed1_timeslice(:,:,:) = 0.0
    int_focnsed_timeslice(:,:,:) = 0.0
    int_fsedocn_timeslice(:,:,:) = 0.0
    ! initialize time-slice data - GOLDSTEIn
    int_opsi_timeslice(:,:)  = 0.0
    int_opsia_timeslice(:,:) = 0.0
    int_opsip_timeslice(:,:) = 0.0
    int_zpsi_timeslice(:,:)  = 0.0
    int_psi_timeslice(:,:)   = 0.0
    int_u_timeslice(:,:,:,:) = 0.0
    ! integrated time slice storage arrays - diagnostics
    int_diag_bio_timeslice(:,:,:)       = 0.0
    int_diag_geochem_timeslice(:,:,:,:) = 0.0
    int_diag_Fe_timeslice(:,:,:,:)      = 0.0
    int_diag_weather_timeslice(:,:,:)   = 0.0
    int_diag_airsea_timeslice(:,:,:)    = 0.0
    int_diag_redox_timeslice(:,:,:,:)   = 0.0
    ! ### ADD ADDITIONAL TIME-SLICE ARRAY INITIALIZATIONS HERE ################################################################### !
    !
    ! ############################################################################################################################ !
  END SUBROUTINE sub_init_int_timeslice
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE INTEGRATED TIME-SERIES VALUE ARRAYS
  SUBROUTINE sub_init_int_timeseries()
    ! initialize integrated time
    int_t_sig       = 0.0
    int_t_sig_count = 0
    ! initialize time-series data
    int_misc_gemlite_sig    = 0.0
    int_ocn_tot_M_sig       = 0.0
    int_ocn_tot_M_sur_sig   = 0.0
    int_ocn_tot_V_sig       = 0.0
    int_ocn_sig(:)          = 0.0
    int_fexport_sig(:)      = 0.0
    int_fracdom_sig(:)      = 0.0
    int_ocnatm_sig(:)       = 0.0
    int_focnatm_sig(:)      = 0.0
    int_focnsed_sig(:)      = 0.0
    int_fsedocn_sig(:)      = 0.0
    int_ocn_sur_sig(:)      = 0.0
    int_ocn_opn_sig(:)      = 0.0
    int_ocn_ben_sig(:)      = 0.0
    int_carb_sur_sig(:)     = 0.0
    int_carb_opn_sig(:)     = 0.0
    int_carb_ben_sig(:)     = 0.0
    int_misc_age_sig        = 0.0
    int_misc_age_sur_sig    = 0.0
    int_misc_age_ben_sig    = 0.0
    int_misc_seaice_sig     = 0.0
    int_misc_seaice_sig_th  = 0.0
    int_misc_seaice_sig_vol = 0.0
    int_misc_opsi_min_sig   = 0.0
    int_misc_opsi_max_sig   = 0.0
    int_misc_opsia_min_sig  = 0.0
    int_misc_opsia_max_sig  = 0.0
    int_misc_SLT_sig        = 0.0
    int_misc_det_Fe_tot_sig = 0.0
    int_misc_det_Fe_dis_sig = 0.0
    int_misc_ocn_solfor_sig = 0.0
    int_misc_opn_solfor_sig = 0.0
    int_misc_ocn_fxsw_sig   = 0.0
    int_misc_opn_fxsw_sig   = 0.0
    int_ocnsed_sig(:)       = 0.0
    int_diag_bio_sig(:)     = 0.0
    int_diag_geochem_sig(:) = 0.0
    int_diag_weather_sig(:) = 0.0
    int_diag_airsea_sig(:)  = 0.0
    int_diag_misc_2D_sig(:) = 0.0
    int_diag_forcing_sig(:) = 0.0
    int_diag_redox_sig(:)   = 0
    int_diag_ecogem_part    = 0.0
    int_diag_ecogem_remin   = 0.0
    ! high resolution 3D! (an exception to the time-series concept that rather spoils things)
    if (ctrl_data_save_3d_sig) int_misc_3D_sig(:,:,:,:) = 0.0
    ! ### ADD ADDITIONAL TIME-SERIES ARRAY INITIALIZATIONS HERE ################################################################## !
    !
    ! ############################################################################################################################ !
  END SUBROUTINE sub_init_int_timeseries
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE FORCING ARRAYS
  SUBROUTINE sub_init_force()
    force_restore_locn(:,:,:,:)    = 0.0
    force_restore_locn_I(:,:,:,:)  = 0.0
    force_restore_locn_II(:,:,:,:) = 0.0
    force_restore_ocn_sig(:,:,:)  = 0.0
    force_restore_ocn_sig_x(:)    = 0.0
    force_restore_ocn_sig_i(:,:)  = 0
    force_restore_ocn_tconst      = 0.0
    force_restore_ocn_select(:)   = .FALSE.
    force_restore_ocn_sur(:)      = .FALSE.
    force_restore_atm(:,:,:)      = 0.0
    force_restore_atm_I(:,:,:)    = 0.0
    force_restore_atm_II(:,:,:)   = 0.0
    force_restore_atm_sig(:,:,:)  = 0.0
    force_restore_atm_sig_x(:)    = 0.0
    force_restore_atm_sig_i(:,:)  = 0
    force_restore_atm_tconst      = 0.0
    force_restore_atm_select(:)   = .FALSE.
    !force_restore_sed(:,:,:)      = 0.0
    !force_restore_sed_I(:,:,:)    = 0.0
    !force_restore_sed_II(:,:,:)   = 0.0
    !force_restore_sed_sig(:,:,:)  = 0.0
    !force_restore_sed_sig_x(:)    = 0.0
    !force_restore_sed_sig_i(:,:)  = 0
    !force_restore_sed_tconst      = 0.0
    !force_restore_sed_select(:)   = .FALSE.
    force_flux_locn(:,:,:,:)       = 0.0
    force_flux_locn_I(:,:,:,:)     = 0.0
    force_flux_locn_II(:,:,:,:)    = 0.0
    force_flux_ocn_sig(:,:,:)     = 0.0
    force_flux_ocn_sig_x(:)       = 0.0
    force_flux_ocn_sig_i(:,:)     = 0
    force_flux_ocn_select(:)      = .FALSE.
    force_flux_ocn_scale(:)       = .FALSE.
    force_flux_atm(:,:,:)         = 0.0
    force_flux_atm_I(:,:,:)       = 0.0
    force_flux_atm_II(:,:,:)      = 0.0
    force_flux_atm_sig(:,:,:)     = 0.0
    force_flux_atm_sig_x(:)       = 0.0
    force_flux_atm_sig_i(:,:)     = 0
    force_flux_atm_select(:)      = .FALSE.
    force_flux_sed_scale(:)       = .FALSE.
    force_flux_sed(:,:,:)         = 0.0
    force_flux_sed_I(:,:,:)       = 0.0
    force_flux_sed_II(:,:,:)      = 0.0
    force_flux_sed_sig(:,:,:)     = 0.0
    force_flux_sed_sig_x(:)       = 0.0
    force_flux_sed_sig_i(:,:)     = 0
    force_flux_sed_select(:)      = .FALSE.
    force_flux_sed_scale(:)       = .FALSE.
    ! misc
    force_solconst_sig(:,:)       = 0.0
    force_restore_docn_nuts(:)    = 0.0
    force_atm_uniform(:)          = 2
    force_ocn_uniform(:)          = 2
    force_sed_uniform(:)          = 2
    force_atm_point_i(:)          = 01
    force_ocn_point_i(:)          = 01
    force_sed_point_i(:)          = 01
    force_atm_point_j(:)          = 01
    force_ocn_point_j(:)          = 01
    force_sed_point_j(:)          = 01
    force_ocn_point_k(:)          = 01
    ! ### ADD ADDITIONAL FORCINGS ARRAY INITIALIZATIONS HERE ##################################################################### !
    !
    ! ############################################################################################################################ !
  END SUBROUTINE sub_init_force
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE AUDIT INVENTORY ARRAYS
  SUBROUTINE sub_init_audit()
    audit_ocn_init(:)       = 0.0
    audit_ocn_old(:)        = 0.0
    audit_ocn_new(:)        = 0.0
    audit_ocn_delta(:)      = 0.0
  END SUBROUTINE sub_init_audit
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE DIAGNOSTICS ARRAYS
  SUBROUTINE sub_init_diag()
    diag_bio(:,:,:)          = 0.0
    diag_geochem(:,:,:,:)    = 0.0
!!!   diag_weather(:,:,:)      = 0.0
    diag_airsea(:,:,:)       = 0.0
    diag_ecogem_part(:,:,:)  = 0.0
    diag_ecogem_remin(:,:,:) = 0.0
  END SUBROUTINE sub_init_diag
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'PHYSICS' - OCEAN
  SUBROUTINE sub_init_phys_ocn()
    ! local variables
    INTEGER::i,j,k
    REAL,DIMENSION(0:n_k+1)::loc_grid_dz,loc_grid_dza
    ! initialize local variables
    loc_grid_dz(0:n_k+1)  = 0.0
    loc_grid_dz(1:n_k)    = goldstein_dz(:)
    loc_grid_dza(0:n_k+1) = 0.0
    loc_grid_dza(1:n_k)   = goldstein_dza(:); loc_grid_dza(n_k) = loc_grid_dz(n_k)/2.0
    ! zero array
    phys_ocn(:,:,:,:) = 0.0
    ! initialize array values
    ! NOTE: initialize basic grid structure values for the (i,j,k) grid, not just ocean-only points
    ! NOTE: depth in in unit of m BELOW sealevel (i.e., a +ve scale)
    ! NOTE: set default rho
    DO i=1,n_i
       DO j=1,n_j
          DO k=1,n_k
             phys_ocn(ipo_lat,i,j,k)      = (180.0/const_pi)*ASIN(goldstein_s(j))
             phys_ocn(ipo_lon,i,j,k)      = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
             phys_ocn(ipo_dlat,i,j,k)     = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
             phys_ocn(ipo_dlon,i,j,k)     = (360.0/n_i)
             phys_ocn(ipo_latn,i,j,k)     = (180.0/const_pi)*ASIN(goldstein_sv(j))
             phys_ocn(ipo_lone,i,j,k)     = (360.0/n_i)*real(i) + par_grid_lon_offset
             phys_ocn(ipo_Dmid,i,j,k)     = SUM(goldstein_dsc*loc_grid_dza(k:n_k))
             phys_ocn(ipo_dD,i,j,k)       = goldstein_dsc*loc_grid_dz(k)
             phys_ocn(ipo_Dbot,i,j,k)     = SUM(goldstein_dsc*loc_grid_dz(k:n_k))
             phys_ocn(ipo_Dtop,i,j,k)     = SUM(goldstein_dsc*loc_grid_dz(k+1:n_k+1))
          end do
          DO k=goldstein_k1(i,j),n_k
             phys_ocn(ipo_A,i,j,k)        = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
             phys_ocn(ipo_rA,i,j,k)       = 1.0 / phys_ocn(ipo_A,i,j,k)
             phys_ocn(ipo_V,i,j,k)        = phys_ocn(ipo_dD,i,j,k)*phys_ocn(ipo_A,i,j,k)
             phys_ocn(ipo_M,i,j,k)        = conv_m3_kg*phys_ocn(ipo_V,i,j,k)
             phys_ocn(ipo_rM,i,j,k)       = 1.0 / phys_ocn(ipo_M,i,j,k)
             phys_ocn(ipo_mask_ocn,i,j,k) = 1.0
             phys_ocn(ipo_rho,i,j,k)      = conv_m3_kg
          END DO
       END DO
    END DO
  END SUBROUTINE sub_init_phys_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'PHYSICS' - OCEAN-ATMOSPHERE INTERFACE
  SUBROUTINE sub_init_phys_ocnatm()
    ! local variables
    INTEGER::i,j
    integer::loc_len
    CHARACTER(len=255)::loc_filename
    ! zero array
    phys_ocnatm(:,:,:) = 0.0
    ! set alt dir path string length
    loc_len = LEN_TRIM(par_pindir_name)
    ! initialize array values
    DO i=1,n_i
       DO j=1,n_j
          phys_ocnatm(ipoa_lat,i,j)  = (180.0/const_pi)*ASIN(goldstein_s(j))
          phys_ocnatm(ipoa_lon,i,j)  = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
          phys_ocnatm(ipoa_dlat,i,j) = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
          phys_ocnatm(ipoa_dlon,i,j) = (360.0/n_i)
          phys_ocnatm(ipoa_A,i,j)    = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
          phys_ocnatm(ipoa_rA,i,j)   = 1.0/ phys_ocnatm(ipoa_A,i,j)
          IF (n_k >= goldstein_k1(i,j)) THEN
             phys_ocnatm(ipoa_seaice,i,j) = 0.0
             phys_ocnatm(ipoa_wspeed,i,j)      = 0.0
             phys_ocnatm(ipoa_mask_ocn,i,j) = 1.0
          END IF
       END DO
    END DO
    ! load prescribed sea-ice cover (if requested)
    ! NOTE: convert from %cover to fractional cover
    if (ctrl_force_seaice) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_seaice_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_seaice(:,:))
       par_phys_seaice(:,:) = par_phys_seaice(:,:)/100.0
    end if
    ! load prescribed wind-speed (if requested)
    ! NOTE: (m s-1)
    if (ctrl_force_windspeed) then
       if (loc_len > 0) then
          loc_filename = TRIM(par_pindir_name)//TRIM(par_windspeed_file)
       else
          loc_filename = TRIM(par_indir_name)//TRIM(par_windspeed_file)
       end if
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_windspeed(:,:))
    end if
  END SUBROUTINE sub_init_phys_ocnatm
  ! ****************************************************************************************************************************** !


  ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
  ! INITIALIZE 'PHYSICS' - OCEAN
  SUBROUTINE sub_data_init_phys_ocn()
    ! local variables
    INTEGER::i,j,k
    integer::loc_n,loc_k1
    REAL,DIMENSION(0:n_k+1)::loc_grid_dz,loc_grid_dza
    ! initialize local variables
    loc_grid_dz(0:n_k+1)  = 0.0
    loc_grid_dz(1:n_k)    = goldstein_dz(:)
    loc_grid_dza(0:n_k+1) = 0.0
    loc_grid_dza(1:n_k)   = goldstein_dza(:); loc_grid_dza(n_k) = loc_grid_dz(n_k)/2.0
    ! initialize array values
    ! NOTE: depth in in unit of m BELOW sealevel (i.e., a +ve scale)
    ! NOTE: set default rho
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_k1 = goldstein_k1(i,j)
          IF (n_k >= loc_k1) THEN
             loc_n = loc_n + 1
             vphys_ocn(loc_n)%i = i
             vphys_ocn(loc_n)%j = j
             vphys_ocn(loc_n)%k1 = loc_k1
             ! initialize, becasue not all 'k' depths are valid
             vphys_ocn(loc_n)%mk(:,:) = 0.0
             DO k=n_k,loc_k1,-1
                vphys_ocn(loc_n)%mk(ipo_lat,k)      = (180.0/const_pi)*ASIN(goldstein_s(j))
                vphys_ocn(loc_n)%mk(ipo_lon,k)      = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
                vphys_ocn(loc_n)%mk(ipo_dlat,k)     = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
                vphys_ocn(loc_n)%mk(ipo_dlon,k)     = (360.0/n_i)
                vphys_ocn(loc_n)%mk(ipo_latn,k)     = (180.0/const_pi)*ASIN(goldstein_sv(j))
                vphys_ocn(loc_n)%mk(ipo_lone,k)     = (360.0/n_i)*real(i) + par_grid_lon_offset
                vphys_ocn(loc_n)%mk(ipo_Dmid,k)     = SUM(goldstein_dsc*loc_grid_dza(k:n_k))
                vphys_ocn(loc_n)%mk(ipo_dD,k)       = goldstein_dsc*loc_grid_dz(k)
                vphys_ocn(loc_n)%mk(ipo_Dbot,k)     = SUM(goldstein_dsc*loc_grid_dz(k:n_k))
                vphys_ocn(loc_n)%mk(ipo_Dtop,k)     = SUM(goldstein_dsc*loc_grid_dz(k+1:n_k+1))
                vphys_ocn(loc_n)%mk(ipo_A,k)        = 2.0*const_pi*(const_rEarth**2)*(1.0/real(n_i))* &
                     & (goldstein_sv(j) - goldstein_sv(j-1))
                vphys_ocn(loc_n)%mk(ipo_rA,k)       = 1.0 / vphys_ocn(loc_n)%mk(ipo_A,k)
                vphys_ocn(loc_n)%mk(ipo_V,k)        = vphys_ocn(loc_n)%mk(ipo_dD,k)*vphys_ocn(loc_n)%mk(ipo_A,k)
                vphys_ocn(loc_n)%mk(ipo_M,k)        = conv_m3_kg*vphys_ocn(loc_n)%mk(ipo_V,k)
                vphys_ocn(loc_n)%mk(ipo_rM,k)       = 1.0 / vphys_ocn(loc_n)%mk(ipo_M,k)
                vphys_ocn(loc_n)%mk(ipo_mask_ocn,k) = 1.0
                vphys_ocn(loc_n)%mk(ipo_rho,k)      = conv_m3_kg
             end DO
          end if
       end do
    end do
  END SUBROUTINE sub_data_init_phys_ocn
  ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !


  ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
  ! INITIALIZE 'PHYSICS' - OCEAN-ATMOSPHERE INTERFACE
  SUBROUTINE sub_data_init_phys_ocnatm()
    ! local variables
    INTEGER::i,j
    integer::loc_len
    integer::loc_n
    CHARACTER(len=255)::loc_filename
    ! set alt dir path string length
    loc_len = LEN_TRIM(par_pindir_name)
    ! initialize array values
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_n = loc_n + 1
          vphys_ocnatm(loc_n)%i = i
          vphys_ocnatm(loc_n)%j = j
          ! initialize, becasue not all 'k' depths are valid
          vphys_ocnatm(loc_n)%m(:) = 0.0
          vphys_ocnatm(loc_n)%m(ipoa_lat)  = (180.0/const_pi)*ASIN(goldstein_s(j))
          vphys_ocnatm(loc_n)%m(ipoa_lon)  = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
          vphys_ocnatm(loc_n)%m(ipoa_dlat) = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
          vphys_ocnatm(loc_n)%m(ipoa_dlon) = (360.0/n_i)
          vphys_ocnatm(loc_n)%m(ipoa_A)    = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
          vphys_ocnatm(loc_n)%m(ipoa_rA)   = 1.0/ vphys_ocnatm(loc_n)%m(ipoa_A)
          IF (n_k >= goldstein_k1(i,j)) THEN
             vphys_ocnatm(loc_n)%m(ipoa_seaice)   = 0.0
             vphys_ocnatm(loc_n)%m(ipoa_wspeed)   = 0.0
             vphys_ocnatm(loc_n)%m(ipoa_mask_ocn) = 1.0
          END IF
       end do
    end do
    ! load prescribed sea-ice cover (if requested)
    ! NOTE: convert from %cover to fractional cover
    if (ctrl_force_seaice) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_seaice_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_seaice(:,:))
       par_phys_seaice(:,:) = par_phys_seaice(:,:)/100.0
    end if
    ! load prescribed wind-speed (if requested)
    ! NOTE: (m s-1)
    if (ctrl_force_windspeed) then
       if (loc_len > 0) then
          loc_filename = TRIM(par_pindir_name)//TRIM(par_windspeed_file)
       else
          loc_filename = TRIM(par_indir_name)//TRIM(par_windspeed_file)
       end if
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_windspeed(:,:))
    end if
  END SUBROUTINE sub_data_init_phys_ocnatm
  ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER COMPOSITION - OCEAN
  SUBROUTINE sub_init_tracer_ocn_comp()
    ! local variables
    INTEGER::i,j,k,io
    real::loc_tot,loc_frac,loc_standard
    ! initialize global arrays
    ocn(:,:,:,:) = 0.0
    ! set <ocn> array
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             DO io=1,n_ocn
                IF (ocn_select(io)) THEN
                   SELECT CASE (ocn_type(io))
                   CASE (1)
                      ocn(io,i,j,k) = ocn_init(io)
                   case (n_itype_min:n_itype_max)
                      loc_tot  = ocn_init(ocn_dep(io))
                      loc_standard = const_standards(ocn_type(io))
                      loc_frac = fun_calc_isotope_fraction(ocn_init(io),loc_standard)
                      ocn(io,i,j,k) = loc_frac*loc_tot
                   case (n_itype_minR:n_itype_maxR)
                      if (io == io_Sr_87Sr) then
                         ocn(io,i,j,k) = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,ocn_init(:),1)
                      elseif (io == io_Sr_88Sr) then
                         ocn(io,i,j,k) = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,ocn_init(:),2)
                      end if
                   END SELECT
                end IF
             end DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in)
  END SUBROUTINE sub_init_tracer_ocn_comp
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER FORCING - ATMOSPHERE
  SUBROUTINE sub_init_tracer_forcing_atm()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::n,ia,ios
    INTEGER::loc_n_elements,loc_n_start
    REAL::loc_force_restore_tconst
    LOGICAL::loc_force_restore_select,loc_force_flux_select,loc_force_flux_scale
    logical::loc_airsea_eqm
    integer::loc_ia
    integer::loc_force_uniform
    integer::loc_force_point_i,loc_force_point_j
    CHARACTER(len=255)::loc_filename
    ! initialize global variables.
    force_restore_atm_select(:)   = .FALSE.
    force_flux_atm_select(:)      = .FALSE.
    force_flux_atm_scale(:)       = .FALSE.
    ocnatm_airsea_eqm(:)          = .FALSE.
    ! check file format
    loc_filename = TRIM(par_fordir_name)//'configure_forcings_atm.dat'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    ! read in default (uniform) atmopshere tracer values
    DO n = 1,loc_n_elements
       if (ctrl_force_oldformat) then
          READ(unit=in,FMT=*,iostat=ios)   &
               & loc_ia,                   & ! COLUMN #00: TRACER NUMBER
               & loc_force_restore_select, & ! COLUMN #01: include restoring forcing of tracer?
               & loc_force_restore_tconst, & ! COLUMN #02: time constant of restoring forcing (years)
               & loc_force_flux_select,    & ! COLUMN #03: include flux forcing of tracer?
               & loc_force_flux_scale,     & ! COLUMN #04: scale flux forcing of tracer?
               & loc_airsea_eqm              ! COLUMN #05: assume ocean in equilibrium with atmosphere?
          call check_iostat(ios,__LINE__,__FILE__)
          loc_force_uniform = -99
          loc_force_point_i = 0
          loc_force_point_j = 0
       else
          READ(unit=in,FMT=*,iostat=ios)   &
               & loc_ia,                   & ! COLUMN #00: TRACER NUMBER
               & loc_force_restore_select, & ! COLUMN #01: include restoring forcing of tracer?
               & loc_force_restore_tconst, & ! COLUMN #02: time constant of restoring forcing (years)
               & loc_force_flux_select,    & ! COLUMN #03: include flux forcing of tracer?
               & loc_force_flux_scale,     & ! COLUMN #04: scale flux forcing of tracer?
               & loc_airsea_eqm,           & ! COLUMN #05: assume ocean in equilibrium with atmosphere?
               & loc_force_uniform,        & ! COLUMN #06: make forcing uniform over this dimension
               & loc_force_point_i,        & ! COLUMN #07: i grid location of point forcing
               & loc_force_point_j           ! COLUMN #08: j grid location of point forcing
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       ia = loc_ia
       force_restore_atm_select(ia) = loc_force_restore_select
       force_restore_atm_tconst(ia) = loc_force_restore_tconst
       force_flux_atm_select(ia)    = loc_force_flux_select
       force_flux_atm_scale(ia)     = loc_force_flux_scale
       ocnatm_airsea_eqm(ia)        = loc_airsea_eqm
       force_atm_uniform(ia)        = loc_force_uniform
       force_atm_point_i(ia)        = loc_force_point_i
       force_atm_point_j(ia)        = loc_force_point_j
       if (force_atm_uniform(ia) > 0) force_flux_atm_scale(ia) = .true.
       if (loc_force_restore_select .AND. (loc_force_restore_tconst < const_real_nullsmall)) then
          CALL sub_report_error( &
               & 'biogem_data','sub_init_atm', &
               & 'Please do not set elected tracer restoring constants to zero (gem_config_atm.par) - '// &
               & 'it can only lead to much unpleasantness later on', &
               & 'STOPPING', &
               & (/const_real_null/),.TRUE. &
               & )
       end if
       IF (loc_force_restore_select .AND. loc_force_flux_select) then
          CALL sub_report_error( &
               & 'biogem_data','init_atm', &
               & 'You are being greedy ... and have both flux AND restoring atmospheric forcing selected'// &
               & '(gem_config_atm.par) - Is this really what you intended?', &
               & 'CONTINUING', &
               & (/const_real_null/),.false. &
               & )
       end if
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! blanket namelist over-ride of forcing point source
    IF ((par_force_point_i > 0) .AND. (par_force_point_j > 0)) then
       force_atm_point_i(:) = par_force_point_i
       force_atm_point_j(:) = par_force_point_j
    end IF
  END SUBROUTINE sub_init_tracer_forcing_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER FORCING - OCEAN
  SUBROUTINE sub_init_tracer_forcing_ocn()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::n,io,ios
    INTEGER::loc_n_elements,loc_n_start
    REAL::loc_force_restore_tconst
    LOGICAL::loc_force_restore_select,loc_force_restore_sur
    LOGICAL::loc_force_flux_select,loc_force_flux_scale
    integer::loc_io
    integer::loc_force_uniform
    integer::loc_force_point_i,loc_force_point_j,loc_force_point_k
    CHARACTER(len=255)::loc_filename
    ! initialize global arrays
    force_restore_ocn_select(:) = .FALSE.
    force_flux_ocn_select(:)    = .FALSE.
    force_flux_ocn_scale(:)     = .FALSE.
    ! check file format
    loc_filename = TRIM(par_fordir_name)//'configure_forcings_ocn.dat'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    !
    DO n = 1,loc_n_elements
       if (ctrl_force_oldformat) then
          READ(unit=in,FMT=*,iostat=ios)   &
               & loc_io,                   & ! COLUMN #00: TRACER NUMBER
               & loc_force_restore_select, & ! COLUMN #01: include restoring forcing of tracer?
               & loc_force_restore_sur,    & ! COLUMN #02: restrict restoring forcing to surface?
               & loc_force_restore_tconst, & ! COLUMN #03: time constant of restoring forcing (years)
               & loc_force_flux_select,    & ! COLUMN #04: include flux forcing of tracer?
               & loc_force_flux_scale        ! COLUMN #05: scale flux forcing of tracer?
          call check_iostat(ios,__LINE__,__FILE__)
          loc_force_uniform = -99
          loc_force_point_i = 0
          loc_force_point_j = 0
          loc_force_point_k = 0
       else
          READ(unit=in,FMT=*,iostat=ios)   &
               & loc_io,                   & ! COLUMN #00: TRACER NUMBER
               & loc_force_restore_select, & ! COLUMN #01: include restoring forcing of tracer?
               & loc_force_restore_sur,    & ! COLUMN #02: restrict restoring forcing to surface?
               & loc_force_restore_tconst, & ! COLUMN #03: time constant of restoring forcing (years)
               & loc_force_flux_select,    & ! COLUMN #04: include flux forcing of tracer?
               & loc_force_flux_scale,     & ! COLUMN #05: scale flux forcing of tracer?
               & loc_force_uniform,        & ! COLUMN #06: make forcing uniform over this dimension
               & loc_force_point_i,        & ! COLUMN #07: i grid location of point forcing
               & loc_force_point_j,        & ! COLUMN #08: j grid location of point forcing
               & loc_force_point_k           ! COLUMN #09: k grid location of point forcing
          call check_iostat(ios,__LINE__,__FILE__)
       endif
       io = loc_io
       force_restore_ocn_select(io) = loc_force_restore_select
       force_restore_ocn_sur(io)    = loc_force_restore_sur
       force_restore_ocn_tconst(io) = loc_force_restore_tconst
       force_flux_ocn_select(io)    = loc_force_flux_select
       force_flux_ocn_scale(io)     = loc_force_flux_scale
       force_ocn_uniform(io)        = loc_force_uniform
       force_ocn_point_i(io)        = loc_force_point_i
       force_ocn_point_j(io)        = loc_force_point_j
       force_ocn_point_k(io)        = loc_force_point_k
       if (force_ocn_uniform(io) > 0) force_flux_ocn_scale(io) = .true.
       ! set local depth limit for ocean boundary conditions (i.e., as as to achieve a surface-only forcing)
       ! NOTE: ensure that land-surface information is preserved (i.e, 'k > n_k')
       ! NOTE: there is currently no restriction of flux forcing to the ocean surface only
       IF (force_restore_ocn_sur(io)) THEN
          force_restore_ocn_k1(io,:,:) = MAX(goldstein_k1(:,:),n_k)
       ELSE
          force_restore_ocn_k1(io,:,:) = goldstein_k1(:,:)
       END IF
       force_flux_ocn_k1(io,:,:) = goldstein_k1(:,:)
       if (loc_force_restore_select .AND. (loc_force_restore_tconst < const_real_nullsmall)) then
          CALL sub_report_error( &
               & 'biogem_data','sub_init_tracer_forcing_ocn', &
               & 'Please do not set selected tracer restoring constants to zero (gem_config_ocn.par) - '// &
               & 'it can only lead to much unpleasantness later on', &
               & 'STOPPING', &
               & (/const_real_null/),.TRUE. &
               & )
       end if
       IF (loc_force_restore_select .AND. loc_force_flux_select) then
          CALL sub_report_error( &
               & 'biogem_data','sub_init_tracer_forcing_ocn', &
               & 'You are being greedy ... and have both flux AND restoring atmospheric forcing selected'// &
               & '(gem_config_atm.par) - Is this really what you intended?', &
               & 'CONTINUING', &
               & (/const_real_null/),.false. &
               & )
       end if
    end DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! blanket namelist over-ride of forcing point source
    IF ((par_force_point_i > 0) .AND. (par_force_point_j > 0) .AND. (par_force_point_k > 0)) then
       force_ocn_point_i(:) = par_force_point_i
       force_ocn_point_j(:) = par_force_point_j
       force_ocn_point_k(:) = par_force_point_k
    end IF

  END SUBROUTINE sub_init_tracer_forcing_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER FORCING - SEDIMENTS
  SUBROUTINE sub_init_tracer_forcing_sed()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::n,is,ios
    INTEGER::loc_n_elements,loc_n_start
    LOGICAL::loc_force_flux_select,loc_force_flux_scale
    integer::loc_is
    integer::loc_force_uniform
    integer::loc_force_point_i,loc_force_point_j
    CHARACTER(len=255)::loc_filename
    ! initialize global variables
    force_flux_sed_select(:) = .FALSE.
    force_flux_sed_scale(:)  = .FALSE.
    ! check file format
    loc_filename = TRIM(par_fordir_name)//'configure_forcings_sed.dat'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    ! read in default sediment tracer info
    DO n = 1,loc_n_elements
       if (ctrl_force_oldformat) then
          READ(unit=in,FMT=*,iostat=ios) &
               & loc_is,                 & ! COLUMN #00: TRACER NUMBER
               & loc_force_flux_select,  & ! COLUMN #01: include flux forcing of tracer?
               & loc_force_flux_scale      ! COLUMN #02: scale flux forcing of tracer?
          call check_iostat(ios,__LINE__,__FILE__)
          loc_force_uniform = -99
          loc_force_point_i = 0
          loc_force_point_j = 0
       else
          READ(unit=in,FMT=*,iostat=ios) &
               & loc_is,                 & ! COLUMN #00: TRACER NUMBER
               & loc_force_flux_select,  & ! COLUMN #01: include flux forcing of tracer?
               & loc_force_flux_scale,   & ! COLUMN #02: scale flux forcing of tracer?
               & loc_force_uniform,      & ! COLUMN #03: make forcing uniform over this dimension
               & loc_force_point_i,      & ! COLUMN #04: i grid location of point forcing
               & loc_force_point_j         ! COLUMN #05: j grid location of point forcing
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       is = loc_is
       force_flux_sed_select(is) = loc_force_flux_select
       force_flux_sed_scale(is)  = loc_force_flux_scale
       force_sed_uniform(is)     = loc_force_uniform
       force_sed_point_i(is)     = loc_force_point_i
       force_sed_point_j(is)     = loc_force_point_j
       if (force_sed_uniform(is) > 0) force_flux_sed_scale(is) = .true.
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! blanket namelist over-ride of forcing point source
    IF ((par_force_point_i > 0) .AND. (par_force_point_j > 0)) then
       force_sed_point_i(:) = par_force_point_i
       force_sed_point_j(:) = par_force_point_j
    end IF
  END SUBROUTINE sub_init_tracer_forcing_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! META-OPTION SETUP AND PARAMETER VALUE CONSISTENCY CHECK
  SUBROUTINE sub_check_par_biogem()
    ! local variables
    LOGICAL::loc_flag
    integer::loc_i,loc_tot_i
    CHARACTER(len=255)::loc_string
    CHARACTER(len=255)::loc_string1,loc_string2
    integer::l,io,ia,is

    ! *** set-up ***
    ! initialize variables
    loc_flag = .FALSE.
    opt_select(:) = .FALSE.
    ! set derived tracer selection options
    opt_select(iopt_select_carbchem)   = ocn_select(io_DIC) .AND. ocn_select(io_ALK)
    opt_select(iopt_select_ocnatm_CO2) = opt_select(iopt_select_carbchem) .AND. atm_select(ia_pCO2)

    ! *** parameter consistency check - biological productivity ***
    ! first ... check for ECOGEM selction
    If ( flag_ecogem .AND. (par_bio_prodopt /= 'NONE') ) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If ECOGEM is selcted, par_bio_prodopt must be NONE', &
            & 'ALTERING INTERNAL PARAMETER VALUE; CONTINUING', &
            & (/const_real_null/),.false. &
            & )
       par_bio_prodopt = 'NONE'
    end IF
    ! check first-order consistency between biologial option, and selected dissolved and sedimentary tracers
    ! NOTE: only the existence of inconsistency will be highlighted, not exactly what the problem is ...
    SELECT CASE (par_bio_prodopt)
    CASE (                        &
         & '1N1T_PO4restore',     &
         & '1N1T_PO4restoreLL',   &
         & '1N1T_PO4MM',          &
         & '1N1T_PO4MM_Tdep',     &
         & '2N1T_PO4MM_SiO2',     &
         & '2N2T_PN_Tdep',        &
         & '3N2T_PNFe_Tdep',      &
         & 'bio_PFe',             &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       IF (.NOT. ocn_select(io_PO4)) loc_flag = .TRUE.
       IF (.NOT. sed_select(is_POP)) loc_flag = .TRUE.
    end select
    SELECT CASE (par_bio_prodopt)
    CASE (                        &
         & '2N1T_PO4MM_SiO2',     &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       IF (.NOT. ocn_select(io_SiO2)) loc_flag = .TRUE.
       IF (.NOT. sed_select(is_opal)) loc_flag = .TRUE.
    end select
    SELECT CASE (par_bio_prodopt)
    case (                    &
         & '2N2T_PO4MM_NO3',   &
         & '2N2T_PN_Tdep',     &
         & '3N2T_PNFe_Tdep'    &
         & )
       IF (.NOT. ocn_select(io_NO3)) loc_flag = .TRUE.
       IF (.NOT. ocn_select(io_N2)) loc_flag = .TRUE.
       IF (.NOT. ocn_select(io_NH4)) loc_flag = .TRUE.
       IF (.NOT. sed_select(is_PON)) loc_flag = .TRUE.
    end select
    SELECT CASE (par_bio_prodopt)
    case (                    &
         & '3N2T_PNFe_Tdep'   &
         & )
       IF (.NOT. ocn_select(io_Fe)) loc_flag = .TRUE.
       IF (.NOT. ocn_select(io_FeL)) loc_flag = .TRUE.
       IF (.NOT. ocn_select(io_L)) loc_flag = .TRUE.
       IF (.NOT. sed_select(is_POFe)) loc_flag = .TRUE.
       IF (.NOT. sed_select(is_POM_Fe)) loc_flag = .TRUE.
    end select
    if (loc_flag) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'Your chosen biological option '//trim(par_bio_prodopt)// &
            & ' is not consistent with the selected ocean (gem_config_ocn.par) and/or sediment (gem_config_sed) tracers. '// &
            & 'Go double-check your selected options, because frankly, I cant be bothered to do your job for you.', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
       loc_flag = .FALSE.
    end IF
    ! #### ADD CHECKS OF ADDITIONAL BIOLOGICAL OPTIONS HERE ###################################################################### !
    !
    ! ############################################################################################################################ !
    ! check nutrient restoring tracer self-consistency
    SELECT CASE (par_bio_prodopt)
    CASE (                     &
         & '1N1T_PO4restore',  &
         & '1N1T_PO4restoreLL' &
         & )
       IF (.NOT. force_restore_ocn_select(io_PO4)) THEN
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'PO4 restoring MUST be enabled (FILE: configure_forcings_ocn.dat) in conjunction with the '//&
               & '<1 x nutrient, 1 x taxa: PO4 restoring biological production> option', &
               & 'ALTERING INTERNAL PARAMETER VALUE; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          force_restore_ocn_select(io_PO4) = .TRUE.
          force_flux_ocn_select(io_PO4) = .FALSE.
          force_restore_ocn_sur(io_PO4) = .TRUE.
          force_restore_ocn_k1(io_PO4,:,:) = MAX(goldstein_k1(:,:),n_k)
       END IF
    CASE (               &
         & 'bio_POCflux' &
         & )
       IF (.NOT. force_flux_sed_select(is_POC)) THEN
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'POC flux forcing MUST be enabled (FILE: configure_forcings_sed.dat) in conjunction with the '//&
               & 'POC flux based biological production option', &
               & 'STOPPING', &
               & (/const_real_null/),.true. &
               & )
       END IF
    end select
    ! check that the necessary dissolved organic matter tracers have been selected for each particulate (sed) tracer selected and
    ! de-select all DOM tracers (including dependents) if no DOM production is specified
    if (par_bio_red_DOMfrac > const_real_nullsmall) then
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_tot_i = conv_POM_DOM_i(0,is)
          do loc_i=1,loc_tot_i
             io = conv_POM_DOM_i(loc_i,is)
             if (.NOT. ocn_select(io)) THEN
                loc_flag = .TRUE.
                loc_string = string_ocn(io)
             end if
          end do
       end do
       if (loc_flag) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'you have rather cheekily set a non-zero fraction of dissolved organic matter production '//&
               & 'but have failed to ensure that you have the necessary DOM tracers selected - '//TRIM(loc_string)// &
               & '[HINT: there must be a corresponding dissolved tracer for each particulate tracer selected '// &
               & 'Sadly, it is too late to automatically select '//TRIM(loc_string)// &
               & ' and a bit risky to set DOM to zero for you :(', &
               & 'STOPPING', &
               & (/const_real_null/),.true. &
               & )
          loc_flag = .FALSE.
       end if
    else
       if (loc_flag) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'although the production of dissolved organic matter production is set to zero '//&
               & 'you have carelessly left some DOM tracers selected (FILE: gem_config_ocn.par) '// &
               & 'The model will run quicker by de-selecting all currently selected DOM tracers.', &
               & 'CONTINUING', &
               & (/const_real_null/),.FALSE. &
               & )
          loc_flag = .FALSE.
       end if
    end if
    ! check Fe cycle self-consistency
    if (ocn_select(io_Fe) .OR. ocn_select(io_TDFe)) then
       SELECT CASE (trim(opt_geochem_Fe))
       CASE ('hybrid','lookup_4D')
          ! NOTE: do not need to explicitly check for io_TDFe (again!)
          if (.NOT. ocn_select(io_TL)) THEN
             loc_flag = .TRUE.
          end if
       case default
          ! NOTE: do not need to explicitly check for io_Fe (again!)
          if ((.NOT. ocn_select(io_L)) .OR. (.NOT. ocn_select(io_FeL))) THEN
             loc_flag = .TRUE.
          end if
       end select
       if (loc_flag) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'The selected Fe tracers (base config) does not match the selected Fe scheme (user config).', &
               & 'STOPPING', &
               & (/const_real_null/),.TRUE. &
               & )
          loc_flag = .FALSE.
       end if
    end if
    ! check color tracers
    if (ctrl_force_ocn_age .AND. (.NOT.(ocn_select(io_colr) .AND. ocn_select(io_colb)))) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'Parameter: ctrl_force_ocn_age is selected (true), but the necessary red and blue ocean tracers are not.'// &
               & 'The automatic age tracer option is hence deselected.', &
               & 'CONTINUING', &
               & (/const_real_null/),.FALSE. &
               & )
       ctrl_force_ocn_age = .false.
    end if

    ! *** parameter consistency check - isotopes, forcings ***
    ! OCEAN TRACERS
    do io=1,n_ocn
       IF (ocn_select(io)) THEN
          if (.not. ocn_select(ocn_dep(io))) then
             loc_string = string_ocn(ocn_dep(io))
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'If an isotope tracer is selected, the associated bulk ocean tracer '//TRIM(loc_string)// &
                  & ' must be selected', &
                  & 'STOPPING', &
                  & (/const_real_null/),.true. &
                  & )
          end if
          if (ocn_select(ocn_dep(io)) .AND. (io /= ocn_dep(io))) then
             if ( &
                  & (force_restore_ocn_select(io) .AND. (.NOT. force_restore_ocn_select(ocn_dep(io)))) &
                  & .OR. &
                  & (.NOT. (force_restore_ocn_select(io)) .AND. force_restore_ocn_select(ocn_dep(io))) &
                  & ) then
                loc_string1 = string_ocn(io)
                loc_string2 = string_ocn(ocn_dep(io))
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                     & ' have been selected, but a restoring forcing for only one of them has been selected.', &
                     & 'CONTINUING', &
                     & (/const_real_null/),.false. &
                     & )
             end if
             if ( &
                  & (force_flux_ocn_select(io) .AND. (.NOT. force_flux_ocn_select(ocn_dep(io)))) &
                  & .OR. &
                  & (.NOT. (force_flux_ocn_select(io)) .AND. force_flux_ocn_select(ocn_dep(io))) &
                  & ) then
                loc_string1 = string_ocn(io)
                loc_string2 = string_ocn(ocn_dep(io))
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                     & ' have been selected, but a flux forcing for only one of them has been selected.', &
                     & 'CONTINUING', &
                     & (/const_real_null/),.false. &
                     & )
             end if
          end if
       else
          if (force_restore_ocn_select(io)) then
             loc_string = string_ocn(io)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the ocean tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a restoring forcing of this tracer cannot be performed', &
                  & 'RESTORING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_restore_ocn_select(io) = .FALSE.
          end if
          if (force_flux_ocn_select(io)) then
             loc_string = string_ocn(io)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the ocean tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a flux forcing of this tracer cannot be performed', &
                  & 'FLUX FORCING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_flux_ocn_select(io) = .FALSE.
          end if
       end if
    end do
    ! ATMOSPHERE TRACERS
    do ia=1,n_atm
       IF (atm_select(ia)) THEN
          if (.not. atm_select(atm_dep(ia))) then
             loc_string = string_atm(atm_dep(ia))
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'If an isotope tracer is selected, the associated bulk atmosphere tracer '//TRIM(loc_string)// &
                  & ' must be selected', &
                  & 'STOPPING', &
                  & (/const_real_null/),.true. &
                  & )
          end if
          if (atm_select(atm_dep(ia)) .AND. (io /= atm_type(ia))) then
             if ( &
                  & (force_restore_atm_select(ia) .AND. (.NOT. force_restore_atm_select(atm_dep(ia)))) &
                  & .OR. &
                  & (.NOT. (force_restore_atm_select(ia)) .AND. force_restore_atm_select(atm_dep(ia))) &
                  & ) then
                loc_string1 = string_atm(ia)
                loc_string2 = string_atm(atm_dep(ia))
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                     & ' have been selected, but a restoring forcing for only one of them has been selected.', &
                     & 'CONTINUING', &
                     & (/const_real_null/),.false. &
                     & )
             end if
             if ( &
                  & (force_flux_atm_select(ia) .AND. (.NOT. force_flux_atm_select(atm_dep(ia)))) &
                  & .OR. &
                  & (.NOT. (force_flux_atm_select(ia)) .AND. force_flux_atm_select(atm_dep(ia))) &
                  & ) then
                loc_string1 = string_atm(ia)
                loc_string2 = string_atm(atm_dep(ia))
                If ((ia == ia_pCO2_14C) .AND. (atm_dep(ia) == ia_pCO2)) then
                   CALL sub_report_error( &
                        & 'biogem_data','sub_check_par', &
                        & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                        & ' have been selected, but a flux forcing for only one of them has been selected.', &
                        & 'CONTINUING', &
                        & (/const_real_null/),.false. &
                        & )
                else
                   CALL sub_report_error( &
                        & 'biogem_data','sub_check_par', &
                        & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                        & ' have been selected, but a flux forcing for only one of them has been selected.', &
                        & 'CONTINUING', &
                        & (/const_real_null/),.false. &
                        & )
                end If
             end if
          end if
       else
          if (force_restore_atm_select(ia)) then
             loc_string = string_atm(ia)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the atmospheric tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a restoring forcing of this tracer cannot be performed', &
                  & 'RESTORING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_restore_atm_select(ia) = .FALSE.
          end if
          if (force_flux_atm_select(ia)) then
             loc_string = string_atm(ia)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the atmospheric tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a flux forcing of this tracer cannot be performed', &
                  & 'RESTORING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_flux_atm_select(ia) = .FALSE.
          end if
       end IF
    end do
    ! SEDIMENT TRACERS
    do is=1,n_sed
       IF (sed_select(is)) THEN
          if (.not. sed_select(sed_dep(is))) then
             loc_string1 = string_sed(is)
             loc_string2 = string_sed(sed_dep(is))
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'If an isotope or other dependent tracer  '//TRIM(loc_string1)//' is selected, '// &
                  & 'the associated bulk sediment tracer '//TRIM(loc_string), &
                  & 'STOPPING', &
                  & (/const_real_null/),.true. &
                  & )
          end if
          if (sed_select(sed_dep(is)) .AND. (is /= sed_dep(is)) .AND. (sed_type(is) /= par_sed_type_scavenged)) then
             if ( &
                  & (force_flux_sed_select(is) .AND. (.NOT. force_flux_sed_select(sed_dep(is)))) &
                  & .OR. &
                  & (.NOT. (force_flux_sed_select(is)) .AND. force_flux_sed_select(sed_dep(is))) &
                  & ) then
                loc_string1 = string_sed(is)
                loc_string2 = string_sed(sed_dep(is))
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                     & ' have been selected, but a flux forcing for only one of them has been selected.', &
                     & 'CONTINUING', &
                     & (/const_real_null/),.false. &
                     & )
             end if
          end if
       else
          if (force_flux_sed_select(is)) then
             loc_string = string_sed(is)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the sediment tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a flux forcing of this tracer cannot be performed', &
                  & 'RESTORING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_flux_sed_select(is) = .FALSE.
          end if
       end IF
    end do

    ! *** FIX UP AND MAKE GENERIC ***
    ! verify ocn-atm carbon cycle option selection
    IF (atm_select(ia_pCO2) .NEQV. ocn_select(io_DIC)) THEN
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'Do you really mean to select CO2 in the atmosphere but not in the ocean (or vice versa)?', &
            & 'CONTINUING', &
            & (/const_real_null/),.false. &
            & )
    ENDIF

    ! *** parameter consistency check - selected tracers and sediment-sediment option combinations ***
    do is=1,n_sed
       if (sed_type(is) == par_sed_type_frac) then
          if (( .NOT. sed_select(is)) .AND. (sed_select(sed_dep(is)))) then
             loc_string2 = string_sed(is)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'A frac2 tracer must be selected associated with '//TRIM(loc_string2), &
                  & 'STOPPING', &
                  & (/const_real_null/),.true. &
                  & )
          end if
       end if
    end do
    IF (sed_select(is_CaCO3) .AND. (.NOT. sed_select(is_POC))) THEN
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par','The POC tracer must be selected with CaCO3 in biogem_config_sed.par ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    If (sed_select(is_CaCO3_age) .AND. (.NOT. sed_select(is_CaCO3))) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If the sediment CaCO3 age tracer is requested, then the solid CaCO3 tracer must be selected ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    !
    If (sed_select(is_POC) .AND. (.NOT. sed_select(is_POC_frac2))) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If the organic matter tracer is requested, then the 2nd fraction (is_POC_frac2) tracer must be selected ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    If (sed_select(is_CaCO3) .AND. (.NOT. sed_select(is_CaCO3_frac2))) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If the CaCO3 tracer is requested, then the 2nd fraction (is_CaCO3_frac2) tracer must be selected ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    If (sed_select(is_opal) .AND. (.NOT. sed_select(is_opal_frac2))) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If the opal tracer is requested, then the 2nd fraction (is_opal_frac2) tracer must be selected ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF

    ! *** parameter consistency check - selected sediment-ocean tracer option combinations ***
    if (par_bio_prodopt /= 'NONE') then
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          select case (sed_type(is))
          case (par_sed_type_bio,par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_scavenged, &
               & n_itype_min:n_itype_max)
             loc_tot_i = conv_sed_ocn_i(0,is)
             If (loc_tot_i < 1) then
                loc_string2 = string_sed(is)
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'Particulate tracer '//TRIM(loc_string2)// &
                     & ' does does not have *any* corresponding ocean tracer selected', &
                     & 'STOPPING ...', &
                     & (/const_real_null/),.true. &
                     & )
             end If
             do loc_i=1,loc_tot_i
                io = conv_sed_ocn_i(loc_i,is)
                if (abs(conv_sed_ocn(io,is)) > const_real_nullsmall) then
                   if (.NOT. ocn_select(io)) then
                      loc_string1 = string_ocn(io)
                      loc_string2 = string_sed(is)
                      CALL sub_report_error( &
                           & 'biogem_data','sub_check_par', &
                           & 'Particulate tracer '//TRIM(loc_string2)// &
                           & ' does does not have *all possible* corresponding ocean tracers selected, such as '//TRIM(loc_string1)// &
                           & ' (BUT may not ened them ...)', &
                           & 'CONTINUING', &
                           & (/const_real_null/),.false. &
                           & )
                   end if
                end if
             end do
          end SELECT
       end DO
    end if

    ! *** parameter consistency check - 14C ***
    ! NOTE: just test ocean tracers
    IF (ocn_select(io_DIC_14C) .AND. (.NOT. ocn_select(io_DIC_13C))) THEN
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'To select 14C isotope tracers, 13C isotope tracers MUST also be selected', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    end if

    ! *** parameter consistency check - data save options ***
    IF (.NOT. opt_select(iopt_select_carbchem)) THEN
       IF (ctrl_data_save_sig_carb_sur) THEN
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'You do not have sufficent ocean tracers selected for a marine carbon cycle', &
               & '[ctrl_data_save_sig_carb_sur] HAS BEEN DE-SELECTED; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          ctrl_data_save_sig_carb_sur = .FALSE.
       end if
       If (ctrl_data_save_slice_carb) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'You do not have sufficent ocean tracers selected for a marine carbon cycle', &
               & '[ctrl_data_save_slice_carb] HAS BEEN DE-SELECTED; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          ctrl_data_save_slice_carb = .FALSE.
       end if
       If (ctrl_data_save_slice_carbconst) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'You do not have sufficent ocean tracers selected for a marine carbon cycle', &
               & '[ctrl_data_save_slice_carbconst] HAS BEEN DE-SELECTED; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          ctrl_data_save_slice_carbconst = .FALSE.
       end IF
    end IF

! *** transport matrix paramater consistency checks ***
    if(ctrl_data_diagnose_TM)THEN
         if((par_data_TM_start+n_k).gt.par_misc_t_runtime.and.(par_data_TM_start-n_k).gt.0.0)then
                 call sub_report_error( &
		         & 'biogem_data','sub_check_par', &
			 & 'Diagnosing transport matrix will take longer than the run. par_data_TM_start has been set to finish at end of run', &
			 & '[par_data_TM_start] HAS BEEN CHANGED TO ALLOW MATRIX DIAGNOSIS TO FINISH', &
			 & (/const_real_null/),.false. &
			 & )
                 par_data_TM_start=par_misc_t_runtime-n_k
         end if
         if((par_data_TM_start+n_k).gt.par_misc_t_runtime.and.(par_data_TM_start-n_k).lt.0.0)then
                 call sub_report_error( &
		         & 'biogem_data','sub_check_par', &
			 & 'The run is too short to diagnose a full transport matrix', &
			 & '[ctrl_data_diagnose_TM] HAS BEEN DE-SELECTED; CONTINUING', &
			 & (/const_real_null/),.false. &
			 & )
                 ctrl_data_diagnose_TM=.false.
         end if
         if(ctrl_bio_preformed)then
                 call sub_report_error( &
		         & 'biogem_data','sub_check_par', &
			 & 'Diagnosing transport matrix will overwrite preformed tracers', &
			 & '[ctrl_data_diagnose_TM] HAS BEEN DE-SELECTED; CONTINUING', &
			 & (/const_real_null/),.false. &
			 & )
                 ctrl_data_diagnose_TM=.false.
         end if
         if(conv_kocn_kbiogem.gt.1.0)then
                 call sub_report_error( &
		         & 'biogem_data','sub_check_par', &
			 & 'Biogem timestep ratio should not be greater than 1 for a transport matrix', &
			 & 'STOPPING', &
			 & (/const_real_null/),.true. &
			 & )
         end if
        if((conv_kocn_ksedgem/par_data_save_slice_n).ne.par_data_TM_avg_n)then ! n.b. conv_ksedgem = n_timesteps!!
                 call sub_report_error( &
		         & 'biogem_data','sub_check_par', &
			 & 'The intra-annual saving intervals you have chosen do not correspond to the transport matrix averaging', &
			 & '[par_data_save_slice_n] HAS BEEN SET TO MATCH MATRIX AVERAGING INTERVAL', &
			 & (/const_real_null/),.false. &
			 & )
                par_data_save_slice_n=conv_kocn_ksedgem/par_data_TM_avg_n
         end if
    end if

  END SUBROUTINE sub_check_par_biogem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DATA SAVE META CONFIG
  SUBROUTINE sub_adj_par_save()

    select case (par_data_save_level)
    case (0:99)
       ctrl_data_save_slice_ocnatm = .false.
       ctrl_data_save_slice_ocn = .false.
       ctrl_data_save_slice_ocnsed = .false.
       ctrl_data_save_slice_fairsea = .false.
       ctrl_data_save_slice_focnatm = .false.
       ctrl_data_save_slice_focnsed = .false.
       ctrl_data_save_slice_fsedocn = .false.
       ctrl_data_save_slice_bio = .false.
       ctrl_data_save_slice_carb = .false.
       ctrl_data_save_slice_carbconst = .false.
       ctrl_data_save_slice_phys_atm = .false.
       ctrl_data_save_slice_phys_ocn = .false.
       ctrl_data_save_slice_misc = .false.
       ctrl_data_save_slice_diag = .false.
       ctrl_data_save_slice_diag_bio = .false.
       ctrl_data_save_slice_diag_geochem = .false.
       ctrl_data_save_slice_diag_proxy = .false.
       ctrl_data_save_slice_diag_tracer = .false.
       ctrl_data_save_sig_ocnatm = .false.
       ctrl_data_save_sig_ocn = .false.
       ctrl_data_save_sig_fexport = .false.
       ctrl_data_save_sig_fairsea = .false.
       ctrl_data_save_sig_ocnsed = .false.
       ctrl_data_save_sig_focnatm = .false.
       ctrl_data_save_sig_focnsed = .false.
       ctrl_data_save_sig_fsedocn = .false.
       ctrl_data_save_sig_ocn_sur = .false.
       ctrl_data_save_sig_carb_sur = .false.
       ctrl_data_save_sig_misc = .false.
       ctrl_data_save_sig_diag = .false.
       ctrl_data_save_derived = .false.
       ctrl_data_save_GLOBAL = .false.
    case default
       ! set new *non namelist* defined sub-options (to broadly retain back-compatability)
       ctrl_data_save_slice_diag_bio = ctrl_data_save_slice_diag
       ctrl_data_save_slice_diag_geochem = ctrl_data_save_slice_diag
       ctrl_data_save_slice_diag_proxy = ctrl_data_save_slice_diag
       ctrl_data_save_slice_diag_tracer = ctrl_data_save_slice_diag
       ctrl_data_save_sig_diag_bio = ctrl_data_save_sig_diag
       ctrl_data_save_sig_diag_geochem = ctrl_data_save_sig_diag
    end select

    ! meta meta options
    if (ctrl_data_save_slice_cdrmip) par_data_save_level = 0

    ! set BASIC options
    select case (par_data_save_level)
    case (2:99)
       ctrl_data_save_slice_ocnatm = .true.
       ctrl_data_save_slice_ocn = .true.
       ctrl_data_save_slice_misc = .true.
       if (flag_sedgem) ctrl_data_save_slice_ocnsed = .true.
       ctrl_data_save_sig_ocnatm = .true.
       ctrl_data_save_sig_ocn = .true.
       ctrl_data_save_sig_ocn_sur = .true.
       ctrl_data_save_sig_misc = .true.
       ctrl_data_save_GLOBAL = .true.
       if (flag_sedgem) ctrl_data_save_sig_ocnsed = .true.
    case default
       ! NOTHING
    end select

    select case (par_data_save_level)
    case (0)
       ! save NOTHING
    case (1)
       ! MINIMUM (biogeochem ONLY)
       ctrl_data_save_slice_misc = .false.
       ctrl_data_save_sig_misc = .false.
    case (2)
       ! BASIC (biogeochem + BASIC physics)
    case (3)
       ! BASIC + biology diagnostics
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
    case (4)
       ! BASIC + geochem diagnostics
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
    case (5)
       ! BASIC + biology + geochem diagnostics
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
    case (6)
       ! BASIC + tracer diagnostics
       ctrl_data_save_slice_diag_tracer = .true.
       ctrl_data_save_sig_diag = .true.
    case (7)
       ! BASIC + tracer + proxy diagnostics
       ctrl_data_save_slice_diag_proxy = .true.
       ctrl_data_save_slice_diag_tracer = .true.
       ctrl_data_save_sig_diag = .true.
    case (8)
       ! BASIC + biology + geochem + tracer + proxy diagnostics
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_sur = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       ctrl_data_save_slice_diag_proxy = .true.
       ctrl_data_save_slice_diag_tracer = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
       ctrl_data_save_derived = .true.
    case (9)
       ! BASIC + full physics
       ctrl_data_save_slice_phys_atm = .true.
       ctrl_data_save_slice_phys_ocn = .true.
    case (10)
       ! OCEAN ACIDIFICATION & FOSSIL FUEL GAMES
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_sur = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       ctrl_data_save_slice_ocnsed = .false.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
    case (14)
       ! BASIC + FULL geochem diagnostics
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
       ctrl_bio_remin_redox_save=.true.
    case (15)
       ! BASIC + biology + geochem diagnostics
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
       ctrl_bio_remin_redox_save=.true.
    case (18)
       ! BASIC + biology + geochem + tracer + proxy diagnostics
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       ctrl_data_save_slice_diag_proxy = .true.
       ctrl_data_save_slice_diag_tracer = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
       ctrl_data_save_derived = .true.
       ctrl_bio_remin_redox_save=.true.
    case (99)
       ! EVERYTHING
       ctrl_data_save_slice_ocnatm = .true.
       ctrl_data_save_slice_ocn = .true.
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_fairsea = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_carbconst = .true.
       ctrl_data_save_slice_phys_atm = .true.
       ctrl_data_save_slice_phys_ocn = .true.
       ctrl_data_save_slice_misc = .true.
       ctrl_data_save_slice_diag = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       ctrl_data_save_slice_diag_proxy = .true.
       ctrl_data_save_slice_diag_tracer = .true.
       if (flag_sedgem) ctrl_data_save_slice_ocnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_ocnatm = .true.
       ctrl_data_save_sig_ocn = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_ocn_sur = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_misc = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       ctrl_data_save_derived = .true.
       ctrl_data_save_GLOBAL = .true.
       if (flag_sedgem) ctrl_data_save_sig_ocnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
       ctrl_bio_remin_redox_save=.true.
    case default
       ! [leave user-specified settings]
    end select

    ! detrmine whether to save inversion diagnostics
    ctrl_data_save_inversion = .false.
    IF ( &
         & (force_restore_atm_select(ia_pCO2) .AND. (force_flux_atm_select(ia_pCO2) .OR. force_flux_ocn_select(io_DIC))) &
         & .OR. &
         & (force_restore_ocn_select(io_colr) .AND. (force_flux_atm_select(ia_pCO2) .OR. force_flux_ocn_select(io_DIC))) &
         & .OR. &
         & (force_restore_atm_select(ia_pCO2_13C) .AND. force_flux_atm_select(ia_pCO2_13C)) &
         & .OR. &
         & (force_restore_ocn_select(io_DIC_13C) .AND. force_flux_atm_select(ia_pCO2_13C)) &
         & .OR. &
         & (force_restore_ocn_select(io_DOM_C_13C) .AND. force_flux_atm_select(ia_pCO2_13C)) &
         & .OR. &
         & (force_restore_ocn_select(io_DIC_13C) .AND. force_flux_ocn_select(io_DIC_13C)) &
         & .OR. &
         & (force_restore_ocn_select(io_ALK) .AND. force_flux_ocn_select(io_ALK)) &
         & .OR. &
         & (force_restore_ocn_select(io_Ca_44Ca) .AND. force_flux_ocn_select(io_Ca_44Ca)) &
         & ) THEN
       ctrl_data_save_inversion = .true.
    end IF

    ! determine if no biology at all
    If ((par_bio_prodopt == 'NONE') .AND. (.NOT. flag_ecogem)) then
       ctrl_data_save_slice_bio      = .false.
       ctrl_data_save_slice_diag_bio = .false.
       ctrl_data_save_sig_fexport    = .false.
       ctrl_data_save_sig_diag_bio   = .false.
    end if

  END SUBROUTINE sub_adj_par_save
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE CARBONATE SYSTEM
  SUBROUTINE sub_init_carb()
    ! local variables
    INTEGER::i,j,k
    ! zero arrays
    ! NOTE: leave carb_TSn array at its initialized state
    !       so that a full update of carb constants etc is ALWAYS performed upon the first call to tstep_biogem
    carbconst(:,:,:,:) = 0.0
    carb(:,:,:,:)      = 0.0
    carbalk(:,:,:,:)   = 0.0
    carbisor(:,:,:,:)  = 0.0
    carb_TSn(:,:,:,:)  = 0.0
    ! initialize arrays
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             ! calculate carbonate constants
             CALL sub_calc_carbconst(         &
                  & phys_ocn(ipo_Dmid,i,j,k), &
                  & ocn(io_T,i,j,k),          &
                  & ocn(io_S,i,j,k),          &
                  & carbconst(:,i,j,k)        &
                  & )
             ! adjust carbonate constants
             if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
                call sub_adj_carbconst(   &
                     & ocn(io_Ca,i,j,k),  &
                     & ocn(io_Mg,i,j,k),  &
                     & carbconst(:,i,j,k) &
                     & )
             END if
             IF (opt_select(iopt_select_carbchem)) then
                ! estimate Ca and borate concentrations (if not selected and therefore explicitly treated)
                IF (.NOT. ocn_select(io_Ca))  ocn(io_Ca,i,j,k)  = fun_calc_Ca(ocn(io_S,i,j,k))
                IF (.NOT. ocn_select(io_B))   ocn(io_B,i,j,k)   = fun_calc_Btot(ocn(io_S,i,j,k))
                IF (.NOT. ocn_select(io_SO4)) ocn(io_SO4,i,j,k) = fun_calc_SO4tot(ocn(io_S,i,j,k))
                IF (.NOT. ocn_select(io_F))   ocn(io_F,i,j,k)   = fun_calc_Ftot(ocn(io_S,i,j,k))
                ! seed default initial ocean pH
                carb(ic_H,i,j,k) = 10**(-7.8)
                ! calculate carbonate chemistry
                CALL sub_calc_carb(        &
                     & ocn(io_DIC,i,j,k),  &
                     & ocn(io_ALK,i,j,k),  &
                     & ocn(io_Ca,i,j,k),   &
                     & ocn(io_PO4,i,j,k),  &
                     & ocn(io_SiO2,i,j,k), &
                     & ocn(io_B,i,j,k),    &
                     & ocn(io_SO4,i,j,k),  &
                     & ocn(io_F,i,j,k),    &
                     & ocn(io_H2S,i,j,k),  &
                     & ocn(io_NH4,i,j,k),  &
                     & carbconst(:,i,j,k), &
                     & carb(:,i,j,k),      &
                     & carbalk(:,i,j,k)    &
                     & )
                ! estimate Revelle factor
                CALL sub_calc_carb_RF0(      &
                     & ocn(io_DIC,i,j,k),  &
                     & ocn(io_ALK,i,j,k),  &
                     & ocn(io_PO4,i,j,k),  &
                     & ocn(io_SiO2,i,j,k), &
                     & ocn(io_B,i,j,k),    &
                     & ocn(io_SO4,i,j,k),  &
                     & ocn(io_F,i,j,k),    &
                     & ocn(io_H2S,i,j,k),  &
                     & ocn(io_NH4,i,j,k),  &
                     & carbconst(:,i,j,k), &
                     & carb(:,i,j,k)    &
                     & )
                ! calculate carbonate system isotopic properties
                if (ocn_select(io_DIC_13C)) then
                   call sub_calc_carb_r13C(      &
                        & ocn(io_T,i,j,k),       &
                        & ocn(io_DIC,i,j,k),     &
                        & ocn(io_DIC_13C,i,j,k), &
                        & carb(:,i,j,k),         &
                        & carbisor(:,i,j,k)      &
                        & )
                end IF
                if (ocn_select(io_DIC_14C)) then
                   call sub_calc_carb_r14C(      &
                        & ocn(io_T,i,j,k),       &
                        & ocn(io_DIC,i,j,k),     &
                        & ocn(io_DIC_14C,i,j,k), &
                        & carb(:,i,j,k),         &
                        & carbisor(:,i,j,k)      &
                        & )
                end IF
             end if
          END DO
       END DO
    END DO
  END SUBROUTINE sub_init_carb
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE SOLUBILITY CONSTANTS
  SUBROUTINE sub_init_solconst()
    ! local variables
    INTEGER::i,j
    ! zero arrays
    ocnatm_airsea_solconst(:,:,:) = 0.0
    ! initialize array
    DO i=1,n_i
       DO j=1,n_j
          if (n_k >= goldstein_k1(i,j)) then
             call sub_calc_solconst(i,j)
          end if
       END DO
    END DO
  END SUBROUTINE sub_init_solconst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE DATA SAVING
  SUBROUTINE sub_init_data_save()
    ! local variables
    INTEGER::n
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::loc_i
    real::loc_data_scale

    ! *** set time series data save interval details ***
    ! initialize time series indices
    par_data_save_sig_i = n_data_max
    par_data_save_sig(:) = 0.0
    ! load data
    loc_filename = TRIM(par_indir_name)//TRIM(par_infile_sig_name)
    loc_data_scale = 1.0
    CALL sub_load_data_t1(loc_filename,loc_data_scale,par_data_save_sig,loc_n_elements)
    ! if no elements, populate array with default time interval steps
    IF (loc_n_elements == 0) THEN
       ! limit the time-series integration interval
       if (par_data_save_sig_dt > const_real_nullsmall) then
          loc_n_elements = INT(par_misc_t_runtime/par_data_save_sig_dt + const_real_nullsmall)
          do while (loc_n_elements > n_data_max)
             par_data_save_sig_dt = 10.0*par_data_save_sig_dt
             loc_n_elements = INT(par_misc_t_runtime/par_data_save_sig_dt + const_real_nullsmall)
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_data_save','time-series save interval (biogem_config.par) too short - '// &
                  & 'was [lower value] and is now [upper value] (years)', &
                  & 'CONTINUING', &
                  & (/par_data_save_sig_dt,par_data_save_sig_dt/10.0/),.FALSE. &
                  & )
          end do
          DO n=1,loc_n_elements
             par_data_save_sig(n) = &
                  & real(n - 0.5)*par_data_save_sig_dt + (par_misc_t_runtime - real(loc_n_elements)*par_data_save_sig_dt)
          END DO
       else
          CALL sub_report_error( &
               & 'biogem_data','sub_init_data_save','time-series save interval (biogem_config.par) '// &
               & 'must be non-zero and positive', &
               & 'STOPPING', &
               & (/const_real_null/),.TRUE. &
               & )
       endif
    end IF
    ! find first save time lying within total model run-time
    ! NOTE: <loc_i> will be zero if no valid time points have been requested in the time series input file,
    !       and the array has not been populated automatically
    ! NOTE: ensure that the first identified time-series time is at least a full integration interval (required value)
    !       from the start time of the model run
    loc_i = loc_n_elements
    DO while (loc_i > 0)
       IF ( &
            & par_data_save_sig(loc_i) &
            & < &
            & (par_misc_t_runtime - par_data_save_sig_dt/2.0 + par_misc_t_err) &
            & ) THEN
          EXIT
       ELSE
          loc_i = loc_i - 1
       END IF
    END DO
    par_data_save_sig_i = loc_i
    ! automatically populate run end (if requested)
    if (ctrl_data_save_sig_autoend) then
       DO loc_i=par_data_save_sig_i,1,-1
          if (par_data_save_sig(loc_i) < (1.0 - par_data_save_sig_dt/2.0 + par_misc_t_err)) then
             IF (par_data_save_sig(loc_i) > (par_data_save_sig_dt/2.0 - par_misc_t_err)) then
                exit
             else
                par_data_save_sig(loc_i) = par_data_save_sig_dt/2.0
                exit
             end if
          end if
       END DO
    end if

    ! *** set time slice data save details ***
    ! NOTE: DO NOT populate the time-slice array automatically if the data file is empty
    ! initialize time slice indices
    par_data_save_timeslice_i = n_data_max
    par_data_save_timeslice(:) = 0.0
    ! load data
    loc_filename = TRIM(par_indir_name)//TRIM(par_infile_slice_name)
    loc_data_scale = 1.0
    CALL sub_load_data_t1(loc_filename,loc_data_scale,par_data_save_timeslice,loc_n_elements)
    ! find first save time lying within total model run-time
    ! NOTE: <par_data_save_timeslice_i> will be zero if no valid time slices have been requested in the time slice input file
    ! NOTE: ensure that the first identified time-slice time is at least a full integration interval (required value)
    !       from the start time of the model run
    loc_i = loc_n_elements
    DO while (loc_i > 0)
       IF (par_data_save_timeslice(loc_i) < (par_misc_t_runtime - par_data_save_slice_dt/2.0 + par_misc_t_err)) THEN
          EXIT
       ELSE
          loc_i = loc_i - 1
       END IF
    END DO
    if (par_data_save_timeslice(loc_i) < (par_data_save_slice_dt/2.0 - par_misc_t_err)) then
       loc_i = 0
    end if
    par_data_save_timeslice_i = loc_i
    if (par_data_save_timeslice_i == 0) then
       CALL sub_report_error( &
            & 'biogem_data','sub_init_data_save', &
            & 'No time-slice dates listed in file biogem_save_timeslice.dat fall within the model start and end years', &
            & 'CONTINUING', &
            & (/const_real_null/),.false. &
            & )
    end if
    ! automatically populate run end (if requested)
    if (ctrl_data_save_slice_autoend) then
       if (par_data_save_timeslice_i == 0) then
          par_data_save_timeslice(1) = par_data_save_slice_dt/2.0
          par_data_save_timeslice_i = 1
          CALL sub_report_error( &
               & 'biogem_data','sub_init_data_save', &
               & 'ADDED: single save at end of run', &
               & 'CONTINUING', &
               & (/const_real_null/),.false. &
               & )
       else
          DO loc_i=par_data_save_timeslice_i,1,-1
             if (par_data_save_timeslice(loc_i) < (1.0 - par_data_save_slice_dt/2.0 + par_misc_t_err)) then
                IF (par_data_save_timeslice(loc_i) > (par_data_save_slice_dt/2.0 - par_misc_t_err)) then
                   exit
                else
                   par_data_save_timeslice(loc_i) = par_data_save_slice_dt/2.0
                   exit
                end if
             end if
          END DO
       end if
    end if

  END SUBROUTINE sub_init_data_save
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  SUBROUTINE sub_init_data_save_orb()
    USE biogem_lib
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::ios
    CHARACTER(len=255)::loc_filename                           ! filename string
    CHARACTER(len=255)::loc_str                                !
    CHARACTER(len=6)::loc_locstr                               !
    integer::nloc,nvar
    integer::loc_len,loc_pos
    real::loc_t = 0.0
    ! -------------------------------------------------------- !
    ! INITIALIZE ARRAYS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! allocate time array
    ALLOCATE(orb_pts_time(1:n_orb_pts_nmax),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    orb_pts_time(:) = 0.0
    ! -------------------------------------------------------- ! determine loc array size
    if (n_orb_pts_nmax > 0) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_infile_orb_pts_loc_name)
       n_orb_pts_nloc = fun_calc_data_n(loc_filename)
    else
       n_orb_pts_nloc = 0
    end if
    ! -------------------------------------------------------- ! allocate loc array
    ALLOCATE(orb_pts_loc(1:n_orb_pts_nloc,2),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! -------------------------------------------------------- ! load loc data
    if (n_orb_pts_nloc > 0) then
       call sub_load_data_npt(loc_filename,n_orb_pts_nloc,orb_pts_loc)
    end if
    ! -------------------------------------------------------- ! determine var array size
    if (n_orb_pts_nmax > 0) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_infile_orb_pts_var_name)
       n_orb_pts_nvar = fun_calc_data_n(loc_filename)
    else
       n_orb_pts_nvar = 0
    end if
    ! -------------------------------------------------------- ! allocate var array
    ALLOCATE(orb_pts_var(1:n_orb_pts_nvar),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! -------------------------------------------------------- ! load var data
    if (n_orb_pts_nvar > 0) then
       call sub_load_data_nstr(loc_filename,n_orb_pts_nvar,orb_pts_var)
    end if
    ! -------------------------------------------------------- ! allocate results array
    ! n_orb_ptse_nmax == number of orbital data points
    ! n_orb_ptse_nloc == number of orbital data point locations
    ! n_orb_ptse_nvar == number of orbital data point variables
    ALLOCATE(orb_pts(1:n_orb_pts_nmax,1:n_orb_pts_nloc,1:n_orb_pts_nvar),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! zero array
    orb_pts(:,:,:) = 0.0
    ! -------------------------------------------------------- !
    ! CREATE FILE PIPES
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! create header
    loc_pos = 1
    loc_str(loc_pos:loc_pos+5) = '% year'
    loc_pos = loc_pos+6
    DO nvar=1,n_orb_pts_nvar
       loc_str(loc_pos:loc_pos+2) = ' / '
       loc_pos = loc_pos+3
       loc_len = len(trim(orb_pts_var(nvar)(:)))
       loc_str(loc_pos:loc_pos+loc_len-1) = trim(orb_pts_var(nvar)(:))
       loc_pos = loc_pos+loc_len
    end do
    ! pad string array to make it happy
    loc_str(loc_pos:255) = ''
    ! -------------------------------------------------------- ! create files
    DO nloc=1,n_orb_pts_nloc
       loc_locstr = 'i'//fun_conv_num_char_n(2,orb_pts_loc(nloc,1))//'j'//fun_conv_num_char_n(2,orb_pts_loc(nloc,2))
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_orb',loc_locstr,string_results_ext)
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_str)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    end do
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_init_data_save_orb
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE RESTORING FORCING - OCEAN
  ! NOTE: options are:
  !        3 == 3D (uniform across entire ocean volumne)
  !        2 == 2D (uniform across surface)
  !        0 == forcing at a point
  !       -2 == BENTHIC (spatially explicit on benthic surface -- needs 2D file '_BEN')
  !       -3 == LEVEL (spatially explicit on any depth surface -- needs 2D file '_LVL')
  !       ELSE, 3D spatially explicit forcing (needs a pair of 3D files '_I' and '_II')
  SUBROUTINE sub_init_force_restore_ocn()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::l,i,j,k,io
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk
    real,DIMENSION(2)::loc_data_scale
    ! LOOP
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       IF (force_restore_ocn_select(io)) THEN
          force_restore_ocn_sig_i(io,:) = n_data_max
          force_restore_ocn_sig(io,:,:) = 0.0
          ! load forcing data array #I
          loc_ijk(:,:,:) = const_real_null
          if (force_ocn_uniform(io) == 3) then
             loc_ijk(:,:,:) = 0.0
          elseif (force_ocn_uniform(io) == 2) then
             loc_ijk(:,:,n_k) = 0.0
          elseif (force_ocn_uniform(io) == 0) then
             loc_ijk(force_ocn_point_i(io),force_ocn_point_j(io),force_ocn_point_k(io)) = 0.0
          elseif (force_ocn_uniform(io) == -1) then
             loc_ijk(:,:,n_k) = 0.0
          elseif (force_ocn_uniform(io) == -2) then
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(:,:,goldstein_k1(i,j)) = 0.0
                   end if
                end DO
             end DO
          elseif (force_ocn_uniform(io) == -3) then
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(:,:,force_ocn_point_k(io)) = 0.0
                   end if
                end DO
             end DO
          elseif (force_ocn_uniform(io) == -4) then
             loc_ijk(:,:,n_k) = 0.0
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(:,:,goldstein_k1(i,j)) = 0.0
                   end if
                end DO
             end DO
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                DO k=force_restore_ocn_k1(io,i,j),n_k
                   force_restore_locn_I(l,i,j,k) = loc_ijk(i,j,k)
                end do
             end do
          end DO
          ! load forcing data array #II
          loc_ijk(:,:,:) = const_real_null
          if (force_ocn_uniform(io) == 3) then
             loc_ijk(:,:,:) = phys_ocn(ipo_mask_ocn,:,:,:)
          elseif (force_ocn_uniform(io) == 2) then
             loc_ijk(:,:,n_k) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_ocn_uniform(io) == 0) then
             loc_ijk(force_ocn_point_i(io),force_ocn_point_j(io),force_ocn_point_k(io)) = 1.0
          elseif (force_ocn_uniform(io) == -1) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_SUR'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,n_k) = loc_ij(i,j)
                   end if
                end do
             end DO
          elseif (force_ocn_uniform(io) == -2) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_BEN'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,goldstein_k1(i,j)) = loc_ij(i,j)
                   end if
                end do
             end DO
          elseif (force_ocn_uniform(io) == -3) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_LVL'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,force_ocn_point_k(io)) = loc_ij(i,j)
                   end if
                end do
             end DO
          elseif (force_ocn_uniform(io) == -4) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_SUR'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,n_k) = loc_ij(i,j)
                   end if
                end do
             end DO
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_BEN'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,goldstein_k1(i,j)) = loc_ij(i,j)
                   end if
                end do
             end DO
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                DO k=force_restore_ocn_k1(io,i,j),n_k
                   force_restore_locn_II(l,i,j,k) = loc_ijk(i,j,k)
                end do
             end do
          end DO
          ! load forcing time series data
          loc_filename =TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = (/par_ocn_force_scale_time(io), par_ocn_force_scale_val(io)/)
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_restore_ocn_sig(io,:,:),loc_n_elements)
          ! set default forcing index values
          ! NOTE: catch missing time series data
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_restore_ocn','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_restore_ocn_sig_i(io,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          ! NOTE: this will catch both possible mismatches of forcing signal and model integration specification
          !       i.e., for _not_ the BP option;
          !             a signal time start year that is after the model start year
          !             or a signal time year that is before the model end year
          !             (or visa versa for a BP time scale)
          if ( &
               & (minval(force_restore_ocn_sig(io,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_restore_ocn_sig(io,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_restore_ocn', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or'// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_restore_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE RESTORING FORCING - ATMOSPHERE
  ! NOTE: options are:
  !        2 == 2D (uniform across surface)
  !        0 == forcing at a point
  !       ELSE, 2D spatially explicit forcing (needs a pair of 2D files '_I' and '_II')
  SUBROUTINE sub_init_force_restore_atm()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::l,i,j,ia
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(2)::loc_data_scale
    ! LOOP
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       IF (force_restore_atm_select(ia)) THEN
          force_restore_atm_sig_i(ia,:) = n_data_max
          force_restore_atm_sig(ia,:,:) = 0.0
          ! load forcing data array #I
          loc_ij(:,:) = const_real_null
          if (force_atm_uniform(ia) == 2) then
             loc_ij(:,:) = 0.0
          elseif (force_atm_uniform(ia) == 0) then
             loc_ij(force_atm_point_i(ia),force_atm_point_j(ia)) = 0.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_atm_'//TRIM(string_atm(ia))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_restore_atm_I(ia,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing data array #II
          loc_ij(:,:) = const_real_null
          if (force_atm_uniform(ia) == 2) then
             loc_ij(:,:) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_atm_uniform(ia) == 0) then
             loc_ij(force_atm_point_i(ia),force_atm_point_j(ia)) = 1.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_atm_'//TRIM(string_atm(ia))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_restore_atm_II(ia,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing time series data
          loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_atm_'//TRIM(string_atm(ia))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = (/par_atm_force_scale_time(ia), par_atm_force_scale_val(ia)/)
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_restore_atm_sig(ia,:,:),loc_n_elements)
          ! set default forcing index values
          ! NOTE: catch missing time series data
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_restore_atm','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_restore_atm_sig_i(ia,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          ! NOTE: this will catch both possible mismatches of forcing signal and model integration specification
          !       i.e., for _not_ the BP option;
          !             a signal time start year that is after the model start year
          !             or a signal time year that is before the model end year
          !             (or visa versa for a BP time scale)
          if ( &
               & (minval(force_restore_atm_sig(ia,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_restore_atm_sig(ia,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_restore_atm', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or'// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_restore_atm
  ! ****************************************************************************************************************************** !


!!$  ! INITIALIZE RESTORING FORCING - SEDIMENTS
!!$  SUBROUTINE sub_init_force_restore_sed(dum_is)
!!$    ! dummy arguments
!!$    INTEGER,INTENT(in)::dum_is
!!$    ! local varisbles
!!$    CHARACTER(len=255)::loc_filename
!!$    INTEGER::loc_n_elements
!!$    ! initislize forcing signal indices
!!$    force_restore_sed_sig_i(dum_is,:) = n_data_max
!!$    force_restore_sed_sig(dum_is,:,:) = 0.0
!!$    ! load forcing data array #I
!!$    loc_filename = TRIM(par_indir_name)//'biogem_force_restore_sed_'//TRIM(string_sed(dum_is))//'_I'//TRIM(string_data_ext)
!!$    CALL sub_load_data_ij(loc_filename,n_i,n_j,force_restore_sed_I(dum_is,:,:))
!!$    ! load forcing data array #II
!!$    loc_filename = TRIM(par_indir_name)//'biogem_force_restore_sed_'//TRIM(string_sed(dum_is))//'_II'//TRIM(string_data_ext)
!!$    CALL sub_load_data_ij(loc_filename,n_i,n_j,force_restore_sed_II(dum_is,:,:))
!!$    ! load forcing time series data
!!$    loc_filename = TRIM(par_indir_name)//'biogem_force_restore_sed_'//TRIM(string_sed(dum_is))//'_sig'//TRIM(string_data_ext)
!!$    CALL sub_load_data_t2(loc_filename,force_restore_sed_sig(dum_is,:,:),loc_n_elements)
!!$    ! set default forcing index values
!!$    ! NOTE: catch missing time series data
!!$    if (loc_n_elements == 0) THEN
!!$       CALL sub_report_error( &
!!$            & 'biogem_data','init_force_restore_sed','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
!!$            & 'STOPPING', &
!!$            & (/const_real_null/),.TRUE. &
!!$            & )
!!$    else
!!$       force_restore_sed_sig_i(dum_is,:) = loc_n_elements
!!$    end if
!!$    ! warn if forcing information appears to be 'incomplete'
!!$    ! NOTE: this will catch both possible mismatches of forcing signal and model integration specification
!!$    !       i.e., for _not_ the BP option;
!!$    !             a signal time start year that is after the model start year
!!$    !             or a signal time year that is before the model end year
!!$    !             (or visa versa for a BP time scale)
!!$    if (maxval(force_restore_sed_sig(dum_is,1,1:loc_n_elements)) < par_misc_t_runtime) then
!!$       CALL sub_report_error( &
!!$            & 'biogem_data','sub_init_force_restore_sed', &
!!$            & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
!!$            & '(1) alter the model start time year (the goin file) '// &
!!$            & '(2) alter the forcing signal start time year (FILE: '//TRIM(loc_filename)//') '// &
!!$            & '(3) leave everything well alone '// &
!!$            & '(it is legitamite to start a model forcing part way through a run by defining a partial time signal)', &
!!$            & 'CONTINUING', &
!!$            & (/const_real_null/),.false. &
!!$            & )
!!$    end if
!!$    if (minval(force_restore_sed_sig(dum_is,1,1:loc_n_elements)) > 0.0) then
!!$       CALL sub_report_error( &
!!$            & 'biogem_data','sub_init_force_restore_sed', &
!!$            & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
!!$            & '(1) alter the model start time year and/or run length (the goin file) '// &
!!$            & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') '// &
!!$            & '(3) leave everything well alone '// &
!!$            & '(it is legitamite to stop a model forcing part way through a run by defining a partial time signal)', &
!!$            & 'CONTINUING', &
!!$            & (/const_real_null/),.false. &
!!$            & )
!!$    end if
!!$  END SUBROUTINE sub_init_force_restore_sed


  ! ****************************************************************************************************************************** !
  ! INITIALIZE FLUX FORCING - OCEAN
  ! NOTE: options are:
  !        3 == 3D (uniform across entire ocean volumne)
  !        2 == 2D (uniform across surface)
  !        0 == forcing at a point
  !       -1 == SURFACE (spatially explicit @ surface -- needs 2D file '_SUR')
  !       -2 == BENTHIC (spatially explicit on benthic surface -- needs 2D file '_BEN')
  !       -3 == LEVEL (spatially explicit on any depth surface -- needs 2D file '_LVL')
  !       ELSE, 3D spatially explicit forcing (needs a pair of 3D files '_I' and '_II')
  SUBROUTINE sub_init_force_flux_ocn()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::l,i,j,k,io
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk
    real,DIMENSION(2)::loc_data_scale
    ! LOOP
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       IF (force_flux_ocn_select(io)) THEN
          force_flux_ocn_sig_i(io,:) = n_data_max
          force_flux_ocn_sig(io,:,:) = 0.0
          ! load forcing data array #I
          loc_ijk(:,:,:) = const_real_zero
          if (force_ocn_uniform(io) == 3) then
             loc_ijk(:,:,:) = 0.0
          elseif (force_ocn_uniform(io) == 2) then
             loc_ijk(:,:,n_k) = 0.0
          elseif (force_ocn_uniform(io) == 0) then
             loc_ijk(force_ocn_point_i(:),force_ocn_point_j(:),force_ocn_point_k(:)) = 0.0
          elseif (force_ocn_uniform(io) == -1) then
             loc_ijk(:,:,n_k) = 0.0
          elseif (force_ocn_uniform(io) == -2) then
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(:,:,goldstein_k1(i,j)) = 0.0
                   end if
                end DO
             end DO
          elseif (force_ocn_uniform(io) == -3) then
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(:,:,force_ocn_point_k(io)) = 0.0
                   end if
                end DO
             end DO
          elseIF ((par_force_point_i > 0) .AND. (par_force_point_j > 0) .AND. (par_force_point_k > 0)) then
             loc_ijk(force_ocn_point_i(:),force_ocn_point_j(:),force_ocn_point_k(:)) = 0.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                DO k=force_flux_ocn_k1(io,i,j),n_k
                   force_flux_locn_I(l,i,j,k) = loc_ijk(i,j,k)
                end do
             end do
          end DO
          ! load forcing data array #II
          loc_ijk(:,:,:) = const_real_zero
          if (force_ocn_uniform(io) == 3) then
             loc_ijk(:,:,:) = phys_ocn(ipo_mask_ocn,:,:,:)
          elseif (force_ocn_uniform(io) == 2) then
             loc_ijk(:,:,n_k) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_ocn_uniform(io) == 0) then
             loc_ijk(force_ocn_point_i(io),force_ocn_point_j(io),force_ocn_point_k(io)) = 1.0
          elseif (force_ocn_uniform(io) == -1) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_SUR'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,n_k) = loc_ij(i,j)
                   end if
                end do
             end DO
          elseif (force_ocn_uniform(io) == -2) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_BEN'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,goldstein_k1(i,j)) = loc_ij(i,j)
                   end if
                end do
             end DO
          elseif (force_ocn_uniform(io) == -3) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_LVL'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,force_ocn_point_k(io)) = loc_ij(i,j)
                   end if
                end do
             end DO
          elseIF ((par_force_point_i > 0) .AND. (par_force_point_j > 0) .AND. (par_force_point_k > 0)) then
             loc_ijk(force_ocn_point_i(io),force_ocn_point_j(io),force_ocn_point_k(io)) = 1.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                DO k=force_flux_ocn_k1(io,i,j),n_k
                   force_flux_locn_II(l,i,j,k) = loc_ijk(i,j,k)
                end do
             end do
          end DO
          ! load forcing time series data
          loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = (/par_ocn_force_scale_time(io), par_ocn_force_scale_val(io)/)
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_flux_ocn_sig(io,:,:),loc_n_elements)
          ! set default forcing index values
          ! NOTE: catch missing time series data
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_flux_ocn','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_flux_ocn_sig_i(io,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          ! NOTE: this will catch both possible mismatches of forcing signal and model integration specification
          !       i.e., for _not_ the BP option;a signal time start year that is after the model start year
          !             or a signal time year that is before the model end year (or visa versa for a BP time scale)
          if ( &
               & (minval(force_flux_ocn_sig(io,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_flux_ocn_sig(io,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_flux_ocn', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or '// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_flux_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE FLUX FORCING - ATMOSPHERE
  ! NOTE: options are:
  !        2 == 2D (uniform across surface)
  !        0 == forcing at a point
  !       -1 == SURFACE (spatially explicit across surface -- needs 2D file '_SUR')
  !       ELSE, 2D spatially explicit forcing (needs a pair of 2D files '_I' and '_II')
  SUBROUTINE sub_init_force_flux_atm()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::l,i,j,ia
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(2)::loc_data_scale
    ! LOOP
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       IF (force_flux_atm_select(ia)) THEN
          force_flux_atm_sig_i(ia,:) = n_data_max
          force_flux_atm_sig(ia,:,:) = 0.0
          ! load forcing data array #I
          loc_ij(:,:) = const_real_zero
          if (force_atm_uniform(ia) == 2) then
             loc_ij(:,:) = 0.0
          elseif (force_atm_uniform(ia) == 0) then
             loc_ij(force_atm_point_i(ia),force_atm_point_j(ia)) = 0.0
          elseif (force_atm_uniform(ia) == -1) then
             loc_ij(:,:) = 0.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_atm_'//TRIM(string_atm(ia))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_flux_atm_I(ia,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing data array #II
          loc_ij(:,:) = const_real_zero
          if (force_atm_uniform(ia) == 2) then
             loc_ij(:,:) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_atm_uniform(ia) == 0) then
             loc_ij(force_atm_point_i(ia),force_atm_point_j(ia)) = 1.0
          elseif (force_atm_uniform(ia) == -1) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_atm_'//TRIM(string_atm(ia))//'_SUR'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_atm_'//TRIM(string_atm(ia))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_flux_atm_II(ia,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing time series data
          loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_atm_'//TRIM(string_atm(ia))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = (/par_atm_force_scale_time(ia), par_atm_force_scale_val(ia)/)
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_flux_atm_sig(ia,:,:),loc_n_elements)
          ! set default forcing index values
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_flux_atm','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_flux_atm_sig_i(ia,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          if ( &
               & (minval(force_flux_atm_sig(ia,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_flux_atm_sig(ia,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_flux_atm', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or '// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_flux_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE FLUX FORCING - SEDIMENTS
  ! NOTE: options are:
  !        2 == 2D (uniform across surface)
  !        0 == forcing at a point
  !       -1 == SURFACE (spatially explicit across surface -- needs 2D file '_SUR')
  !       ELSE, 2D spatially explicit forcing (needs a pair of 2D files '_I' and '_II')
  SUBROUTINE sub_init_force_flux_sed()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::l,i,j,is
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(2)::loc_data_scale
    ! LOOP
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       IF (force_flux_sed_select(is)) THEN
          force_flux_sed_sig_i(is,:) = n_data_max
          force_flux_sed_sig(is,:,:) = 0.0
          ! load forcing data array #I
          loc_ij(:,:) = 0.0
          if (force_sed_uniform(is) == 2) then
             loc_ij(:,:) = 0.0
          elseif (force_sed_uniform(is) == 0) then
             loc_ij(force_sed_point_i(is),force_sed_point_j(is)) = 0.0
          elseif (force_sed_uniform(is) == -1) then
             loc_ij(:,:) = 0.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_sed_'//TRIM(string_sed(is))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_flux_sed_I(is,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing data array #II
          loc_ij(:,:) = 0.0
          if (force_sed_uniform(is) == 2) then
             loc_ij(:,:) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_sed_uniform(is) == 0) then
             loc_ij(force_sed_point_i(is),force_sed_point_j(is)) = 1.0
          elseif (force_sed_uniform(is) == -1) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_sed_'//TRIM(string_sed(is))//'_SUR'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_sed_'//TRIM(string_sed(is))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_flux_sed_II(is,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing time series data
          loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_sed_'//TRIM(string_sed(is))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = 1.0
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_flux_sed_sig(is,:,:),loc_n_elements)
          ! set default forcing index values
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_flux_sed','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_flux_sed_sig_i(is,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          if ( &
               & (minval(force_flux_sed_sig(is,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_flux_sed_sig(is,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_flux_sed', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or'// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_flux_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD IRON SPECIATION LOOK-UP TABLES
  SUBROUTINE sub_data_init_lookup_4D_Fe()
    USE genie_util, ONLY: check_unit, check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::a,b,c,d
    CHARACTER(len=255)::loc_filename
    integer::ios  ! for file checks
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe_file_1)
    par_lookup_Fe_n_1 = fun_calc_data_n(loc_filename)
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe_file_2)
    par_lookup_Fe_n_2 = fun_calc_data_n(loc_filename)
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe_file_3)
    par_lookup_Fe_n_3 = fun_calc_data_n(loc_filename)
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe_file_4)
    par_lookup_Fe_n_4 = fun_calc_data_n(loc_filename)
    ! -------------------------------------------------------- !
    ! DIMENSION LOOKUP TABLE ARRAYS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! vectors
    ALLOCATE(lookup_Fe_1D_1(par_lookup_Fe_n_1),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(lookup_Fe_1D_2(par_lookup_Fe_n_2),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(lookup_Fe_1D_3(par_lookup_Fe_n_3),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(lookup_Fe_1D_4(par_lookup_Fe_n_4),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! -------------------------------------------------------- ! table
    ALLOCATE(lookup_Fe_4D_Fe3(par_lookup_Fe_n_1,par_lookup_Fe_n_2,par_lookup_Fe_n_3,par_lookup_Fe_n_4),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(lookup_Fe_4D_geo(par_lookup_Fe_n_1,par_lookup_Fe_n_2,par_lookup_Fe_n_3,par_lookup_Fe_n_4),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! -------------------------------------------------------- !
    ! READ IN LOOKUP TABLE AXES
    ! -------------------------------------------------------- ! #1
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe_file_1)
    call sub_load_data_i(loc_filename,par_lookup_Fe_n_1,lookup_Fe_1D_1)
    ! -------------------------------------------------------- ! #2
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe_file_2)
    call sub_load_data_i(loc_filename,par_lookup_Fe_n_2,lookup_Fe_1D_2)
    ! -------------------------------------------------------- ! #3
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe_file_3)
    call sub_load_data_i(loc_filename,par_lookup_Fe_n_3,lookup_Fe_1D_3)
    ! -------------------------------------------------------- ! #4
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe_file_4)
    call sub_load_data_i(loc_filename,par_lookup_Fe_n_4,lookup_Fe_1D_4)
    ! -------------------------------------------------------- !
    ! READ IN LOOKUP TABLE DATA -- Fe3
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! open file pipe
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_Fe3_file)
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! -------------------------------------------------------- ! read in data
    DO a = 1,par_lookup_Fe_n_1
       DO b = 1,par_lookup_Fe_n_2
          DO c = 1,par_lookup_Fe_n_3
             DO d = 1,par_lookup_Fe_n_4
                READ(unit=in,FMT=*,iostat=ios) lookup_Fe_4D_Fe3(a,b,c,d)
                call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! -------------------------------------------------------- ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! -------------------------------------------------------- !
    ! READ IN LOOKUP TABLE DATA -- geo
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! open file pipe
    loc_filename = TRIM(par_indir_name)//TRIM(par_lookup_geo_file)
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! -------------------------------------------------------- ! read in data
    DO a = 1,par_lookup_Fe_n_1
       DO b = 1,par_lookup_Fe_n_2
          DO c = 1,par_lookup_Fe_n_3
             DO d = 1,par_lookup_Fe_n_4
                READ(unit=in,FMT=*,iostat=ios) lookup_Fe_4D_geo(a,b,c,d)
                call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! -------------------------------------------------------- ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_init_lookup_4D_Fe
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DATA SAVING ROUTINES - GOLDSTEIn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE GRID DATA
  SUBROUTINE sub_data_save_topography()
    ! local variables
    CHARACTER(len=255)::loc_filename
    ! (i,j) topography (max height in m)
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_topography'//TRIM(string_data_ext)
    CALL sub_save_data_ij(loc_filename,n_i,n_j,-maxval(phys_ocn(ipo_mask_ocn,:,:,:)*phys_ocn(ipo_Dbot,:,:,:),3))
    ! grid point centre
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lat_mid'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,phys_ocn(ipo_lat,:,:,:))
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lon_mid'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,phys_ocn(ipo_lon,:,:,:))
    ! grid point limits
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lat_n'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,phys_ocn(ipo_latn,:,:,:))
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lat_s'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,phys_ocn(ipo_latn,:,:,:) - phys_ocn(ipo_dlat,:,:,:))
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lon_e'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,phys_ocn(ipo_lone,:,:,:))
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lon_w'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,phys_ocn(ipo_lone,:,:,:) - phys_ocn(ipo_dlon,:,:,:))
    ! layer height (m)
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lay_top'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,-phys_ocn(ipo_Dtop,:,:,:))
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lay_bot'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,-phys_ocn(ipo_Dbot,:,:,:))
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_lay_mid'//TRIM(string_data_ext)
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,-phys_ocn(ipo_Dmid,:,:,:))
  END SUBROUTINE sub_data_save_topography
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE STREAMFUNCTION DATA
  SUBROUTINE sub_data_save_goldstein_opsi()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::j,k,ios
    REAL::loc_scale
    REAL,DIMENSION(n_k+1)::loc_grid_dz
    CHARACTER(len=255)::loc_filename
    ! initialize local variables
    loc_grid_dz(:) = 0.0
    loc_scale = goldstein_dsc*goldstein_usc*const_rEarth*1.0E-6
    !
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_opsi_lat'//TRIM(string_data_ext)
    call check_unit(out,__LINE__,__FILE__)
    OPEN(out,file=loc_filename,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    DO k=n_k,0,-1
       WRITE(unit=out,fmt='(999e14.6)',iostat=ios) ((180.0/const_pi) * ASIN(goldstein_sv(j)),j=0,n_j)
       call check_iostat(ios,__LINE__,__FILE__)
    ENDDO
    CLOSE(out,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    !
    loc_grid_dz(1:n_k) = goldstein_dz(:)
    loc_filename = TRIM(par_outdir_name)//TRIM(par_outfile_name)//'_grid_opsi_depth'//TRIM(string_data_ext)
    call check_unit(out,__LINE__,__FILE__)
    OPEN(out,file=loc_filename,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    DO k=n_k,0,-1
       WRITE(unit=out,fmt='(999e14.6)',iostat=ios) (SUM(-goldstein_dsc * loc_grid_dz(k+1:n_k+1)),j=0,n_j)
       call check_iostat(ios,__LINE__,__FILE__)
    ENDDO
    CLOSE(out,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    !
    loc_filename= &
         & fun_data_timeslice_filename( &
         & par_outdir_name,trim(par_outfile_name)//'_slice','misc_goldstein_opsi',string_results_ext)
    call check_unit(out,__LINE__,__FILE__)
    OPEN(out,file=loc_filename,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    DO k=n_k,0,-1
       WRITE(unit=out,fmt='(999e14.6)',iostat=ios) (loc_scale*int_opsi_timeslice(j,k)/int_t_timeslice,j=0,n_j)
       call check_iostat(ios,__LINE__,__FILE__)
    ENDDO
    CLOSE(out,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    loc_filename= &
         & fun_data_timeslice_filename( &
         & par_outdir_name,trim(par_outfile_name)//'_slice','misc_goldstein_opsia',string_results_ext)
    call check_unit(out,__LINE__,__FILE__)
    OPEN(out,file=loc_filename,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    DO k=n_k,0,-1
       WRITE(unit=out,fmt='(999e14.6)',iostat=ios) (loc_scale*int_opsia_timeslice(j,k)/int_t_timeslice,j=0,n_j)
       call check_iostat(ios,__LINE__,__FILE__)
    ENDDO
    CLOSE(out,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    loc_filename= &
         & fun_data_timeslice_filename( &
         & par_outdir_name,trim(par_outfile_name)//'_slice','misc_goldstein_opsip',string_results_ext)
    call check_unit(out,__LINE__,__FILE__)
    OPEN(out,file=loc_filename,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    DO k=n_k,0,-1
       WRITE(unit=out,fmt='(999e14.6)',iostat=ios) (loc_scale*int_opsip_timeslice(j,k)/int_t_timeslice,j=0,n_j)
       call check_iostat(ios,__LINE__,__FILE__)
    ENDDO
    CLOSE(out)

  END SUBROUTINE sub_data_save_goldstein_opsi
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE VELOCITY FIELD DATA
  SUBROUTINE sub_data_save_goldstein_u()
    ! local variables
    CHARACTER(len=255)::loc_filename
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk
    ! save data
    ! NOTE: scale to give velocity components in units of (m s-1);
    !       for the horizontal velocity components, the scale factor is usc (= 0.05) [Edwards and Shepherd, 2002]
    !       for the vertical velocity component, the overall scale factor is usc*dsc/rsc
    !       (= 0.05*4000.0/6.36e6) [Edwards and Shepherd, 2002]
    loc_filename= fun_data_timeslice_filename( &
         & par_outdir_name,trim(par_outfile_name)//'_slice','misc_goldstein_u_1',string_results_ext)
    loc_ijk(:,:,:) = goldstein_usc*int_u_timeslice(1,:,:,:)/int_t_timeslice
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
    loc_filename= fun_data_timeslice_filename( &
         & par_outdir_name,trim(par_outfile_name)//'_slice','misc_goldstein_u_2',string_results_ext)
    loc_ijk(:,:,:) = goldstein_usc*int_u_timeslice(2,:,:,:)/int_t_timeslice
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
    loc_filename= fun_data_timeslice_filename( &
         & par_outdir_name,trim(par_outfile_name)//'_slice','misc_goldstein_u_3',string_results_ext)
    loc_ijk(:,:,:) = (goldstein_usc*goldstein_dsc/const_rEarth)*int_u_timeslice(3,:,:,:)/int_t_timeslice
    CALL sub_save_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
  END SUBROUTINE sub_data_save_goldstein_u
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! MISCELLANEOUS ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! OUTPUT AUDIT DIAGNOSTICS
  SUBROUTINE sub_data_audit_diagnostics()
    ! local variables
    INTEGER::l,io
    REAL::loc_ocn_rM,loc_ocn_R
    ! calculate local constants
    loc_ocn_rM = 1.0/SUM(phys_ocn(ipo_M,:,:,:))
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       SELECT CASE (io)
          ! \/\/\/ MAKE MODIFICATIONS TO REPORT ADDITIONAL TRACER AUDIT HERE
       CASE (io_DIC,io_NO3,io_PO4,io_Fe,io_O2,io_SiO2,io_ALK,io_Ca,io_B,io_SO4,io_F,io_colr,io_colb)
          if (audit_ocn_init(io) > const_real_nullsmall) then
             loc_ocn_R = audit_ocn_new(io)/audit_ocn_init(io)
          end if
          PRINT*,'INITIAL / FINAL ',string_ocn(io),' inventory:', &
               & audit_ocn_init(io), &
               & '/', &
               & audit_ocn_new(io), &
               & '( == ', 100.0*(loc_ocn_R - 1.0), '% )'
       end SELECT
    END DO
  END SUBROUTINE sub_data_audit_diagnostics
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! REPLACE PARAMETER VALUES FOR CALIBRATION
  SUBROUTINE sub_init_bio_calibration()
    USE genie_util, ONLY:check_unit,check_iostat
    USE biogem_lib
    IMPLICIT NONE
    ! locals
    integer::ios
    ! *** LOAD PARAMETERS ***
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file='goin_BIOGEM',status='old',action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! skip over previously read in parameter section
    READ(unit=in,fmt='(1X)',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(unit=in,fmt='(1X)',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(unit=in,fmt='(1X)',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(unit=in,fmt='(1X)',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(unit=in,fmt='(1X)',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(unit=in,fmt='(1X)',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(unit=in,fmt='(1X)',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(unit=in,fmt='(1X)',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! organic particulate carbon cycling
    read(unit=in,fmt=*,iostat=ios) par_bio_k0_PO4            ! maximum PO4 uptake rate
    call check_iostat(ios,__LINE__,__FILE__)
    read(unit=in,fmt=*,iostat=ios) par_bio_c0_PO4            ! PO4 half-sat constant
    call check_iostat(ios,__LINE__,__FILE__)
    read(unit=in,fmt=*,iostat=ios) par_bio_remin_POC_frac2   ! partitioning of POC into fraction #2
    call check_iostat(ios,__LINE__,__FILE__)
    read(unit=in,fmt=*,iostat=ios) par_bio_remin_POC_eL1     ! e-folding depth of POC fraction #1
    call check_iostat(ios,__LINE__,__FILE__)
!!$    read(unit=in,fmt=*) par_bio_remin_POC_eL2     ! e-folding depth of POC fraction #2
    ! inorganic particulate carbon cycling
    read(unit=in,fmt=*,iostat=ios) par_bio_red_POC_CaCO3     ! CaCO3:POC export 'rain ratio' scalar
    call check_iostat(ios,__LINE__,__FILE__)
    read(unit=in,fmt=*,iostat=ios) par_bio_red_POC_CaCO3_pP  ! calcification rate power
    call check_iostat(ios,__LINE__,__FILE__)
    read(unit=in,fmt=*,iostat=ios) par_bio_remin_CaCO3_frac2 ! partitioning of CaCO3 into fraction #2
    call check_iostat(ios,__LINE__,__FILE__)
    read(unit=in,fmt=*,iostat=ios) par_bio_remin_CaCO3_eL1   ! e-folding depth of CaCO3 fraction #1
    call check_iostat(ios,__LINE__,__FILE__)
!!$    read(unit=in,fmt=*) par_bio_remin_CaCO3_eL2   ! e-folding depth of CaCO3 fraction #2
    ! inorganic carbon cycling
!!$    read(unit=in,fmt=*) par_bio_red_DOMfrac       ! DOM fraction of export production
!!$    read(unit=in,fmt=*) par_bio_remin_DOMlifetime ! lifetime of DOM
    close(unit=in)
    ! DISPLAY
    print*,' '
    print*,'replace parameter values for calibration:'
    print*,'par_bio_k0_PO4            : ',par_bio_k0_PO4
    print*,'par_bio_c0_PO4            : ',par_bio_c0_PO4
    print*,'par_bio_remin_POC_frac2   : ',par_bio_remin_POC_frac2
    print*,'par_bio_remin_POC_eL1     : ',par_bio_remin_POC_eL1
!!$    print*,'par_bio_remin_POC_eL2     : ',par_bio_remin_POC_eL2
    print*,'par_bio_red_POC_CaCO3     : ',par_bio_red_POC_CaCO3
    print*,'par_bio_red_POC_CaCO3_pP  : ',par_bio_red_POC_CaCO3_pP
    print*,'par_bio_remin_CaCO3_frac2 : ',par_bio_remin_CaCO3_frac2
    print*,'par_bio_remin_CaCO3_eL1   : ',par_bio_remin_CaCO3_eL1
!!$    print*,'par_bio_remin_CaCO3_eL2   : ',par_bio_remin_CaCO3_eL2
!!$    print*,'par_bio_red_DOMfrac       : ',par_bio_red_DOMfrac
!!$    print*,'par_bio_remin_DOMlifetime : ',par_bio_remin_DOMlifetime
    print*,' '
  end SUBROUTINE sub_init_bio_calibration
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE SOLAR CONSTANT FORCING
  SUBROUTINE sub_init_force_solconst()
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::loc_n_elements
    CHARACTER(len=255)::loc_filename
    real,DIMENSION(2)::loc_data_scale
    ! -------------------------------------------------------- !
    ! LOAD TIME-SERIES DATA
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! load forcing time series data
    loc_filename = TRIM(par_fordir_name)//'biogem_force_solconst_sig'//TRIM(string_data_ext)
    loc_data_scale(:) = 1.0
    CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_solconst_sig(:,:),loc_n_elements)
    ! -------------------------------------------------------- ! check that time elements == time-series data saving
    if (loc_n_elements /= par_data_save_sig_i) THEN
       CALL sub_report_error( &
            & 'biogem_data','sub_init_force_solconst','PLEASE ENSURE THAT THE SAME TIME ELEMENTS ARE PRESENT IN: '&
            & //TRIM(loc_filename)//'AS IN THE TIME-SERIES SPECIFICATION FILE', &
            & 'STOPPING', &
            & (/const_real_null/),.TRUE. &
            & )
    end if
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_init_force_solconst
  ! ****************************************************************************************************************************** !


!!$  ! ****************************************************************************************************************************** !
!!$  ! INITIALIZE SOLAR CONSTANT FORCING
!!$  SUBROUTINE sub_init_force_solconst()
!!$    ! local variables
!!$    CHARACTER(len=255)::loc_filename
!!$    INTEGER::loc_n_elements
!!$    real,DIMENSION(2)::loc_data_scale
!!$    ! load forcing time series data
!!$    loc_filename = TRIM(par_fordir_name)//'biogem_force_solconst_sig'//TRIM(string_data_ext)
!!$    loc_data_scale(:) = 1.0
!!$    CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_solconst_sig(:,:),loc_n_elements)
!!$    ! check that time elements are identical to those for time-series data saving
!!$    if (size(force_solconst_sig(1,:)) /= size(par_data_save_sig(:))) THEN
!!$       CALL sub_report_error( &
!!$            & 'biogem_data','sub_init_force_solconst','PLEASE ENSURE THAT THE SAME TIME ELEMENTS ARE PRESENT IN: '&
!!$            & //TRIM(loc_filename)//'AS IN THE TIME-SERIES SPECIFICATION FILE', &
!!$            & 'STOPPING', &
!!$            & (/const_real_null/),.TRUE. &
!!$            & )
!!$    end if
!!$  END SUBROUTINE sub_init_force_solconst
!!$  ! ****************************************************************************************************************************** !


END MODULE biogem_data
