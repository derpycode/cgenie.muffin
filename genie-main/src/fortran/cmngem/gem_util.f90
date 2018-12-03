! ******************************************************************************************************************************** !
! gem_util.f90
! GEochemistry Model
! UTILITIES MODULE
! ******************************************************************************************************************************** !


MODULE gem_util


  USE gem_cmn
  IMPLICIT NONE
  SAVE


CONTAINS



  ! ****************************************************************************************************************************** !
  ! *** TRACER DEFINITIONS ******************************************************************************************************* !
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DEFINE RELATIONSHIPS BETWEEN TRACERS
  SUBROUTINE sub_def_tracerrelationships()
    ! OCEAN-ATMOSPHERE
    ! (compositional) relational operator for converting between dissolved and gaseous forms
    ! convert gaseous species -> dissolved
    ! NOTE: populate unused elements with zero
    ! ### INSERT DEFINITIONS FOR ADDITIONAL OCN/ATM RELATIONSHIPS HERE ########################################################### !
    conv_atm_ocn(:,:) = 0.0
    conv_atm_ocn(io_DIC,ia_pCO2)         = 1.0
    conv_atm_ocn(io_DIC_13C,ia_pCO2_13C) = 1.0
    conv_atm_ocn(io_DIC_14C,ia_pCO2_14C) = 1.0
    conv_atm_ocn(io_O2,ia_pO2)           = 1.0
    conv_atm_ocn(io_O2_18O,ia_pO2_18O)   = 1.0
    conv_atm_ocn(io_N2,ia_pN2)           = 1.0
    conv_atm_ocn(io_N2_15N,ia_pN2_15N)   = 1.0
    conv_atm_ocn(io_CH4,ia_pCH4)         = 1.0
    conv_atm_ocn(io_CH4_13C,ia_pCH4_13C) = 1.0
    conv_atm_ocn(io_CH4_14C,ia_pCH4_14C) = 1.0
    conv_atm_ocn(io_SF6,ia_pSF6)         = 1.0
    conv_atm_ocn(io_N2O,ia_pN2O)         = 1.0
    conv_atm_ocn(io_N2O_15N,ia_pN2O_15N) = 1.0
    conv_atm_ocn(io_H2S,ia_pH2S)         = 1.0
    conv_atm_ocn(io_H2S_34S,ia_pH2S_34S) = 1.0
    conv_atm_ocn(io_CFC11,ia_pCFC11)     = 1.0
    conv_atm_ocn(io_CFC12,ia_pCFC12)     = 1.0
    ! convert dissolved species -> gaseous
    conv_ocn_atm(:,:) = 0.0
    conv_ocn_atm(ia_pCO2,io_DIC)         = 1.0/conv_atm_ocn(io_DIC,ia_pCO2)
    conv_ocn_atm(ia_pCO2_13C,io_DIC_13C) = 1.0/conv_atm_ocn(io_DIC_13C,ia_pCO2_13C)
    conv_ocn_atm(ia_pCO2_14C,io_DIC_14C) = 1.0/conv_atm_ocn(io_DIC_14C,ia_pCO2_14C)
    conv_ocn_atm(ia_pO2,io_O2)           = 1.0/conv_atm_ocn(io_O2,ia_pO2)
    conv_ocn_atm(ia_pO2_18O,io_O2_18O)   = 1.0/conv_atm_ocn(io_O2_18O,ia_pO2_18O)
    conv_ocn_atm(ia_pN2,io_N2)           = 1.0/conv_atm_ocn(io_N2,ia_pN2)
    conv_ocn_atm(ia_pN2_15N,io_N2_15N)   = 1.0/conv_atm_ocn(io_N2_15N,ia_pN2_15N)
    conv_ocn_atm(ia_pCH4,io_CH4)         = 1.0/conv_atm_ocn(io_CH4,ia_pCH4)
    conv_ocn_atm(ia_pCH4_13C,io_CH4_13C) = 1.0/conv_atm_ocn(io_CH4_13C,ia_pCH4_13C)
    conv_ocn_atm(ia_pCH4_14C,io_CH4_14C) = 1.0/conv_atm_ocn(io_CH4_14C,ia_pCH4_14C)
    conv_ocn_atm(ia_pSF6,io_SF6)         = 1.0/conv_atm_ocn(io_SF6,ia_pSF6)
    conv_ocn_atm(ia_pN2O,io_N2O)         = 1.0/conv_atm_ocn(io_N2O,ia_pN2O)
    conv_ocn_atm(ia_pN2O_15N,io_N2O_15N) = 1.0/conv_atm_ocn(io_N2O_15N,ia_pN2O_15N)
    conv_ocn_atm(ia_pH2S,io_H2S)         = 1.0/conv_atm_ocn(io_H2S,ia_pH2S)
    conv_ocn_atm(ia_pH2S_34S,io_H2S_34S) = 1.0/conv_atm_ocn(io_H2S_34S,ia_pH2S_34S)
    conv_ocn_atm(ia_pCFC11,io_CFC11)     = 1.0/conv_atm_ocn(io_CFC11,ia_pCFC11)
    conv_ocn_atm(ia_pCFC12,io_CFC12)     = 1.0/conv_atm_ocn(io_CFC12,ia_pCFC12)
    ! ############################################################################################################################ !
    ! OCEAN-SEDIMENT
    ! (compositional) relational operator for converting between dissolved and particulate forms
    ! NOTE: populate unused elements with zero
    ! NOTE: SOME VALUES ARE MODIFIED SUBSEQUENTLY (esp Redfield ratios)
    ! NOTE: foram tracers undergo no mass transformation
    ! ### INSERT DEFINITIONS FOR ADDITIONAL OCN/SED RELATIONSHIPS HERE ########################################################### !
    ! convert solid species -> dissolved
    conv_sed_ocn(:,:) = 0.0
    conv_sed_ocn(io_DIC,is_POC)              = 1.0
    conv_sed_ocn(io_DIC_13C,is_POC_13C)      = 1.0
    conv_sed_ocn(io_DIC_14C,is_POC_14C)      = 1.0
    conv_sed_ocn(io_PO4,is_POP)              = 1.0
    conv_sed_ocn(io_NO3,is_PON)              = 1.0
    conv_sed_ocn(io_NO3_15N,is_PON_15N)      = 1.0
    conv_sed_ocn(io_ALK,is_PON)              = -1.0
    conv_sed_ocn(io_Fe,is_POFe)              = 1.0
    conv_sed_ocn(io_TDFe,is_POFe)             = 1.0
    conv_sed_ocn(io_O2,is_POC)               = -1.0
    conv_sed_ocn(io_O2,is_PON)               = -1.0
    conv_sed_ocn(io_O2,is_POP)               = -1.0
    conv_sed_ocn(io_Cd,is_POCd)              = 1.0
    conv_sed_ocn(io_Cd_114Cd,is_POCd_114Cd)  = 1.0
    conv_sed_ocn(io_I,is_POI)                = 1.0
    conv_sed_ocn(io_DIC,is_CaCO3)            = 1.0
    conv_sed_ocn(io_DIC_13C,is_CaCO3_13C)    = 1.0
    conv_sed_ocn(io_DIC_14C,is_CaCO3_14C)    = 1.0
    conv_sed_ocn(io_ALK,is_CaCO3)            = 2.0
    conv_sed_ocn(io_Ca,is_CaCO3)             = 1.0
    conv_sed_ocn(io_Ca_44Ca,is_CaCO3_44Ca)   = 1.0
    conv_sed_ocn(io_Cd,is_CdCO3)             = 1.0
    conv_sed_ocn(io_Cd_114Cd,is_CdCO3_114Cd) = 1.0
    conv_sed_ocn(io_Li,is_LiCO3)             = 1.0
    conv_sed_ocn(io_Li_7Li,is_LiCO3_7Li)     = 1.0
    conv_sed_ocn(io_Li,is_detLi)             = 1.0
    conv_sed_ocn(io_Li_7Li,is_detLi_7Li)     = 1.0
    conv_sed_ocn(io_SiO2,is_opal)            = 1.0
    conv_sed_ocn(io_SiO2_30Si,is_opal_30Si)  = 1.0
    conv_sed_ocn(io_Fe,is_POM_Fe)            = 1.0
    conv_sed_ocn(io_Fe,is_CaCO3_Fe)          = 1.0
    conv_sed_ocn(io_Fe,is_opal_Fe)           = 1.0
    conv_sed_ocn(io_Fe,is_det_Fe)            = 1.0
    conv_sed_ocn(io_TDFe,is_POM_Fe)            = 1.0
    conv_sed_ocn(io_TDFe,is_CaCO3_Fe)          = 1.0
    conv_sed_ocn(io_TDFe,is_opal_Fe)           = 1.0
    conv_sed_ocn(io_TDFe,is_det_Fe)            = 1.0
    conv_sed_ocn(io_231Pa,is_POM_231Pa)      = 1.0
    conv_sed_ocn(io_231Pa,is_CaCO3_231Pa)    = 1.0
    conv_sed_ocn(io_231Pa,is_opal_231Pa)     = 1.0
    conv_sed_ocn(io_231Pa,is_det_231Pa)      = 1.0
    conv_sed_ocn(io_230Th,is_POM_230Th)      = 1.0
    conv_sed_ocn(io_230Th,is_CaCO3_230Th)    = 1.0
    conv_sed_ocn(io_230Th,is_opal_230Th)     = 1.0
    conv_sed_ocn(io_230Th,is_det_230Th)      = 1.0
    conv_sed_ocn(io_Nd,is_POM_Nd)            = 1.0
    conv_sed_ocn(io_Nd,is_CaCO3_Nd)          = 1.0
    conv_sed_ocn(io_Nd,is_opal_Nd)           = 1.0
    conv_sed_ocn(io_Nd,is_det_Nd)            = 1.0
    conv_sed_ocn(io_Nd_144Nd,is_POM_Nd_144Nd)   = 1.0
    conv_sed_ocn(io_Nd_144Nd,is_CaCO3_Nd_144Nd) = 1.0
    conv_sed_ocn(io_Nd_144Nd,is_opal_Nd_144Nd)  = 1.0
    conv_sed_ocn(io_Nd_144Nd,is_det_Nd_144Nd)   = 1.0
    conv_sed_ocn(io_H2S,is_POM_S)               = 1.0
    conv_sed_ocn(io_H2S_34S,is_POM_S_34S)       = 1.0
    conv_sed_ocn(io_Mo,is_POM_MoS2)             = 1.0
    conv_sed_ocn(io_H2S,is_POM_MoS2)            = 2.0
    conv_sed_ocn(io_Mo,is_CaCO3_MoS2)           = 1.0
    conv_sed_ocn(io_H2S,is_CaCO3_MoS2)          = 2.0
    conv_sed_ocn(io_Mo,is_opal_MoS2)            = 1.0
    conv_sed_ocn(io_H2S,is_opal_MoS2)           = 2.0
    conv_sed_ocn(io_Mo,is_det_MoS2)             = 1.0
    conv_sed_ocn(io_H2S,is_det_MoS2)            = 2.0
    conv_sed_ocn(io_Mo_98Mo,is_POM_MoS2_98Mo)   = 1.0
    conv_sed_ocn(io_H2S_34S,is_POM_MoS2_34S)    = 2.0
    conv_sed_ocn(io_Mo_98Mo,is_CaCO3_MoS2_98Mo) = 1.0
    conv_sed_ocn(io_H2S_34S,is_CaCO3_MoS2_34S)  = 2.0
    conv_sed_ocn(io_Mo_98Mo,is_opal_MoS2_98Mo)  = 1.0
    conv_sed_ocn(io_H2S_34S,is_opal_MoS2_34S)   = 2.0
    conv_sed_ocn(io_Mo_98Mo,is_det_MoS2_98Mo)   = 1.0
    conv_sed_ocn(io_H2S_34S,is_det_MoS2_34S)    = 2.0
    conv_sed_ocn(io_Ba,is_POBa)                 = 1.0
    conv_sed_ocn(io_Ba_138Ba,is_POBa_138Ba)     = 1.0
    conv_sed_ocn(io_Ba,is_POM_BaSO4)               = 1.0
    conv_sed_ocn(io_Ba_138Ba,is_POM_BaSO4_138Ba)   = 1.0
    conv_sed_ocn(io_Sr,is_SrCO3)                = 1.0
    conv_sed_ocn(io_Sr_87Sr,is_SrCO3_87Sr)      = 1.0
    conv_sed_ocn(io_Sr_88Sr,is_SrCO3_88Sr)      = 1.0
    ! convert dissolved species -> solid
    conv_ocn_sed(:,:) = 0.0
    conv_ocn_sed(is_POC,io_DIC)              = 1.0/conv_sed_ocn(io_DIC,is_POC)
    conv_ocn_sed(is_POC_13C,io_DIC_13C)      = 1.0/conv_sed_ocn(io_DIC_13C,is_POC_13C)
    conv_ocn_sed(is_POC_14C,io_DIC_14C)      = 1.0/conv_sed_ocn(io_DIC_14C,is_POC_14C)
    conv_ocn_sed(is_POP,io_PO4)              = 1.0/conv_sed_ocn(io_PO4,is_POP)
    conv_ocn_sed(is_PON,io_NO3)              = 1.0/conv_sed_ocn(io_NO3,is_PON)
    conv_ocn_sed(is_PON_15N,io_NO3_15N)      = 1.0/conv_sed_ocn(io_NO3_15N,is_PON_15N)
    conv_ocn_sed(is_PON,io_ALK)              = 1.0/conv_sed_ocn(io_ALK,is_PON)
    conv_ocn_sed(is_POFe,io_Fe)              = 1.0/conv_sed_ocn(io_Fe,is_POFe)
    conv_ocn_sed(is_POFe,io_TDFe)              = 1.0/conv_sed_ocn(io_TDFe,is_POFe)
    conv_ocn_sed(is_POC,io_O2)               = 1.0/conv_sed_ocn(io_O2,is_POC)
    conv_ocn_sed(is_POP,io_O2)               = 1.0/conv_sed_ocn(io_O2,is_POP)
    conv_ocn_sed(is_PON,io_O2)               = 1.0/conv_sed_ocn(io_O2,is_PON)
    conv_ocn_sed(is_POCd,io_Cd)              = 1.0/conv_sed_ocn(io_Cd,is_POCd)
    conv_ocn_sed(is_POCd_114Cd,io_Cd_114Cd)  = 1.0/conv_sed_ocn(io_Cd_114Cd,is_POCd_114Cd)
    conv_ocn_sed(is_POI,io_IO3)                = 1.0
    conv_ocn_sed(is_POI,io_O2)                = -(3.0/2.0)
    conv_ocn_sed(is_CaCO3,io_DIC)            = 1.0/conv_sed_ocn(io_DIC,is_CaCO3)
    conv_ocn_sed(is_CaCO3_13C,io_DIC_13C)    = 1.0/conv_sed_ocn(io_DIC_13C,is_CaCO3_13C)
    conv_ocn_sed(is_CaCO3_14C,io_DIC_14C)    = 1.0/conv_sed_ocn(io_DIC_14C,is_CaCO3_14C)
    conv_ocn_sed(is_CaCO3,io_ALK)            = 1.0/conv_sed_ocn(io_ALK,is_CaCO3)
    conv_ocn_sed(is_CaCO3,io_Ca)             = 1.0/conv_sed_ocn(io_Ca,is_CaCO3)
    conv_ocn_sed(is_CaCO3_44Ca,io_Ca_44Ca)   = 1.0/conv_sed_ocn(io_Ca_44Ca,is_CaCO3_44Ca)
    conv_ocn_sed(is_CdCO3,io_Cd)             = 1.0/conv_sed_ocn(io_Cd,is_CdCO3)
    conv_ocn_sed(is_CdCO3_114Cd,io_Cd_114Cd) = 1.0/conv_sed_ocn(io_Cd_114Cd,is_CdCO3_114Cd)
    conv_ocn_sed(is_LiCO3,io_Li)             = 1.0/conv_sed_ocn(io_Li,is_LiCO3)
    conv_ocn_sed(is_LiCO3_7Li,io_Li_7Li)     = 1.0/conv_sed_ocn(io_Li_7Li,is_LiCO3_7Li)
    conv_ocn_sed(is_detLi,io_Li)             = 1.0/conv_sed_ocn(io_Li,is_detLi)
    conv_ocn_sed(is_detLi_7Li,io_Li_7Li)     = 1.0/conv_sed_ocn(io_Li_7Li,is_detLi_7Li)
    conv_ocn_sed(is_opal,io_SiO2)            = 1.0/conv_sed_ocn(io_SiO2,is_opal)
    conv_ocn_sed(is_opal_30Si,io_SiO2_30Si)  = 1.0/conv_sed_ocn(io_SiO2_30Si,is_opal_30Si)
    conv_ocn_sed(is_POM_Fe,io_Fe)            = 1.0/conv_sed_ocn(io_Fe,is_POM_Fe)
    conv_ocn_sed(is_CaCO3_Fe,io_Fe)          = 1.0/conv_sed_ocn(io_Fe,is_CaCO3_Fe)
    conv_ocn_sed(is_opal_Fe,io_Fe)           = 1.0/conv_sed_ocn(io_Fe,is_opal_Fe)
    conv_ocn_sed(is_det_Fe,io_Fe)            = 1.0/conv_sed_ocn(io_Fe,is_det_Fe)
    conv_ocn_sed(is_POM_Fe,io_TDFe)            = 1.0/conv_sed_ocn(io_TDFe,is_POM_Fe)
    conv_ocn_sed(is_CaCO3_Fe,io_TDFe)          = 1.0/conv_sed_ocn(io_TDFe,is_CaCO3_Fe)
    conv_ocn_sed(is_opal_Fe,io_TDFe)           = 1.0/conv_sed_ocn(io_TDFe,is_opal_Fe)
    conv_ocn_sed(is_det_Fe,io_TDFe)            = 1.0/conv_sed_ocn(io_TDFe,is_det_Fe)
    conv_ocn_sed(is_POM_231Pa,io_231Pa)      = 1.0/conv_sed_ocn(io_231Pa,is_POM_231Pa)
    conv_ocn_sed(is_CaCO3_231Pa,io_231Pa)    = 1.0/conv_sed_ocn(io_231Pa,is_CaCO3_231Pa)
    conv_ocn_sed(is_opal_231Pa,io_231Pa)     = 1.0/conv_sed_ocn(io_231Pa,is_opal_231Pa)
    conv_ocn_sed(is_det_231Pa,io_231Pa)      = 1.0/conv_sed_ocn(io_231Pa,is_det_231Pa)
    conv_ocn_sed(is_POM_230Th,io_230Th)      = 1.0/conv_sed_ocn(io_230Th,is_POM_230Th)
    conv_ocn_sed(is_CaCO3_230Th,io_230Th)    = 1.0/conv_sed_ocn(io_230Th,is_CaCO3_230Th)
    conv_ocn_sed(is_opal_230Th,io_230Th)     = 1.0/conv_sed_ocn(io_230Th,is_opal_230Th)
    conv_ocn_sed(is_det_230Th,io_230Th)      = 1.0/conv_sed_ocn(io_230Th,is_det_230Th)
    conv_ocn_sed(is_POM_Nd,io_Nd)            = 1.0/conv_sed_ocn(io_Nd,is_POM_Nd)
    conv_ocn_sed(is_CaCO3_Nd,io_Nd)          = 1.0/conv_sed_ocn(io_Nd,is_CaCO3_Nd)
    conv_ocn_sed(is_opal_Nd,io_Nd)           = 1.0/conv_sed_ocn(io_Nd,is_opal_Nd)
    conv_ocn_sed(is_det_Nd,io_Nd)            = 1.0/conv_sed_ocn(io_Nd,is_det_Nd)
    conv_ocn_sed(is_POM_Nd_144Nd,io_Nd_144Nd)   = 1.0/conv_sed_ocn(io_Nd_144Nd,is_POM_Nd_144Nd)
    conv_ocn_sed(is_CaCO3_Nd_144Nd,io_Nd_144Nd) = 1.0/conv_sed_ocn(io_Nd_144Nd,is_CaCO3_Nd_144Nd)
    conv_ocn_sed(is_opal_Nd_144Nd,io_Nd_144Nd)  = 1.0/conv_sed_ocn(io_Nd_144Nd,is_opal_Nd_144Nd)
    conv_ocn_sed(is_det_Nd_144Nd,io_Nd_144Nd)   = 1.0/conv_sed_ocn(io_Nd_144Nd,is_det_Nd_144Nd)
    conv_ocn_sed(is_POM_S,io_H2S)               = 1.0/conv_sed_ocn(io_H2S,is_POM_S)
    conv_ocn_sed(is_POM_S_34S,io_H2S_34S)       = 1.0/conv_sed_ocn(io_H2S_34S,is_POM_S_34S)
    conv_ocn_sed(is_POM_MoS2,io_Mo) = 1.0/conv_sed_ocn(io_Mo,is_POM_MoS2)
    conv_ocn_sed(is_POM_MoS2,io_H2S) = 1.0/conv_sed_ocn(io_H2S,is_POM_MoS2)
    conv_ocn_sed(is_CaCO3_MoS2,io_Mo) = 1.0/conv_sed_ocn(io_Mo,is_CaCO3_MoS2)
    conv_ocn_sed(is_CaCO3_MoS2,io_H2S) = 1.0/conv_sed_ocn(io_H2S,is_CaCO3_MoS2)
    conv_ocn_sed(is_opal_MoS2,io_Mo) = 1.0/conv_sed_ocn(io_Mo,is_opal_MoS2)
    conv_ocn_sed(is_opal_MoS2,io_H2S) = 1.0/conv_sed_ocn(io_H2S,is_opal_MoS2)
    conv_ocn_sed(is_det_MoS2,io_Mo) = 1.0/conv_sed_ocn(io_Mo,is_det_MoS2)
    conv_ocn_sed(is_det_MoS2,io_H2S) = 1.0/conv_sed_ocn(io_H2S,is_det_MoS2)
    conv_ocn_sed(is_POM_MoS2_98Mo,io_Mo_98Mo) = 1.0/conv_sed_ocn(io_Mo_98Mo,is_POM_MoS2_98Mo)
    conv_ocn_sed(is_POM_MoS2_34S,io_H2S_34S) = 1.0/conv_sed_ocn(io_H2S_34S,is_POM_MoS2_34S)
    conv_ocn_sed(is_CaCO3_MoS2_98Mo,io_Mo_98Mo) = 1.0/conv_sed_ocn(io_Mo_98Mo,is_CaCO3_MoS2_98Mo)
    conv_ocn_sed(is_CaCO3_MoS2_34S,io_H2S_34S) = 1.0/conv_sed_ocn(io_H2S_34S,is_CaCO3_MoS2_34S)
    conv_ocn_sed(is_opal_MoS2_98Mo,io_Mo_98Mo) = 1.0/conv_sed_ocn(io_Mo_98Mo,is_opal_MoS2_98Mo)
    conv_ocn_sed(is_opal_MoS2_34S,io_H2S_34S) = 1.0/conv_sed_ocn(io_H2S_34S,is_opal_MoS2_34S)
    conv_ocn_sed(is_det_MoS2_98Mo,io_Mo_98Mo) = 1.0/conv_sed_ocn(io_Mo_98Mo,is_det_MoS2_98Mo)
    conv_ocn_sed(is_det_MoS2_34S,io_H2S_34S) = 1.0/conv_sed_ocn(io_H2S_34S,is_det_MoS2_34S)
    conv_ocn_sed(is_POBa,io_Ba)                 = 1.0/conv_sed_ocn(io_Ba,is_POBa)
    conv_ocn_sed(is_POBa_138Ba,io_Ba_138Ba)     = 1.0/conv_sed_ocn(io_Ba_138Ba,is_POBa_138Ba)
    conv_ocn_sed(is_POM_BaSO4,io_Ba)               = 1.0/conv_sed_ocn(io_Ba,is_POM_BaSO4)
    conv_ocn_sed(is_POM_BaSO4_138Ba,io_Ba_138Ba)   = 1.0/conv_sed_ocn(io_Ba_138Ba,is_POM_BaSO4_138Ba)
    conv_ocn_sed(is_SrCO3,io_Sr)                = 1.0/conv_sed_ocn(io_Sr,is_SrCO3)
    conv_ocn_sed(is_SrCO3_87Sr,io_Sr_87Sr)      = 1.0/conv_sed_ocn(io_Sr_87Sr,is_SrCO3_87Sr)
    conv_ocn_sed(is_SrCO3_88Sr,io_Sr_88Sr)      = 1.0/conv_sed_ocn(io_Sr_88Sr,is_SrCO3_88Sr)
    ! ############################################################################################################################ !
    ! DISSOLVED-PARTICULATE
    ! (compositional) relational operator for converting between DOM and POM
    ! convert POM -> DOM
    ! NOTE: populate unused elements with zero
    ! ### INSERT DEFINITIONS FOR ADDITIONAL DOM/POM RELATIONSHIPS HERE ########################################################### !
    conv_POM_DOM(:,:) = 0.0
    conv_POM_DOM(io_DOM_C,is_POC)               = 1.0
    conv_POM_DOM(io_DOM_C_13C,is_POC_13C)       = 1.0
    conv_POM_DOM(io_DOM_C_14C,is_POC_14C)       = 1.0
    conv_POM_DOM(io_DOM_N,is_PON)               = 1.0
    conv_POM_DOM(io_DOM_N_15N,is_PON_15N)       = 1.0
    conv_POM_DOM(io_DOM_P,is_POP)               = 1.0
    conv_POM_DOM(io_DOM_Cd,is_POCd)             = 1.0
    conv_POM_DOM(io_DOM_Cd_114Cd,is_POCd_114Cd) = 1.0
    conv_POM_DOM(io_DOM_Fe,is_POFe)             = 1.0
    conv_POM_DOM(io_DOM_I,is_POI)               = 1.0
    ! convert DOM -> POM
    conv_DOM_POM(:,:) = 0.0
    conv_DOM_POM(is_POC,io_DOM_C)               = 1.0/conv_POM_DOM(io_DOM_C,is_POC)
    conv_DOM_POM(is_POC_13C,io_DOM_C_13C)       = 1.0/conv_POM_DOM(io_DOM_C_13C,is_POC_13C)
    conv_DOM_POM(is_POC_14C,io_DOM_C_14C)       = 1.0/conv_POM_DOM(io_DOM_C_14C,is_POC_14C)
    conv_DOM_POM(is_PON,io_DOM_N)               = 1.0/conv_POM_DOM(io_DOM_N,is_PON)
    conv_DOM_POM(is_PON_15N,io_DOM_N_15N)       = 1.0/conv_POM_DOM(io_DOM_N_15N,is_PON_15N)
    conv_DOM_POM(is_POP,io_DOM_P)               = 1.0/conv_POM_DOM(io_DOM_P,is_POP)
    conv_DOM_POM(is_POCd,io_DOM_Cd)             = 1.0/conv_POM_DOM(io_DOM_Cd,is_POCd)
    conv_DOM_POM(is_POCd_114Cd,io_DOM_Cd_114Cd) = 1.0/conv_POM_DOM(io_DOM_Cd_114Cd,is_POCd_114Cd)
    conv_DOM_POM(is_POFe,io_DOM_Fe)             = 1.0/conv_POM_DOM(io_DOM_Fe,is_POFe)
    conv_DOM_POM(is_POI,io_DOM_I)               = 1.0/conv_POM_DOM(io_DOM_I,is_POI)
    ! ############################################################################################################################ !
    ! RDISSOLVED-PARTICULATE
    ! (compositional) relational operator for converting between RDOM and POM
    ! convert POM -> RDOM
    ! NOTE: populate unused elements with zero
    ! ### INSERT DEFINITIONS FOR ADDITIONAL DOM/POM RELATIONSHIPS HERE ########################################################### !
    conv_POM_RDOM(:,:) = 0.0
    conv_POM_RDOM(io_RDOM_C,is_POC)               = 1.0
    conv_POM_RDOM(io_RDOM_C_13C,is_POC_13C)       = 1.0
    conv_POM_RDOM(io_RDOM_C_14C,is_POC_14C)       = 1.0
    conv_POM_RDOM(io_RDOM_N,is_PON)               = 1.0
    conv_POM_RDOM(io_RDOM_N_15N,is_PON_15N)       = 1.0
    conv_POM_RDOM(io_RDOM_P,is_POP)               = 1.0
    conv_POM_RDOM(io_RDOM_Cd,is_POCd)             = 1.0
    conv_POM_RDOM(io_RDOM_Cd_114Cd,is_POCd_114Cd) = 1.0
    conv_POM_RDOM(io_RDOM_Fe,is_POFe)             = 1.0
    conv_POM_RDOM(io_RDOM_I,is_POI)               = 1.0
    ! convert RDOM -> POM
    conv_RDOM_POM(:,:) = 0.0
    conv_RDOM_POM(is_POC,io_RDOM_C)               = 1.0/conv_POM_RDOM(io_RDOM_C,is_POC)
    conv_RDOM_POM(is_POC_13C,io_RDOM_C_13C)       = 1.0/conv_POM_RDOM(io_RDOM_C_13C,is_POC_13C)
    conv_RDOM_POM(is_POC_14C,io_RDOM_C_14C)       = 1.0/conv_POM_RDOM(io_RDOM_C_14C,is_POC_14C)
    conv_RDOM_POM(is_PON,io_RDOM_N)               = 1.0/conv_POM_RDOM(io_RDOM_N,is_PON)
    conv_RDOM_POM(is_PON_15N,io_RDOM_N_15N)       = 1.0/conv_POM_RDOM(io_RDOM_N_15N,is_PON_15N)
    conv_RDOM_POM(is_POP,io_RDOM_P)               = 1.0/conv_POM_RDOM(io_RDOM_P,is_POP)
    conv_RDOM_POM(is_POCd,io_RDOM_Cd)             = 1.0/conv_POM_RDOM(io_RDOM_Cd,is_POCd)
    conv_RDOM_POM(is_POCd_114Cd,io_RDOM_Cd_114Cd) = 1.0/conv_POM_RDOM(io_RDOM_Cd_114Cd,is_POCd_114Cd)
    conv_RDOM_POM(is_POFe,io_RDOM_Fe)             = 1.0/conv_POM_RDOM(io_RDOM_Fe,is_POFe)
    conv_RDOM_POM(is_POI,io_RDOM_I)               = 1.0/conv_POM_RDOM(io_RDOM_I,is_POI)
  END SUBROUTINE sub_def_tracerrelationships
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE DECAY CONSTANT ARRAYS
  SUBROUTINE sub_def_tracer_decay()
    ! set default array values
    const_lambda_atm(:) = 0.0
    const_lambda_ocn(:) = 0.0
    const_lambda_sed(:) = 0.0
    ! atm tracers
    const_lambda_atm(ia_pCO2_14C) = const_lambda_14C
    const_lambda_atm(ia_pCH4_14C) = const_lambda_14C
    ! ocn tracers
    const_lambda_ocn(io_DIC_14C) = const_lambda_14C
    const_lambda_ocn(io_DOM_C_14C) = const_lambda_14C
    const_lambda_ocn(io_RDOM_C_14C) = const_lambda_14C
    const_lambda_ocn(io_CH4_14C) = const_lambda_14C
    const_lambda_ocn(io_231Pa) = const_lambda_231Pa
    const_lambda_ocn(io_230Th) = const_lambda_230Th
    ! sed tracers
    const_lambda_sed(is_POC_14C) = const_lambda_14C
    const_lambda_sed(is_CaCO3_14C) = const_lambda_14C
    const_lambda_sed(is_foram_p_14C) = const_lambda_14C
    const_lambda_sed(is_foram_b_14C) = const_lambda_14C
    const_lambda_sed(is_POM_231Pa) = const_lambda_231Pa
    const_lambda_sed(is_POM_230Th) = const_lambda_230Th
    const_lambda_sed(is_CaCO3_231Pa) = const_lambda_231Pa
    const_lambda_sed(is_CaCO3_230Th) = const_lambda_230Th
    const_lambda_sed(is_det_231Pa) = const_lambda_231Pa
    const_lambda_sed(is_det_230Th) = const_lambda_230Th
    const_lambda_sed(is_opal_231Pa) = const_lambda_231Pa
    const_lambda_sed(is_opal_230Th) = const_lambda_230Th
  END SUBROUTINE sub_def_tracer_decay
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** I/O ROUTINES ************************************************************************************************************* !
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FIND INDEX OF STRING
  FUNCTION fun_find_str_i(dum_str,dum_data)
    ! common blocks
    IMPLICIT NONE
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    INTEGER::fun_find_str_i
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    CHARACTER(len=*),INTENT(in)::dum_str
    CHARACTER(len=*),DIMENSION(:),INTENT(in)::dum_data
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::i,loc_imax
    ! -------------------------------------------------------- !
    ! FIND i
    ! -------------------------------------------------------- !
    loc_imax = size(dum_data)
    i = 0
    do i=1,loc_imax
       if (dum_str == dum_data(i)) exit 
    end do
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
    fun_find_str_i = i
    ! -------------------------------------------------------- !
  END FUNCTION fun_find_str_i
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT AN INTEGER NUMBER INTO AN n-CHARACTER STRING
  ! NOTE: errors are generated here if the number of digits 'dum_n' is too small for the passed integer
  FUNCTION fun_conv_num_char_n(dum_n,dum_integer)
    IMPLICIT NONE
    ! dummy valiables
    INTEGER,INTENT(in)::dum_n
    INTEGER,INTENT(in)::dum_integer
    ! result variable
    CHARACTER(len=dum_n)::fun_conv_num_char_n
    ! local variables
    INTEGER::n
    INTEGER::loc_integer,loc_digit
    real::loc_real
    ! check integer value
    IF (dum_integer >= INT(10**dum_n)) THEN
       CALL sub_report_error(                                                   &
            & 'gem_util','fun_conv_num_char_n',                                 &
            & 'dum_integer >= int(10**dum_n)',                                  &
            & 'STOPPING',                                                       &
            & (/REAL(dum_integer),REAL(INT(10**dum_n))/),.TRUE.                 &
            & )
    END IF
    ! convert to string
    ! NOTE: when extracting an integer digit, add on a fraction to the real number before integer conversion
    !       to ensure that the integer part is correctly extracted
    loc_integer = dum_integer
    DO n=dum_n,1,-1
       loc_real = REAL(loc_integer)
       loc_digit = INT(loc_real*10.0**(-(n-1)) + conv_s_yr)
       WRITE(fun_conv_num_char_n(dum_n-(n-1):dum_n-(n-1)),'(i1)') loc_digit
       loc_integer = loc_integer - INT(REAL(loc_digit)*10.0**(n-1))
    END DO
  END FUNCTION fun_conv_num_char_n
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CHECK DATA FILE FORMAT
  SUBROUTINE sub_check_fileformat(dum_filename,dum_n_elements,dum_n_start)
    IMPLICIT NONE
    ! dummy arguments
    CHARACTER(LEN=*),INTENT(in)::dum_filename
    INTEGER,INTENT(out)::dum_n_elements,dum_n_start
    ! local variables
    INTEGER::n
    CHARACTER(LEN=15)::loc_string_start
    CHARACTER(LEN=13)::loc_string_end
    integer::ios
    ! initialize local variables
    dum_n_elements = 0
    dum_n_start    = 0
    ! *** check that the number of lines of data in the file is correct ***
    ! NOTE: the position of the '-END-OF-DATA-' marker is checked
    ! open file pipe
    OPEN(unit=in,status='old',file=dum_filename,action='read',IOSTAT=ios)
    If (ios /= 0) then
       CALL sub_report_error(                                   &
            & 'gem_util','fun_sub_check_fileformat',            &
            & 'File <'//trim(dum_filename)//'> does not exist', &
            & 'STOPPING',                                       &
            & (/const_real_zero/),.true.                        &
            & )
    else
       ! check for start-of-file tag
       n = 0
       DO
          READ(unit=in,fmt='(A15)') loc_string_start
          n = n + 1
          IF (loc_string_start == '-START-OF-DATA-') THEN
             dum_n_start = n
             EXIT
          END IF
          IF (n > 32767) THEN
             CALL sub_report_error( &
                  & 'biogem_lib','check_fileformat','missing -START-OF-DATA- tag in '//TRIM(dum_filename), &
                  & 'STOPPING', &
                  & (/REAL(n)/),.TRUE. &
                  & )
          END IF
       END DO
       ! count number of lines of data and check for '-END-OF-DATA-' end of file tag
       n = 0
       DO
          READ(unit=in,fmt='(A13)') loc_string_end
          IF (loc_string_end == '-END-OF-DATA-') THEN
             dum_n_elements = n
             EXIT
          ELSE
             n = n + 1
          END IF
          IF (n > 32767) THEN
             CALL sub_report_error( &
                  & 'biogem_lib','check_fileformat','missing -END-OF-DATA- tag in '//TRIM(dum_filename), &
                  & 'STOPPING', &
                  & (/REAL(n)/),.TRUE. &
                  & )
          END IF
       END DO
    end if
    ! close filepipe
    CLOSE(unit=in)
  END SUBROUTINE sub_check_fileformat
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! REPORT ERROR
  ! (generic error reporting subroutine)
  SUBROUTINE sub_report_error(dum_mod,dum_proc,dum_mes,dum_act,dum_data,dum_fatal)
    IMPLICIT NONE
    ! dummy arguments
    CHARACTER(LEN=*),INTENT(in)::dum_mod
    CHARACTER(LEN=*),INTENT(in)::dum_proc
    CHARACTER(LEN=*),INTENT(in)::dum_mes
    CHARACTER(LEN=*),INTENT(in)::dum_act
    REAL,DIMENSION(:),INTENT(in)::dum_data
    LOGICAL,INTENT(in)::dum_fatal
    ! local variables
    INTEGER::loc_n,loc_n_max
    ! set default maximum number of dummy <dum_data> argument values to be displayed
    loc_n_max = SIZE(dum_data)
    ! display dummy data and exit if requested
    IF (dum_fatal) THEN
       PRINT*,' '
       PRINT*,'*** FATAL ERROR ***'
       print*,' -> Originating location in code [module,subroutine]: '//dum_mod//','//dum_proc
       PRINT*,' -> ERROR MESSAGE: '//dum_mes
       if ((loc_n_max == 1) .AND. (dum_data(loc_n_max) <= const_real_null)) then
          PRINT*,' -> ERROR DATA:    ','[NONE]'
       else
          DO loc_n = 1,loc_n_max
             PRINT*,' -> ERROR DATA:    ',dum_data(loc_n)
          END DO
       end if
       PRINT*,' -> ERROR ACTION:  '//dum_act
       PRINT*,' '
       PRINT*,'*********** END ************'
       PRINT*,' '
       STOP
    ELSE
       PRINT*,' '
       PRINT*,'*** WARNING ***'
       print*,' -> Originating location in code [module,subroutine]: '//dum_mod//','//dum_proc
       PRINT*,' -> ERROR MESSAGE: '//dum_mes
       if ((loc_n_max == 1) .AND. (dum_data(loc_n_max) <= const_real_null)) then
       else
          DO loc_n = 1,loc_n_max
             PRINT*,' -> ERROR DATA:    ',dum_data(loc_n)
          end do
       end if
       PRINT*,' -> ERROR ACTION:  '//dum_act
       PRINT*,' '
    END IF
  END SUBROUTINE sub_report_error
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DETERMINE DATA FILE LENGTH
  function fun_calc_data_n(dum_filename)
    ! common blocks
    IMPLICIT NONE
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    integer::fun_calc_data_n
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    CHARACTER(len=*),INTENT(in)::dum_filename
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::n,loc_n
    integer::ios
    CHARACTER(len=255)::loc_data
    ! -------------------------------------------------------- !
    ! DETERMINE n
    ! -------------------------------------------------------- !
    n = 0
    OPEN(unit=in,status='old',file=dum_filename,action="READ",IOSTAT=ios)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'gem_util','fun_calc_data_n', &
            & 'File <'//trim(dum_filename)//'> does not exist', &
            & 'STOPPING', &
            & (/const_real_zero/),.true. &
            & )
    else
    DO
       READ(unit=in,fmt='(A255)',iostat=ios) loc_data
       IF (ios < 0) THEN
          loc_n = n
          EXIT
       ELSEIF (ios > 0) THEN
          PRINT*,'File <'//trim(dum_filename)//'>: Problem with data format'
          STOP
       else
          n=n+1
       END IF
    end DO
    end if
    CLOSE(unit=in)
    ! -------------------------------------------------------- !
    ! RETURN RESULT
    ! -------------------------------------------------------- !
    fun_calc_data_n = loc_n
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END function fun_calc_data_n
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD IN (i,j) LOCATION DATA
  subroutine sub_load_data_npt(dum_filename,dum_n,dum_data)
    ! common blocks
    IMPLICIT NONE
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    CHARACTER(len=*),INTENT(in)::dum_filename
    integer,intent(in)::dum_n
    integer,INTENT(inout),DIMENSION(dum_n,2)::dum_data
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::n
    integer::ios
    ! -------------------------------------------------------- !
    ! READ DATA
    ! -------------------------------------------------------- !
    OPEN(unit=in,status='old',file=TRIM(dum_filename),action='read',IOSTAT=ios)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'gem_util','sub_load_data_npt', &
            & 'File <'//trim(dum_filename)//'> does not exist', &
            & 'STOPPING', &
            & (/const_real_zero/),.true. &
            & )
    else
       DO n=1,dum_n,1
          READ(unit=in,fmt=*) dum_data(n,1),dum_data(n,2)
       ENDDO
    end if
    CLOSE(in)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END subroutine sub_load_data_npt
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD IN STRING DATA
  subroutine sub_load_data_nstr(dum_filename,dum_n,dum_char)
    ! common blocks
    IMPLICIT NONE
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    CHARACTER(len=*),INTENT(in)::dum_filename
    integer,intent(in)::dum_n
    CHARACTER(len=31),INTENT(inout),DIMENSION(dum_n)::dum_char
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::n
    integer::ios
    ! -------------------------------------------------------- !
    ! READ DATA
    ! -------------------------------------------------------- !
    OPEN(unit=in,status='old',file=TRIM(dum_filename),action='read',IOSTAT=ios)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'gem_util','sub_load_data_nstr', &
            & 'File <'//trim(dum_filename)//'> does not exist', &
            & 'STOPPING', &
            & (/const_real_zero/),.true. &
            & )
    else
       DO n=1,dum_n,1
          READ(unit=in,fmt=*) dum_char(n)(:)
       ENDDO
    end if
    CLOSE(in)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END subroutine sub_load_data_nstr
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD IN i (1D) DATA
  subroutine sub_load_data_i(dum_filename,dum_i,dum_data)
    ! common blocks
    IMPLICIT NONE
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    CHARACTER(len=*),INTENT(in)::dum_filename
    integer,intent(in)::dum_i
    REAL,INTENT(inout),DIMENSION(dum_i)::dum_data
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::i
    integer::ios
    ! -------------------------------------------------------- !
    ! READ DATA
    ! -------------------------------------------------------- !
    OPEN(unit=in,status='old',file=TRIM(dum_filename),action='read',IOSTAT=ios)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'gem_util','fun_load_data_i', &
            & 'File <'//trim(dum_filename)//'> does not exist', &
            & 'STOPPING', &
            & (/const_real_zero/),.true. &
            & )
    else
       DO i=1,dum_i,1
          READ(unit=in,fmt=*) dum_data(i)
       ENDDO
    end if
    CLOSE(in)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END subroutine sub_load_data_i
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD IN ij (2D) DATA
  SUBROUTINE sub_load_data_ij(dum_filename,dum_i,dum_j,dum_data)
    ! common blocks
    IMPLICIT NONE
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename
    integer,intent(in)::dum_i,dum_j
    REAL,INTENT(inout),DIMENSION(dum_i,dum_j)::dum_data
    ! local variables
    INTEGER::i,j
    integer::ios
    ! read data
    OPEN(unit=in,status='old',file=TRIM(dum_filename),action='read',IOSTAT=ios)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'gem_util','sub_load_data_ij', &
            & 'File <'//trim(dum_filename)//'> does not exist', &
            & 'STOPPING', &
            & (/const_real_zero/),.true. &
            & )
    else
       DO j=dum_j,1,-1
          READ(unit=in,fmt=*) (dum_data(i,j),i=1,dum_i)
       ENDDO
    end if
    CLOSE(in)
  END SUBROUTINE sub_load_data_ij
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD IN ijk (3D) DATA
  SUBROUTINE sub_load_data_ijk(dum_filename,dum_i,dum_j,dum_k,dum_data)
    ! common blocks
    IMPLICIT NONE
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename
    integer,intent(in)::dum_i,dum_j,dum_k
    REAL,INTENT(inout),DIMENSION(dum_i,dum_j,dum_k)::dum_data
    ! local variables
    INTEGER::i,j,k
    integer::ios
    ! save data
    OPEN(unit=in,status='old',file=TRIM(dum_filename),action='read',IOSTAT=ios)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'gem_util','sub_load_data_ijk', &
            & 'File <'//trim(dum_filename)//'> does not exist', &
            & 'STOPPING', &
            & (/const_real_zero/),.true. &
            & )
    else
       DO k=dum_k,1,-1
          DO j=dum_j,1,-1
             READ(unit=in,fmt=*) (dum_data(i,j,k),i=1,dum_i)
          ENDDO
       END DO
    end if
    CLOSE(in)
  END SUBROUTINE sub_load_data_ijk
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE ij (2D) DATA
  SUBROUTINE sub_save_data_ij(dum_filename,dum_i,dum_j,dum_data)
    ! common blocks
    IMPLICIT NONE
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename
    integer,intent(in)::dum_i,dum_j
    REAL,INTENT(in),DIMENSION(dum_i,dum_j)::dum_data
    ! local variables
    INTEGER::i,j
    ! save data
    OPEN(unit=out,file=TRIM(dum_filename),action='write')
    DO j=dum_j,1,-1
       WRITE(unit=out,fmt='(999e14.6)') (dum_data(i,j),i=1,dum_i)
    ENDDO
    CLOSE(out)
  END SUBROUTINE sub_save_data_ij
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE ijk (3D) DATA
  SUBROUTINE sub_save_data_ijk(dum_filename,dum_i,dum_j,dum_k,dum_data)
    ! common blocks
    IMPLICIT NONE
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename
    integer,intent(in)::dum_i,dum_j,dum_k
    REAL,INTENT(in),DIMENSION(dum_i,dum_j,dum_k)::dum_data
    ! local variables
    INTEGER::i,j,k
    ! save data
    OPEN(unit=out,file=TRIM(dum_filename),action='write')
    DO k=dum_k,1,-1
       DO j=dum_j,1,-1
          WRITE(unit=out,fmt='(999e14.6)') (dum_data(i,j,k),i=1,dum_i)
       ENDDO
    END DO
    CLOSE(out)
  END SUBROUTINE sub_save_data_ijk
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! COPY (LOAD AND RE-SAVE) A FILE
  SUBROUTINE sub_copy_ascii_file(dum_filename_in,dum_filename_out)
    ! common blocks
    IMPLICIT NONE
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename_in
    CHARACTER(len=*),INTENT(in)::dum_filename_out
    ! local variables
    INTEGER::n
    INTEGER::loc_n_elements,loc_n_start
    character(len=255),dimension(32767)::loc_string
    ! *** INPUT DATA ***
    ! check file format
    CALL sub_check_fileformat(dum_filename_in,loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=dum_filename_in,action='read')
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! read in data as text
    DO n = 1,loc_n_elements
       read(unit=in,fmt='(A255)') loc_string(n)
    end do
    CLOSE(unit=in)
    ! *** RE-SAVE DATA ***
    ! open file pipe
    OPEN(unit=out,file=TRIM(dum_filename_out),status='replace',action='write')
    write(unit=out,fmt='(A15)') '-START-OF-DATA-'
    DO n = 1,loc_n_elements
       write(unit=out,FMT=*) trim(loc_string(n))
    end do
    write(unit=out,fmt='(A13)') '-END-OF-DATA-'
    CLOSE(unit=out)
  END SUBROUTINE sub_copy_ascii_file
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** ISOTOPE CALCULATION ROUTINES ********************************************************************************************* !
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE DELTA (FROM mol TOTAL ABUNDANCE AND mol ISOTOPE ABUNDANCE)
  FUNCTION fun_calc_isotope_delta(dum_totabundance,dum_isoabundance,dum_standard,dum_allow_negative,dum_null)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_isotope_delta
    ! dummy arguments
    REAL,INTENT(in)::dum_totabundance,dum_isoabundance,dum_standard
    logical,INTENT(in)::dum_allow_negative
    REAL,INTENT(in)::dum_null
    ! local variables
    real::loc_fractionalabundance,loc_R
    ! calculate local variables and return function value
    ! Convert from r to R (see Zeebe and Wolf-Gladrow, 2001])
    ! NOTE: return an uncalculatable value as const_real_null so as to be flagged as a NaN in netCDF
    ! NOTE: allow negative total abundances in the case of fluxes out (== a -ve. flux) if the dum_allow_negative flag is .TRUE.
    ! NOTE: capture the unlikely event of total (bulk) being equal to isotopic abundance ...
    IF (                                                                             &
         & ((abs(dum_totabundance) > const_real_nullsmall) .AND. dum_allow_negative) &
         & .OR.                                                                      &
         & (dum_totabundance > const_real_nullsmall)                                 &
         & ) THEN
       loc_fractionalabundance = dum_isoabundance/dum_totabundance
       if ((1.0 - loc_fractionalabundance) > const_real_nullsmall) then
          loc_R = loc_fractionalabundance/(1.0 - loc_fractionalabundance)
          fun_calc_isotope_delta = 1000.0*(loc_R/dum_standard - 1.0)
       else
          fun_calc_isotope_delta = dum_null
       endif
    else
       fun_calc_isotope_delta = dum_null
    end IF
  END FUNCTION fun_calc_isotope_delta
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE FRACTIONAL ISOTOPIC ABUNDANCE OF TOTAL
  FUNCTION fun_calc_isotope_fraction(dum_delta,dum_standard)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_isotope_fraction
    ! dummy arguments
    REAL,INTENT(in)::dum_delta,dum_standard
    ! local variables
    real::loc_R
    ! calculate local variables
    loc_R = dum_standard*(1.0 + dum_delta/1000.0)
    ! return function value
    ! (and convert from R to r)
    fun_calc_isotope_fraction = loc_R/(1.0 + loc_R)
  END FUNCTION fun_calc_isotope_fraction
  ! ****************************************************************************************************************************** !

  
  ! ****************************************************************************************************************************** !
  ! CALCULATE DELTA (FROM mol light mass ISOTOPE AND mol ISOTOPE ABUNDANCE)
  ! NOTE: based on a ratio of one mass to the other, rather than to total
  !       (basically as per fun_calc_isotope_delta except skipping the conversion from the r notation)
  FUNCTION fun_calc_isotope_deltaR(dum_isoabundance0,dum_isoabundance1,dum_standard,dum_null)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_isotope_deltaR
    ! dummy arguments
    REAL,INTENT(in)::dum_isoabundance0,dum_isoabundance1,dum_standard
    REAL,INTENT(in)::dum_null
    ! local variables
    real::loc_R
    ! calculate local variables and return function value
    IF (abs(dum_isoabundance0) > const_real_nullsmall) THEN
       loc_R = dum_isoabundance1/dum_isoabundance0
       fun_calc_isotope_deltaR = 1000.0*(loc_R/dum_standard - 1.0)
    else
       fun_calc_isotope_deltaR = dum_null
    end IF
  END FUNCTION fun_calc_isotope_deltaR
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE FRACTIONAL ISOTOPIC ABUNDANCE OF TOTAL
  ! NOTE: based on a ratio of one mass to the other, rather than to total
  !       (basically as per fun_calc_isotope_fraction except skipping the conversion to the r notation (from R))
  FUNCTION fun_calc_isotope_fractionR(dum_delta,dum_standard)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_isotope_fractionR
    ! dummy arguments
    REAL,INTENT(in)::dum_delta,dum_standard
    ! return function value
    fun_calc_isotope_fractionR = dum_standard*(1.0 + dum_delta/1000.0)
  END FUNCTION fun_calc_isotope_fractionR
  ! ****************************************************************************************************************************** !

  
  ! ****************************************************************************************************************************** !
  ! CALCULATE lightest isotope abundance (from total and ratios)
  ! NOTE: assuming 3 different masses
  FUNCTION fun_calc_isotope_abundanceR0(dum_totabundance,dum_R1,dum_R2)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_isotope_abundanceR0
    ! dummy arguments
    REAL,INTENT(in)::dum_totabundance,dum_R1,dum_R2
    ! calculate local variables and return function value
    fun_calc_isotope_abundanceR0 = dum_totabundance/(1.0 + dum_R1 + dum_R2)
  END FUNCTION fun_calc_isotope_abundanceR0
  ! ****************************************************************************************************************************** !

  
  ! ****************************************************************************************************************************** !
  ! CALCULATE isotope abundance
  ! NOTE: assuming 3 different masses
  FUNCTION fun_calc_isotope_abundanceR012ocn(dum_i1,dum_i2,dum_ocn,dum_select)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_isotope_abundanceR012ocn
    ! dummy arguments
    integer,INTENT(in)::dum_i1,dum_i2,dum_select             !
    real,dimension(1:n_ocn),INTENT(in)::dum_ocn              !
    ! local variables
    real::loc_standard1,loc_standard2
    real::loc_R1,loc_R2
    real::loc_tot
    ! calculate local variables
    ! NOTE: in calculating loc_tot, either dum_i1 or dum_i2 can be used to derive the bulk tracer index
    loc_standard1 = const_standardsR(ocn_type(dum_i1))
    loc_R1 = fun_calc_isotope_fractionR(dum_ocn(dum_i1),loc_standard1)
    loc_standard2 = const_standardsR(ocn_type(dum_i2))
    loc_R2 = fun_calc_isotope_fractionR(dum_ocn(dum_i2),loc_standard2)
    loc_tot = fun_calc_isotope_abundanceR0(dum_ocn(ocn_dep(dum_i1)),loc_R1,loc_R2)
    SELECT CASE (dum_select)
    CASE (0)
       fun_calc_isotope_abundanceR012ocn = loc_tot
    CASE (1)
       fun_calc_isotope_abundanceR012ocn = loc_R1*loc_tot
    CASE (2)
       fun_calc_isotope_abundanceR012ocn = loc_R2*loc_tot
    CASE DEFAULT
       ! ERROR
       CALL sub_report_error( &
            & 'gem_util','fun_calc_isotope_abundanceR012ocn', &
            & 'Invalid parameter value for dum_select:'//fun_conv_num_char_n(1,dum_select), &
            & 'EXITING', &
            & (/const_real_null/),.true. &
            & )
    END SELECT
  END FUNCTION fun_calc_isotope_abundanceR012ocn
  ! ****************************************************************************************************************************** !

  
  ! ****************************************************************************************************************************** !
  ! CALCULATE isotope abundance
  ! NOTE: assuming 3 different masses
  FUNCTION fun_calc_isotope_abundanceR012sed(dum_i1,dum_i2,dum_sed,dum_select)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_isotope_abundanceR012sed
    ! dummy arguments
    integer,INTENT(in)::dum_i1,dum_i2,dum_select             !
    real,dimension(1:n_sed),INTENT(in)::dum_sed              !
    ! local variables
    real::loc_standard1,loc_standard2
    real::loc_R1,loc_R2
    real::loc_tot
    ! calculate local variables
    ! NOTE: in calculating loc_tot, either dum_i1 or dum_i2 can be used to derive the bulk tracer index
    loc_standard1 = const_standardsR(sed_type(dum_i1))
    loc_R1 = fun_calc_isotope_fractionR(dum_sed(dum_i1),loc_standard1)
    loc_standard2 = const_standardsR(sed_type(dum_i2))
    loc_R2 = fun_calc_isotope_fractionR(dum_sed(dum_i2),loc_standard2)
    loc_tot = fun_calc_isotope_abundanceR0(dum_sed(sed_dep(dum_i1)),loc_R1,loc_R2)
    SELECT CASE (dum_select)
    CASE (0)
       fun_calc_isotope_abundanceR012sed = loc_tot
    CASE (1)
       fun_calc_isotope_abundanceR012sed = loc_R1*loc_tot
    CASE (2)
       fun_calc_isotope_abundanceR012sed = loc_R2*loc_tot
    CASE DEFAULT
       ! ERROR
       CALL sub_report_error( &
            & 'gem_util','fun_calc_isotope_abundanceR012sed', &
            & 'Invalid parameter value for dum_select:'//fun_conv_num_char_n(1,dum_select), &
            & 'EXITING', &
            & (/const_real_null/),.true. &
            & )
    END SELECT
  END FUNCTION fun_calc_isotope_abundanceR012sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT: d14C -> D14C
  FUNCTION fun_convert_delta14CtoD14C(dum_delta13C,dum_delta14C)
    IMPLICIT NONE
    ! result variable
    REAL::fun_convert_delta14CtoD14C
    ! dummy arguments
    REAL,INTENT(in)::dum_delta13C,dum_delta14C
    ! return function value
    ! NOTE: see Stuiver and Polach [1977] (Stuiver and Robinson [1974])
    fun_convert_delta14CtoD14C = 1000.0* &
         &( &
         &   (1.0 + dum_delta14C/1000.0) * &
         &   (0.975**2)/((1.0 + dum_delta13C/1000.0)**2) - &
         &   1.0 &
         & )
  END FUNCTION fun_convert_delta14CtoD14C
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT: D14C -> d14C
  FUNCTION fun_convert_D14Ctodelta14C(dum_delta13C,dum_D14C)
    IMPLICIT NONE
    ! result variable
    REAL::fun_convert_D14Ctodelta14C
    ! dummy arguments
    REAL,INTENT(in)::dum_delta13C,dum_D14C
    ! return function value
    ! NOTE: see Stuiver and Polach [1977] (Stuiver and Robinson [1974])
    fun_convert_D14Ctodelta14C = 1000.0* &
         &( &
         &   (1.0 + dum_D14C/1000.0) * &
         &   ((1.0 + dum_delta13C/1000.0)**2)/(0.975**2) - &
         &   1.0 &
         & )
  END FUNCTION fun_convert_D14Ctodelta14C
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT: D14C -> RADIOCARBON AGE
  FUNCTION fun_convert_D14Ctoage(dum_D14C)
    IMPLICIT NONE
    ! result variable
    REAL::fun_convert_D14Ctoage
    ! dummy arguments
    REAL,INTENT(in)::dum_D14C
    ! return function value
    IF ((1.0 + dum_D14C/1000.0) > const_real_nullsmall) THEN
       fun_convert_D14Ctoage = -1.*log(1.0 + dum_D14C/1000.0)/const_lambda_14C_libby
    else
       fun_convert_D14Ctoage = 0.0
    endif
  END FUNCTION fun_convert_D14Ctoage
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** MISCELLANEOUS ROUTINES *************************************************************************************************** !
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE DENSITY OF SEA-WATER
  ! NOTE: from Winton and Sarachik [1993]
  ! NOTE: rho in units of (kg m-3)
  ! NOTE: salinity is in (o/oo)
  ! NOTE: temperature must be converted from (K) to (degrees Celcius)
  FUNCTION fun_calc_rho(T,S)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_rho
    ! dummy arguments
    REAL,INTENT(in)::T,S
    ! local variables
    REAL::T_C
    ! convert units of local variables
    T_C = T - const_zeroC
    ! return function value
    fun_calc_rho = 1000.0 + (0.7968 * S - 0.0559 * T_C - 0.0063 * T_C**2 + 3.7315E-05 * T_C**3)
  END FUNCTION fun_calc_rho
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE PRESSURE MODIFIER
  FUNCTION fun_corr_p(dum_TC,dum_P,dum_rRtimesT,dum_dp)
    ! result variable
    REAL::fun_corr_p
    ! dummy arguments
    REAL,INTENT(IN)::dum_TC,dum_P,dum_rRtimesT
    REAL,INTENT(IN),DIMENSION(5)::dum_dp
    ! return function value
    fun_corr_p = &
         & ( &
         &   -(dum_dp(1) + dum_dp(2)*dum_TC + dum_dp(3)*dum_TC*dum_TC) + &
         &   (5.0E-4*(dum_dp(4) + dum_dp(5)*dum_TC))*dum_P &
         & )*dum_P*dum_rRtimesT
  END FUNCTION fun_corr_p
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** 'MATHS' ROUTINES ********************************************************************************************************* !
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SOLVE A QUADRATIC EQUATION
  ! ax2 + bx + c = 0.0
  FUNCTION fun_quad_root(a,b,c)
    IMPLICIT NONE
    ! result variable
    REAL,DIMENSION(2)::fun_quad_root
    ! dummy arguments
    REAL,INTENT(in)::a,b,c
    ! local variables
    real::D
    ! calculate discriminant
    D = b**2 - 4.0*a*c
    ! return value
    if (D < -const_real_nullsmall) then
       ! no real roots
       fun_quad_root(1) = 0.0
       fun_quad_root(2) = 0.0
    else
       ! 2 (or potentially one) real roots
       fun_quad_root(1) = (-b + (b**2 - 4.0*a*c)**0.5)/(2.0*a)
       fun_quad_root(2) = (-b - (b**2 - 4.0*a*c)**0.5)/(2.0*a)
    end if
  END FUNCTION fun_quad_root
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE 'POTENTIAL' OXIDIZING CAPACITY
  FUNCTION fun_potO2cap(dum_select,dum_ocn,dum_ocn_remin)
    IMPLICIT NONE
    ! result variable
    real::fun_potO2cap
    ! dummy arguments
    logical,INTENT(in),DIMENSION(n_ocn)::dum_select
    REAL,INTENT(in),DIMENSION(n_ocn)::dum_ocn
    REAL,INTENT(in),DIMENSION(n_ocn)::dum_ocn_remin
    ! return value
    fun_potO2cap = 0.0
    if (dum_select(io_O2)) fun_potO2cap = &
         & fun_potO2cap + dum_ocn(io_O2) + dum_ocn_remin(io_O2)
    if (dum_select(io_NO3) .AND. dum_select(io_N2)) fun_potO2cap = &
         & fun_potO2cap + 1.25*(dum_ocn(io_NO3) + dum_ocn_remin(io_NO3))
    if (dum_select(io_SO4) .AND. dum_select(io_H2S)) fun_potO2cap = &
         & fun_potO2cap + 2.0*(dum_ocn(io_SO4) + dum_ocn_remin(io_SO4))
    ! cap potential oxidation capacity at zero
    if (fun_potO2cap < const_real_nullsmall) fun_potO2cap = 0.0
  END FUNCTION fun_potO2cap
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LINEARILY INTERPOLATE IN 4 DIMENSIONAL SPACE
  FUNCTION fun_interp_4Dvec(a,b,c,d,vec_a,vec_b,vec_c,vec_d,array)
    IMPLICIT NONE
    ! result variable
    REAL::fun_interp_4Dvec
    ! dummy arguments
    REAL,INTENT(in)::a
    REAL,INTENT(in)::b
    REAL,INTENT(in)::c
    REAL,INTENT(in)::d
    REAL,INTENT(in),DIMENSION(:)::vec_a
    REAL,INTENT(in),DIMENSION(:)::vec_b
    REAL,INTENT(in),DIMENSION(:)::vec_c
    REAL,INTENT(in),DIMENSION(:)::vec_d
    REAL,INTENT(in),DIMENSION(size(vec_a),size(vec_b),size(vec_c),size(vec_d))::array
    ! local variables
    integer::n,n_max
    REAL::a1,a2
    REAL::b1,b2
    REAL::c1,c2
    REAL::d1,d2
    INTEGER::i_a1,i_a2
    INTEGER::i_b1,i_b2
    INTEGER::i_c1,i_c2
    INTEGER::i_d1,i_d2

    ! *** calculate grid points enclosing the passes point coordinates ***
    ! find dimension pair; indices and values -- a
    n_max = size(vec_a)
    if (a < vec_a(1)) then
       i_a1 = 1
       i_a2 = 2
    elseif (a >= vec_a(n_max)) then
       i_a1 = n_max-1
       i_a2 = n_max
    else
       DO n = 2,n_max
          if (vec_a(n) >= a) then
             i_a1 = n-1
             i_a2 = n
             exit
          end if
       end do
    end if
    a1 = vec_a(i_a1)
    a2 = vec_a(i_a2)
    ! find dimension pair; indices and values -- b
    n_max = size(vec_b)
    if (b < vec_b(1)) then
       i_b1 = 1
       i_b2 = 2
    elseif (b >= vec_b(n_max)) then
       i_b1 = n_max-1
       i_b2 = n_max
    else
       DO n = 2,n_max
          if (vec_b(n) >= b) then
             i_b1 = n-1
             i_b2 = n
             exit
          end if
       end do
    end if
    b1 = vec_b(i_b1)
    b2 = vec_b(i_b2)
    ! find dimension pair; indices and values -- c
    n_max = size(vec_c)
    if (c < vec_c(1)) then
       i_c1 = 1
       i_c2 = 2
    elseif (c >= vec_c(n_max)) then
       i_c1 = n_max-1
       i_c2 = n_max
    else
       DO n = 2,n_max
          if (vec_c(n) >= c) then
             i_c1 = n-1
             i_c2 = n
             exit
          end if
       end do
    end if
    c1 = vec_c(i_c1)
    c2 = vec_c(i_c2)
    ! find dimension pair; indices and values -- d
    n_max = size(vec_d)
    if (d < vec_d(1)) then
       i_d1 = 1
       i_d2 = 2
    elseif (d >= vec_d(n_max)) then
       i_d1 = n_max-1
       i_d2 = n_max
    else
       DO n = 2,n_max
          if (vec_d(n) >= d) then
             i_d1 = n-1
             i_d2 = n
             exit
          end if
       end do
    end if
    d1 = vec_d(i_d1)
    d2 = vec_d(i_d2)

    ! *** return function value ***
    ! interpolate
    ! NOTE: see 'Applied Numerical Methods with Software' by Nakamura for details of 1-D and 2-D interpolation
    fun_interp_4Dvec = (1.0 / ((a2-a1)*(b2-b1)*(c2-c1)*(d2-d1))) * &
         & ( &
         &   (a-a1)*(b-b1)*(c-c1)*(d-d1) * array(i_a2,i_b2,i_c2,i_d2) + &
         &   (a-a1)*(b-b1)*(c-c1)*(d2-d) * array(i_a2,i_b2,i_c2,i_d1) + &
         &   (a-a1)*(b-b1)*(c2-c)*(d-d1) * array(i_a2,i_b2,i_c1,i_d2) + &
         &   (a-a1)*(b-b1)*(c2-c)*(d2-d) * array(i_a2,i_b2,i_c1,i_d1) + &
         &   (a-a1)*(b2-b)*(c-c1)*(d-d1) * array(i_a2,i_b1,i_c2,i_d2) + &
         &   (a-a1)*(b2-b)*(c-c1)*(d2-d) * array(i_a2,i_b1,i_c2,i_d1) + &
         &   (a-a1)*(b2-b)*(c2-c)*(d-d1) * array(i_a2,i_b1,i_c1,i_d2) + &
         &   (a-a1)*(b2-b)*(c2-c)*(d2-d) * array(i_a2,i_b1,i_c1,i_d1) + &
         &   (a2-a)*(b-b1)*(c-c1)*(d-d1) * array(i_a1,i_b2,i_c2,i_d2) + &
         &   (a2-a)*(b-b1)*(c-c1)*(d2-d) * array(i_a1,i_b2,i_c2,i_d1) + &
         &   (a2-a)*(b-b1)*(c2-c)*(d-d1) * array(i_a1,i_b2,i_c1,i_d2) + &
         &   (a2-a)*(b-b1)*(c2-c)*(d2-d) * array(i_a1,i_b2,i_c1,i_d1) + &
         &   (a2-a)*(b2-b)*(c-c1)*(d-d1) * array(i_a1,i_b1,i_c2,i_d2) + &
         &   (a2-a)*(b2-b)*(c-c1)*(d2-d) * array(i_a1,i_b1,i_c2,i_d1) + &
         &   (a2-a)*(b2-b)*(c2-c)*(d-d1) * array(i_a1,i_b1,i_c1,i_d2) + &
         &   (a2-a)*(b2-b)*(c2-c)*(d2-d) * array(i_a1,i_b1,i_c1,i_d1) &
         & )

  END FUNCTION fun_interp_4Dvec
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LINEARILY INTERPOLATE IN 4 DIMENSIONAL SPACE
  FUNCTION fun_interp_4D(array,a,b,c,d,a_max,b_max,c_max,d_max, &
       & i_a_min,i_a_max,i_b_min,i_b_max,i_c_min,i_c_max,i_d_min,i_d_max)
    IMPLICIT NONE
    ! result variable
    REAL::fun_interp_4D
    ! dummy arguments
    INTEGER,INTENT(in)::i_a_min,i_a_max
    INTEGER,INTENT(in)::i_b_min,i_b_max
    INTEGER,INTENT(in)::i_c_min,i_c_max
    INTEGER,INTENT(in)::i_d_min,i_d_max
    REAL,INTENT(in),DIMENSION(i_a_min:i_a_max,i_b_min:i_b_max, &
         & i_c_min:i_c_max,i_d_min:i_d_max)::array
    REAL,INTENT(in)::a
    REAL,INTENT(in)::b
    REAL,INTENT(in)::c
    REAL,INTENT(in)::d
    REAL,INTENT(in)::a_max
    REAL,INTENT(in)::b_max
    REAL,INTENT(in)::c_max
    REAL,INTENT(in)::d_max
    ! local variables
    INTEGER::i_a,i_b,i_c,i_d
    REAL::a1,a2
    REAL::b1,b2
    REAL::c1,c2
    REAL::d1,d2
    INTEGER::i_a1,i_a2
    INTEGER::i_b1,i_b2
    INTEGER::i_c1,i_c2
    INTEGER::i_d1,i_d2

    ! *** calculate grid points enclosing the passes point coordinates ***
    ! calculate integer values each point coordinate
    i_a = INT(a * (1.0 / a_max) * i_a_max)
    i_b = INT(b * (1.0 / b_max) * i_b_max)
    i_c = INT(c * (1.0 / c_max) * i_c_max)
    i_d = INT(d * (1.0 / d_max) * i_d_max)
    ! calculate:
    ! (a) bounding grid points along each dimension ('i_x1' and 'i_x2')
    !     NOTE: if the possition of the point along any one dimensions is on or past the
    !           boundary of that dimension, then both grid points are set to the boundary grid point
    ! (b) bounding points of the interval containing the point in question ('x1' and 'x2')
    !     NOTE: the interval between these two points is fixed at the resolution along that dimension
    !           of the look-up table, even if the point in question lies outside the bounding space
    !           (this is to prevent divide-by-zero errors in the interpolation)
    ! (c) if the position of the point in question falls outside of the table boundary along any dimension,
    !     the value at the required point is estimated via a linear extrapolation
    !     using the last two points long that particular dimension
    ! parameter 'a'
    IF (a >= a_max) THEN
       i_a1 = i_a_max - 1
       i_a2 = i_a_max
       a1   = i_a1 * (1.0 / i_a_max) * a_max
       a2   = i_a2 * (1.0 / i_a_max) * a_max
    ELSE IF (a < (i_a_min * (a_max / i_a_max))) THEN
       i_a1 = i_a_min
       i_a2 = i_a_min + 1
       a1   = i_a1 * (1.0 / i_a_max) * a_max
       a2   = i_a2 * (1.0 / i_a_max) * a_max
    ELSE IF (a < 0.0) THEN
       i_a1 = i_a - 1
       i_a2 = i_a
       a1   = i_a1 * (1.0 / i_a_max) * a_max
       a2   = i_a2 * (1.0 / i_a_max) * a_max
    ELSE
       i_a1 = i_a
       i_a2 = i_a + 1
       a1   = i_a1 * (1.0 / i_a_max) * a_max
       a2   = i_a2 * (1.0 / i_a_max) * a_max
    ENDIF
    ! parameter 'b'
    IF (b >= b_max) THEN
       i_b1 = i_b_max - 1
       i_b2 = i_b_max
       b1   = i_b1 * (1.0 / i_b_max) * b_max
       b2   = i_b2 * (1.0 / i_b_max) * b_max
    ELSE IF (b < (i_b_min * (b_max / i_b_max))) THEN
       i_b1 = i_b_min
       i_b2 = i_b_min + 1
       b1   = i_b1 * (1.0 / i_b_max) * b_max
       b2   = i_b2 * (1.0 / i_b_max) * b_max
    ELSE IF (b < 0.0) THEN
       i_b1 = i_b - 1
       i_b2 = i_b
       b1   = i_b1 * (1.0 / i_b_max) * b_max
       b2   = i_b2 * (1.0 / i_b_max) * b_max
    ELSE
       i_b1 = i_b
       i_b2 = i_b + 1
       b1   = i_b1 * (1.0 / i_b_max) * b_max
       b2   = i_b2 * (1.0 / i_b_max) * b_max
    ENDIF
    ! parameter 'c'
    IF (c >= c_max) THEN
       i_c1 = i_c_max - 1
       i_c2 = i_c_max
       c1   = (i_c1 - 1) * (1.0 / i_c_max) * c_max
       c2   = (i_c2 - 0) * (1.0 / i_c_max) * c_max
    ELSE IF (c < (i_c_min * (c_max / i_c_max))) THEN
       i_c1 = i_c_min
       i_c2 = i_c_min + 1
       c1   = i_c1 * (1.0 / i_c_max) * c_max
       c2   = i_c2 * (1.0 / i_c_max) * c_max
    ELSE IF (c < 0.0) THEN
       i_c1 = i_c - 1
       i_c2 = i_c
       c1   = i_c1 * (1.0 / i_c_max) * c_max
       c2   = i_c2 * (1.0 / i_c_max) * c_max
    ELSE
       i_c1 = i_c
       i_c2 = i_c + 1
       c1   = i_c1 * (1.0 / i_c_max) * c_max
       c2   = i_c2 * (1.0 / i_c_max) * c_max
    ENDIF
    ! parameter 'd'
    IF (d >= d_max) THEN
       i_d1 = i_d_max - 1
       i_d2 = i_d_max
       d1   = i_d1 * (1.0 / i_d_max) * d_max
       d2   = i_d2 * (1.0 / i_d_max) * d_max
    ELSE IF (d < (i_d_min * (d_max / i_d_max))) THEN
       i_d1 = i_d_min
       i_d2 = i_d_min + 1
       d1   = i_d1 * (1.0 / i_d_max) * d_max
       d2   = i_d2 * (1.0 / i_d_max) * d_max
    ELSE IF (d < 0.0) THEN
       i_d1 = i_d - 1
       i_d2 = i_d
       d1   = i_d1 * (1.0 / i_d_max) * d_max
       d2   = i_d2 * (1.0 / i_d_max) * d_max
    ELSE
       i_d1 = i_d
       i_d2 = i_d + 1
       d1   = i_d1 * (1.0 / i_d_max) * d_max
       d2   = i_d2 * (1.0 / i_d_max) * d_max
    ENDIF

    ! *** return function value ***
    ! interpolate
    ! NOTE: see 'Applied Numerical Methods with Software' by Nakamura for details of 1-D and 2-D interpolation
    fun_interp_4D = (1.0 / ((a2-a1)*(b2-b1)*(c2-c1)*(d2-d1))) * &
         & ( &
         &   (a-a1)*(b-b1)*(c-c1)*(d-d1) * array(i_a2,i_b2,i_c2,i_d2) + &
         &   (a-a1)*(b-b1)*(c-c1)*(d2-d) * array(i_a2,i_b2,i_c2,i_d1) + &
         &   (a-a1)*(b-b1)*(c2-c)*(d-d1) * array(i_a2,i_b2,i_c1,i_d2) + &
         &   (a-a1)*(b-b1)*(c2-c)*(d2-d) * array(i_a2,i_b2,i_c1,i_d1) + &
         &   (a-a1)*(b2-b)*(c-c1)*(d-d1) * array(i_a2,i_b1,i_c2,i_d2) + &
         &   (a-a1)*(b2-b)*(c-c1)*(d2-d) * array(i_a2,i_b1,i_c2,i_d1) + &
         &   (a-a1)*(b2-b)*(c2-c)*(d-d1) * array(i_a2,i_b1,i_c1,i_d2) + &
         &   (a-a1)*(b2-b)*(c2-c)*(d2-d) * array(i_a2,i_b1,i_c1,i_d1) + &
         &   (a2-a)*(b-b1)*(c-c1)*(d-d1) * array(i_a1,i_b2,i_c2,i_d2) + &
         &   (a2-a)*(b-b1)*(c-c1)*(d2-d) * array(i_a1,i_b2,i_c2,i_d1) + &
         &   (a2-a)*(b-b1)*(c2-c)*(d-d1) * array(i_a1,i_b2,i_c1,i_d2) + &
         &   (a2-a)*(b-b1)*(c2-c)*(d2-d) * array(i_a1,i_b2,i_c1,i_d1) + &
         &   (a2-a)*(b2-b)*(c-c1)*(d-d1) * array(i_a1,i_b1,i_c2,i_d2) + &
         &   (a2-a)*(b2-b)*(c-c1)*(d2-d) * array(i_a1,i_b1,i_c2,i_d1) + &
         &   (a2-a)*(b2-b)*(c2-c)*(d-d1) * array(i_a1,i_b1,i_c1,i_d2) + &
         &   (a2-a)*(b2-b)*(c2-c)*(d2-d) * array(i_a1,i_b1,i_c1,i_d1) &
         & )

  END FUNCTION fun_interp_4D
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LINEARILY INTERPOLATE IN 5 DIMENSIONAL SPACE
  FUNCTION fun_interp_5D(array,a,b,c,d,e,a_max,b_max,c_max,d_max,e_max, &
       & i_a_min,i_a_max,i_b_min,i_b_max,i_c_min,i_c_max,i_d_min,i_d_max,i_e_min,i_e_max)
    IMPLICIT NONE
    ! result variable
    REAL::fun_interp_5D
    ! dummy arguments
    INTEGER,INTENT(in)::i_a_min,i_a_max
    INTEGER,INTENT(in)::i_b_min,i_b_max
    INTEGER,INTENT(in)::i_c_min,i_c_max
    INTEGER,INTENT(in)::i_d_min,i_d_max
    INTEGER,INTENT(in)::i_e_min,i_e_max
    REAL,INTENT(in),DIMENSION(i_a_min:i_a_max,i_b_min:i_b_max, &
         & i_c_min:i_c_max,i_d_min:i_d_max,i_e_min:i_e_max)::array
    REAL,INTENT(in)::a
    REAL,INTENT(in)::b
    REAL,INTENT(in)::c
    REAL,INTENT(in)::d
    REAL,INTENT(in)::e
    REAL,INTENT(in)::a_max
    REAL,INTENT(in)::b_max
    REAL,INTENT(in)::c_max
    REAL,INTENT(in)::d_max
    REAL,INTENT(in)::e_max
    ! local variables
    integer::i_a,i_b,i_c,i_d,i_e
    REAL::a1,a2
    REAL::b1,b2
    REAL::c1,c2
    REAL::d1,d2
    REAL::e1,e2
    INTEGER::i_a1,i_a2
    INTEGER::i_b1,i_b2
    INTEGER::i_c1,i_c2
    INTEGER::i_d1,i_d2
    INTEGER::i_e1,i_e2

    ! *** calculate grid points enclosing the passes point coordinates ***
    ! calculate integer values each point coordinate
    i_a = INT(a * (1.0 / a_max) * i_a_max)
    i_b = INT(b * (1.0 / b_max) * i_b_max)
    i_c = INT(c * (1.0 / c_max) * i_c_max)
    i_d = INT(d * (1.0 / d_max) * i_d_max)
    i_e = INT(e * (1.0 / e_max) * i_e_max)
    ! calculate:
    ! (a) bounding grid points along each dimension ('i_x1' and 'i_x2')
    !     NOTE: if the possition of the point along any one dimensions is on or past the
    !           boundary of that dimension, then both grid points are set to the boundary grid point
    ! (b) bounding points of the interval containing the point in question ('x1' and 'x2')
    !     NOTE: the interval between these two points is fixed at the resolution along that dimension
    !           of the look-up table, even if the point in question lies outside the bounding space
    !           (this is to prevent divide-by-zero errors in the interpolation)
    ! (c) if the position of the point in question falls outside of the table boundary along any dimension,
    !     the value at the required point is estimated via a linear extrapolation
    !     using the last two points long that particular dimension
    ! parameter 'a'
    IF (a >= a_max) THEN
       i_a1 = i_a_max - 1
       i_a2 = i_a_max
       a1   = i_a1 * (1.0 / i_a_max) * a_max
       a2   = i_a2 * (1.0 / i_a_max) * a_max
    ELSE IF (a < (i_a_min * (a_max / i_a_max))) THEN
       i_a1 = i_a_min
       i_a2 = i_a_min + 1
       a1   = i_a1 * (1.0 / i_a_max) * a_max
       a2   = i_a2 * (1.0 / i_a_max) * a_max
    ELSE IF (a < 0.0) THEN
       i_a1 = i_a - 1
       i_a2 = i_a
       a1   = i_a1 * (1.0 / i_a_max) * a_max
       a2   = i_a2 * (1.0 / i_a_max) * a_max
    ELSE
       i_a1 = i_a
       i_a2 = i_a + 1
       a1   = i_a1 * (1.0 / i_a_max) * a_max
       a2   = i_a2 * (1.0 / i_a_max) * a_max
    ENDIF
    ! parameter 'b'
    IF (b >= b_max) THEN
       i_b1 = i_b_max - 1
       i_b2 = i_b_max
       b1   = i_b1 * (1.0 / i_b_max) * b_max
       b2   = i_b2 * (1.0 / i_b_max) * b_max
    ELSE IF (b < (i_b_min * (b_max / i_b_max))) THEN
       i_b1 = i_b_min
       i_b2 = i_b_min + 1
       b1   = i_b1 * (1.0 / i_b_max) * b_max
       b2   = i_b2 * (1.0 / i_b_max) * b_max
    ELSE IF (b < 0.0) THEN
       i_b1 = i_b - 1
       i_b2 = i_b
       b1   = i_b1 * (1.0 / i_b_max) * b_max
       b2   = i_b2 * (1.0 / i_b_max) * b_max
    ELSE
       i_b1 = i_b
       i_b2 = i_b + 1
       b1   = i_b1 * (1.0 / i_b_max) * b_max
       b2   = i_b2 * (1.0 / i_b_max) * b_max
    ENDIF
    ! parameter 'c'
    IF (c >= c_max) THEN
       i_c1 = i_c_max - 1
       i_c2 = i_c_max
       c1   = (i_c1 - 1) * (1.0 / i_c_max) * c_max
       c2   = (i_c2 - 0) * (1.0 / i_c_max) * c_max
    ELSE IF (c < (i_c_min * (c_max / i_c_max))) THEN
       i_c1 = i_c_min
       i_c2 = i_c_min + 1
       c1   = i_c1 * (1.0 / i_c_max) * c_max
       c2   = i_c2 * (1.0 / i_c_max) * c_max
    ELSE IF (c < 0.0) THEN
       i_c1 = i_c - 1
       i_c2 = i_c
       c1   = i_c1 * (1.0 / i_c_max) * c_max
       c2   = i_c2 * (1.0 / i_c_max) * c_max
    ELSE
       i_c1 = i_c
       i_c2 = i_c + 1
       c1   = i_c1 * (1.0 / i_c_max) * c_max
       c2   = i_c2 * (1.0 / i_c_max) * c_max
    ENDIF
    ! parameter 'd'
    IF (d >= d_max) THEN
       i_d1 = i_d_max - 1
       i_d2 = i_d_max
       d1   = i_d1 * (1.0 / i_d_max) * d_max
       d2   = i_d2 * (1.0 / i_d_max) * d_max
    ELSE IF (d < (i_d_min * (d_max / i_d_max))) THEN
       i_d1 = i_d_min
       i_d2 = i_d_min + 1
       d1   = i_d1 * (1.0 / i_d_max) * d_max
       d2   = i_d2 * (1.0 / i_d_max) * d_max
    ELSE IF (d < 0.0) THEN
       i_d1 = i_d - 1
       i_d2 = i_d
       d1   = i_d1 * (1.0 / i_d_max) * d_max
       d2   = i_d2 * (1.0 / i_d_max) * d_max
    ELSE
       i_d1 = i_d
       i_d2 = i_d + 1
       d1   = i_d1 * (1.0 / i_d_max) * d_max
       d2   = i_d2 * (1.0 / i_d_max) * d_max
    ENDIF
    ! parameter 'e'
    IF (e >= e_max) THEN
       i_e1 = i_e_max - 1
       i_e2 = i_e_max
       e1   = i_e1 * (1.0 / i_e_max) * e_max
       e2   = i_e2 * (1.0 / i_e_max) * e_max
    ELSE IF (e < (i_e_min * (e_max / i_e_max))) THEN
       i_e1 = i_e_min
       i_e2 = i_e_min + 1
       e1   = i_e1 * (1.0 / i_e_max) * e_max
       e2   = i_e2 * (1.0 / i_e_max) * e_max
    ELSE IF (e < 0.0) THEN
       i_e1 = i_e - 1
       i_e2 = i_e
       e1   = i_e1 * (1.0 / i_e_max) * e_max
       e2   = i_e2 * (1.0 / i_e_max) * e_max
    ELSE
       i_e1 = i_e
       i_e2 = i_e + 1
       e1   = i_e1 * (1.0 / i_e_max) * e_max
       e2   = i_e2 * (1.0 / i_e_max) * e_max
    ENDIF

    ! *** return function value ***
    ! interpolate
    ! NOTE: see 'Applied Numerical Methods with Software' by Nakamura for details of 1-D and 2-D interpolation
    fun_interp_5D = (1.0 / ((a2-a1)*(b2-b1)*(c2-c1)*(d2-d1)*(e2-e1))) * &
         & ( &
         &   (a-a1)*(b-b1)*(c-c1)*(d-d1)*(e-e1) * array(i_a2,i_b2,i_c2,i_d2,i_e2) + &
         &   (a-a1)*(b-b1)*(c-c1)*(d-d1)*(e2-e) * array(i_a2,i_b2,i_c2,i_d2,i_e1) + &
         &   (a-a1)*(b-b1)*(c-c1)*(d2-d)*(e-e1) * array(i_a2,i_b2,i_c2,i_d1,i_e2) + &
         &   (a-a1)*(b-b1)*(c-c1)*(d2-d)*(e2-e) * array(i_a2,i_b2,i_c2,i_d1,i_e1) + &
         &   (a-a1)*(b-b1)*(c2-c)*(d-d1)*(e-e1) * array(i_a2,i_b2,i_c1,i_d2,i_e2) + &
         &   (a-a1)*(b-b1)*(c2-c)*(d-d1)*(e2-e) * array(i_a2,i_b2,i_c1,i_d2,i_e1) + &
         &   (a-a1)*(b-b1)*(c2-c)*(d2-d)*(e-e1) * array(i_a2,i_b2,i_c1,i_d1,i_e2) + &
         &   (a-a1)*(b-b1)*(c2-c)*(d2-d)*(e2-e) * array(i_a2,i_b2,i_c1,i_d1,i_e1) + &
         &   (a-a1)*(b2-b)*(c-c1)*(d-d1)*(e-e1) * array(i_a2,i_b1,i_c2,i_d2,i_e2) + &
         &   (a-a1)*(b2-b)*(c-c1)*(d-d1)*(e2-e) * array(i_a2,i_b1,i_c2,i_d2,i_e1) + &
         &   (a-a1)*(b2-b)*(c-c1)*(d2-d)*(e-e1) * array(i_a2,i_b1,i_c2,i_d1,i_e2) + &
         &   (a-a1)*(b2-b)*(c-c1)*(d2-d)*(e2-e) * array(i_a2,i_b1,i_c2,i_d1,i_e1) + &
         &   (a-a1)*(b2-b)*(c2-c)*(d-d1)*(e-e1) * array(i_a2,i_b1,i_c1,i_d2,i_e2) + &
         &   (a-a1)*(b2-b)*(c2-c)*(d-d1)*(e2-e) * array(i_a2,i_b1,i_c1,i_d2,i_e1) + &
         &   (a-a1)*(b2-b)*(c2-c)*(d2-d)*(e-e1) * array(i_a2,i_b1,i_c1,i_d1,i_e2) + &
         &   (a-a1)*(b2-b)*(c2-c)*(d2-d)*(e2-e) * array(i_a2,i_b1,i_c1,i_d1,i_e1) + &
         &   (a2-a)*(b-b1)*(c-c1)*(d-d1)*(e-e1) * array(i_a1,i_b2,i_c2,i_d2,i_e2) + &
         &   (a2-a)*(b-b1)*(c-c1)*(d-d1)*(e2-e) * array(i_a1,i_b2,i_c2,i_d2,i_e1) + &
         &   (a2-a)*(b-b1)*(c-c1)*(d2-d)*(e-e1) * array(i_a1,i_b2,i_c2,i_d1,i_e2) + &
         &   (a2-a)*(b-b1)*(c-c1)*(d2-d)*(e2-e) * array(i_a1,i_b2,i_c2,i_d1,i_e1) + &
         &   (a2-a)*(b-b1)*(c2-c)*(d-d1)*(e-e1) * array(i_a1,i_b2,i_c1,i_d2,i_e2) + &
         &   (a2-a)*(b-b1)*(c2-c)*(d-d1)*(e2-e) * array(i_a1,i_b2,i_c1,i_d2,i_e1) + &
         &   (a2-a)*(b-b1)*(c2-c)*(d2-d)*(e-e1) * array(i_a1,i_b2,i_c1,i_d1,i_e2) + &
         &   (a2-a)*(b-b1)*(c2-c)*(d2-d)*(e2-e) * array(i_a1,i_b2,i_c1,i_d1,i_e1) + &
         &   (a2-a)*(b2-b)*(c-c1)*(d-d1)*(e-e1) * array(i_a1,i_b1,i_c2,i_d2,i_e2) + &
         &   (a2-a)*(b2-b)*(c-c1)*(d-d1)*(e2-e) * array(i_a1,i_b1,i_c2,i_d2,i_e1) + &
         &   (a2-a)*(b2-b)*(c-c1)*(d2-d)*(e-e1) * array(i_a1,i_b1,i_c2,i_d1,i_e2) + &
         &   (a2-a)*(b2-b)*(c-c1)*(d2-d)*(e2-e) * array(i_a1,i_b1,i_c2,i_d1,i_e1) + &
         &   (a2-a)*(b2-b)*(c2-c)*(d-d1)*(e-e1) * array(i_a1,i_b1,i_c1,i_d2,i_e2) + &
         &   (a2-a)*(b2-b)*(c2-c)*(d-d1)*(e2-e) * array(i_a1,i_b1,i_c1,i_d2,i_e1) + &
         &   (a2-a)*(b2-b)*(c2-c)*(d2-d)*(e-e1) * array(i_a1,i_b1,i_c1,i_d1,i_e2) + &
         &   (a2-a)*(b2-b)*(c2-c)*(d2-d)*(e2-e) * array(i_a1,i_b1,i_c1,i_d1,i_e1) &
         & )

  END FUNCTION fun_interp_5D
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** GRID ROUTINES ************************************************************************************************************ !
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! GET LON CENTRE GRID VECTOR
  FUNCTION fun_get_grid_lon(dum_n_i)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    integer,INTENT(in)::dum_n_i                                  !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,dimension(1:dum_n_i)::fun_get_grid_lon               !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::i
    ! -------------------------------------------------------- !
    ! CALCULATE AND RETURN GRID VARIABLE
    ! -------------------------------------------------------- !
    DO i=1,dum_n_i
       fun_get_grid_lon(i) = (360.0/dum_n_i)*(real(i)-0.5) + par_grid_lon_offset
    end DO
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_get_grid_lon
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! GET LON EDGE GRID VECTOR
  FUNCTION fun_get_grid_lone(dum_n_i)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    integer,INTENT(in)::dum_n_i                                  !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,dimension(1:dum_n_i+1)::fun_get_grid_lone            !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::i
    ! -------------------------------------------------------- !
    ! CALCULATE AND RETURN GRID VARIABLE
    ! -------------------------------------------------------- !
    DO i=1,dum_n_i+1
       fun_get_grid_lone(i) = (360.0/dum_n_i)*real(i-1) + par_grid_lon_offset
    end DO
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_get_grid_lone
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! GET LAT CENTRE GRID VECTOR
  FUNCTION fun_get_grid_lat(dum_n_j)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    integer,INTENT(in)::dum_n_j                                !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,dimension(1:dum_n_j)::fun_get_grid_lat                !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::j
    real::loc_grid_ds
    real,dimension(1:dum_n_j)::loc_grid_sv,loc_grid_s          !
    ! -------------------------------------------------------- !
    ! CALCULATE LOCAL GRID CONSTANTS
    ! -------------------------------------------------------- !
    loc_grid_ds = (sin(const_pi/2.0) - sin(-const_pi/2.0))/dum_n_j
    do j=1,dum_n_j
       loc_grid_sv(j) = sin(-const_pi/2.0) + j*loc_grid_ds
       loc_grid_s(j)  = loc_grid_sv(j) - 0.5*loc_grid_ds;
    end do
    ! -------------------------------------------------------- !
    ! CALCULATE AND RETURN GRID VARIABLE
    ! -------------------------------------------------------- !
    DO j=1,dum_n_j
       fun_get_grid_lat(j) = (180.0/const_pi)*ASIN(loc_grid_s(j))
    end DO
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_get_grid_lat
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! GET LAT EDGE GRID VECTOR
  FUNCTION fun_get_grid_late(dum_n_j)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    integer,INTENT(in)::dum_n_j                                !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,dimension(1:dum_n_j+1)::fun_get_grid_late             !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::j
    real::loc_grid_ds
    real,dimension(1:dum_n_j+1)::loc_grid_sv                   !
    ! -------------------------------------------------------- !
    ! CALCULATE LOCAL GRID CONSTANTS
    ! -------------------------------------------------------- !
    loc_grid_ds = (sin(const_pi/2.0) - sin(-const_pi/2.0))/dum_n_j
    do j=1,dum_n_j+1
       loc_grid_sv(j) = sin(-const_pi/2.0) + (j-1)*loc_grid_ds
    end do
    ! -------------------------------------------------------- !
    ! CALCULATE AND RETURN GRID VARIABLE
    ! -------------------------------------------------------- !
    DO j=1,dum_n_j+1
       fun_get_grid_late(j) = (180.0/const_pi)*ASIN(loc_grid_sv(j))
    end DO
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_get_grid_late
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! GET LON CENTRE GRID VECTOR
  FUNCTION fun_get_grid_z(dum_n_k,dum_dsc)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    integer,INTENT(in)::dum_n_k                                !
    real,intent(in)::dum_dsc                                   ! max ocean depth (m)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,dimension(1:dum_n_k)::fun_get_grid_z                  ! equivalent to GOLDSTEIN variable zro
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k                                                 ! local counting variable
    real::loc_ez0
    real::loc_z1
    real::loc_tv1,loc_tv2,loc_tv3,loc_tv4,loc_tv5
    real,dimension(1:dum_n_k)::loc_dz,loc_dza
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- ! For variable (exponential) dz use ez0 > 0, else use ez0 < 0
    loc_ez0 = 0.1
    ! -------------------------------------------------------- !
    ! CALCULATE AND RETURN GRID VECTOR
    ! -------------------------------------------------------- !
    ! tv3 is the depth of the kth w level from the top
    ! tv5 is the depth of the k+1th density level from the top
    loc_z1 = loc_ez0*((1.0 + 1.0/loc_ez0)**(1.0/real(dum_n_k)) - 1.0)
    loc_tv1 = 0.0
    loc_tv2 = 0.0
    loc_tv4 = loc_ez0*((loc_z1/loc_ez0 + 1.0)**0.5 - 1.0)
    do k=1,dum_n_k
       if (loc_ez0 > 0.0) then
          loc_tv3 = loc_ez0*((loc_z1/loc_ez0+1)**real(k) - 1.0)
          loc_dz(dum_n_k-k+1) = loc_tv3 - loc_tv2
          loc_tv2 = loc_tv3
          loc_tv5 = loc_ez0*((loc_z1/loc_ez0+1)**(real(k)+0.5) - 1.0)
          if(k < dum_n_k) then
             loc_dza(dum_n_k-k) = loc_tv5 - loc_tv4
          else
             loc_dza(k) = loc_dz(dum_n_k)/2.0
          end if
          loc_tv4 = loc_tv5
          loc_tv1 = loc_tv1 + loc_dz(dum_n_k-k+1)
       else
          loc_dz(k)  = 1.0/real(dum_n_k)
          loc_dza(k) = 1.0/real(dum_n_k)
       endif
    enddo
    DO k=1,dum_n_k
       fun_get_grid_z(k) = dum_dsc*SUM(loc_dza(k:dum_n_k))
    end DO
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_get_grid_z
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! GET LON EDGE GRID VECTOR
  FUNCTION fun_get_grid_ze(dum_n_k,dum_dsc)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    integer,INTENT(in)::dum_n_k                                !
    real,intent(in)::dum_dsc                                   ! max ocean depth (m)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,dimension(0:dum_n_k)::fun_get_grid_ze               !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::k                                                 ! local counting variable
    real::loc_ez0
    real::loc_z1
    real::loc_tv1,loc_tv2,loc_tv3,loc_tv4,loc_tv5
    real,dimension(1:dum_n_k)::loc_dz,loc_dza
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- ! For variable (exponential) dz use ez0 > 0, else use ez0 < 0
    loc_ez0 = 0.1
    ! -------------------------------------------------------- !
    ! CALCULATE AND RETURN GRID VECTOR
    ! -------------------------------------------------------- !
    ! tv3 is the depth of the kth w level from the top
    ! tv5 is the depth of the k+1th density level from the top
    loc_z1 = loc_ez0*((1.0 + 1.0/loc_ez0)**(1.0/real(dum_n_k)) - 1.0)
    loc_tv1 = 0.0
    loc_tv2 = 0.0
    loc_tv4 = loc_ez0*((loc_z1/loc_ez0 + 1.0)**0.5 - 1.0)
    do k=1,dum_n_k
       if (loc_ez0 > 0.0) then
          loc_tv3 = loc_ez0*((loc_z1/loc_ez0+1)**real(k) - 1.0)
          loc_dz(dum_n_k-k+1) = loc_tv3 - loc_tv2
          loc_tv2 = loc_tv3
          loc_tv5 = loc_ez0*((loc_z1/loc_ez0+1)**(real(k)+0.5) - 1.0)
          if(k < dum_n_k) then
             loc_dza(dum_n_k-k) = loc_tv5 - loc_tv4
          else
             loc_dza(k) = loc_dz(dum_n_k)/2.0
          end if
          loc_tv4 = loc_tv5
          loc_tv1 = loc_tv1 + loc_dz(dum_n_k-k+1)
       else
          loc_dz(k)  = 1.0/real(dum_n_k)
          loc_dza(k) = 1.0/real(dum_n_k)
       endif
    enddo
    DO k=1,dum_n_k
       fun_get_grid_ze(k-1) = dum_dsc*SUM(loc_dz(k:dum_n_k))
    end DO
    fun_get_grid_ze(dum_n_k) = 0.0
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_get_grid_ze
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** TRACER ROUTINES ********************************************************************************************************** !
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE THE INDICES OF ALL (NON-ZERO) TRACER RELATIONSHIPS
  SUBROUTINE sub_calc_tracerrelationships_i()
    ! local variables
    INTEGER::ia,io,is
    integer::loc_tot_i
    ! zero arrays
    conv_ocn_sed_i(:,:) = 0
    conv_sed_ocn_i(:,:) = 0
    conv_ocn_atm_i(:,:) = 0
    conv_atm_ocn_i(:,:) = 0
    conv_DOM_POM_i(:,:) = 0
    conv_POM_DOM_i(:,:) = 0
    conv_sed_ocn_i_O(:,:)    = 0
    conv_sed_ocn_i_N(:,:)    = 0
    conv_sed_ocn_i_S(:,:)    = 0
    conv_sed_ocn_i_meth(:,:) = 0
    ! identify the indices of all non-zero transformation values in the conversion array for ocn -> sed
    do io=1,n_ocn
       loc_tot_i = 0
       do is=1,n_sed
          if (abs(conv_ocn_sed(is,io)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_ocn_sed_i(loc_tot_i,io) = is
          end if
       end do
       conv_ocn_sed_i(0,io) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for sed -> ocn
    do is=1,n_sed
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(conv_sed_ocn(io,is)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_sed_ocn_i(loc_tot_i,is) = io
          end if
       end do
       conv_sed_ocn_i(0,is) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for ocn -> atm
    do io=1,n_ocn
       loc_tot_i = 0
       do ia=1,n_atm
          if (abs(conv_ocn_atm(ia,io)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_ocn_atm_i(loc_tot_i,io) = ia
          end if
       end do
       conv_ocn_atm_i(0,io) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for atm -> ocn
    do ia=1,n_atm
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(conv_atm_ocn(io,ia)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_atm_ocn_i(loc_tot_i,ia) = io
          end if
       end do
       conv_atm_ocn_i(0,ia) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for DOM -> POM
    do io=1,n_ocn
       loc_tot_i = 0
       do is=1,n_sed
          if (abs(conv_DOM_POM(is,io)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_DOM_POM_i(loc_tot_i,io) = is
          end if
       end do
       conv_DOM_POM_i(0,io) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for POM -> DOM
    do is=1,n_sed
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(conv_POM_DOM(io,is)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_POM_DOM_i(loc_tot_i,is) = io
          end if
       end do
       conv_POM_DOM_i(0,is) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for RDOM -> POM
    do io=1,n_ocn
       loc_tot_i = 0
       do is=1,n_sed
          if (abs(conv_RDOM_POM(is,io)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_RDOM_POM_i(loc_tot_i,io) = is
          end if
       end do
       conv_RDOM_POM_i(0,io) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for POM -> RDOM
    do is=1,n_sed
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(conv_POM_RDOM(io,is)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_POM_RDOM_i(loc_tot_i,is) = io
          end if
       end do
       conv_POM_RDOM_i(0,is) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for sed -> ocn
    ! NOTE: oxic conditions
    do is=1,n_sed
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(conv_sed_ocn_O(io,is)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_sed_ocn_O(loc_tot_i,is) = io
          end if
       end do
       conv_sed_ocn_i_O(0,is) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for sed -> ocn
    ! NOTE: N-reduction redox conditions
    do is=1,n_sed
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(conv_sed_ocn_N(io,is)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_sed_ocn_N(loc_tot_i,is) = io
          end if
       end do
       conv_sed_ocn_i_N(0,is) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for sed -> ocn
    ! NOTE: S-reduction redox conditions
    do is=1,n_sed
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(conv_sed_ocn_S(io,is)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_sed_ocn_i_S(loc_tot_i,is) = io
          end if
       end do
       conv_sed_ocn_i_S(0,is) = loc_tot_i
    end do
    ! identify the indices of all non-zero transformation values in the conversion array for sed -> ocn
    ! NOTE: methanogenesis
    do is=1,n_sed
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(conv_sed_ocn_meth(io,is)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             conv_sed_ocn_i_meth(loc_tot_i,is) = io
          end if
       end do
       conv_sed_ocn_i_meth(0,is) = loc_tot_i
    end do
  END SUBROUTINE sub_calc_tracerrelationships_i
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! RE-CALCULATE THE INDICES OF ALL (NON-ZERO) TRACER SED->OCN RELATIONSHIPS
  FUNCTION fun_recalc_tracerrelationships_i(dum_conv_sed_ocn)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real,dimension(1:n_ocn,1:n_sed),INTENT(in)::dum_conv_sed_ocn              !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    integer,dimension(0:n_ocn,0:n_sed)::fun_recalc_tracerrelationships_i         !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::io,is
    integer::loc_tot_i
    integer,dimension(0:n_ocn,0:n_sed)::loc_conv_sed_ocn_i         !
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    loc_conv_sed_ocn_i(:,:) = 0
    ! -------------------------------------------------------- !
    ! Re-CALCULATE INDICES
    ! -------------------------------------------------------- !
    ! identify the indices of all non-zero transformation values in the conversion array for sed -> ocn
    do is=1,n_sed
       loc_tot_i = 0
       do io=1,n_ocn
          if (abs(dum_conv_sed_ocn(io,is)) > const_real_nullsmall) then
             loc_tot_i = loc_tot_i + 1
             loc_conv_sed_ocn_i(loc_tot_i,is) = io
          end if
       end do
       loc_conv_sed_ocn_i(0,is) = loc_tot_i
    end do
    ! return function result
    fun_recalc_tracerrelationships_i = loc_conv_sed_ocn_i
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_recalc_tracerrelationships_i
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT TO COMPACT INDEX FORMAT
  FUNCTION fun_conv_sedocn2lslo(dum_sed_ocn)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real,dimension(n_ocn,n_sed),INTENT(in)::dum_sed_ocn !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,dimension(n_l_ocn,n_l_sed)::fun_conv_sedocn2lslo !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::io,is
    real,dimension(n_l_ocn,n_l_sed)::loc_lslo               !
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    loc_lslo(:,:) = 0
    ! -------------------------------------------------------- !
    ! TRANSFORM INDICES
    ! -------------------------------------------------------- !
    ! re-index array to compact tracer format
    do is=1,n_sed
       do io=1,n_ocn
          if (ocn_select(io) .AND. sed_select(is) .AND. (abs(dum_sed_ocn(io,is)) > const_real_nullsmall)) then
             loc_lslo(io2l(io),is2l(is)) = dum_sed_ocn(io,is)
          end if
       end do
    end do
    ! return function result
    fun_conv_sedocn2lslo = loc_lslo
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_conv_sedocn2lslo
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT TO COMPACT INDEX FORMAT
  FUNCTION fun_conv_sedocn2lslo_i(dum_sed_ocn_i)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    integer,dimension(0:n_ocn,0:n_sed),INTENT(in)::dum_sed_ocn_i !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    integer,dimension(0:n_l_ocn,0:n_l_sed)::fun_conv_sedocn2lslo_i !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::io,is
    integer,dimension(0:n_l_ocn,0:n_l_sed)::loc_lslo_i               !
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    loc_lslo_i(:,:) = 0
    ! -------------------------------------------------------- !
    ! TRANSFORM INDICES
    ! -------------------------------------------------------- !
    ! re-index array to compact tracer format
    ! NOTE: the counter 'io' here is only a count of the number of ocean tracer, and not s epcific ocean tracer
    !       (the specific ocean tracer number is held in dum_sed_ocn_i(io,is)
    !       hence, it is dum_sed_ocn_i(io,is) that is converted to the compact tracer numbering format for ocean tracers
    !       ('is' is converted to the compact tracer numbering format for solid tracers as normal)
    do is=1,n_sed
       loc_lslo_i(0,is2l(is)) = dum_sed_ocn_i(0,is)
       do io=1,n_ocn
          if (abs(dum_sed_ocn_i(io,is)) > 0) then
             loc_lslo_i(io,is2l(is)) = io2l(dum_sed_ocn_i(io,is))
          end if
       end do
    end do
    ! return function result
    fun_conv_sedocn2lslo_i = loc_lslo_i
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_conv_sedocn2lslo_i
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT TO COMPACT INDEX FORMAT
  FUNCTION fun_conv_ocnsed2lols(dum_ocn_sed)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real,dimension(n_sed,n_ocn),INTENT(in)::dum_ocn_sed !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,dimension(n_l_sed,n_l_ocn)::fun_conv_ocnsed2lols !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::io,is
    real,dimension(n_l_sed,n_l_ocn)::loc_lols               !
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    loc_lols(:,:) = 0
    ! -------------------------------------------------------- !
    ! TRANSFORM INDICES
    ! -------------------------------------------------------- !
    ! re-index array to compact tracer format
    do io=1,n_ocn
       do is=1,n_sed
          if (ocn_select(io) .AND. sed_select(is) .AND. (abs(dum_ocn_sed(is,io)) > const_real_nullsmall)) then
             loc_lols(is2l(is),io2l(io)) = dum_ocn_sed(is,io)
          end if
       end do
    end do
    ! return function result
    fun_conv_ocnsed2lols = loc_lols
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_conv_ocnsed2lols
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT TO COMPACT INDEX FORMAT
  FUNCTION fun_conv_ocnsed2lols_i(dum_ocn_sed_i)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    integer,dimension(0:n_sed,0:n_ocn),INTENT(in)::dum_ocn_sed_i !
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    integer,dimension(0:n_l_sed,0:n_l_ocn)::fun_conv_ocnsed2lols_i !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::io,is
    integer,dimension(0:n_l_sed,0:n_l_ocn)::loc_lols_i               !
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    loc_lols_i(:,:) = 0
    ! -------------------------------------------------------- !
    ! TRANSFORM INDICES
    ! -------------------------------------------------------- !
    ! re-index array to compact tracer format
    do io=1,n_ocn
       do is=1,n_sed
          loc_lols_i(0,io2l(io)) = dum_ocn_sed_i(0,io)
          if (ocn_select(io) .AND. sed_select(is) .AND. (abs(dum_ocn_sed_i(is,io)) > 0)) then
             loc_lols_i(is2l(is),io2l(io)) = is2l(dum_ocn_sed_i(is,io))
          end if
       end do
    end do
    ! return function result
    fun_conv_ocnsed2lols_i = loc_lols_i
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_conv_ocnsed2lols_i
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DEFINE AND INITIALIZE ATMOSPHERIC (atm) TRACERS
  SUBROUTINE sub_init_tracer_atm()
    ! local variables
    INTEGER::n,ia,l
    INTEGER::loc_n_elements,loc_n_start
    INTEGER::loc_index,loc_dep,loc_type
    real::loc_min,loc_max
    CHARACTER(len=16)::loc_string_name
    CHARACTER(len=128)::loc_string_longname
    CHARACTER(len=12)::loc_string_unit
    CHARACTER(len=255)::loc_filename
    ! initialize global arrays
    atm_dep(:)    = 0
    atm_type(:)   = 0
    string_atm_tname(:)  = ' '
    string_atm_unit(:)   = ' '
    string_atm_tlname(:) = ' '
    atm_mima(:,:)        = 0.0
    conv_ia_lselected(:) = 0
    ! check file format and determine number of lines of data
    loc_filename = TRIM(par_gem_indir_name)//'tracer_define.atm'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=loc_filename,action='read')
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! zero selected tracer counter
    l = 0
    ! count number of incuded ('active') tracers
    DO n = 1,loc_n_elements
       IF (atm_select(n)) THEN
          l = l + 1
       end if
    END DO
    ! set number of active tracers and allocate tracer index conversion array size
    n_l_atm = l
    ALLOCATE(conv_iselected_ia(n_l_atm),STAT=error)
    ALLOCATE(l2ia(n_l_atm),STAT=error)
    ! re-set filepipe
    REWIND(unit=in)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! zero selected tracer counter
    l = 0
    ! read in atmosphere tracer selection
    ! NOTE: assign string regardless of whether the tracer is 'selected' or not
    DO n = 1,loc_n_elements
       READ(unit=in,FMT=*)         &
            & loc_string_name,     & ! COLUMN #01: tracer variable name
            & loc_index,           & ! COLUMN #02: tracer variable identifier
            & loc_dep,             & ! COLUMN #03: tracer variable dependencies
            & loc_type,            & ! COLUMN #04: tracer variable type
            & loc_string_longname, & ! COLUMN #05: long tracer name
            & loc_string_unit,     & ! COLUMN #06: tracer unit
            & loc_min,             & ! COLUMN #07: tracer min
            & loc_max                ! COLUMN #08: tracer max
       ia = loc_index
       string_atm(ia) = loc_string_name
       string_longname_atm(ia) = loc_string_longname
       atm_dep(ia) = loc_dep
       atm_type(ia) = loc_type
       IF (atm_select(ia)) THEN
          l = l + 1
          conv_iselected_ia(l) = ia
          l2ia(l) = ia
          conv_ia_lselected(ia) = l
          ia2l(ia) = l
          string_atm_tname(l) = loc_string_name
          string_atm_unit(l) = loc_string_unit
          string_atm_tlname(l) = loc_string_longname
          atm_mima(l,1) = loc_min
          atm_mima(l,2) = loc_max
       ENDIF
    END DO
    ! close file pipe
    CLOSE(unit=in)
    ! isotope parameter selection consistency check
    do ia=1,n_atm
       IF (atm_select(ia)) THEN
          if (.not. atm_select(atm_dep(ia))) then
             CALL sub_report_error( &
                  & 'atchem_data','sub_init_tracer_atm', &
                  & 'If an isotopic tracer is selected, the associated bulk atmosphere tracer '// &
                  & TRIM(string_atm(atm_dep(ia)))//' '// &
                  & 'must be selected (FILE: gem_config_atm.par)', &
                  & 'OFFENDING TRACER HAS BEEN DE-SELECTED', &
                  & (/const_real_null/),.false. &
                  & )
             atm_select(ia) = .FALSE.
          end if
       end IF
    end do
  END SUBROUTINE sub_init_tracer_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DEFINE AND INITIALIZE OCEAN (ocn) TRACERS
  SUBROUTINE sub_init_tracer_ocn()
    ! local variables
    INTEGER::n,io,l
    INTEGER::loc_n_elements,loc_n_start
    INTEGER::loc_index,loc_dep,loc_type
    real::loc_min,loc_max
    CHARACTER(len=16)::loc_string_name
    CHARACTER(len=128)::loc_string_longname
    CHARACTER(len=12)::loc_string_unit
    CHARACTER(len=255)::loc_filename
    ! initialize global arrays
    ocn_dep(:)    = 0
    ocn_type(:)   = 0
    string_ocn_tname(:)  = ' '
    string_ocn_unit(:)   = ' '
    string_ocn_tlname(:) = ' '
    ocn_mima(:,:)        = 0.0
    conv_io_lselected(:) = 0
    ! check file format and determine number of lines of data
    loc_filename = TRIM(par_gem_indir_name)//'tracer_define.ocn'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=loc_filename,action='read')
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! zero selected tracer counter
    l = 0
    ! count number of incuded ('active') tracers
    DO n = 1,loc_n_elements
       IF (ocn_select(n)) THEN
          l = l + 1
       end if
    END DO
    ! set number of active tracers and allocate tracer index conversion array size
    n_l_ocn = l
    ALLOCATE(conv_iselected_io(n_l_ocn),STAT=error)
    ALLOCATE(l2io(n_l_ocn),STAT=error)
    ! re-set filepipe
    REWIND(unit=in)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! zero selected tracer counter
    l = 0
    ! read in ocean tracer selection
    ! NOTE: assign string regardless of whether the tracer is 'selected' or not
    DO n = 1,loc_n_elements
       READ(unit=in,FMT=*)         &
            & loc_string_name,     & ! COLUMN #01: tracer variable name
            & loc_index,           & ! COLUMN #02: tracer variable identifier
            & loc_dep,             & ! COLUMN #03: tracer variable dependencies
            & loc_type,            & ! COLUMN #04: tracer variable type
            & loc_string_longname, & ! COLUMN #05: long tracer name
            & loc_string_unit,     & ! COLUMN #06: tracer unit
            & loc_min,             & ! COLUMN #07: tracer min
            & loc_max                ! COLUMN #08: tracer max
       io = loc_index
       string_ocn(io) = loc_string_name
       string_longname_ocn(io) = loc_string_longname
       ocn_dep(io) = loc_dep
       ocn_type(io) = loc_type
       IF (ocn_select(io)) THEN
          l = l + 1
          conv_iselected_io(l) = io
          l2io(l) = io
          conv_io_lselected(io) = l
          io2l(io) = l
          string_ocn_tname(l) = loc_string_name
          string_ocn_unit(l) = loc_string_unit
          string_ocn_tlname(l) = loc_string_longname
          ocn_mima(l,1) = loc_min
          ocn_mima(l,2) = loc_max
       ENDIF
    END DO
    ! close file pipe
    CLOSE(unit=in)
    ! isotope parameter selection consistency check
    do io=1,n_ocn
       IF (ocn_select(io)) THEN
          if (.not. ocn_select(ocn_dep(io))) then
             CALL sub_report_error( &
                  & 'atchem_data','sub_init_tracer_ocn', &
                  & 'If an isotopic tracer is selected, the associated bulk ocean tracer '// &
                  & TRIM(string_ocn(ocn_dep(io)))//' '// &
                  & 'must be selected (FILE: gem_config_ocn.par)', &
                  & 'OFFENDING TRACER HAS BEEN DE-SELECTED', &
                  & (/const_real_null/),.false. &
                  & )
             ocn_select(io) = .FALSE.
          end if
       end IF
    end do
  END SUBROUTINE sub_init_tracer_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DEFINE AND INITIALIZE SEDIMENT (sed) TRACERS
  SUBROUTINE sub_init_tracer_sed()
    ! local variables
    INTEGER::n,is,l
    INTEGER::loc_n_elements,loc_n_start
    INTEGER::loc_index,loc_type,loc_dep
    CHARACTER(len=16)::loc_string_name
    CHARACTER(len=128)::loc_string_longname
    CHARACTER(len=12)::loc_string_unit
    CHARACTER(len=255)::loc_filename
    real::loc_min, loc_max
    ! initialize global variables
    sed_dep(:)    = 0
    sed_type(:)   = 0
    string_sed_tname(:)  = ' '
    string_sed_unit(:)   = ' '
    string_sed_tlname(:) = ' '
    sed_mima(:,:)        = 0.0
    conv_is_lselected(:) = 0
    ! check file format
    loc_filename = TRIM(par_gem_indir_name)//'tracer_define.sed'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=loc_filename,action='read')
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! zero selected tracer counter
    l = 0
    ! count number of included ('active') tracers
    DO n = 1,loc_n_elements
       IF (sed_select(n)) THEN
          l = l + 1
       end if
    END DO
    ! set number of active tracers and allocate tracer index conversion array size
    n_l_sed = l
    ALLOCATE(conv_iselected_is(n_l_sed),STAT=error)
    ALLOCATE(l2is(n_l_sed),STAT=error)
    ! re-set filepipe
    REWIND(unit=in)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! zero selected tracer counter
    l = 0
    ! read in sediment tracer selection
    ! NOTE: assign string regardless of whether the tracer is 'selected' or not
    DO n = 1,loc_n_elements
       READ(unit=in,FMT=*)         &
            & loc_string_name,     & ! COLUMN #01: tracer variable name
            & loc_index,           & ! COLUMN #02: tracer variable identifier
            & loc_dep,             & ! COLUMN #03: tracer variable dependencies
            & loc_type,            & ! COLUMN #04: tracer variable type
            & loc_string_longname, & ! COLUMN #05: long tracer name
            & loc_string_unit,     & ! COLUMN #06: tracer units
            & loc_min,             & ! COLUMN #07: tracer min
            & loc_max                ! COLUMN #08: tracer max
       is = loc_index
       string_sed(is) = loc_string_name
       string_longname_sed(is) = loc_string_longname
       sed_dep(is) = loc_dep
       sed_type(is) = loc_type
       IF (sed_select(is)) then
          l = l + 1
          conv_iselected_is(l) = is
          l2is(l) = is
          conv_is_lselected(is) = l
          is2l(is) = l
          string_sed_tname(l) = loc_string_name
          string_sed_unit(l) = loc_string_unit
          string_sed_tlname(l) = loc_string_longname
          sed_mima(l,1) = loc_min
          sed_mima(l,2) = loc_max
       end if
    END DO
    ! close file pipe
    CLOSE(unit=in)
    ! isotope parameter selection consistency check
    do is=1,n_sed
       IF (sed_select(is)) THEN
          if (.not. sed_select(sed_dep(is))) then
             CALL sub_report_error( &
                  & 'sedgem_data','sub_init_tracer_sed', &
                  & 'If an isotopic tracer is selected, the associated bulk sediment tracer ' &
                  & //TRIM(string_sed(sed_dep(is)))//' '// &
                  & 'must be selected (FILE: gem_config_sed.par)', &
                  & 'OFFENDING TRACER HAS BEEN DE-SELECTED', &
                  & (/const_real_null/),.false. &
                  & )
             sed_select(is) = .FALSE.
          end if
       end IF
    end do
  END SUBROUTINE sub_init_tracer_sed
  ! ****************************************************************************************************************************** !


END MODULE gem_util

