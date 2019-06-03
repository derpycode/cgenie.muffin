! ******************************************************************************************************************************** !
! gem_carbchem.f90
! Carbonate chemistry Module
! AQUEOUS CARBONATE CHEMISTRY ROUTINES
! SEE: Ridgwell, A., J. Hargreaves, N. Edwards, J. Annan, T. Lenton, R. Marsh, A. Yool, and A. Watson,
!      Marine geochemical data assimilation in an efficient Earth System Model of global biogeochemical cycling,
!      Biogeosciences 4, 87-104 (2007)
! ******************************************************************************************************************************** !


MODULE gem_carbchem


  use gem_cmn
  use gem_util
  IMPLICIT NONE
  SAVE


  ! ****************************************************************************************************************************** !
  ! DEFINE COEFFICIENTS FOR PRESSURE CORRECTION OF DISSOCIATION CONSTANTS
  ! NOTE: parameter values for the effect of pressure on K1 and K2 are from Millero [1995]
  ! NOTE: parameter values for the effect of pressure on KB are from Millero [1979, 1995] (but without the salinity dependence)
  ! NOTE: parameter values for the effect of pressure on KW are from Millero [1983]
  ! NOTE: parameter values for the effect of pressure on KSi are assumed to be the same as for KB
  ! NOTE: parameter values for the effect of pressure on KHF and KHSO4 are from Millero [1995]
  ! NOTE: parameter values for the effect of pressure on KP1, KP2, and KP3 are from Millero [1995]
  ! NOTE: parameter values for the effect of pressure on Ksp for calcite are from Ingle [1975]
  ! NOTE: parameter values for the effect of pressure on Ksp for aragonite are from Millero [1979]
  ! NOTE: parameter values for the effect of pressure on KHS are from Millero [1983]
  ! NOTE: parameter values for the effect of pressure on KHN4 are from Millero [1995]
  ! NOTE: all pressure correction choices follow Lewis and Wallace [1998] ('CO2SYS.EXE' program) whereever possible
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpH2CO3    = (/ -2.550E+1, +1.271E-1, +0.000E+0, -3.080E+0, +8.770E-2 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpHCO3     = (/ -1.582E+1, -2.190E-2, +0.000E+0, +1.130E+0, -1.475E-1 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpBO3H3    = (/ -2.948E+1, +1.622E-1, +2.608E-3, -2.840E+0, +0.000E+0 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpH2O      = (/ -2.002E+1, +1.119E-1, -1.409E-3, -5.130E+0, +7.940E-2 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpH4SiO4   = (/ -2.948E+1, +1.622E-1, +2.608E-3, -2.840E+0, +0.000E+0 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpHSO4     = (/ -1.803E+1, +4.660E-2, +3.160E-4, -4.530E+0, +9.000E-2 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpHF       = (/ -9.780E+0, -9.000E-3, -9.420E-4, -3.910E+0, +5.400E-2 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpH3PO4    = (/ -1.451E+1, +1.211E-1, -3.210E-4, -2.670E+0, +4.270E-2 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpH2PO4    = (/ -2.312E+1, +1.758E-1, -2.647E-3, -5.150E+0, +9.000E-2 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpHPO4     = (/ -2.657E+1, +2.020E-1, -3.042E-3, -4.080E+0, +7.140E-2 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpCaCO3cal = (/ -4.876E+1, +5.304E-1, +0.000E+0, -1.176E+1, +3.692E-1 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpCaCO3arg = (/ -4.596E+1, +5.304E-1, +0.000E+0, -1.176E+1, +3.692E-1 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpH2S      = (/ -1.107E+1, +9.000E-3, -9.420E-4, +2.890E+0, +5.400E-2 /)
  REAL,PARAMETER,DIMENSION(5)::carbchem_dpNH4      = (/ -2.643E+0, +8.890E-1, -9.050E-3, -5.030E+0, +8.140E-2 /)


CONTAINS


  ! ****************************************************************************************************************************** !
  ! CALCULATE EQUILIBRIUM CONSTANTS
  ! NOTE: the conversion between mol (kg-H2O)-1 (molarity) and mol (kg-sol)-1 is;
  !       ln ki[mol (kg-sol)-1] = ln ki[mol (kg-H2O)-1] + ln(1.0 - 0.001005 S)
  ! NOTE: pressure in units of (bar) (1 m depth approx = 1 dbar pressure)
  ! NOTE: pk - -log10(k)
  !       -> this conversion is needed where dissociation constants are given as fits for pk rather than ln(k)
  SUBROUTINE sub_calc_carbconst(dum_D,dum_T,dum_S,dum_carbconst)
    ! dummy variables
    REAL,INTENT(in)::dum_D,dum_T,dum_S
    REAL,DIMENSION(1:n_carbconst),intent(inout)::dum_carbconst
    ! local variables
    REAL::loc_P ! pressure (bar)
    real::loc_conv_molaritytoconc
    real::loc_conv_freetoSWS,loc_conv_freetototal,loc_conv_totaltoSWS
    REAL::loc_S,loc_S_p05,loc_S_p15,loc_S_p20
    real::loc_I,loc_I_p05,loc_I_p15,loc_I_p20
    real::loc_ION,loc_ION_p05
    real::loc_Cl
    real::loc_T,loc_rT,loc_Tr100,loc_T_ln,loc_T_log,loc_TC
    real::loc_rRtimesT
    real::loc_Ftot,loc_SO4tot
    ! calculate local constants -- T and S
    ! NOTE: restrict valid T,S range for empirical fit (see Millero [1995])
    !	    'DicksonMillero' :  0.0  <= T <= 35.0 C
    !                          20.0  <= S <= 43.0 PSU
    !	    'Hansson'        :  0.0  <= T <= 30.0 C
    !                          20.0  <= S <= 40.0 PSU
    !	    'Roy'            :  0.0  <= T <= 45.0 C
    !                          20.0  <= S <= 43.0 PSU
    !	    original default :  2.0  <= T <= 35.0 C
    !                          26.0  <= S <= 43.0 PSU
    if (dum_T <  (const_zeroC +  par_carbchem_Tmin))  then
       loc_T = const_zeroC +  par_carbchem_Tmin
    elseif (dum_T > (const_zeroC + par_carbchem_Tmax)) then
       loc_T = const_zeroC + par_carbchem_Tmax
    else
       loc_T = dum_T
    endif
    if (dum_S < par_carbchem_Smin) then
       loc_S = par_carbchem_Smin
    elseif (dum_S > par_carbchem_Smax) then
       loc_S = par_carbchem_Smax
    else
       loc_S = dum_S
    endif
    ! calculate local constants
    loc_P        = dum_D/10.0
    loc_S_p05    = loc_S**0.5
    loc_S_p15    = loc_S**1.5
    loc_S_p20    = loc_S*loc_S
    loc_T_ln     = LOG(loc_T)
    loc_T_log    = LOG10(loc_T)
    loc_rT       = 1.0/loc_T
    loc_Tr100    = loc_T/100.0
    loc_TC       = loc_T - const_zeroC
    loc_rRtimesT = 1.0/(const_R*loc_T)
    loc_I        = fun_calc_I(loc_S)
    loc_I_p05    = loc_I**0.5   
    loc_I_p15    = loc_I**1.5   
    loc_I_p20    = loc_I*loc_I
    loc_Cl       = fun_calc_Cl(dum_S)
    loc_ION      = fun_calc_ION(loc_Cl)
    loc_ION_p05  = loc_ION**0.5
    ! calculate conversion between mol (kg-H2O)-1 (molarity) to mol (kg-sol)-1 concentration scales
    loc_conv_molaritytoconc = LOG(1 - 0.001005*loc_S)
    ! calculate conversions between pH scales; free [H] scale; [mol (kg-sol)-1]
    ! NOTE: there will be an error induced if [F-] and/or [SO42-] is treated as an explicit tracer elsewhere
    !       (i.e., if elsewhere these concentrations are not just simply estimated from salinity)
    loc_SO4tot = fun_calc_SO4tot(loc_S)
    loc_Ftot   = fun_calc_Ftot(loc_S)
    dum_carbconst(icc_kHSO4) = &
         & EXP( &
         &   fun_calc_lnkHSO4(loc_rT,loc_T_ln,loc_I_p05,loc_I,loc_I_p15,loc_I_p20) + &
         &   loc_conv_molaritytoconc &
         & )
    loc_conv_freetototal = log(1.0 + loc_SO4tot/dum_carbconst(icc_kHSO4))
    dum_carbconst(icc_kHF) = &
         & EXP( &
         &   fun_calc_lnkHF(loc_T,loc_ION_p05) + &
         &   loc_conv_molaritytoconc + &
         &   loc_conv_freetototal &
         & )
    loc_conv_freetoSWS   = log(1.0 + loc_SO4tot/dum_carbconst(icc_kHSO4) + loc_Ftot/dum_carbconst(icc_kHF))
    loc_conv_totaltoSWS  = -loc_conv_freetototal + loc_conv_freetoSWS
    ! calculate carbonate system constants; SWS [H] scale; [mol (kg-sol)-1]
    ! NOTE: the Mehrbach et al. [1973] carbonate dissociation constant choice is the default
    select case (trim(par_carbconstset_name))
    case ('DicksonMillero')
       dum_carbconst(icc_k1) = &
            & EXP( &
            &   fun_calc_lnk1_DicksonMillero(loc_rT,loc_S,loc_S_p20) + &
            &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpH2CO3) &
            & )
       dum_carbconst(icc_k2)= &
            & EXP( &
            &   fun_calc_lnk2_DicksonMillero(loc_rT,loc_S,loc_S_p20) + &
            &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpHCO3) &
            & )
    case ('Hansson')
       dum_carbconst(icc_k1) = &
            & EXP( &
            &   fun_calc_lnk1_Hansson(loc_rT,loc_S,loc_S_p20) + &
            &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpH2CO3) &
            & )
       dum_carbconst(icc_k2)= &
            & EXP( &
            &   fun_calc_lnk2_Hansson(loc_rT,loc_T_ln,loc_S,loc_S_p20) + &
            &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpHCO3) &
            & )
    case ('Roy')
       dum_carbconst(icc_k1) = &
            & EXP( &
            &   fun_calc_lnk1_Roy(loc_rT,loc_T_ln,loc_S_p05,loc_S,loc_S_p15) + &
            &   loc_conv_molaritytoconc + &
            &   loc_conv_totaltoSWS + & 
            &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpH2CO3) &
            & )
       dum_carbconst(icc_k2)= &
            & EXP( &
            &   fun_calc_lnk2_Roy(loc_rT,loc_T_ln,loc_S_p05,loc_S,loc_S_p15) + &
            &   loc_conv_molaritytoconc + &
            &   loc_conv_totaltoSWS + & 
            &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpHCO3) &
            & )
    case default
       dum_carbconst(icc_k1) = &
            & EXP( &
            &   fun_calc_lnk1_Mehrbach(loc_rT,loc_T_ln,loc_S,loc_S_p20) + &
            &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpH2CO3) &
            & )
       dum_carbconst(icc_k2)= &
            & EXP( &
            &   fun_calc_lnk2_Mehrbach(loc_rT,loc_S,loc_S_p20) + &
            &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpHCO3) &
            & )
    end SELECT
    dum_carbconst(icc_k) = &
         & dum_carbconst(icc_k1)/dum_carbconst(icc_k2)
    dum_carbconst(icc_kB) = &
         & EXP( &
         &   fun_calc_lnkB(loc_T,loc_rT,loc_T_ln,loc_S,loc_S_p05,loc_S_p15,loc_S_p20) + &
         &   loc_conv_molaritytoconc + &
         &   loc_conv_totaltoSWS + &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpBO3H3) &
         & )
    dum_carbconst(icc_kW) = &
         & EXP( &
         &   fun_calc_lnkW(loc_rT,loc_T_ln,loc_S,loc_S_p05) + &
         &   fun_corr_p(loc_Tc,loc_P,loc_rRtimesT,carbchem_dpH2O) &
         & )
    dum_carbconst(icc_kSi) = &
         & EXP( &
         &   fun_calc_lnkSi(loc_rT,loc_T_ln,loc_I,loc_I_p05) + &
         &   loc_conv_molaritytoconc + &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpH4SiO4) &
         & )
    dum_carbconst(icc_kHF) = &
         & EXP( &
         &   fun_calc_lnkHF(loc_T,loc_ION_p05) + &
         &   loc_conv_molaritytoconc + &
         &   loc_conv_freetoSWS + &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpHF) &
         & )
    dum_carbconst(icc_kHSO4) = &
         & EXP( &
         &   fun_calc_lnkHSO4(loc_rT,loc_T_ln,loc_I_p05,loc_I,loc_I_p15,loc_I_p20) + &
         &   loc_conv_molaritytoconc + &
         &   loc_conv_freetoSWS + & 
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpHSO4) &
         & )
    dum_carbconst(icc_kP1) = &
         & EXP( &
         &   fun_calc_lnkP1(loc_T,loc_T_ln,loc_S,loc_S_p05) + &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpH3PO4) &
         & )
    dum_carbconst(icc_kP2) = &
         & EXP( &
         &   fun_calc_lnkP2(loc_T,loc_T_ln,loc_S,loc_S_p05) + &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpH2PO4) &
         & )
    dum_carbconst(icc_kP3) = &
         & EXP( &
         &   fun_calc_lnkP3(loc_T,loc_S,loc_S_p05) + &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpHPO4) &
         & )
    dum_carbconst(icc_kH2S) = &
         & EXP( &
         &   fun_calc_lnkH2S(loc_rT,loc_T_ln,loc_S,loc_S_p05) + &
         &   loc_conv_totaltoSWS + &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpH2S) &
         & )
    dum_carbconst(icc_kNH4) = &
         & EXP( &
         &   fun_calc_lnkNH4(loc_T,loc_rT,loc_S,loc_S_p05) + &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpNH4) &
         & )
    dum_carbconst(icc_kcal) = &
         & EXP( &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpCaCO3cal) &
         & ) * &
         &   10**(fun_calc_logkcal(loc_T,loc_rT,loc_T_log,loc_S,loc_S_p05,loc_S_p15))
    dum_carbconst(icc_karg) = &
         & EXP( &
         &   fun_corr_p(loc_TC,loc_P,loc_rRtimesT,carbchem_dpCaCO3arg) &
         & ) * &
         &   10**(fun_calc_logkarg(loc_T,loc_rT,loc_T_log,loc_S,loc_S_p05,loc_S_p15))
    dum_carbconst(icc_QCO2) = &
         & EXP( &
         &   fun_calc_lnQ_CO2(loc_Tr100,loc_rT,loc_S) &
         & )
    dum_carbconst(icc_QO2) = &
         & EXP( &
         &   fun_calc_lnQ_O2(loc_Tr100,loc_rT,loc_S) &
         & )
  END SUBROUTINE sub_calc_carbconst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ADJUST CARBONATE EQUILIBRIUM CONSTANTS
  ! NOTE: to take into account the deviation from modern of:
  ! (1) Mg/Ca ratio
  SUBROUTINE sub_adj_carbconst( &
       & dum_Ca,                &
       & dum_Mg,                &
       & dum_carbconst)
    ! dummy variables
    REAL,INTENT(in)::dum_Ca,dum_Mg
    REAL,DIMENSION(n_carbconst),intent(inout)::dum_carbconst
    ! local variables
    real::loc_alpha
    real::loc_ratio
    ! initialize local variables
    loc_alpha = 3.655E-8
    loc_ratio = 1.0
    if (dum_Ca > const_real_nullsmall) loc_ratio = dum_Mg/dum_Ca
    ! (1a) adjust Ksp
    !      NOTE: following Tyrrell and Zeebe [2004]
    dum_carbconst(icc_kcal) = dum_carbconst(icc_kcal) - loc_alpha*(const_conc_MgtoCa - loc_ratio)
    ! (1b) adjust K1 and K2
    dum_carbconst(icc_k1) = (1.0 + 0.155*(dum_Mg - const_conc_Mg)/const_conc_Mg)*dum_carbconst(icc_k1)
    dum_carbconst(icc_k2) = (1.0 + 0.422*(dum_Mg - const_conc_Mg)/const_conc_Mg)*dum_carbconst(icc_k2)
  end SUBROUTINE sub_adj_carbconst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FIND A SOLUTION FOR THE AQUEOUS CARBONATE CHEMISTRY SYSTEM
  ! NOTE: works from a previous (or default initialized) value of pH
  ! NOTE: in calculating loc_SO4, [H+] concentration is adjusted to the free hydrogen ion concentration
  !       following OCMIP-2
  ! NOTE: definition of alkalinity follows Dickson [1994]
  ! NOTE: see Zeebe and Wolf-Gladrow [2001]
  ! POTTED SUMMARY;
  ! total alkalinity, TA (variable dum_ALK) can be expressed in terms of the charge imbalance of the conservative ions;
  ! TA = SUM(concervative cations) - SUM(conservative anions)
  !    = [HCO3-] + 2[CO32-] + [B(OH)4-] + [OH-] + [HPO42-] + 2[PO43-] + [H3SiO4-] + [NH3] + [HS-]
  !      - [H+] - [HSO4-] - [HF-] - [H3PO4]
  ! [Dickson, 1994]
  ! NOTE: definition of alkalinity differs slightly from Dickson [1981] where there is an additional contribution from [S2-]
  ! writing carbonate alkalinity (variable loc_ALK_DIC);
  ! CA = [HCO3-] + 2[CO32-]
  ! => CA = TA
  !         - [B(OH)4-] - [OH-] - [HPO42-] - 2[PO43-] - [H3SiO4-] - [NH3] - [HS-]
  !         + [H+] + [HSO4-] + [HF-] + [H3PO4]
  ! NOTE: not accounted for here are the effects of nitrate and ammonia assimilation by plants;
  !       NO3- uptake is balanced by OH- production, and alkalinity increases
  !       HN4+ uptake is balanced by H+ production, and alkalinity decreases
  !       (see Zeebe and Wolf-Gladrow [2001], page 51)
  ! NOTE: [H] is on the pH(SWS) unless otherwise noted
  ! NOTE: the potential for rapidly oxidzing species such as NH4 and esp. H2S to have a negative concentration is tested for
  !       + also saves unnecessary calculation when NH4 and H2S are not selected as tracers (and have zero concentrations)
  SUBROUTINE sub_calc_carb( &
       & dum_DIC,dum_ALK,dum_Ca, &
       & dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot, &
       & dum_carbconst,dum_carb,dum_carbalk)
    ! dummy variables
    REAL,INTENT(in)::dum_DIC,dum_ALK,dum_Ca
    REAL,INTENT(in)::dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot
    REAL,DIMENSION(n_carbconst),intent(in)::dum_carbconst
    REAL,DIMENSION(n_carb),INTENT(inout)::dum_carb
    REAL,DIMENSION(n_carbalk),INTENT(inout)::dum_carbalk
    ! local variables
    INTEGER::n
    real::loc_OH,loc_H3SiO4,loc_H4BO4,loc_HSO4,loc_HF,loc_H3PO4,loc_H2PO4,loc_HPO4,loc_PO4,loc_HS,loc_NH3
    REAL::loc_zed
    REAL::loc_ALK_DIC,loc_conc_CO2,loc_conc_CO3,loc_conc_HCO3
    REAL::loc_H,loc_H_old,loc_H1,loc_H2,loc_H_p2,loc_H_p3,loc_H_free,loc_H_total
    REAL::loc_sat_cal,loc_sat_arg
    real::loc_r
    ! initialize loop variables
    n = 1
    loc_H = dum_carb(ic_H)
    loc_HF = 0.0
    loc_HS = 0.0
    loc_NH3 = 0.0

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! *** IMPLICIT [H] SOLUTION LOOP START ***
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    block_solvehloop: DO

       ! make a copy of the [H] value from the previous iteration (or seeded value if first iteration)
       loc_H_old = loc_H
       ! local pre-calculated powers
       loc_H_p2 = loc_H*loc_H
       loc_H_p3 = loc_H*loc_H_p2
       ! [H+] on alternative pH scales
       loc_H_free = loc_H/(1.0 + dum_SO4tot/dum_carbconst(icc_kHSO4) + dum_Ftot/dum_carbconst(icc_kHF))
       loc_H_total = loc_H_free*(1.0 + dum_SO4tot/dum_carbconst(icc_kHSO4))
       ! ion product of water; H2O <-kW-> H+ + OH-
       loc_OH = dum_carbconst(icc_kW)/loc_H
       ! boric acid; B(OH)3 + H2O <-kB-> B(OH)4- + H+
       loc_H4BO4 = dum_Btot/(1.0 + loc_H/dum_carbconst(icc_kB))
       ! SiO2 + 2H2O <-kSi-> H+ + Si(OH)3O-
       loc_H3SiO4 = dum_SiO2tot/(1.0 + loc_H/dum_carbconst(icc_kSi))
       ! bisulphate; HSO4- <-kHSO4-> H+ + SO42-
       loc_HSO4 = dum_SO4tot/(1.0 + dum_carbconst(icc_kHSO4)/loc_H)
       ! hydrogen floride; HF <-kHF-> H+ + F-
       loc_HF = dum_Ftot/(1.0 + dum_carbconst(icc_kHF)/loc_H)
       ! hydrogen sulphide; H2S <-kH2S-> H+ + HS-
       if (dum_H2Stot > const_real_nullsmall) loc_HS = dum_H2Stot/(1.0 + loc_H/dum_carbconst(icc_kH2S))
       ! ammonium; NH4+ <-kNH4-> H+ + NH3
       if (dum_NH4tot > const_real_nullsmall) loc_NH3 = dum_NH4tot/(1.0 + loc_H/dum_carbconst(icc_kNH4))
       ! phosphoric acid
       loc_H3PO4 = dum_PO4tot/( &
            & 1.0 + &
            & dum_carbconst(icc_kP1)/loc_H + &
            & (dum_carbconst(icc_kP1)*dum_carbconst(icc_kP2))/loc_H_p2 + &
            & (dum_carbconst(icc_kP1)*dum_carbconst(icc_kP2)*dum_carbconst(icc_kP3))/loc_H_p3 &
            & )
       loc_H2PO4 = dum_PO4tot/( &
            & 1.0 + &
            & loc_H/dum_carbconst(icc_kP1) + &
            & dum_carbconst(icc_kP2)/loc_H + &
            & (dum_carbconst(icc_kP2)*dum_carbconst(icc_kP3))/loc_H_p2 &
            & )
       loc_HPO4 = dum_PO4tot/( &
            & 1.0 + &
            & loc_H/dum_carbconst(icc_kP2) + &
            & loc_H_p2/(dum_carbconst(icc_kP1)*dum_carbconst(icc_kP2)) + &
            & dum_carbconst(icc_kP3)/loc_H &
            & )
       loc_PO4 = dum_PO4tot/( &
            & 1.0 + &
            & loc_H/dum_carbconst(icc_kP3) + &
            & loc_H_p2/(dum_carbconst(icc_kP2)*dum_carbconst(icc_kP3)) + &
            & loc_H_p3/(dum_carbconst(icc_kP1)*dum_carbconst(icc_kP2)*dum_carbconst(icc_kP3)) &
            & )
       ! calculate carbonate alkalinity
       loc_ALK_DIC = dum_ALK &
            & - loc_H4BO4 - loc_OH - loc_HPO4 - 2.0*loc_PO4 - loc_H3SiO4 - loc_NH3 - loc_HS &
            & + loc_H + loc_HSO4 + loc_HF + loc_H3PO4
       ! estimate the partitioning between the aqueous carbonate species, and then make two independent estimates of [H];
       ! -> one using the 1st carbonate dissociation constant and estimated [CO2] and [HCO3-] concentrations
       ! -> the other using the 2nd carbonate dissociation constant and estimated [HCO3-] and [CO32-] concentrations
       loc_zed = ( &
            &   (4.0*loc_ALK_DIC + dum_DIC*dum_carbconst(icc_k) - loc_ALK_DIC*dum_carbconst(icc_k))**2 + &
            &   4.0*(dum_carbconst(icc_k) - 4.0)*loc_ALK_DIC**2 &
            & )**0.5
       loc_conc_HCO3 = (dum_DIC*dum_carbconst(icc_k) - loc_zed)/(dum_carbconst(icc_k) - 4.0)
       loc_conc_CO3 = &
            & ( &
            &   loc_ALK_DIC*dum_carbconst(icc_k) - dum_DIC*dum_carbconst(icc_k) - &
            &   4.0*loc_ALK_DIC + loc_zed &
            & ) &
            & /(2.0*(dum_carbconst(icc_k) - 4.0))
       loc_conc_CO2 = dum_DIC - loc_ALK_DIC + &
            & ( &
            &   loc_ALK_DIC*dum_carbconst(icc_k) - dum_DIC*dum_carbconst(icc_k) - &
            &   4.0*loc_ALK_DIC + loc_zed &
            & ) &
            & /(2.0*(dum_carbconst(icc_k) - 4.0))        
       loc_H1 = dum_carbconst(icc_k1)*loc_conc_CO2/loc_conc_HCO3
       loc_H2 = dum_carbconst(icc_k2)*loc_conc_HCO3/loc_conc_CO3
       ! test for -ve [H]
       IF ((loc_H1 < const_real_nullsmall) .OR. (loc_H2 < const_real_nullsmall)) THEN
          CALL sub_report_error(                                                         &
               & 'gem_carbchem.f90','sub_calc_carb',                                     &
               & 'Numerical instability at step; '//fun_conv_num_char_n(4,n)//           &
               & ' / Data; dum_DIC,dum_ALK,dum_Ca,dum_SO4tot,dum_H2Stot,dum_NH4tot,'//   &
               & 'pH(SWS), pH (OLD), pH (guess #1), pH (guess #2)',                      &
               & 'CARBONATE CHEMISTRY COULD NOT BE UPDATED :(',                          &
               & (/dum_DIC,dum_ALK,dum_Ca,dum_SO4tot,dum_H2Stot,dum_NH4tot,              &
               & -LOG10(loc_H),-LOG10(loc_H_old),-LOG10(loc_H1),-LOG10(loc_H2)/),.false. &
               & )
          Print*,' > WHAT-IT-MEANS (maybe ...): '
          Print*,'   (1) Check the FIRST TWO lines of the ERROR DATA (ocean DIC and ALK):'
          Print*,'       -> These should be ... reasonable ... of order a few thousand umol kg-1'
          Print*,'          (units reported are mol kg-1)'
          Print*,'       -> NaNs, negative values, obsenely low (<< a few hundred umol kg-1) or,'
          Print*,'          high (>> tens of thousands) are indicative of array bounds problems.'
          Print*,'          Array bounds problems are mostly commonly due to'
          Print*,'          (A) incorrect ocean dimension compared to the compiled executable,'
          Print*,'          (B) incorrect number of biogeochem tracers in the ocean '
          Print*,'              compared to compiled executable.'
          Print*,'          => Carry out a *** make cleanall *** (from ~/genie/genie-main).' 
          Print*,'   2) If the relative values of DIC and ALK differ by factor of ca. 2'
          Print*,'       it may not be possible to solve for aqueous carbonate chemsitry.' 
          Print*,'       => View the netCDF distribution of DIC, ALK (or other tracers);'
          Print*,'          -> Extreme hotspots or minima may reflect problems associated with'
          Print*,'             circulation or sea-ice instabilities.'
          Print*,'             Extreme freshening (or salinity) in topographically restricted'
          Print*,'             (and/or shallow) seas can also cause problems.'
          Print*,'   (3) Extreme values of Ca and SO4 (#3, #4) are only vanishingly possible' 
          Print*,'       except due to incorrect compiled array dimension or extreme salinity.' 
          Print*,' > NOTE: The circulation model is very robust and will not easily fall over.'
          Print*,'         BIOGEM is something of a canary in this respect and will report'
          Print*,'         unrealistic chemistries diagnostic of problems elsewhere.' 
          Print*,'         *** THIS ERROR MESSAGE THUS DOES NOT NECESSARILY INDICATE ***'
          Print*,'         *** SOMETHING AMISS WITH THE BIOGEOCHEMSITRY CODE PER SE. ***'
          Print*,' > Refer to user-manual for info on altering the error behavior.'
          Print*,' '
          error_carbchem = .TRUE.
          if (ctrl_carbchem_fail) then
             error_stop = .TRUE.
             exit
          else
             ! re-seed [H+]
             call RANDOM_NUMBER(loc_r)
             loc_H1 = 10**(-4.7 - 2.5*dum_ALK/dum_DIC - (loc_r - 0.5))
             loc_H2 = loc_H1
          end if
       ENDIF
       ! the implicit bit!
       loc_H = SQRT(loc_H1*loc_H2)
       ! test for the relative change in [H] falling below some criterion (at which point the solution is assumed stable)
       IF (ABS(1.0 - loc_H/loc_H_old) < (1.0E-8/loc_H)*par_carbchem_pH_tolerance) then
          ! calculate result variables
          dum_carb(ic_conc_CO2)  = loc_conc_CO2
          dum_carb(ic_conc_CO3)  = loc_conc_CO3
          dum_carb(ic_conc_HCO3) = loc_conc_HCO3
          dum_carb(ic_fug_CO2)   = loc_conc_CO2/dum_carbconst(icc_QCO2) 
          dum_carb(ic_ohm_cal)   = dum_Ca*loc_conc_CO3/dum_carbconst(icc_kcal)
          dum_carb(ic_ohm_arg)   = dum_Ca*loc_conc_CO3/dum_carbconst(icc_karg)  
          dum_carb(ic_H)         = loc_H 
          dum_carb(ic_pHsws)     = -log10(loc_H)
          ! calculate value of [CO3--] at calcite and aragonite saturation
          ! NOTE: this assumes that the value of omega is unity at saturation (definition!)
          loc_sat_cal = dum_carbconst(icc_kcal) * 1.0/dum_Ca
          loc_sat_arg = dum_carbconst(icc_karg) * 1.0/dum_Ca
          dum_carb(ic_dCO3_cal) = loc_conc_CO3 - loc_sat_cal
          dum_carb(ic_dCO3_arg) = loc_conc_CO3 - loc_sat_arg
          ! set alkalinity contributions
          dum_carbalk(ica_HCO3)   = loc_conc_HCO3
          dum_carbalk(ica_CO3)    = 2.0*loc_conc_CO2
          dum_carbalk(ica_H4BO4)  = loc_H4BO4
          dum_carbalk(ica_OH)     = loc_OH
          dum_carbalk(ica_HPO4)   = loc_HPO4
          dum_carbalk(ica_PO4)    = 2.0*loc_PO4
          dum_carbalk(ica_H3SiO4) = loc_H3SiO4
          dum_carbalk(ica_NH3)    = loc_NH3
          dum_carbalk(ica_HS)     = loc_HS
          dum_carbalk(ica_H)      = -loc_H
          dum_carbalk(ica_HSO4)   = -loc_HSO4
          dum_carbalk(ica_HF)     = -loc_HF
          dum_carbalk(ica_H3PO4)  = -loc_H3PO4
          EXIT
       else
          n = n + 1
       end if
       ! test for whether we are likely to be waiting all bloody day for the algorithm to solve sweet FA
       IF (n > par_carbchem_pH_iterationmax) THEN
          CALL sub_report_error(                                                                            &
               & 'gem_carbchem.f90','sub_calc_carb',                                                        &
               & 'Number of steps taken without successfully solving for pH = '//fun_conv_num_char_n(4,n)// &
               & ' out of: '//fun_conv_num_char_n(4,par_carbchem_pH_iterationmax)//' maximum allowed'//     &
               & ' / Data; dum_DIC,dum_ALK,dum_Ca,dum_SO4tot,dum_H2Stot,dum_NH4tot,'//                      &
               & 'pH(SWS), pH (OLD), pH (guess #1), pH (guess #2)',                                         &
               & 'CARBONATE CHEMISTRY COULD NOT BE UPDATED :(',                                             &
               & (/dum_DIC,dum_ALK,dum_Ca,dum_SO4tot,dum_H2Stot,dum_NH4tot,                                 &
               & -LOG10(loc_H),-LOG10(loc_H_old),-LOG10(loc_H1),-LOG10(loc_H2)/),.false.                    &
               & )
          error_carbchem = .TRUE.
          if (ctrl_carbchem_fail) then
             error_stop = .TRUE.
             exit
          else
             ! re-seed [H+]
             call RANDOM_NUMBER(loc_r)
             loc_H1 = 10**(-4.7 - 2.5*dum_ALK/dum_DIC - (loc_r - 0.5))
             loc_H2 = loc_H1
             n = 1
          end if
       END IF

    END DO block_solvehloop

    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ! *** IMPLICIT [H] SOLUTION LOOP end ***
    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  END SUBROUTINE sub_calc_carb
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE REVELLE FACTOR
  ! NOTE: as per sub_calc_carb
  SUBROUTINE sub_calc_carb_RF0( &
       & dum_DIC,dum_ALK, &
       & dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot, &
       & dum_carbconst,dum_carb)
    ! dummy variables
    REAL,INTENT(in)::dum_DIC,dum_ALK
    REAL,INTENT(in)::dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot
    REAL,DIMENSION(n_carbconst),intent(in)::dum_carbconst
    REAL,DIMENSION(n_carb),INTENT(inout)::dum_carb
    ! local variables
    INTEGER::n
    real::loc_OH,loc_H3SiO4,loc_H4BO4,loc_HSO4,loc_HF,loc_H3PO4,loc_H2PO4,loc_HPO4,loc_PO4,loc_HS,loc_NH3
    REAL::loc_zed
    REAL::loc_ALK_DIC,loc_conc_CO2,loc_conc_CO3,loc_conc_HCO3
    REAL::loc_H,loc_H_old,loc_H1,loc_H2,loc_H_p2,loc_H_p3,loc_H_free,loc_H_total
    real::loc_DIC_RFO
    ! initialize loop variables
    n = 1
    loc_H = dum_carb(ic_H)
    loc_HF = 0.0
    loc_HS = 0.0
    loc_NH3 = 0.0

    ! perturb DIC
    loc_DIC_RFO = dum_DIC + 1.0e-6

    block_solvehloop: DO

       ! make a copy of the [H] value from the previous iteration (or seeded value if first iteration)
       loc_H_old = loc_H
       ! local pre-calculated powers
       loc_H_p2 = loc_H*loc_H
       loc_H_p3 = loc_H*loc_H_p2
       ! [H+] on alternative pH scales
       loc_H_free = loc_H/(1.0 + dum_SO4tot/dum_carbconst(icc_kHSO4) + dum_Ftot/dum_carbconst(icc_kHF))
       loc_H_total = loc_H_free*(1.0 + dum_SO4tot/dum_carbconst(icc_kHSO4))
       ! ion product of water; H2O <-kW-> H+ + OH-
       loc_OH = dum_carbconst(icc_kW)/loc_H
       ! boric acid; B(OH)3 + H2O <-kB-> B(OH)4- + H+
       loc_H4BO4 = dum_Btot/(1.0 + loc_H/dum_carbconst(icc_kB))
       ! SiO2 + 2H2O <-kSi-> H+ + Si(OH)3O-
       loc_H3SiO4 = dum_SiO2tot/(1.0 + loc_H/dum_carbconst(icc_kSi))
       ! bisulphate; HSO4- <-kHSO4-> H+ + SO42-
       loc_HSO4 = dum_SO4tot/(1.0 + dum_carbconst(icc_kHSO4)/loc_H)
       ! hydrogen floride; HF <-kHF-> H+ + F-
       loc_HF = dum_Ftot/(1.0 + dum_carbconst(icc_kHF)/loc_H)
       ! hydrogen sulphide; H2S <-kH2S-> H+ + HS-
       if (dum_H2Stot > const_real_nullsmall) loc_HS = dum_H2Stot/(1.0 + loc_H/dum_carbconst(icc_kH2S))
       ! ammonium; NH4+ <-kNH4-> H+ + NH3
       if (dum_NH4tot > const_real_nullsmall) loc_NH3 = dum_NH4tot/(1.0 + loc_H/dum_carbconst(icc_kNH4))
       ! phosphoric acid
       loc_H3PO4 = dum_PO4tot/( &
            & 1.0 + &
            & dum_carbconst(icc_kP1)/loc_H + &
            & (dum_carbconst(icc_kP1)*dum_carbconst(icc_kP2))/loc_H_p2 + &
            & (dum_carbconst(icc_kP1)*dum_carbconst(icc_kP2)*dum_carbconst(icc_kP3))/loc_H_p3 &
            & )
       loc_H2PO4 = dum_PO4tot/( &
            & 1.0 + &
            & loc_H/dum_carbconst(icc_kP1) + &
            & dum_carbconst(icc_kP2)/loc_H + &
            & (dum_carbconst(icc_kP2)*dum_carbconst(icc_kP3))/loc_H_p2 &
            & )
       loc_HPO4 = dum_PO4tot/( &
            & 1.0 + &
            & loc_H/dum_carbconst(icc_kP2) + &
            & loc_H_p2/(dum_carbconst(icc_kP1)*dum_carbconst(icc_kP2)) + &
            & dum_carbconst(icc_kP3)/loc_H &
            & )
       loc_PO4 = dum_PO4tot/( &
            & 1.0 + &
            & loc_H/dum_carbconst(icc_kP3) + &
            & loc_H_p2/(dum_carbconst(icc_kP2)*dum_carbconst(icc_kP3)) + &
            & loc_H_p3/(dum_carbconst(icc_kP1)*dum_carbconst(icc_kP2)*dum_carbconst(icc_kP3)) &
            & )
       ! calculate carbonate alkalinity
       loc_ALK_DIC = dum_ALK &
            & - loc_H4BO4 - loc_OH - loc_HPO4 - 2.0*loc_PO4 - loc_H3SiO4 - loc_NH3 - loc_HS &
            & + loc_H + loc_HSO4 + loc_HF + loc_H3PO4
       ! estimate the partitioning between the aqueous carbonate species, and then make two independent estimates of [H]
       loc_zed = ( &
            &   (4.0*loc_ALK_DIC + loc_DIC_RFO*dum_carbconst(icc_k) - loc_ALK_DIC*dum_carbconst(icc_k))**2 + &
            &   4.0*(dum_carbconst(icc_k) - 4.0)*loc_ALK_DIC**2 &
            & )**0.5
       loc_conc_HCO3 = (loc_DIC_RFO*dum_carbconst(icc_k) - loc_zed)/(dum_carbconst(icc_k) - 4.0)
       loc_conc_CO3 = &
            & ( &
            &   loc_ALK_DIC*dum_carbconst(icc_k) - loc_DIC_RFO*dum_carbconst(icc_k) - &
            &   4.0*loc_ALK_DIC + loc_zed &
            & ) &
            & /(2.0*(dum_carbconst(icc_k) - 4.0))
       loc_conc_CO2 = loc_DIC_RFO - loc_ALK_DIC + &
            & ( &
            &   loc_ALK_DIC*dum_carbconst(icc_k) - loc_DIC_RFO*dum_carbconst(icc_k) - &
            &   4.0*loc_ALK_DIC + loc_zed &
            & ) &
            & /(2.0*(dum_carbconst(icc_k) - 4.0))        
       loc_H1 = dum_carbconst(icc_k1)*loc_conc_CO2/loc_conc_HCO3
       loc_H2 = dum_carbconst(icc_k2)*loc_conc_HCO3/loc_conc_CO3
       ! the implicit bit!
       loc_H = SQRT(loc_H1*loc_H2)
       IF (ABS(1.0 - loc_H/loc_H_old) < par_carbchem_pH_tolerance) then
          ! calcualate and return Revelle factor
          dum_carb(ic_RF0) = (loc_conc_CO2/dum_carb(ic_conc_CO2) - 1.0)/(loc_DIC_RFO/dum_DIC - 1.0)
          EXIT
       else
          n = n + 1
       end if
       if ((loc_H1 < const_real_nullsmall) .OR. (loc_H2 < const_real_nullsmall) .OR. (n > par_carbchem_pH_iterationmax)) THEN
          ! set null Revelle factor
          dum_carb(ic_RF0) = 0.0
          exit
       end if

    END DO block_solvehloop

  END SUBROUTINE sub_calc_carb_RF0
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE CARBONATE SYSTEM ISOTOPIC RATIOS (r13C)
  ! NOTE: fractonation factors after Zhang et al. [1995]
  ! NOTE: all equations following Zeebe and Wolf-Gladrow [2001] (section 3.2)
  subroutine sub_calc_carb_r13C(dum_T,dum_DIC,dum_DIC_13C,dum_carb,dum_carbisor)
    ! dummy variables
    REAL,INTENT(in)::dum_T,dum_DIC,dum_DIC_13C
    REAL,DIMENSION(n_carb),INTENT(in)::dum_carb 
    REAL,DIMENSION(n_carbisor),INTENT(inout)::dum_carbisor
    ! local variables
    real::loc_TC
    real::loc_epsilon_bg,loc_epsilon_dg,loc_epsilon_cg,loc_epsilon_cb,loc_epsilon_db
    real::loc_DIC_r13C,loc_CO2_r13C,loc_HCO3_r13C,loc_CO3_r13C
    real::loc_DIC_d13C,loc_CO2_d13C,loc_HCO3_d13C,loc_CO3_d13C
    ! initialize local variables
    loc_TC = dum_T - const_zeroC
    loc_DIC_d13C = fun_calc_isotope_delta(dum_DIC,dum_DIC_13C,const_standards(11),.FALSE.,const_real_null)
    ! calculate local fractionation factors
    ! epsilon_bg = epsilon(HCO3- - CO2(G))
    ! epsilon_dg = epsilon(CO2 - CO2(g))
    ! epsilon_cg = epsilon(CO32- - CO2(g))
    ! epsilon_cb = epsilon(CO32- - HCO3-)
    ! epsilon_db = epsilon(CO2 - HCO3-)
    loc_epsilon_bg = -0.1141*loc_TC + 10.78
    loc_epsilon_dg = +0.0049*loc_TC - 1.31
    loc_epsilon_cg = -0.052*loc_TC + 7.22
    loc_epsilon_cb = loc_epsilon_cg - loc_epsilon_bg/(1.0 + loc_epsilon_bg*1.0E-3)
    loc_epsilon_db = loc_epsilon_dg - loc_epsilon_bg/(1.0 + loc_epsilon_bg*1.0E-3)
    ! calculate aqueous system deltas
    loc_HCO3_d13C = &
         & (loc_DIC_d13C*dum_DIC - (loc_epsilon_db*dum_carb(ic_conc_CO2) + loc_epsilon_cb*dum_carb(ic_conc_CO3)))/ &
         & ( &
         &   (1.0 + loc_epsilon_db*1.0E-3)*dum_carb(ic_conc_CO2) + &
         &   dum_carb(ic_conc_HCO3) + &
         &   (1.0 + loc_epsilon_cb*1.0E-3)*dum_carb(ic_conc_CO3) &
         & )
    loc_CO2_d13C = loc_epsilon_db + loc_HCO3_d13C*(1.0 + loc_epsilon_db*1.0E-3)
    loc_CO3_d13C = loc_epsilon_cb + loc_HCO3_d13C*(1.0 + loc_epsilon_cb*1.0E-3)
    ! check - calculate d13C of DIC from component isotopic values
    loc_CO2_r13C  = fun_calc_isotope_fraction(loc_CO2_d13C,const_standards(11))
    loc_HCO3_r13C = fun_calc_isotope_fraction(loc_HCO3_d13C,const_standards(11))
    loc_CO3_r13C  = fun_calc_isotope_fraction(loc_CO3_d13C,const_standards(11))
    loc_DIC_r13C  = &
         & (loc_CO2_r13C*dum_carb(ic_conc_CO2) + loc_HCO3_r13C*dum_carb(ic_conc_HCO3) + loc_CO3_r13C*dum_carb(ic_conc_CO3))/ &
         dum_DIC
    loc_DIC_d13C = fun_calc_isotope_delta(dum_DIC,loc_DIC_r13C*dum_DIC,const_standards(11),.FALSE.,const_real_null)
    ! write to results variable
    dum_carbisor(ici_DIC_r13C)  = loc_DIC_r13C
    dum_carbisor(ici_CO2_r13C)  = loc_CO2_r13C
    dum_carbisor(ici_HCO3_r13C) = loc_HCO3_r13C
    dum_carbisor(ici_CO3_r13C)  = loc_CO3_r13C
  end subroutine sub_calc_carb_r13C
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE CARBONATE SYSTEM ISOTOPIC RATIOS (r14C)
  ! NOTE: fractonation factors after Zhang et al. [1995]
  ! NOTE: all equations following Zeebe and Wolf-Gladrow [2001] (section 3.2)
  subroutine sub_calc_carb_r14C(dum_T,dum_DIC,dum_DIC_14C,dum_carb,dum_carbisor)
    ! dummy variables
    REAL,INTENT(in)::dum_T,dum_DIC,dum_DIC_14C
    REAL,DIMENSION(n_carb),INTENT(in)::dum_carb 
    REAL,DIMENSION(n_carbisor),INTENT(inout)::dum_carbisor
    ! local variables
    real::loc_TC
    real::loc_epsilon_bg,loc_epsilon_dg,loc_epsilon_cg,loc_epsilon_cb,loc_epsilon_db
    real::loc_DIC_r14C,loc_CO2_r14C,loc_HCO3_r14C,loc_CO3_r14C
    real::loc_DIC_d14C,loc_CO2_d14C,loc_HCO3_d14C,loc_CO3_d14C
    ! initialize local variables
    loc_TC = dum_T - const_zeroC
    loc_DIC_d14C = fun_calc_isotope_delta(dum_DIC,dum_DIC_14C,const_standards(12),.FALSE.,const_real_null)
    ! calculate local fractionation factors
    ! epsilon_bg = epsilon(HCO3- - CO2(G))
    ! epsilon_dg = epsilon(CO2 - CO2(g))
    ! epsilon_cg = epsilon(CO32- - CO2(g))
    ! epsilon_cb = epsilon(CO32- - HCO3-)
    ! epsilon_db = epsilon(CO2 - HCO3-)
    loc_epsilon_bg = 2.0*(-0.1141*loc_TC + 10.78)
    loc_epsilon_dg = 2.0*(+0.0049*loc_TC - 1.31)
    loc_epsilon_cg = 2.0*(-0.052*loc_TC + 7.22)
    loc_epsilon_cb = loc_epsilon_cg - loc_epsilon_bg/(1.0 + loc_epsilon_bg*1.0E-3)
    loc_epsilon_db = loc_epsilon_dg - loc_epsilon_bg/(1.0 + loc_epsilon_bg*1.0E-3)
    ! calculate aqueous system deltas
    loc_HCO3_d14C = &
         & (loc_DIC_d14C*dum_DIC - (loc_epsilon_db*dum_carb(ic_conc_CO2) + loc_epsilon_cb*dum_carb(ic_conc_CO3)))/ &
         & ( &
         &   (1.0 + loc_epsilon_db*1.0E-3)*dum_carb(ic_conc_CO2) + &
         &   dum_carb(ic_conc_HCO3) + &
         &   (1.0 + loc_epsilon_cb*1.0E-3)*dum_carb(ic_conc_CO3) &
         & )
    loc_CO2_d14C = loc_epsilon_db + loc_HCO3_d14C*(1.0 + loc_epsilon_db*1.0E-3)
    loc_CO3_d14C = loc_epsilon_cb + loc_HCO3_d14C*(1.0 + loc_epsilon_cb*1.0E-3)
    ! check - calculate d14C of DIC from component isotopic values
    loc_CO2_r14C  = fun_calc_isotope_fraction(loc_CO2_d14C,const_standards(12))
    loc_HCO3_r14C = fun_calc_isotope_fraction(loc_HCO3_d14C,const_standards(12))
    loc_CO3_r14C  = fun_calc_isotope_fraction(loc_CO3_d14C,const_standards(12))
    loc_DIC_r14C  = &
         & (loc_CO2_r14C*dum_carb(ic_conc_CO2) + loc_HCO3_r14C*dum_carb(ic_conc_HCO3) + loc_CO3_r14C*dum_carb(ic_conc_CO3))/ &
         dum_DIC
    loc_DIC_d14C = fun_calc_isotope_delta(dum_DIC,loc_DIC_r14C*dum_DIC,const_standards(12),.FALSE.,const_real_null)
    ! write to results variable
    dum_carbisor(ici_DIC_r14C)  = loc_DIC_r14C
    dum_carbisor(ici_CO2_r14C)  = loc_CO2_r14C
    dum_carbisor(ici_HCO3_r14C) = loc_HCO3_r14C
    dum_carbisor(ici_CO3_r14C)  = loc_CO3_r14C
  end subroutine sub_calc_carb_r14C
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE CHLORINITY
  FUNCTION fun_calc_Cl(dum_S)
    ! result variable
    REAL::fun_calc_Cl
    ! dummy arguments
    REAL,INTENT(IN)::dum_S
    ! chlorinity [Zeebe and Wolf-Gladrow, 2001]
    fun_calc_Cl = dum_S/1.80655
    If (fun_calc_Cl < const_real_nullsmall) fun_calc_cl = const_real_nullsmall
  END FUNCTION fun_calc_Cl
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE THE IONIC STRENGTH OF SEAWATER
  FUNCTION fun_calc_ION(dum_Cl)
    ! result variable
    REAL::fun_calc_ION
    ! dummy arguments
    REAL,INTENT(IN)::dum_Cl
    ! the ionic strength calculated after Zeebe and Wolf-Gladrow [2001]
    If (dum_Cl > const_real_nullsmall) then
       fun_calc_ION = 0.00147 + 0.03592*dum_Cl + 0.000068*dum_Cl*dum_Cl
    else
       fun_calc_ION = const_real_nullsmall
    end If
  END FUNCTION fun_calc_ION
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ALTERNATIVE MEASURE OF THE IONIC STRENGTH OF SEAWATER
  FUNCTION fun_calc_I(dum_S)
    ! result variable
    REAL::fun_calc_I
    ! dummy arguments
    REAL,INTENT(IN)::dum_S
    ! the ionic strength calculated after Millero [1982]
    If (dum_S > const_real_nullsmall) then
       fun_calc_I = 19.924*dum_S/(1000.0 - 1.005*dum_S)
    else
       fun_calc_I = const_real_nullsmall
    end If
  END FUNCTION fun_calc_I
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ESTIMATE CONCENTRATION OF Ca2+ FROM SALINITY
  ! NOTE: this is not utilized if Ca is selected as an explicit tracer
  FUNCTION fun_calc_Ca(dum_S)
    ! result variable
    REAL::fun_calc_Ca
    ! dummy arguments
    REAL,INTENT(IN)::dum_S
    ! total Ca++ concentration estimated after Millero [1982]
    fun_calc_Ca = 0.01028*dum_S/35.0
    If (fun_calc_Ca < const_real_nullsmall) fun_calc_Ca = const_real_nullsmall
  END FUNCTION fun_calc_Ca
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ESTIMATE TOTAL BORON CONCENTRATION FROM SALINITY
  FUNCTION fun_calc_Btot(dum_S)
    ! result variable
    REAL::fun_calc_Btot
    ! dummy arguments
    REAL,INTENT(IN)::dum_S
    ! total boroic acid in seawater estimated after Uppstrom [1974]
    ! NOTE: this is to provide consistence with the 'CO2SYS' model of Lewis and Wallace
    ! NOTE: the alternative formulation is; Btot = 0.000426*dum_S/35.0 [Millero, 1982, 1995]
    fun_calc_Btot = 0.000416*dum_S/35.0
    If (fun_calc_Btot < const_real_nullsmall) fun_calc_Btot = const_real_nullsmall
  END FUNCTION fun_calc_Btot
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ESTIMATE CONCENTRATION OF F- FROM SALINITY
  FUNCTION fun_calc_Ftot(dum_S)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_Ftot
    ! dummy arguments
    REAL,INTENT(IN)::dum_S
    ! total F- concentration estimated after Millero [1982]
    fun_calc_Ftot = 0.00007*dum_S/35.0
    If (fun_calc_Ftot < const_real_nullsmall) fun_calc_Ftot = const_real_nullsmall
  END FUNCTION fun_calc_Ftot
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ESTIMATE CONCENTRATION OF SO42-
  ! NOTE: this is not utilized if SO42- is selected as an explicit tracer
  FUNCTION fun_calc_SO4tot(dum_S)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_SO4tot
    ! dummy arguments
    REAL,INTENT(IN)::dum_S
    ! total SO4-- concentration (= HSO4- + SO42-)
    ! NOTE: the alternative formulation is; Stot = 0.0293*dum_S/35.0 [Millero, 1982]
    fun_calc_SO4tot = 0.02824*dum_S/35.0
    If (fun_calc_SO4tot < const_real_nullsmall) fun_calc_SO4tot = const_real_nullsmall
  END FUNCTION fun_calc_SO4tot
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KB)
  FUNCTION fun_calc_lnkB(dum_T,dum_rT,dum_T_ln,dum_S,dum_S_p05,dum_S_p15,dum_S_p20)
    ! result variable
    REAL::fun_calc_lnkB
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_rT,dum_T_ln,dum_S,dum_S_p05,dum_S_p15,dum_S_p20
    ! apparent ionization constant of boric acid [Dickson, 1990]
    ! NOTE: total pH scale; [mol (kg-H20)-1]
    fun_calc_lnkB = 148.0248 + 137.194*dum_S_p05 + 1.62247*dum_S &
         & + (-8966.90 - 2890.51*dum_S_p05 - 77.942*dum_S + 1.726*dum_S_p15 - 0.0993*dum_S_p20)*dum_rT &
         & + (-24.4344 - 25.085*dum_S_p05 - 0.2474*dum_S)*dum_T_ln &
         & + 0.053105*dum_S_p05*dum_T
  END FUNCTION fun_calc_lnkB
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KW)
  FUNCTION fun_calc_lnkW(dum_rT,dum_T_ln,dum_S,dum_S_p05)
    ! result variable
    REAL::fun_calc_lnkW
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_T_ln,dum_S,dum_S_p05
    ! apparent ionization constant of water [Millero, 1992]
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    fun_calc_lnkW = 148.9802 - 13847.26*dum_rT - 23.6521*dum_T_ln &
         & + (-5.977 + 118.67*dum_rT + 1.0495*dum_T_ln)*dum_S_p05 - 0.01615*dum_S
  END FUNCTION fun_calc_lnkW
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KSi)
  FUNCTION fun_calc_lnkSi(dum_rT,dum_T_ln,dum_I,dum_I_p05)
    ! result variable
    REAL::fun_calc_lnkSi
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_T_ln,dum_I,dum_I_p05
    ! calculation of the dissociation constant of SiO2 [Yao and Millero, 1995]
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    fun_calc_lnkSi = 117.40 - 8904.2*dum_rT - 19.334*dum_T_ln &
         & + (3.5913 - 458.79*dum_rT)*dum_I_p05 + (-1.5998 + 188.74*dum_rT)*dum_I + (0.07871 - 12.1652*dum_rT)*dum_I*dum_I
  END FUNCTION fun_calc_lnkSi
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KNF)
  FUNCTION fun_calc_lnkHF(dum_T,dum_ION_p05)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_lnkHF
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_ION_p05
    ! calculation of the dissociation constant of HF [Dickson and Riley, 1979]
    ! NOTE: free pH scale; [mol (kg-sol)-1]
    fun_calc_lnkHF = 1590.2/dum_T - 12.641 + 1.525*dum_ION_p05
  END FUNCTION fun_calc_lnkHF
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE LN(KP1)
  FUNCTION fun_calc_lnkP1(dum_T,dum_T_ln,dum_S,dum_S_p05)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_lnkP1
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_T_ln,dum_S,dum_S_p05
    ! calculation of the 1st dissociation constant of phosphoric acid in seawater  [Yao and Millero, 1995]
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    fun_calc_lnkP1 = 115.54 - 4576.752/dum_T - 18.453*dum_T_ln &
         + (0.69171 - 106.736/dum_T)*dum_S_p05 + (-0.01844 - 0.65643/dum_T)*dum_S
  END FUNCTION fun_calc_lnkP1
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KP2)
  FUNCTION fun_calc_lnkP2(dum_T,dum_T_ln,dum_S,dum_S_p05)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_lnkP2
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_T_ln,dum_S,dum_S_p05
    ! calculation of the 2nd dissociation constant of phosphoric acid in seawater [Yao and Millero, 1995]
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    fun_calc_lnkP2 = 172.1033 - 8814.715/dum_T - 27.927*dum_T_ln &
         + (1.3566 - 160.340/dum_T)*dum_S_p05 + (-0.05778 + 0.37335/dum_T)*dum_S
  END FUNCTION fun_calc_lnkP2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KP3)
  FUNCTION fun_calc_lnkP3(dum_T,dum_S,dum_S_p05)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_lnkP3
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_S,dum_S_p05
    ! calculation of the 3rd dissociation constant of phosphoric acid in seawater [Yao and Millero, 1995]
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    fun_calc_lnkP3 = -18.126 - 3070.75/dum_T + &
         (2.81197 + 17.27039/dum_T)*dum_S_p05 + (-0.09984 - 44.99486/dum_T)*dum_S
  END FUNCTION fun_calc_lnkP3
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KHSO4)
  FUNCTION fun_calc_lnkHSO4(dum_rT,dum_T_ln,dum_I_p05,dum_I,dum_I_p15,dum_I_p20)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_lnkHSO4
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_T_ln,dum_I_p05,dum_I,dum_I_p15,dum_I_p20
    ! calculation of the dissociation constant of HSO4 [Dickson, 1990]
    ! NOTE: free pH scale; [mol (kg-H2O)-1]
    fun_calc_lnkHSO4 = 141.328 - 4276.1*dum_rT - 23.093*dum_T_ln &
         & + (324.57 - 13856.0*dum_rT - 47.986*dum_T_ln)*dum_I_p05 &
         & + (-771.54 + 35474.0*dum_rT + 114.723*dum_T_ln)*dum_I &
         & -2698.0*dum_rT*dum_I_p15 + 1776.0*dum_rT*dum_I_p20
  END FUNCTION fun_calc_lnkHSO4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE LN(KHSO4)
  ! NOTE: this particular (alternative) empirical relationship is not currently employed
  FUNCTION fun_calc_lnkHSO4_Khoo(dum_T,dum_I_p05)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_lnkHSO4_Khoo
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_I_p05
    ! calculation of the dissociation constant of HSO4 [Khoo et al. 1977]
    ! NOTE: free pH scale; [mol (kg-sol)-1] (???)
    fun_calc_lnkHSO4_Khoo = LOG(10**(647.59/dum_T - 6.3451 + 0.019085*dum_T - 0.5208*dum_I_p05))
  END FUNCTION fun_calc_lnkHSO4_Khoo
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KH2S)
  FUNCTION fun_calc_lnkH2S(dum_rT,dum_T_ln,dum_S,dum_S_p05)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_lnkH2S
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_T_ln,dum_S,dum_S_p05
    ! calculation of the dissociation constant of HS [Millero et al., 1988]
    ! NOTE: total pH scale; [mol (kg-sol)-1]
    fun_calc_lnkH2S = 225.838 - 13275.3*dum_rT - 34.6435*dum_T_ln + 0.3449*dum_S_p05 - 0.0274*dum_S
  END FUNCTION fun_calc_lnkH2S
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(KNH4)
  FUNCTION fun_calc_lnkNH4(dum_T,dum_rT,dum_S,dum_S_p05)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_lnkNH4
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_rT,dum_S,dum_S_p05
    ! calculation of the dissociation constant of NH4 [Yao and Millero, 1995]
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    fun_calc_lnkNH4 = -6285.33*dum_rT + 0.0001635*dum_T - 0.25444 + &
         & (0.46532 - 123.7184*dum_rT)*dum_S_p05 + (-0.01992 + 3.17556*dum_rT)*dum_S
  END FUNCTION fun_calc_lnkNH4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(K1) DISSOCIATION CONSTANT
  FUNCTION fun_calc_lnk1_Mehrbach(dum_rT,dum_T_ln,dum_S,dum_S_p20)
    ! result variable
    REAL::fun_calc_lnk1_Mehrbach
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_T_ln,dum_S,dum_S_p20
    ! first carbonic acid apparent ionization constant; Mehrbach et al. [1973] (re-fit by [Dickson and Millero 1987])
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    ! NOTE: convert pk rather to ln(k) by; pk - -log10(k)
    fun_calc_lnk1_Mehrbach = LOG(10**( &
         & -(3670.7*dum_rT - 62.008 + 9.7944*dum_T_ln - 0.0118*dum_S + 0.000116*dum_S_p20) &
         & ))
  END FUNCTION fun_calc_lnk1_Mehrbach
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(K2) DISSOCIATION CONSTANT
  FUNCTION fun_calc_lnk2_Mehrbach(dum_rT,dum_S,dum_S_p20)
    ! result variable
    REAL::fun_calc_lnk2_Mehrbach
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_S,dum_S_p20
    ! second carbonic acid apparent ionization constant; Mehrbach et al. [1973] (re-fit by [Dickson and Millero 1987])
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    ! NOTE: convert pk rather to ln(k) by; pk - -log10(k)
    fun_calc_lnk2_Mehrbach = LOG(10**( &
         &  -(1394.7*dum_rT + 4.777 - 0.0184*dum_S + 0.000118*dum_S_p20) &
         &  ))
  END FUNCTION fun_calc_lnk2_Mehrbach
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(K1) DISSOCIATION CONSTANT
  FUNCTION fun_calc_lnk1_Hansson(dum_rT,dum_S,dum_S_p20)
    ! result variable
    REAL::fun_calc_lnk1_Hansson
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_S,dum_S_p20
    ! first carbonic acid apparent ionization constant; Hansson [1973]
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    ! NOTE: convert pk rather to ln(k) by; pk - -log10(k)
    fun_calc_lnk1_Hansson = LOG(10**( &
         & -(851.4*dum_rT + 3.237 - 0.0106*dum_S + 0.000105*dum_S_p20) &
         & ))
  END FUNCTION fun_calc_lnk1_Hansson
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(K2) DISSOCIATION CONSTANT
  FUNCTION fun_calc_lnk2_Hansson(dum_rT,dum_T_ln,dum_S,dum_S_p20)
    ! result variable
    REAL::fun_calc_lnk2_Hansson
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_T_ln,dum_S,dum_S_p20
    ! second carbonic acid apparent ionization constant; Hansson [1973]
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    ! NOTE: convert pk rather to ln(k) by; pk - -log10(k)
    fun_calc_lnk2_Hansson = LOG(10**( &
         &  -(-3885.4*dum_rT + 125.844 - 18.141*dum_T_ln - 0.0192*dum_S + 0.000132*dum_S_p20) &
         &  ))
  END FUNCTION fun_calc_lnk2_Hansson
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(K1) DISSOCIATION CONSTANT
  FUNCTION fun_calc_lnk1_DicksonMillero(dum_rT,dum_S,dum_S_p20)
    ! result variable
    REAL::fun_calc_lnk1_DicksonMillero
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_S,dum_S_p20
    ! first carbonic acid apparent ionization constant; combined Dickson and Millero [1987] fit
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    ! NOTE: convert pk rather to ln(k) by; pk - -log10(k)
    fun_calc_lnk1_DicksonMillero = LOG(10**( &
         & -(845.0*dum_rT + 3.284 - 0.0098*dum_S + 0.000087*dum_S_p20) &
         & ))
  END FUNCTION fun_calc_lnk1_DicksonMillero
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(K2) DISSOCIATION CONSTANT
  FUNCTION fun_calc_lnk2_DicksonMillero(dum_rT,dum_S,dum_S_p20)
    ! result variable
    REAL::fun_calc_lnk2_DicksonMillero
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_S,dum_S_p20
    ! second carbonic acid apparent ionization constant; combined Dickson and Millero [1987] fit
    ! NOTE: SWS pH scale; [mol (kg-sol)-1]
    ! NOTE: convert pk rather to ln(k) by; pk - -log10(k)
    fun_calc_lnk2_DicksonMillero = LOG(10**( &
         &  -(1377.3*dum_rT + 4.824 - 0.0185*dum_S + 0.000122*dum_S_p20) &
         &  ))
  END FUNCTION fun_calc_lnk2_DicksonMillero
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(K1) DISSOCIATION CONSTANT
  FUNCTION fun_calc_lnk1_Roy(dum_rT,dum_T_ln,dum_S_p05,dum_S,dum_S_p15)
    ! result variable
    REAL::fun_calc_lnk1_Roy
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_T_ln,dum_S_p05,dum_S,dum_S_p15
    ! first carbonic acid apparent ionization constant [Roy et al., 1993]
    ! NOTE: total pH scale; [mol (kg-sol)-1]
    fun_calc_lnk1_Roy = 2.83655 - 2307.1266*dum_rT - 1.5529413*dum_T_ln &
         & + (-0.20760841 - 4.0484*dum_rT)*dum_S_p05 + 0.08468345*dum_S - 0.00654208*dum_S_p15
  END FUNCTION fun_calc_lnk1_Roy
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(K1) DISSOCIATION CONSTANT
  FUNCTION fun_calc_lnk2_Roy(dum_rT,dum_T_ln,dum_S_p05,dum_S,dum_S_p15)
    ! result variable
    REAL::fun_calc_lnk2_Roy
    ! dummy arguments
    REAL,INTENT(IN)::dum_rT,dum_T_ln,dum_S_p05,dum_S,dum_S_p15
    ! second carbonic acid apparent ionization constant [Roy et al., 1993]
    ! NOTE: total pH scale; [mol (kg-sol)-1]
    fun_calc_lnk2_Roy = -9.226508 - 3351.6106*dum_rT - 0.2005743*dum_T_ln &
         & + (-0.106901773 - 23.9722*dum_rT)*dum_S_p05 + 0.1130822*dum_S - 0.00846934*dum_S_p15
  END FUNCTION fun_calc_lnk2_Roy
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(Q_CO2)
  ! NOTE: alternative calculations of gas solubility may be substituted elsewhere
  FUNCTION fun_calc_lnQ_CO2(dum_Tr100,dum_rT,dum_S)
    ! result variable
    REAL::fun_calc_lnQ_CO2
    ! dummy arguments
    REAL,INTENT(IN)::dum_Tr100,dum_rT,dum_S
    ! Henry's Law constant for CO2 with concentrations as moles per kg of solution [Weiss, 1974].
    ! The dependence of the solubility coefficient on hydrostatic pressure is ignored.
    fun_calc_lnQ_CO2 = -60.2409 + 93.4517*(100*dum_rT) + 23.3585*LOG(dum_Tr100) &
         & +dum_S*(0.023517 - 0.023656*(dum_Tr100) + 0.0047036*(dum_Tr100)**2)
  END FUNCTION fun_calc_lnQ_CO2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ln(Q_O2)
  ! NOTE: alternative calculations of gas solubility may be substituted elsewhere
  FUNCTION fun_calc_lnQ_O2(dum_Tr100,dum_rT,dum_S)
    ! result variable
    REAL::fun_calc_lnQ_O2
    ! dummy arguments
    REAL,INTENT(IN)::dum_Tr100,dum_rT,dum_S
    ! Henry's Law constant for O2 with concentrations as moles per kg of solution.
    ! Adapted from a formula from Millero and Sohn [1992] giving O2 solubility in units of umol kg-1,
    ! back to a Henry's Law constant for O2 in units of mol kg-1 atm-1.
    ! The dependence of the solubility coefficient on hydrostatic pressure is ignored.
    fun_calc_lnQ_O2 = -173.9894 + 255.5907*(100.0*dum_rT) + 146.4813*LOG(dum_Tr100) - 22.2040*(dum_Tr100) &
         & + dum_S*(-0.037362 + 0.016504*(dum_Tr100) - 0.0020564*(dum_Tr100)**2) &
         & - LOG(1.0E6) - LOG(0.20946)
  END FUNCTION fun_calc_lnQ_O2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE log10(Kcal)
  FUNCTION fun_calc_logkcal(dum_T,dum_rT,dum_T_log,dum_S,dum_S_p05,dum_S_p15)
    ! result variable
    REAL::fun_calc_logkcal
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_rT,dum_T_log,dum_S,dum_S_p05,dum_S_p15
    ! Calculation of the solubility constant of calcite.
    ! return function value
    fun_calc_logkcal = -171.9065 - 0.077993*dum_T + 2839.319*dum_rT + 71.595*dum_T_log &
         & + (-0.77712 + 0.0028426*dum_T + 178.34*dum_rT)*dum_S_p05 - 0.07711*dum_S + 0.0041249*dum_S_p15
  END FUNCTION fun_calc_logkcal
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE log10(Karg)
  FUNCTION fun_calc_logkarg(dum_T,dum_rT,dum_T_log,dum_S,dum_S_p05,dum_S_p15)
    ! result variable
    REAL::fun_calc_logkarg
    ! dummy arguments
    REAL,INTENT(IN)::dum_T,dum_rT,dum_T_log,dum_S,dum_S_p05,dum_S_p15
    ! Calculation of the solubility constant of aragonite.
    fun_calc_logkarg = -171.945 - 0.077993*dum_T + 2903.293*dum_rT + 71.595*dum_T_log &
         & + (-0.068393 + 0.0017276*dum_T + 88.135*dum_rT)*dum_S_p05 - 0.10018*dum_S + 0.0059415*dum_S_p15
  END FUNCTION fun_calc_logkarg
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ESTIMATE DIC GIVEN dCO3 AND ALK
  function fun_find_DIC_from_dCO3(                                                  &
       & dum_dCO3,dum_ALK,dum_DIC_low,dum_DIC_high,dum_Ca,                          &
       & dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot, &
       & dum_carbconst)
    ! result variable
    REAL::fun_find_DIC_from_dCO3
    ! dummy variables
    REAL,INTENT(in)::dum_dCO3,dum_ALK,dum_DIC_low,dum_DIC_high,dum_Ca
    REAL,INTENT(in)::dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot
    REAL,DIMENSION(n_carbconst),intent(in)::dum_carbconst
    ! local variables
    REAL::loc_DIC,loc_DIC_low,loc_DIC_high
    REAL,DIMENSION(n_carb)::loc_carb
    REAL,DIMENSION(n_carbalk)::loc_carbalk
    ! initialize low DIC search limits
    loc_DIC_low  = dum_DIC_low
    loc_DIC_high = dum_DIC_high
    ! carry out DIC search
    DO
       loc_DIC = (loc_DIC_low + loc_DIC_high)/2.0
       call sub_calc_carb(                                                               &
            & loc_DIC,dum_ALK,dum_Ca,                                                    &
            & dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot, &
            & dum_carbconst,loc_carb,loc_carbalk)
       IF (loc_carb(ic_dCO3_cal) < dum_dco3) THEN
          loc_DIC_high = loc_DIC
       ELSE
          loc_DIC_low = loc_DIC
       ENDIF
       ! test for dCO3 estimate getting 'sufficiently' close to target value
       IF (ABS(loc_carb(ic_dCO3_cal) - dum_dCO3) < 0.01E-6) EXIT
       ! test for upper or lower bounds of search limit being approached
       if ((dum_DIC_high - loc_DIC_low) < 1.0E-6) then
          loc_DIC = dum_DIC_high
          exit
       end if
       if ((loc_DIC_high - dum_DIC_low) < 1.0E-6) then
          loc_DIC = dum_DIC_low
          exit
       end if
    ENDDO
    ! return function value
    fun_find_DIC_from_dCO3 = loc_DIC
  END function fun_find_DIC_from_dCO3
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! ESTIMATE ALK GIVEN dCO3 AND DIC
  function fun_find_ALK_from_dCO3(                                                  &
       & dum_dCO3,dum_DIC,dum_ALK_low,dum_ALK_high,dum_Ca,                          &
       & dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot, &
       & dum_carbconst)
    ! result variable
    REAL::fun_find_ALK_from_dCO3
    ! dummy variables
    REAL,INTENT(in)::dum_dCO3,dum_DIC,dum_ALK_low,dum_ALK_high,dum_Ca
    REAL,INTENT(in)::dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot
    REAL,DIMENSION(n_carbconst),intent(in)::dum_carbconst
    ! local variables
    real::loc_ALK,loc_ALK_low,loc_ALK_high
    REAL,DIMENSION(n_carb)::loc_carb
    REAL,DIMENSION(n_carbalk)::loc_carbalk
    ! initialize low DIC search limits
    loc_ALK_low  = dum_ALK_low
    loc_ALK_high = dum_ALK_high
    ! carry out ALK search
    DO
       loc_ALK = (loc_ALK_low + loc_ALK_high)/2.0
       call sub_calc_carb(                                                               &
            & dum_DIC,loc_ALK,dum_Ca,                                                    &
            & dum_PO4tot,dum_SiO2tot,dum_Btot,dum_SO4tot,dum_Ftot,dum_H2Stot,dum_NH4tot, &
            & dum_carbconst,loc_carb,loc_carbalk)
       IF (loc_carb(ic_dCO3_cal) > dum_dco3) THEN
          loc_ALK_high = loc_ALK
       ELSE
          loc_ALK_low = loc_ALK
       ENDIF
       ! test for dCO3 estimate getting 'sufficiently' close to target value
       IF (ABS(loc_carb(ic_dCO3_cal) - dum_dCO3) < 0.01E-6) EXIT
       ! test for upper or lower bounds of search limit being approached
       if ((dum_ALK_high - loc_ALK_low) < 1.0E-6) then
          loc_ALK = dum_ALK_high
          exit
       end if
       if ((loc_ALK_high - dum_ALK_low) < 1.0E-6) then
          loc_ALK = dum_ALK_low
          exit
       end if
    ENDDO
    ! return function value
    fun_find_ALK_from_dCO3 = loc_ALK
  END function fun_find_ALK_from_dCO3
  ! ****************************************************************************************************************************** !
  
  
  ! ****************************************************************************************************************************** !
  ! CALCULATE Corg d13C (/d14C)
  FUNCTION fun_Corg_Rfrac(dum_T,dum_conc_CO2,dum_CO2_r13C,dum_Corg_ef,dum_14C)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    REAL::fun_Corg_Rfrac
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real::dum_T
    real::dum_conc_CO2
    real::dum_CO2_r13C
    real::dum_Corg_ef
    logical::dum_14C
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_Kq
    real::loc_delta_Corg
    real::loc_alpha
    real::loc_R
    ! -------------------------------------------------------- !
    ! CALCULATE carbon isotope fractionation between DIC and POC
    ! -------------------------------------------------------- !
    ! NOTE: double fractionation for 14C
    loc_Kq = const_d13C_DIC_Corg_Q2_c + const_d13C_DIC_Corg_Q2_x*dum_T + const_d13C_DIC_Corg_Q2_x2*dum_T**2
    loc_delta_Corg = -dum_Corg_ef + (dum_Corg_ef - const_d13C_DIC_Corg_ed)*loc_Kq/dum_conc_CO2
    if (dum_14C) loc_delta_Corg = 2.0*loc_delta_Corg
    loc_alpha = 1.0 + loc_delta_Corg/1000.0
    loc_R = dum_CO2_r13C/(1.0 - dum_CO2_r13C)
    ! -------------------------------------------------------- ! return function value
    fun_Corg_Rfrac = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_Corg_Rfrac
  ! ****************************************************************************************************************************** !
  
  
  ! ****************************************************************************************************************************** !
  ! CALCULATE Corg d13C (/d14C) [Popp et al. fractionation scheme]
  FUNCTION fun_Corg_Rfrac_Poppetal(dum_conc_CO2,dum_CO2_r13C,dum_Corg_ef,dum_Corg_b,dum_14C)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    REAL::fun_Corg_Rfrac_Poppetal
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real::dum_conc_CO2
    real::dum_CO2_r13C
    real::dum_Corg_ef
    real::dum_Corg_b
    logical::dum_14C
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_delta_Corg
    real::loc_alpha
    real::loc_R
    ! -------------------------------------------------------- !
    ! CALCULATE carbon isotope fractionation between DIC and POC
    ! -------------------------------------------------------- !
    ! NOTE: double fractionation for 14C
    loc_delta_Corg = -dum_Corg_ef + dum_Corg_b/(1.0E6*dum_conc_CO2)
    if (dum_14C) loc_delta_Corg = 2.0*loc_delta_Corg
    loc_alpha = 1.0 + loc_delta_Corg/1000.0
    loc_R = dum_CO2_r13C/(1.0 - dum_CO2_r13C)
    ! -------------------------------------------------------- ! return function value
    fun_Corg_Rfrac_Poppetal = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END FUNCTION fun_Corg_Rfrac_Poppetal
  ! ****************************************************************************************************************************** !


END MODULE gem_carbchem


