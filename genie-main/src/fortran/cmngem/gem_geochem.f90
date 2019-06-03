! ******************************************************************************************************************************** !
! gem_geochem.f90
! Geochemistry (non carbonate chemistry) Model
! ******************************************************************************************************************************** !


MODULE gem_geochem


  use gem_cmn
  use gem_util
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! CALCULATE SOLUBILITY COEFFICIENT
  function fun_calc_solconst(dum_ia,dum_T,dum_S,dum_rho)
    ! result variable
    REAL::fun_calc_solconst
    ! dummy arguments
    integer,INTENT(in)::dum_ia
    real,INTENT(in)::dum_T,dum_S,dum_rho
    ! local variables
    REAL::loc_T,loc_rT,loc_Tr100,loc_S
    ! calculate local constants
    ! NOTE: pressure in units of (bar) (1 m depth approx = 1 dbar pressure)
    ! NOTE: temperature in K
    ! NOTE: restrict valid T,S range for empirical fit
    ! NOTE: original default was the same as for Mehrbach K1, K2 CONSTANTS:
    !       2.0  <= T <= 35 C
    !       26.0 <= S <= 43 PSU
    ! NOTE: hence carbchem_*, rather than geochem_* parameter set (logically: geochem_* as per air-sea gas exchange ...)
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
    ! set local constants
    loc_rT    = 1.0/loc_T
    loc_Tr100 = loc_T/100.0
    ! calculate Solubility Coefficients (mol/(kg atm)) and return function value
    ! NOTE: for CO2 and N2O, the soluability coefficient is in units of mol/(kg atm)
    !       rather than as a Bunsen Solubility Coefficient (see Wanninkohf [1992])
    !       => convert units for others
    ! NOTE: for CFC-11 and CFC-12, the soluability coefficient is in units of mol/(kg atm)
    !       rather than as a Bunsen Solubility Coefficient (see Wanninkohf [1992])
    !       (actaully, it is not really this simple and K should be corrected for water vapour pressure and lame things like that)
    SELECT CASE (dum_ia)
    CASE (ia_pCO2,ia_pN2O)
       fun_calc_solconst = EXP( &
            & par_bunsen_coef(1,dum_ia) + par_bunsen_coef(2,dum_ia)*(100*loc_rT) + par_bunsen_coef(3,dum_ia)*LOG(loc_Tr100) + &
            & loc_S* &
            & (par_bunsen_coef(4,dum_ia) + par_bunsen_coef(5,dum_ia)*(loc_Tr100) + par_bunsen_coef(6,dum_ia)*(loc_Tr100)**2) &
            &  )
    CASE (ia_pCFC11,ia_pCFC12)
       fun_calc_solconst = EXP( &
            & par_bunsen_coef(1,dum_ia) + par_bunsen_coef(2,dum_ia)*(100*loc_rT) + par_bunsen_coef(3,dum_ia)*LOG(loc_Tr100) + &
            & loc_S* &
            & (par_bunsen_coef(4,dum_ia) + par_bunsen_coef(5,dum_ia)*(loc_Tr100) + par_bunsen_coef(6,dum_ia)*(loc_Tr100)**2) &
            &  )
    CASE default
       fun_calc_solconst = EXP( &
            & par_bunsen_coef(1,dum_ia) + par_bunsen_coef(2,dum_ia)*(100*loc_rT) + par_bunsen_coef(3,dum_ia)*LOG(loc_Tr100) + &
            & loc_S* &
            & (par_bunsen_coef(4,dum_ia) + par_bunsen_coef(5,dum_ia)*(loc_Tr100) + par_bunsen_coef(6,dum_ia)*(loc_Tr100)**2) &
            &  )/ &
            & (dum_rho*const_V)
    END SELECT
  end function fun_calc_solconst
  ! ****************************************************************************************************************************** !


END MODULE gem_geochem

