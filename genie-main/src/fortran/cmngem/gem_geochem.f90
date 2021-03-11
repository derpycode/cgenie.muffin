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
  ! Fe SPECIATION
  function fun_box_calc_geochem_Fe(dum_FeT,dum_LT,dum_par_K_FeL)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,DIMENSION(1:3)::fun_box_calc_geochem_Fe
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real,INTENT(in)::dum_FeT,dum_LT
    real,INTENT(in)::dum_par_K_FeL
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_Fe,loc_FeL,loc_L
    real,DIMENSION(2)::loc_roots
    ! -------------------------------------------------------- !
    ! CALCULATE IRON SPECIATION
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! solve Fe speciation equation
    ! K = FeL / (Fe*L) (e.g. see: Parekth et al. [2005])
    ! => FeL = Fe*L*K
    !    conservation relations:
    !    FeL + Fe = FeT => Fe = FeT - FeL
    !    FeL + L  = LT  => L  = LT  - FeL
    !    substitute:
    !    FeL = (FeT - FeL)*(LT - FeL)*K
    !    => FeL/K = FeT*LT + FeL^2 - LT*FeL - FeT*FeL
    !    => FeL/K = FeL^2 - (LT + FeT)*FeL + FeT*LT
    !    => 1.0*FeL^2 - (LT + FeT + 1.0/K)*FeL + FeT*LT = 0.0
    !       solve as: ax2 + bx + c = 0.0
    !                 where x = FeL
    loc_roots(:) = fun_quad_root(1.0,-(dum_LT + dum_FeT + 1.0/dum_par_K_FeL),dum_FeT*dum_LT)
    ! -------------------------------------------------------- ! filter returned roots
    if (maxval(loc_roots(:)) < const_real_nullsmall) then
       IF (ctrl_debug_reportwarnings) THEN
          CALL sub_report_error( &
               & 'biogem_box.f90','sub_calc_geochem_Fe', &
               & 'No REAL root in Fe speciation calculation (or maybe zero ...).'// &
               & ' / Data: loc_FeL(OLD),loc_Fe(OLD),loc_L(OLD),dum_FeT,dum_LT,', &
               & 'SOD THIS FOR A GAME OF SOLDIERS: calculation abondoned ...', &
               & (/loc_FeL,loc_Fe,loc_L,dum_FeT,dum_LT/),.false. &
               & )
          error_stop = .FALSE.
       end IF
    elseif ((minval(loc_roots(:)) > dum_FeT) .AND. (minval(loc_roots(:)) > dum_LT)) then
       IF (ctrl_debug_reportwarnings) THEN
          CALL sub_report_error( &
               & 'biogem_box.f90','sub_calc_geochem_Fe', &
               & 'No solution to Fe speciation calculation possible ... :('// &
               & ' / Data: loc_FeL(OLD),loc_Fe(OLD),loc_L(OLD),dum_FeT,dum_LT,', &
               & 'SOD THIS FOR A GAME OF SOLDIERS: calculation abondoned ...', &
               & (/loc_FeL,loc_Fe,loc_L,dum_FeT,dum_LT/),.false. &
               & )
          error_stop = .FALSE.
       end IF
    else
       if (minval(loc_roots(:)) < const_real_nullsmall) then
          loc_FeL = maxval(loc_roots(:))
       else
          loc_FeL = minval(loc_roots(:))
       end if
       loc_Fe  = dum_FeT - loc_FeL
       loc_L   = dum_LT - loc_FeL
    end if
    ! -------------------------------------------------------- !
    ! RETURN RESULT
    ! -------------------------------------------------------- !
    fun_box_calc_geochem_Fe(1) = loc_Fe
    fun_box_calc_geochem_Fe(2) = loc_FeL
    fun_box_calc_geochem_Fe(3) = loc_L
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end function fun_box_calc_geochem_Fe
  ! ****************************************************************************************************************************** !


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


  ! ****************************************************************************************************************************** !
  function fun_box_calc_spec_Fe2(dum_Fe2,dum_H2S,dum_par_bio_FeS_abioticohm_cte)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real,DIMENSION(1:3)::fun_box_calc_spec_Fe2
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real,INTENT(in)::dum_Fe2,dum_H2S
    real,INTENT(in)::dum_par_bio_FeS_abioticohm_cte
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_Fe2,loc_H2S,loc_FeS
    real,DIMENSION(2)::loc_roots
    ! -------------------------------------------------------- !
    ! CALCULATE reduced IRON SPECIATION
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! solve reduced Fe speciation equation
    ! 
    ! calculate solution for equilibrium:
    ! Fe2_eq*H2S_eq/FeS_eq = K
    ! Fe2_eq = Fe2_in - x; H2S_eq = H2S_in - x; FeS_eq = FeS_in + x
    ! (Fe2_in - x)(H2S_in - x) = K*FeS_in + K*x
    ! Fe_in*H2S_in - H2S_in*x - Fe_in*x + x^2 = K*FeS_in + K*x
    ! x^2 - (H2S_in + Fe_in + K) + (Fe_in*H2S_in - K*FeS_in) = 0
    
    ! K = Fe2_eq*H2S_eq/FeS_eq 
    ! => Fe2_eq*H2S_eq = K*FeS_eq
    !    conservation relations:
    !    Fe2_eq + FeS_eq = Fe2_tot => Fe2_eq = Fe2_tot - FeS_eq
    !    H2S_eq + FeS_eq = H2S_tot => H2S_eq = H2S_tot - FeS_eq
    !    substitute:
    !    (Fe2_tot - FeS_eq)(H2S_tot - FeS_eq) = K*FeS_eq
    !    => Fe2_tot*H2S_tot - Fe2_tot*FeS_eq - H2S_tot*FeS_eq + FeS_eq^2 = K*FeS_eq
    !    => 1.0*FeS_eq^2 - (Fe2_tot + H2S_tot + K) + Fe2_tot*H2S_tot = 0.0
    !    solve as: ax2 + bx + c = 0.0
    !                 where x = FeS_eq
    loc_roots(:) = fun_quad_root(1.0,-(dum_Fe2 + dum_H2S + dum_par_bio_FeS_abioticohm_cte),dum_Fe2*dum_H2S)
    ! -------------------------------------------------------- ! filter returned roots
    if (maxval(loc_roots(:)) < const_real_nullsmall) then
       IF (ctrl_debug_reportwarnings) THEN
          CALL sub_report_error( &
               & 'biogem_box.f90','sub_calc_spec_Fe2', &
               & 'No REAL root in Fe2 speciation calculation (or maybe zero ...).'// &
               & ' / Data: loc_Fe2(OLD),loc_H2S(OLD),loc_FeS(OLD),dum_Fe2,dum_H2S,', &
               & 'SOD THIS FOR A GAME OF SOLDIERS: calculation abondoned ...', &
               & (/loc_Fe2,loc_H2S,loc_FeS,dum_Fe2,dum_H2S/),.false. &
               & )
          error_stop = .FALSE.
       end IF
    elseif ((minval(loc_roots(:)) > dum_Fe2) .AND. (minval(loc_roots(:)) > dum_H2S)) then
       IF (ctrl_debug_reportwarnings) THEN
          CALL sub_report_error( &
               & 'biogem_box.f90','sub_calc_spec_Fe2', &
               & 'No solution to Fe2 speciation calculation possible ... :('// &
               & ' / Data: loc_Fe2(OLD),loc_H2S(OLD),loc_FeS(OLD),dum_Fe2,dum_H2S,', &
               & 'SOD THIS FOR A GAME OF SOLDIERS: calculation abondoned ...', &
               & (/loc_Fe2,loc_H2S,loc_FeS,dum_Fe2,dum_H2S/),.false. &
               & )
          error_stop = .FALSE.
       end IF
    else
       if (minval(loc_roots(:)) < const_real_nullsmall) then
          loc_FeS = maxval(loc_roots(:))
       else
          loc_FeS = minval(loc_roots(:))
       end if
       if (loc_FeS > MIN(dum_Fe2,dum_H2S)) then
           loc_FeS = MIN(dum_Fe2,dum_H2S)
       end if
       loc_Fe2  = dum_Fe2 - loc_FeS
       loc_H2S  = dum_H2S - loc_FeS
    end if
    ! -------------------------------------------------------- !
    ! RETURN RESULT
    ! -------------------------------------------------------- !
    fun_box_calc_spec_Fe2(1) = loc_Fe2 
    fun_box_calc_spec_Fe2(2) = loc_H2S
    fun_box_calc_spec_Fe2(3) = loc_FeS
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end function fun_box_calc_spec_Fe2
  !****************************************************************************************************************************** !


END MODULE gem_geochem

