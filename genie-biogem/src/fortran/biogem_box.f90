! *************************************************************************************************
! biogem_box.f90
! C-GOLDSTEIn/BioGeM
! MISCELLANEOUS MECHANICS OF THE SYSTEM
! *************************************************************************************************


MODULE biogem_box


  use gem_carbchem
  use gem_geochem
  USE biogem_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! OCEAN-ATMOSPHERE EXCHANGE
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SOLUBILITY COEFFICIENT
  subroutine sub_calc_solconst(dum_i,dum_j)
    ! dummy arguments
    integer,INTENT(in)::dum_i,dum_j
    ! local variables
    integer::l,ia
    ! calculate Solubility Coefficients (mol/(kg atm))
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       IF (atm_type(ia) == 1) then
          ocnatm_airsea_solconst(ia,dum_i,dum_j) = &
               & fun_calc_solconst(ia,ocn(io_T,dum_i,dum_j,n_k),ocn(io_S,dum_i,dum_j,n_k),phys_ocn(ipo_rho,dum_i,dum_j,n_k))
       end if
    end do
  end subroutine sub_calc_solconst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE WIND SPEED FROM WIND STRESS
  function fun_calc_u()
    ! result variable
    REAL,dimension(n_i,n_j)::fun_calc_u ! units of (m s-1)
    ! local variables
    integer::i,j
    real::tv,tv2,tv3
    ! calculate wind speed from wind stress - see: gseta.F:
    ! usurf(i,j) = sqrt((sqrt(tv**2 + tv2**2))*rh0sc*dsc*usc*fsc/(rhoair*cd*scf))
    fun_calc_u(:,:) = 0.0
    do j=1,n_j
       tv3 = 0.0
       do i=1,n_i
          if (phys_ocnatm(ipoa_mask_ocn,i,j) > const_real_nullsmall) then
             tv = phys_ocnatm(ipoa_tau_u,i,j)
          elseif (i == 1) then
             tv = (phys_ocnatm(ipoa_tau_u,i,j) + phys_ocnatm(ipoa_tau_u,n_i,j))/2.0
          else
             tv = (phys_ocnatm(ipoa_tau_u,i,j) + phys_ocnatm(ipoa_tau_u,i - 1,j))/2.0
          endif
          if (j == 1) then
             tv2 = phys_ocnatm(ipoa_tau_v,i,j)/2.0
          else
             tv2 = (phys_ocnatm(ipoa_tau_v,i,j) + phys_ocnatm(ipoa_tau_v,i,j - 1))/2.0
          endif
          fun_calc_u(i,j) = sqrt((sqrt(tv**2 + tv2**2))/(goldstein_rhoair*goldstein_cd))
          tv3 = tv3 + fun_calc_u(i,j)
       enddo
       do i=1,n_i
          if ((j < 2) .OR. (j > (n_j - 1))) fun_calc_u(i,j) = tv3/real(n_i)
       enddo
    enddo
  END function fun_calc_u
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE PISTON VELOCITY
  SUBROUTINE sub_calc_pv(dum_i,dum_j)
    ! dummy arguments
    INTEGER::dum_i,dum_j
    ! local variables
    integer::l,ia
    REAL::loc_Sc
    REAL::loc_TC,loc_TC2,loc_TC3
    real::loc_u2
    ! set local variables
    ! temperature powers
    ! NOTE: temeprature must be converted to the correct units (degrees C)
    ! NOTE: original valid temperature range was 0 - 30 C for the Schmidt number empirical fit - see: Wanninkhof et al. [1992]
    if (ocn(io_T,dum_i,dum_j,n_k) <  (const_zeroC +  par_geochem_Tmin))  then
       loc_TC = par_geochem_Tmin
    elseif (ocn(io_T,dum_i,dum_j,n_k) > (const_zeroC + par_geochem_Tmax)) then
       loc_TC = par_geochem_Tmax
    else
       loc_TC = ocn(io_T,dum_i,dum_j,n_k) - const_zeroC
    endif
    loc_TC2 = loc_TC*loc_TC
    loc_TC3 = loc_TC2*loc_TC
    ! wind speed^2
    loc_u2 = phys_ocnatm(ipoa_wspeed,dum_i,dum_j)**2
    !  calculate piston velocity
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       IF (atm_type(ia) == 1) then
          ! calculate gas transfer Schmidt number
          loc_Sc =                           &
               & par_Sc_coef(1,ia)         - &
               & par_Sc_coef(2,ia)*loc_TC  + &
               & par_Sc_coef(3,ia)*loc_TC2 - &
               & par_Sc_coef(4,ia)*loc_TC3
          ! calculate CO2 gas transfer velocity (piston velocity)
          ! NOTE: from Wanninkhof [1992] equation 1/3
          ! NOTE: convert from units of (cm hr-1) to (m yr-1)
          ! NOTE: pre-calculate 1.0/660 (= 1.515E-3)
          ocnatm_airsea_pv(ia,dum_i,dum_j) = conv_cm_m*conv_yr_hr*par_gastransfer_a*loc_u2*(loc_Sc*1.515E-3)**(-0.5)
       end if
    end do
  END SUBROUTINE sub_calc_pv
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE AIR-SEA GAS EXCHANGE
  FUNCTION fun_calc_ocnatm_flux(dum_i,dum_j,dum_atm,dum_dt)
    ! result variable
    REAL,dimension(n_atm)::fun_calc_ocnatm_flux ! units of (mol yr-1)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    REAL,dimension(n_atm),INTENT(in)::dum_atm
    REAL,INTENT(in)::dum_dt
    ! local variables
    integer::l,ia,io
    REAL,dimension(n_atm)::loc_focnatm,loc_fatmocn
    real::loc_alpha_k,loc_alpha_alpha
    real::loc_alpha_sa,loc_alpha_as
    real::loc_rho
    real::loc_TC
    real::loc_r13C_ocn,loc_r14C_ocn,loc_r18O_ocn,loc_r34S_ocn
    real::loc_r13C_atm,loc_r14C_atm,loc_r18O_atm,loc_r34S_atm
    real::loc_R_atm,loc_R_ocn
    real::loc_ocn,loc_atm
    real::loc_A
    real::loc_r_dflux_deqm
    real::loc_buff
    REAL,dimension(n_atm)::loc_dflux
    REAL,dimension(n_ocn)::loc_deqm

    ! *** INITIALIZE VARIABLES ***
    !
    loc_alpha_as = 0.0
    !
    loc_focnatm(:) = 0.0
    loc_fatmocn(:) = 0.0
    loc_rho = phys_ocn(ipo_rho,dum_i,dum_j,n_k)
    loc_TC = ocn(io_T,dum_i,dum_j,n_k) - const_zeroC
    ! area available for air-sea gas transfer
    loc_A = (1.0 - phys_ocnatm(ipoa_seaice,dum_i,dum_j))*phys_ocnatm(ipoa_A,dum_i,dum_j)

    ! *** calculate air-sea gas exchange fluxes ***
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       if (.NOT. ocnatm_airsea_eqm(ia)) then
          ! set corresponding ocean tracer
          ! NOTE: assume that there is a one-to-one mapping from atm tracers to ocn tracers,
          !       with the corresponding ocean tracer index given in the i=1 index position of the conv_atm_ocn_i array
          io = conv_atm_ocn_i(1,ia)
          SELECT CASE (atm_type(ia))
          CASE (0)
             ! [do nothing]
          CASE (1)
             ! calculate bulk gas exchange
             ! set local ocean and atmosphere tracer variables
             ! NOTE: check for special case of CO2
             !       -> there are 3 dissolved components associated with CO2(g) (CO2, HCO3, CO3)
             !          but only CO2(aq) is relevant to air-sea gas exchange)
             !       -> also, don't restrict air-sea gas exchange on the basis of estimated equilibrium with CO2(aq)
             !          because the buffering provided by eqm with HCO3 and CO3 is not taken into account
             !          => set a relative buffering factor for CO2,
             !             chosen to ensure numerical stability yet not unduely restrict air-sea CO2 exchange
             ! NOTE: local atmospheric tracer value has Bunsen Solubility Coefficient implicit in its value
             loc_atm = ocnatm_airsea_solconst(ia,dum_i,dum_j)*dum_atm(ia)
             if (io == io_DIC) then
                loc_ocn = carb(ic_conc_CO2,dum_i,dum_j,n_k)
                ! calculate limitation of air-sea exchange of CO2 based on Revelle factor (see: Zeebe and Wolf-Gladwor [2001])
                ! NOTE: RF0 = d[CO2]/[CO2] / dDIC/DIC
                !       => d[CO2]/dDIC = RF0 * [CO2]/DIC
                !          => loc_buff = 1.0 / (RF0 * [CO2]/DIC)
                ! NOTE: the new factor is not infinitely different from the previous code of ([CO2] + [CO3]) / [CO2]
                If (carb(ic_RF0,dum_i,dum_j,n_k) > const_real_nullsmall) then
                   loc_buff = 1.0/(carb(ic_RF0,dum_i,dum_j,n_k)*carb(ic_conc_CO2,dum_i,dum_j,n_k)/ocn(io_DIC,dum_i,dum_j,n_k))
                else
                   loc_ocn  = 0.0
                   loc_atm  = 0.0
                   loc_buff = 1.0
                endif
             else
                loc_ocn = ocn(io,dum_i,dum_j,n_k)
                loc_buff = 1.0
             end if
             ! make sure nothing 'nasty' can happen if a tracer has a -ve concentration
             ! (if shouldn't really have in the first place, but, well ...)
             if (loc_ocn < const_real_nullsmall) loc_ocn = 0.0
             if (loc_atm < const_real_nullsmall) loc_atm = 0.0
             ! calculate gas exchange fluxes ocn->atm and atm->ocn
             ! NOTE: units of (mol yr-1)
             ! NOTE: the solubility coefficient must be converted to units of mol/(kg atm) from a Bunsen Solubility Coefficient
             loc_focnatm(ia) = ocnatm_airsea_pv(ia,dum_i,dum_j)*loc_A*loc_rho*loc_ocn
             loc_fatmocn(ia) = ocnatm_airsea_pv(ia,dum_i,dum_j)*loc_A*loc_rho*loc_atm
             ! check for 'excessive' gas transfer (i.e., with the potential to lead to numerical instability)
             ! => rescale the fluxes to that the ocean surface is brought exactly into equilibrium
             ! NOTE: in the case of DIC, only CO2(aq) is considered
             ! NOTE: no account is taken of molar changes due to ocean circulation and biological activity in making this check
             ! calculate the molar magnitude of ocean deficit or surfit w.r.t. the atmosphere
             loc_deqm(io) = phys_ocn(ipo_dD,dum_i,dum_j,n_k)*phys_ocnatm(ipoa_A,dum_i,dum_j)*loc_rho*loc_buff*abs(loc_atm - loc_ocn)
             ! calculate the molar transfer that would normally then be applied
             loc_dflux(ia) = dum_dt*abs(loc_focnatm(ia) - loc_fatmocn(ia))
             ! ensure that molar transfer does not exceed the current disequilibrium
             ! (i.e., ensure that a +ve disequilibrium is not turned into a larger -ve disequilibrium at the next time-step)
             If (loc_deqm(io) > const_real_nullsmall) then
                loc_r_dflux_deqm = loc_dflux(ia)/loc_deqm(io)
                if (loc_r_dflux_deqm > par_airsea_r_dflux_deqm_max) then
                   loc_focnatm(ia) = (par_airsea_r_dflux_deqm_max/loc_r_dflux_deqm)*loc_focnatm(ia)
                   loc_fatmocn(ia) = (par_airsea_r_dflux_deqm_max/loc_r_dflux_deqm)*loc_fatmocn(ia)
                   IF (ctrl_debug_reportwarnings) then
                      print*,'WARNING: excessive air-sea flux of ',trim(string_atm(ia)), &
                           & ' prevented at (',fun_conv_num_char_n(2,dum_i),',',fun_conv_num_char_n(2,dum_j),')'
                   end IF
                end if
             end If
          case default
             ! calculate bulk gas exchange
             ! set local ocean and atmosphere tracer variables
             ! NOTE: check for special case of CO2
             !       -> there are 3 dissolved components associated with CO2(g) (CO2, HCO3, CO3)
             !          but only CO2(aq) is relevant to air-sea gas exchange)
             !       -> also, don't restrict air-sea gas exchange on the basis of estimated equilibrium with CO2(aq)

             ! calculate derived isotopic exchange
             ! NOTE: assume that associated bulk tracer flux has already been calculated (i.e., earlier during this routine)
             ! NOTE: convert r (13C/(13C + 12C)) ratio to R (13C/12C) before applying fractionation factor alpha
             ! NOTE: assume that the total mass of C is approximately equal to 12C + 13C
             ! NOTE: for 14C, the standard is already in the form: 14C/C
             SELECT CASE (ia)
             CASE (ia_pCO2_13C)
                ! isotopic fluxes - 13C
                loc_r13C_atm = dum_atm(ia_pCO2_13C)/dum_atm(ia_pCO2)
                loc_r13C_ocn = carbisor(ici_CO2_r13C,dum_i,dum_j,n_k)
                loc_R_atm = loc_r13C_atm/(1.0 - loc_r13C_atm)
                loc_R_ocn = loc_r13C_ocn/(1.0 - loc_r13C_ocn)
                ! overall 13C fractionationscheme following Marchal et al. [1998]
                ! NOTE: fractionation factors taken from Zhang et al. [1995]
                ! NOTE: some notation borrowed from Yamahaka and Tajika [1996]
                ! NOTE: in the sea->air fractionation, the fractionation between DIC and CO2(aq) is already taken into account
                !       in that the fractionation is applied to the d13C of CO2(aq) rather than bulk DIC
                ! kinetic fractionation
                loc_alpha_k = 0.99912
                ! air-sea equilibrium fractionation between aqueous CO2 and gaseous CO2
                loc_alpha_alpha = 0.99869 + 4.9E-6*loc_TC
                ! overall fractionation factors in both directions
                loc_alpha_as = loc_alpha_alpha*loc_alpha_k
                loc_alpha_sa = loc_alpha_k
                ! calculate fluxes
                loc_fatmocn(ia_pCO2_13C) = (loc_alpha_as*loc_R_atm/(1.0 + loc_alpha_as*loc_R_atm))*loc_fatmocn(ia_pCO2)
                loc_focnatm(ia_pCO2_13C) = (loc_alpha_sa*loc_R_ocn/(1.0 + loc_alpha_sa*loc_R_ocn))*loc_focnatm(ia_pCO2)
             CASE (ia_pCO2_14C)
                ! isotopic fluxes - 14C
                loc_r14C_atm = dum_atm(ia_pCO2_14C)/dum_atm(ia_pCO2)
                loc_r14C_ocn = carbisor(ici_CO2_r14C,dum_i,dum_j,n_k)
                loc_R_atm = loc_r14C_atm/(1.0 - loc_r14C_atm)
                loc_R_ocn = loc_r14C_ocn/(1.0 - loc_r14C_ocn)
                loc_fatmocn(ia_pCO2_14C) = (loc_alpha_as**2*loc_R_atm/(1.0 + loc_alpha_as**2*loc_R_atm))*loc_fatmocn(ia_pCO2)
                loc_focnatm(ia_pCO2_14C) = (loc_alpha_sa**2*loc_R_ocn/(1.0 + loc_alpha_sa**2*loc_R_ocn))*loc_focnatm(ia_pCO2)
             CASE (ia_pCH4_13C)
                if (dum_atm(ia_pCH4) > const_real_nullsmall) then
                   loc_r13C_atm = dum_atm(ia_pCH4_13C)/dum_atm(ia_pCH4)
                   loc_fatmocn(ia_pCH4_13C) = loc_r13C_atm*loc_fatmocn(ia_pCH4)
                end if
                if (ocn(io_CH4,dum_i,dum_j,n_k) > const_real_nullsmall) then
                   loc_r13C_ocn = ocn(io_CH4_13C,dum_i,dum_j,n_k)/ocn(io_CH4,dum_i,dum_j,n_k)
                   loc_focnatm(ia_pCH4_13C) = loc_r13C_ocn*loc_focnatm(ia_pCH4)
                end if
             CASE (ia_pCH4_14C)
                if (dum_atm(ia_pCH4) > const_real_nullsmall) then
                   loc_r14C_atm = dum_atm(ia_pCH4_14C)/dum_atm(ia_pCH4)
                   loc_fatmocn(ia_pCH4_14C) = loc_r14C_atm*loc_fatmocn(ia_pCH4)
                end if
                if (ocn(io_CH4,dum_i,dum_j,n_k) > const_real_nullsmall) then
                   loc_r14C_ocn = ocn(io_CH4_14C,dum_i,dum_j,n_k)/ocn(io_CH4,dum_i,dum_j,n_k)
                   loc_focnatm(ia_pCH4_14C) = loc_r14C_ocn*loc_focnatm(ia_pCH4)
                end if
             CASE (ia_pO2_18O)
                if (dum_atm(ia_pO2) > const_real_nullsmall) then
                   loc_r18O_atm = dum_atm(ia_pO2_18O)/dum_atm(ia_pO2)
                   loc_fatmocn(ia_pO2_18O) = loc_r18O_atm*loc_fatmocn(ia_pO2)
                end if
                if (ocn(io_O2,dum_i,dum_j,n_k) > const_real_nullsmall) then
                   loc_r18O_ocn = ocn(io_O2_18O,dum_i,dum_j,n_k)/ocn(io_O2,dum_i,dum_j,n_k)
                   loc_focnatm(ia_pO2_18O) = loc_r18O_ocn*loc_focnatm(ia_pO2)
                end if
             CASE (ia_pH2S_34S)
                if (dum_atm(ia_pH2S) > const_real_nullsmall) then
                   loc_r34S_atm = dum_atm(ia_pH2S_34S)/dum_atm(ia_pH2S)
                   loc_fatmocn(ia_pH2S_34S) = loc_r34S_atm*loc_fatmocn(ia_pH2S)
                end if
                if (ocn(io_H2S,dum_i,dum_j,n_k) > const_real_nullsmall) then
                   loc_r34S_ocn = ocn(io_H2S_34S,dum_i,dum_j,n_k)/ocn(io_H2S,dum_i,dum_j,n_k)
                   loc_focnatm(ia_pH2S_34S) = loc_r34S_ocn*loc_focnatm(ia_pH2S)
                end if
             case default
                ! ### INSERT CODE TO DEAL WITH ADDITIONAL ISOTOPES ############################################################### !
                !
                ! ################################################################################################################ !
             end SELECT
          end SELECT
          ! calculate net gas transfer and set results variable
          fun_calc_ocnatm_flux(ia) = loc_focnatm(ia) - loc_fatmocn(ia)
       end if
    end do

  END FUNCTION fun_calc_ocnatm_flux
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SURFACE OCEAN PROCESSES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE BIOLOGICAL PRODUCTIVITY
  SUBROUTINE sub_calc_bio(dum_i,dum_j,dum_k1,dum_dt)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    INTEGER,INTENT(in)::dum_k1
    real,intent(in)::dum_dt
    select case (par_bio_prodopt)
    case (                         &
         & '1N1T_PO4restore',      &
         & '1N1T_PO4restoreLL',    &
         & '1N1T_PO4MM',           &
         & '1N1T_PO4MM_Tdep',      &
         & '2N1T_PO4MM_SiO2',      &
         & '1N1T_PO4MM_Cd',        &
         & '2N2T_PO4MM_NO3',       &
         & '2N2T_PN_Tdep',         &
         & '3N2T_PNFe_Tdep',       &
         & 'Payal_Cd',             &
         & 'bio_P',                &
         & 'bio_PFe',              &
         & 'bio_PFe_OCMIP2',       &
         & 'bio_PFeSi',            &
         & 'bio_PFeSi_Ridgwell02', &
         & 'bio_POCflux'           &
         & )
       ! biologically induced (mass balance) schemes
       call sub_calc_bio_uptake(dum_i,dum_j,dum_k1,dum_dt)
    case default
       ! NOTHING
    end select
  end SUBROUTINE sub_calc_bio
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE BIOLOGICAL TRACER UPTAKE AT THE SURFACE OCEAN -- biologically induced (mass balance) schemes
  ! NOTE: assume complete homogenization over mixed layer
  !       => calculate uptake based on tracer concentrations in surface layer only
  SUBROUTINE sub_calc_bio_uptake(dum_i,dum_j,dum_k1,dum_dt)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    INTEGER,INTENT(in)::dum_k1
    real,intent(in)::dum_dt
    ! local variables
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real::loc_dPO4
    real::loc_dPO4_1,loc_dPO4_2
    real::loc_dPO4_sp,loc_dPO4_nsp
    real::loc_ohm,loc_co3                                               !
    real::loc_frac_N2fix
    real::loc_frac_Fe2
    real::loc_ficefree,loc_intI,loc_kI,loc_kT
    real,dimension(n_ocn,n_k)::loc_bio_uptake                           !
    real,dimension(n_sed,n_k)::loc_bio_part_DOM                         !
    real,dimension(n_sed,n_k)::loc_bio_part_RDOM                        !
    real::loc_delta_Corg,loc_delta_CaCO3
    real::loc_alpha,loc_delta,loc_standard
    real::loc_r13C,loc_r14C
    real::loc_r15N,loc_r30Si,loc_r114Cd,loc_r7Li,loc_r44Ca
    real::loc_86Sr,loc_87Sr,loc_88Sr
    real::loc_192Os,loc_188Os,loc_187Os
    real::loc_R,loc_r18O
    real::loc_PO4,loc_Cd,loc_FeT,loc_SiO2,loc_N,loc_IO3
    real::loc_FeT_56Fe,loc_r56Fe
    real::loc_kPO4,loc_kPO4_sp,loc_kPO4_nsp,loc_kN
    real::loc_kFe,loc_kFe_sp,loc_kFe_nsp
    real::loc_kSiO2,loc_kSiO2_sp,loc_kSiO2_nsp
    real::loc_kCd
    real::loc_TC
    real::loc_bio_red_DOMfrac,loc_bio_red_RDOMfrac,loc_bio_red_DOMtotal !
    real::loc_PPoverZeu,loc_rPOC
    real::loc_r_POM_DOM,loc_r_POM_RDOM                                  !
    real::loc_bio_red_POC_POFe_sp,loc_bio_red_POC_POFe_nsp
    real::loc_d13C_DIC_Corg_ef
    real::loc_bio_NP,loc_red
    real::loc_dN2,loc_dNH4,loc_dNO3
    integer::loc_k_mld
    real,dimension(n_ocn)::loc_ocn                             !

    ! *** INITIALIZE VARIABLES ***
    !
    loc_dPO4 = 0.0
    loc_delta_Corg = 0.0
    loc_delta_CaCO3 = 0.0
    !
    loc_kPO4 = 0.0
    loc_kPO4_sp = 0.0
    loc_kPO4_nsp = 0.0
    loc_kFe = 0.0
    loc_kFe_sp  = 0.0
    loc_kFe_nsp  = 0.0
    loc_kSiO2 = 0.0
    loc_kSiO2_sp = 0.0
    loc_kSiO2_nsp = 0.0
    loc_kI = 0.0
    loc_kT = 0.0
    !
    loc_Cd = 0.0
    loc_IO3 = 0.0
    loc_dPO4_1 = 0.0
    loc_dPO4_2 = 0.0
    loc_dPO4_sp = 0.0
    loc_dPO4_nsp = 0.0
    loc_frac_N2fix = 0.0
    loc_bio_uptake(:,:) = 0.0
    loc_bio_part_DOM(:,:) = 0.0
    loc_bio_part_RDOM(:,:) = 0.0
    loc_k_mld = dum_k1
    loc_bio_red_DOMfrac = 0.0
    loc_bio_red_RDOMfrac = 0.0
    loc_d13C_DIC_Corg_ef = 0.0
    loc_bio_red_DOMtotal = 0.0
    !
    loc_ocn = 0.0

    ! *** APPLY VARIABLE P:C RATIO ? ***
    ! par_bio_red_PC_flex = 1 --> activate variable stoichiometry
    ! par_bio_red_PC_flex = 2 --> activate variable stoichiometry with limit at high PO4
    ! par_bio_red_PC_flex = 0 --> default fixed Redfield stoichiometry
    if (par_bio_red_PC_flex > 0) then
       ! default par_bio_red_PC_alpha1 = 1.0
       ! default par_bio_red_PC_alpha2 = 1.0
       ! to achieve average P:C more similar to fixed Redfield modern run, use  PC_alpha1 = 1.1643
       ! an alternative is to scale only PC_alpha2 = 1.16
       bio_part_red(is_POC,is_POP,dum_i,dum_j) = &
            & par_bio_red_PC_alpha1 * (6.9e-3 * ocn(io_PO4,dum_i,dum_j,n_k)*1.0e6 + par_bio_red_PC_alpha2*6.0e-3)
       if (par_bio_red_PC_flex == 2) then   ! limit C:P at high PO4 (because no data for PO4 > 1.7 uM in Galbraith & Martiny, 2015)
          if (bio_part_red(is_POC,is_POP,dum_i,dum_j) > par_bio_red_PC_max) then
             bio_part_red(is_POC,is_POP,dum_i,dum_j) = par_bio_red_PC_max
          end if
       end if
       bio_part_red(is_POP,is_POC,dum_i,dum_j) = 1.0/bio_part_red(is_POC,is_POP,dum_i,dum_j)
    elseif (par_bio_red_PC_flex == -1) then
       bio_part_red(is_POC,is_POP,:,:) = par_bio_red_PC_max
       bio_part_red(is_POP,is_POC,:,:) = 1.0/bio_part_red(is_POC,is_POP,dum_i,dum_j)
    else
       bio_part_red(is_POP,is_POC,:,:) = par_bio_red_POP_POC
       bio_part_red(is_POC,is_POP,:,:) = 1.0/bio_part_red(is_POP,is_POC,dum_i,dum_j)
    end if
    ! set local N:P
    loc_bio_NP = bio_part_red(is_POC,is_PON,dum_i,dum_j)*bio_part_red(is_POP,is_POC,dum_i,dum_j)

    ! *** CALCULATE MIXED LAYER PROPERTIES ***
    ! NOTE: MLD is stored as a POSITIVE depth below the surface
    ! ### temp code ############################################################################################################## !
    !DO k=n_k,1,-1
    !   If (phys_ocn(ipo_Dbot,dum_i,dum_j,k) >= phys_ocnatm(ipoa_mld,dum_i,dum_j)) then
    !      loc_k_mld = k
    !      exit
    !   end If
    !end DO
    ! tmp fix: set MLD to zero
    loc_k_mld = n_k
    ! ### temp code ############################################################################################################## !

    ! *** CALCULATE LOCAL NUTRIENT CONCENTRATIONS & LIMITATIONS ***
    !
    loc_ocn(:) = ocn(:,dum_i,dum_j,n_k)
    !
    if (ocn_select(io_PO4)) then
       loc_PO4 = ocn(io_PO4,dum_i,dum_j,n_k)
       select case (par_bio_prodopt)
       CASE (             &
            & 'bio_PFeSi' &
            & )
          loc_kPO4_sp  = loc_PO4/(loc_PO4 + par_bio_c0_PO4_sp)
          loc_kPO4_nsp = loc_PO4/(loc_PO4 + par_bio_c0_PO4_nsp)
          diag_bio(idiag_bio_kPO4_sp,dum_i,dum_j)  = dum_dt*loc_kPO4_sp
          diag_bio(idiag_bio_kPO4_nsp,dum_i,dum_j) = dum_dt*loc_kPO4_nsp
       case default
          loc_kPO4 = loc_PO4/(loc_PO4 + par_bio_c0_PO4)
          diag_bio(idiag_bio_kPO4,dum_i,dum_j) = dum_dt*loc_kPO4
       end select
    else
       loc_PO4 = 0.0
    end if
    if (ocn_select(io_Fe) .OR. ocn_select(io_TDFe)) then
       SELECT CASE (trim(opt_geochem_Fe))
       CASE ('OLD','ALT')
          ! NOTE: combine free and ligand-bound tracers for total 'bioavailable' iron
          loc_FeT = ocn(io_Fe,dum_i,dum_j,n_k) + ocn(io_FeL,dum_i,dum_j,n_k)
          if (sed_select(is_POFe_56Fe)) loc_FeT_56Fe = ocn(io_Fe_56Fe,dum_i,dum_j,n_k) + ocn(io_FeL_56Fe,dum_i,dum_j,n_k)
       CASE ('FeFe2TL')
          ! combine Fe2 and Fe3 (which implicitly includes ligand-bound iron) tracers for total 'bioavailable' iron
          ! deal with working out what proportions of what are taken up, later ...
          loc_FeT = ocn(io_Fe,dum_i,dum_j,n_k) + ocn(io_Fe2,dum_i,dum_j,n_k)
          if (sed_select(is_POFe_56Fe)) loc_FeT_56Fe = ocn(io_Fe_56Fe,dum_i,dum_j,n_k) + ocn(io_Fe2_56Fe,dum_i,dum_j,n_k)
!!$          ! leave as just Fe (3) representing total 'bioavailable' iron
!!$          ! (otherwise there are redox complicatons of varying proportions of Fe3 vs. Fe2 being taken up, and later, remin-ed)
!!$          loc_FeT = ocn(io_Fe,dum_i,dum_j,n_k)
!!$          if (sed_select(is_POFe_56Fe)) loc_FeT_56Fe = ocn(io_Fe_56Fe,dum_i,dum_j,n_k)
       CASE ('hybrid')
          ! NOTE: no need to sum separate tracers, as io_TDFe *is* total dissolved and assumed bioavailable Fe pool
          loc_FeT = ocn(io_TDFe,dum_i,dum_j,n_k)
          if (sed_select(is_POFe_56Fe)) loc_FeT_56Fe = ocn(io_TDFe_56Fe,dum_i,dum_j,n_k)
       CASE ('lookup_4D')
          ! NOTE: function takes inputs: temperature, [H+], TDFe, TL as a single array
          loc_FeT = fun_box_calc_lookup_Fe_4D_Fe3(                           &
               & (/ ocn(io_T,dum_i,dum_j,n_k), carb(ic_H,dum_i,dum_j,n_k),   &
               & ocn(io_TDFe,dum_i,dum_j,n_k), ocn(io_TL,dum_i,dum_j,n_k) /) &
               & )
          if (sed_select(is_POFe_56Fe) .AND. ocn(io_TDFe,dum_i,dum_j,n_k)>const_rns) then
             loc_FeT_56Fe = ocn(io_TDFe_56Fe,dum_i,dum_j,n_k)*(loc_FeT/ocn(io_TDFe,dum_i,dum_j,n_k))
          else
             loc_FeT_56Fe = 0.0
          end if
       case default
          ! NOTHING!
          loc_FeT      = 0.0
          loc_FeT_56Fe = 0.0
       end SELECT
       select case (par_bio_prodopt)
       CASE (                        &
            & 'bio_PFeSi',           &
            & 'bio_PFeSi_Ridgwell02' &
            & )
          loc_kFe_sp  = loc_FeT/(loc_FeT + par_bio_c0_Fe_sp)
          loc_kFe_nsp = loc_FeT/(loc_FeT + par_bio_c0_Fe_nsp)
          diag_bio(idiag_bio_kFe_sp,dum_i,dum_j)  = dum_dt*loc_kFe_sp
          diag_bio(idiag_bio_kFe_nsp,dum_i,dum_j) = dum_dt*loc_kFe_nsp
       case default
          loc_kFe = loc_FeT/(loc_FeT + par_bio_c0_Fe)
          diag_bio(idiag_bio_kFe,dum_i,dum_j) = dum_dt*loc_kFe
       end select
    else
       loc_FeT = 0.0
    end if
    if (sed_select(is_CaCO3)) then
       loc_ohm = carb(ic_ohm_cal,dum_i,dum_j,n_k)
       loc_co3 = carb(ic_conc_CO3,dum_i,dum_j,n_k)
    end if
    if (ocn_select(io_Cd)) then
       loc_Cd = ocn(io_Cd,dum_i,dum_j,n_k)
       loc_kCd = loc_Cd/(loc_Cd + par_bio_c0_Cd)
    end if
    if (ocn_select(io_SiO2)) then
       loc_SiO2 = ocn(io_SiO2,dum_i,dum_j,n_k)
       select case (par_bio_prodopt)
       CASE (                        &
            & 'bio_PFeSi',           &
            & 'bio_PFeSi_Ridgwell02' &
            & )
          loc_kSiO2_sp  = loc_SiO2/(loc_SiO2 + par_bio_c0_SiO2_sp)
          loc_kSiO2_nsp = 0.0
          diag_bio(idiag_bio_kSiO2_sp,dum_i,dum_j) = dum_dt*loc_kSiO2_sp
          diag_bio(idiag_bio_kSiO2_nsp,dum_i,dum_j) = dum_dt*loc_kSiO2_nsp
       case default
          loc_kSiO2 = loc_SiO2/(loc_SiO2 + par_bio_c0_SiO2)
          diag_bio(idiag_bio_kSiO2,dum_i,dum_j) = dum_dt*loc_kSiO2
       end select
    else
       loc_SiO2 = 0.0
    end if
    if (ocn_select(io_NO3) .and. ocn_select(io_NH4)) then
       loc_N = ocn(io_NO3,dum_i,dum_j,n_k) + ocn(io_NH4,dum_i,dum_j,n_k)
       loc_kN = loc_N/(loc_N + par_bio_c0_N)
    else
       loc_N = 0.0
       loc_kN = 0.0
    end if
    if (ocn_select(io_IO3)) then
       loc_IO3 = ocn(io_IO3,dum_i,dum_j,n_k)
    end if

    ! *** CALCULATE PRODUCTIVITY MODIFIERS ***
    ! fractional ice-free coverage
    loc_ficefree = (1.0 - phys_ocnatm(ipoa_seaice,dum_i,dum_j))
    ! insolation modifier
    ! ### EDIT ADD AND/OR EXTEND BIOLOGICAL OPTIONS ############################################################################## !
    select case (par_bio_prodopt)
    case (                      &
         & '1N1T_PO4restoreLL', &
         & '1N1T_PO4MM',        &
         & '1N1T_PO4MM_Tdep',   &
         & '2N1T_PO4MM_SiO2',   &
         & '1N1T_PO4MM_Cd',     &
         & '2N2T_PO4MM_NO3',    &
         & '2N2T_PN_Tdep',      &
         & '3N2T_PNFe_Tdep'     &
         & )
       loc_kI = phys_ocnatm(ipoa_solfor,dum_i,dum_j)/phys_solar_constant
    case (                        &
         & 'Payal_Cd',            &
         & 'bio_P',               &
         & 'bio_PFe',             &
         & 'bio_PFe_OCMIP2',      &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       ! calculate integrated insolation over depth of entire mixed layer
       ! => assume e-folding depth of 20 m (set in <par_bio_eI>) [Doney et al., 2006] (i.e., as in OCMIP-2 definition)
       ! I(d) = I(0)*exp(-d/L)
       ! => integrating from d=0 to d=D:
       !    I(d)int = I(0)*(-L)*exp(-D/L) - I(0)*(-L)*exp(-0/L)
       !            = I(0)*L*(exp(-0/L) - exp(-D/L))
       !            = I(0)*L*(1.0 - exp(-D/L))
       !    I(d)ave = I(0)*L*(1.0 - exp(-D/L))/D
       ! NOTE: slight deviation from OCMIP-2, as much as it is possible to understand the text in Doney et al. [2006] ... ;)
       ! NOTE: assumes that the uppermost cell depth in GENIE is approximately equal to z(crit) (production zone depth)
       !       => the cell can either be wholly or partly within the mixed layer; cannot be wholly below
       ! ### temp code ########################################################################################################### !
       !If (phys_ocn(ipo_Dbot,dum_i,dum_j,n_k) >= phys_ocnatm(ipoa_mld,dum_i,dum_j)) then
       !   ! ml entirely within uppermost (surface) cell
       !   loc_intI = phys_ocnatm(ipoa_fxsw,dum_i,dum_j)*par_bio_I_eL* &
       !        & (1.0 - exp(-phys_ocn(ipo_Dbot,dum_i,dum_j,n_k)/par_bio_I_eL))/phys_ocn(ipo_Dbot,dum_i,dum_j,n_k)
       !else
       !   ! ml deeper than uppermost (surface) cell
       !   loc_intI = phys_ocnatm(ipoa_fxsw,dum_i,dum_j)*par_bio_I_eL* &
       !        & (1.0 - exp(-phys_ocnatm(ipoa_mld,dum_i,dum_j)/par_bio_I_eL))/phys_ocnatm(ipoa_mld,dum_i,dum_j)
       !end If
       ! assume zero MLD
       loc_intI = phys_ocnatm(ipoa_fxsw,dum_i,dum_j)*par_bio_I_eL* &
            & (1.0 - exp(-phys_ocn(ipo_Dbot,dum_i,dum_j,n_k)/par_bio_I_eL))/phys_ocn(ipo_Dbot,dum_i,dum_j,n_k)
       ! ### temp code ########################################################################################################### !
       loc_kI = loc_intI/(loc_intI + par_bio_c0_I)
    case default
       loc_kI = 0.0
    end select
    diag_bio(idiag_bio_kI,dum_i,dum_j) = dum_dt*loc_kI
    ! ############################################################################################################################ !
    ! temperature
    loc_TC = ocn(io_T,dum_i,dum_j,n_k) - const_zeroC
    SELECT CASE (par_bio_prodopt)
    case (                          &
         & 'bio_P',                 &
         & 'bio_PFe',               &
         & 'bio_PFeSi',             &
         & 'bio_PFeSi_Ridgwell02',  &
         & '1N1T_PO4MM_Tdep',       &
         & '2N2T_PN_Tdep',          &
         & '3N2T_PNFe_Tdep'         &
         & )
       loc_kT = par_bio_kT0*exp((loc_TC+par_bio_kT_dT)/par_bio_kT_eT)
    case (           &
         & 'bio_PFe_OCMIP2' &
         & )
       loc_kT = (loc_TC + 2.0)/(loc_TC + 10.0)
    case default
       loc_kT = 0.0
    end SELECT
    diag_bio(idiag_bio_kT,dum_i,dum_j) = dum_dt*loc_kT

    ! *** CALCULATE PO4 DEPLETION ***
    ! NOTE: production is calculated as the concentration of newly-formed particulate material in the surface ocean layer
    !       that occurs within any single time step
    !       i.e., loc_dPO4 is in units of (mol kg-1)
    ! ### EDIT ADD AND/OR EXTEND BIOLOGICAL OPTIONS ############################################################################## !
    SELECT CASE (par_bio_prodopt)
    CASE ('NONE')
       ! 'nought going on ('abiological')
       loc_dPO4 = 0.0
    CASE ( &
         & '1N1T_PO4restore' &
         & )
       ! 1 x nutrient, 1 x 'taxa': PO4 restoring
       ! NOTE: filter for positive productivity; indicated by a negative value of <dum_docn_restore>
       !       (i.e., predicted model nutrient concentrations are higher than the restoring target)
       if (force_restore_docn_nuts(io_PO4) < -const_real_nullsmall) then
          loc_dPO4 = &
               loc_ficefree*(-force_restore_docn_nuts(io_PO4))
       else
          loc_dPO4 = 0.0
       end if
    CASE (                     &
         & '1N1T_PO4restoreLL' &
         & )
       ! 1 x nutrient, 1 x 'taxa': PO4 restoring + light limitation
       ! NOTE: filter for positive productivity; indicated by a negative value of <dum_docn_restore>
       !       (i.e., predicted model nutrient concentrations are higher than the restoring target)
       if (force_restore_docn_nuts(io_PO4) < -const_real_nullsmall) then
          loc_dPO4 = &
               & loc_ficefree* &
               & loc_kI* &
               & (-force_restore_docn_nuts(io_PO4))
       else
          loc_dPO4 = 0.0
       end if
    CASE (                    &
         & '1N1T_PO4MM',      &
         & '2N1T_PO4MM_SiO2', &
         & '1N1T_PO4MM_Cd'    &
         & )
       ! 1 x nutrient, 1 x 'taxa': PO4 Michaelis-Menton
       if (loc_PO4 > const_real_nullsmall) then
          loc_dPO4 = &
               & dum_dt* &
               & loc_ficefree* &
               & loc_kI* &
               & loc_kPO4* &
               & par_bio_k0_PO4
       else
          loc_dPO4 = 0.0
       end if
       if (ocn_select(io_NO3) .AND. ocn(io_NO3,dum_i,dum_j,n_k) < const_real_nullsmall) loc_dPO4 = 0.0
    CASE (                     &
         & '1N1T_PO4MM_Tdep'   &
         & )
       ! Same as '1N1T_PO4MM' with T limitation - Fanny (July 2011)
       if (loc_PO4 > const_real_nullsmall) then
          loc_dPO4 =                            &
               & dum_dt*                        &
               & loc_ficefree*                  &
               & loc_kI*                        &
               & loc_kT*                        &
               & loc_kPO4 *                     &
               & par_bio_mu1*loc_PO4
       else
          loc_dPO4 = 0.0
       end if
    CASE (            &
         & 'Payal_Cd' &
         & )
       ! Parekh et al. [2005] scheme
       if (loc_PO4 > const_real_nullsmall .AND. loc_FeT > const_real_nullsmall) then
          loc_dPO4 = &
               & dum_dt* &
               & loc_ficefree* &
               & loc_kI* &
               & min(loc_kPO4,loc_kFe)* &
               & par_bio_k0_PO4
       else
          loc_dPO4 = 0.0
       end if
    CASE (         &
         & 'bio_P' &
         & )
       ! structure of uptake parameterization after Doney et al. [2006]
       ! NOTE: the scaling for MLD > the compensation depth in Doney et al. [2006] is implicitly account for
       !       by the creation of organic matter throughout the MLD layersmax
       !      (the explicit equivalent would be to add the term: max(1.0,phys_ocnatm(ipoa_mld,dum_i,dum_j)/par_bio_zc)
       if (loc_PO4 > const_real_nullsmall) then
          loc_dPO4 =                                                                                                    &
               & dum_dt*                                                                                                &
               & loc_ficefree*                                                                                          &
               & loc_kT*                                                                                                &
               & loc_kPO4*                                                                                              &
               & loc_kI*                                                                                                &
               & loc_PO4/                                                                                               &
               & par_bio_tau
       else
          loc_dPO4 = 0.0
       end if
    CASE (                  &
         & 'bio_PFe',       &
         & 'bio_PFe_OCMIP2' &
         & )
       ! structure of uptake parameterization after Doney et al. [2006]
       ! NOTE: the scaling for MLD > the compensation depth in Doney et al. [2006] is implicitly account for
       !       by the creation of organic matter throughout the MLD layersmax
       !      (the explicit equivalent would be to add the term: max(1.0,phys_ocnatm(ipoa_mld,dum_i,dum_j)/par_bio_zc)
       if (loc_PO4 > const_real_nullsmall .AND. loc_FeT > const_real_nullsmall) then
          loc_dPO4 =                                                                                                    &
               & dum_dt*                                                                                                &
               & loc_ficefree*                                                                                          &
               & loc_kT*                                                                                                &
               & min(loc_kPO4,loc_kFe)*                                                                                 &
               & loc_kI*                                                                                                &
               & min(loc_PO4,bio_part_red(is_POC,is_POP,dum_i,dum_j)*loc_FeT/bio_part_red(is_POC,is_POFe,dum_i,dum_j))/ &
               & par_bio_tau
       else
          loc_dPO4 = 0.0
       end if
    CASE (             &
         & 'bio_PFeSi' &
         & )
       if (loc_PO4 > const_real_nullsmall .AND. loc_FeT > const_real_nullsmall .AND. loc_SiO2 > const_real_nullsmall) then
          loc_dPO4_sp =                                                                                                 &
               & dum_dt*                                                                                                &
               & loc_ficefree*                                                                                          &
               & loc_kT*                                                                                                &
               & min(loc_kPO4_sp,loc_kFe_sp,loc_kSiO2_sp)*                                                              &
               & loc_kI*                                                                                                &
               & min(loc_PO4,bio_part_red(is_POC,is_POP,dum_i,dum_j)*loc_FeT/bio_part_red(is_POC,is_POFe,dum_i,dum_j))/ &
               & par_bio_tau_sp
          loc_dPO4_nsp =                                                                                                &
               & dum_dt*                                                                                                &
               & loc_ficefree*                                                                                          &
               & loc_kT*                                                                                                &
               & min(loc_kPO4_nsp,loc_kFe_nsp)*                                                                         &
               & loc_kI*                                                                                                &
               & min(loc_PO4,bio_part_red(is_POC,is_POP,dum_i,dum_j)*loc_FeT/bio_part_red(is_POC,is_POFe,dum_i,dum_j))/ &
               & par_bio_tau_nsp
          loc_dPO4 = loc_dPO4_sp + loc_dPO4_nsp
       else
          loc_dPO4_sp  = 0.0
          loc_dPO4_nsp = 0.0
          loc_dPO4     = 0.0
       end if
    CASE ( &
         & '2N2T_PO4MM_NO3' &
         & )
       ! 2 x nutrient, 2 x 'taxa': PO4, NO3 Michaelis-Menton
       ! calculate PO4 depletion; loc_dPO4_1 is non-Nfixer productivity, loc_dPO4_2 is N-fixer productivity
       ! (after Fennel et al., 2005)
       ! NOTE: tidied up and also adjusted to allow N2 fixation when no fixed N of any sort exists
       ! NOTE: assume that dissolved N2 is never limiting
       if (loc_PO4 > const_real_nullsmall) then
          if (loc_N > const_real_nullsmall) then
             loc_dPO4_1 =                 &
                  & dum_dt*               &
                  & loc_ficefree*         &
                  & loc_kI*               &
                  & min(loc_kPO4,loc_kN)* &
                  & par_bio_mu1*          &
                  & min(loc_PO4,loc_N/par_bio_red_POP_PON)
          else
             loc_dPO4_1 = 0.0
          endif
          ! Need to add productivity from nitrogen fixation if conditions are right
          if (loc_N < par_bio_N2fixthresh .and. (loc_N/loc_PO4) < par_bio_red_POP_PON) then
             loc_dPO4_2 =         &
                  & dum_dt*       &
                  & loc_ficefree* &
                  & loc_kI*       &
                  & loc_kPO4*     &
                  & par_bio_mu2*  &
                  & loc_PO4
          else
             loc_dPO4_2 = 0.0
          endif
       else
          loc_dPO4_1 = 0.0
          loc_dPO4_2 = 0.0
       end if
       ! calculate total production (= PO4 uptate)
       loc_dPO4 = loc_dPO4_1 + loc_dPO4_2
       ! calculate fraction of total (phosphate based) production supported by N2 fixation
       if(loc_dPO4 > const_real_nullsmall) loc_frac_N2fix = loc_dPO4_2/loc_dPO4
    CASE ( &
         & 'bio_POCflux' &
         & )
       ! prescribed POC flux
       ! NOTE: force_restore_docn_nuts has been assigned a negative sign (in the forcing update of the main biogem subroutine) ...
       ! NOTE: allow depletion of PO4 < 0.0 so as to force export production to the prescribed value
       ! NOTE: correct for DOM (given that force_restore_docn_nuts has been devied from a prescribed particulate flux)
       ! NOTE: assume par_bio_red_DOMfrac for the DOM fraction 
       !       (loc_bio_red_DOMtotal has not been set to a non-zero value yet because the code that did it was moved back ...)
       loc_dPO4 = -force_restore_docn_nuts(io_PO4)/(1.0 - par_bio_red_DOMfrac)
    CASE ( &
         & '2N2T_PN_Tdep' &
         & )
       ! 2 x nutrient, 2 x 'taxa': PO4, DIN Michaelis-Menten - Fanny (July 2010)
       ! biomass=limiting nutrient, dynamical threshold and higher N:P ratio for nitrogen fixers
       ! loc_dPO4_1 is non-Nfixer productivity, loc_dPO4_2 is N-fixer productivity
       ! NOTE: as it stands: if there is no fixed nitrogen of any sort, there will be no N2 fixation either(!)
       if (loc_PO4 > const_real_nullsmall .and. loc_N > const_real_nullsmall) then
          loc_dPO4_1 =                          &
               & dum_dt*                        &
               & loc_ficefree*                  &
               & loc_kI*                        &
               & loc_kT*                        &
               & min(loc_kPO4,loc_kN)*          &
               & par_bio_mu1*                   &
               & min(loc_PO4,loc_N/par_bio_red_POP_PON)
          ! Dynamical N2 fixation threshold
          if ( loc_N/loc_PO4 < par_bio_red_POP_PON .and. &
               & (loc_N < par_bio_c0_N/(par_bio_N2fixdyn*(par_bio_mu1/par_bio_mu2*(1+par_bio_c0_PO4/loc_PO4)-1)) )) then
             loc_dPO4_2 =                       &
                  & dum_dt*                     &
                  & loc_ficefree*               &
                  & loc_kI*                     &
                  & loc_kT*                     &
                  & loc_kPO4*                   &
                  & par_bio_mu2*                &
                  & loc_PO4
          else
             loc_dPO4_2 = 0.0
          endif
       else
          loc_dPO4_1 = 0.0
          loc_dPO4_2 = 0.0
       end if
       ! calculate total production (= PO4 uptake)
       loc_dPO4 = loc_dPO4_1 + loc_dPO4_2
       ! calculate fraction of total production supported by N2 fixation
       ! NOTE: equivalent to:
       !       loc_frac_N2fix = par_bio_NPdiaz*loc_dPO4_2 / (par_bio_red_POP_PON*loc_dPO4_1 + par_bio_NPdiaz*loc_dPO4_2)
       if(loc_dPO4_2*par_bio_NPdiaz > const_real_nullsmall) then
          loc_frac_N2fix = 1.0/(1.0 + par_bio_red_POP_PON*loc_dPO4_1/(par_bio_NPdiaz*loc_dPO4_2))
       else
          loc_frac_N2fix = 0.0
       end if

    CASE ( &
         & '3N2T_PNFe_Tdep' &
         & )
       ! 3 x nutrient, 2 x 'taxa': PO4, DIN, Fe Michaelis-Menten - Fanny (July 2010)
       ! calculate PO4 depletion; loc_dPO4_1 is non-Nfixer productivity, loc_dPO4_2 is N-fixer productivity
       ! (similar to 2N2T_TPN with Fe limitation)
       ! NOTE: as it stands: if there is no fixed nitrogen of any sort, there will be no N2 fixation either(!)
       if (loc_PO4 > const_real_nullsmall .and. loc_N > const_real_nullsmall   &
            & .and. loc_FeT > const_real_nullsmall) then
          loc_dPO4_1 =                            &
               & dum_dt*                          &
               & loc_ficefree*                    &
               & loc_kI*                          &
               & loc_kT*                          &
               & min(loc_kPO4,loc_kN,loc_kFe) *   &
               & par_bio_mu1*                     &
               & min( &
               &    loc_PO4,loc_N/par_bio_red_POP_PON, &
               &    loc_FeT*bio_part_red(is_POC,is_POP,dum_i,dum_j)*bio_part_red(is_POFe,is_POC,dum_i,dum_j) &
               & )
          ! Need to add productivity from nitrogen fixation if conditions are right
          if (loc_N < par_bio_N2fixthresh .and. loc_N/loc_PO4 <  par_bio_red_POP_PON    &
               & .and. loc_N/loc_FeT < bio_part_red(is_POC,is_POP,dum_i,dum_j)*bio_part_red(is_POFe,is_POC,dum_i,dum_j)) then
             loc_dPO4_2 =                   &
                  & dum_dt*                 &
                  & loc_ficefree*           &
                  & loc_kI*                 &
                  & loc_kT*                 &
                  & min(loc_kPO4,loc_FeT/(loc_FeT+par_bio_c0_Fe_Diaz))*     &
                  & par_bio_mu2*            &
                  & min(loc_PO4,loc_FeT*par_bio_c0_PO4/par_bio_c0_Fe_Diaz)
          else
             loc_dPO4_2 = 0.0
          end if
       else
          loc_dPO4_1 = 0.0
          loc_dPO4_2 = 0.0
       end if
       ! calculate total production (= PO4 uptake)
       loc_dPO4 = loc_dPO4_1 + loc_dPO4_2
       ! calculate fraction of total production supported by N2 fixation
       ! NOTE: equivalent to:
       !       loc_frac_N2fix = par_bio_NPdiaz*loc_dPO4_2 / (par_bio_red_POP_PON*loc_dPO4_1 + par_bio_NPdiaz*loc_dPO4_2)
       if(loc_dPO4_2*par_bio_NPdiaz > const_real_nullsmall) then
          loc_frac_N2fix = 1.0/(1.0 + par_bio_red_POP_PON*loc_dPO4_1/(par_bio_NPdiaz*loc_dPO4_2))
       else
          loc_frac_N2fix = 0.0
       end if
    end select
    ! ############################################################################################################################ !

    ! *** SET DOM FRACTIONS ****************************************************************************************************** !
    select case (opt_bio_red_DOMfrac)
    case ('dunne')
       ! NOTE: from Dunne et al. [2005]
       !       rPOC = 0.419 + 0.0582 * ln(PP/Zeu) - 0.0101 * C :: for 0.04 < rPOC < 0.72
       !       where: temperature (T) is in units of degrees C
       !              vertically integrated primary production (PP) is in units of mmol C m-2 d-1
       !              the depth of the euphotic zone (Zeu) is in m
       !       Units conversion: mmol C m-2 d-1 / m == mmol C m-3 d-1
       !                         == 1.0E-3 * 1027.649 mol C kg-1 d-1
       !       BIOGEM calculates PP as mol kg-1 per time-step
       ! calculate carbon PP/Zeu from PO4 uptake (loc_dPO4), in required units
       ! NOTE: ignore differences in C:P of loc_dPO4_1, loc_dPO4_2 uptake fractions
       ! NOTE: this needs compelte re-turning becasue BIOGEM nutrient uptake does not correspond well to PP
       !       (and there may also be issues with use of a fixed mixed (surface) layer depth)
       loc_PPoverZeu = 1.0E+3*1027.649*bio_part_red(is_POP,is_POC,dum_i,dum_j)*loc_dPO4/(conv_yr_d*dum_dt)
       loc_rPOC  = 0.419 + 0.0582*log(loc_PPoverZeu) - 0.0101*loc_TC
       ! cap loc_rPOC range
       If (loc_rPOC < 0.04) loc_rPOC = 0.04
       If (loc_rPOC > 0.72) loc_rPOC = 0.72
       ! set DOMfrac
       ! NOTE: for now, ignore RDOM creation via primary production ...
       loc_bio_red_DOMfrac  = 1.0 - loc_rPOC
       loc_bio_red_RDOMfrac = 0.0
    case ('simple')
       ! NOTE: just the T-dependent part of Dunne et al. [2005]
       !       rPOC = 0.419 - 0.0101 * C :: for 0.04 < rPOC < 0.72
       loc_rPOC = par_bio_red_DOMfrac_Tdep_const - par_bio_red_DOMfrac_Tdep_gamma*loc_TC
       ! cap loc_rPOC range
       ! NOTE: simplified range
       If (loc_rPOC < 0.0) loc_rPOC = 0.0
       If (loc_rPOC > 1.0) loc_rPOC = 1.0
       ! set DOMfrac
       ! NOTE: for now, ignore RDOM creation via primary production ...
       loc_bio_red_DOMfrac  = 1.0 - loc_rPOC
       loc_bio_red_RDOMfrac = 0.0
    case ('DEFAULT')
       loc_bio_red_DOMfrac  = par_bio_red_DOMfrac
       loc_bio_red_RDOMfrac = par_bio_red_RDOMfrac
    case default
       CALL sub_report_error( &
            & 'biogem_box','sub_calc_bio_uptake', &
            & 'Unrecognised <opt_bio_red_DOMfrac> option: '//TRIM(opt_bio_red_DOMfrac)//'. ' // &
            & 'Valid options: dunne, simple, DEFAULT', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    end select

    ! *** ADJUST FOR TOTAL DOM + RDOM ******************************************************************************************** !
    ! check for total DOM fraction exceeding 1.0 and re-scale (proportionally and to sum to 1.0)
    loc_bio_red_DOMtotal = loc_bio_red_DOMfrac + loc_bio_red_RDOMfrac
    if (loc_bio_red_DOMtotal > 1.0) then
       loc_bio_red_DOMfrac = loc_bio_red_DOMfrac/loc_bio_red_DOMtotal
       loc_bio_red_RDOMfrac = 1.0 - loc_bio_red_DOMfrac
       loc_bio_red_DOMtotal = 1.0
    end if

    ! *** ADJUST PARTICULATE COMPOSITION 'REDFIELD' RATIOS *********************************************************************** !
    !
    ! CaCO3
    ! NOTE: a correction is made for the fact that a proportion of the POM is transformed into DOM,
    !      whereas the initially calculated CaCO3 and opal fluxes do not change
    !      => re-scale CaCO3 and opal ratios so that the prescribed export ratio value better reflects final export composition
    if (sed_select(is_CaCO3)) then
       select case (opt_bio_CaCO3toPOCrainratio)
       case ('prescribed')
          ! fixed, spatially explicit
          bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)*par_bio_CaCO3toPOCrainratio(dum_i,dum_j)
       case ('Heinze2004')
          ! Heinze [2004] saturation dependent parameterization
          ! NOTE: par_bio_red_POC_CaCO3_CO2aqREF in (umol kg-1)
          bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)* &
               & par_bio_red_POC_CaCO3*(1.0 - 0.012*(1.0E6*carb(ic_conc_CO2,dum_i,dum_j,n_k) - par_bio_red_POC_CaCO3_CO2aqREF))
          if (bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) < const_real_nullsmall) bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = 0.0
       case ('Gehlenetal2007')
          ! Gehlen et al. [2007] saturation dependent parameterization
          if (loc_ohm > 1.0) then
             bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)* &
                  & par_bio_red_POC_CaCO3*(loc_ohm - 1.0)/(par_bio_red_POC_CaCO3_Kmax + (loc_ohm - 1.0))
          else
             bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = 0.0
          end if
       case ('Ridgwelletal2007ab')
          ! Ridgwell et al. [2007a,b] saturation dependent parameterization
          if (loc_ohm > 1.0) then
             bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)* &
                  & par_bio_red_POC_CaCO3*(loc_ohm - 1.0)**par_bio_red_POC_CaCO3_pP
          else
             bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = 0.0
          end if
       case ('HofmannandSchellnhuber2009')
          ! Hofmann and Schellnhuber [2009] (Barker et al. [2003]) [CO32-] dependent parameterization
          bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)* &
               & par_bio_red_POC_CaCO3*exp(0.0083*(1.0E6*loc_co3 - par_bio_red_POC_CaCO3_CO3REF))
       case default
          ! fixed, uniform
          bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)*par_bio_red_POC_CaCO3
       end select
       ! adjust for ecosystem composition in proportion of PO4 update by NSP to total PO4 uptake (SP + NSP)
       ! NOTE: the CaCO3:POC ratio is implemented in such a way that the prescribed parameter value is in respect
       !       ONLY to nsp associated POC export (NOT total POC export)
       SELECT CASE (par_bio_prodopt)
       case (                        &
            & 'bio_PFeSi'            &
            & )
          if (loc_dPO4 > const_real_nullsmall) then
             bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (loc_dPO4_nsp/loc_dPO4)*bio_part_red(is_POC,is_CaCO3,dum_i,dum_j)
          end if
       end SELECT
    end if
    !
    ! OPAL
    ! If no H4SiO3 limitation is considered in the biological production, modify Si:C uptake according to H4SiO4 depletion
    ! (NOTE: this is just to ensure that SiO2 does not drop below zero - it does not consistute true nutrient limitation)
    ! For H4SiO4-limited biological production, adjust Si:C uptake for Fe replete conditions (Ridgwell et al. [2002], GBC)
    if (sed_select(is_opal)) then
       SELECT CASE (par_bio_prodopt)
       case (                        &
            & 'bio_PFeSi'            &
            & )
          if (ocn(io_SiO2,dum_i,dum_j,n_k) > const_real_nullsmall) then
             SELECT CASE (opt_bio_red_SitoC)
             case ('Ridgwell2001')
                ! NOTE: OLD == bio_part_red(is_POC,is_opal,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)*par_bio_red_POC_opal* &
                !                   & ((0.25E-9/(loc_FeT+0.125E-9))+1.0)
                bio_part_red(is_POC,is_opal,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)*par_bio_red_POC_opal* &
                     & ((par_part_red_opal_FeTKSp/(loc_FeT+par_part_red_opal_FeToff))+1.0)
             case ('Jones2018')
                bio_part_red(is_POC,is_opal,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)*par_bio_red_POC_opal* &
                     & (par_part_red_opal_FeTKSp + loc_FeT)/max(loc_FeT,par_part_red_opal_FeToff)
             case default
                bio_part_red(is_POC,is_opal,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)*par_bio_red_POC_opal
             end SELECT
          else
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = 0.0
          end if
          ! adjust for ecosystem composition in proportion to PO4 uptake by SP to total PO4 uptake (SP + NSP)
          if (loc_dPO4 > const_real_nullsmall) then
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = (loc_dPO4_sp/loc_dPO4)*bio_part_red(is_POC,is_opal,dum_i,dum_j)
          else
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = 0.0
          end if
       case default
          if (ocn(io_SiO2,dum_i,dum_j,n_k) > const_real_nullsmall) then
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = (1.0 - loc_bio_red_DOMtotal)*par_bio_red_POC_opal*loc_kSiO2
          else
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = 0.0
          end if
       end SELECT
    end if
    !
    ! TRACE METALS: Fe
    ! modify Fe:C cellular quotient according to Fe limitation
    ! NOTE: following Ridgwell [2001] (mean parameter values from diatom and coccolithophorid parameterizations)
    ! NOTE: default uniform Fe:C ratio has already been set during model initialization
    ! NOTE: for options 'bio_PFeSi*' this has no effect, as below two different ratios will be used for sicliceous
    ! and non-siliceous phytoplankton. Finally an appropriate mixture of these two ratios (according to the ratio
    ! of siliceous and non-siliceous phytoplankton produced) will be used to update the value computed here.
    ! KEY: par_bio_FetoC_pP == power in [FeT] dependent Fe:C ratio equation [Ridgwell, 2001] (-0.4225)
    !      par_bio_FetoC_K  == scaling in [FeT] dependent Fe:C ratio equation [Ridgwell, 2001] (103684.0)
    !      par_bio_FetoC_C  == constant in [FeT] dependent Fe:C ratio equation [Ridgwell, 2001] (0.0)
    if (sed_select(is_POFe)) then
       SELECT CASE (par_bio_prodopt)
       case (                        &
            & 'bio_PFeSi'            &
            & )
          ! deal with case where there are 2 distinct physotplaknton 'groups'
          ! => weight the respective Fe:C by the contribution to total export
          !    NOTE: the respective PO4 uptake (== export) rates are used as a proxy for carbon export (assuming uniform C:P)
          if (.NOT. ctrl_bio_red_fixedFetoC) then
             if (ctrl_bio_red_Ridgwell2001FetoC) then
                ! Ridgwell [2001]
                if (loc_FeT > par_part_red_FeTmin) then
                   loc_bio_red_POC_POFe_sp = 1.0/ &
                        & MIN(333000.0,15000.0 + 115623.0*(1.0E9*(loc_FeT - par_part_red_FeTmin))**(-0.65))
                   loc_bio_red_POC_POFe_nsp = 1.0/ &
                        & MIN(333000.0,20000.0 + 31805.0*(1.0E9*(loc_FeT - par_part_red_FeTmin))**(-0.65))
                   if (loc_dPO4 > const_real_nullsmall) then
                      bio_part_red(is_POC,is_POFe,dum_i,dum_j) = &
                           & (loc_dPO4_sp*loc_bio_red_POC_POFe_sp + loc_dPO4_nsp*loc_bio_red_POC_POFe_nsp)/loc_dPO4
                   else
                      ![default (fixed) Redfield ratio already set]
                   end if
                else
                   bio_part_red(is_POC,is_POFe,dum_i,dum_j) = 1.0/333000.0
                end if
             else
                ! unified scheme
                if (loc_FeT > par_part_red_FeTmin) then
                   bio_part_red(is_POC,is_POFe,dum_i,dum_j) = 1.0/ &
                        & MIN(par_part_red_FetoCmax,(par_bio_FetoC_C + par_bio_FetoC_K*(1.0E9*loc_FeT)**(par_bio_FetoC_pP)))
                else
                   bio_part_red(is_POC,is_POFe,dum_i,dum_j) = 1.0/par_part_red_FetoCmax
                end if
             end if
          else
             ![default (fixed) Redfield ratio already set]
          end if
       case default
          ! default case: single homogeneous plankton mass -- no complications! :)
          if (.NOT. ctrl_bio_red_fixedFetoC) then
             if (loc_FeT > par_part_red_FeTmin) then
                bio_part_red(is_POC,is_POFe,dum_i,dum_j) = 1.0/ &
                     & MIN(par_part_red_FetoCmax,(par_bio_FetoC_C + par_bio_FetoC_K*(1.0E9*loc_FeT)**(par_bio_FetoC_pP)))
             else
                bio_part_red(is_POC,is_POFe,dum_i,dum_j) = 1.0/par_part_red_FetoCmax
             end if
          else
             ![default (fixed) Redfield ratio already set]
          end if
       end SELECT
    end if
    !
    ! TRACE METALS: Cd (:POC)
    ! NOTE: multiple distinct plankton 'groups' are NOT currently accounted for
    if (ocn_select(io_Cd)) then
       if (loc_Cd > const_real_nullsmall) then
          if (ctrl_force_POCdtoPOCrainratio) then
             ! (1) take POCd:POC ratio from prescribed 2-D field
             bio_part_red(is_POC,is_POCd,dum_i,dum_j) = par_bio_POCdtoPOCrainratio(dum_i,dum_j)
          elseif (ctrl_bio_red_CdtoC_Felim) then
             ! (2) calculate POCd:POC ratio according to nutrient limitation
             !     assume enhanced Cd uptake when Fe nutrient limitation dominates other nutrient limitations
             !     .AND. there is Fe limitation in the first place
             !     NOTE: simply take nutrient 'limitation' as being defined by an ambient conc. less than the half-sat conc.
             bio_part_red(is_POC,is_POCd,dum_i,dum_j) = &
                  & par_bio_red_CdtoC_Felim_min + (1.0 - loc_kFe)*(par_bio_red_CdtoC_Felim_max - par_bio_red_CdtoC_Felim_min)
          else
             ! (3) default scheme
             !     NOTE: the default scheme has two components;
             !           par_bio_red_POC_POCd is a parameter for defining a fixed cellular C:Cd (Cd/C) ratio
             !           the remainder of the parameterization is the Elderfield and Rickaby [2000] partition coefficient model
             if (ctrl_force_Cd_alpha) par_bio_red_POC_POCd_alpha = par_bio_Cd_alpha(dum_i,dum_j)
             if (loc_PO4 > const_real_nullsmall) then
                bio_part_red(is_POC,is_POCd,dum_i,dum_j) = par_bio_red_POC_POCd + par_bio_red_POC_POCd_alpha* &
                     & loc_Cd/(bio_part_red(is_POP,is_POC,dum_i,dum_j)*loc_PO4)
             else
                bio_part_red(is_POC,is_POCd,dum_i,dum_j) = 0.0
             end if
          end if
       else
          bio_part_red(is_POC,is_POCd,dum_i,dum_j) = 0.0
       end if
    end if
    !
    ! TRACE ELEMENTS: I (:POC)
    if (ocn_select(io_IO3)) then
       bio_part_red(is_POC,is_POI,dum_i,dum_j) = par_bio_red_POC_POI
       !       ! NOTE: I:C scales with ambient [IO3-]
       !       bio_part_red(is_POC,is_POI,dum_i,dum_j) = &
       !            & par_bio_red_POC_POI*loc_IO3/par_bio_red_POC_POI_C0
    end if
    !
    ! TRACE METALS: Cd (:CaCO3)
    if (ocn_select(io_Cd) .AND. ocn_select(io_Ca)) then
       bio_part_red(is_CaCO3,is_CdCO3,dum_i,dum_j) = par_bio_red_CaCO3_CdCO3 + par_bio_red_CaCO3_CdCO3_alpha* &
            & ocn(io_Cd,dum_i,dum_j,n_k)/ocn(io_Ca,dum_i,dum_j,n_k)
    end if
    !
    ! TRACE METALS: Li
    if (ocn_select(io_Li) .AND. ocn_select(io_Ca)) then
       bio_part_red(is_CaCO3,is_LiCO3,dum_i,dum_j) = par_bio_red_CaCO3_LiCO3 + par_bio_red_CaCO3_LiCO3_alpha* &
            & ocn(io_Li,dum_i,dum_j,n_k)/ocn(io_Ca,dum_i,dum_j,n_k)
    end if
    !
    ! TRACE METALS: Sr
    if (ocn_select(io_Sr) .AND. ocn_select(io_Sr)) then
       bio_part_red(is_CaCO3,is_SrCO3,dum_i,dum_j) = par_bio_red_CaCO3_SrCO3 + par_bio_red_CaCO3_SrCO3_alpha* &
            & ocn(io_Sr,dum_i,dum_j,n_k)/ocn(io_Ca,dum_i,dum_j,n_k)
    end if
    !
    ! TRACE METALS: Os
    if (ocn_select(io_Os) .AND. ocn_select(io_Os)) then
       bio_part_red(is_CaCO3,is_OsCO3,dum_i,dum_j) = par_bio_red_CaCO3_OsCO3 + par_bio_red_CaCO3_OsCO3_alpha* &
            & ocn(io_Os,dum_i,dum_j,n_k)/ocn(io_Ca,dum_i,dum_j,n_k)
    end if

    ! *** CALCULATE ISOTOPIC FRACTIONATION ************************************************************************************** !
    ! NOTE: implement isotopic fraction as a 'Redfield' ratio (populate array <bio_part_red>)
    ! NOTE: *** REMEMBER to convert r (13C/(13C + 12C)) ratio to R (13C/12C) before applying fractionation factor alpha **********
    !       *** and then convert back to little r again to create the isotopic 'Redfield ratio' **********************************
    ! NOTE: assume that the total mass of C is approximately equal to 12C + 13C
    ! NOTE: assume for 14C, the standard is already in the form: 14C/C
    ! NOTE: the array <carbisor> represents its isotopic ratio as little 'r'
    ! NOTE: T-dependent fractionation for calcite following Mook [1986]
    ! NOTE: CaCO3 fractionation w.r.t. HCO3-
    !
    ! ---------------------------------------------------------- !
    ! d13C [POC]
    ! ---------------------------------------------------------- !
    if (sed_select(is_POC_13C)) then
       ! ------------------------------------------------------- ! calculate productivity-weighted ef
       !                                                           in the case of multiple plankton 'groups'
       SELECT CASE (par_bio_prodopt)
       CASE (             &
            & 'bio_PFeSi' &
            & )
          if (loc_dPO4 > const_real_nullsmall) then
             loc_d13C_DIC_Corg_ef = (loc_dPO4_sp*par_d13C_DIC_Corg_ef_sp + loc_dPO4_nsp*par_d13C_DIC_Corg_ef_nsp)/loc_dPO4
          else
             loc_d13C_DIC_Corg_ef = par_d13C_DIC_Corg_ef
          endif
       case default
          loc_d13C_DIC_Corg_ef = par_d13C_DIC_Corg_ef
       end select
       ! ------------------------------------------------------- ! calculate the 13C/12C fractionation between DIC and POC
       SELECT CASE (opt_d13C_DIC_Corg)
       CASE ('Poppetal')
          bio_part_red(is_POC,is_POC_13C,dum_i,dum_j) = &
               & fun_Corg_Rfrac_Poppetal(carb(ic_conc_CO2,dum_i,dum_j,n_k), &
               & carbisor(ici_CO2_r13C,dum_i,dum_j,n_k),loc_d13C_DIC_Corg_ef,par_d13C_DIC_Corg_b,.false.)
       CASE ('FIXED')
          loc_r13C = ocn(io_DIC_13C,dum_i,dum_j,n_k)/ocn(io_DIC,dum_i,dum_j,n_k)
          ! apply fixed fractionation
          loc_alpha = 1.0 + par_d13C_DIC_Corg_epsilon/1000.0
          loc_R = loc_r13C/(1.0 - loc_r13C)
          bio_part_red(is_POC,is_POC_13C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
       case default
          bio_part_red(is_POC,is_POC_13C,dum_i,dum_j) = &
               & fun_Corg_Rfrac(ocn(io_T,dum_i,dum_j,n_k),carb(ic_conc_CO2,dum_i,dum_j,n_k), &
               & carbisor(ici_CO2_r13C,dum_i,dum_j,n_k),loc_d13C_DIC_Corg_ef,.false.)
       end select
    end if
    ! ---------------------------------------------------------- !
    ! d14C [POC]
    ! ---------------------------------------------------------- !
    if (sed_select(is_POC_14C)) then
       SELECT CASE (opt_d13C_DIC_Corg)
       CASE ('Poppetal')
          bio_part_red(is_POC,is_POC_14C,dum_i,dum_j) = &
               & fun_Corg_Rfrac_Poppetal(carb(ic_conc_CO2,dum_i,dum_j,n_k), &
               & carbisor(ici_CO2_r14C,dum_i,dum_j,n_k),loc_d13C_DIC_Corg_ef,par_d13C_DIC_Corg_b,.true.)
       CASE ('FIXED')
          loc_r14C = ocn(io_DIC_14C,dum_i,dum_j,n_k)/ocn(io_DIC,dum_i,dum_j,n_k)
          ! apply fixed fractionation
          loc_alpha = 1.0 + 2.0*par_d13C_DIC_Corg_epsilon/1000.0
          loc_R = loc_r14C/(1.0 - loc_r14C)
          bio_part_red(is_POC,is_POC_14C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)          
       case default
          bio_part_red(is_POC,is_POC_14C,dum_i,dum_j) = &
               & fun_Corg_Rfrac(ocn(io_T,dum_i,dum_j,n_k),carb(ic_conc_CO2,dum_i,dum_j,n_k), &
               & carbisor(ici_CO2_r14C,dum_i,dum_j,n_k),loc_d13C_DIC_Corg_ef,.true.)
       end select
    end if
    ! ---------------------------------------------------------- !
    ! d56Fe [POFe]
    ! ---------------------------------------------------------- !
    if (sed_select(is_POFe_56Fe)) then
       ! NOTE: no need to specify Fe scheme, as the values of loc_FeT and loc_FeT_56Fe have already been populated according to
       !       the specific configuration of explicit (tracer) dissolved Fe pools
       ! NOTE: no biological uptake fractionation assumed (currently)
       if (loc_FeT>const_rns) then
          loc_r56Fe = loc_FeT_56Fe/loc_FeT
       else
          loc_r56Fe = 0.0
       end if
       bio_part_red(is_POFe,is_POFe_56Fe,dum_i,dum_j) = loc_r56Fe
    end if
    ! ---------------------------------------------------------- !
    ! carbon isotopes in CaCO3
    ! ---------------------------------------------------------- !
    ! d13C [CaCO3]
    if (sed_select(is_CaCO3_13C)) then
       ! calculate 13C/12C fractionation between DIC and CaCO3
       loc_delta_CaCO3 = 15.10 - 4232.0/ocn(io_T,dum_i,dum_j,n_k)
       loc_alpha = 1.0 + loc_delta_CaCO3/1000.0
       loc_R = carbisor(ici_HCO3_r13C,dum_i,dum_j,n_k)/(1.0 - carbisor(ici_HCO3_r13C,dum_i,dum_j,n_k))
       bio_part_red(is_CaCO3,is_CaCO3_13C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    ! d14C [CaCO3]
    if (sed_select(is_CaCO3_14C)) then
       ! calculate 14C/C fractionation between DIC and CaCO3
       loc_alpha = 1.0 + 2.0*loc_delta_CaCO3/1000.0
       loc_R = carbisor(ici_HCO3_r14C,dum_i,dum_j,n_k)/(1.0 - carbisor(ici_HCO3_r14C,dum_i,dum_j,n_k))
       bio_part_red(is_CaCO3,is_CaCO3_14C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    ! d44Ca [CaCO3]
    if (sed_select(is_CaCO3_44Ca)) then
       ! calculate 44Ca/40Ca fractionation between Ca and CaCO3
       loc_r44Ca = ocn(io_Ca_44Ca,dum_i,dum_j,n_k)/ocn(io_Ca,dum_i,dum_j,n_k)
       loc_R = loc_r44Ca/(1.0 - loc_r44Ca)
       SELECT CASE (opt_d44Ca_Ca_CaCO3)
       CASE ('Fantle')
          ! D44Ca_calcite-Ca(aq) = -0.066649·omega_calcite - 0.320614
          loc_alpha = 1.0 + (-0.066649*loc_ohm - 0.320614)/1000.0
       CASE ('Komar')
          ! D44Ca_calcite-Ca(aq) = −(1.31 ± 0.12) + (3.69 ± 0.59) [CO2−3](mmol/kg)
          ! NOTE: here, [CO32-] converted from mmol kg-1 to umol kg-1
          loc_alpha = 1.0 + (-1.31 + 3.69*loc_co3*1.0E3)/1000.0
       case default
          ! fixed fractionation
          loc_alpha = 1.0 + par_d44Ca_CaCO3_epsilon/1000.0
       end select
       bio_part_red(is_CaCO3,is_CaCO3_44Ca,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    !
    ! d30Si [opal]
    if (sed_select(is_opal_30Si)) then
       if (ocn(io_SiO2,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r30Si = ocn(io_SiO2_30Si,dum_i,dum_j,n_k)/ocn(io_SiO2,dum_i,dum_j,n_k)
       else
          loc_r30Si = 0.0
       endif
       ! Fractionation of 30Si during opal formation by diatoms
       loc_alpha = 1.0 + par_d30Si_opal_epsilon/1000.0
       loc_R = loc_r30Si/(1.0 - loc_r30Si)
       bio_part_red(is_opal,is_opal_30Si,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    !
    ! d114Cd [POCd]
    if (sed_select(is_POCd_114Cd)) then
       ! calculate 114/???Cd fractionation between Cd and POCd
       ! NOTE: d114Cd = 1.0002 (w.r.t. 110/112?) == -0.2 o/oo
       !       mean ocean d114Cd = 0.04 o/oo
       if (ocn(io_Cd,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r114Cd = ocn(io_Cd_114Cd,dum_i,dum_j,n_k)/ocn(io_Cd,dum_i,dum_j,n_k)
       else
          loc_r114Cd = 0.0
       end if
       loc_alpha = 1.0 + par_d114Cd_POCd_epsilon/1000.0
       loc_R = loc_r114Cd/(1.0 - loc_r114Cd)
       bio_part_red(is_POCd,is_POCd_114Cd,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    !
    ! d114Cd [CaCO3]
    if (sed_select(is_CdCO3_114Cd)) then
       ! calculate 114/???Cd fractionation between Cd and CdCO3
       if (ocn(io_Cd,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r114Cd = ocn(io_Cd_114Cd,dum_i,dum_j,n_k)/ocn(io_Cd,dum_i,dum_j,n_k)
       else
          loc_r114Cd = 0.0
       end if
       loc_alpha = 1.0 + par_d114Cd_CdCO3_epsilon/1000.0
       loc_R = loc_r114Cd/(1.0 - loc_r114Cd)
       bio_part_red(is_CdCO3,is_CdCO3_114Cd,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    !
    ! d7Li [CaCO3]
    if (sed_select(is_LiCO3_7Li)) then
       ! calculate 7/6Li fractionation between Li and LiCO3
       if (ocn(io_Li,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r7Li = ocn(io_Li_7Li,dum_i,dum_j,n_k)/ocn(io_Li,dum_i,dum_j,n_k)
       else
          loc_r7Li = 0.0
       end if
       loc_alpha = 1.0 + par_d7Li_LiCO3_epsilon/1000.0
       loc_R = loc_r7Li/(1.0 - loc_r7Li)
       bio_part_red(is_LiCO3,is_LiCO3_7Li,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    !
    ! 87 + 88Sr [CaCO3]
    ! NOTE: 87 has half the fractionation as for 88 (hence there is no explicit par_d87Sr_SrCO3_alpha parameter)
    ! NOTE: be lazy and add deltas ...
    if (sed_select(is_SrCO3_87Sr) .AND. sed_select(is_SrCO3_87Sr)) then
       if (ocn(io_Sr,dum_i,dum_j,n_k) > const_real_nullsmall) then
          ! initialization
          loc_86Sr = ocn(io_Sr,dum_i,dum_j,n_k)-ocn(io_Sr_87Sr,dum_i,dum_j,n_k)-ocn(io_Sr_88Sr,dum_i,dum_j,n_k)
          loc_ocn(io_Sr) = ocn(io_Sr,dum_i,dum_j,n_k)
          ! calculate d87Sr of export
          loc_87Sr = ocn(io_Sr_87Sr,dum_i,dum_j,n_k)
          loc_standard = const_standardsR(ocn_type(io_Sr_87Sr))
          loc_delta = fun_calc_isotope_deltaR(loc_86Sr,loc_87Sr,loc_standard,const_real_null) + par_d88Sr_SrCO3_epsilon/2.0
          loc_ocn(io_Sr_87Sr) = loc_delta
          ! calculate d88Sr of export
          loc_88Sr = ocn(io_Sr_88Sr,dum_i,dum_j,n_k)
          loc_standard = const_standardsR(ocn_type(io_Sr_88Sr))
          loc_delta = fun_calc_isotope_deltaR(loc_86Sr,loc_88Sr,loc_standard,const_real_null) + par_d88Sr_SrCO3_epsilon
          loc_ocn(io_Sr_88Sr) = loc_delta
          ! calculate new Sr ISOTOPE abundance -- 87Sr
          loc_87Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_ocn(:),1)
          ! calculate new Sr ISOTOPE abundance -- 88Sr
          loc_88Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_ocn(:),2)
          ! calculate equivalent Redfield ratios (comapred to bulk Sr)
          bio_part_red(is_SrCO3,is_SrCO3_87Sr,dum_i,dum_j) = loc_87Sr/ocn(io_Sr,dum_i,dum_j,n_k)
          bio_part_red(is_SrCO3,is_SrCO3_88Sr,dum_i,dum_j) = loc_88Sr/ocn(io_Sr,dum_i,dum_j,n_k)
       else
          bio_part_red(is_SrCO3,is_SrCO3_87Sr,dum_i,dum_j) = 0.0
          bio_part_red(is_SrCO3,is_SrCO3_88Sr,dum_i,dum_j) = 0.0
       end if
    end if
    !
    ! 187 + 188Os [CaCO3]
    ! NOTE: be lazy and add deltas ...
    if (sed_select(is_OsCO3_187Os) .AND. sed_select(is_OsCO3_187Os)) then
       if (ocn(io_Os,dum_i,dum_j,n_k) > const_real_nullsmall) then
          ! initialization
          loc_192Os = ocn(io_Os,dum_i,dum_j,n_k)-ocn(io_Os_187Os,dum_i,dum_j,n_k)-ocn(io_Os_188Os,dum_i,dum_j,n_k)
          loc_ocn(io_Os) = ocn(io_Os,dum_i,dum_j,n_k)
          ! calculate 187Os/192Os of export
          loc_187Os = ocn(io_Os_187Os,dum_i,dum_j,n_k)
          loc_standard = const_standardsR(ocn_type(io_Os_187Os))
          loc_delta = fun_calc_isotope_deltaR(loc_192Os,loc_187Os,loc_standard,const_real_null) + par_d187Os_OsCO3_epsilon
          loc_ocn(io_Os_187Os) = loc_delta
          ! calculate 188Os/192Os of export
          loc_188Os = ocn(io_Os_188Os,dum_i,dum_j,n_k)
          loc_standard = const_standardsR(ocn_type(io_Os_188Os))
          loc_delta = fun_calc_isotope_deltaR(loc_192Os,loc_188Os,loc_standard,const_real_null) + par_d188Os_OsCO3_epsilon
          loc_ocn(io_Os_188Os) = loc_delta
          ! calculate new Os ISOTOPE abundance -- 187Os
          loc_187Os = fun_calc_isotope_abundanceR012ocn(io_Os_187Os,io_Os_188Os,loc_ocn(:),1)
          ! calculate new Os ISOTOPE abundance -- 188Os
          loc_188Os = fun_calc_isotope_abundanceR012ocn(io_Os_187Os,io_Os_188Os,loc_ocn(:),2)
          ! calculate equivalent Redfield ratios (comapred to bulk Os)
          bio_part_red(is_OsCO3,is_OsCO3_187Os,dum_i,dum_j) = loc_187Os/ocn(io_Os,dum_i,dum_j,n_k)
          bio_part_red(is_OsCO3,is_OsCO3_188Os,dum_i,dum_j) = loc_188Os/ocn(io_Os,dum_i,dum_j,n_k)
       else
          bio_part_red(is_OsCO3,is_OsCO3_187Os,dum_i,dum_j) = 0.0
          bio_part_red(is_OsCO3,is_OsCO3_188Os,dum_i,dum_j) = 0.0
       end if
    end if

    ! ### INSERT CODE TO DEAL WITH ADDITIONAL ISOTOPES ########################################################################### !
    !
    ! ############################################################################################################################ !

    ! -------------------------------------------------------- !
    ! CALCULATE BULK EXPORT
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! establish POC currency
    ! NOTE: calculate export in currency of particulate carbon (rather than PO4)
    ! NOTE: put everything into particulate form initially, but re-scale later to account for DOM export
    bio_part(is_POC,dum_i,dum_j,loc_k_mld:n_k) = bio_part_red(is_POP,is_POC,dum_i,dum_j)*loc_dPO4
    ! -------------------------------------------------------- ! set bulk export (CaCO3, opal)
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       select case (sed_type(is))
       case (par_sed_type_bio)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(is_POC,is,dum_i,dum_j)*bio_part(is_POC,dum_i,dum_j,loc_k_mld:n_k)
       end select
    end DO
    ! -------------------------------------------------------- !
    ! CALCULATE ASSOCIATED ELEMENTAL EXPORT
    ! -------------------------------------------------------- !
    ! NOTE: scavenging is handled elsewhere
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       select case (sed_type(is))
       case (par_sed_type_POM)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(is_POC,is,dum_i,dum_j)*bio_part(is_POC,dum_i,dum_j,loc_k_mld:n_k)
       case (par_sed_type_CaCO3)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(is_CaCO3,is,dum_i,dum_j)*bio_part(is_CaCO3,dum_i,dum_j,loc_k_mld:n_k)
       case (par_sed_type_opal)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(is_opal,is,dum_i,dum_j)*bio_part(is_opal,dum_i,dum_j,loc_k_mld:n_k)
       end select
       ! ----------------------------------------------------- ! Correction for higher N:P ratio of N2 fixers
       ! NOTE: Fanny (June 2010)
       if (sed_dep(is) == is_PON) then
          SELECT CASE (par_bio_prodopt)
          CASE ( &
               & '2N2T_PN_Tdep',  &
               & '3N2T_PNFe_Tdep' &
               & )
             bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = loc_bio_NP*loc_dPO4_1 + par_bio_NPdiaz*loc_dPO4_2
          END select
       end if
       ! ----------------------------------------------------- ! Correction for higher Fe:P ratio for N2 fixers
       ! NOTE: Calculation from Kfe/Kp of diazo
       ! NOTE: Fanny (July 2010)
       if (sed_dep(is) == is_POFe) then
          SELECT CASE (par_bio_prodopt)
          CASE ('3N2T_PNFe_Tdep')
             bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
                  & bio_part_red(is_POC,is_POFe,dum_i,dum_j)*bio_part_red(is_POP,is_POC,dum_i,dum_j)*loc_dPO4_1 &
                  & + par_bio_c0_Fe_Diaz/par_bio_c0_PO4*loc_dPO4_2
          END select
       end if
       ! ----------------------------------------------------- ! Kick the can down the road & deal with all the complex N shit later
       ! settings zeros (and no N in POM) for now
       ! (e.g. as isotopes will be complex to unravel after the fact)
       ! NOTE: AR 19/01/16
       if (is == is_PON) then
          SELECT CASE (par_bio_prodopt)
          CASE ( &
               & '2N2T_PO4MM_NO3' &
               & )
             bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = 0.0
          END select
       end if
    end DO
    ! -------------------------------------------------------- !
    ! CALCULATE ASSOCIATED ISOTOPIC EXPORT
    ! -------------------------------------------------------- !
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       select case (sed_type(is))
       case (n_itype_min:n_itype_max)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(sed_dep(is),is,dum_i,dum_j)*bio_part(sed_dep(is),dum_i,dum_j,loc_k_mld:n_k)
       case (n_itype_minR:n_itype_maxR)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(sed_dep(is),is,dum_i,dum_j)*bio_part(sed_dep(is),dum_i,dum_j,loc_k_mld:n_k)
       end select
    end do
    ! -------------------------------------------------------- !
    ! CALCULATE INORGANIC UPTAKE
    ! -------------------------------------------------------- !
    ! convert particulate sediment tracer indexed array concentrations to (dissolved) tracer indexed array
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_tot_i = conv_sed_ocn_i(0,is)
       do loc_i=1,loc_tot_i
          io = conv_sed_ocn_i(loc_i,is)
          loc_bio_uptake(io,loc_k_mld:n_k) = loc_bio_uptake(io,loc_k_mld:n_k) + &
               & conv_sed_ocn(io,is)*bio_part(is,dum_i,dum_j,loc_k_mld:n_k)
       end do
    end DO
    ! -------------------------------------------------------- !
    ! ADJUST ELEMENTAL UPTAKE
    ! -------------------------------------------------------- !
    ! I cycle
    ! NOTE: IO3- is transformed to I- within the cell (default species exchange with POI is I- not IO3-).
    !       But as default, the reverse of remineralization is applied in order to calculate dissolved update,
    !       meaning that only the I- speices would be removed (rather than IO3- removed and O2 released).
    !       This fix ensures that IO3- is taken up, but 3/2 O2 is then released to balance the O2 cycle
    !       becasue I- is going to be released when POI is remineralized.
    !       ... not ideal to have to have 'exceptions' like this :(
    if (ocn_select(io_IO3)) then
       loc_bio_uptake(io_IO3,loc_k_mld:n_k) = loc_bio_uptake(io_IO3,loc_k_mld:n_k) + loc_bio_uptake(io_I,loc_k_mld:n_k)
       loc_bio_uptake(io_O2,loc_k_mld:n_k)  = loc_bio_uptake(io_O2,loc_k_mld:n_k)  - 1.5*loc_bio_uptake(io_I,loc_k_mld:n_k)
       loc_bio_uptake(io_I,loc_k_mld:n_k)   = 0.0
    end if
    ! Fe cycle
    ! NOTE: when Fe3+ and Fe2+ are selected, we are taking them up in proportion to minimize numerical issues
    !       (e.g. of removing all Fe2+ in a single time-step)
    !       release 1/4 mol O2 per mol Fe3+ -> Fe2+ conversion (negative uptake)
    ! NOTE: by default, iron uptake is assumed as Fe2+
    if (ocn_select(io_Fe) .AND. ocn_select(io_Fe2)) then
       If (loc_FeT > const_rns) then
          ! relative availablity of Fe2+ 
          loc_frac_Fe2 = ocn(io_Fe2,dum_i,dum_j,n_k)/loc_FeT
          if (loc_frac_Fe2 < const_rns) loc_frac_Fe2 = 0.0
          if (loc_frac_Fe2 > 1.0)       loc_frac_Fe2 = 1.0
          ! correct bulk iron source balance
          ! NOTE: can directly replace Fe and Fe2 in loc_bio_uptake, but only adjust existing O2 value
          loc_bio_uptake(io_Fe,n_k)  = (1.0 - loc_frac_Fe2)*loc_bio_uptake(io_Fe2,n_k)
          loc_bio_uptake(io_O2,n_k)  = loc_bio_uptake(io_O2,n_k) - (1.0/4.0)*loc_bio_uptake(io_Fe,n_k)
          loc_bio_uptake(io_Fe2,n_k) = loc_frac_Fe2*loc_bio_uptake(io_Fe2,n_k)
          ! correct isotope source balance
          If (ocn_select(io_Fe_56Fe) .AND. (ocn_select(io_Fe2_56Fe))) then
             if (ocn(io_Fe,dum_i,dum_j,n_k) > const_rns) then
                loc_bio_uptake(io_Fe_56Fe,n_k) =  &
                     & ocn(io_Fe_56Fe,dum_i,dum_j,n_k)*(loc_bio_uptake(io_Fe,n_k)/ocn(io_Fe,dum_i,dum_j,n_k))
             else
                loc_bio_uptake(io_Fe_56Fe,n_k) = 0.0
             end if
             if (ocn(io_Fe2,dum_i,dum_j,n_k) > const_rns) then
                loc_bio_uptake(io_Fe2_56Fe,n_k) = &
                     & ocn(io_Fe2_56Fe,dum_i,dum_j,n_k)*(loc_bio_uptake(io_Fe2,n_k)/ocn(io_Fe2,dum_i,dum_j,n_k))
             else
                loc_bio_uptake(io_Fe2_56Fe,n_k) = 0.0
             end if
             ! replace the 56Fe value in POM
             bio_part(is_POFe_56Fe,dum_i,dum_j,n_k) = loc_bio_uptake(io_Fe_56Fe,n_k) + loc_bio_uptake(io_Fe2_56Fe,n_k)
          end if
       else
          loc_frac_Fe2 = 1.0
       end if
    else
       loc_frac_Fe2 = 0.0
    end if
    ! N cycle
    ! non-standard productivity schemes
    SELECT CASE (par_bio_prodopt)
    CASE ( &
         & '2N2T_PO4MM_NO3' &
         & )
       ! diazatrophs -- simply (but then assuming, ultimately: PON --> 0.5N2):
       ! N2 --> 2PON
       loc_dN2 = par_bio_NPdiaz*loc_dPO4_2
       bio_part(is_PON,dum_i,dum_j,n_k) = bio_part(is_PON,dum_i,dum_j,n_k) + loc_dN2
       loc_bio_uptake(io_N2,n_k) = loc_bio_uptake(io_N2,n_k) + 0.5*loc_dN2
       ! ammonia assimilation (consistent with the assumed remin conservation equation):
       ! NH4+ + (3/4)O2 --> PON + (3/2)H2O + H+
       If (par_bio_red_POP_PON*loc_dPO4_1 < ocn(io_NH4,dum_i,dum_j,n_k)) then
          loc_dNH4 = par_bio_red_POP_PON*loc_dPO4_1
       else
          loc_dNH4 = ocn(io_NH4,dum_i,dum_j,n_k)
       end if
       bio_part(is_PON,dum_i,dum_j,n_k) = bio_part(is_PON,dum_i,dum_j,n_k) + loc_dNH4
       loc_bio_uptake(io_NH4,n_k) = loc_bio_uptake(io_NH4,n_k) + loc_dNH4
       loc_bio_uptake(io_O2,n_k)  = loc_bio_uptake(io_O2,n_k)  + (3.0/4.0)*loc_dNH4
       loc_bio_uptake(io_ALK,n_k) = loc_bio_uptake(io_ALK,n_k) + loc_dNH4
       loc_dNO3 = par_bio_red_POP_PON*loc_dPO4_1 - loc_dNH4
       ! nitrate uptake, assuming:
       ! H+ + NO3- --> PON + (5/4)O2 + (1/2)H2O
       bio_part(is_PON,dum_i,dum_j,n_k) = bio_part(is_PON,dum_i,dum_j,n_k) + loc_dNO3
       loc_bio_uptake(io_NO3,n_k) = loc_bio_uptake(io_NO3,n_k) + loc_dNO3
       loc_bio_uptake(io_O2,n_k)  = loc_bio_uptake(io_O2,n_k)  - (5.0/4.0)*loc_dNO3
       loc_bio_uptake(io_ALK,n_k) = loc_bio_uptake(io_ALK,n_k) - loc_dNO3
       ! isotopes
       if (sed_select(is_PON_15N)) then
          ! N2
          if (ocn(io_N2,dum_i,dum_j,n_k) > const_real_nullsmall) then
             loc_r15N = ocn(io_N2_15N,dum_i,dum_j,n_k)/ocn(io_N2,dum_i,dum_j,n_k)
          else
             loc_r15N = 0.0
          end if
          loc_alpha = 1.0 + par_bio_uptake_dN2_epsilon/1000.0
          loc_R = loc_r15N/(1.0 - loc_r15N)
          loc_red = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
          bio_part(is_PON_15N,dum_i,dum_j,n_k) = loc_red*loc_dN2
          loc_bio_uptake(io_N2_15N,n_k) = loc_bio_uptake(io_N2_15N,n_k) + 0.5*loc_red*loc_dN2
          ! NH4
          if (ocn(io_NH4,dum_i,dum_j,n_k) > const_real_nullsmall) then
             loc_r15N = ocn(io_NH4_15N,dum_i,dum_j,n_k)/ocn(io_NH4,dum_i,dum_j,n_k)
          else
             loc_r15N = 0.0
          end if
          loc_alpha = 1.0 + par_bio_uptake_dNH4_epsilon/1000.0
          loc_R = loc_r15N/(1.0 - loc_r15N)
          loc_red = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
          bio_part(is_PON_15N,dum_i,dum_j,n_k) = loc_red*loc_dNH4
          loc_bio_uptake(io_NH4_15N,n_k) = loc_bio_uptake(io_NH4_15N,n_k) + loc_red*loc_dNH4
          ! NO3
          if (ocn(io_NO3,dum_i,dum_j,n_k) > const_real_nullsmall) then
             loc_r15N = ocn(io_NO3_15N,dum_i,dum_j,n_k)/ocn(io_NO3,dum_i,dum_j,n_k)
          else
             loc_r15N = 0.0
          end if
          loc_alpha = 1.0 + par_bio_uptake_dNO3_epsilon/1000.0
          loc_R = loc_r15N/(1.0 - loc_r15N)
          loc_red = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
          bio_part(is_PON_15N,dum_i,dum_j,n_k) = loc_red*loc_dNO3
          loc_bio_uptake(io_NO3_15N,n_k) = loc_bio_uptake(io_NO3_15N,n_k) + loc_red*loc_dNO3
       end if
    CASE ( &
         & '2N2T_PN_Tdep',   &
         & '3N2T_PNFe_Tdep'  &
         & )
       ! -------------------------------------------------------- ! adjustment due to N2 uptake
       ! adjust default biological tracer uptake stoichiometry due to N2 fixation (replacing some NO3 consumption)
       ! NOTE: the correction is a little involved as the NO3 updake requirement implicitly includes
       !       (a) NO3 uptake by 'normal' phytoplankton, plus
       !       (b) N2 fixation ... converted to NO3 'uptake' but at a different N:P compared to 'normal' phytoplankton
       !       once the N2 uptake (anomoly) is calculated, the remaining terms are adjusted based on the N2 anomoly
       ! NOTE: assuemd stiochometry: 2NO3- + 2H+ <-> (5/2)O2 + N2 + H2O
       !                             NO3- + H2O + 2H+ <-> 2O2 + NH4+
       ! NOTE: because the N2 uptake (anomoly) is being used, no prior sources or sinks of N2 are assumed
       loc_bio_uptake(io_N2,loc_k_mld:n_k) = &
            & 0.5* &
            & ( &
            &   (par_bio_NPdiaz/loc_bio_NP) / ((1.0/loc_frac_N2fix - 1.0) + (par_bio_NPdiaz/loc_bio_NP)) &
            & )* &
            & loc_bio_uptake(io_NO3,loc_k_mld:n_k)
       loc_bio_uptake(io_O2,loc_k_mld:n_k)  = loc_bio_uptake(io_O2,loc_k_mld:n_k)  + (5.0/2.0)*loc_bio_uptake(io_N2,loc_k_mld:n_k)
       loc_bio_uptake(io_ALK,loc_k_mld:n_k) = loc_bio_uptake(io_ALK,loc_k_mld:n_k) + 2.0*loc_bio_uptake(io_N2,loc_k_mld:n_k)
       loc_bio_uptake(io_NO3,loc_k_mld:n_k) = loc_bio_uptake(io_NO3,loc_k_mld:n_k) - 2.0*loc_bio_uptake(io_N2,loc_k_mld:n_k)
       ! -------------------------------------------------------- ! adjustment due to NH4 uptake
       ! adjust default biological tracer uptake stoichiometry due to NH4 consumption (replacing some NO3 consumption)
       ! assuming: NH4 is consummed first (Fanny - July 2011)
       loc_bio_uptake(io_NH4,loc_K_mld:n_k) = &
            & min(loc_bio_uptake(io_NO3,loc_k_mld:n_k),ocn(io_NH4,dum_i,dum_j,loc_k_mld:n_k))
       loc_bio_uptake(io_O2,loc_k_mld:n_k)  = loc_bio_uptake(io_O2,loc_k_mld:n_k)  + 2.0*loc_bio_uptake(io_NH4,loc_k_mld:n_k)
       loc_bio_uptake(io_ALK,loc_k_mld:n_k) = loc_bio_uptake(io_ALK,loc_k_mld:n_k) + 2.0*loc_bio_uptake(io_NH4,loc_k_mld:n_k)
       loc_bio_uptake(io_NO3,loc_k_mld:n_k) = loc_bio_uptake(io_NO3,loc_k_mld:n_k) - 1.0*loc_bio_uptake(io_NH4,loc_k_mld:n_k)
    END select
    ! -------------------------------------------------------- !
    ! RE-SCALE FOR DISSOLVED ORGANIC MATTER PRODUCTION
    ! -------------------------------------------------------- !
    ! calculate DOM components and adjust POM accordingly
    int_fracdom(:) = 0.0
    DO l=1,n_l_sed
       loc_r_POM_DOM  = 0.0
       loc_r_POM_RDOM = 0.0
       is = conv_iselected_is(l)
       ! create DOM fraction
       loc_tot_i = conv_POM_DOM_i(0,is)
       do loc_i=1,loc_tot_i
          io = conv_POM_DOM_i(loc_i,is)
          ! set POM->DOM conversion modifier
          select case (ocn_dep(io))
          case (io_DOM_P)
             loc_r_POM_DOM = par_bio_red_rP_POM_DOM
          case (io_DOM_N)
             loc_r_POM_DOM = par_bio_red_rN_POM_DOM
          case default
             loc_r_POM_DOM = 1.0
          end select
          ! calculate decrease in particulate fraction
          loc_bio_part_DOM(is,loc_k_mld:n_k) = loc_r_POM_DOM*loc_bio_red_DOMfrac*bio_part(is,dum_i,dum_j,loc_k_mld:n_k)
          ! create (and add) dissolved tracers
          bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) = bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) + loc_bio_part_DOM(is,loc_k_mld:n_k)
       end do
       ! create RDOM fraction
       loc_tot_i = conv_POM_RDOM_i(0,is)
       do loc_i=1,loc_tot_i
          io = conv_POM_RDOM_i(loc_i,is)
          ! set POM->DOM conversion modifier
          select case (ocn_dep(io))
          case (io_RDOM_P)
             loc_r_POM_RDOM = par_bio_red_rP_POM_RDOM
          case (io_RDOM_N)
             loc_r_POM_RDOM = par_bio_red_rN_POM_RDOM
          case default
             loc_r_POM_RDOM = 1.0
          end select
          ! calculate decrease in particulate fraction
          loc_bio_part_RDOM(is,loc_k_mld:n_k) = loc_r_POM_RDOM*loc_bio_red_RDOMfrac*bio_part(is,dum_i,dum_j,loc_k_mld:n_k)
          ! create (and add) dissolved tracers
          bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) = bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) + loc_bio_part_RDOM(is,loc_k_mld:n_k)
       end do
       ! save total DOM fraction [NOTE: not a global mean ... just this (i,j) location ...]
       if ((loc_r_POM_DOM + loc_r_POM_RDOM) > const_real_nullsmall) then
          int_fracdom(is) = loc_r_POM_DOM*loc_bio_red_DOMfrac + loc_r_POM_RDOM*loc_bio_red_RDOMfrac
       else
          int_fracdom(is) = 0.0
       end if
    end do
    ! decrease particulate fraction
    bio_part(:,dum_i,dum_j,loc_k_mld:n_k) = bio_part(:,dum_i,dum_j,loc_k_mld:n_k) - &
         (loc_bio_part_DOM(:,loc_k_mld:n_k) + loc_bio_part_RDOM(:,loc_k_mld:n_k))

    ! *** INITIAL PARTICULATE FRACTION PARTITIONING ***
    ! set partitioning between differently remineralized particulate fluxes
    ! NOTE: this code should ideally be replaced by a generic algorithm
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (ctrl_bio_remin_POC_ballast) then
       DO k=n_k,loc_k_mld,-1
          if (bio_part(is_POC,dum_i,dum_j,k) > const_real_nullsmall) then
             bio_part(is_POC_frac2,dum_i,dum_j,k) =                                    &
                  & (                                                                  &
                  &   par_bio_remin_kc(dum_i,dum_j)*bio_part(is_CaCO3,dum_i,dum_j,k) + &
                  &   par_bio_remin_ko(dum_i,dum_j)*bio_part(is_opal,dum_i,dum_j,k) +  &
                  &   par_bio_remin_kl(dum_i,dum_j)*bio_part(is_det,dum_i,dum_j,k)     &
                  & )                                                                  &
                  & /bio_part(is_POC,dum_i,dum_j,k)
          else
             bio_part(is_POC_frac2,dum_i,dum_j,k) = 0.0
          end if
          if (bio_part(is_POC_frac2,dum_i,dum_j,k) > 1.0) bio_part(is_POC_frac2,dum_i,dum_j,k) = 1.0
       end DO
    else
       loc_kPO4 = loc_PO4/(loc_PO4 + par_bio_remin_POC_c0frac2)
       bio_part(is_POC_frac2,dum_i,dum_j,loc_k_mld:n_k) = (1.0-loc_kPO4)*par_bio_remin_POC_dfrac2 + par_bio_remin_POC_frac2
    end if
    bio_part(is_CaCO3_frac2,dum_i,dum_j,loc_k_mld:n_k) = par_bio_remin_CaCO3_frac2
    bio_part(is_opal_frac2,dum_i,dum_j,loc_k_mld:n_k)  = par_bio_remin_opal_frac2
    ! set 'b' exponent in the Martin curve (if selected)
    if (par_bio_remin_fun == 'Henson2012') then
       par_bio_remin_b(dum_i,dum_j) = (0.024 * loc_TC) - 1.06
    elseif (par_bio_remin_fun == 'Martin1987') then
       par_bio_remin_b(dum_i,dum_j) = par_bio_remin_martin_b
    end if
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ! *** DIRECTLY CREATE PLANKTON TRACERS ***
    !
    ! NOTE: benthic foram tracer values are set in SEDGEM
    if (sed_select(is_CaCO3) .AND. sed_select(is_foram_p_13C)) then
       ! calculate 13C/12C fractionation between DIC and CaCO3
       SELECT CASE (opt_bio_foram_p_13C_delta)
       CASE ('NONE')
          loc_delta_CaCO3 = 0.0
       case ('ABIOTIC')
          loc_delta_CaCO3 = 15.10 - 4232.0/ocn(io_T,dum_i,dum_j,n_k)
       case ('SPERO')
          ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          loc_delta_CaCO3 = 0.0
          ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       end SELECT
       loc_alpha = 1.0 + loc_delta_CaCO3/1000.0
       loc_R = carbisor(ici_HCO3_r13C,dum_i,dum_j,n_k)/(1.0 - carbisor(ici_HCO3_r13C,dum_i,dum_j,n_k))
       bio_part_red(is_CaCO3,is_foram_p_13C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
       bio_part(is_foram_p_13C,dum_i,dum_j,loc_k_mld:n_k) = &
            & bio_part_red(is_CaCO3,is_foram_p_13C,dum_i,dum_j)*bio_part(is_CaCO3,dum_i,dum_j,loc_k_mld:n_k)
    end if
    if (sed_select(is_CaCO3) .AND. sed_select(is_CaCO3_18O)) then
       ! assume no fractionation for now
       ! also: assume no interaction with bulk ocean chemsitry (i.e. no removal of 18O from the ocean)
       loc_delta_CaCO3 = 0.0
       loc_alpha = 1.0 + loc_delta_CaCO3/1000.0
       if (ocn(io_O2,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r18O = ocn(io_O2_18O,dum_i,dum_j,n_k)/ocn(io_O2,dum_i,dum_j,n_k)
       else
          loc_r18O = 1.0
       end if
       loc_R = loc_r18O/(1.0 - loc_r18O)
       bio_part_red(is_CaCO3,is_CaCO3_18O,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
       bio_part(is_CaCO3_18O,dum_i,dum_j,loc_k_mld:n_k) = &
            & bio_part_red(is_CaCO3,is_CaCO3_18O,dum_i,dum_j)*bio_part(is_CaCO3,dum_i,dum_j,loc_k_mld:n_k)
    end if

    ! *** Fe SCAVENGING ***
    ! calculate scavenging of Fe from water column by newly formed particulates
    ! NOTE: need to calculate the effective residence time of particulates in the surface later(s)
    !       => employ sinking velocity for scavenging
    ! NOTE: FOR NOW ... KEEP OLD STYLE SUBROUTINE -- TO UTILIZE THE NEW ONE, BIO_PART AND BIO_REMIN
    !       MUST BE REFORMULATED IN THE COMPACT TRACER NOTATION
    if (ocn_select(io_Fe)) then
       SELECT CASE (trim(opt_geochem_Fe))
       CASE ('OLD')
          DO k=n_k,loc_k_mld,-1
             if (ocn(io_Fe,dum_i,dum_j,k) > const_real_nullsmall) then
                call sub_calc_scav_Fe(                                                      &
                     & dum_dt,                                                              &
                     & phys_ocn(ipo_Dbot,dum_i,dum_j,k)/par_bio_remin_sinkingrate_reaction, &
                     & ocn(io_Fe,dum_i,dum_j,k),                                            &
                     & bio_part(:,dum_i,dum_j,k),                                           &
                     & bio_remin(:,dum_i,dum_j,k)                                           &
                     & )
             end if
          end DO
       case default
          ! DO NOTHING
       end SELECT
    end if

    ! *** WRITE DATA ***
    ! set modification of tracer concentrations
    ! NOTE: depletion of dissolved species as a result of biological productivity is implimented as negative remineralization
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) = bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) - loc_bio_uptake(io,loc_k_mld:n_k)
    end do
    ! record diagnostics
    ! NOTE: scale productivity modifiers by time-step to get correct average
    ! ### NOTE ################################################################################################################### !
    ! need to adjust (units?) diagnostics consistent with MLD changes(?)
    ! added *2 for nitrogen fixation diagnostic to calculate rate in molN/kg/yr (rather than molN2/kg/yr) - Fanny (July 2010)
    ! ############################################################################################################################ !
    diag_bio(idiag_bio_dPO4,dum_i,dum_j) = loc_dPO4
    diag_bio(idiag_bio_DOMfrac,dum_i,dum_j) = dum_dt*loc_bio_red_DOMfrac
    SELECT CASE (par_bio_prodopt)
    CASE ( &
         & '2N2T_PO4MM_NO3', &
         & '2N2T_PN_Tdep',   &
         & '3N2T_PNFe_Tdep'  &
         & )
       diag_bio(idiag_bio_frac_Fe2,dum_i,dum_j)   = dum_dt*loc_frac_Fe2
       diag_bio(idiag_bio_dPO4_1,dum_i,dum_j)     = loc_dPO4_1
       diag_bio(idiag_bio_dPO4_2,dum_i,dum_j)     = loc_dPO4_2
       diag_bio(idiag_bio_N2fixation,dum_i,dum_j) = loc_bio_uptake(io_N2,n_k)*2
       diag_bio(idiag_bio_NH4assim,dum_i,dum_j)   = loc_bio_uptake(io_NH4,n_k)
    case (                  &
         & 'bio_PFe',       &
         & 'bio_PFe_OCMIP2' &
         & )
       diag_bio(idiag_bio_knut,dum_i,dum_j)     = dum_dt*min(loc_kPO4,loc_kFe)
       diag_bio(idiag_bio_frac_Fe2,dum_i,dum_j) = dum_dt*loc_frac_Fe2
    case (                        &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       diag_bio(idiag_bio_knut,dum_i,dum_j)     = dum_dt*min(loc_kPO4,loc_kFe)
       diag_bio(idiag_bio_frac_Fe2,dum_i,dum_j) = dum_dt*loc_frac_Fe2
       diag_bio(idiag_bio_dPO4_1,dum_i,dum_j)   = loc_dPO4_sp
       diag_bio(idiag_bio_dPO4_2,dum_i,dum_j)   = loc_dPO4_nsp
       ! sp vs. nsp diagnostics
       ! NOTE: simply use the existing array value if unable to calculate a new one ...
       if (loc_dPO4_nsp*bio_part(is_POC,dum_i,dum_j,n_k) > const_real_nullsmall) then
          diag_bio(idiag_bio_CaCO3toPOC_nsp,dum_i,dum_j) = (loc_dPO4/loc_dPO4_nsp)* &
               & bio_part(is_CaCO3,dum_i,dum_j,n_k)/bio_part(is_POC,dum_i,dum_j,n_k)
       else
          diag_bio(idiag_bio_CaCO3toPOC_nsp,dum_i,dum_j) = diag_bio(idiag_bio_CaCO3toPOC_nsp,dum_i,dum_j)
       endif
       if (loc_dPO4_sp*bio_part(is_POC,dum_i,dum_j,n_k) > const_real_nullsmall) then
          diag_bio(idiag_bio_opaltoPOC_sp,dum_i,dum_j) = (loc_dPO4/loc_dPO4_sp)* &
               & bio_part(is_opal,dum_i,dum_j,n_k)/bio_part(is_POC,dum_i,dum_j,n_k)
       else
          diag_bio(idiag_bio_opaltoPOC_sp,dum_i,dum_j) = diag_bio(idiag_bio_opaltoPOC_sp,dum_i,dum_j)
       endif
       if (loc_dPO4 > const_real_nullsmall) then
          diag_bio(idiag_bio_fspPOC,dum_i,dum_j) = (loc_dPO4_sp/loc_dPO4)
       else
          diag_bio(idiag_bio_fspPOC,dum_i,dum_j) = diag_bio(idiag_bio_fspPOC,dum_i,dum_j)
       endif
    end SELECT
  end SUBROUTINE sub_calc_bio_uptake
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE PREFORMED TRACERS
  SUBROUTINE sub_calc_bio_preformed(dum_i,dum_j,dum_atm)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j
    REAL,dimension(n_atm),INTENT(in)::dum_atm
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::io
    real,dimension(n_ocn)::loc_ocn
    real::loc_tot,loc_frac,loc_standard
    real::loc_d14Catm,loc_d14Cocn
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    ! zero local array
    loc_ocn(:) = 0.0
    ! set array equal to ocean surface tracer concentrations
    loc_ocn(:) = ocn(:,dum_i,dum_j,n_k)
    ! -------------------------------------------------------- !
    ! SET PRE-FORMED TRACERS
    ! -------------------------------------------------------- !
    ! NOTE: Preformed C14 age is just 8033*ln((D14Catm+1000)/(D14Cocn+1000)) at the surface
    !       d14C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
    !       where: loc_tot = io_DIC; loc_frac = io_DIC_14C; loc_standard = const_standards(ocn_type(io_DIC_14C))
    !       D14C = fun_convert_delta14CtoD14C(loc_d13C,loc_d14C)
    ! NOTE: io_col9 is Csoft
    ! NOTE: if radiocarbon is not selected, then io_col8 is used as 13C of Csoft
    if (ctrl_bio_preformed) then
       do io=io_col0,io_col9
          if (ocn_select(io)) then
             select case (io)
             CASE (io_col0)
                if (ocn_select(io_DIC)) bio_remin(io,dum_i,dum_j,n_k)     = loc_ocn(io_DIC)     - loc_ocn(io)
             CASE (io_col1)
                if (ocn_select(io_ALK)) bio_remin(io,dum_i,dum_j,n_k)     = loc_ocn(io_ALK)     - loc_ocn(io)
             CASE (io_col2)
                if (ocn_select(io_O2)) bio_remin(io,dum_i,dum_j,n_k)      = loc_ocn(io_O2)      - loc_ocn(io)
             CASE (io_col3)
                if (ocn_select(io_PO4)) bio_remin(io,dum_i,dum_j,n_k)     = loc_ocn(io_PO4)     - loc_ocn(io)
             CASE (io_col4)
                if (ocn_select(io_NO3)) bio_remin(io,dum_i,dum_j,n_k)     = loc_ocn(io_NO3)     - loc_ocn(io)
             CASE (io_col5)
                if (ocn_select(io_Fe)) bio_remin(io,dum_i,dum_j,n_k)      = loc_ocn(io_Fe)      - loc_ocn(io)
                if (ocn_select(io_TDFe)) bio_remin(io,dum_i,dum_j,n_k)    = loc_ocn(io_TDFe)    - loc_ocn(io)
             CASE (io_col6)
                if (ocn_select(io_SiO2)) bio_remin(io,dum_i,dum_j,n_k)    = loc_ocn(io_SiO2)    - loc_ocn(io)
             CASE (io_col7)
                if (ocn_select(io_DIC_13C)) bio_remin(io,dum_i,dum_j,n_k) = loc_ocn(io_DIC_13C) - loc_ocn(io)
             CASE (io_col8)
                if (ocn_select(io_DIC_14C)) then
                   loc_standard = const_standards(ocn_type(io_DIC_14C))
                   loc_tot     = dum_atm(ia_pCO2)
                   loc_frac    = dum_atm(ia_pCO2_14C)
                   loc_d14Catm = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                   loc_standard = const_standards(ocn_type(io_DIC_14C))
                   loc_tot     = loc_ocn(io_DIC)
                   loc_frac    = loc_ocn(io_DIC_14C)
                   loc_d14Cocn = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                   bio_remin(io,dum_i,dum_j,n_k) = &
                        & const_lamda_14C_libby*log( (loc_d14Catm+1000.0)/(loc_d14Cocn+1000.0) ) - loc_ocn(io)
                else
                   bio_remin(io,dum_i,dum_j,n_k) = 0.0 - loc_ocn(io)                  
                end if
             CASE (io_col9)
                bio_remin(io,dum_i,dum_j,n_k) = 0.0 - loc_ocn(io)
             end select
          end if
       end do
    end if
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_calc_bio_preformed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ABIOTIC TRACER UPTAKE AT THE SURFACE OCEAN
  SUBROUTINE sub_calc_bio_uptake_abio(dum_i,dum_j,dum_k1,dum_dt)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dt
    ! local variables
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real,dimension(n_sed,n_k)::loc_bio_part
    real::loc_ohm
    real::loc_delta_CaCO3
    real::loc_alpha
    real::loc_R,loc_r7Li
    integer::loc_kmax

    ! *** INITIALIZE VARIABLES ***
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_uptake(io,:) = 0.0
    end do
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       loc_bio_part(is,:) = 0.0
    end DO
    ! restrict abiotic precipitation to the surface if requested
    if (ctrl_bio_CaCO3precip_sur) then
       loc_kmax = n_k
    else
       loc_kmax = dum_k1
    end if

    ! *** CALCULATE CaCO3 PRECIPITATION ***
    DO k=n_k,dum_k1,-1
       ! re-calculate carbonate dissociation constants
       CALL sub_calc_carbconst(                 &
            & phys_ocn(ipo_Dmid,dum_i,dum_j,k), &
            & ocn(io_T,dum_i,dum_j,k),          &
            & ocn(io_S,dum_i,dum_j,k),          &
            & carbconst(:,dum_i,dum_j,k)        &
            & )
       ! adjust carbonate constants
       if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
          call sub_adj_carbconst(           &
               & ocn(io_Ca,dum_i,dum_j,k),  &
               & ocn(io_Mg,dum_i,dum_j,k),  &
               & carbconst(:,dum_i,dum_j,k) &
               & )
       end if
       ! re-estimate Ca and borate concentrations from salinity (if not selected and therefore explicitly treated)
       IF (.NOT. ocn_select(io_Ca))  ocn(io_Ca,dum_i,dum_j,k)  = fun_calc_Ca(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_B))   ocn(io_B,dum_i,dum_j,k)   = fun_calc_Btot(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_SO4)) ocn(io_SO4,dum_i,dum_j,k) = fun_calc_SO4tot(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_F))   ocn(io_F,dum_i,dum_j,k)   = fun_calc_Ftot(ocn(io_S,dum_i,dum_j,k))
       ! re-calculate surface ocean carbonate chemistry
       CALL sub_calc_carb(             &
            & ocn(io_DIC,dum_i,dum_j,k),  &
            & ocn(io_ALK,dum_i,dum_j,k),  &
            & ocn(io_Ca,dum_i,dum_j,k),   &
            & ocn(io_PO4,dum_i,dum_j,k),  &
            & ocn(io_SiO2,dum_i,dum_j,k), &
            & ocn(io_B,dum_i,dum_j,k),    &
            & ocn(io_SO4,dum_i,dum_j,k),  &
            & ocn(io_F,dum_i,dum_j,k),    &
            & ocn(io_H2S,dum_i,dum_j,k),  &
            & ocn(io_NH4,dum_i,dum_j,k),  &
            & carbconst(:,dum_i,dum_j,k), &
            & carb(:,dum_i,dum_j,k),      &
            & carbalk(:,dum_i,dum_j,k)    &
            & )
       ! select for calcite vs. aragonite precipitation
       if (par_bio_CaCO3precip_calcite) then
          loc_ohm = carb(ic_ohm_cal,dum_i,dum_j,k)
       else
          loc_ohm = carb(ic_ohm_arg,dum_i,dum_j,k)
       end if
       if (loc_ohm > par_bio_CaCO3precip_abioticohm_min) then
          loc_bio_part(is_CaCO3,k) = &
               & dum_dt*par_bio_CaCO3precip_sf*(par_bio_CaCO3precip_abioticohm_min - 1.0)**par_bio_CaCO3precip_exp
       else
          loc_bio_part(is_CaCO3,k) = 0.0
       end if
       if (sed_select(is_CaCO3_13C)) then
          ! re-calculate carbonate system isotopic properties
          if (ocn_select(io_DIC_13C)) then
             call sub_calc_carb_r13C(           &
                  & ocn(io_T,dum_i,dum_j,k),       &
                  & ocn(io_DIC,dum_i,dum_j,k),     &
                  & ocn(io_DIC_13C,dum_i,dum_j,k), &
                  & carb(:,dum_i,dum_j,k),         &
                  & carbisor(:,dum_i,dum_j,k)      &
                  & )
          end IF
          ! calculate 13C/12C fractionation between DIC and CaCO3
          ! NOTE: T-dependent fractionation for calcite following Mook [1986]
          ! NOTE: CaCO3 fractionation w.r.t. HCO3-
          loc_delta_CaCO3 = 15.10 - 4232.0/ocn(io_T,dum_i,dum_j,k)
          loc_alpha = 1.0 + loc_delta_CaCO3/1000.0
          loc_R = carbisor(ici_HCO3_r13C,dum_i,dum_j,k)/(1.0 - carbisor(ici_HCO3_r13C,dum_i,dum_j,k))
          loc_bio_part(is_CaCO3_13C,k) = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_bio_part(is_CaCO3,k)
       end if
       ! Li
       if (ocn_select(io_Li) .AND. ocn_select(io_Ca)) then
          loc_bio_part(is_LiCO3,k) = &
               & ( &
               & par_bio_red_CaCO3_LiCO3 + par_bio_red_CaCO3_LiCO3_alpha* &
               & ocn(io_Li,dum_i,dum_j,k)/ocn(io_Ca,dum_i,dum_j,k)    &
               & ) * &
               & loc_bio_part(is_CaCO3,k)
          if (sed_select(is_LiCO3_7Li)) then
             ! calculate 7/6Li fractionation between Li and LiCO3
             if (ocn(io_Li,dum_i,dum_j,n_k) > const_real_nullsmall) then
                loc_r7Li = ocn(io_Li_7Li,dum_i,dum_j,n_k)/ocn(io_Li,dum_i,dum_j,n_k)
             else
                loc_r7Li = 0.0
             end if
             loc_alpha = 1.0 + par_d7Li_LiCO3_epsilon/1000.0
             loc_R = loc_r7Li/(1.0 - loc_r7Li)
             loc_bio_part(is_LiCO3_7Li,k) = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_bio_part(is_LiCO3,k)
          end if
       end if
       ! convert particulate sediment tracer indexed array concentrations to (dissolved) tracer indexed array
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_tot_i = conv_sed_ocn_i(0,is)
          do loc_i=1,loc_tot_i
             io = conv_sed_ocn_i(loc_i,is)
             loc_bio_uptake(io,k) = loc_bio_uptake(io,k) + conv_sed_ocn(io,is)*loc_bio_part(is,k)
          end do
       end DO
    end DO

    ! *** SET MODIFICATION OF TRACER CONCENTRATIONS ***
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) - loc_bio_uptake(io,:)
    end do

    ! *** SET MODIFICATION OF PARTICULATE CONCENTRATIONS ***
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       bio_part(is,dum_i,dum_j,:) = bio_part(is,dum_i,dum_j,:) + loc_bio_part(is,:)
    end DO

  end SUBROUTINE sub_calc_bio_uptake_abio
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! IRON SPECIATION
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Fe SPECIATION -- goethite
  ! NOTE: this scheme assumes the conservation and transport of: total dissolved Fe and total L
  ! NOTE: not used in this function, but [Fe] is used to interface with old Fe-tracer based code
  function fun_box_calc_lookup_Fe_4D_Fe3(dum_ip)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real::fun_box_calc_lookup_Fe_4D_Fe3
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real,dimension(4),INTENT(in)::dum_ip
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real,dimension(4)::loc_ip
    real::loc_Fe3
    ! -------------------------------------------------------- !
    ! CALCULATE IRON SPECIATION
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- !
    loc_ip(1) = dum_ip(1) - const_zeroC                        ! temperature (C)
    loc_ip(2) = -log10(dum_ip(2))                              ! pH (from [H+])
    loc_ip(3) = 1.0E9*dum_ip(3)                                ! total dissolved Fe concentration [nM]
    loc_ip(4) = 1.0E9*dum_ip(4)                                ! total ligand concentration [nM]
    ! -------------------------------------------------------- !
    loc_Fe3 = fun_interp_4Dvec(                                         &
         & loc_ip(1),loc_ip(2),loc_ip(3),loc_ip(4),                     &
         & lookup_Fe_1D_1,lookup_Fe_1D_2,lookup_Fe_1D_3,lookup_Fe_1D_4, &
         & lookup_Fe_4D_Fe3)
    ! -------------------------------------------------------- !
    ! RETURN RESULT
    ! -------------------------------------------------------- !
    ! NOTE: loc_Fe3 in units of mol kg-1
    fun_box_calc_lookup_Fe_4D_Fe3 = loc_Fe3
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end function fun_box_calc_lookup_Fe_4D_Fe3
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Fe SPECIATION -- goethite
  ! NOTE: this scheme assumes the conservation and transport of: total dissolved Fe and total L
  ! NOTE: not used in this function, but [Fe] is used to interface with old Fe-tracer based code
  function fun_box_calc_lookup_Fe_4D_geo(dum_ip)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    real::fun_box_calc_lookup_Fe_4D_geo
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real,dimension(4),INTENT(in)::dum_ip
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real,dimension(4)::loc_ip
    real::loc_geo
    ! -------------------------------------------------------- !
    ! CALCULATE IRON SPECIATION
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! initialize variables
    loc_ip(1) = dum_ip(1) - const_zeroC                        ! temperature (C)
    loc_ip(2) = -log10(dum_ip(2))                              ! pH (from [H+])
    loc_ip(3) = 1.0E9*dum_ip(3)                                ! total dissolved Fe concentration [nM]
    loc_ip(4) = 1.0E9*dum_ip(4)                                ! total ligand concentration [nM]
    ! -------------------------------------------------------- !
    loc_geo = fun_interp_4Dvec(                                         &
         & loc_ip(1),loc_ip(2),loc_ip(3),loc_ip(4),                     &
         & lookup_Fe_1D_1,lookup_Fe_1D_2,lookup_Fe_1D_3,lookup_Fe_1D_4, &
         & lookup_Fe_4D_geo)
    ! NOTE: loc_geo in units of mol kg-1
    ! -------------------------------------------------------- !
    ! RETURN RESULT
    ! -------------------------------------------------------- !
    fun_box_calc_lookup_Fe_4D_geo = loc_geo
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end function fun_box_calc_lookup_Fe_4D_geo
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Fe SPECIATION
  SUBROUTINE sub_calc_geochem_Fe(dum_i,dum_j,dum_k1,dum_focnFe)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,dimension(n_k),INTENT(in)::dum_focnFe
    ! local variables
    INTEGER::k
    real::loc_Fe,loc_FeL,loc_L
    real::loc_FeT,loc_LT
    real,DIMENSION(2)::loc_roots

    ! *** CALCULATE Fe SPECIATION THROUGHOUT THE WATER COLUMN***
    DO k=n_k,dum_k1,-1
       ! initialize variables
       loc_Fe  = ocn(io_Fe,dum_i,dum_j,k) + bio_remin(io_Fe,dum_i,dum_j,k) + dum_focnFe(k)
       loc_FeL = ocn(io_FeL,dum_i,dum_j,k) + bio_remin(io_FeL,dum_i,dum_j,k)
       loc_L   = ocn(io_L,dum_i,dum_j,k) + bio_remin(io_L,dum_i,dum_j,k)
       loc_FeT = loc_FeL + loc_Fe
       loc_LT  = loc_FeL + loc_L
       ! solve Fe speciation equation:
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
       loc_roots(:) = fun_quad_root(1.0,-(loc_LT + loc_FeT + 1.0/par_K_FeL),loc_FeT*loc_LT)
       ! filter returned roots
       if (maxval(loc_roots(:)) < const_real_nullsmall) then
          IF (ctrl_debug_reportwarnings) THEN
             CALL sub_report_error( &
                  & 'biogem_box.f90','sub_calc_geochem_Fe', &
                  & 'No REAL root in Fe speciation calculation (or maybe zero ...).'// &
                  & ' / Data: dum_i,dum_j,k,loc_FeL(OLD),loc_Fe(OLD),loc_L(OLD),loc_FeT(OLD),loc_LT(OLD),', &
                  & 'SOD THIS FOR A GAME OF SOLDIERS: calculation abondoned ...', &
                  & (/real(dum_i),real(dum_j),real(k),loc_FeL,loc_Fe,loc_L,loc_FeT,loc_LT/),.false. &
                  & )
             error_stop = .FALSE.
          end IF
       elseif ((minval(loc_roots(:)) > loc_FeT) .AND. (minval(loc_roots(:)) > loc_LT)) then
          IF (ctrl_debug_reportwarnings) THEN
             CALL sub_report_error( &
                  & 'biogem_box.f90','sub_calc_geochem_Fe', &
                  & 'No solution to Fe speciation calculation possible ... :('// &
                  & ' / Data: dum_i,dum_j,k,loc_FeL(OLD),loc_Fe(OLD),loc_L(OLD),loc_FeT(OLD),loc_LT(OLD),', &
                  & 'SOD THIS FOR A GAME OF SOLDIERS: calculation abondoned ...', &
                  & (/real(dum_i),real(dum_j),real(k),loc_FeL,loc_Fe,loc_L,loc_FeT,loc_LT/),.false. &
                  & )
             error_stop = .FALSE.
          end IF
       else
          if (minval(loc_roots(:)) < const_real_nullsmall) then
             loc_FeL = maxval(loc_roots(:))
          else
             loc_FeL = minval(loc_roots(:))
          end if
          loc_Fe  = loc_FeT - loc_FeL
          loc_L   = loc_LT - loc_FeL
       end if
       ! re-calculate reminerlization arrays to give rise to calculated Fe speciation
       ! NOTE: subtract <dum_focnFe> again because it is added subsequently in the main BIOGEM loop through <locijk_focn>
       bio_remin(io_Fe,dum_i,dum_j,k)  = loc_Fe - ocn(io_Fe,dum_i,dum_j,k) - dum_focnFe(k)
       bio_remin(io_FeL,dum_i,dum_j,k) = loc_FeL - ocn(io_FeL,dum_i,dum_j,k)
       bio_remin(io_L,dum_i,dum_j,k)   = loc_L - ocn(io_L,dum_i,dum_j,k)
    end DO

  end SUBROUTINE sub_calc_geochem_Fe
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! REMINERALIZATION
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_box_remin_redfield(dum_ocn,dum_conv_ls_lo)
    ! ---------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! ---------------------------------------------------------- !
    real,dimension(1:n_l_ocn),INTENT(in)::dum_ocn                !
    real,dimension(1:n_l_ocn,1:n_l_sed),INTENT(inout)::dum_conv_ls_lo             !
    ! ---------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! ---------------------------------------------------------- !
    real::loc_k
    real::loc_O2,loc_NO3,loc_FeOOH,loc_SO4
    real::loc_kO2,loc_kNO3,loc_kFeOOH,loc_kSO4,loc_kmeth
    real::loc_kiO2,loc_kiNO3,loc_kiFeOOH,loc_kiSO4
    real::loc_r15N,loc_r56Fe,loc_r34S
    real::loc_alpha,loc_R
    ! ---------------------------------------------------------- ! initialize
    loc_k     = 0.0
    loc_r15N  = 0.0
    loc_r56Fe = 0.0
    loc_r34S  = 0.0
    ! ---------------------------------------------------------- !
    ! CREATE REMIN ARRAY
    ! ---------------------------------------------------------- !
    ! ---------------------------------------------------------- ! set MM-type rate limitations
    ! NOTE: equation form follows Arndt et al. [2013] (ESR) and Boudreau [1997] (book)
    ! NOTE: truncate loc concentrations at zero to avoid negative values being propagated ...
    ! NOTE: catch a local oxidation value of const_real_nullsmall (or less),
    !       as this means that the oxidant was probably negative in the first place
    !       => disable that particular redox remin pathway by setting the kinetic parameter to ZERO
    ! NOTE: to disable inhibition, set the inhibition parameter to a very large number, then
    !       e.g. par_bio_remin_ci_O2/(par_bio_remin_ci_O2 + loc_O2) approaches a value of 1.0
    if (ocn_select(io_O2)) then
       loc_O2 = dum_ocn(io2l(io_O2))
       if (loc_O2 <= const_real_nullsmall) then
          loc_O2   = 0.0
          loc_kO2  = 0.0
          loc_kiO2 = 1.0
       else
          loc_kO2 = loc_O2/(loc_O2 + par_bio_remin_c0_O2)
          loc_kiO2 = par_bio_remin_ci_O2/(par_bio_remin_ci_O2 + loc_O2)
       end if
       loc_k    = loc_k + par_bio_remin_k_O2*loc_kO2
    else
       loc_O2   = 0.0
       loc_kO2  = 0.0
       loc_kiO2 = 1.0
    end if
    if (ocn_select(io_NO3)) then
       loc_NO3 = dum_ocn(io2l(io_NO3))
       if (loc_NO3 <= const_real_nullsmall) then
          loc_NO3   = 0.0
          loc_kNO3  = 0.0
          loc_kiNO3 = 1.0
       else
          loc_kNO3 = loc_NO3/(loc_NO3 + par_bio_remin_c0_NO3)
          loc_kiNO3 = par_bio_remin_ci_NO3/(par_bio_remin_ci_NO3 + loc_NO3)
       end if
       loc_k     = loc_k + par_bio_remin_k_NO3*loc_kNO3*loc_kiO2
    else
       loc_NO3   = 0.0
       loc_kNO3  = 0.0
       loc_kiNO3 = 1.0
    end if
    if (ocn_select(io_FeOOH)) then
       loc_FeOOH = dum_ocn(io2l(io_FeOOH))
       if (loc_FeOOH <= const_real_nullsmall) then
          loc_FeOOH   = 0.0
          loc_kFeOOH  = 0.0
          loc_kiFeOOH = 1.0
       else
          loc_kFeOOH = loc_FeOOH/(loc_FeOOH + par_bio_remin_c0_FeOOH)
          loc_kiFeOOH = par_bio_remin_ci_FeOOH/(par_bio_remin_ci_FeOOH + loc_FeOOH)
       end if
       loc_k     = loc_k + par_bio_remin_k_FeOOH*loc_kFeOOH*loc_kiNO3*loc_kiO2
    else
       loc_FeOOH   = 0.0
       loc_kFeOOH  = 0.0
       loc_kiFeOOH = 1.0
    end if
    if (ocn_select(io_SO4)) then
       loc_SO4 = dum_ocn(io2l(io_SO4))
       if (loc_SO4 <= const_real_nullsmall) then
          loc_SO4   = 0.0
          loc_kSO4  = 0.0
          loc_kiSO4 = 1.0
       else
          loc_kSO4  = loc_SO4/(loc_SO4 + par_bio_remin_c0_SO4)
          loc_kiSO4 = par_bio_remin_ci_SO4/(par_bio_remin_ci_SO4 + loc_SO4)
       end if
       loc_k     = loc_k + par_bio_remin_k_SO4*loc_kSO4*loc_kiFeOOH*loc_kiNO3*loc_kiO2
    else
       loc_SO4   = 0.0
       loc_kSO4  = 0.0
       loc_kiSO4 = 1.0
    end if
    if (ocn_select(io_CH4)) then
       loc_kmeth = 1.0
       loc_k     = loc_k + par_bio_remin_k_meth*loc_kmeth*loc_kiSO4*loc_kiFeOOH*loc_kiNO3*loc_kiO2
    else
       loc_kmeth = 0.0
    end if
    ! ---------------------------------------------------------- ! remove normalization for kinetic scheme
    if (ctrl_bio_remin_POC_kinetic) loc_k = 1.0
    ! ---------------------------------------------------------- ! check *some* remin occurs in non-kinetic scheme
    ! NOTE: if a hard threshold is selected (subsequent test), CH4 production is assumed if no oxidants remain
    !       (or no remin will occur ...)
    ! here: parameters adjusted in factor (par_bio_remin_k_O2*loc_kO2/loc_k) to make this unity
    ! if no remin would otherwise occur
    if ((.NOT. ctrl_bio_remin_POC_kinetic) .AND. (loc_k < const_real_nullsmall)) then
       loc_kO2 = 1.0
       loc_k   = 1.0/par_bio_remin_k_O2
    end if
    ! ---------------------------------------------------------- ! modify for hard threshold scheme
    if (ctrl_bio_remin_thresh) then
       if (loc_O2 > par_bio_remin_cthresh_O2) then
          loc_k      = par_bio_remin_k_O2*loc_kO2
          loc_kNO3   = 0.0
          loc_kFeOOH = 0.0
          loc_kSO4   = 0.0
          loc_kmeth  = 0.0
       elseif (loc_NO3 > par_bio_remin_cthresh_NO3) then
          loc_kO2    = 0.0
          loc_k      = par_bio_remin_k_NO3*loc_kNO3*loc_kiO2
          loc_kFeOOH = 0.0
          loc_kSO4   = 0.0
          loc_kmeth  = 0.0
       elseif (loc_FeOOH > par_bio_remin_cthresh_FeOOH) then
          loc_kO2    = 0.0
          loc_kNO3   = 0.0
          loc_k      = par_bio_remin_k_FeOOH*loc_kFeOOH*loc_kiNO3*loc_kiO2
          loc_kSO4   = 0.0
          loc_kmeth  = 0.0
       elseif (loc_SO4 > par_bio_remin_cthresh_SO4) then
          loc_kO2    = 0.0
          loc_kNO3   = 0.0
          loc_kFeOOH = 0.0
          loc_k      = par_bio_remin_k_SO4*loc_kSO4*loc_kiFeOOH*loc_kiNO3*loc_kiO2
          loc_kmeth  = 0.0
       else
          loc_kO2    = 0.0
          loc_kNO3   = 0.0
          loc_kFeOOH = 0.0
          loc_kSO4   = 0.0
          loc_k      = par_bio_remin_k_meth*loc_kmeth*loc_kiSO4*loc_kiFeOOH*loc_kiNO3*loc_kiO2
       end if
    end if
    ! ---------------------------------------------------------- ! calculate weighted remin array
    ! NOTE: normalize to 1.0 if a non-kinetic decay scheme is being used
    !       (an exception is the basic temperature-only scheme which also needs to be normalized)
    if (ocn_select(io_O2)) then
       if (ocn_select(io_NO3)) then
          if (ocn_select(io_FeOOH)) then
             if (ocn_select(io_SO4)) then
                if (ocn_select(io_CH4)) then
                   ! O2 + NO3 + FeOOH + SO4 + CH4
                   dum_conv_ls_lo(:,:) = &
                        & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                        & (par_bio_remin_k_NO3*loc_kNO3*loc_kiO2/loc_k)*conv_ls_lo_N(:,:) + &
                        & (par_bio_remin_k_FeOOH*loc_kFeOOH*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_Fe(:,:) + &
                        & (par_bio_remin_k_SO4*loc_kSO4*loc_kiFeOOH*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_S(:,:) + &
                        & (par_bio_remin_k_meth*loc_kmeth*loc_kiSO4*loc_kiFeOOH*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_meth(:,:)
                else
                   ! O2 + NO3 + FeOOH + SO4
                   dum_conv_ls_lo(:,:) = &
                        & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                        & (par_bio_remin_k_NO3*loc_kNO3*loc_kiO2/loc_k)*conv_ls_lo_N(:,:) + &
                        & (par_bio_remin_k_FeOOH*loc_kFeOOH*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_Fe(:,:) + &
                        & (par_bio_remin_k_SO4*loc_kSO4*loc_kiFeOOH*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_S(:,:)
                end if
             else
                ! O2 + NO3 + FeOOH
                dum_conv_ls_lo(:,:) = &
                     & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                     & (par_bio_remin_k_NO3*loc_kNO3*loc_kiO2/loc_k)*conv_ls_lo_N(:,:) + &
                     & (par_bio_remin_k_FeOOH*loc_kFeOOH*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_Fe(:,:)
             end if
          else
             if (ocn_select(io_SO4)) then
                if (ocn_select(io_CH4)) then
                   ! O2 + NO3 + SO4 + CH4
                   dum_conv_ls_lo(:,:) = &
                        & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                        & (par_bio_remin_k_NO3*loc_kNO3*loc_kiO2/loc_k)*conv_ls_lo_N(:,:) + &
                        & (par_bio_remin_k_SO4*loc_kSO4*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_S(:,:) + &
                        & (par_bio_remin_k_meth*loc_kmeth*loc_kiSO4*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_meth(:,:)
                else
                   ! O2 + NO3 + SO4
                   dum_conv_ls_lo(:,:) = &
                        & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                        & (par_bio_remin_k_NO3*loc_kNO3*loc_kiO2/loc_k)*conv_ls_lo_N(:,:) + &
                        & (par_bio_remin_k_SO4*loc_kSO4*loc_kiNO3*loc_kiO2/loc_k)*conv_ls_lo_S(:,:)
                end if
             else
                ! O2 + NO3
                dum_conv_ls_lo(:,:) = &
                     & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                     & (par_bio_remin_k_NO3*loc_kNO3*loc_kiO2/loc_k)*conv_ls_lo_N(:,:)
             end if
          endif
       elseif (ocn_select(io_FeOOH)) then
          if (ocn_select(io_SO4)) then
             if (ocn_select(io_CH4)) then
                ! O2 + FeOOH + SO4 + CH4
                dum_conv_ls_lo(:,:) = &
                     & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                     & (par_bio_remin_k_FeOOH*loc_kFeOOH*loc_kiO2/loc_k)*conv_ls_lo_Fe(:,:) + &
                     & (par_bio_remin_k_SO4*loc_kSO4*loc_kiFeOOH*loc_kiO2/loc_k)*conv_ls_lo_S(:,:) + &
                     & (par_bio_remin_k_meth*loc_kmeth*loc_kiSO4*loc_kiFeOOH*loc_kiO2/loc_k)*conv_ls_lo_meth(:,:)
             else
                ! O2 + FeOOH + SO4
                dum_conv_ls_lo(:,:) = &
                     & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                     & (par_bio_remin_k_FeOOH*loc_kFeOOH*loc_kiO2/loc_k)*conv_ls_lo_Fe(:,:) + &
                     & (par_bio_remin_k_SO4*loc_kSO4*loc_kiFeOOH*loc_kiO2/loc_k)*conv_ls_lo_S(:,:)
             end if
          else
             ! O2 + FeOOH
             dum_conv_ls_lo(:,:) = &
                  & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                  & (par_bio_remin_k_FeOOH*loc_kFeOOH*loc_kiO2/loc_k)*conv_ls_lo_Fe(:,:)
          end if
       elseif (ocn_select(io_SO4)) then
          if (ocn_select(io_CH4)) then
             ! O2 + SO4 + CH4
             dum_conv_ls_lo(:,:) = &
                  & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                  & (par_bio_remin_k_SO4*loc_kSO4*loc_kiO2/loc_k)*conv_ls_lo_S(:,:) + &
                  & (par_bio_remin_k_meth*loc_kmeth*loc_kiSO4*loc_kiO2/loc_k)*conv_ls_lo_meth(:,:)
          else
             ! O2 + SO4
             dum_conv_ls_lo(:,:) = &
                  & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                  & (par_bio_remin_k_SO4*loc_kSO4*loc_kiO2/loc_k)*conv_ls_lo_S(:,:)
          end if
       else
          if (ocn_select(io_CH4)) then
             ! O2 + CH4
             dum_conv_ls_lo(:,:) = &
                  & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:) + &
                  & (par_bio_remin_k_meth*loc_kmeth*loc_kiO2/loc_k)*conv_ls_lo_meth(:,:)
          else
             ! O2
             dum_conv_ls_lo(:,:) = &
                  & (par_bio_remin_k_O2*loc_kO2/loc_k)*conv_ls_lo_O(:,:)
          end if
       end if
    else
       dum_conv_ls_lo(:,:) = 0.0
    end if
    ! ---------------------------------------------------------- !
    ! CORRECT FOR ISOTOPES (in SO4 and NO3)
    ! ---------------------------------------------------------- !
    ! test for N isotopes selected
    ! NOTE: value of loc_NO3 already determined
    ! ### INSERT CODE TO DEAL WITH N ISOTOPES #################################################################################### !
    !
    ! ############################################################################################################################ !
    ! test for S isotopes selected
    ! NOTE: value of loc_SO4 already determined
    if (ocn_select(io_SO4_34S) .AND. ocn_select(io_H2S_34S)) then
       if (loc_SO4 > const_real_nullsmall) then
          loc_r34S  = dum_ocn(io2l(io_SO4_34S)) / loc_SO4
       else
          loc_r34S  = 0.0
       end if
       loc_alpha = 1.0 + par_d34S_Corg_SO4_epsilon/1000.0
       loc_R     = loc_r34S/(1.0 - loc_r34S)
       dum_conv_ls_lo(io2l(io_SO4_34S),:) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)*dum_conv_ls_lo(io2l(io_SO4),:)!dum_conv_ls_lo(io2l(io_SO4_34S),:)
       dum_conv_ls_lo(io2l(io_H2S_34S),:) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)*dum_conv_ls_lo(io2l(io_H2S),:)!dum_conv_ls_lo(io2l(io_H2S_34S),:)
    end if
    ! test for Fe isotopes selected
    ! NOTE: value of loc_FeOOH already determined
    !if (ocn_select(io_FeOOH_56Fe) .AND. ocn_select(io_Fe2_56Fe)) then
    !   if (loc_FeOOH > const_real_nullsmall) then
    !      loc_r56Fe  = dum_ocn(io2l(io_FeOOH_56Fe)) / loc_FeOOH
    !   else
    !      loc_r56Fe  = 0.0
    !   end if
    !   loc_alpha = 1.0 + par_d56Fe_Corg_FeOOH_epsilon/1000.0
    !   loc_R     = loc_r56Fe/(1.0 - loc_r56Fe)
    !   dum_conv_ls_lo(io2l(io_FeOOH_56Fe),:) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)*dum_conv_ls_lo(io2l(io_FeOOH_56Fe),:)
    !   dum_conv_ls_lo(io2l(io_Fe2_56Fe),:) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)*dum_conv_ls_lo(io2l(io_Fe2_56Fe),:)
    !end if
    ! ---------------------------------------------------------- !
    ! END
    ! ---------------------------------------------------------- !
  end SUBROUTINE sub_box_remin_redfield
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WATER COLUMN REMINSERALTZAION OF DISSOLVED ORGANIC MATTER (ALL FRACTIONS)
  SUBROUTINE sub_box_remin_DOM(dum_vocn,dum_vbio_remin,dum_dtyr)
    ! dummy arguments
    type(fieldocn),INTENT(in)::dum_vocn                                 !
    type(fieldocn),INTENT(inout)::dum_vbio_remin                        !
    real,intent(in)::dum_dtyr                                           !
    ! local variables                                                   !
    integer::l,io,is,k,id                                                  !
    integer::lo,ls                                                      !
    integer::loc_m,loc_tot_m                                            !
    real::tmp_bio_remin
    real::loc_bio_remin_DOMratio,loc_bio_remin_RDOMratio                !
    real::loc_bio_remin_DOMlifetime
    real::loc_intI                                                      ! local integrated insolation
    real,dimension(n_l_ocn,n_k)::loc_vbio_remin                         !
    real,dimension(n_sed,n_k)::loc_bio_part                             !
    integer::loc_i,loc_j,loc_k1
    real,dimension(n_l_ocn,n_l_sed)::loc_conv_ls_lo                       !
!!$    CHARACTER(len=31)::loc_string     !
    real,DIMENSION(n_diag_redox,n_k)::loc_diag_redox
!!$    ! define and allocate local arrays
!!$    real,DIMENSION(:,:),ALLOCATABLE::loc_diag_redox
!!$    allocate(loc_diag_redox(n_diag_redox,n_k),STAT=alloc_error)

    ! *** INITIALIZE VARIABLES ***
    ! set local grid point (i,j) information
    loc_i = dum_vocn%i
    loc_j = dum_vocn%j
    loc_k1 = dum_vocn%k1
    ! initialize local tracer arrays
    loc_vbio_remin(:,:)     = 0.0
    loc_bio_part(:,:)       = 0.0
    loc_conv_ls_lo(:,:)   = 0.0
    !
    if (ctrl_bio_remin_redox_save) loc_diag_redox(:,:) = 0.0

    ! *** REMINERALIZE DISSOLVED ORGANIC MATTER ***
    ! NOTE: the new algorithm converts the fraction of DOM marked to be remineralized first into POM before applying the
    !       'usual' generic conversion of sed -> ocn tracers, so as to avoid the need for 'special cases'
    !       (such as of the link between DON and ALK, or POP and ALK)
    ! NOTE: for photolysis (RDOM breakdown restricted to the surface ocean layer), the lifetime is simply divided
    !       by the integrated insolation
    DO k=n_k,loc_k1,-1
       ! calculate DOM lifetime modifier
       ! NOTE: dum_vocn%mk(1,k) is the local temeprature (K)
       ! NOTE: T-dependent DOM remin assumes the same Ea1 as for particulate organic matter remin (par_bio_remin_POC_Ea1)
       !       (but scaling rate constant differs -- par_bio_remin_DOC_K1)
       ! NOTE: check that no more DOM than exists (100% or fraction 1.0), is remineralized
       If (ctrl_bio_remin_DOM_Tdep) then
          loc_bio_remin_DOMratio = dum_dtyr*par_bio_remin_DOC_K1*exp(-par_bio_remin_POC_Ea1/(const_R_SI*dum_vocn%mk(1,k)))
       else
          loc_bio_remin_DOMratio = dum_dtyr/par_bio_remin_DOMlifetime
       end if
       if (loc_bio_remin_DOMratio > 1.0) loc_bio_remin_DOMratio = 1.0
       ! store (surface) lifetime
       if (k == n_k) loc_bio_remin_DOMlifetime = 1.0/(loc_bio_remin_DOMratio/dum_dtyr)
       ! calculate RDOM lifetime modifier
       if (ctrl_bio_remin_RDOM_photolysis) then
          ! restrict photolysis to the surface (n = n_k) layer (otherwise, set a 'high' (effectively infinite) lifetime)
          if (k == n_k) then
             If (phys_ocn(ipo_Dbot,loc_i,loc_j,n_k) >= phys_ocnatm(ipoa_mld,loc_i,loc_j)) then
                ! ml entirely within uppermost (surface) cell
                loc_intI = phys_ocnatm(ipoa_fxsw,loc_i,loc_j)*par_bio_I_eL* &
                     & (1.0 - exp(-phys_ocn(ipo_Dbot,loc_i,loc_j,n_k)/par_bio_I_eL))/phys_ocn(ipo_Dbot,loc_i,loc_j,n_k)
             else
                ! ml deeper than uppermost (surface) cell
                loc_intI = phys_ocnatm(ipoa_fxsw,loc_i,loc_j)*par_bio_I_eL* &
                     & (1.0 - exp(-phys_ocnatm(ipoa_mld,loc_i,loc_j)/par_bio_I_eL))/phys_ocnatm(ipoa_mld,loc_i,loc_j)
             end If
             if (par_bio_remin_RDOMlifetime > loc_intI*dum_dtyr) then
                loc_bio_remin_RDOMratio = loc_intI*dum_dtyr/par_bio_remin_RDOMlifetime
             else
                loc_bio_remin_RDOMratio = 1.0
             end if
          else
             loc_bio_remin_RDOMratio = 0.0
          endif
       else
          if (par_bio_remin_RDOMlifetime > dum_dtyr) then
             loc_bio_remin_RDOMratio = dum_dtyr/par_bio_remin_RDOMlifetime
          else
             loc_bio_remin_RDOMratio = 1.0
          end if
       endif
       ! convert dissolved organic matter to POM as first step in carrying out remineralization
       ! reduce DOM concentrations (as a negative remin tracer anomoly value)
       ! DOM
       if (ocn_select(io_DOM_C)) then
          if (dum_vocn%mk(conv_io_lselected(io_DOM_C),k) > const_real_nullsmall) then
             DO l=3,n_l_ocn
                io = conv_iselected_io(l)
                loc_tot_m = conv_DOM_POM_i(0,io)
                do loc_m=1,loc_tot_m
                   is = conv_DOM_POM_i(loc_m,io)
                   loc_bio_part(is,k)  = loc_bio_part(is,k)  + conv_DOM_POM(is,io)*loc_bio_remin_DOMratio*dum_vocn%mk(l,k)
                   loc_vbio_remin(l,k) = loc_vbio_remin(l,k) - loc_bio_remin_DOMratio*dum_vocn%mk(l,k)
                end do
             end do
          end if
       end if
       ! RDOM
       if (ocn_select(io_RDOM_C)) then
          if (dum_vocn%mk(conv_io_lselected(io_RDOM_C),k) > const_real_nullsmall) then
             DO l=3,n_l_ocn
                io = conv_iselected_io(l)
                loc_tot_m = conv_RDOM_POM_i(0,io)
                do loc_m=1,loc_tot_m
                   is = conv_RDOM_POM_i(loc_m,io)
                   loc_bio_part(is,k)  = loc_bio_part(is,k)  + conv_RDOM_POM(is,io)*loc_bio_remin_RDOMratio*dum_vocn%mk(l,k)
                   loc_vbio_remin(l,k) = loc_vbio_remin(l,k) - loc_bio_remin_RDOMratio*dum_vocn%mk(l,k)
                end do
             end do
          end if
       end if
       ! carry out the remineralization (POM -> inorganic constitutents) itself
       ! (1) create temporary remin conversion array depending on prevailing redox conditions
       call sub_box_remin_redfield(dum_vocn%mk(:,k),loc_conv_ls_lo(:,:))
       ! (2) carry out actual remin
       !     NOTE: catch non-selected tracers (index 0 in the conversion io -> l)
       DO ls=1,n_l_sed
          loc_tot_m = conv_ls_lo_i(0,ls)
          do loc_m=1,loc_tot_m
             lo = conv_ls_lo_i(loc_m,ls)
             if (lo > 0) then
                tmp_bio_remin = loc_conv_ls_lo(lo,ls)*loc_bio_part(l2is(ls),k)
                loc_vbio_remin(lo,k) = loc_vbio_remin(lo,k) + tmp_bio_remin
                if (ctrl_bio_remin_redox_save) then
!!$                   loc_string = 'reminD_'//trim(string_sed(l2is(ls)))//'_d'//trim(string_ocn(l2io(lo)))
!!$                   id = fun_find_str_i(trim(loc_string),string_diag_redox)
                   id = conv_lslo2idD(ls,lo)
                   loc_diag_redox(id,k) = tmp_bio_remin
                end if
             end if
          end do
       end DO

    end DO

    ! *** WRITE GLOBAL ARRAY DATA ***
    ! remin diagnostics
    if (ctrl_bio_remin_redox_save) then
       diag_redox(:,loc_i,loc_j,:) = diag_redox(:,loc_i,loc_j,:) + loc_diag_redox(:,:)
       if (ocn_select(io_col9) .AND. (.NOT. ctrl_bio_preformed_CsoftPOConly)) then
!!$          loc_string = 'reminD_'//trim(string_sed(is_POC))//'_d'//trim(string_ocn(io_DIC))
!!$          id = fun_find_str_i(trim(loc_string),string_diag_redox)
          id = conv_lslo2idD(is2l(is_POC),io2l(io_DIC))
          loc_vbio_remin(io2l(io_col9),:) = loc_vbio_remin(io2l(io_col9),:) + loc_diag_redox(id,:)
       end if
       if (ocn_select(io_col8) .AND. (.NOT. ctrl_bio_preformed_CsoftPOConly) .AND. (.NOT. ocn_select(io_DIC_14C)))then
!!$          loc_string = 'reminD_'//trim(string_sed(is_POC_13C))//'_d'//trim(string_ocn(io_DIC_13C))
!!$          id = fun_find_str_i(trim(loc_string),string_diag_redox)
          id = conv_lslo2idD(is2l(is_POC_13C),io2l(io_DIC_13C))
          loc_vbio_remin(io2l(io_col8),:) = loc_vbio_remin(io2l(io_col8),:) + loc_diag_redox(id,:)
       end if
    end if
    ! write out (surface) lifetime for integration/averaging
    diag_bio(idiag_bio_DOMlifetime,loc_i,loc_j) = dum_dtyr*loc_bio_remin_DOMlifetime
    ! write ocean tracer remineralization field (global array)
    dum_vbio_remin%mk(:,:) = loc_vbio_remin(:,:)
!!$    ! deallocate local arrays
!!$    DEALLOCATE(loc_diag_redox,STAT=alloc_error)

  end SUBROUTINE sub_box_remin_DOM
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE WATER COLUMN TRANSFORMATIONS INVOLVING (MOSTLY BIOGENIC) PARTICULATE MATTER
  SUBROUTINE sub_box_remin_part(dum_dtyr,dum_vocn,dum_vphys_ocn,dum_vbio_part,dum_vbio_remin)
    ! dummy arguments
    real,intent(in)::dum_dtyr
    type(fieldocn),INTENT(in)::dum_vocn                            !
    type(fieldocn),INTENT(in)::dum_vphys_ocn                            !
    type(fieldocn),INTENT(inout)::dum_vbio_part                         !
    type(fieldocn),INTENT(inout)::dum_vbio_remin                        !
    ! local variables
    integer::dum_i, dum_j                                      ! 'virtual' dummy indices (derived from dum_vocn)
    integer::l,is
    integer::lo,ls                                                      !
    integer::id
    INTEGER::k,kk,loc_bio_remin_min_k,loc_klim
    integer::loc_k1
    integer::loc_m,loc_tot_m
    real,dimension(1:n_l_ocn)::loc_vocn                                 !
    real,dimension(1:3)::loc_FeFELL
    real::loc_R,loc_alpha
    real::loc_T,loc_SiO2                                                !
    real::loc_Si_eq,loc_u
    real::loc_scav_Fe,loc_r56Fe
    real::loc_H2S_34S
    real::tmp_bio_remin    !
    real::loc_bio_remin_dD
    real::loc_bio_remin_max_D                                         !
    real::loc_bio_remin_layerratio
    real::loc_bio_remin_sinkingrate_physical                            ! particle sinking rate -- physical
    real::loc_bio_remin_sinkingrate_reaction                            ! particle sinking rate -- for geochemical reactions
    real::loc_bio_remin_dt_physical                                     ! layer residence time (in years)
    real::loc_bio_remin_dt_reaction                                     ! layer residence time (for calculating scavenging)
    real::loc_bio_remin_POC_frac1,loc_bio_remin_POC_frac2
    real::loc_bio_part_POC_ratio
    real::loc_bio_remin_CaCO3_frac1,loc_bio_remin_CaCO3_frac2
    real::loc_bio_part_CaCO3_ratio
    real::loc_bio_remin_opal_frac1,loc_bio_remin_opal_frac2
    real::loc_bio_part_opal_ratio
!!!real::loc_bio_part_FeOOH_ratio,loc_bio_part_FeS2_ratio,loc_bio_part_FeCO3_ratio
    real::loc_eL_size                                                   ! local efolding depth varying with ecosystem size structure JDW
    real::loc_size0                                                     ! JDW
    real::loc_part_tot,loc_part_tot_mineral
    real,dimension(n_l_ocn,n_l_sed)::loc_conv_ls_lo            !
    real,dimension(1:n_l_sed,1:n_k)::loc_bio_part_TMP
    real,dimension(1:n_l_sed,1:n_k)::loc_bio_part_OLD
    real,dimension(1:n_l_sed,1:n_k)::loc_bio_part
    real,dimension(1:n_l_ocn,1:n_k)::loc_bio_remin
    real,dimension(1:n_l_sed,1:n_k)::loc_bio_settle
    real,dimension(1:n_l_sed)::loc_bio_part_remin
    real,DIMENSION(n_diag_precip,n_k)::loc_diag_precip
!!$    CHARACTER(len=31)::loc_string     !
    real,DIMENSION(n_diag_redox,n_k)::loc_diag_redox
!!$    ! define and allocate local arrays
!!$    real,DIMENSION(:,:),ALLOCATABLE::loc_diag_redox
!!$    allocate(loc_diag_redox(n_diag_redox,n_k),STAT=alloc_error)

    ! *** INITIALIZE VARIABLES ***
    !
    loc_bio_remin_POC_frac1 = 0.0
    loc_bio_remin_POC_frac2 = 0.0
    loc_bio_part_POC_ratio = 0.0
    loc_bio_remin_CaCO3_frac1 = 0.0
    loc_bio_remin_CaCO3_frac2 = 0.0
    loc_bio_part_CaCO3_ratio = 0.0
    loc_bio_remin_opal_frac1 = 0.0
    loc_bio_remin_opal_frac2 = 0.0
    loc_bio_part_opal_ratio = 0.0
    ! set 'virtual' dummy grid location indices
    dum_i = dum_vbio_part%i
    dum_j = dum_vbio_part%j
    loc_k1 = dum_vbio_part%k1
    ! copy particulate tracer field to temporary array and reset value of particulate tracer field
    loc_bio_part_OLD(:,:) = dum_vbio_part%mk(:,:)
    dum_vbio_part%mk(:,:) = 0.0
    ! initialize local particulate tracer field array
    loc_bio_part(:,:) = 0.0
    ! initialize remineralization tracer array
    loc_bio_remin(:,:) = 0.0
    ! initialize diagnostics tracers (reset array)
    if (ocn_select(io_H2S) .AND. ocn_select(io_Fe2) .AND. sed_select(is_FeOOH)) then
       diag_react(idiag_react_FeOOH_dFe2,dum_i,dum_j,:) = 0.0
       diag_react(idiag_react_FeOOH_dH2S,dum_i,dum_j,:) = 0.0
       diag_react(idiag_react_FeOOH_dSO4,dum_i,dum_j,:) = 0.0
       diag_react(idiag_react_FeOOH_dALK,dum_i,dum_j,:) = 0.0
    end if
    if (ocn_select(io_H2S) .AND. ocn_select(io_Fe2) .AND. sed_select(is_POM_FeOOH)) then
       diag_react(idiag_react_FeOOH_dFe2,dum_i,dum_j,:) = 0.0
       diag_react(idiag_react_FeOOH_dH2S,dum_i,dum_j,:) = 0.0
       diag_react(idiag_react_FeOOH_dSO4,dum_i,dum_j,:) = 0.0
       diag_react(idiag_react_FeOOH_dALK,dum_i,dum_j,:) = 0.0
    end if
    ! pre-calculate size reciprocal
    loc_size0=1.0/par_bio_remin_POC_size0 ! JDW
    !
    loc_bio_settle(:,:) = 0.0
    !
    loc_vocn(:) = 0.0
    ! replace loc_k1 with virtual grid layer (if virtual grid world selected)
    ! NOTE: don't allow deeper remin than the 'real' ocean (k1)
    if (ctrl_force_Vgrid) then
       loc_k1 = max(loc_k1,force_Vgrid(dum_i,dum_j))
    end if
    ! -------------------------------------------------------- ! set water column particulate tracer loop limit and sinking rate
    ! => test for sinking in any one time-step being less than the max depth of the ocean
    if (dum_dtyr*par_bio_remin_sinkingrate_physical <= goldstein_dsc) then
       ! assume particules could be present at any/every depth in the local water column
       ! set sinking rate for geochemical reactions to the physical sinking rate
       loc_klim = loc_k1
       loc_bio_remin_sinkingrate_physical = par_bio_remin_sinkingrate_physical
       loc_bio_remin_sinkingrate_reaction = par_bio_remin_sinkingrate_physical
    else
       ! leave sinking rate alone ... but could e.g. set to exactly match the ocean depth in one time-step
       loc_klim = n_k
       loc_bio_remin_sinkingrate_physical = par_bio_remin_sinkingrate_physical
       loc_bio_remin_sinkingrate_reaction = par_bio_remin_sinkingrate_reaction
    end if
    ! replace reaction sinking rate if size-dependent sinking is selected
    select case (par_bio_remin_fun)
    case ('KriestOschlies2008_explicit')
       loc_bio_remin_sinkingrate_reaction = &
            & par_bio_remin_POC_w0*((loc_bio_part_OLD(is2l(is_POC_size),n_k))*loc_size0)**par_bio_remin_POC_eta
    end select
    ! -------------------------------------------------------- ! test for possibilty of precip in water column
    ! if so: assume particules could be present at any/every depth in the local water column
    ! if not: assume particulates present only in surface layer
    if (sed_select(is_Fe3Si2O4) .OR. sed_select(is_FeCO3) .OR. sed_select(is_FeS2) .OR. sed_select(is_FeOOH)) then
       loc_klim = loc_k1
    end if
    ! local remin transformation arrays
    loc_conv_ls_lo(:,:)   = 0.0
    !
    if (ctrl_bio_remin_redox_save) then
       loc_diag_redox(:,:)  = 0.0
       loc_diag_precip(:,:) = 0.0
    end if

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! *** k WATER-COLUMN LOOP START ***
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    DO k=n_k,loc_klim,-1
       ! find some particulates in the water column
       ! NOTE: save total mineral load (seperately from total particulate load)
       loc_part_tot = 0.0
       if (sed_select(is_CaCO3)) loc_part_tot = loc_part_tot + loc_bio_part_OLD(is2l(is_CaCO3),k)
       if (sed_select(is_opal))  loc_part_tot = loc_part_tot + loc_bio_part_OLD(is2l(is_opal),k)
       if (sed_select(is_det))   loc_part_tot = loc_part_tot + loc_bio_part_OLD(is2l(is_det),k)
       loc_part_tot_mineral = loc_part_tot
       if (sed_select(is_POC))   loc_part_tot = loc_part_tot + loc_bio_part_OLD(is2l(is_POC),k)
       If (loc_part_tot  > const_real_nullsmall) then
          ! if the identified particulate material is already residing in the bottom-most ocean layer, flag as sediment flux
          If (k == loc_k1) then
             loc_bio_remin_min_k = loc_k1 - 1
          else
             ! determine the deepest layer that sinking material can reach within the current time-step
             ! NOTE: do this regardless of whether a fixed remineralization profile is selected, or whether
             !       remineralization is calculated as a function of residence time in an ocena layer
             !       (and ambient environmental conditions)
             ! NOTE: trap the situation where the depth of the sediment surface is surpassed
             ! NOTE: start loop from the layer lying below the one in which the identified particulate material resides
             loc_bio_remin_min_k = loc_k1 - 1
             loc_bio_remin_max_D = dum_vphys_ocn%mk(ipo_Dbot,k) + dum_dtyr*loc_bio_remin_sinkingrate_physical
             do kk=k-1,loc_k1,-1
                If (dum_vphys_ocn%mk(ipo_Dbot,kk) > loc_bio_remin_max_D) then
                   loc_bio_remin_min_k = kk
                   exit
                end if
             end do
          end if
          ! zero local (temporary) particulate field array, and seed value at location in water column identified
          loc_bio_part_TMP(:,:) = 0.0
          loc_bio_part_TMP(:,k) = loc_bio_part_OLD(:,k)

          ! -------------------------------------------------- !
          ! SURFACE PARTICLE PROCESSES
          ! -------------------------------------------------- !
          ! NOTE: special case (remin loop if for n_k-1 down)
          If (k == n_k) then
             ! ----------------------------------------------- ! surface particle scavenging
             ! NOTE: special case (remin loop if for n_k-1 down)
             ! NOTE: calculate Fe speciation first!
             ! NOTE: for the lookup table, the goethite concentration is assigned to the first array index (otherwise used for [Fe])
             if (ocn_select(io_Fe) .OR. ocn_select(io_TDFe)) then
                ! calculate surface residence time (yr) of particulates in ocean layer (from layer thickness and sinking speed)
                loc_bio_remin_dD = dum_vphys_ocn%mk(ipo_dD,k)
                if (loc_bio_remin_sinkingrate_reaction > const_real_nullsmall) &
                     & loc_bio_remin_dt_reaction = loc_bio_remin_dD/loc_bio_remin_sinkingrate_reaction
                ! calculate Fe scavenging
                SELECT CASE (trim(opt_geochem_Fe))
                case ('OLD')
                   loc_FeFeLL(:) = 0.0
                CASE ('ALT')
                   loc_FeFeLL(:) = fun_box_calc_geochem_Fe(                         &
                        & dum_vocn%mk(io2l(io_Fe),k) + dum_vocn%mk(io2l(io_FeL),k), &
                        & dum_vocn%mk(io2l(io_L),k) + dum_vocn%mk(io2l(io_FeL),k),  &
                        & par_K_FeL &
                        & )
                   loc_bio_remin(io2l(io_Fe),k)  = loc_bio_remin(io2l(io_Fe),k)  + (loc_FeFeLL(1) - dum_vocn%mk(io2l(io_Fe),k))
                   loc_bio_remin(io2l(io_FeL),k) = loc_bio_remin(io2l(io_FeL),k) + (loc_FeFeLL(2) - dum_vocn%mk(io2l(io_FeL),k))
                   loc_bio_remin(io2l(io_L),k)   = loc_bio_remin(io2l(io_L),k)   + (loc_FeFeLL(3) - dum_vocn%mk(io2l(io_L),k))
                CASE ('FeFe2TL')
                   loc_FeFeLL(:) = fun_box_calc_geochem_Fe( &
                        & dum_vocn%mk(io2l(io_Fe),k), &
                        & dum_vocn%mk(io2l(io_TL),k), &
                        & par_K_FeL &
                        & )
                CASE ('hybrid')
                   loc_FeFeLL(:) = fun_box_calc_geochem_Fe( &
                        & dum_vocn%mk(io2l(io_TDFe),k), &
                        & dum_vocn%mk(io2l(io_TL),k), &
                        & par_K_FeL &
                        & )
                CASE ('lookup_4D')
                   loc_FeFeLL(1) = fun_box_calc_lookup_Fe_4D_geo(                 &
                        & (/ ocn(io_T,dum_i,dum_j,k), carb(ic_H,dum_i,dum_j,k),   &
                        & ocn(io_TDFe,dum_i,dum_j,k), ocn(io_TL,dum_i,dum_j,k) /) &
                        & )
                case default
                   loc_FeFeLL(:) = 0.0
                end SELECT
                ! calculate mean Fe isotopic composition
                ! NOTE: isotopes not enabled for all of the Fe schemes
                If ( ocn_select(io_Fe_56Fe) .OR. ocn_select(io_TDFe_56Fe) ) then
                   SELECT CASE (trim(opt_geochem_Fe))
                   CASE ('OLD','ALT')
                      ! re-partition (homogonise) Fe isotopes in proportion to the Fe and FeL pools
                      if ( (loc_FeFeLL(1)+loc_FeFeLL(2)) > const_rns ) then
                         loc_r56Fe = (dum_vocn%mk(io2l(io_Fe_56Fe),kk) + dum_vocn%mk(io2l(io_FeL_56Fe),kk))/ &
                              & (loc_FeFeLL(1) + loc_FeFeLL(2))
                      else
                         loc_r56Fe = 0.0
                      end if
                      loc_bio_remin(io2l(io_Fe_56Fe),kk)  = loc_bio_remin(io2l(io_Fe_56Fe),kk)  + &
                           & (loc_r56Fe*loc_FeFeLL(1) - dum_vocn%mk(io2l(io_Fe_56Fe),kk))
                      loc_bio_remin(io2l(io_FeL_56Fe),kk) = loc_bio_remin(io2l(io_FeL_56Fe),kk) + &
                           & (loc_r56Fe*loc_FeFeLL(2) - dum_vocn%mk(io2l(io_FeL_56Fe),kk))
                   CASE ('FeFe2TL')
                      ! assume only a single total pool (Fe3) is scavenged => no need to homogenize ...
                      if (dum_vocn%mk(io2l(io_Fe),k) > const_rns) then
                         loc_r56Fe = dum_vocn%mk(io2l(io_Fe_56Fe),k)/dum_vocn%mk(io2l(io_Fe),k)
                      else
                         loc_r56Fe = 0.0
                      end if
                   CASE ('hybrid','lookup_4D')
                      ! single total pool ... no need to homogenize ...
                      if (dum_vocn%mk(io2l(io_TDFe),k) > const_rns) then
                         loc_r56Fe = dum_vocn%mk(io2l(io_TDFe_56Fe),k)/dum_vocn%mk(io2l(io_TDFe),k)
                      else
                         loc_r56Fe = 0.0
                      end if
                   case default
                      ! NOTHING!
                   end SELECT
                end if
                SELECT CASE (trim(opt_geochem_Fe))
                CASE ('ALT','OLD')
                   if (loc_FeFeLL(1) > const_real_nullsmall) then
                      loc_scav_Fe = fun_box_scav_Fe(          &
                           & dum_dtyr,                        &
                           & loc_bio_remin_dt_reaction,       &
                           & loc_FeFeLL(1),                   &
                           & loc_bio_part_TMP(is2l(is_POC),k) &
                           & )
                      loc_bio_part_TMP(is2l(is_POM_Fe),k) = loc_bio_part_TMP(is2l(is_POM_Fe),k) + loc_scav_Fe
                      loc_bio_remin(io2l(io_Fe),k) = loc_bio_remin(io2l(io_Fe),k) - loc_scav_Fe
                      If (ocn_select(io_Fe_56Fe)) then
                         loc_bio_remin(io2l(io_Fe_56Fe),k) = loc_bio_remin(io2l(io_Fe_56Fe),k) - &
                              & loc_r56Fe*loc_scav_Fe
                         loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),k) = loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),k) + &
                              & loc_r56Fe*loc_scav_Fe
                      end if
                   end if
                CASE ('FeFe2TL')
                   if (loc_FeFeLL(1) > const_real_nullsmall) then
                      loc_scav_Fe = fun_box_scav_Fe(          &
                           & dum_dtyr,                        &
                           & loc_bio_remin_dt_reaction,       &
                           & loc_FeFeLL(1),                   &
                           & loc_bio_part_TMP(is2l(is_POC),k) &
                           & )
                      if (sed_select(is_POM_FeOOH)) then
                         loc_bio_part_TMP(is2l(is_POM_FeOOH),k) = loc_bio_part_TMP(is2l(is_POM_FeOOH),k) + loc_scav_Fe
                         loc_bio_remin(io2l(io_Fe),k) = loc_bio_remin(io2l(io_Fe),k) - loc_scav_Fe
                         If (ocn_select(io_Fe_56Fe)) then
                            loc_bio_remin(io2l(io_Fe_56Fe),k) = loc_bio_remin(io2l(io_Fe_56Fe),k) - &
                                 & loc_r56Fe*loc_scav_Fe
                            loc_bio_part_TMP(is2l(is_POM_FeOOH_56Fe),k) = loc_bio_part_TMP(is2l(is_POM_FeOOH_56Fe),k) + &
                                 & loc_r56Fe*loc_scav_Fe
                         end if
                         ! ----------------------------------- ! MODIFY DET TRACER FLUX
                         loc_bio_part_TMP(is2l(is_det),k) = loc_bio_part_TMP(is2l(is_det),k) + loc_scav_Fe
                         ! ----------------------------------- ! record geochem diagnostics (mol kg-1)
                         loc_diag_precip(idiag_precip_FeOOH_dFe,k) = &
                              & loc_diag_precip(idiag_precip_FeOOH_dFe,k) + loc_scav_Fe
                      else
                         loc_bio_part_TMP(is2l(is_POM_Fe),k) = loc_bio_part_TMP(is2l(is_POM_Fe),k) + loc_scav_Fe
                         loc_bio_remin(io2l(io_Fe),k) = loc_bio_remin(io2l(io_Fe),k) - loc_scav_Fe
                         If (ocn_select(io_Fe_56Fe)) then
                            loc_bio_remin(io2l(io_Fe_56Fe),k) = loc_bio_remin(io2l(io_Fe_56Fe),k) - &
                                 & loc_r56Fe*loc_scav_Fe
                            loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),k) = loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),k) + &
                                 & loc_r56Fe*loc_scav_Fe
                         end if
                      end if
                   end if
                CASE ('hybrid','lookup_4D')
                   if (loc_FeFeLL(1) > const_real_nullsmall) then
                      loc_scav_Fe = fun_box_scav_Fe(          &
                           & dum_dtyr,                        &
                           & loc_bio_remin_dt_reaction,       &
                           & loc_FeFeLL(1),                   &
                           & loc_bio_part_TMP(is2l(is_POC),k) &
                           & )
                      loc_bio_part_TMP(is2l(is_POM_Fe),k) = loc_bio_part_TMP(is2l(is_POM_Fe),k) + loc_scav_Fe
                      loc_bio_remin(io2l(io_TDFe),k) = loc_bio_remin(io2l(io_TDFe),k) - loc_scav_Fe
                   end if
                CASE default
                   ! NOTHING
                end SELECT
             end if
             ! ----------------------------------------------- ! ADDITIONAL (OCEAN SURFACE) PARTICLE TRANSFORMATIONS
             ! ----------------------------------------------- ! example
             ! ### INSERT CODE ################################################################################################### !
             ! if (sed_select(is_xxx)) then
             ! end if
             ! ################################################################################################################### !
          end If

          ! >>>>>>>>>>>>>>>>>>>>>>>>>
          ! *** kk SUB-LOOP START ***
          ! >>>>>>>>>>>>>>>>>>>>>>>>>

          ! for each of the three (POC, CaCO3, and opal) primary remineralizable species (if selected),
          ! loop down remineralization column identified previously;
          ! (1) calculating the fractional remineralization in each layer, moving the particulate remainder to the layer below
          ! (2) calculate tracer remineralization from particulate supply from layer above
          ! (3) update particulate tracer field for current layer
          ! then, if the sediments are reached, calculate sediment flux
          ! NOTE: the particulate tracer field is in units of mol kg-1, following the (dissolved) ocean tracers, and as a result,
          !       corrections must be made for changes in ocean layer thickness
          do kk=k-1,loc_bio_remin_min_k,-1
             ! test to see whether the ocean bottom has been reached
             If (kk >= loc_k1) then
                ! calculate ratio of layer thicknesses
                ! (used to convert particulate matter concentrations as particulates settle through the water column
                !  comprising layers of non-uniform thickness)
                loc_bio_remin_layerratio = dum_vphys_ocn%mk(ipo_dD,kk+1)/dum_vphys_ocn%mk(ipo_dD,kk)
                loc_bio_remin_dD = dum_vphys_ocn%mk(ipo_dD,kk)
                ! calculate residence time (yr) of particulates in ocean layer (from layer thickness and sinking speed)
                ! NOTE: sinking rate has units of (m yr-1) (converted from parameter file input units)
                if (loc_bio_remin_sinkingrate_physical > const_real_nullsmall) &
                     & loc_bio_remin_dt_physical = loc_bio_remin_dD/loc_bio_remin_sinkingrate_physical
                if (loc_bio_remin_sinkingrate_reaction > const_real_nullsmall) &
                     & loc_bio_remin_dt_reaction = loc_bio_remin_dD/loc_bio_remin_sinkingrate_reaction

                ! *** Calculate fractional change in particulate fluxes ***

                ! -------------------------------------------- ! carbonate
                if (sed_select(is_CaCO3)) then
                   If (.NOT. ctrl_bio_remin_CaCO3_fixed) then
                      ! ### INSERT CODE ########################################################################################## !
                      !
                      ! ########################################################################################################## !
                   else
                      ! if both reminerilization lengths have been set to zero,
                      ! then under undersaturated conditions assume that all CaCO3 dissolves
                      ! NOTE: requires that saturation state at the grid point has already been solved for (if not updated)
                      if (par_bio_remin_CaCO3_eL1 < const_real_nullsmall .AND. par_bio_remin_CaCO3_eL2 < const_real_nullsmall) then
                         if (carb(ic_ohm_cal,dum_i,dum_j,kk) < 1.0) then
                            loc_bio_remin_CaCO3_frac1 = 1.0
                            loc_bio_remin_CaCO3_frac2 = 1.0
                         else
                            loc_bio_remin_CaCO3_frac1 = 0.0
                            loc_bio_remin_CaCO3_frac2 = 0.0
                         end if
                      else
                         loc_bio_remin_CaCO3_frac1 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_CaCO3_eL1))
                         loc_bio_remin_CaCO3_frac2 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_CaCO3_eL2))
                      end if
                   endif
                   ! calculate the ratio of particulate tracer between layers
                   loc_bio_part_CaCO3_ratio = 1.0 - &
                        & ( &
                        &   (1.0 - loc_bio_part_TMP(is2l(is_CaCO3_frac2),kk+1))*loc_bio_remin_CaCO3_frac1 + &
                        &   loc_bio_part_TMP(is2l(is_CaCO3_frac2),kk+1)*loc_bio_remin_CaCO3_frac2 &
                        & )
                   ! calculate change in partitioning between different fractions
                   l = conv_is_lselected(is_CaCO3_frac2)
                   if (loc_bio_part_TMP(l,kk+1) > const_real_nullsmall) then
                      loc_bio_part_TMP(l,kk) = &
                           & (1.0 - loc_bio_remin_CaCO3_frac2)*loc_bio_part_TMP(l,kk+1)/loc_bio_part_CaCO3_ratio
                   else
                      loc_bio_part_TMP(l,kk) = 0.0
                   end if
                end if

                ! -------------------------------------------- ! opal
                if (sed_select(is_opal)) then
                   If (.NOT. ctrl_bio_remin_opal_fixed) then
                      ! set local variables - temperature (K) and silicic acid concentration (mol kg-1)
                      loc_T     = dum_vocn%mk(conv_io_lselected(io_T),kk)
                      loc_SiO2  = dum_vocn%mk(conv_io_lselected(io_SiO2),kk)
                      ! calculate opal equilibrium H4SiO4 saturation concentration
                      loc_Si_eq = conv_umol_mol*10.0**(6.44 - 968.0/loc_T)
                      ! calculate degree of opal undersatruation
                      loc_u     = (loc_Si_eq - loc_SiO2)/loc_Si_eq
                      IF (loc_u > const_real_one)       loc_u = 1.0
                      IF (loc_u < const_real_nullsmall) loc_u = 0.0
                      ! calculate opal fractional dissolution
                      ! NOTE: for now, assume that both opal 'fractions' behave identically
                      loc_bio_remin_opal_frac1 = 1.0 - EXP(                                  &
                           & -loc_bio_remin_dt_reaction*par_bio_remin_opal_K*                &
                           & (1.0/0.71)*                                                     &
                           & (                                                               &
                           &   (0.16*(1.0 + (loc_T - const_zeroC)/15.0)*loc_u) +             &
                           &   (0.55*((1.0 + (loc_T - const_zeroC)/400.0)**4.0*loc_u)**9.25) &
                           & )                                                               &
                           & )
                      if (loc_bio_remin_opal_frac1 > const_real_one) loc_bio_remin_opal_frac1 = 1.0
                      loc_bio_remin_opal_frac2 = loc_bio_remin_opal_frac1
                   else
                      loc_bio_remin_opal_frac1 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_opal_eL1))
                      loc_bio_remin_opal_frac2 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_opal_eL2))
                   endif
                   ! calculate the ratio of particulate tracer between layers
                   loc_bio_part_opal_ratio = 1.0 - &
                        & ( &
                        &   (1.0 - loc_bio_part_TMP(is2l(is_opal_frac2),kk+1))*loc_bio_remin_opal_frac1 + &
                        &   loc_bio_part_TMP(is2l(is_opal_frac2),kk+1)*loc_bio_remin_opal_frac2 &
                        & )
                   ! calculate change in partitioning between different fractions
                   l = is2l(is_opal_frac2)
                   if (loc_bio_part_TMP(l,kk+1) > const_real_nullsmall) then
                      loc_bio_part_TMP(l,kk) = &
                           & (1.0 - loc_bio_remin_opal_frac2)*loc_bio_part_TMP(l,kk+1)/loc_bio_part_opal_ratio
                   else
                      loc_bio_part_TMP(l,kk) = 0.0
                   end if
                end if

                ! -------------------------------------------- ! particulate organic matter
                if (sed_select(is_POC)) then
                   If (.NOT. ctrl_bio_remin_POC_fixed) then
                      ! set local variables - temperature (K)
                      loc_T = dum_vocn%mk(conv_io_lselected(io_T),kk)
                      ! calculate POC fractional remin
                      loc_bio_remin_POC_frac1 = &
                           & loc_bio_remin_dt_reaction*par_bio_remin_POC_K1*exp(-par_bio_remin_POC_Ea1/(const_R_SI*loc_T))
                      loc_bio_remin_POC_frac2 = &
                           & loc_bio_remin_dt_reaction*par_bio_remin_POC_K2*exp(-par_bio_remin_POC_Ea2/(const_R_SI*loc_T))
                      ! check for an impossible >1.0 degradation fraction
                      if (loc_bio_remin_POC_frac1 >= 1.0) loc_bio_remin_POC_frac1 = 1.0
                      if (loc_bio_remin_POC_frac2 >= 1.0) loc_bio_remin_POC_frac2 = 1.0
                   else
                      ! FRACTION #1
                      select case (par_bio_remin_fun)
                      case ('Martin1987','Henson2012')
                         loc_bio_remin_POC_frac1 = 1.0 -                                                                &
                              & (                                                                                       &
                              & (phys_ocn(ipo_Dbot,dum_i,dum_j,kk)/par_bio_remin_z0)**(par_bio_remin_b(dum_i,dum_j))   &
                              & /                                                                                       &
                              & (phys_ocn(ipo_Dbot,dum_i,dum_j,kk+1)/par_bio_remin_z0)**(par_bio_remin_b(dum_i,dum_j)) &
                              & )
                      case ('KriestOschlies2008') ! efolding depth dependent on mean plankton diameter JDW
                         ! NOTE: size is in esd
                         loc_eL_size = &
                              & par_bio_remin_POC_eL0*((loc_bio_part_OLD(is2l(is_POC_size),n_k))*loc_size0)**par_bio_remin_POC_eta
                         loc_bio_remin_POC_frac1 = (1.0 - EXP(-loc_bio_remin_dD/loc_eL_size))
                      case default
                         loc_bio_remin_POC_frac1 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_POC_eL1))
                      end select
                      ! FRACTION #2
                      if (ctrl_bio_remin_POC_ballast) then
                         ! NOTE: check that there is a non-zero mineral particule load
                         ! NOTE: previously, it was checked that both the POC flux and frac are non zero
                         if (loc_part_tot_mineral > const_real_nullsmall) then
                            loc_bio_remin_POC_frac2 = 1.0 -                                                                   &
                                 & (                                                                                          &
                                 &   loc_bio_part_CaCO3_ratio*par_bio_remin_kc(dum_i,dum_j)*loc_bio_part_TMP(is_CaCO3,kk+1) + &
                                 &   loc_bio_part_opal_ratio*par_bio_remin_ko(dum_i,dum_j)*loc_bio_part_TMP(is_opal,kk+1) +   &
                                 &   1.0*par_bio_remin_kl(dum_i,dum_j)*loc_bio_part_TMP(is_det,kk+1)                          &
                                 & ) &
                                 & / &
                                 & ( &
                                 &   par_bio_remin_kc(dum_i,dum_j)*loc_bio_part_TMP(is_CaCO3,kk+1) + &
                                 &   par_bio_remin_ko(dum_i,dum_j)*loc_bio_part_TMP(is_opal,kk+1) +   &
                                 &   par_bio_remin_kl(dum_i,dum_j)*loc_bio_part_TMP(is_det,kk+1)  &
                                 & )
                         else
                            loc_bio_remin_POC_frac2 = 0.0
                         end if
                      else
                         select case (par_bio_remin_fun)
                         case ('Martin1987','Henson2012','KriestOschlies2008')
                            loc_bio_remin_POC_frac2 = 0.0
                         case default
                            loc_bio_remin_POC_frac2 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_POC_eL2))
                         end select
                      end if
                   endif
                   ! calculate the ratio of particulate tracer between layers
                   loc_bio_part_POC_ratio = 1.0 - &
                        & ( &
                        &   (1.0 - loc_bio_part_TMP(is2l(is_POC_frac2),kk+1))*loc_bio_remin_POC_frac1 + &
                        &   loc_bio_part_TMP(is2l(is_POC_frac2),kk+1)*loc_bio_remin_POC_frac2 &
                        & )
                   ! calculate change in partitioning between different fractions
                   l = is2l(is_POC_frac2)
                   if (loc_bio_part_TMP(l,kk+1) > const_real_nullsmall) then
                      loc_bio_part_TMP(l,kk) =  &
                           & (1.0 - loc_bio_remin_POC_frac2)*loc_bio_part_TMP(l,kk+1)/loc_bio_part_POC_ratio
                   else
                      loc_bio_part_TMP(l,kk) = 0.0
                   end if
                end if
                ! -------------------------------------------- ! ADDITIONAL (OCEAN SURFACE) PARTICLE TRANSFORMATIONS
                ! additional particle transformations
                ! NOTE: the layer where the particles originate is kk+1,
                !       and the current layer (where particle transformations are aplied), is kk
                ! NOTE: tracer concentrations are accessed from the vbectorised array, e.g.
                !       loc_SiO2 = dum_vocn%mk(conv_io_lselected(io_SiO2),kk)
                ! NOTE: the particle tracer concentration in layer kk, before any particle transformation, is:
                !       loc_bio_part_TMP(l,kk+1)*loc_bio_remin_layerratio
                !       (i.e., the concentration in layer kk+1, multiplied by a dilution factor (as deeper layers are thicker)
                !       where l is derived from the is particle index:
                !       l = is2l(is)
                ! NOTE: the time (years) spent by a particle in the layer (kk) is given by the variable: loc_bio_remin_dt
                ! -------------------------------------------- ! example
                ! ### INSERT CODE ################################################################################################ !
                ! if (sed_select(is_xxx)) then
                !    ! default, no dissolution (particle reduction/oxidation)
                !   loc_bio_part_xxx_ratio = 1.0
                ! end if
                ! ################################################################################################################ !

                ! *** Calculate particle concentrations in layer below ***
                ! calculate local (temporary) particulate tracer concentration;
                ! (a) take the particulate concentration in the layer above
                ! (b) modify it by the remineralization ratio to take into account dissolution loss, and
                ! (c) convert to the equivalent particulate concentration in the new (lower) layer
                ! NOTE: NO fractionation (in elemental composition or isotopic ratio) is currently assumed
                !       => additional CASE statements are required to deal with specific fractionation cases
                ! NOTE: adjust fraction of scavenged material that can be returned (par_scav_fremin)
                !       => e.g., par_scav_fremin = 1.0 will result in
                !          scavanged material being returned in the same proportion as remineralization of the scavenger
                ! NOTE: par_sed_type_misc => no remin (e.g. S bound refractory organic matter)
                DO l=1,n_l_sed
                   is = conv_iselected_is(l)
                   ! -------------------------------------- ! particulate organic matter
                   ! (plus elemental components, particle-reactive scavenged elements)
                   if ( &
                        & (sed_dep(is) == is_POC) .OR. &
                        & (sed_type(is) == par_sed_type_POM) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_POM) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_scavenged) &
                        & ) then
                      if ( (sed_type(is) == par_sed_type_scavenged) .OR. (sed_type(sed_dep(is)) == par_sed_type_scavenged) ) then
                         loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)* &
                              & loc_bio_remin_layerratio*(1.0 - par_scav_fremin*(1.0 - loc_bio_part_POC_ratio))
                      else
                         loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)* &
                              & loc_bio_remin_layerratio*loc_bio_part_POC_ratio
                      end if
                      ! -------------------------------------- ! carbonate
                      ! (plus elemental components, particle-reactive scavenged elements)
                   else if ( &
                        & (sed_dep(is) == is_CaCO3) .OR. &
                        & (sed_type(is) == par_sed_type_CaCO3) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_CaCO3) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_scavenged) &
                        & ) then
                      if ( (sed_type(is) == par_sed_type_scavenged) .OR. (sed_type(sed_dep(is)) == par_sed_type_scavenged) ) then
                         loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)* &
                              & loc_bio_remin_layerratio*(1.0 - par_scav_fremin*(1.0 - loc_bio_part_CaCO3_ratio))
                      else
                         loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)* &
                              & loc_bio_remin_layerratio*loc_bio_part_CaCO3_ratio
                      end if
                      ! -------------------------------------- ! opal
                      ! (plus elemental components, particle-reactive scavenged elements)
                   else if ( &
                        & (sed_dep(is) == is_opal) .OR. &
                        & (sed_type(is) == par_sed_type_opal) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_opal) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_scavenged) &
                        & ) then
                      if ( (sed_type(is) == par_sed_type_scavenged) .OR. (sed_type(sed_dep(is)) == par_sed_type_scavenged) ) then
                         loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)* &
                              & loc_bio_remin_layerratio*(1.0 - par_scav_fremin*(1.0 - loc_bio_part_opal_ratio))
                      else
                         loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)* &
                              & loc_bio_remin_layerratio*loc_bio_part_opal_ratio
                      end if
                      ! -------------------------------------- ! SPECIAL CASES EXAMPLE
                      ! (comes before generic detrital tracer handling)
                      ! ### INSERT CODE ############################################################################################# !
                      ! else if ( &
                      !      & (is == is_xxx) .OR. &
                      !      & (sed_dep(is) == is_xxx) &
                      !      & ) then
                      !    loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)* &
                      !         & loc_bio_remin_layerratio*loc_bio_part_xxx_ratio
                      ! ############################################################################################################# !
                      ! -------------------------------------- ! (remaining) detrital
                      ! (plus elemental components, particle-reactive scavenged elements)
                   else if ( &
                        & (sed_dep(is) == is_det) .OR. &
                        & (sed_type(is) == par_sed_type_det) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_det) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_scavenged) &
                        & ) then
                      loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)* &
                           & loc_bio_remin_layerratio
                   end if
                   ! ----------------------------------------- ! exceptions
                   ! (1) do not remineralize S in S-linked POM (assumed a refractory fraction)
                   !     i.e. just re-apply a layer ratio concentration change
                   if (is == is_POM_S) loc_bio_part_TMP(l,kk) = loc_bio_part_TMP(l,kk+1)*loc_bio_remin_layerratio

                end do

                ! -------------------------------------------- !
                ! CALCULATE INCREASE IN TRACER CONCENTRATIONS DUE TO PARTICLE REMINERALIZATION
                ! -------------------------------------------- !
                ! add 'missing' (remineralized) particulate sediment tracers to respective remineralization array components
                ! NOTE: ensure the particulate concentration in the upper layer is scaled w.r.t.
                !       the difference in relative layer thickness
                DO l=1,n_l_sed
                   if (ctrl_force_sed_reflective_POM) then
                      if (kk == loc_k1) then
                         ! set reflective boundary conditions for POM components
                         ! -> for when the oxic-only / Redfield remin provided by SEDGEM is inappropriate / OMEN-SED not selected
                         ! NOTE: sed_dep(l2is(l)) == is_POC covers C isotopes BUT needs to depdend on POC or POM)
                         !       sed_type(l2is(l)) == par_sed_type_POM covers e.g. incorporated Fe
                         !       sed_type(sed_dep(l2is(l))) == par_sed_type_POM covers isotopes of e.g. incorporated Fe
                         if ( &
                              & ( l2is(l) == is_POC ) .OR. &
                              & ( (sed_type(l2is(l)) > 10) .AND. (sed_dep(l2is(l)) == is_POC) ) .OR. &
                              & ( (sed_type(l2is(l)) > 10) .AND. (sed_dep(l2is(l)) == par_sed_type_POM) ) .OR. &
                              & ( sed_type(l2is(l)) == par_sed_type_POM ) .OR. &
                              & ( sed_type(sed_dep(l2is(l))) == par_sed_type_POM ) &
                              & ) then
                            loc_bio_part_TMP(l,kk) = 0.0
                         end if
                      end if
                   end if
                   loc_bio_part_remin(l) = (loc_bio_remin_layerratio*loc_bio_part_TMP(l,kk+1) - loc_bio_part_TMP(l,kk))
!!$                   ! create RDOM fraction
!!$                   is = conv_iselected_is(l)
!!$                   loc_tot_i = conv_POM_RDOM_i(0,is)
!!$                   do loc_i=1,loc_tot_i
!!$                      io = conv_POM_RDOM_i(loc_i,is)
!!$                      ! set POM->RDOM conversion modifier
!!$                      select case (ocn_dep(io))
!!$                      case (io_RDOM_P)
!!$                         loc_r_POM_RDOM = par_bio_red_rP_POM_RDOM
!!$                      case (io_DOM_N)
!!$                         loc_r_POM_RDOM = par_bio_red_rN_POM_RDOM
!!$                      case default
!!$                         loc_r_POM_RDOM = 1.0
!!$                      end select
!!$                      ! add RDOM tracers
!!$                      loc_bio_remin(io,kk) = loc_bio_remin(io,kk) + loc_r_POM_RDOM*par_bio_remin_RDOMfrac*loc_bio_part_remin(is)
!!$                      ! decrease particulate fraction to be remineralized
!!$                      loc_bio_part_remin(is) = (1.0 - loc_r_POM_RDOM*par_bio_remin_RDOMfrac)*loc_bio_part_remin(is)
!!$                   end do
                end DO
                ! carry out the remineralization (POM -> inorganic constitutents) itself
                ! (0) make tempoary conversion of is_POM_FeOOH -> io_FeOOH
                if (sed_select(is_POM_FeOOH) .AND. ocn_select(io_FeOOH)) then
                   loc_vocn(io2l(io_FeOOH)) = loc_bio_part_TMP(is2l(is_POM_FeOOH),kk)
                end if
                ! (1) create temporary remin conversion array depending on prevailing redox conditions
                call sub_box_remin_redfield(dum_vocn%mk(:,kk)+loc_vocn(:),loc_conv_ls_lo(:,:))
                ! (2) carry out actual remin
                !     NOTE: catch non-selected tracers (index 0 in the conversion io -> l)
                DO ls=1,n_l_sed
                   loc_tot_m = conv_ls_lo_i(0,ls)
                   do loc_m=1,loc_tot_m
                      lo = conv_ls_lo_i(loc_m,ls)
                      if (lo > 0) then
                         tmp_bio_remin = loc_conv_ls_lo(lo,ls)*loc_bio_part_remin(ls)
                         loc_bio_remin(lo,kk) = loc_bio_remin(lo,kk) + tmp_bio_remin
                         if (ctrl_bio_remin_redox_save) then
!!$                            loc_string = 'reminP_'//trim(string_sed(l2is(ls)))//'_d'//trim(string_ocn(l2io(lo)))
!!$                            id = fun_find_str_i(trim(loc_string),string_diag_redox)
                            id = conv_lslo2idP(ls,lo)
                            loc_diag_redox(id,kk) = loc_diag_redox(id,kk) + tmp_bio_remin
                         end if
                      end if
                   end do
                end DO
                ! (3) make conversion of io_FeOOH -> is_POM_FeOOH
                !     NOTE: io_Fe remin (release of Fe from FeOOH) is positive, meaning the pretend io_FeOOH remin is negative
                if (sed_select(is_POM_FeOOH) .AND. ocn_select(io_FeOOH)) then
                   if (sed_select(is_POM_FeOOH_56Fe) .AND. ocn_select(io_FeOOH_56Fe)) then
                      if (loc_bio_part_TMP(is2l(is_POM_FeOOH),kk) > const_rns) then
                         loc_r56Fe = loc_bio_part_TMP(is2l(is_POM_FeOOH_56Fe),kk)/loc_bio_part_TMP(is2l(is_POM_FeOOH),kk)
                      else
                         loc_r56Fe = 0.0
                      end if
                      loc_alpha = 1.0 + par_d56Fe_Corg_FeOOH_epsilon/1000.0
                      loc_R     = loc_r56Fe/(1.0 - loc_r56Fe)
                      loc_bio_part_TMP(is2l(is_POM_FeOOH_56Fe),kk) = loc_bio_part_TMP(is2l(is_POM_FeOOH_56Fe),kk) + &
                           & loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)*loc_bio_remin(io2l(io_FeOOH),kk)
                      loc_bio_remin(io2l(io_Fe2_56Fe),kk) = loc_bio_remin(io2l(io_Fe2_56Fe),kk) - &
                           & loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)*loc_bio_remin(io2l(io_FeOOH),kk)
                   end if
                   loc_bio_part_TMP(is2l(is_POM_FeOOH),kk) = &
                        & loc_bio_part_TMP(is2l(is_POM_FeOOH),kk) + loc_bio_remin(io2l(io_FeOOH),kk)
                   loc_bio_remin(io2l(io_FeOOH),kk) = 0.0
                end if
                ! *** Scavenge Fe from water column ***
                ! NOTE: Fe scavenging must be called AFTER particulates have been 'moved' to the next layer down
                !       - they are assumed to start at the BASE of the originating layer,
                !         which is why they are not scavenged from level (kk+1)
                ! NOTE: calculate Fe speciation first!
                ! NOTE: for the lookup table, the goethite concentration is assigned to the first array index
                !       (otherwise used for [Fe])
                ! NOTE: in the re-balancing of Fe-FeL-L (scheme 'ALT') -- this can only occur once in each layer per time-step
                !       (as particlaes cannot over take themselves ...
                !        except in the special case of non instantaneous remin and particle-size dependent sinking ...)
                if (ocn_select(io_Fe) .OR. ocn_select(io_TDFe)) then
                   loc_r56Fe = 0.0
                   ! calculate Fe speciation
                   SELECT CASE (trim(opt_geochem_Fe))
                   case ('OLD')
                      loc_FeFeLL(1) = dum_vocn%mk(io2l(io_Fe),kk)
                   CASE ('ALT')
                      loc_FeFeLL(:) = fun_box_calc_geochem_Fe(                                  &
                           & dum_vocn%mk(io2l(io_Fe),kk) + dum_vocn%mk(io2l(io_FeL),kk), &
                           & dum_vocn%mk(io2l(io_L),kk) + dum_vocn%mk(io2l(io_FeL),kk),   &
                           & par_K_FeL &
                           & )
                      ! re-partition Fe species
                      loc_bio_remin(io2l(io_Fe),kk)  = loc_bio_remin(io2l(io_Fe),kk)  + &
                           & (loc_FeFeLL(1) - dum_vocn%mk(io2l(io_Fe),kk))
                      loc_bio_remin(io2l(io_FeL),kk) = loc_bio_remin(io2l(io_FeL),kk) + &
                           & (loc_FeFeLL(2) - dum_vocn%mk(io2l(io_FeL),kk))
                      loc_bio_remin(io2l(io_L),kk)   = loc_bio_remin(io2l(io_L),kk)   + &
                           & (loc_FeFeLL(3) - dum_vocn%mk(io2l(io_L),kk))
                   CASE ('FeFe2TL')
                      loc_FeFeLL(:) = fun_box_calc_geochem_Fe( &
                           & dum_vocn%mk(io2l(io_Fe),kk), &
                           & dum_vocn%mk(io2l(io_TL),kk), &
                           & par_K_FeL &
                           & )
                   CASE ('hybrid')
                      loc_FeFeLL(:) = fun_box_calc_geochem_Fe( &
                           & dum_vocn%mk(io2l(io_TDFe),kk), &
                           & dum_vocn%mk(io2l(io_TL),kk), &
                           & par_K_FeL &
                           & )
                   CASE ('lookup_4D')
                      loc_FeFeLL(1) = fun_box_calc_lookup_Fe_4D_geo( &
                           & (/ ocn(io_T,dum_i,dum_j,kk), carb(ic_H,dum_i,dum_j,kk), &
                           &  ocn(io_TDFe,dum_i,dum_j,kk), ocn(io_TL,dum_i,dum_j,kk) /) &
                           & )
                   case default
                      loc_FeFeLL(:) = 0.0
                   end SELECT
                   ! calculate mean Fe isotopic composition
                   ! NOTE: isotopes not enabled for all of the Fe schemes
                   If ( ocn_select(io_Fe_56Fe) .OR. ocn_select(io_TDFe_56Fe) ) then
                      SELECT CASE (trim(opt_geochem_Fe))
                      CASE ('OLD','ALT')
                         ! re-partition (homogonise) Fe isotopes across Fe and FeL pools
                         if ( (loc_FeFeLL(1)+loc_FeFeLL(2)) > const_rns ) then
                            loc_r56Fe = (dum_vocn%mk(io2l(io_Fe_56Fe),kk) + dum_vocn%mk(io2l(io_FeL_56Fe),kk))/ &
                                 & (loc_FeFeLL(1) + loc_FeFeLL(2))
                         else
                            loc_r56Fe = 0.0
                         end if
                         loc_bio_remin(io2l(io_Fe_56Fe),kk)  = loc_bio_remin(io2l(io_Fe_56Fe),kk)  + &
                              & (loc_r56Fe*loc_FeFeLL(1) - dum_vocn%mk(io2l(io_Fe_56Fe),kk))
                         loc_bio_remin(io2l(io_FeL_56Fe),kk) = loc_bio_remin(io2l(io_FeL_56Fe),kk) + &
                              & (loc_r56Fe*loc_FeFeLL(2) - dum_vocn%mk(io2l(io_FeL_56Fe),kk))
                      CASE ('FeFe2TL')
                         ! assume single total pool (Fe3) ... no need to homogenize ...
                         if (dum_vocn%mk(io2l(io_Fe),kk) > const_rns) then
                            loc_r56Fe = dum_vocn%mk(io2l(io_Fe_56Fe),kk)/dum_vocn%mk(io2l(io_Fe),kk)
                         else
                            loc_r56Fe = 0.0
                         end if
                      CASE ('hybrid','lookup_4D')
                         ! single total pool ... no need to homogenize ...
                         if (dum_vocn%mk(io2l(io_TDFe),kk) > const_rns) then
                            loc_r56Fe = dum_vocn%mk(io2l(io_TDFe_56Fe),kk)/dum_vocn%mk(io2l(io_TDFe),kk)
                         else
                            loc_r56Fe = 0.0
                         end if
                      case default
                         ! NOTHING!
                      end SELECT
                   end if
                   ! calculate scavenging (and isotopes)
                   ! NOTE: isotopes not enabled for all of the Fe schemes
                   SELECT CASE (trim(opt_geochem_Fe))
                   CASE ('OLD','ALT')
                      if (loc_FeFeLL(1) > const_real_nullsmall) then
                         loc_scav_Fe = fun_box_scav_Fe(           &
                              & dum_dtyr,                         &
                              & loc_bio_remin_dt_reaction,        &
                              & loc_FeFeLL(1),                    &
                              & loc_bio_part_TMP(is2l(is_POC),kk) &
                              & )
                         loc_bio_remin(io2l(io_Fe),kk) = loc_bio_remin(io2l(io_Fe),kk) - loc_scav_Fe
                         loc_bio_part_TMP(is2l(is_POM_Fe),kk) = loc_bio_part_TMP(is2l(is_POM_Fe),kk) + loc_scav_Fe
                         If (ocn_select(io_Fe_56Fe)) then
                            loc_bio_remin(io2l(io_Fe_56Fe),kk) = loc_bio_remin(io2l(io_Fe_56Fe),kk) - &
                                 & loc_r56Fe*loc_scav_Fe
                            loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),kk) = loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),kk) + &
                                 & loc_r56Fe*loc_scav_Fe
                         end if
                      end if
                   CASE ('FeFe2TL')
                      if (loc_FeFeLL(1) > const_real_nullsmall) then
                         loc_scav_Fe = fun_box_scav_Fe(           &
                              & dum_dtyr,                         &
                              & loc_bio_remin_dt_reaction,        &
                              & loc_FeFeLL(1),                    &
                              & loc_bio_part_TMP(is2l(is_POC),kk) &
                              & )
                         if (sed_select(is_POM_FeOOH)) then
                            loc_bio_remin(io2l(io_Fe),kk) = loc_bio_remin(io2l(io_Fe),kk) - loc_scav_Fe
                            loc_bio_part_TMP(is2l(is_POM_FeOOH),kk) = loc_bio_part_TMP(is2l(is_POM_FeOOH),kk) + loc_scav_Fe
                            If (ocn_select(io_Fe_56Fe)) then
                               loc_bio_remin(io2l(io_Fe_56Fe),kk) = loc_bio_remin(io2l(io_Fe_56Fe),kk) - loc_r56Fe*loc_scav_Fe
                               loc_bio_part_TMP(is2l(is_POM_FeOOH_56Fe),kk)  = loc_bio_part_TMP(is2l(is_POM_FeOOH_56Fe),kk) + &
                                    & loc_r56Fe*loc_scav_Fe
                            end if
                            ! ----------------------------------- ! MODIFY DET TRACER FLUX
                            loc_bio_part_TMP(is2l(is_det),kk) = loc_bio_part_TMP(is2l(is_det),kk) + loc_scav_Fe
                            ! ----------------------------------- ! record geochem diagnostics (mol kg-1)
                            loc_diag_precip(idiag_precip_FeOOH_dFe,kk) = &
                                 & loc_diag_precip(idiag_precip_FeOOH_dFe,kk) + loc_scav_Fe
                         else
                            loc_bio_remin(io2l(io_Fe),kk) = loc_bio_remin(io2l(io_Fe),kk) - loc_scav_Fe
                            loc_bio_part_TMP(is2l(is_POM_Fe),kk) = loc_bio_part_TMP(is2l(is_POM_Fe),kk) + loc_scav_Fe
                            If (ocn_select(io_Fe_56Fe)) then
                               loc_bio_remin(io2l(io_Fe_56Fe),kk) = loc_bio_remin(io2l(io_Fe_56Fe),kk) - &
                                    & loc_r56Fe*loc_scav_Fe
                               loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),kk) = loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),kk) + &
                                    & loc_r56Fe*loc_scav_Fe
                            end if
                         end if
                      end if
                   CASE ('hybrid','lookup_4D')
                      if (loc_FeFeLL(1) > const_real_nullsmall) then
                         loc_scav_Fe = fun_box_scav_Fe(           &
                              & dum_dtyr,                         &
                              & loc_bio_remin_dt_reaction,        &
                              & loc_FeFeLL(1),                    &
                              & loc_bio_part_TMP(is2l(is_POC),kk) &
                              & )
                         loc_bio_remin(io2l(io_TDFe),kk) = loc_bio_remin(io2l(io_TDFe),kk) - loc_scav_Fe
                         loc_bio_part_TMP(is2l(is_POM_Fe),kk) = loc_bio_part_TMP(is2l(is_POM_Fe),kk) + loc_scav_Fe
                         If (ocn_select(io_TDFe_56Fe)) then
                            loc_bio_remin(io2l(io_TDFe_56Fe),kk) = loc_bio_remin(io2l(io_TDFe_56Fe),kk) - &
                                 & loc_r56Fe*loc_scav_Fe
                            loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),kk) = loc_bio_part_TMP(is2l(is_POM_Fe_56Fe),kk) + &
                                 & loc_r56Fe*loc_scav_Fe
                         end if
                      end if
                   case default
                      ! NOTHING!
                   end SELECT
                end if

                ! *** Scavenge H2S from water column ***
                if (ocn_select(io_H2S) .AND. sed_select(is_POM_S)) then
                   if (dum_vocn%mk(io2l(io_H2S),kk)>const_rns) then
                      call sub_box_scav_H2S(               &
                           & dum_i,dum_j,kk,               &
                           & dum_dtyr,                     &
                           & loc_bio_remin_dt_reaction,    &
                           & dum_vocn%mk(io2l(io_H2S),kk), &
                           & loc_bio_part_TMP(:,kk),       &
                           & loc_bio_remin(:,kk)           &
                           & )
                   end if
                end if

                ! *** Scavenge Os from water column ***
                if ((ocn_select(io_Os) .AND. sed_select(is_POM_Os)) .AND. (par_bio_remin_kOstoPOMOS > const_real_nullsmall)) then
                   if (dum_vocn%mk(io2l(io_Os),kk) > const_real_nullsmall) then
                      call sub_box_scav_Os(                     &
                           & dum_dtyr,                          &
                           & loc_bio_remin_dt_reaction,         &
                           & dum_vocn%mk(io2l(io_Os),kk),       &
                           & dum_vocn%mk(io2l(io_Os_187Os),kk), &
                           & dum_vocn%mk(io2l(io_Os_188Os),kk), &
                           & dum_vocn%mk(io2l(io_O2),kk),       &
                           & loc_bio_part_TMP(:,kk),            &
                           & loc_bio_remin(:,kk)                &
                           & )
                   end if
                end if

                ! *** Reduce FeOOH ***
                ! NOTE: test for both forms (scavenged and 'free')
                ! NOTE: ensure io_H2S_34S is selected ... or pass a zero (that is not used in the subroutine) for 34S (H2S)
                !       (otherwise ... io2l(io_H2S) when io_H2S has not bee selected is index 0)
                if (ocn_select(io_H2S) .AND. ocn_select(io_Fe2)) then
                   if (ocn_select(io_H2S_34S)) then
                      loc_H2S_34S = dum_vocn%mk(io2l(io_H2S_34S),kk)
                   else
                      loc_H2S_34S = 0.0
                   end if
                   if (sed_select(is_FeOOH)) then
                      if (dum_vocn%mk(io2l(io_H2S),kk)>const_rns .AND. loc_bio_part_TMP(is2l(is_FeOOH),kk)>const_rns) then
                         call sub_box_react_FeOOH_H2S(        &
                              & dum_i,dum_j,kk,               &
                              & dum_dtyr,                     &
                              & loc_bio_remin_dt_reaction,    &
                              & dum_vocn%mk(io2l(io_H2S),kk), &
                              & loc_H2S_34S,                  &
                              & loc_bio_part_TMP(:,kk),       &
                              & loc_bio_remin(:,kk)           &
                              & )
                      end if
                   elseif (sed_select(is_POM_FeOOH)) then
                      if (dum_vocn%mk(io2l(io_H2S),kk)>const_rns .AND. loc_bio_part_TMP(is2l(is_POM_FeOOH),kk)>const_rns) then
                         call sub_box_react_POMFeOOH_H2S(     &
                              & dum_i,dum_j,kk,               &
                              & dum_dtyr,                     &
                              & loc_bio_remin_dt_reaction,    &
                              & dum_vocn%mk(io2l(io_H2S),kk), &
                              & loc_H2S_34S,                  &
                              & loc_bio_part_TMP(:,kk),       &
                              & loc_bio_remin(:,kk)           &
                              & )
                      end if
                   end if
                end if

             end If
          end do

          ! <<<<<<<<<<<<<<<<<<<<<<<
          ! *** kk SUB-LOOP END ***
          ! <<<<<<<<<<<<<<<<<<<<<<<

          ! *** UPDATE PARTICULATE MATTER INFORMATION ***
          ! update local ocean particulate tracer field - store residual particulate tracer at the point of
          ! the deepest level reached
          ! NOTE: do not store if the sediment surface is reached
          ! NOTE: to be correct, the conc for par_sed_type_frac needs to be flux-weighted when summed ...
          If (loc_bio_remin_min_k >= loc_k1) then
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                SELECT CASE (sed_type(is))
                case (par_sed_type_frac)
                   loc_bio_part(l,loc_bio_remin_min_k) = loc_bio_part_TMP(l,loc_bio_remin_min_k)
                case default
                   loc_bio_part(l,loc_bio_remin_min_k) = loc_bio_part(l,loc_bio_remin_min_k) + &
                        & loc_bio_part_TMP(l,loc_bio_remin_min_k)
                end SELECT
             end do
          end if
          ! record particulate fluxes at base of each layer (units of: mol per time-step)
          ! NOTE: implicitly includes sedimentation flux (kk=dum_k1)
          ! NOTE: to be correct, the flux for par_sed_type_frac needs to be flux-weighted when summed ...
          !       for now, just scaled by the time-step (so it is integrated properly later)
          do kk=k,loc_bio_remin_min_k+1,-1
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                SELECT CASE (sed_type(is))
                case (par_sed_type_frac)
                   loc_bio_settle(l,kk) = dum_dtyr*loc_bio_part_TMP(l,kk)
                case default
                   loc_bio_settle(l,kk) = loc_bio_settle(l,kk) + dum_vphys_ocn%mk(ipo_M,kk)*loc_bio_part_TMP(l,kk)
                end SELECT
             end do
          end do

       end If

    end do

    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ! *** k WATER-COLUMN LOOP END ***
    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    ! *** WRITE GLOBAL ARRAY DATA ***
    ! NOTE: because sub_calc_bio_remin is the first call in the sequence of events (in biogem_main),
    !       data arrays are over-written rather than incremented
    ! remin diagnostics
    if (ctrl_bio_remin_redox_save) then
       diag_redox(:,dum_i,dum_j,:)  = diag_redox(:,dum_i,dum_j,:)  + loc_diag_redox(:,:)
       diag_precip(:,dum_i,dum_j,:) = diag_precip(:,dum_i,dum_j,:) + loc_diag_precip(:,:)
       if (ocn_select(io_col9)) then
!!$          loc_string = 'reminP_'//trim(string_sed(is_POC))//'_d'//trim(string_ocn(io_DIC))
!!$          id = fun_find_str_i(trim(loc_string),string_diag_redox)
          id = conv_lslo2idP(is2l(is_POC),io2l(io_DIC))
          loc_bio_remin(io2l(io_col9),:) = loc_bio_remin(io2l(io_col9),:) + loc_diag_redox(id,:)
       end if
       if (ocn_select(io_col8) .AND. (.NOT. ocn_select(io_DIC_14C)))then
!!$          loc_string = 'reminP_'//trim(string_sed(is_POC_13C))//'_d'//trim(string_ocn(io_DIC_13C))
!!$          id = fun_find_str_i(trim(loc_string),string_diag_redox)
          id = conv_lslo2idP(is2l(is_POC_13C),io2l(io_DIC_13C))
          loc_bio_remin(io2l(io_col8),:) = loc_bio_remin(io2l(io_col8),:) + loc_diag_redox(id,:)
       end if
    end if
    ! record settling fluxes
    ! NOTE: units of mol per time-step
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       bio_settle(is,dum_i,dum_j,:) = loc_bio_settle(l,:)
    end do
    ! write ocean tracer field and settling flux arrays (global array)
    dum_vbio_part%mk(:,:) = loc_bio_part(:,:)
    ! write ocean tracer remineralization field (global array)
    dum_vbio_remin%mk(:,:) = dum_vbio_remin%mk(:,:) + loc_bio_remin(:,:)
!!$    ! deallocate local arrays
!!$    DEALLOCATE(loc_diag_redox,STAT=alloc_error)

  END SUBROUTINE sub_box_remin_part
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate FeOOH dissolution (reaction with H2S)
  ! NOTE: calling of this sub is conditional on both H2S and FeOOH not being zero
  !       (so divide-by-zero issues should already have be screened for ...)
  SUBROUTINE sub_box_react_FeOOH_H2S( &
       & dum_i,dum_j,dum_k,dum_dtyr,dum_dt_scav,dum_ocn_H2S,dum_ocn_H2S_34S,dum_bio_part,dum_bio_remin &
       & )
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k
    REAL,INTENT(in)::dum_dtyr
    REAL,INTENT(in)::dum_dt_scav
    REAL,INTENT(in)::dum_ocn_H2S,dum_ocn_H2S_34S
    real,dimension(n_l_sed),INTENT(inout)::dum_bio_part
    real,dimension(n_l_ocn),INTENT(inout)::dum_bio_remin
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_H2S,loc_H2S_34S,loc_r34S,loc_R_34S
    real::loc_r56Fe,loc_R_56Fe
    real::loc_part_den_FeOOH
    real::loc_dFeOOH
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- ! set local solutes
    loc_H2S = dum_ocn_H2S
    loc_H2S_34S = dum_ocn_H2S_34S
    ! -------------------------------------------------------- ! extract density of FeOOH
    loc_part_den_FeOOH = dum_bio_part(is2l(is_FeOOH))
    ! -------------------------------------------------------- !
    ! CALCULATE FeOOH dissolution rate
    ! -------------------------------------------------------- !
    ! reaction:
    !           FeOOH + 1/8H2S --> 1/8SO4 + Fe2
    ! and where FeOOH comes from:
    !           Fe2 + 1/4O2  --> Fe3
    !           Fe3+ + 2*H2O --> FeOOH + 3*H+
    ! or, overall, ignoring water etc:
    !           Fe2 + 1/4O2 --> FeOOH
    ! NOTE: par_bio_remin_kFeOOHtoFe2 == kinetic constant for FeOOH reduction using sulphide in M-1 yr-1
    ! -------------------------------------------------------- ! calculate reaction rate
    loc_dFeOOH = dum_dt_scav*par_bio_remin_kFeOOHtoFe2*loc_part_den_FeOOH*(loc_H2S**(1.0/2.0))
    ! cap scavenged POM_FeOOH and dissolved H2S consumption
    loc_dFeOOH = min(loc_dFeOOH,loc_f*loc_part_den_FeOOH,loc_f*(8.0/1.0)*loc_H2S)
    ! -------------------------------------------------------- ! implement reaction
    dum_bio_part(is2l(is_FeOOH)) = dum_bio_part(is2l(is_FeOOH)) - loc_dFeOOH
    dum_bio_remin(io2l(io_H2S))  = dum_bio_remin(io2l(io_H2S))  - (1.0/8.0)*loc_dFeOOH
    dum_bio_remin(io2l(io_Fe2))  = dum_bio_remin(io2l(io_Fe2))  + loc_dFeOOH
    dum_bio_remin(io2l(io_SO4))  = dum_bio_remin(io2l(io_SO4))  + (1.0/8.0)*loc_dFeOOH
    dum_bio_remin(io2l(io_ALK))  = dum_bio_remin(io2l(io_ALK))  - 2.0*(1.0/8.0)*loc_dFeOOH
    ! -------------------------------------------------------- ! implement reaction -- isotopes
    ! NOTE: only explicitly test for 2 isotope tracers selected (4 total)
    ! NOTE: no fractionation (currently)
    if (ocn_select(io_Fe2_56Fe)) then
       loc_r56Fe = dum_bio_part(is2l(is_FeOOH_56Fe))/loc_part_den_FeOOH
       loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe) 
       dum_bio_remin(io2l(io_Fe2_56Fe)) = dum_bio_remin(io2l(io_Fe2_56Fe)) + &
            & par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_dFeOOH
       dum_bio_part(is2l(is_FeOOH_56Fe)) = dum_bio_part(is2l(is_FeOOH_56Fe)) - &
            & par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_dFeOOH
    end if
    if (ocn_select(io_H2S_34S)) then
       loc_r34S = loc_H2S_34S/loc_H2S
       loc_R_34S = loc_r34S/(1.0 - loc_r34S) 
       dum_bio_remin(io2l(io_H2S_34S)) = dum_bio_remin(io2l(io_H2S_34S)) - &
            & par_d34S_ISO_alpha*loc_R_34S/(1.0 + par_d34S_ISO_alpha*loc_R_34S)*(1.0/8.0)*loc_dFeOOH
       dum_bio_remin(io2l(io_SO4_34S)) = dum_bio_remin(io2l(io_SO4_34S)) + &
            & par_d34S_ISO_alpha*loc_R_34S/(1.0 + par_d34S_ISO_alpha*loc_R_34S)*(1.0/8.0)*loc_dFeOOH
    end if
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record geochem diagnostics (mol kg-1)
    diag_react(idiag_react_FeOOH_dFe2,dum_i,dum_j,dum_k) = diag_react(idiag_react_FeOOH_dFe2,dum_i,dum_j,dum_k) + &
         & loc_dFeOOH
    diag_react(idiag_react_FeOOH_dH2S,dum_i,dum_j,dum_k) = diag_react(idiag_react_FeOOH_dH2S,dum_i,dum_j,dum_k) - &
         & (1.0/8.0)*loc_dFeOOH
    diag_react(idiag_react_FeOOH_dSO4,dum_i,dum_j,dum_k) = diag_react(idiag_react_FeOOH_dSO4,dum_i,dum_j,dum_k) + &
         & (1.0/8.0)*loc_dFeOOH
    diag_react(idiag_react_FeOOH_dALK,dum_i,dum_j,dum_k) = diag_react(idiag_react_FeOOH_dALK,dum_i,dum_j,dum_k) - &
         & 2.0*(1.0/8.0)*loc_dFeOOH
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_react_FeOOH_H2S
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate FeOOH dissolution (reaction with H2S) for 'scavenged' FeOOH (POM_FeOOH)
  ! NOTE: calling of this sub is conditional on both H2S and FeOOH not being zero
  !       (so divide-by-zero issues should already have be screened for ...)
  ! NOTE: for now, this is simply an edited copy of sub_box_react_FeOOH_H2S
  !       -> a cleaner solution (not involving duplicating code) should be possible and implemented ... sometime ...
  SUBROUTINE sub_box_react_POMFeOOH_H2S( &
       & dum_i,dum_j,dum_k,dum_dtyr,dum_dt_scav,dum_ocn_H2S,dum_ocn_H2S_34S,dum_bio_part,dum_bio_remin &
       & )
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k
    REAL,INTENT(in)::dum_dtyr
    REAL,INTENT(in)::dum_dt_scav
    REAL,INTENT(in)::dum_ocn_H2S,dum_ocn_H2S_34S
    real,dimension(n_l_sed),INTENT(inout)::dum_bio_part
    real,dimension(n_l_ocn),INTENT(inout)::dum_bio_remin
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_H2S,loc_H2S_34S,loc_R_34S,loc_r34S
    real::loc_r56Fe,loc_R_56Fe
    real::loc_part_den_FeOOH
    real::loc_dFeOOH
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- ! set local solutes
    loc_H2S     = dum_ocn_H2S
    loc_H2S_34S = dum_ocn_H2S_34S
    ! -------------------------------------------------------- ! extract density of FeOOH
    loc_part_den_FeOOH = dum_bio_part(is2l(is_POM_FeOOH))
    ! -------------------------------------------------------- !
    ! CALCULATE FeOOH dissolution rate
    ! -------------------------------------------------------- !
    ! reaction:
    !           FeOOH + 1/8H2S --> 1/8SO4 + Fe2
    ! and where FeOOH comes from:
    !           Fe2 + 1/4O2  --> Fe3
    !           Fe3+ + 2*H2O --> FeOOH + 3*H+
    ! or, overall, ignoring water etc:
    !           Fe2 + 1/4O2 --> FeOOH
    ! NOTE: par_bio_remin_kFeOOHtoFe2 == kinetic constant for FeOOH reduction using sulphide in M-1 yr-1
    ! -------------------------------------------------------- ! calculate reaction rate
    loc_dFeOOH = dum_dt_scav*par_bio_remin_kFeOOHtoFe2*loc_part_den_FeOOH*(loc_H2S**(1.0/2.0))
    ! cap scavenged POM_FeOOH and dissolved H2S consumption
    loc_dFeOOH = min(loc_dFeOOH,loc_f*loc_part_den_FeOOH,loc_f*(8.0/1.0)*loc_H2S)
    ! -------------------------------------------------------- ! implement reaction
    dum_bio_part(is2l(is_POM_FeOOH)) = dum_bio_part(is2l(is_POM_FeOOH)) - loc_dFeOOH
    dum_bio_remin(io2l(io_H2S))      = dum_bio_remin(io2l(io_H2S))  - (1.0/8.0)*loc_dFeOOH
    dum_bio_remin(io2l(io_Fe2))      = dum_bio_remin(io2l(io_Fe2))  + loc_dFeOOH
    dum_bio_remin(io2l(io_SO4))      = dum_bio_remin(io2l(io_SO4))  + (1.0/8.0)*loc_dFeOOH
    dum_bio_remin(io2l(io_ALK))      = dum_bio_remin(io2l(io_ALK))  - 2.0*(1.0/8.0)*loc_dFeOOH
    ! -------------------------------------------------------- ! implement reaction -- isotopes
    ! NOTE: only explicitly test for 2 isotope tracers selected (4 total)
    ! NOTE: no fractionation (currently)
    if (ocn_select(io_Fe2_56Fe)) then
       loc_r56Fe  = dum_bio_part(is2l(is_POM_FeOOH_56Fe))/loc_part_den_FeOOH
       loc_R_56Fe = loc_r56Fe/(1.0 - loc_r56Fe) 
       dum_bio_remin(io2l(io_Fe2_56Fe)) = dum_bio_remin(io2l(io_Fe2_56Fe)) + &
            & par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_dFeOOH
       dum_bio_part(is2l(is_POM_FeOOH_56Fe)) = dum_bio_part(is2l(is_POM_FeOOH_56Fe)) - &
            & par_d56Fe_Fered_alpha*loc_R_56Fe/(1.0 + par_d56Fe_Fered_alpha*loc_R_56Fe)*loc_dFeOOH
    end if
    if (ocn_select(io_H2S_34S)) then
       loc_r34S  = loc_H2S_34S/loc_H2S
       loc_R_34S = loc_r34S/(1.0 - loc_r34S) 
       dum_bio_remin(io2l(io_H2S_34S)) = dum_bio_remin(io2l(io_H2S_34S)) - &
            & par_d34S_ISO_alpha*loc_R_34S/(1.0 + par_d34S_ISO_alpha*loc_R_34S)*(1.0/8.0)*loc_dFeOOH
       dum_bio_remin(io2l(io_SO4_34S)) = dum_bio_remin(io2l(io_SO4_34S)) + &
            & par_d34S_ISO_alpha*loc_R_34S/(1.0 + par_d34S_ISO_alpha*loc_R_34S)*(1.0/8.0)*loc_dFeOOH
    end if
    ! -------------------------------------------------------- !
    ! DIAGNOSTICS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! record geochem diagnostics (mol kg-1)
    diag_react(idiag_react_POMFeOOH_dFe2,dum_i,dum_j,dum_k) = diag_react(idiag_react_POMFeOOH_dFe2,dum_i,dum_j,dum_k) + &
         & loc_dFeOOH
    diag_react(idiag_react_POMFeOOH_dH2S,dum_i,dum_j,dum_k) = diag_react(idiag_react_POMFeOOH_dH2S,dum_i,dum_j,dum_k) - &
         & (1.0/8.0)*loc_dFeOOH
    diag_react(idiag_react_POMFeOOH_dSO4,dum_i,dum_j,dum_k) = diag_react(idiag_react_POMFeOOH_dSO4,dum_i,dum_j,dum_k) + &
         & (1.0/8.0)*loc_dFeOOH
    diag_react(idiag_react_POMFeOOH_dALK,dum_i,dum_j,dum_k) = diag_react(idiag_react_POMFeOOH_dALK,dum_i,dum_j,dum_k) - &
         & 2.0*(1.0/8.0)*loc_dFeOOH
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_react_POMFeOOH_H2S
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SCAVENGING
  ! consider:
  !           (i)   with no remin, the concentration of particles should be uniform throughout the water column
  !           (ii)  however, in order to have explicit particle concentrations in the water column and transform to dissolved,
  !                 concentrations are reduced as the layer thickness increase (hence concerving total mass)
  !           (iii) this reduced effective layer concentration scales with a longer residence time of the particles in the layer
  !                 (loc_bio_remin_dt_reaction)
  !           (iv)  if the reaction involves dtime*concentration, this is the same (as both change with layer thickness ratio)
  !           (v)   however ... if the particle concentration is to a non unity power (e.g. for Fe),
  !                 then the concentration needs to be corrected and the time-step rather than the residence time used
  !
  ! worked:   (a)   surface ocean production is x0 mol m-3 over time-step dt0 (yr) [converting from mol kg-1]
  !                 the surface layer thickness is dl0 (m), giving an export flux of x0*dl0/dt0 mol m-2 yr-1
  !                 for a settling rate of u m yr-1, the particule density is then x0*dl0/dt0/u (mol m-3)
  !           (b)   local particle density (with no remin) is x = x0 * dl0/dl (scaled by the relative ocean layer thickness)
  !                 => local flux = x/(dl0/dl)*dl0/dt0 = x*dl/dt0
  !                 => local density = x*dl/dt0/u where dl/u == local residence time, dt
  !                                  = x*dt/dt0 (mol kg-1)
  ! hence ...
  !                 effecively, what we are saying is that the particle density for reaction is equal to
  !                 the model-represented layer density, x, corrected by the residence time (dt) divided by the time-step (dt0)
  !                 because the reaction occurs over time dt0, we have dt0*(dt/dt0) = dt
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate Fe scavenging
  ! *** NEW TRACER NOTATION ***
  ! NOTE: pass in [TDFe] rather than [Fe] if using new Fe scheme
  ! NOTE: allow remin array for [Fe] to be written to, but correct later (to [TDFe]) outside of subroutine
  ! NOTE: revised for a single generic particle and scavenging
  ! NOTE: particle field to be passed in, in units of mol kg-1
  function fun_box_scav_Fe(dum_dtyr,dum_dt_scav,dum_ocn_Fe,dum_PART)
    ! -------------------------------------------------------- !
    ! RESULT VARIABLE
    ! -------------------------------------------------------- !
    REAL::fun_box_scav_Fe
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    REAL,INTENT(in)::dum_dtyr
    REAL,INTENT(in)::dum_dt_scav
    REAL,INTENT(in)::dum_ocn_Fe
    real,intent(in)::dum_PART
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_scav_dFe
    real::loc_part_den
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE RESULT VARIABLE
    ! -------------------------------------------------------- !
    fun_box_scav_Fe = 0.0
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- !
    ! CALCULATE Fe SCAVENGING
    ! -------------------------------------------------------- !
    ! NOTE: residence time in each ocean layer must be used in the Parekh et al. [2005] model
    ! NOTE: Dutkiewicz et al. [2005] scavenging rate par_scav_Fe_Ks has been converted to units of (yr-1)
    ! NOTE: the scavenging model of Parekh et al. [2005] follows Honeyman et al. [1988]
    !       in this, the scavenging rate (day-1) is equal to tau*Rf(Cp)**phi, where Rf = 0.0791 l mg-1 day-1 and phi = 0.58
    !       becasue Cp is in units of mg l-1, dum_PART must be converted from mol kg-1 == conv_g_mg*conv_POC_mol_g/conv_kg_l
    !       par_scav_Fe_sf_POC scales for Fe scavening vs. Th and is a tuned parameter ... (value 0.2 in Parekh et al. [2005])
    if (dum_PART > const_real_nullsmall) then
       SELECT CASE (trim(opt_bio_Fe_scav))
       CASE ('Dutkiewicz')
          ! -------------------------------------------------- ! calculate scavenging following Dutkiewicz et al. [2005]
          ! NOTE: here we just use the time-step as part conc is not involved
          loc_scav_dFe = dum_dtyr*par_scav_Fe_k0*dum_ocn_Fe
       CASE ('Parekh')
          ! -------------------------------------------------- ! calculate scavenging following Parekh et al. [2005]
          loc_part_den = (conv_g_mg*conv_POC_mol_g*dum_PART/conv_kg_l) * dum_dt_scav/dum_dtyr
          loc_scav_dFe = dum_dtyr*par_scav_Fe_sf_POC*par_scav_Fe_k0*(loc_part_den**par_scav_Fe_exp)*dum_ocn_Fe
       case default
          ! -------------------------------------------------- ! remove all Fe (limited by reactant depletion rule later)
          loc_scav_dFe = dum_ocn_Fe
       end select
       ! ----------------------------------------------------- ! set output [Fe]
       ! cap Fe scavenging at maximum of available Fe
       fun_box_scav_Fe = MIN(loc_scav_dFe,loc_f*dum_ocn_Fe)
       ! ----------------------------------------------------- !
    end if
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end function fun_box_scav_Fe
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate Fe scavenging
  ! *** OLD TRACER NOTATION ***
  ! NOTE: not upgraded for either is_FeOOH nor 56Fe
  SUBROUTINE sub_calc_scav_Fe(dum_dtyr,dum_dt_scav,dum_ocn_Fe,dum_bio_part,dum_bio_remin)
    ! dummy arguments
    REAL,INTENT(in)::dum_dtyr
    REAL,INTENT(in)::dum_dt_scav
    REAL,INTENT(in)::dum_ocn_Fe
    real,dimension(n_sed),INTENT(inout)::dum_bio_part
    real,dimension(n_ocn),INTENT(inout)::dum_bio_remin
    ! local variables
    real::loc_scav_Fe_k_POC,loc_scav_Fe_k_CaCO3,loc_scav_Fe_k_opal,loc_scav_Fe_k_det
    real::loc_scav_Fe_k_tot
    real::loc_scav_dFe_tot
    real::loc_part_den_POC,loc_part_den_CaCO3,loc_part_den_opal,loc_part_den_det
    real::loc_part_den_tot

    ! *** Calculate Fe scavenging ***
    ! NOTE: residence time in each ocean layer must be estimated for the Parekh et al. [2005] model,
    !       with dum_bio_part passed in units of mol kg-1 for a specific cell
    !       BUT the mass this represents is actually spread across multiple cells during each time step
    !       i.e., in any cell, this density of material in effect exists only for a fraction of that time-step
    !       => normalize by the fraction of time spent in that cell during the time-step
    !          = residence time / time-step
    !            (could also be: cell thickness / (time-step x local velocity))
    !       -> note that ultimately, dum_dtyr/dum_dtyr cancel out leaving the residence time dum_dtyr
    !          (this could then be simplified, but it is easier(?) to see how things pan out with the effective
    !           particle concentration adjusted up-front by dum_dt_scav/dum_dtyr)
    ! NOTE: Dutkiewicz et al. [2005] scavenging rate par_scav_Fe_Ks has been converted to units of (yr-1)
    if (sed_select(is_POM_Fe)) then
       loc_part_den_POC = (conv_g_mg*conv_POC_mol_g*dum_bio_part(is_POC)/conv_kg_l) * dum_dt_scav/dum_dtyr
    else
       loc_part_den_POC = 0.0
    end if
    if (sed_select(is_CaCO3_Fe)) then
       loc_part_den_CaCO3 = (conv_g_mg*conv_cal_mol_g*dum_bio_part(is_CaCO3)/conv_kg_l) * dum_dt_scav/dum_dtyr
    else
       loc_part_den_CaCO3 = 0.0
    end if
    if (sed_select(is_opal_Fe)) then
       loc_part_den_opal  = (conv_g_mg*conv_opal_mol_g*dum_bio_part(is_opal)/conv_kg_l) * dum_dt_scav/dum_dtyr
    else
       loc_part_den_opal  = 0.0
    end if
    if (sed_select(is_det_Fe)) then
       loc_part_den_det   = (conv_g_mg*conv_det_mol_g*dum_bio_part(is_det)/conv_kg_l) * dum_dt_scav/dum_dtyr
    else
       loc_part_den_det   = 0.0
    end if
    loc_part_den_tot = loc_part_den_POC + loc_part_den_CaCO3 + loc_part_den_opal + loc_part_den_det
    if (loc_part_den_tot > const_real_nullsmall) then
       if (ctrl_bio_Fe_fixedKscav) then
          ! calculate scavenging following Dutkiewicz et al. [2005]
          ! net scavenging rate
          loc_scav_Fe_k_tot = par_scav_Fe_ks
          ! particle-specific scavenging rates
          loc_scav_Fe_k_POC = (loc_part_den_POC/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_CaCO3 = (loc_part_den_CaCO3/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_opal = (loc_part_den_opal/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_det = (loc_part_den_det/loc_part_den_tot)*loc_scav_Fe_k_tot
          ! calculate total Fe scavenged
          loc_scav_dFe_tot = dum_dtyr*loc_scav_Fe_k_tot*dum_ocn_Fe
       else
          ! calculate scavenging following Parekh et al. [2005]
          ! particle-specific scavenging rates
          loc_scav_Fe_k_POC   = par_scav_Fe_sf_POC*par_scav_Fe_k0*loc_part_den_POC**par_scav_Fe_exp
          loc_scav_Fe_k_CaCO3 = par_scav_Fe_sf_CaCO3*par_scav_Fe_k0*loc_part_den_CaCO3**par_scav_Fe_exp
          loc_scav_Fe_k_opal  = par_scav_Fe_sf_opal*par_scav_Fe_k0*loc_part_den_opal**par_scav_Fe_exp
          loc_scav_Fe_k_det   = par_scav_Fe_sf_det*par_scav_Fe_k0*loc_part_den_det**par_scav_Fe_exp
          ! net scavenging rate
          loc_scav_Fe_k_tot = loc_scav_Fe_k_POC + loc_scav_Fe_k_CaCO3 + loc_scav_Fe_k_opal + loc_scav_Fe_k_det
          ! calculate total Fe scavenged
          loc_scav_dFe_tot = dum_dtyr*loc_scav_Fe_k_tot*dum_ocn_Fe
       end if
       ! calculate Fe scavenged by particulates
       ! and update local remineralization array to take into account the removal of Fe from solution
       if (loc_scav_Fe_k_tot > const_real_nullsmall) then
          dum_bio_part(is_POM_Fe)   = dum_bio_part(is_POM_Fe) + (loc_scav_Fe_k_POC/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          dum_bio_part(is_CaCO3_Fe) = dum_bio_part(is_CaCO3_Fe) + (loc_scav_Fe_k_CaCO3/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          dum_bio_part(is_opal_Fe)  = dum_bio_part(is_opal_Fe) + (loc_scav_Fe_k_opal/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          dum_bio_part(is_det_Fe)   = dum_bio_part(is_det_Fe) + (loc_scav_Fe_k_det/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          dum_bio_remin(io_Fe) = dum_bio_remin(io_Fe) - loc_scav_dFe_tot
       end if
    end if

  end SUBROUTINE sub_calc_scav_Fe
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate Os scavenging
  SUBROUTINE sub_box_scav_Os(dum_dtyr,dum_dt_scav, &
       & dum_ocn_Os,dum_ocn_Os_187Os,dum_ocn_Os_188Os,dum_ocn_O2,dum_bio_part,dum_bio_remin)
    ! dummy arguments
    REAL,INTENT(in)::dum_dtyr
    REAL,INTENT(in)::dum_dt_scav
    REAL,INTENT(in)::dum_ocn_Os,dum_ocn_Os_187Os,dum_ocn_Os_188Os,dum_ocn_O2
    real,dimension(n_sed),INTENT(inout)::dum_bio_part
    real,dimension(n_ocn),INTENT(inout)::dum_bio_remin
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_Os,loc_Os_187Os,loc_Os_188Os
    real::loc_Os_scavenging,loc_Os_187Os_scavenging,loc_Os_188Os_scavenging
    real::loc_f
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- ! maximum fraction consumed in any given geochemical reaction
    loc_f = dum_dtyr/par_bio_geochem_tau
    ! -------------------------------------------------------- ! set local solutes
    loc_Os = dum_ocn_Os
    loc_Os_187Os = dum_ocn_Os_187Os
    loc_Os_188Os = dum_ocn_Os_188Os
    ! -------------------------------------------------------- !
    ! CALCULATE Os SCAVENGING
    ! -------------------------------------------------------- !
    ! estimate Os scavenging
    ! NOTE: select if scavenging should be dependent on ambient O2 concentration.
    !       If yes, Os is only scavenged below an adjustable O2 threshold.
    if ((ctrl_Os_scav_O2_dep) .and. (dum_ocn_O2 < par_scav_Os_O2_threshold)) then
       loc_Os_scavenging = par_bio_remin_kOstoPOMOS*loc_Os*dum_dt_scav*dum_bio_part(is2l(is_POC))
    elseif (.not. ctrl_Os_scav_O2_dep) then
       loc_Os_scavenging = par_bio_remin_kOstoPOMOS*loc_Os*dum_dt_scav*dum_bio_part(is2l(is_POC))
    else
       loc_Os_scavenging = 0.0
    end if
    ! Make sure the amount of scavenged Os >= 0
    loc_Os_scavenging = max(0.0,loc_Os_scavenging)
    ! Assume no isotopic fractionation during scavenging
    loc_Os_187Os_scavenging = loc_Os_187Os*loc_Os_scavenging/loc_Os
    loc_Os_188Os_scavenging = loc_Os_188Os*loc_Os_scavenging/loc_Os
    ! implement scavenging
    dum_bio_remin(io2l(io_Os))          = dum_bio_remin(io2l(io_Os)) - loc_Os_scavenging
    dum_bio_remin(io2l(io_Os_187Os))    = dum_bio_remin(io2l(io_Os_187Os)) - loc_Os_187Os_scavenging
    dum_bio_remin(io2l(io_Os_188Os))    = dum_bio_remin(io2l(io_Os_188Os)) - loc_Os_188Os_scavenging
    dum_bio_part(is2l(is_POM_Os))       = dum_bio_part(is2l(is_POM_Os)) + loc_Os_scavenging
    dum_bio_part(is2l(is_POM_Os_187Os)) = dum_bio_part(is2l(is_POM_Os_187Os)) + loc_Os_187Os_scavenging
    dum_bio_part(is2l(is_POM_Os_188Os)) = dum_bio_part(is2l(is_POM_Os_188Os)) + loc_Os_188Os_scavenging
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_scav_Os
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate H2S scavenging
  SUBROUTINE sub_box_scav_H2S(dum_i,dum_j,dum_k,dum_dtyr,dum_dt_scav,dum_ocn_H2S,dum_bio_part,dum_bio_remin)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k
    REAL,INTENT(in)::dum_dtyr
    REAL,INTENT(in)::dum_dt_scav
    REAL,INTENT(in)::dum_ocn_H2S
    real,dimension(n_l_sed),INTENT(inout)::dum_bio_part
    real,dimension(n_l_ocn),INTENT(inout)::dum_bio_remin
    ! local variables
    real::loc_H2S,loc_part_den_POCl
    real::loc_H2S_scavenging
    real::loc_dt

    ! *** Calculate H2S scavenging ***
    ! set local residence time in layer
    ! NOTE: the residence time should have been: dum_dtyr*(dum_dt_scav/dum_dtyr) == dum_dt_scav
    !       as per the Fe scavenging scheme (I know ... confusing ...)
    if (ctrl_scav_H2S_dt_old) then
       loc_dt = dum_dt_scav*(dum_dt_scav/dum_dtyr)
    else
       loc_dt = dum_dt_scav
    end if
    ! set local variables
    loc_H2S = dum_ocn_H2S
    ! density of labile POC
    loc_part_den_POCl = (1.0 - dum_bio_part(is2l(is_POC_frac2)))*dum_bio_part(is2l(is_POC))
    ! estimate H2S scavenging
    ! NOTE: cap H2S removal at the minimum of ([H2S], [labile POC])
    SELECT CASE (opt_bio_remin_scavenge_H2StoPOMS)
    CASE ('oxidationanalogue')
       ! simply substituting [O2] for POC concentration!
       ! NOTE: H2S oxidation analogue: -d[H2S]/dt = k1[H2S][O2]
       ! NOTE: par_bio_remin_kH2StoSO4 units are (M-1 yr-1)
       ! NOTE: the concentration that dum_bio_part represents is actually spread across multiple cells during each time step
       !       i.e., in any cell, this density of material in effect exists only for a fraction of that time-step
       !       => normalize by the fraction of time spent in that cell during the time-step (== residence time / time-step)
       loc_H2S_scavenging = loc_dt*par_bio_remin_kH2StoSO4*loc_H2S*loc_part_den_POCl
       !       print*, 'oxidationanalogue, par_bio_remin_kH2StoSO4 ', par_bio_remin_kH2StoSO4
       !       print*, 'loc_H2S, loc_H2S_scavenging', loc_H2S, loc_H2S_scavenging
    CASE ('kinetic')
       !        print*, 'kinetic '
       !        print*, 'par_bio_remin_kH2StoPOMS ', par_bio_remin_kH2StoPOMS
       ! use rate constant from Dale et al. 2009 k = 0.2 M-1 yr-1
       ! NOTE: the concentration that dum_bio_part represents is actually spread across multiple cells during each time step
       !       i.e., in any cell, this density of material in effect exists only for a fraction of that time-step
       !       => normalize by the fraction of time spent in that cell during the time-step (== residence time / time-step)
       loc_H2S_scavenging = loc_dt*par_bio_remin_kH2StoPOMS*loc_H2S*loc_part_den_POCl
    CASE ('complete')
       loc_H2S_scavenging = min(loc_part_den_POCl,loc_H2S)
    case default
       !        print*, 'No sulphurization '
       loc_H2S_scavenging = 0.0
    end select
    ! cap scavenging
    loc_H2S_scavenging = min(loc_H2S_scavenging,loc_part_den_POCl,loc_H2S)
    if (loc_H2S_scavenging < const_real_nullsmall) loc_H2S_scavenging = 0.0
    ! bulk tracer conversion
    ! NOTE: is_POC_frac2 should not end up with a value exceeding 1.0
    dum_bio_remin(io2l(io_H2S))      = dum_bio_remin(io2l(io_H2S)) - loc_H2S_scavenging
    dum_bio_part(is2l(is_POM_S))     = dum_bio_part(is2l(is_POM_S)) + loc_H2S_scavenging
    dum_bio_part(is2l(is_POC_frac2)) = dum_bio_part(is2l(is_POC_frac2)) + loc_H2S_scavenging/dum_bio_part(is2l(is_POC))
    if (dum_bio_part(is2l(is_POC_frac2)) > 1.1) stop
    if (dum_bio_part(is2l(is_POC_frac2)) > 1.0) dum_bio_part(is2l(is_POC_frac2)) = 1.0
    ! -------------------------------------------------------- ! record diagnostics (mol kg-1)
    diag_react(idiag_react_POMS_dH2S,dum_i,dum_j,dum_k) = loc_H2S_scavenging

  end SUBROUTINE sub_box_scav_H2S
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FORCING FUNCTION ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Updating environment at the current (BioGeM) model time w.r.t. a defined signal function
  SUBROUTINE sub_update_sig(dum_t,dum_sig,dum_sig_i,dum_x)
    ! dummy arguments
    REAL, INTENT(in)::dum_t
    REAL,INTENT(in),DIMENSION(n_data_max)::dum_sig
    INTEGER,INTENT(inout),DIMENSION(2)::dum_sig_i
    REAL, INTENT(out)::dum_x
    ! update forcing signal indices (if required) and carry put linear interpolation
    ! NOTE: t(1) is the lower age bounding point, t(2) is the upper age bounding point
    IF (dum_sig_i(1) > 1) THEN
       IF (dum_t < dum_sig(dum_sig_i(1))) THEN
          DO
             dum_sig_i(1) = dum_sig_i(1) - 1
             IF (dum_t > dum_sig(dum_sig_i(1))) THEN
                ! found correct index - exit loop
                EXIT
             ELSEIF (dum_sig_i(1) == 1) THEN
                EXIT
             END IF
          END DO
       END IF
    END IF
    IF (dum_sig_i(2) > 1) THEN
       IF (dum_t < dum_sig(dum_sig_i(2))) THEN
          DO
             dum_sig_i(2) = dum_sig_i(2) - 1
             IF (dum_t >= dum_sig(dum_sig_i(2))) THEN
                ! come too far - add one back to index(2) and exit loop
                dum_sig_i(2) = dum_sig_i(2) + 1
                EXIT
             ELSEIF (dum_sig_i(2) == 1) THEN
                EXIT
             END IF
          END DO
       END IF
    END IF
    ! calculate relative position of current time w.r.t. upper and lower bounding points of the signal function
    ! NOTE: if upper and lower bounding points are identical
    !       (i.e., if current time is outside of maximum or minimum signal time values)
    !       avoid divide-by-zero problems and assume a value of 0.5
    IF (ABS(dum_sig(dum_sig_i(2)) - dum_sig(dum_sig_i(1))) > const_real_nullsmall) THEN
       dum_x = (dum_sig(dum_sig_i(2)) - dum_t)/(dum_sig(dum_sig_i(2)) - dum_sig(dum_sig_i(1)))
    ELSE
       dum_x = 0.5
    ENDIF
  END SUBROUTINE sub_update_sig
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Update ocean restoring forcing function value
  SUBROUTINE sub_update_force_restore_ocn(dum_t,dum_io)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_io
    ! local variables
    INTEGER::i,j,k
    integer::loc_l
    REAL::loc_x
    REAL::loc_force_restore_ocn
    real::loc_tot,loc_standard
    !
    loc_l = conv_io_lselected(dum_io)
    ! calculate new forcing time series values
    CALL sub_update_sig(dum_t,force_restore_ocn_sig(dum_io,1,:),force_restore_ocn_sig_i(dum_io,:),loc_x)
    force_restore_ocn_sig_x(dum_io) = &
         & (1 - loc_x)*force_restore_ocn_sig(dum_io,2,force_restore_ocn_sig_i(dum_io,2)) + &
         & loc_x*force_restore_ocn_sig(dum_io,2,force_restore_ocn_sig_i(dum_io,1))
    ! *** update prescribed (restoring) boundary conditions ***
    ! NOTE: use different <k> limits for the ocean restoring forcing loop (to enable surface-only forcing to be implemented)
    ! NOTE: flux forcings are in units of mol a-1
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             loc_force_restore_ocn = &
                  & force_restore_locn_I(loc_l,i,j,k) + &
                  & force_restore_ocn_sig_x(dum_io)*(force_restore_locn_II(loc_l,i,j,k) - force_restore_locn_I(loc_l,i,j,k))
             SELECT CASE (ocn_type(dum_io))
             CASE (0,1)
                force_restore_locn(loc_l,i,j,k) = loc_force_restore_ocn
             case (n_itype_min:n_itype_max)
                loc_tot  = force_restore_locn(conv_io_lselected(ocn_dep(dum_io)),i,j,k)
                loc_standard = const_standards(ocn_type(dum_io))
                force_restore_locn(loc_l,i,j,k) = fun_calc_isotope_fraction(loc_force_restore_ocn,loc_standard)*loc_tot
             END SELECT
          END DO
       END DO
    END DO
  END SUBROUTINE sub_update_force_restore_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Update ocean flux forcing function value
  SUBROUTINE sub_update_force_flux_ocn(dum_t,dum_io)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_io
    ! local variables
    INTEGER::i,j,k
    integer::loc_l
    REAL::loc_x
    REAL::loc_force_flux_ocn_tot
    REAL::loc_force_flux_ocn_rtot
    REAL::loc_force_flux_ocn
    real::loc_tot,loc_standard
    !
    loc_l = conv_io_lselected(dum_io)
    ! calculate new forcing time series values
    CALL sub_update_sig(dum_t,force_flux_ocn_sig(dum_io,1,:),force_flux_ocn_sig_i(dum_io,:),loc_x)
    force_flux_ocn_sig_x(dum_io) = &
         & (1 - loc_x)*force_flux_ocn_sig(dum_io,2,force_flux_ocn_sig_i(dum_io,2)) + &
         & loc_x*force_flux_ocn_sig(dum_io,2,force_flux_ocn_sig_i(dum_io,1))
    ! *** update flux boundary conditions ***
    ! NOTE: use different <k> limits for the ocean restoring forcing loop (to enable surface-only forcing to be implemented)
    ! NOTE: flux forcings are in units of mol yr-1
    loc_force_flux_ocn_tot = 0.0
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             loc_force_flux_ocn = &
                  & force_flux_locn_I(loc_l,i,j,k) + &
                  & force_flux_ocn_sig_x(dum_io)*(force_flux_locn_II(loc_l,i,j,k) - force_flux_locn_I(loc_l,i,j,k))
             SELECT CASE (ocn_type(dum_io))
             CASE (0,1)
                force_flux_locn(loc_l,i,j,k) = loc_force_flux_ocn
                loc_force_flux_ocn_tot = loc_force_flux_ocn_tot + loc_force_flux_ocn
             case (n_itype_min:n_itype_max)
                loc_tot  = force_flux_locn(conv_io_lselected(ocn_dep(dum_io)),i,j,k)
                loc_standard = const_standards(ocn_type(dum_io))
                force_flux_locn(loc_l,i,j,k) = fun_calc_isotope_fraction(loc_force_flux_ocn,loc_standard)*loc_tot
             END SELECT
          END DO
       END DO
    END DO
    ! normalize flux forcings (if selected) so that the total flux is equal to the magnitude (at the current time step)
    ! defined in the forcing signal file
    IF (force_flux_ocn_scale(dum_io)) THEN
       if (abs(loc_force_flux_ocn_tot) > const_real_nullsmall) then
          loc_force_flux_ocn_rtot = 1.0/loc_force_flux_ocn_tot
       else
          loc_force_flux_ocn_rtot = 0.0
       end if
       DO i=1,n_i
          DO j=1,n_j
             DO k=goldstein_k1(i,j),n_k
                SELECT CASE (ocn_type(dum_io))
                CASE (0,1)
                   force_flux_locn(loc_l,i,j,k) = force_flux_ocn_sig_x(dum_io)*force_flux_locn(loc_l,i,j,k)*loc_force_flux_ocn_rtot
                end SELECT
                ! Calculate Os isotope flux forcings
                if (dum_io == io_Os) then
                   loc_tot = force_flux_locn(conv_io_lselected(io_Os),i,j,k)/( &
                        & 1.0 + &
                        & force_flux_ocn_sig_x(io_Os_187Os)*force_flux_ocn_sig_x(io_Os_188Os) + &
                        & force_flux_ocn_sig_x(io_Os_188Os) &
                        & )
                   force_flux_locn(conv_io_lselected(io_Os_187Os),i,j,k) = &
                        & force_flux_ocn_sig_x(io_Os_187Os)*force_flux_ocn_sig_x(io_Os_188Os)*loc_tot
                   force_flux_locn(conv_io_lselected(io_Os_188Os),i,j,k) = &
                        & force_flux_ocn_sig_x(io_Os_188Os)*loc_tot
                end if
             END DO
          END DO
       END DO
    END IF
  END SUBROUTINE sub_update_force_flux_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Update atmosphere tracer restoring forcing function value
  SUBROUTINE sub_update_force_restore_atm(dum_t,dum_ia)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_ia
    ! local variables
    INTEGER::i,j
    REAL::loc_x
    REAL::loc_force_restore_atm
    real::loc_tot,loc_standard
    ! calculate new atmosphere forcing time series values
    CALL sub_update_sig(dum_t,force_restore_atm_sig(dum_ia,1,:),force_restore_atm_sig_i(dum_ia,:),loc_x)
    force_restore_atm_sig_x(dum_ia) = &
         & (1 - loc_x)*force_restore_atm_sig(dum_ia,2,force_restore_atm_sig_i(dum_ia,2)) + &
         & loc_x*force_restore_atm_sig(dum_ia,2,force_restore_atm_sig_i(dum_ia,1))
    ! update prescribed (restoring) boundary conditions
    DO i=1,n_i
       DO j=1,n_j
          loc_force_restore_atm =  &
               & force_restore_atm_I(dum_ia,i,j) + &
               & force_restore_atm_sig_x(dum_ia)*(force_restore_atm_II(dum_ia,i,j) - force_restore_atm_I(dum_ia,i,j))
          SELECT CASE (atm_type(dum_ia))
          CASE (0,1)
             force_restore_atm(dum_ia,i,j) = loc_force_restore_atm
          case (n_itype_min:n_itype_max)
             loc_tot  = force_restore_atm(atm_dep(dum_ia),i,j)
             loc_standard = const_standards(atm_type(dum_ia))
             force_restore_atm(dum_ia,i,j) = fun_calc_isotope_fraction(loc_force_restore_atm,loc_standard)*loc_tot
          END SELECT
       END DO
    END DO
  END SUBROUTINE sub_update_force_restore_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Update atmosphere tracer flux forcing function value
  SUBROUTINE sub_update_force_flux_atm(dum_t,dum_ia)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_ia
    ! local variables
    INTEGER::i,j
    REAL::loc_x
    REAL::loc_force_flux_atm_tot
    REAL::loc_force_flux_atm_rtot
    REAL::loc_force_flux_atm
    real::loc_tot,loc_standard
    ! calculate new atmosphere forcing time series values
    CALL sub_update_sig(dum_t,force_flux_atm_sig(dum_ia,1,:),force_flux_atm_sig_i(dum_ia,:),loc_x)
    force_flux_atm_sig_x(dum_ia) = &
         & (1 - loc_x)*force_flux_atm_sig(dum_ia,2,force_flux_atm_sig_i(dum_ia,2)) + &
         & loc_x*force_flux_atm_sig(dum_ia,2,force_flux_atm_sig_i(dum_ia,1))
    ! update flux boundary conditions
    ! NOTE: flux forcings are in units of mol yr-1
    ! NOTE: Easter Egg -- if bulk flux and isotopic compositoin are identical, then assume
    !       that the forcing flux consists entirely of the isotopic species
    !       (a way of e.g. introducing pure 14C to the atmosphere)
    ! NOTE: for testing for equivalent fluxes -- remember the SCALING of the bulk tracer that occurs first ...
    loc_force_flux_atm_tot = 0.0
    DO i=1,n_i
       DO j=1,n_j
          loc_force_flux_atm = &
               & force_flux_atm_I(dum_ia,i,j) + &
               & force_flux_atm_sig_x(dum_ia)*(force_flux_atm_II(dum_ia,i,j) - force_flux_atm_I(dum_ia,i,j))
          SELECT CASE (atm_type(dum_ia))
          CASE (0,1)
             force_flux_atm(dum_ia,i,j) = loc_force_flux_atm
             loc_force_flux_atm_tot = loc_force_flux_atm_tot + loc_force_flux_atm
          case (n_itype_min:n_itype_max)
             loc_tot  = force_flux_atm(atm_dep(dum_ia),i,j)
             loc_standard = const_standards(atm_type(dum_ia))
             force_flux_atm(dum_ia,i,j) = fun_calc_isotope_fraction(loc_force_flux_atm,loc_standard)*loc_tot
             if (abs(force_flux_atm_sig_x(dum_ia) - force_flux_atm_sig_x(atm_dep(dum_ia))) < const_real_nullsmall) then
                force_flux_atm(dum_ia,i,j) = loc_tot
             end if
          END SELECT
       END DO
    END DO
    ! normalize flux forcings (if selected) so that the total flux is equal to the magnitude (at the current time step)
    ! defined in the forcing signal file
    ! NOTE: only re-scale type 1 atmosphere tracers -
    !       the isotopic tracers will be automatically normalized because they are related directly to the total flux,
    !       and when this subroutine call is made for an isotopic tracer,
    !       it has already been called to deal with the related bulk tracer where the normalization is done
    IF (force_flux_atm_scale(dum_ia)) THEN
       if (abs(loc_force_flux_atm_tot) > const_real_nullsmall) then
          loc_force_flux_atm_rtot = 1.0/loc_force_flux_atm_tot
       else
          loc_force_flux_atm_rtot = 0.0
       end if
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (atm_type(dum_ia))
             CASE (0,1)
                force_flux_atm(dum_ia,i,j) = force_flux_atm(dum_ia,i,j)*force_flux_atm_sig_x(dum_ia)*loc_force_flux_atm_rtot
             END SELECT
          END DO
       END DO
    END IF
  END SUBROUTINE sub_update_force_flux_atm
  ! ****************************************************************************************************************************** !


!!$  ! *** update sediment tracer restoring forcing function value ***
!!$  ! <<< GENERIC >>>
!!$  SUBROUTINE sub_update_force_restore_sed(dum_t,dum_is)
!!$    ! dummy arguments
!!$    REAL,INTENT(in)::dum_t
!!$    INTEGER,INTENT(in)::dum_is
!!$    ! local variables
!!$    INTEGER::i,j
!!$    REAL::loc_x
!!$    ! calculate new sediment tracer forcing time series values
!!$    CALL sub_update_sig(dum_t,force_restore_sed_sig(dum_is,1,:),force_restore_sed_sig_i(dum_is,:),loc_x)
!!$    force_restore_sed_sig_x(dum_is) = &
!!$         & (1 - loc_x)*force_restore_sed_sig(dum_is,2,force_restore_sed_sig_i(dum_is,2)) + &
!!$         & loc_x*force_restore_sed_sig(dum_is,2,force_restore_sed_sig_i(dum_is,1))
!!$    ! update prescribed (restoring) boundary conditions
!!$    DO i=1,n_i
!!$       DO j=1,n_j
!!$          force_restore_sed(dum_is,i,j) = &
!!$               & force_restore_sed_I(dum_is,i,j) + &
!!$               & force_restore_sed_sig_x(dum_is)*(force_restore_sed_II(dum_is,i,j) - force_restore_sed_I(dum_is,i,j))
!!$       END DO
!!$    END DO
!!$  END SUBROUTINE sub_update_force_restore_sed


  ! ****************************************************************************************************************************** !
  ! Update sediment tracer flux forcing function value
  SUBROUTINE sub_update_force_flux_sed(dum_t,dum_is)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_is
    ! local variables
    INTEGER::i,j
    REAL::loc_x
    REAL::loc_force_flux_sed_tot
    REAL::loc_force_flux_sed_rtot
    REAL::loc_force_flux_sed
    real::loc_tot,loc_standard
    ! calculate new sediment tracer forcing time series values
    CALL sub_update_sig(dum_t,force_flux_sed_sig(dum_is,1,:),force_flux_sed_sig_i(dum_is,:),loc_x)
    force_flux_sed_sig_x(dum_is) = &
         & (1 - loc_x)*force_flux_sed_sig(dum_is,2,force_flux_sed_sig_i(dum_is,2)) + &
         & loc_x*force_flux_sed_sig(dum_is,2,force_flux_sed_sig_i(dum_is,1))
    ! update flux boundary conditions
    ! NOTE: flux forcings are in units of mol yr-1
    loc_force_flux_sed_tot = 0.0
    DO i=1,n_i
       DO j=1,n_j
          loc_force_flux_sed = &
               & force_flux_sed_I(dum_is,i,j) + &
               & force_flux_sed_sig_x(dum_is)*(force_flux_sed_II(dum_is,i,j) - force_flux_sed_I(dum_is,i,j))
          SELECT CASE (sed_type(dum_is))
          CASE (par_sed_type_bio,par_sed_type_abio,par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
             force_flux_sed(dum_is,i,j) = loc_force_flux_sed
             loc_force_flux_sed_tot = loc_force_flux_sed_tot + loc_force_flux_sed
          case (n_itype_min:n_itype_max)
             loc_tot  = force_flux_sed(sed_dep(dum_is),i,j)
             loc_standard = const_standards(sed_type(dum_is))
             force_flux_sed(dum_is,i,j) = fun_calc_isotope_fraction(loc_force_flux_sed,loc_standard)*loc_tot
          END SELECT
       END DO
    END DO
    ! normalize flux forcings (if selected) so that the total flux is equal to the magnitude (at the current time step)
    ! defined in the forcing signal file
    ! NOTE: add det tracer to normalization
    IF (force_flux_sed_scale(dum_is)) THEN
       if (abs(loc_force_flux_sed_tot) > const_real_nullsmall) then
          loc_force_flux_sed_rtot = 1.0/loc_force_flux_sed_tot
       else
          loc_force_flux_sed_rtot = 0.0
       end if
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(dum_is))
             CASE (par_sed_type_bio,par_sed_type_abio)
                force_flux_sed(dum_is,i,j) = force_flux_sed(dum_is,i,j)*force_flux_sed_sig_x(dum_is)*loc_force_flux_sed_rtot
             end SELECT
          END DO
       END DO
    END IF
  END SUBROUTINE sub_update_force_flux_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! MISCELLANEOUS FORCING
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! MISCELLANEOUS GEOCHEMICAL TRANSFORMATIONS
  SUBROUTINE sub_box_misc_geochem(dum_i,dum_j,dum_k1,dum_dtyr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,k
    integer::loc_l_min,loc_l_max,loc_k
    real::loc_flux
    real,dimension(n_ocn,n_k)::loc_bio_remin
    ! -------------------------------------------------------- !
    ! INITIALIZE VARIABLES
    ! -------------------------------------------------------- !
    if (ctrl_force_GOLDSTEInTS .OR. (n_l_ocn < 3)) then
       loc_l_min = 1
       if (ctrl_force_GOLDSTEInTSonly) then
          loc_l_max = 2
       else
          loc_l_max = n_l_ocn
       endif
    else
       loc_l_min = 3
       loc_l_max = n_l_ocn
       if (ctrl_misc_geoeng_noDIC) loc_l_min = 4
    endif
    ! initialize remineralization tracer arrays
    DO l=loc_l_min,loc_l_max
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! -------------------------------------------------------- !
    ! GEOENGINEERING
    ! -------------------------------------------------------- !
    IF ((trim(opt_misc_geoeng) /= 'NONE') .AND. (par_misc_kmin_pipe < n_k)) THEN
       select case (opt_misc_geoeng)
       case('pipes')
          ! -------------------------------------------------- ! initialize
          ! NOTE: flux in units of m-3 per time step (rather than in kg units)
          loc_flux = dum_dtyr*par_misc_2D_scale*par_misc_2D(dum_i,dum_j)
          ! set source k level
          ! NOTE: cap at deepest ocean level
          loc_k = par_misc_kmin_pipe
          if (loc_k < dum_k1) loc_k = dum_k1
          DO l=loc_l_min,loc_l_max
             io = conv_iselected_io(l)
             loc_bio_remin(io,n_k) = loc_bio_remin(io,n_k) + &
                  & loc_flux*ocn(io,dum_i,dum_j,loc_k)/phys_ocn(ipo_V,dum_i,dum_j,n_k) - &
                  & loc_flux*ocn(io,dum_i,dum_j,n_k)/phys_ocn(ipo_V,dum_i,dum_j,n_k)
             DO k=n_k-1,loc_k,-1
                loc_bio_remin(io,k) = loc_bio_remin(io,k) + &
                     & loc_flux*ocn(io,dum_i,dum_j,k+1)/phys_ocn(ipo_V,dum_i,dum_j,k) - &
                     & loc_flux*ocn(io,dum_i,dum_j,k)/phys_ocn(ipo_V,dum_i,dum_j,k)
             end do
          end do
       case default
          ! -------------------------------------------------- ! nothing doing ...
       end select
    end IF
    ! -------------------------------------------------------- !
    ! WRITE GLOBAL ARRAY DATA
    ! -------------------------------------------------------- !
    ! write ocean tracer remineralization field (global array)
    DO l=loc_l_min,loc_l_max
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_misc_geochem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! BRINE REJECTION MECHANISM
  SUBROUTINE sub_calc_misc_brinerejection(dum_dtyr,dum_i,dum_j,dum_fT,dum_fS)
    ! dummy arguments
    real,intent(in)::dum_dtyr                                      !
    INTEGER,INTENT(in)::dum_i,dum_j                                !
    real,INTENT(inout)::dum_fT,dum_fS                              !
    ! local variables
    integer::l,io                                                  !
    integer::loc_k1                                                ! local topography
    real::loc_dV,loc_rM,loc_frac                                   !
    real,dimension(n_ocn,n_k)::loc_bio_remin                       !

    ! *** BLAH ***
    ! set local constants
    loc_k1 = goldstein_k1(dum_i,dum_j)
    loc_dV = phys_ocnatm(ipoa_seaice_dV,dum_i,dum_j)
    loc_rM = phys_ocn(ipo_M,dum_i,dum_j,n_k)/phys_ocn(ipo_M,dum_i,dum_j,loc_k1)
    ! initialize variables
    dum_fT = 0.0
    dum_fS = 0.0
    loc_bio_remin(:,:) = 0.0

    ! *** BLAH ***
    ! carry out brine transfer from surface to depth
    if ((loc_dV > const_real_nullsmall) .AND. (dum_j <= par_misc_brinerejection_jmax)) then
       ! calculate fractional volume transfer of tracers from surface to benthic cell
       loc_frac = par_misc_brinerejection_frac* &
            & (const_rho_seaice/phys_ocn(ipo_rho,dum_i,dum_j,n_k))*(loc_dV/phys_ocn(ipo_V,dum_i,dum_j,n_k))
       ! calculate T,S fluxes
       dum_fT = 0.0
       dum_fS = loc_frac*ocn(io_S,dum_i,dum_j,n_k)*phys_ocn(ipo_M,dum_i,dum_j,n_k)/dum_dtyr
       if (ctrl_misc_brinerejection_bgc) then
          ! calculate biogeochem tracer concentration changes
          DO l=3,n_l_ocn
             io = conv_iselected_io(l)
             loc_bio_remin(io,n_k)    = -loc_frac*ocn(io,dum_i,dum_j,n_k)
             loc_bio_remin(io,loc_k1) = -loc_rM*loc_bio_remin(io,n_k)
          end DO
       end if
    end if

    ! *** WRITE DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do

  end SUBROUTINE sub_calc_misc_brinerejection
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INVENTORY AUDIT ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate ocean tracer inventory
  FUNCTION fun_calc_ocn_tot()
    ! result variable
    REAL,dimension(n_ocn)::fun_calc_ocn_tot
    ! local variables
    INTEGER::l,i,j,k,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_sed,n_i,n_j,n_k)::loc_bio_part
    real,dimension(n_ocn,n_i,n_j,n_k)::loc_bio_part_ocn
    real,dimension(n_ocn,n_i,n_j,n_k)::loc_ocn
    ! set local variables
    loc_bio_part(:,:,:,:)     = 0.0
    loc_bio_part_ocn(:,:,:,:) = 0.0
    loc_ocn(:,:,:,:)          = 0.0
    ! set default result
    fun_calc_ocn_tot(:) = 0.0
    ! convert particulate sediment and dissolved organic matter tracer concentrations to (dissolved) tracers
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             loc_ocn(:,i,j,k) = ocn(:,i,j,k)
             loc_bio_part(:,i,j,k) = bio_part(:,i,j,k)
             DO l=3,n_l_ocn
                io = conv_iselected_io(l)
                loc_tot_i = conv_DOM_POM_i(0,io)
                do loc_i=1,loc_tot_i
                   is = conv_DOM_POM_i(loc_i,io)
                   loc_bio_part(is,i,j,k)  = loc_bio_part(is,i,j,k) + conv_DOM_POM(is,io)*loc_ocn(io,i,j,k)
                   loc_ocn(io,i,j,k) = 0.0
                end do
             end do
!!$             DO l=3,n_l_ocn
!!$                io = conv_iselected_io(l)
!!$                loc_tot_i = conv_RDOM_POM_i(0,io)
!!$                do loc_i=1,loc_tot_i
!!$                   is = conv_RDOM_POM_i(loc_i,io)
!!$                   loc_bio_part(is,i,j,k)  = loc_bio_part(is,i,j,k) + conv_RDOM_POM(is,io)*loc_ocn(io,i,j,k)
!!$                   loc_ocn(io,i,j,k) = 0.0
!!$                end do
!!$             end do
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                loc_tot_i = conv_sed_ocn_i(0,is)
                do loc_i=1,loc_tot_i
                   io = conv_sed_ocn_i(loc_i,is)
                   loc_bio_part_ocn(io,i,j,k) = loc_bio_part_ocn(io,i,j,k) + conv_sed_ocn(io,is)*loc_bio_part(is,i,j,k)
                end do
             end DO
          end DO
       end DO
    end DO
    ! determine ocean tracer inventory (mol)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       fun_calc_ocn_tot(io) = sum(phys_ocn(ipo_M,:,:,:)*(loc_ocn(io,:,:,:) + loc_bio_part_ocn(io,:,:,:)))
    end do
    fun_calc_ocn_tot(:) = fun_audit_combinetracer(fun_calc_ocn_tot(:))
  END FUNCTION fun_calc_ocn_tot
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Carry out updated tracer audit
  SUBROUTINE sub_audit_update()
    ! local variables
    INTEGER::l,io
    REAL,dimension(n_ocn)::loc_audit_ocn_relerr
    ! set local variables
    loc_audit_ocn_relerr(:)   = 0.0
    ! calculate inventory drift
    audit_ocn_new(:) = fun_calc_ocn_tot()
    ! adjust ocean tracer inventory change (audit_ocn_delta) to combine different forms of the same element
    audit_ocn_delta(:) = fun_audit_combinetracer(audit_ocn_delta(:))
    ! calculate relative change in tracer inventories
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       if (abs(audit_ocn_new(io)) > const_real_nullsmall) then
          loc_audit_ocn_relerr(io) = (audit_ocn_new(io) - (audit_ocn_old(io) + audit_ocn_delta(io)))/audit_ocn_new(io)
       else
          loc_audit_ocn_relerr(io) = 0.0
       end if
    end DO
    ! compare current (ocean) tracer inventory with estimate since last audit,
    ! upate maximum error encountered, and report error if relative change exceeds pre-defined threshold
    ! NOTE: do not report 14C (ocn 'type' 12), because it decays and is 'lost' in mass terms ...
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       SELECT CASE (ocn_type(io))
       CASE (1,n_itype_min:n_itype_max)
          IF (ABS(loc_audit_ocn_relerr(io)) > par_misc_audit_relerr) THEN
             CALL sub_report_error('biogem_box','audit_update', &
                  & '(ocean) tracer inventory drift: new(top)/old(middle)/expected(bottom)): '//TRIM(string_ocn(io)), &
                  & 'n/a', &
                  & (/ &
                  &   audit_ocn_new(io), &
                  &   audit_ocn_old(io), &
                  &   (audit_ocn_old(io) + audit_ocn_delta(io)) &
                  & /), &
                  & ctrl_audit_fatal)
          ENDIF
          audit_ocn_old(io) = audit_ocn_new(io)
          audit_ocn_delta(io) = 0.0
       end SELECT
    END DO
  END SUBROUTINE sub_audit_update
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Combine different forms of the same element
  function fun_audit_combinetracer(dum_ocn)
    ! result variable
    real,dimension(n_ocn)::fun_audit_combinetracer
    ! dummy arguments
    REAL,dimension(n_ocn),INTENT(in)::dum_ocn
    ! initialze result variable
    fun_audit_combinetracer(:) = dum_ocn(:)
    ! adjust ocean tracer inventory change (audit_ocn_delta) to combine different forms of the same element
    ! NOTE: combine transformable tracer pairs when testing for drift;
    !       NO3 + N2 (N2 -> NO3 during nitrogen fixation, and NO3 -> N2 during denitrification))
    !       CO2 + CH4
    !       SO4 + H2S
    ! NOTE: adjust ALK for H2S (assumed created via sulphate reduction and thus ocean ALK increase)
    ! NOTE: subtract 2.0 x NH4 from O2 potential inventory to take into account virtual O2 liberation during ammoniam oxidation:
    !       NH4+ + 2O2 -> NO3- + 2H+ + H2O
    !       BUT ... -(3.0/4.0) balances NH4 production from PON (WHY???)
    ! NOTE: subtract 2.0 x N2 from O2 potential inventory to take into account virtual O2 liberation during denitrification:
    !       2NO3- + 2H+ -> N2 + 5/2O2 + H2O <--> 5O2 + 2N2 + 2H2O -> 4NO3- + 4H+
    ! NOTE: ALK changes associayed with NO3- are taken into account in the NO3- budget
    !       => no additional/explicit ALK budgeting w.r.t either N2 or NH4 is required
    ! NOTE: for O2 -- account for the deficit from reduced forms
    !       => do not attempt to implicitly oxidize reduced forms and calculate the resulting O2 deficit
    ! NOTE: for ALK -- count the charges (+vs and -ve)
    !       (excluding PO4)
    ! NOTE: Fe3 has 1/4 O2 vs. Fe2
    if (ocn_select(io_CH4)) then
       fun_audit_combinetracer(io_DIC) = fun_audit_combinetracer(io_DIC) + dum_ocn(io_CH4)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  - 2.0*dum_ocn(io_CH4)
    end if
    fun_audit_combinetracer(io_CH4) = 0.0
    if (ocn_select(io_CH4_13C)) then
       fun_audit_combinetracer(io_DIC_13C) = fun_audit_combinetracer(io_DIC_13C) + dum_ocn(io_CH4_13C)
    end if
    fun_audit_combinetracer(io_CH4_13C) = 0.0
    !if (ocn_select(io_PO4)) then
    !   fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  + 2.0*dum_ocn(io_PO4)
    !end if
    if (ocn_select(io_NO3)) then
       fun_audit_combinetracer(io_ALK) = fun_audit_combinetracer(io_ALK) + dum_ocn(io_NO3)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  + (5.0/4.0)*dum_ocn(io_NO3)
    end if
    ! io_N2O
    if (ocn_select(io_N2O)) then
       fun_audit_combinetracer(io_NO3) = fun_audit_combinetracer(io_NO3) + 2.0*dum_ocn(io_N2O)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  + 0.5*dum_ocn(io_N2O)
    end if
    fun_audit_combinetracer(io_N2O) = 0.0
    ! io_N2
    if (ocn_select(io_N2)) then
       fun_audit_combinetracer(io_NO3) = fun_audit_combinetracer(io_NO3) + 2.0*dum_ocn(io_N2)
    end if
    fun_audit_combinetracer(io_N2)  = 0.0
    ! io_NH4
    if (ocn_select(io_NH4)) then
       fun_audit_combinetracer(io_NO3) = fun_audit_combinetracer(io_NO3) + dum_ocn(io_NH4)
       fun_audit_combinetracer(io_ALK) = fun_audit_combinetracer(io_ALK) - dum_ocn(io_NH4)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  - (3.0/4.0)*dum_ocn(io_NH4)
    end if
    fun_audit_combinetracer(io_NH4) = 0.0
    ! io_H2S
    if (ocn_select(io_H2S)) then
       fun_audit_combinetracer(io_ALK) = fun_audit_combinetracer(io_ALK) - 2.0*dum_ocn(io_H2S)
       fun_audit_combinetracer(io_SO4) = fun_audit_combinetracer(io_SO4) + dum_ocn(io_H2S)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  - 2.0*dum_ocn(io_H2S)
    end if
    fun_audit_combinetracer(io_H2S) = 0.0
    ! io_Fe2
    if (ocn_select(io_Fe2) .AND. ocn_select(io_Fe)) then
       fun_audit_combinetracer(io_Fe)  = fun_audit_combinetracer(io_Fe)  + dum_ocn(io_Fe2)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  - (1.0/4.0)*dum_ocn(io_Fe2)
    end if
    fun_audit_combinetracer(io_Fe2) = 0.0
    ! io_FeL
    if (ocn_select(io_Fe) .AND. ocn_select(io_FeL)) then
       fun_audit_combinetracer(io_Fe)  = fun_audit_combinetracer(io_Fe)  + dum_ocn(io_FeL)
    end if
    if (ocn_select(io_L) .AND. ocn_select(io_FeL)) then
       fun_audit_combinetracer(io_L)   = fun_audit_combinetracer(io_L)   + dum_ocn(io_FeL)
    end if
    fun_audit_combinetracer(io_FeL) = 0.0
    ! io_I
    if (ocn_select(io_I)) then
       fun_audit_combinetracer(io_IO3) = fun_audit_combinetracer(io_IO3) + dum_ocn(io_I)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2) - (3.0/2.0)*dum_ocn(io_I)
    end if
    fun_audit_combinetracer(io_I) = 0.0
    ! io_H2S_34S
    if (ocn_select(io_SO4_34S) .AND. ocn_select(io_H2S_34S)) then
       fun_audit_combinetracer(io_SO4_34S) = fun_audit_combinetracer(io_SO4_34S) + dum_ocn(io_H2S_34S)
       fun_audit_combinetracer(io_H2S_34S) = 0.0
    end if
    ! io_Fe2_56Fe
    if (ocn_select(io_Fe2_56Fe) .AND. ocn_select(io_Fe_56Fe)) then
       fun_audit_combinetracer(io_Fe_56Fe)  = fun_audit_combinetracer(io_Fe_56Fe)  + dum_ocn(io_Fe2_56Fe)
    end if
    fun_audit_combinetracer(io_Fe2_56Fe) = 0.0
    ! io_FeL_56Fe
    if (ocn_select(io_Fe_56Fe) .AND. ocn_select(io_FeL_56Fe)) then
       fun_audit_combinetracer(io_Fe_56Fe)  = fun_audit_combinetracer(io_Fe_56Fe)  + dum_ocn(io_FeL_56Fe)
    end if
    fun_audit_combinetracer(io_FeL_56Fe) = 0.0
    ! #### INSERT CODE FOR FURTHER SPECIAL CASES ################################################################################# !
    !
    ! ############################################################################################################################ !
  end function fun_audit_combinetracer
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERSION ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Copy tracer array
  SUBROUTINE sub_biogem_copy_ocntots(dum_ts,dum_ts1)
    USE biogem_lib
    ! dummy arguments
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(inout)::dum_ts  ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(inout)::dum_ts1 ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
    ! local variables
    INTEGER::i,j,k,l,io
    real::loc_ocn_mean_S,loc_ocn_tot_V
    ! initialize local variables
    ! NOTE: this is a fudge to avoid compiler warnings for unused variables ...
    !       (the variables in question are only used depending on a specific compile-time option)
    loc_ocn_mean_S = 0.0
    loc_ocn_tot_V  = 0.0
    if (ctrl_misc_Snorm .AND. (n_l_ocn > 2)) then
       ! [SALINITY NORMALIZED SCHEME]
       ! calculate total ocean mass
       loc_ocn_tot_V = sum(phys_ocn(ipo_V,:,:,:))
       ! calculate mean ocean salinity
       loc_ocn_mean_S = SUM(ocn(io_S,:,:,:)*phys_ocn(ipo_V,:,:,:))/loc_ocn_tot_V
       ! copy GOLDSTEIn <ts> array values from the relevant <ocn> array of BioGeM
       ! NOTE: leave T (index 1) and S (index 2) well alone ;-)
       ! NOTE: no offset (array: <tstoocn_offset()>) required for biogeochem-only tracers
       ! NOTE: normalize by relative salinity deviation from ocean mean
       DO i=1,n_i
          DO j=1,n_j
             DO k=goldstein_k1(i,j),n_k
                DO l=3,n_l_ocn
                   io = conv_iselected_io(l)
                   dum_ts(l,i,j,k)  = ocn(io,i,j,k)*(loc_ocn_mean_S/ocn(io_S,i,j,k))
                   dum_ts1(l,i,j,k) = dum_ts(l,i,j,k)
                end do
             END DO
          END DO
       END DO
    else
       ! [NON-SALINITY NORMALIZED SCHEME]
       DO i=1,n_i
          DO j=1,n_j
             DO k=goldstein_k1(i,j),n_k
                DO l=3,n_l_ocn
                   io = conv_iselected_io(l)
                   dum_ts(l,i,j,k)  = ocn(io,i,j,k)
                   dum_ts1(l,i,j,k) = dum_ts(l,i,j,k)
                end do
             END DO
          END DO
       END DO
    end if
  END SUBROUTINE sub_biogem_copy_ocntots
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Copy tracer array
  SUBROUTINE sub_biogem_copy_tstoocn(dum_ts)
    USE biogem_lib
    ! dummy arguments
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(in)::dum_ts ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
    ! local variables
    INTEGER::i,j,k
    ! copy BioGeM <ocn> array values from the relevant <ts> (or <ts1>) array of GOLDSTEIN
    ! NOTE: restrict to T and S
    DO i=1,n_i
       DO j=1,n_j
          DO k=1,n_k
             IF (k >= goldstein_k1(i,j)) THEN
                ocn(io_T,i,j,k) = dum_ts(1,i,j,k) + tstoocn_offset(1)
                ocn(io_S,i,j,k) = dum_ts(2,i,j,k) + tstoocn_offset(2)
             END IF
          END DO
       END DO
    END DO
  END SUBROUTINE sub_biogem_copy_tstoocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Copy tracer array (T,S only)
  SUBROUTINE sub_biogem_copy_ocntotsTS(dum_ts,dum_ts1)
    USE biogem_lib
    ! dummy arguments
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(inout)::dum_ts ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(inout)::dum_ts1 ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
    ! local variables
    INTEGER::i,j,k
    ! copy GOLDSTEIn <ts> array values from the relevant <ocn> array of BioGeM
    ! NOTE: restrict to T and S
    DO i=1,n_i
       DO j=1,n_j
          DO k=1,n_k
             IF (k >= goldstein_k1(i,j)) THEN
                dum_ts(1,i,j,k) = ocn(io_T,i,j,k) - tstoocn_offset(1)
                dum_ts(2,i,j,k) = ocn(io_S,i,j,k) - tstoocn_offset(2)
                dum_ts1(1,i,j,k) = dum_ts(1,i,j,k)
                dum_ts1(2,i,j,k) = dum_ts(2,i,j,k)
             END IF
          END DO
       END DO
    END DO
  END SUBROUTINE sub_biogem_copy_ocntotsTS
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! USED!
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate Fe scavenging
  ! *** NEW TRACER NOTATION -- NOT USED YET ... ***
  ! NOTE: pass in [TDFe] rather than [Fe] if using new Fe scheme
  ! NOTE: allow remin array for [Fe] to be written to, but correct later (to [TDFe]) outside of subroutine
  SUBROUTINE sub_box_scav_Fe(dum_dtyr,dum_dt_scav,dum_ocn_Fe,dum_bio_part,dum_bio_remin)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    REAL,INTENT(in)::dum_dtyr
    REAL,INTENT(in)::dum_dt_scav
    REAL,INTENT(in)::dum_ocn_Fe
    real,dimension(n_l_sed),INTENT(inout)::dum_bio_part
    real,dimension(n_l_ocn),INTENT(inout)::dum_bio_remin
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real::loc_scav_Fe_k_POC,loc_scav_Fe_k_CaCO3,loc_scav_Fe_k_opal,loc_scav_Fe_k_det
    real::loc_scav_Fe_k_tot
    real::loc_scav_dFe_tot
    real::loc_part_den_POC,loc_part_den_CaCO3,loc_part_den_opal,loc_part_den_det
    real::loc_part_den_tot
    ! -------------------------------------------------------- !
    ! CALCULATE Fe SCAVENGING
    ! -------------------------------------------------------- !
    ! NOTE: residence time in each ocean layer must be estimated for the Parekh et al. [2005] model,
    !       with dum_bio_part passed in units of mol kg-1 for a specific cell
    !       BUT the mass this represents is actually spread across multiple cells during each time step
    !       i.e., in any cell, this density of material in effect exists only for a fraction of that time-step
    !       => normalize by the fraction of time spent in that cell during the time-step
    !          = residence time / time-step
    !            (could also be: cell thickness / (time-step x local velocity))
    ! NOTE: Dutkiewicz et al. [2005] scavenging rate par_scav_Fe_Ks has been converted to units of (yr-1)
    if (sed_select(is_POM_Fe)) then
       loc_part_den_POC = (conv_g_mg*conv_POC_mol_g*dum_bio_part(is2l(is_POC))/conv_kg_l) * dum_dt_scav/dum_dtyr
    else
       loc_part_den_POC = 0.0
    end if
    if (sed_select(is_CaCO3_Fe)) then
       loc_part_den_CaCO3 = (conv_g_mg*conv_cal_mol_g*dum_bio_part(is2l(is_CaCO3))/conv_kg_l) * dum_dt_scav/dum_dtyr
    else
       loc_part_den_CaCO3 = 0.0
    end if
    if (sed_select(is_opal_Fe)) then
       loc_part_den_opal  = (conv_g_mg*conv_opal_mol_g*dum_bio_part(is2l(is_opal))/conv_kg_l) * dum_dt_scav/dum_dtyr
    else
       loc_part_den_opal  = 0.0
    end if
    if (sed_select(is_det_Fe)) then
       loc_part_den_det   = (conv_g_mg*conv_det_mol_g*dum_bio_part(is2l(is_det))/conv_kg_l) * dum_dt_scav/dum_dtyr
    else
       loc_part_den_det   = 0.0
    end if
    !
    loc_part_den_tot = loc_part_den_POC + loc_part_den_CaCO3 + loc_part_den_opal + loc_part_den_det
    !
    if (loc_part_den_tot > const_real_nullsmall) then
       if (ctrl_bio_Fe_fixedKscav) then
          ! -------------------------------------------------- !
          ! calculate scavenging following Dutkiewicz et al. [2005]
          ! -------------------------------------------------- !
          ! -------------------------------------------------- ! net scavenging rate
          loc_scav_Fe_k_tot = par_scav_Fe_ks
          ! -------------------------------------------------- ! particle-specific scavenging rates
          loc_scav_Fe_k_POC = (loc_part_den_POC/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_CaCO3 = (loc_part_den_CaCO3/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_opal = (loc_part_den_opal/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_det = (loc_part_den_det/loc_part_den_tot)*loc_scav_Fe_k_tot
          ! -------------------------------------------------- ! calculate total Fe scavenged
          loc_scav_dFe_tot = dum_dtyr*loc_scav_Fe_k_tot*dum_ocn_Fe
       else
          ! -------------------------------------------------- !
          ! calculate scavenging following Parekh et al. [2005]
          ! -------------------------------------------------- !
          ! -------------------------------------------------- ! particle-specific scavenging rates
          loc_scav_Fe_k_POC   = par_scav_Fe_sf_POC*par_scav_Fe_k0*loc_part_den_POC**par_scav_Fe_exp
          loc_scav_Fe_k_CaCO3 = par_scav_Fe_sf_CaCO3*par_scav_Fe_k0*loc_part_den_CaCO3**par_scav_Fe_exp
          loc_scav_Fe_k_opal  = par_scav_Fe_sf_opal*par_scav_Fe_k0*loc_part_den_opal**par_scav_Fe_exp
          loc_scav_Fe_k_det   = par_scav_Fe_sf_det*par_scav_Fe_k0*loc_part_den_det**par_scav_Fe_exp
          ! -------------------------------------------------- ! net scavenging rate
          loc_scav_Fe_k_tot = loc_scav_Fe_k_POC + loc_scav_Fe_k_CaCO3 + loc_scav_Fe_k_opal + loc_scav_Fe_k_det
          ! -------------------------------------------------- ! calculate total Fe scavenged
          loc_scav_dFe_tot = dum_dtyr*loc_scav_Fe_k_tot*dum_ocn_Fe
       end if
       ! ----------------------------------------------------- !
       ! calculate Fe scavenged by particulates
       ! ----------------------------------------------------- !
       ! and update local remineralization array to take into account the removal of Fe from solution
       if (loc_scav_Fe_k_tot > const_real_nullsmall) then
          if (sed_select(is_POM_Fe)) then
             dum_bio_part(is2l(is_POM_Fe)) = &
                  & dum_bio_part(is2l(is_POM_Fe)) + (loc_scav_Fe_k_POC/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          end if
          if (sed_select(is_CaCO3_Fe)) then
             dum_bio_part(is2l(is_CaCO3_Fe)) = &
                  & dum_bio_part(is2l(is_CaCO3_Fe)) + (loc_scav_Fe_k_CaCO3/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          end if
          if (sed_select(is_opal_Fe)) then
             dum_bio_part(is2l(is_opal_Fe))  = &
                  & dum_bio_part(is2l(is_opal_Fe)) + (loc_scav_Fe_k_opal/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          end if
          if (sed_select(is_det_Fe)) then
             dum_bio_part(is2l(is_det_Fe))   = &
                  & dum_bio_part(is2l(is_det_Fe)) + (loc_scav_Fe_k_det/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          end if
          ! -------------------------------------------------- ! write removal of [Fe] to remin array
          dum_bio_remin(io2l(io_Fe)) = dum_bio_remin(io2l(io_Fe)) - loc_scav_dFe_tot
       end if
       ! ----------------------------------------------------- ! 56Fe
!!!
       ! ----------------------------------------------------- !
    end if
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_box_scav_Fe
  ! ****************************************************************************************************************************** !


END MODULE biogem_box
