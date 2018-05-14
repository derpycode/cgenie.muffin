! ******************************************************************************************************************************** !
! sedgem_box_ridgwell2001_sedflx.f90
! Opal sediment diagenesis routines
! ******************************************************************************************************************************** !


MODULE sedgem_box_ridgwell2001_sedflx

  
  use genie_control
  USE gem_cmn
  IMPLICIT NONE
  SAVE
  

  ! *** global configuration parameters ***
  ! set the maximum real number precision to be used
  INTEGER,PARAMETER::nzmax = 52                              ! total number of sediment sub-layers
  INTEGER,PARAMETER::kmax = 51                               ! number of sediment sub-layers used to calculate dissolution flux
  ! constants
  REAL,PARAMETER::T0 = 273.15                                ! 
  REAL,PARAMETER::DSi0 = 4.59E-06                            ! [Si] diffusivity (cm2 s-1) in free water at 0'C
  REAL,PARAMETER::BSi0 = 4.75E-09                            ! bioturbational diffusivity (cm2 s-1) (== 150 cm2 kyr-1)
  REAL,PARAMETER::df_thesh = 1.0E-5                          ! threshold for virtual steady-state (== 0.001%)
  REAL,PARAMETER::t_step = 2.0E+06                           ! time step (s)
  INTEGER,PARAMETER::t_loop_max = 1000000                    ! maximum allowed time steps


CONTAINS


! *************************************************************************************************
! *** sedflx **************************************************************************************
! *************************************************************************************************
  function fun_ridgwell2001_sedflx(dum_frac_opal,dum_SiO2,dum_T,dum_KSi0,dum_opaltorefrac)
    ! result variable
    real::fun_ridgwell2001_sedflx
    ! dummy variables
    REAL,INTENT(IN)::dum_frac_opal,dum_SiO2,dum_T,dum_KSi0,dum_opaltorefrac
    ! local variables
    INTEGER::t
    REAL::conc_Si_eq
    REAL::fSi_new,fSi_old
    REAL,DIMENSION(nzmax)::delz
    REAL,DIMENSION(nzmax)::z
    REAL,DIMENSION(nzmax)::sed_pore
    REAL,DIMENSION(nzmax)::sed_conc_Si
    REAL,DIMENSION(nzmax)::DSi
    REAL,DIMENSION(nzmax)::KSi

    ! *** set local variables ***
    ! configuration parameters
    delz(:) = 10.0 / (kmax - 1.0)
    delz(1) = 0.0
    delz(kmax + 1:nzmax) = 1.0
    ! zero variables
    conc_Si_eq = 0.0
    fSi_new = 0.0
    fSi_old = 0.0

    ! ***  ***
    ! calculate sub-layer mid-depth profile
    z(:) = calc_z(z(:))
    ! initialize sediment system
    CALL init_sedflx_Si(dum_SiO2,sed_conc_Si(:))
    ! calculate porosity profile
    sed_pore(:) = calc_pore(z(:))
    ! calculate [Si] diffusivity profile
    DSi(:) = calc_DSi(dum_T,sed_pore(:))
    ! calculate equilibrium [Si]
    conc_Si_eq = calc_conc_Si_eq(dum_T)
    ! convert equilibriun [Si] to sediment asymptotic [Si] according sediment detrital content
    conc_Si_eq = conv_si_eqlibtoasympt(dum_opaltorefrac)*conc_Si_eq
    ! calculate silica dissolution flux
    DO t = 1,t_loop_max
       !
       fSi_old = fSi_new
       ! calculate Si dissolution rate constant profile
       !    KSi(:) = calc_KSi(dum_KSi0,conc_Si_eq,sed_conc_Si(:))
       KSi(:) = calc_KSi_alt(z(:),dum_KSi0,dum_T,conc_Si_eq,sed_conc_Si(:))
       ! calculate Si fluxes
       ! update [Si] profile
       CALL runge_kutta_4_opal(t_step,dum_frac_opal,sed_conc_Si,sed_pore,delz,DSi,KSi)
       ! test Si sediment interface flux
       fSi_new = DSi(2) * conv_cm3_kg * (sed_conc_Si(2) - sed_conc_Si(1)) / (0.5 * delz(2))
       IF (ABS((fSi_new - fSi_old) / fSi_new) <= df_thesh) EXIT
    ENDDO
    !
    fun_ridgwell2001_sedflx = fSi_new
    
  END function fun_ridgwell2001_sedflx
! *************************************************************************************************


! ******************************************
! *** calculate sediment sub-layer depth ***
! ******************************************
  FUNCTION calc_z(delz)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_z
! dummy arguments
    REAL,INTENT(in),DIMENSION(nzmax)::delz
! local variables
    INTEGER::k
! calculate z and return function value
    calc_z(1) = 0.0
    DO k = 2,kmax
      calc_z(k) = 0.5 * delz(k) + SUM(delz(1:k - 1))
    END DO
  END FUNCTION calc_z
! ******************************************


! *******************************************
! *** calculate sediment porosity profile ***
! *******************************************
  FUNCTION calc_pore(z)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_pore
! dummy arguments
    REAL,INTENT(in),DIMENSION(nzmax)::z
! local variables
    INTEGER::k
! calculate porosity profile and return function value
    calc_pore(1) = 1.0
    DO k = 2,kmax
      calc_pore(k) = 0.690 + 0.260 * (z(k) + 1.0)**(-1.2)
    ENDDO
    calc_pore(kmax + 1:nzmax) = calc_pore(kmax)
  END FUNCTION calc_pore
! *******************************************


! *********************************************************
! *** calculate sediment porosity profile (alternative) ***
! *********************************************************
  FUNCTION calc_pore_alt(sedconcCaCO3,z)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_pore_alt
! dummy arguments
    REAL,INTENT(in)::sedconcCaCO3
    REAL,INTENT(in),DIMENSION(nzmax)::z
! local variables
    INTEGER::k
! calculate porosity profile and return function value
    calc_pore_alt(1) = 1.0
    DO k = 2,kmax
      calc_pore_alt(k) = 0.690 + 0.260 * EXP(-z(k) / (21.5 - 20.0 * sedconcCaCO3))
    ENDDO
    calc_pore_alt(kmax + 1:nzmax) = calc_pore_alt(kmax)
  END FUNCTION calc_pore_alt
! *********************************************************


! ******************************************
! *** calculate opal solubility constant ***
! ******************************************
  FUNCTION calc_conc_Si_eq(botT)
    IMPLICIT NONE
! result variable
    REAL::calc_conc_Si_eq
! dummy arguments
    REAL,INTENT(in)::botT
! calculate conc_Si_eq and return function value
! NOTE: change units from (umol kg-1) to (mol kg-1)
    calc_conc_Si_eq = 10.0**(6.44 - 968.0 / botT)
    calc_conc_Si_eq = 1.0E-06 * calc_conc_Si_eq
  END FUNCTION calc_conc_Si_eq
! ******************************************


! ************************************************************
! *** convert equilibriun [Si] to sediment asymptotic [Si] ***
! ************************************************************
  FUNCTION conv_si_eqlibtoasympt(opaltorefrac)
    IMPLICIT NONE
! result variable
    REAL::conv_si_eqlibtoasympt
! dummy arguments
    REAL,INTENT(in)::opaltorefrac
! convert equilibriun [Si] to sediment asymptotic [Si] according sediment detrital content
    conv_si_eqlibtoasympt = 1.0 - (0.045 * opaltorefrac)**0.60
  END FUNCTION conv_si_eqlibtoasympt
! ************************************************************


! ******************************************************
! *** calculate Si dissolution rate constant profile ***
! ******************************************************
  FUNCTION calc_KSi(KSi0,concSieq,sedconcSi)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_KSi
! dummy arguments
    REAL,INTENT(in)::KSi0
    REAL,INTENT(in)::concSieq
    REAL,INTENT(in),DIMENSION(nzmax)::sedconcSi
! local variables
    INTEGER::k
    REAL::r_Si
! calculate KSi and return function value
    calc_KSi(1) = 0.0
    DO k = 2,kmax
      IF (sedconcSi(k) > concSieq) THEN
        calc_KSi(k) = 0.0
      ELSE
        r_Si = (concSieq - sedconcSi(k)) / concSieq
        calc_KSi(k) = KSi0 * r_Si
      ENDIF
    END DO
    calc_KSi(kmax + 1:nzmax) = 0.0
  END FUNCTION calc_KSi
! ******************************************************


! ********************************************************************
! *** calculate Si dissolution rate constant profile (alternative) ***
! ********************************************************************
  FUNCTION calc_KSi_alt(z,KSi0,botT,concSieq,sedconcSi)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_KSi_alt
! dummy arguments
    REAL,INTENT(in),DIMENSION(nzmax)::z
    REAL,INTENT(in)::KSi0
    REAL,INTENT(in)::botT
    REAL,INTENT(in)::concSieq
    REAL,INTENT(in),DIMENSION(nzmax)::sedconcSi
! local variables
    INTEGER::k
    REAL::T_C
    REAL::r_Si
! calculate local constants
    T_C = botT - const_zeroC
! calculate KSi and return function value
! NOTE: exponential decay of kSi with down-core depth after Van Cappellen and Qiu [1997b]
! NOTE: non-linear dependence of kSi with (concSieq - sedconcSi(k)) / concSieq,
!       (plus additional ad hoc temperature dependence), after Van Cappellen and Qiu [1997b]
    calc_KSi_alt(1) = 0.0
    DO k = 2,kmax
      IF (sedconcSi(k) > concSieq) THEN
        calc_KSi_alt(k) = 0.0
      ELSE
        r_Si = (concSieq - sedconcSi(k)) / concSieq
        calc_KSi_alt(k) = KSi0 * (0.25 + 0.75 * EXP(-z(k) / 6.0)) * &
          & (0.160 * (1.0 + T_C / 15.0) * r_Si + 0.5 * ((1.0 + T_C / 400.0)**4.0 * r_Si)**7.0) / 0.660
      ENDIF
    END DO
    calc_KSi_alt(kmax + 1:nzmax) = 0.0
  END FUNCTION calc_KSi_alt
! ********************************************************************


! ******************************************
! *** calculate [Si] diffusivity profile ***
! ******************************************
  FUNCTION calc_DSi(botT,sedpore)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_DSi
! dummy arguments
    REAL,INTENT(in)::botT
    REAL,INTENT(in),DIMENSION(nzmax)::sedpore
! local variables
    INTEGER::k
    REAL,DIMENSION(nzmax)::form ! formation factor [Ullman and Aller, 1982]
! calculate [Si] diffusivity profile and return function value
! NOTE: [Si] molecular diffusion coefficient (at 0'C) adjusted for temperature
!       (adapted from Hensen et al. [1998])
! NOTE: set the k=1 sub-layer diffusivity equal to that at k=2
! NOTE: calc_DSi(k) = DSi0 / (sedpore(k) * form(k)) : Ullman and Aller [1982]
!       (equivalent to DSi0 * sedpore(k)**1.5)
! NOTE: calc_DSi(k) = DSi0 * sedpore(k)**1.333 : Millington [1959]
    DO k = 2,kmax
      form(k) = 1.0 / (sedpore(k)**2.5)
      calc_DSi(k) = (DSi0 + 1.74E-07 * (botT - const_zeroC)) / (sedpore(k) * form(k))
    ENDDO
    calc_DSi(1) = DSi0 + 1.74E-07 * (botT - const_zeroC)
    calc_DSi(kmax + 1:nzmax) = calc_DSi(kmax)
  END FUNCTION calc_DSi
! ******************************************


! *****************************************
! *** initialize opal diagenesis scheme ***
! *****************************************
  SUBROUTINE init_sedflx_Si(botconcSi,sedconcSi)
    IMPLICIT NONE
! dummy arguments
    REAL,INTENT(in)::botconcSi
    REAL,INTENT(inout),DIMENSION(nzmax)::sedconcSi
! initialzie [Si] to ambient
    sedconcSi(:) = botconcSi
  END SUBROUTINE init_sedflx_Si
! *****************************************


! ******************************************************************
! *** calculate sediment Si distribution change due to diffusion ***
! ******************************************************************
  FUNCTION calc_dsed_Si_diff(DSi,sedconcSi,delz)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_dsed_Si_diff
! dummy arguments
    REAL,INTENT(in),DIMENSION(nzmax)::DSi
    REAL,INTENT(in),DIMENSION(nzmax)::sedconcSi
    REAL,INTENT(in),DIMENSION(nzmax)::delz
! local variables
    INTEGER::k
! calculate change in [Si] in each sub-layer pore-space and return function value
! NOTE: use average Si diffusivity
! NOTE: as [Si] is in stored in units of (mol kg-1),
!       the units must be converted into (mol cm-3) in order to calculate the Si flux
    calc_dsed_Si_diff(1) = 0.0
    DO k = 2,kmax
      calc_dsed_Si_diff(k) = &
        & 0.5 * (DSi(k + 1) + DSi(k)) * &
        & (sedconcSi(k + 1) - sedconcSi(k)) / (0.5 * (delz(k + 1) + delz(k))) + &
        & 0.5 * (DSi(k - 1) + DSi(k)) * &
        & (sedconcSi(k - 1) - sedconcSi(k)) / (0.5 * (delz(k - 1) + delz(k)))
    ENDDO
    calc_dsed_Si_diff(kmax + 1:nzmax) = 0.0
    calc_dsed_Si_diff(:) = conv_cm3_kg * calc_dsed_Si_diff(:)
  END FUNCTION calc_dsed_Si_diff
! ******************************************************************


! ********************************************************************
! *** calculate sediment Si distribution change due to dissolution ***
! ********************************************************************
  FUNCTION calc_dsed_Si_diss(sedconcopal,delz,sedpore,KSi)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_dsed_Si_diss
! dummy arguments
    REAL,INTENT(in)::sedconcopal
    REAL,INTENT(in),DIMENSION(nzmax)::delz
    REAL,INTENT(in),DIMENSION(nzmax)::sedpore
    REAL,INTENT(in),DIMENSION(nzmax)::KSi
! local variables
    INTEGER::k
! return function value
    calc_dsed_Si_diss(1) = 0.0
    DO k = 2,kmax
      calc_dsed_Si_diss(k) = KSi(k) * sedconcopal * delz(k) * (1.0 - sedpore(k)) * conv_opal_cm3_mol
    ENDDO
    calc_dsed_Si_diss(kmax + 1:nzmax) = 0.0
  END FUNCTION calc_dsed_Si_diss
! ********************************************************************


! *********************************************************************
! *** calculate sediment Si distribution change due to bioturbation ***
! *********************************************************************
  FUNCTION calc_dsed_Si_biot(sedconcSi,delz)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_dsed_Si_biot
! dummy arguments
    REAL,INTENT(in),DIMENSION(nzmax)::sedconcSi
    REAL,INTENT(in),DIMENSION(nzmax)::delz
! local variables
    INTEGER::k
    REAL,DIMENSION(nzmax)::temp_sedconcSi
! make a temporary sediment [Si] profile
! NOTE: set the k = 1 layer [Si] to that of k = 2
    temp_sedconcSi(:) = sedconcSi(:)
    temp_sedconcSi(1) = sedconcSi(2)
! calculate change in [Si] in each sub-layer pore-space and return function value
! NOTE: assume a uniform bioturbation rate with depth in the sediments
! NOTE: as [Si] is in stored in units of (mol kg-1),
!       the units must be converted into (mol cm-3) in order to calculate the Si flux
    calc_dsed_Si_biot(1) = 0.0
    DO k = 2,kmax
      calc_dsed_Si_biot(k) = BSi0 * ( &
        & (sedconcSi(k + 1) - sedconcSi(k)) / (0.5 * (delz(k + 1) + delz(k))) + &
        & (sedconcSi(k - 1) - sedconcSi(k)) / (0.5 * (delz(k - 1) + delz(k))) )
    ENDDO
    calc_dsed_Si_biot(kmax + 1:nzmax) = 0.0
    calc_dsed_Si_biot(:) = conv_cm3_kg * calc_dsed_Si_biot(:)
  END FUNCTION calc_dsed_Si_biot
! *********************************************************************


! ****************************************************
! *** convert 2-digit number into character string ***
! ****************************************************
  FUNCTION conv_num_char(n)
    IMPLICIT NONE
! result variable
    CHARACTER(LEN=2)::conv_num_char
! dummy valiables
    INTEGER,INTENT(in)::n
! return value
    IF (n <= 9) THEN
      WRITE(conv_num_char(2:2),'(i1)')n
      conv_num_char(1:1) = "0"
    ELSE
      WRITE(conv_num_char(1:2),'(i2)')n
    ENDIF
  END FUNCTION conv_num_char
! ****************************************************


! *********************************
! *** [Si] dierivative function ***
! *********************************
  FUNCTION calc_dSidt(sedconcopal,sedconcSi,sedpore,delz,DSi,KSi)
    IMPLICIT NONE
! result variable
    REAL,DIMENSION(nzmax)::calc_dSidt
! dummy variables
    REAL,INTENT(in)::sedconcopal
    REAL,INTENT(in),DIMENSION(nzmax)::sedconcSi
    REAL,INTENT(in),DIMENSION(nzmax)::sedpore
    REAL,INTENT(in),DIMENSION(nzmax)::delz
    REAL,INTENT(in),DIMENSION(nzmax)::DSi
    REAL,INTENT(in),DIMENSION(nzmax)::KSi
! local variables
    INTEGER::k
    REAL,DIMENSION(nzmax)::dsed_Si_diff
    REAL,DIMENSION(nzmax)::dsed_Si_diss
    REAL,DIMENSION(nzmax)::dsed_Si_biot
!
    dsed_Si_diff = calc_dsed_Si_diff(DSi,sedconcSi,delz)
    dsed_Si_diss = calc_dsed_Si_diss(sedconcopal,delz,sedpore,KSi)
    dsed_Si_biot = calc_dsed_Si_biot(sedconcSi,delz)
!
    calc_dSidt(1) = 0.0
    DO k = 2,kmax
      calc_dSidt(k) = (dsed_Si_diff(k) + dsed_Si_diss(k) + dsed_Si_biot(k)) / (sedpore(k) * delz(k))
    END DO
    calc_dSidt(kmax + 1:nzmax) = calc_dSidt(kmax)
  END FUNCTION calc_dSidt
! *********************************


! *******************************************************************
! *** 4th order Runge-Kutta differential equation solving routine ***
! *******************************************************************
  SUBROUTINE runge_kutta_4_opal(dt,sedconcopal,sedconcSi,sedpore,delz,DSi,KSi)
    IMPLICIT NONE
! dummy variables
    REAL,INTENT(in)::dt
    REAL,INTENT(in)::sedconcopal
    REAL,INTENT(inout),DIMENSION(nzmax)::sedconcSi
    REAL,INTENT(in),DIMENSION(nzmax)::sedpore
    REAL,INTENT(in),DIMENSION(nzmax)::delz
    REAL,INTENT(in),DIMENSION(nzmax)::DSi
    REAL,INTENT(in),DIMENSION(nzmax)::KSi
! local variables
    REAL,DIMENSION(nzmax)::conc_Si0
    REAL,DIMENSION(nzmax)::k1,k2,k3,k4
! store initial value of Si
    conc_Si0 = sedconcSi
! calculate coefficient 'k1'
    k1 = dt * calc_dSidt(sedconcopal,conc_Si0,sedpore,delz,DSi,KSi)
! calculate coefficient 'k2'
    k2 = dt * calc_dSidt(sedconcopal,conc_Si0 + 0.5 * k1,sedpore,delz,DSi,KSi)
! calculate coefficient 'k3'
    k3 = dt * calc_dSidt(sedconcopal,conc_Si0 + 0.5 * k2,sedpore,delz,DSi,KSi)
! calculate coefficient 'k4'
    k4 = dt * calc_dSidt(sedconcopal,conc_Si0 + k3,sedpore,delz,DSi,KSi)
! estimate the value at forward time t + dt
    sedconcSi = conc_Si0 + (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0
  END SUBROUTINE runge_kutta_4_opal
! *******************************************************************


END MODULE sedgem_box_ridgwell2001_sedflx
