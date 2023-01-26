! ******************************************************************************************************************************** !
! ecogem_box.f90
!
! MISCELLANEOUS ROUTINES
! ******************************************************************************************************************************** !


MODULE ecogem_box


  USE ecogem_lib
  IMPLICIT NONE
  SAVE

CONTAINS


  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  SUBROUTINE quota_status( &
       & biomass,            &
       & quota               &
       & )

    IMPLICIT NONE
    ! ---------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! ---------------------------------------------------------- !
    real,dimension(iomax,npmax),intent(in)  :: biomass
    real,dimension(iomax,npmax),intent(out) :: quota
    ! ---------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! ---------------------------------------------------------- !
    integer :: io
    !
    ! *****************************************************************
    ! ******************** Evaluate Quota Status **********************
    ! *****************************************************************

    ! Calculate quotas
    do io = 2,iomax ! skip carbon index; quota = X:C biomass ratio
       ! quota = nutrient biomass to carbon biomass ratio
       quota(io,:) = biomass(io,:) / biomass(iCarb,:)
       ! check for C biomass < 0
       quota(io,:) = merge(qmin(io,:),quota(io,:),biomass(iCarb,:).le.0.0)   ! Qmin if C biomass <=0
       ! check for outside quota min or max
       quota(io,:) = merge(qmax(io,:),quota(io,:),quota(io,:).gt.qmax(io,:)) ! Qmax if Q>Qmax
       quota(io,:) = merge(qmin(io,:),quota(io,:),quota(io,:).lt.qmin(io,:)) ! Qmin if Q<Qmin

    enddo

  END SUBROUTINE quota_status
  !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  SUBROUTINE quota_limitation( &
       & quota,              &
       & limit,              &
       & VLlimit,            &
       & qreg,               &
       & qreg_h              &
       & )

    IMPLICIT NONE
    ! ---------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! ---------------------------------------------------------- !
    real,dimension(iomax,npmax),intent(in)  :: quota
    real,dimension(iomax,npmax),intent(out) :: limit
    real,dimension(npmax)      ,intent(out) :: VLlimit
    real,dimension(iomax,npmax),intent(out) :: qreg
    real,dimension(iomax,npmax),intent(out) :: qreg_h
    ! ---------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! ---------------------------------------------------------- !
    integer :: io
    !
    ! *****************************************************************
    ! ******************** Evaluate Limitation ***********************
    ! *****************************************************************
    ! VLlimit --> Von Leibig limitation by most limiting nutrient
    !             1 = replete, 0 = no C assimilation
    !             N,Si   - linear from quota
    !             P & Fe - Droop from quota
    !
    ! qreg   --> individual nutrient status for uptake regualtion
    !            0 = quota full, 1 = quota empty
    !            linear for all elements (not needed for Si)
    !            qreg(C,:) = inverse of qreg for most limiting element
    ! qreg_h --> hill number transformed version (qreg_h=qreg^hill)
    !
    ! Initialise
    limit(:,:)       = 0.0 ! (iomax,npmax)
    VLlimit(:)       = 0.0 ! (npmax)
    qreg(:,:)        = 0.0 ! (iomax,npmax)
    qreg_h(:,:)      = 0.0 ! (iomax,npmax)

    ! Calculate quota limitation terms
    ! N and Si take linear form
    if (nquota) limit(iNitr,:) = (quota(iNitr,:) - qmin(iNitr,:)) / ( qmax(iNitr,:) - qmin(iNitr,:))
    if (squota) limit(iSili,:) = (quota(iSili,:) - qmin(iSili,:)) / ( qmax(iSili,:) - qmin(iSili,:))
    ! P and Fe take normalised Droop form
    if (pquota) limit(iPhos,:) = (1.0 - qmin(iPhos,:)/quota(iPhos,:)) / (1.0 - qmin(iPhos,:)/qmax(iPhos,:) )
    if (fquota) limit(iIron,:) = (1.0 - qmin(iIron,:)/quota(iIron,:)) / (1.0 - qmin(iIron,:)/qmax(iIron,:) )

    ! Set Von Leibig limitation according to most limiting nutrient (excluding iCarb=1)
    VLlimit(:) = minval(limit(2:iomax,:),1)

    do io = 2,iomax ! skip carbon index; quota = X:C biomass ratio
       ! Calculate linear regulation term
       qreg(io,:) = (qmax(io,:) - quota(io,:)) / (qmax(io,:) - qmin(io,:) )
       ! Transform regulation term using hill number
       qreg_h(io,:) = qreg(io,:) ** hill
    enddo

  END SUBROUTINE quota_limitation

  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  SUBROUTINE nutrient_uptake(         &
       &                        qreg,    &
       &                        nuts,    &
       &                        gamma_T, &
       &                        up_inorg )

    IMPLICIT NONE
    ! ---------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! ---------------------------------------------------------- !
    real,dimension(iomax,npmax),intent(in)    :: qreg
    real,dimension(iimax)      ,intent(in)    :: nuts
    real,                       intent(in)    :: gamma_T
    real,dimension(iimax,npmax),intent(inout) :: up_inorg
    ! ---------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! ---------------------------------------------------------- !
    integer :: ii,io
    !
    ! *****************************************************************
    ! ******************** Resource Acquisition ***********************
    ! *****************************************************************

    ! initialise
    up_inorg(2:iimax,:) = 0.0
    ! C-specific nutrient uptake
    do ii=2,iimax ! not carbon...
       ! resource and temperature limited uptake
       if (nuts(ii).gt.0.0) then
          up_inorg(ii,:) = gamma_T * vmax(ii,:) * affinity(ii,:) * nuts(ii) &
               & / (vmax(ii,:) + affinity(ii,:) * nuts(ii))
          ! Equivalent to classic Michaelis-Menten form ...
          !     up_inorg(ii,:) = gamma_T * vmax(ii,:) * nuts(ii) / (nuts(ii) +kn(ii,:))
          if (fundamental) up_inorg(ii,:) = gamma_T * vmax(ii,:)
       else
          up_inorg(ii,:) = 0.0
       endif
    enddo

    ! quota satiation
    do ii=2,iimax
       io=nut2quota(ii)
       up_inorg(ii ,:) = up_inorg(ii ,:) * qreg(io,:)
    enddo

    ! ammonium inhibition to NO3 and NO2
    if (useNH4) then
       if (useNO3) up_inorg(iNO3,:) = up_inorg(iNO3,:) * exp(-amminhib*nuts(iNH4))
       if (useNO2) up_inorg(iNO2,:) = up_inorg(iNO2,:) * exp(-amminhib*nuts(iNH4))
    endif
    ! check > 0.0
    up_inorg(:,:) = MERGE(up_inorg(:,:),0.0,up_inorg(:,:).gt.0.0)

  END SUBROUTINE nutrient_uptake

  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! Temperature dependence functions set by control namelist parameter ctrl_tdep_form
  SUBROUTINE t_limitation( &
       & Tlocal,           &
       & gamma_TP,         &
       & gamma_TK          &
       & )

    IMPLICIT NONE
    ! ---------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! ---------------------------------------------------------- !
    real,intent(in)  :: Tlocal
    real,intent(out) :: gamma_TP, gamma_TK
    
    if     (ctrl_tdep_form.eq.'Default')   then ! Ward, Dutkiewicz, Jahn & Follows - L&O (2012) 57(6), 1877-1891
    											! originally Laws et al. - GBC (2000) 14/4, 1231-1246
       gamma_TP=exp(temp_A*(Tlocal-273.15-temp_T0))
       gamma_TK=gamma_TP
       
    elseif (ctrl_tdep_form.eq.'Laws')    then ! Laws et al. - GBC (2000) 14/4, 1231-1246
       gamma_TP = exp(temp_P*(Tlocal-273.15-temp_T0))
       gamma_TK = exp(temp_K*(Tlocal-273.15-temp_T0))
    elseif (ctrl_tdep_form.eq.'Eppley')    then ! Eppley - Fish. Bull. (1972) 70, 1063-1085
       gamma_TP=0.59*exp(0.0633*(Tlocal-273.15))
       gamma_TK=gamma_TP
    elseif (ctrl_tdep_form.eq.'MEDUSA')    then ! Eppley - Fish. Bull. (1972) 70, 1063-1085
       gamma_TP=1.066**(Tlocal-273.15)
       gamma_TK=gamma_TP
    elseif (ctrl_tdep_form.eq.'Bissinger') then ! Bissinger, Montagnes, Sharples and Atkinson - L&O (2008) 53(2), 487-493
       gamma_TP=0.81*exp(0.0631*(Tlocal-273.15))
       gamma_TK=gamma_TP
    else
       print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       print*,"ERROR: ctrl_tdep_form = '"//trim(ctrl_tdep_form)//"' is not a valid temperature dependence function."
       print*,"Stopped in SUBROUTINE t_limitation (ecogem_box)."
       print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       STOP
    endif

    if (gamma_TP.le.0.0.or.gamma_TK.le.0.0) then
       print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       print*,"ERROR: T-dependence function yields gamma_T<=0."
       print*,"Stopped in SUBROUTINE t_limitation (ecogem_box)."
       print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       STOP
    endif

  END SUBROUTINE t_limitation

  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! GEIDER98 - Geider, MacIntyre and Kana (1998) photosynthesis model
  ! Calculates photosynthesis and chlorophyll synthesis.
  ! adapted for multiple nutrients following Moore et al (2002)
  SUBROUTINE photosynthesis(           &
       &                         PARlocal,  &
       &                         biomass,   &
       &                         limit,     &
       &                         VLlimit,   &
       &                         up_inorg,  &
       &                         gamma_T,   &
       &                         PP,        &
       &                         chlsynth,  &
       &                         totPP      &
       &                         )

    IMPLICIT NONE
    ! ---------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! ---------------------------------------------------------- !
    real,                            intent(in) ::PARlocal
    real,dimension(iomax+iChl,npmax),intent(in) ::biomass
    real,dimension(iomax,npmax),     intent(in) ::limit
    real,dimension(npmax),           intent(in) ::VLlimit
    real,dimension(iimax,npmax),     intent(in) ::up_inorg
    real,                            intent(in) ::gamma_T
    real,dimension(npmax),           intent(out)::PP
    real,dimension(npmax),           intent(out)::chlsynth
    real,                            intent(out)::totPP
    ! ---------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! ---------------------------------------------------------- !
    real,dimension(npmax)::isautotrophic
    real                 ::E0
    real,dimension(npmax)::Cbiomass,chl,VCN,Chl2C,costbiosynth
    real,dimension(npmax)::alpha,PCmax,PCPhot,rhochl

    ! initialise
    PP(:)       = 0.0
    chlsynth(:) = 0.0
    totPP       = 0.0

    if (ctrl_photosynth_form.eq.'Geider98') then ! Geider, MacIntyre and Kana - L&O (1998) 43(4), 679-694
       ! adapted for multiple nutrients following Moore, et al. - DSR2 (2002) 49, 403-462
       !-----------------------------------------------------------------
       E0 = PARlocal/0.2174  ! convert from W m^-2 to muEin m^-2 s^-1
       if (E0.gt.1.e-1) then ! only do if there is some meaningful amount of light
          ! Initialisation
          Cbiomass(:) = biomass(iCarb,:) ! mmol C m^-3
          chl(:)      = biomass(iChlo,:) ! mg Chl m^-3
          ! VCN = total N uptake --> sum of NO3, NO2 and NH4 uptake: (mmol N (mmol C)^-1 s^-1)
          VCN(:) = 0.0
          if (nquota) then
             if (useNO3)  VCN(:) = VCN(:) + up_inorg(iNO3,:)
             if (useNO2)  VCN(:) = VCN(:) + up_inorg(iNO2,:)
             if (useNH4)  VCN(:) = VCN(:) + up_inorg(iNH4,:)
          elseif (pquota) then
             VCN(:) = up_inorg(iPO4,:) * 16.0
          else
             print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
             print*,"ERROR: Neither nquota nor pquota are set. Needed for chlorophyll synthesis"
             print*,"Stopped in SUBROUTINE photosynthesis (ecogem_box)."
             print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
             STOP
          endif
          ! iron limitation of light capture: (mmol C (mg chl a)^-1 (mu Ein m^-2)^-1)
          if (useFe) then
             alpha(:) = alphachl(:) * limit(iIron,:)
          else
             alpha(:) = alphachl(:) ! mmol C (mg chl a)^-1 (mu Ein m^-2)^-1
          endif
          ! Photosynthesis Calculations
          ! Chl2C ratio: (mg chl (mmol C)^-1)
          Chl2C(:) = chl(:) / Cbiomass(:)
          Chl2C(:) = MERGE(Chl2C(:),0.0,Cbiomass(:).gt.0.0) ! Check for divide by zero
          ! theoretical light replete photosynthesis given current temperature and nutrient limitation: (s^-1)
          PCmax(:) = vmax(iDIC,:) * VLlimit(:) * gamma_T
          ! light-limited photosynthesis: (s^-1)
          PCPhot(:) = PCmax(:) * (1.0 - exp(-alpha(:)*Chl2C(:)*E0/PCmax))
          PCPhot(:) = MERGE(PCPhot(:),0.0,PCmax.gt.0.0) ! Check for divide by zero
          ! Chlorophyll Synthesis Calculations
          ! Chlorophyll synthesis per mole N uptake: (mg chl (mmol N)^-1)
          rhochl(:) = Chl2Nmax * PCPhot(:) / (alpha(:)*Chl2C(:)*E0)
          rhochl(:) = MERGE(rhochl(:),0.0,(alpha(:)*Chl2C(:)*E0).gt.0.0) ! Check for divide by zero
          ! Chlorophyll synthesis rate: (mg chl m^-3 s^-1)
          chlsynth(:) = rhochl(:) * VCN(:) * Cbiomass(:)
          ! Cost of biosynthesis
          costbiosynth(:) = biosynth*VCN(:)  ! s^-1
          ! Mask strict heterotrophs
          isautotrophic = MERGE(1.0,0.0,autotrophy(:).gt.0.0)
          ! Output variables
          chlsynth(:) =  chlsynth(:) * isautotrophic
          PP(:)       = (PCPhot(:) - costbiosynth(:)) * isautotrophic
          if (fundamental) PP(:) = PCPhot(:) * isautotrophic ! ignore cost of biosynthesis in fund. niche experiment
          totPP       =  sum(PCPhot(:) * Cbiomass(:) * isautotrophic) ! does not include cost of biosynthesis
          !-----------------------------------------------------------------
       else ! else if it is extremely dark
          PP(:)       = 0.0 ! s^-1
          chlsynth(:) = 0.0 ! mg chl m^-3 s^-1
          totPP       = 0.0
       endif
       !-----------------------------------------------------------------
    else
       print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       print*,"ERROR: ctrl_photosynth_form = '"//trim(ctrl_photosynth_form)//"' is not a valid photosynthesis function."
       print*,"Stopped in SUBROUTINE photosynthesis (ecogem_box)."
       print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       STOP
    endif

  END SUBROUTINE photosynthesis

  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  SUBROUTINE grazing(          &
       &                  biomass,  &
       &                  gamma_T,  &
!BAW: zoolimit should be optional &                  zoolimit,  &
       &                  GrazingMat &
       &                 )

    IMPLICIT NONE
    ! ---------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! ---------------------------------------------------------- !
    real,                                  intent(in)  :: gamma_T
    real,dimension(iomax+iChl,npmax)      ,intent(in)  :: biomass
    real,dimension(iomax+iChl,npmax,npmax),intent(out) :: GrazingMat
!BAW: zoolimit should be optional     real,dimension(npmax,npmax),intent(out)            :: zoolimit
    ! ---------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! ---------------------------------------------------------- !
    integer                         :: io,jpred,jprey
    real   ,dimension(npmax)        :: Refuge
    real                            :: tmp1,food1,food2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Matrix based version
    !real   ,dimension(npmax,npmax)  :: Food,Saturation
    !real   ,dimension(npmax,npmax)  :: PreyMat,SwitchMatTop,SwitchMatBot,Switching,X2CRatioMat
    !real   ,dimension(1,npmax)      :: onerow,X2CRatio,PreyC
    !real   ,dimension(npmax,1)      :: onecol,AllGraz,SwitchCol
    !!   PRE-ASSIMILATION grazing of type jpredator by type jprey
    !    GrazingMat(:,:,:)     = 0.0
    !!   Vectors for matrix padding
    !    onerow                = 1.0                                                   ! Row vector
    !    onecol                = 1.0                                                   ! Column vector
    !    PreyC(1,:)            = biomass(iCarb,:)                                      ! Prey biomass vector [1,jprey]
    !!   Predator-dependent (Prey-integrated) terms
    !    Food(:)               = matmul(PreyC(1,:),gkernelT(:,:))                      ! Sum of available prey for each predator [1,jpred] (using transpose of gkernel)
    !    Refuge(:)             = (1.0 - exp(Lambda * Food(:)))                         ! Prey Refuge [1,jpred]
    !    Saturation(:)         = Food(:) / (Food(:)+kg(:))                             ! Saturation of predator feeding by available prey [1,jpred]
    !    Saturation(:)         = MERGE(Saturation(:),0.0,(Food(:)+kg(:)).gt.0.0)       ! check for divide by zeros [ MERGE(TrueSOURCE,FalseSOURCE,CONDITION) ]
    !!   Prey biomass dependent switching
    !    PreyMat(:,:)          = matmul(onecol(:,1:1),PreyC(1:1,:))                    ! Prey biomass replicated in matrix for each predator [jpred,jprey]
    !    SwitchMatTop(:,:)     = (PreyMat(:,:) * gkernel)**ns                          ! Passive (ns=1) or active (ns=2) switching term [jpred,jprey]
    !    SwitchCol(:,1)        = sum(SwitchMatTop,2)                                   ! Sum of switching terms for each predator [jpred,1]
    !    SwitchMatBot(:,:)     = matmul(SwitchCol(:,1:1),onerow(1:1,:))                ! Sum of switching terms replicated in matrix for each prey [jpred,jprey]
    !    Switching(:,:)        = SwitchMatTop(:,:) / SwitchMatBot(:,:)                 ! Switching matrix [jpred,jprey]
    !    Switching(:,:)        = MERGE(Switching(:,:),0.0,SwitchMatBot(:,:).gt.0.0)    ! check for divide by zeros
    !!   Final Grazing Matrix
    !    AllGraz(:,1)          = graz(:) * gamma_T * Saturation(:) * Refuge(:)         ! Temperature limited grazing attack rate down-regulated by saturation and prey refugia  [jpred,1]
    !    GrazingMat(iCarb,:,:) = matmul(AllGraz(:,1:1),onerow(1:1,:)) * Switching(:,:) ! Pad out to matrix and multiply by switching term!  [jpred,jprey]
    !!   Other quotas by prey stoichiometry
    !    do io=2,iomax+iChl
    !      X2CRatio(1,:)       = biomass(io,:) / biomass(iCarb,:)                      ! [1,jprey]
    !      X2CRatio(1,:)       = MERGE(X2CRatio(1,:),0.0,biomass(iCarb,:).gt.0.0)      ! check for divide by zeros
    !      X2CRatioMat(:,:)    = matmul(onecol,X2CRatio)                               ! [jpred,jprey]
    !      GrazingMat(io,:,:)  = GrazingMat(iCarb,:,:) * X2CRatioMat(:,:)              ! [jpred,jprey]
    !    enddo
    !    ! !!!
    !    GrazingMat(:,:,:)     = MERGE(GrazingMat(:,:,:),0.0,GrazingMat(:,:,:).gt.0.0) ! check for -ve values
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Loop based version
    GrazingMat(:,:,:) = 0.0
    Refuge(:) = 0.0
    tmp1      = 0.0
    do jpred=1,npmax ! loop predators
       if (graz(jpred).gt.0.0) then ! if non-zero max grazing rate
          food1 = 0.0 ! available food
          food2 = 0.0 ! available food ^ ns
          do jprey=1,npmax ! sum all the prey carbon of predator, weighted by availability (preference)
             if (gkernel(jpred,jprey).gt.0.0) then
                food1 = food1 +  gkernel(jpred,jprey)*palatability(jprey)*biomass(iCarb,jprey)      ! available food
                food2 = food2 + (gkernel(jpred,jprey)*palatability(jprey) * biomass(iCarb,jprey))**ns_array(jpred) ! available food ^ ns
             endif
          enddo
          ! calculate grazing effort
          if (food1 + kg(jpred).gt.0.0) then
             Refuge(jpred) = (1.0 - exp(Lambda * food1)) ! pref refuge function
             tmp1  = food1 / (food1 + kg(jpred)) * Refuge(jpred) ! grazing "effort" (max 1*gamma_T)
          endif
          ! loop prey to calculate grazing rates on each prey and element
          do jprey=1,npmax
             if (biomass(iCarb,jprey).gt.0.0.and.food2.gt.0.0) then ! if any prey food available
                GrazingMat(iCarb,jpred,jprey) = tmp1 * gamma_T * graz(jpred)   &                        ! total grazing rate
                     &             * (gkernel(jpred,jprey)*palatability(jprey)*biomass(iCarb,jprey))**ns_array(jpred)/food2 ! * switching
!BAW: zoolimit should be optional zoolimit(jpred,jprey) = tmp1 *(gkernel(jpred,jprey)*palatability(jprey)*biomass(iCarb,jprey))**ns_array(jpred)/food2 ! food limitation calulation for zooplankton - Maria May 2019
                ! other organic elements (+ chlorophyll) are grazed in stoichiometric relation to carbon
                do io=2,iomax+iChl
                   if (biomass(iCarb,jprey).gt.0.0) then
                      GrazingMat(io,jpred,jprey) = GrazingMat(iCarb,jpred,jprey) &
                           &                                      * biomass(io,jprey)/biomass(iCarb,jprey)
                   endif
                enddo ! io
             endif ! endif any food available
          enddo ! jprey
       endif  ! endif non-zero max grazing rate
    enddo ! jpred

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  END SUBROUTINE grazing

  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  !ckc fractiontion to calculate nutrient isotopes uptake rates based on up_inorg
  SUBROUTINE nut_fractionation (           &
       up_inorg,    &
       nuts,    &
       nutiso,  &
       diameter,    &
       up_inorgiso  )

    IMPLICIT NONE
    ! ---------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! ---------------------------------------------------------- !
    real,dimension(iimax,npmax),     intent(in) ::up_inorg
    real,dimension(iimax),           intent(in) ::nuts
    real,dimension(iimaxiso),        intent(in) ::nutiso
    real,dimension(npmax),           intent(in) ::diameter
    real,dimension(iimaxiso,npmax),  intent(out)::up_inorgiso
    ! ---------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! ---------------------------------------------------------- !
    real                             :: fract ! fractionation value


    fract = -25.0  !test
    ! size dependent, need mean size for phytoplankton
    up_inorgiso(iDIC_13C,:) = up_inorg(iDIC,:) * 0.0109 !ckc for testing
    !print*, "up inorgio iDIC_13C  ", up_inorgiso(iDIC_13C,:)

  END SUBROUTINE nut_fractionation

  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! CHECK_EGBG_COMPATIBLE
  ! checks nutrients required by ECOGEM are carried by BIOGEM
  SUBROUTINE check_egbg_compatible()

    implicit none

    logical :: errflag
    integer :: nstr
    character(500) :: errmsg

    errflag=.false.
    errmsg=''
    nstr=0
    if (useNO3  .and. .not. ocn_select(io_NO3) ) then
       errflag=.true.
       errmsg=errmsg(1:nstr)//'NO3, '
       nstr=nstr+5
    endif
    if (useNO2  .and. .not. ocn_select(io_NO2) ) then
       errflag=.true.
       errmsg=errmsg(1:nstr)//'NO2, '
       nstr=nstr+5
    endif
    if (useNH4  .and. .not. ocn_select(io_NH4) ) then
       errflag=.true.
       errmsg=errmsg(1:nstr)//'NH4, '
       nstr=nstr+5
    endif
    if (usePO4  .and. .not. ocn_select(io_PO4) ) then
       errflag=.true.
       errmsg=errmsg(1:nstr)//'PO4, '
       nstr=nstr+5
    endif
    if (useFe  .and. .not. ocn_select(io_TDFe) ) then
      errflag=.true.
      errmsg=errmsg(1:nstr)//'TDFe), '
      nstr=nstr+6
    endif
    if (useSiO2 .and. .not. ocn_select(io_SiO2)) then
       errflag=.true.
       errmsg=errmsg(1:nstr)//'SiO2  '
       nstr=nstr+6
    endif

    if (errflag) then
       print*," "
       print*,"! ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       print*,"! ECOGEM wants a nutrient that BIOGEM doesn't have!"
       print*,"! Specifically: ",errmsg(1:nstr-2)
       print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       print*,"! Change either supply (BIOGEM), or demand (ECOGEM)"
       stop
    endif

  END SUBROUTINE check_egbg_compatible
  ! ****************************************************************************************************************************** !
  ! convert a word to lower case
  subroutine lower_case(word)
    character (len=*) , intent(in out) :: word
    integer                            :: i,ic,nlen
    nlen = len(word)
    do i=1,nlen
       ic = ichar(word(i:i))
       if (ic >= 65 .and. ic <= 90) word(i:i) = char(ic+32)
    end do
  end subroutine lower_case
  ! ****************************************************************************************************************************** !

END MODULE ecogem_box
