! ******************************************************************************************************************************** !
! sedgem_box_archer1991_sedflx.f90
! David Archer's (1991) sediment diagenesis routines
! ******************************************************************************************************************************** !


MODULE sedgem_box_archer1991_sedflx


  use genie_control
  USE sedgem_lib
  IMPLICIT NONE
  SAVE


  INTEGER,PARAMETER::nzmax     = 10
  INTEGER,PARAMETER::kmax      = 7
  INTEGER,PARAMETER::nzmax_co3 = 30
  INTEGER,PARAMETER::nmax      = 300
  REAL,PARAMETER,DIMENSION(nzmax)::delz = (/0.0,0.5,0.5,1.0,2.0,3.0,3.0,5.0,5.0,5.0/)

  
CONTAINS


! *************************************************************************************************
! *** sedflx **************************************************************************************
! *************************************************************************************************

! uses a specified calcite concentration to find the
! instantaneous flux from bottom water chemistry
! and the organic carbon rain.  assumes that org
! adjusts quickly.

  function fun_archer1991_sedflx(o2bw,calpc,rainorg,co2,hco3,co3,k1,k2,calsat,db)
    ! result variable
    real::fun_archer1991_sedflx
    ! dummy variables
    REAL,INTENT(IN)::o2bw,calpc,rainorg
    REAL,INTENT(IN)::co2,hco3,co3
    REAL,INTENT(IN)::k1,k2
    REAL,INTENT(IN)::calsat
    REAL,INTENT(IN)::db ! sediment mixing rate, cm2/yr
    ! local variables
    INTEGER::j,l
    REAL::difo2,rc,dissc,dissn,expb,zrct
    real::diftc,difal
    real::ttrorg,ttrcal
    real::ttral,ttrtc
    REAL,DIMENSION(3)::difc
    REAL,DIMENSION(nzmax)::z,o2,orgml,calml,orggg,calgg,form,pore
    REAL,DIMENSION(nzmax,3)::carb,resp_c

    !
    z(1) = 0.
    DO j = 2, kmax
      z(j) = z(j-1) + delz(j)
    END DO
    !
    CALL set_pore(calpc,z,pore)
    ! 
    expb = 3.0
    ! diffusion coefficient for o2
    difo2 = 12.1e-6
    ! diffusion coefficient for co2
    difc(1) = 10.5e-6
    ! diffusion coefficient for hco3
    difc(2) = 6.4e-6
    ! diffusion coefficient for co3
    difc(3) = 5.2e-6 
    ! dissolution rate constant, units of 1/s
    dissc = (par_sed_archer1991_disscpct/100.0) * par_sed_archer1991_dissc ! 1. * 1.1574e-5
    ! dissolution rate order
    dissn = par_sed_archer1991_dissn ! 4.5
    ! organic degradation rate constant, 1/s
    rc = par_sed_archer1991_rc ! 2.E-9
    !
    o2(1) = o2bw
    carb(1,1) = co2
    carb(1,2) = hco3
    carb(1,3) = co3
    
    CALL pore_2_form(pore,form,expb)
    
    CALL o2org(rainorg,rc,difo2,db,z,form,pore,o2,zrct,orgml,orggg,resp_c)
    
    DO l = 2, kmax
      calgg(l) = calpc
    END DO

    CALL sldcon(calml,calgg,100.,pore)
    
    CALL co3ss(resp_c,dissc,dissn,calsat,k1,k2,difc,form, &
      & pore,calgg,carb,ttrorg,ttrcal,ttral,ttrtc,diftc,difal)

    fun_archer1991_sedflx = ttrcal
    
  END function fun_archer1991_sedflx


! *************************************************************************************************
! *** o2org ***************************************************************************************
! *************************************************************************************************

  SUBROUTINE o2org(rainorg,rc,difo2,db,z,form,pore,o2,zrct,orgml,orggg,resp_c)

    REAL,INTENT(in)::rainorg
    REAL,INTENT(in)::db
    REAL,INTENT(inout)::rc,difo2,zrct
    REAL,INTENT(inout),DIMENSION(:)::z,form,pore,o2,orgml,orggg
    REAL,INTENT(inout),DIMENSION(:,:)::resp_c
    
    INTEGER::k,m
    REAL::zup,zdn,sed_cmky,rct_c,rms_c,rct_o2,rms_o2
    REAL,DIMENSION(nzmax)::dopls,domin
    REAL,DIMENSION(nzmax)::dbpls,dbmin
    
    DO k = 2, kmax
      o2(k) = o2(1)
      orggg(k) = 0.0002
    END DO

    CALL sldcon(orgml,orggg,12.,pore)
    
    CALL calc_o2_diff(difo2,form,pore,dopls,domin)

    CALL calc_db(db,dbpls,dbmin,z,pore,12.)
    
    sed_cmky = 1.0
    
    zrct = z(kmax)
    
    CALL orgc(rainorg,rc,zrct,dbpls,dbmin,pore,orggg,orgml,z,rct_c,rms_c)

    CALL o2ss(o2,z,orgml,pore,dopls,domin,rc,zrct,rct_o2,rms_o2)
    
! NOTE: the limit in this do loop was originally 20;
!       it can be increased to improve numerical stability under potenitally-anoxic conditions
    IF(o2(kmax).LT.0) THEN
      zup = 0.
      zdn = z(kmax)
      zrct = (zup + zdn) / 2.
      DO m=1,par_sed_archer1991_iterationmax
        CALL orgc(rainorg,rc,zrct,dbpls,dbmin,pore,orggg,orgml,z,rct_c,rms_c)
        CALL o2ss(o2,z,orgml,pore,dopls,domin,rc,zrct,rct_o2,rms_o2)
        IF( ABS(o2(kmax)).LT. 1.e-7) EXIT
        IF(o2(kmax).LT.0) THEN
          zdn = zrct
          zrct = (zrct + zup) / 2.           
        ELSE
          zup = zrct
          zrct = (zrct + zdn) / 2.
        ENDIF
      END DO
    ENDIF

    CALL get_resp(rc,orgml,zrct,z,resp_c)
    
  END SUBROUTINE o2org


! *************************************************************************************************
! *** get_resp ************************************************************************************
! *************************************************************************************************

  SUBROUTINE get_resp(rc,orgml,zrct,z,resp_c)

    REAL,INTENT(inout)::rc,zrct
    REAL,INTENT(inout),dimension(:)::orgml,z
    REAL,INTENT(inout),dimension(:,:)::resp_c

    INTEGER::i
    
    DO i = 1, kmax
      IF(z(i).le.zrct) THEN
        resp_c(i,1) = rc * orgml(i)
      ELSEIF(z(i-1).le.zrct) THEN
        resp_c(i,1) = rc * orgml(i) * (zrct-z(i-1))/delz(i)
      ELSEIF(z(i-1).GT.zrct) THEN
        resp_c(i,1) = 0.
      ENDIF
      resp_c(i,2) = 0.
      resp_c(i,3) = 0.
    END DO

  END SUBROUTINE get_resp
         

! *************************************************************************************************
! *** orgc ****************************************************************************************
! *************************************************************************************************

  SUBROUTINE orgc(rainorg,rc,zrct,dbpls,dbmin,pore,orggg,orgml,z,smrct,rmserr)
    
    REAL,INTENT(in)::rainorg
    REAL,INTENT(inout)::rc,zrct,smrct,rmserr
    REAL,INTENT(inout),DIMENSION(:)::dbpls,dbmin,pore,orggg,orgml,z
    
    REAL:: res(nzmax), dres(nzmax,3)
    REAL:: react(nzmax), dreac(nzmax)
    REAL:: a(nzmax,nzmax), b(nzmax,1)

    INTEGER::i,j
    
    smrct = 0.

! ***  ***

    DO i=2, kmax
      IF(z(i).le.zrct .AND. orggg(i).GT.0) THEN
        react(i) = - rc * orggg(i) * 3.15e7
        dreac(i) = - rc * 3.15e7
      ELSEIF(z(i-1).le.zrct) THEN
        react(i) = - rc * orggg(i) * 3.15e7 * (zrct-z(i-1))/delz(i)
        dreac(i) = - rc * 3.15e7 * (zrct-z(i-1))/delz(i)
      ELSEIF(z(i-1).GT.zrct) THEN
        react(i) = 0.
        dreac(i) = 0.
      ENDIF
    END DO

! ***  ***

    DO i=3, kmax-1
! residual(i)
      res(i) = dbpls(i) * (orggg(i+1) - orggg(i)) - &
        & dbmin(i) * (orggg(i) - orggg(i-1)) + &
        & react(i)
! dr/dx(i+1)
      dres(i,1) = dbpls(i)
! dr/dx(i)
      dres(i,2) = - dbpls(i) - dbmin(i) + dreac(i)
! dr/dx(i-1)
      dres(i,3) = dbmin(i)
    END DO
    
! ***  ***
    
    i=2
! residual(2)
    res(i) = dbpls(i) * (orggg(i+1) - orggg(i)) + &
      & react(i) + rainorg * 12. / delz(i) / ( 1 - pore(i) ) / 2.5
! dr/dx(i+1)
    dres(i,1) = dbpls(i)
! dr/dx(i)
    dres(i,2) = - dbpls(i) + dreac(i)
! dr/dx(i-1) (not defined anyway for i=2)
    dres(i,3) = 0.0
    
! ***  ***
    
    i = kmax
! residual(kmax)
    res(i) = - dbmin(i) * (orggg(i) - orggg(i-1)) + react(i)
! dr/dx(i+1) (not defined anyway)
    dres(i,1) = 0.0
! dr/dx(i)
    dres(i,2) = - dbmin(i) + dreac(i)
! dr/dx(i-1)
    dres(i,3) = dbmin(i)
    
! ***  ***
    
!   zero the arrays
    DO i=1,kmax
      b(i,1) = 0.
      DO j=1,kmax
        a(i,j) = 0.
      END DO
    END DO
    
! set up the residual array
    DO i=1, kmax-1
      b(i,1) = - res(i+1)
! want res(2) into b(1), res(3) into b(2) etc.
    END DO
    
! diagonal, dri/dxi
    DO i=1, kmax-1
      a(i,i) = dres(i+1,2)
! want dres(2,2) in a(1,1), dres(3,2) in a(2,2) etc
    END DO
    
! upper off-diagonal, dri/dxi+1
    DO i=1,kmax-2
      a(i,i+1) = dres(i+1,1)
! want dres(2,1) in a(1,2), dres(3,1) in a(2,3) etc
    END DO
    
! lower off-diagonal, dri/dxi-1
    DO i=1, kmax-2
      a(i+1,i) = dres(i+2,3)
! want dres(3,3) in a(2,1), dres(4,3) in a(3,2) etc
    END DO
    
    CALL gaussj(a,kmax-1,b,1)
    
! update the concentration array
    rmserr = 0
    DO i=1,kmax-1
      orggg(i+1) = orggg(i+1) + b(i,1)
      rmserr = rmserr + res(i+1)**2
    END DO
    
    CALL sldcon(orgml,orggg,12.,pore) 

    DO i=2, kmax
      IF(z(i).le.zrct) THEN
        react(i) = - rc * orggg(i)*3.15e7
      ELSEIF(z(i-1).le.zrct) THEN
        react(i) = - rc * orggg(i)*3.15e7/pore(i) * (zrct-z(i-1))/delz(i)
      ELSEIF(z(i-1).GT.zrct) THEN
        react(i) = 0.
      ENDIF
! units format:
! [ g diss / g sl yr ]
! [ g sl / cm3 sl ]
! [ cm3 sl / cm3 total ]
! [ cm (total) ]
! = [ g diss / cm2 yr ]
      smrct = smrct + react(i) * 2.5 * ( 1 - pore(i) ) * delz(i)  
    END DO
! into units of moles / cm2 yr
    smrct = - smrct / 12.
    
    DO i=3, kmax-1
! residual(i)
      res(i) = dbpls(i) * (orggg(i+1) - orggg(i)) - dbmin(i) * (orggg(i) - orggg(i-1)) + react(i)
    END DO
    
    i=2
! residual(2)
    res(i) = dbpls(i) * (orggg(i+1) - orggg(i)) + react(i) + &
      & rainorg * 12. / delz(i) / ( 1 - pore(i) ) / 2.5
    
    i = kmax
! residual(kmax)
    res(i) = - dbmin(i) * (orggg(i) - orggg(i-1)) + react(i)
    
    rmserr = 0.
    DO i=2, kmax
      rmserr = rmserr + res(i)**2
      IF(orggg(i).GT.1) orggg(i) = 1.
      IF(orggg(i).LT.0) orggg(i) = 0.
    END DO
    rmserr = rmserr**(0.5)
    
  END SUBROUTINE orgc
  
  
! *************************************************************************************************
! *** o2ss ****************************************************************************************
! *************************************************************************************************
  
  SUBROUTINE o2ss(o2,z,orgml,pore,dopls,domin,rate,zrct,smrct,rmserr)
    
    REAL,INTENT(inout)::rate,zrct,smrct,rmserr
    REAL,INTENT(inout),DIMENSION(:)::o2,z,orgml,pore,dopls,domin

    REAL:: dfplus(nzmax), dfzero(nzmax), dfmins(nzmax)
    REAL:: a(nzmax,nzmax),b(nzmax,1),r(nzmax)

    INTEGER::j,k
    
    DO j=2,kmax-1
      r(j) = (   dopls(j)*(o2(j+1)-o2(j)) - domin(j)*(o2(j)-o2(j-1)) ) 
! units of m / cm2 (total) s
      IF(z(j).le.zrct) THEN
        r(j) = r(j) - 1.3*rate*orgml(j)/pore(j)
      ELSEIF(z(j-1).le.zrct) THEN
        r(j) = r(j) - 1.3*rate*orgml(j)/pore(j) * (zrct-z(j-1))/delz(j)
      ENDIF
! units of m / cm2 (total) s
      dfplus(j) = dopls(j)
      dfzero(j) = -dopls(j) -domin(j)
      dfmins(j) = domin(j)
! appropriate to units of m / cm2 s
    END DO

    j=kmax
    r(j) = ( - domin(j)*(o2(j)-o2(j-1)) )
    IF (z(j).LT.zrct) THEN
      r(j) = r(j) - 1.3*rate*orgml(j)/pore(j)
    ELSEIF(z(j-1).LT.zrct) THEN
      r(j) = r(j) - 1.3*rate*orgml(j) * (zrct-z(j-1))/delz(j)
    ENDIF
    dfplus(j) = 0.
    dfzero(j) = -domin(j)
    dfmins(j) = domin(j)
    DO j=1,kmax-1
      DO k=1,kmax-1
        a(j,k) = 0.
      END DO
    END DO
    DO j=1,kmax-2
      a(j,j+1) = dfplus(j+1)
      a(j,j) = dfzero(j+1)
      a(j+1,j) = dfmins(j+2)
    END DO
    a(kmax-1,kmax-1) = dfzero(kmax) + dfplus(kmax)
    DO j=1,kmax-1
      b(j,1) = - r(j+1)
    END DO

    CALL gaussj(a,kmax-1,b,1)
    
    smrct = 0.
    DO j=2,kmax
      o2(j) = o2(j) + b(j-1,1) 
      IF(z(j).le.zrct) THEN
! units format:
! [ moles O2 / l total second ]
! [ moles O2 / cm3 yr ]
! [ moles O2 / cm2 yr ] 
        smrct = smrct - 1.3*rate*orgml(j) * 3.15e7 / 1000. * delz(j)
      ELSEIF(z(j-1).le.zrct) THEN
        smrct = smrct - 1.3*rate*orgml(j) * (zrct-z(j-1))/delz(j) * 3.15e7 / 1000. * delz(j)
      ENDIF
    END DO

! Calculate RMS error
    rmserr = 0.
    DO j = 2, kmax-1
      r(j) = (   dopls(j)*(o2(j+1)-o2(j)) - domin(j)*(o2(j)-o2(j-1)) ) 
      IF(z(j).le.zrct) THEN
        r(j) = r(j) - 1.3*rate*orgml(j)/pore(j)
      ELSEIF(z(j-1).le.zrct) THEN
        r(j) = r(j) - 1.3*rate*orgml(j) * (zrct-z(j-1))/delz(j)
      ENDIF
      rmserr = rmserr + r(j)**2
    END DO
    j = kmax
    r(j) = ( - domin(j)*(o2(j)-o2(j-1)) )
    IF(z(j).LT.zrct) THEN
      r(j) = r(j) - 1.3*rate*orgml(j)/pore(j)
    ELSEIF(z(j-1).LT.zrct) THEN
      r(j) = r(j) - 1.3*rate*orgml(j) * (zrct-z(j-1))/delz(j)
    ENDIF
    rmserr = rmserr + r(j)**2
    
    rmserr = rmserr**(0.5)

  END SUBROUTINE o2ss


! *************************************************************************************************
! *** calc_o2_diff ********************************************************************************
! *************************************************************************************************
  
  SUBROUTINE calc_o2_diff(difo2,form,pore,dopls,domin)

    REAL,INTENT(inout)::difo2
    REAL,INTENT(inout),DIMENSION(:)::form,pore,dopls,domin
    
    INTEGER::i

    DO i=3,kmax-1
      dopls(i)=difo2 * ((form(i+1)+form(i))/2) * 1 / pore(i) * 2 / ( (delz(i+1)+delz(i)) * delz(i) )
      domin(i)=difo2 * ((form(i-1)+form(i))/2) * 1/pore(i) * 2 / ( (delz(i-1)+delz(i)) * delz(i) )
    END DO

    i=kmax
    dopls(i)=0.
    domin(i)=difo2 * ((form(i-1)+form(i))/2) * 1/pore(i) * 2 / ( (delz(i-1)+delz(i)) * delz(i) )

    i=2
    dopls(i)=difo2 * ((form(i+1)+form(i))/2) * 1/pore(i) * 2 / ( (delz(i+1)+delz(i)) * delz(i) )
    domin(i)=difo2 * (form(i)+1)/2 * 1/pore(i) * 1/delz(i)**2

  END SUBROUTINE calc_o2_diff


! *************************************************************************************************
! *** co3ss ***************************************************************************************
! *************************************************************************************************

  SUBROUTINE co3ss(resp_c,dissc,dissn,csat,u1,u2,difc,form, &
    & pore,calgg,carb,ttrorg,ttrcal,ttral,ttrtc,diftc,difal)

    REAL,INTENT(inout)::dissc,dissn
    REAL,INTENT(in)::csat
    REAL,INTENT(in)::u1,u2
    REAL,INTENT(inout)::ttrorg,ttrcal,ttral,ttrtc,diftc,difal
    REAL,INTENT(inout),DIMENSION(:)::difc
    REAL,INTENT(inout),DIMENSION(:)::form,pore
    REAL,INTENT(in),DIMENSION(:)::calgg
    REAL,INTENT(inout),DIMENSION(:,:)::resp_c,carb
    
    INTEGER::k,l
    REAL,DIMENSION(nzmax)::cal_c
    REAL,DIMENSION(nzmax,3)::dcpls
    REAL,DIMENSION(nzmax,3)::dcmin
    
    CALL calcdc(difc,form,pore,dcpls,dcmin)

    DO k=2, kmax
      carb(k,1) = carb(1,1)
      carb(k,2) = carb(1,2)
      carb(k,3) = csat - 15e-6
    END DO
    carb(2,3) = ( 2 * carb(1,3) + csat ) / 3
    carb(3,3) = ( carb(1,3) + 2 * csat ) / 3
    
    DO l=1,20
      CALL calc_co3(resp_c,dissc,dissn,csat,u1,u2,dcpls,dcmin, &
        & pore,calgg,carb,cal_c)
      CALL diag(ttral,difal,ttrtc,diftc,ttrcal,ttrorg,resp_c,cal_c,carb, &
        & pore,dcmin(2,1),dcmin(2,2),dcmin(2,3))
      IF((diftc.NE.0).AND.(difal.NE.0)) THEN
        IF(( ABS(1 - ABS(ttrtc / diftc)) .LT. 0.03 ) .AND. ( ABS(1 - ABS(ttral / difal)) .LT. 0.03 )) EXIT
      ENDIF
    END DO
    
  END SUBROUTINE co3ss
  
  
! *************************************************************************************************
! *** calc_co3 ************************************************************************************
! *************************************************************************************************
  
! routine calc_co3, which calculates a single iteration
! of the carbonate system chemistry.  must be run 
! several times because of the non-linearity of 
! calcite dissolution kinetics.
  
  SUBROUTINE calc_co3(resp_c,dissc,dissn,csat,disoc1,disoc2,dplus,dminus, &
       & pore,calgg,carb,cal_c)

    REAL,INTENT(inout)::dissc,dissn
    REAL,INTENT(in)::csat
    REAL,INTENT(in)::disoc1,disoc2
    REAL,INTENT(inout),DIMENSION(:)::pore
    REAL,INTENT(in),DIMENSION(:)::calgg
    REAL,INTENT(inout),DIMENSION(:)::cal_c
    REAL,INTENT(inout),DIMENSION(:,:)::resp_c,dplus,dminus,carb

    ! note:'nzmax_co3' must be at least 3 times higher than for other routines

    INTEGER::i,k,l,m,p,o
    REAL::weight,trialw,rmstc,rmsal,rmsph
    REAL,DIMENSION(nzmax_co3,3)::r
    REAL,DIMENSION(3,3,3,nzmax_co3)::dr
    REAL,DIMENSION(nzmax_co3,nzmax_co3)::a
    REAL,DIMENSION(nzmax_co3,1)::b

    DO i=1,3
       carb(kmax+1, i) = carb(kmax, i)
       ! for the bottom boundary condition, no flux
    END DO

    ! the residual terms: array (depth; tc, alk, ph)

    DO k = 2, kmax
       ! total co2 equation
       r(k,1) = 0.
       DO i=1,3
          r(k,1) = r(k,1) + (dplus(k,i) * (carb(k+1,i)-carb(k,i)) - dminus(k,i) * (carb(k,i)-carb(k-1,i)))
          ! units of moles / l *porewater* sec
       END DO
       r(k,1) = r(k,1) + resp_c(k,1) / pore(k) + resp_c(k,2) / pore(k) + resp_c(k,3) / pore(k)
       ! units of moles / l *porewater* sec
       IF(carb(k,3).LT.csat) THEN
          r(k,1) = r(k,1) + dissc*((1-(carb(k,3)/csat))**dissn)*(1-pore(k)) / pore(k)*calgg(k)*(2.5*1000)/(100)
       ENDIF

       ! alkalinity equation
       r(k,2) = dplus(k,3) * (carb(k+1,3)-carb(k,3)) & 
            & - dminus(k,3) * (carb(k,3)-carb(k-1,3)) &
            & + 0.5 * dplus(k,2) * (carb(k+1,2)-carb(k,2)) &
            & - 0.5 * dminus(k,2) * (carb(k,2)-carb(k-1,2))
       r(k,2) = r(k,2) + resp_c(k,3) / pore(k) + 0.5 * resp_c(k,2) / pore(k)
       IF(carb(k,3).LT.csat) THEN
          r(k,2) = r(k,2) + dissc*((1-(carb(k,3)/csat))**dissn)*(1-pore(k)) / pore(k)*calgg(k)*(2.5*1000)/(100)
       ENDIF
       r(k,3) = carb(k,1) * carb(k,3) / carb(k,2)**2 - disoc2 / disoc1
    END DO

    ! the derivitive terms: array (function, variable, 
    ! 'k+'= 3 to 'k-' = 1, and depth level k)
    DO k=2,kmax-1
       dr(1,1,3,k) = dplus(k,1)
       dr(1,1,2,k) = -dplus(k,1) -dminus(k,1)
       dr(1,1,1,k) = dminus(k,1)

       dr(1,2,3,k) = dplus(k,2)
       dr(1,2,2,k) = -dplus(k,2) -dminus(k,2)
       dr(1,2,1,k) = dminus(k,2)

       dr(1,3,3,k) = dplus(k,3)
       dr(1,3,2,k) = -dplus(k,3) -dminus(k,3)
       IF(carb(k,3).LT.csat) THEN
          dr(1,3,2,k) = dr(1,3,2,k) &
               &                    - dissc * dissn &
               &                      * (1-pore(k)) / pore(k)&
               &                      * calgg(k)&
               &                      * (2.5*1000)/100&
               &                      / csat&
               &                 *((1-(carb(k,3)/csat))**(dissn-1.)) 
       ENDIF
       dr(1,3,1,k) = dminus(k,3)

       dr(2,1,3,k) = 0.
       dr(2,1,2,k) = 0.
       dr(2,1,1,k) = 0.

       dr(2,2,3,k) = (0.5) * dplus(k,2)
       dr(2,2,2,k) = -(0.5) * dplus(k,2) -(0.5) * dminus(k,2)
       dr(2,2,1,k) = (0.5) * dminus(k,2)

       dr(2,3,3,k) = dplus(k,3)
       dr(2,3,2,k) = -dplus(k,3) -dminus(k,3)
       IF(carb(k,3).LT.csat) THEN
          dr(2,3,2,k) = dr(2,3,2,k) &
               &                    - dissc * dissn &
               &                      * (1-pore(k)) / pore(k) &
               &                      * calgg(k) &
               &                      * (2.5*1000)/100 &
               &                      / csat &
               &                 *((1-(carb(k,3)/csat))**(dissn-1.)) 
       ENDIF
       dr(2,3,1,k) = dminus(k,3)

       dr(3,1,3,k) = 0.
       dr(3,1,2,k) = carb(k,3) / carb(k,2)**2
       dr(3,1,1,k) = 0.

       dr(3,2,3,k) = 0.
       dr(3,2,2,k) = -(0.5) * carb(k,1) * carb(k,3) / carb(k,2)**3
       dr(3,2,1,k) = 0.

       dr(3,3,3,k) = 0.
       dr(3,3,2,k) = carb(k,1) / carb(k,2)**2
       dr(3,3,1,k) = 0.

    END DO

    ! bottom special conditions
    DO l=1,3
       ! function
       DO i=1,3
          ! variable
          DO m=1,3
             ! above, below
             dr(l,i,m,kmax) = dr(l,i,m,kmax-1)
          END DO
       END DO
    END DO

    ! load the big array
    DO k=1,3*kmax-1
       DO l=1,3*kmax-1
          a(k,l)=0.
       END DO
    END DO
    DO k=2,kmax
       ! depth level
       DO l=1,3
          ! function
          DO m=1,3
             ! up, down
             DO i=1,3
                ! variable
                ! row number
                o = (k-2)*3 + l
                ! column number
                p = (m+k-4)*3 + i
                ! originally, "extras" were put in column 0, out of the way
                ! however, there is no column '0', so they are simply ignored in this version
                IF(p.GT.0) THEN
                   a(o,p) = dr(l,i,m,k)
                   IF(k.EQ.kmax) THEN
                      IF(m.EQ.2) a(o,p) = a(o,p)+dr(l,i,m+1,k)
                   ENDIF
                ENDIF
             END DO
          END DO
       END DO
    END DO

    ! load the residual array
    DO k=2,kmax
       DO l=1,3
          b( (k-2)*3+l, 1) = - r(k,l)
       END DO
    END DO

    CALL gaussj(a,(kmax-1)*3,b,1)

    weight = 1.0
    DO k=2,kmax
       trialw = - 0.75 * carb(k,3) / b( (k-2)*3+3, 1 )
       IF((trialw.GT.0.).AND.(trialw .LT.weight)) weight = trialw
    END DO
    DO k=2,kmax
       DO i=1,3
          carb(k,i) = carb(k,i) + weight * b( (k-2)*3+i, 1)
          ! ENSURE NO NEGATIVE [CO32-] SHIT GOES DOWN
          if (carb(k,i) < -const_real_nullsmall) then
             carb(k,i) = 1.0e-6
             error_Archer = .TRUE.
          end if
       END DO
    END DO

    ! the error terms, after the iteration
    DO k=2,kmax-1
       r(k,1) = 0.
       DO i=1,3
          r(k,1) = r(k,1) + dplus(k,i) * (carb(k+1,i)-carb(k,i)) - dminus(k,i) * (carb(k,i)-carb(k-1,i))
       END DO
       r(k,1) = r(k,1) + resp_c(k,1) / pore(k) + resp_c(k,2) / pore(k) + resp_c(k,3) / pore(k)
       IF(carb(k,3).LT.csat) THEN
          r(k,1) = r(k,1) + dissc*((1-(carb(k,3)/csat))**dissn)*(1-pore(k))*calgg(k)*(2.5*1000)/(100)
       ENDIF
       r(k,2) = dplus(k,3) * (carb(k+1,3)-carb(k,3)) &
            &           - dminus(k,3) * (carb(k,3)-carb(k-1,3)) &
            &     + (0.5) * dplus(k,2) * (carb(k+1,2)-carb(k,2)) &
            &     - (0.5) * dminus(k,2) * (carb(k,2)-carb(k-1,2))
       r(k,2) = r(k,2) + 0.5 * resp_c(k,2) / pore(k) + resp_c(k,3) / pore(k)
       IF(carb(k,3).LT.csat) THEN
          r(k,2) = r(k,2) + dissc*((1-(carb(k,3)/csat))**dissn)*(1-pore(k))*calgg(k)*(2.5*1000)/(100)
       ENDIF
       r(k,3) = carb(k,1) * carb(k,3) / carb(k,2)**2 - disoc2 / disoc1
    END DO

    rmstc = 0.
    rmsal = 0.
    rmsph = 0.
    DO k=2,kmax-1
       rmstc = rmstc + r(k,1)**2
       rmsal = rmsal + r(k,2)**2
       rmsph = rmsph + r(k,3)**2
    END DO
    rmstc = rmstc**(0.5)
    rmsal = rmsal**(0.5)
    rmsph = rmsph**(0.5)

    DO k = 1, kmax
       IF(carb(k,3).LT.csat) THEN
          cal_c(k) = dissc*((1-(carb(k,3)/csat))**dissn)*(1-pore(k))*calgg(k)*(2.5*1000)/(100)
       ELSE
          cal_c(k) = 0. 
       ENDIF
    END DO

  END SUBROUTINE calc_co3


! *************************************************************************************************
! *** calcdc **************************************************************************************
! *************************************************************************************************

  SUBROUTINE calcdc(difc,form,pore,dcpls,dcmin)
    
    REAL,INTENT(inout),DIMENSION(:)::difc
    REAL,INTENT(inout),DIMENSION(:)::form,pore
    REAL,INTENT(inout),DIMENSION(:,:)::dcpls,dcmin

    INTEGER::i,j
    
    DO i=3,kmax-1
      DO j=1,3
        dcpls(i,j)=difc(j) &
          & *(delz(i)*form(i+1)+delz(i+1)*form(i)) &
          & / (delz(i)+delz(i+1)) &
          & * 1 / pore(i) &
          & *(2/((delz(i+1)+delz(i))*delz(i)))
        dcmin(i,j)=difc(j) &
          & *(delz(i)*form(i-1)+delz(i-1)*form(i)) &
          & / (delz(i)+delz(i-1)) &
          & * 1 / pore(i) &
          & *(2/((delz(i-1)+delz(i))*delz(i)))
      END DO
    END DO
    DO j=1,3
      i=kmax
      dcpls(i,j)=0
      dcmin(i,j)=difc(j) &
        & *(delz(i)*form(i-1)+delz(i-1)*form(i)) &
        & / (delz(i)+delz(i-1)) &
        & * 1 / pore(i) &
        & *(2/((delz(i-1)+delz(i))*delz(i)))
      i=2
      dcpls(i,j)=difc(j) &
        & *(delz(i)*form(i+1)+delz(i+1)*form(i)) &
        & / (delz(i)+delz(i+1)) &
        & * 1 / pore(i) &
        & *(2/((delz(i+1)+delz(i))*delz(i)))
      dcmin(i,j)=difc(j) &
        & *(form(i)+1)/2 &
        & * 1 / pore(i) &
        & *(1/(delz(i)**2))
    END DO

  END SUBROUTINE calcdc


! *************************************************************************************************
! *** diag ****************************************************************************************
! *************************************************************************************************

! file 'diag.for', which calculates the diffusive fluxes of
! o2, total co2, and alkalinity at the sediment-water
! interface, and also the integrated reaction rates of
! those quantities.  used by co3main to determine when to
! stop repeating the co3 subroutine.

  SUBROUTINE diag(ttral,difal,ttrtc,diftc,ttrcal,ttrorg,resp_c,cal_c,carb, &
    & pore,dcco2,dchco3,dcco3)

    REAL,INTENT(inout)::ttral,difal,ttrtc,diftc,ttrcal,ttrorg,dcco2,dchco3,dcco3
    REAL,INTENT(inout),DIMENSION(:)::cal_c,pore
    REAL,INTENT(inout),DIMENSION(:,:)::carb,resp_c

    INTEGER::j,k
    REAL,DIMENSION(3)::ttreac,difflx

! zero the diagnostics variables, ttreac and flux
    DO j=1,3
      ttreac(j) = 0.
      difflx(j) = 0.
    END DO
    ttrcal = 0.
    ttrorg = 0.

! reaction rates are in units of mol species/cm2 (total) y
    DO k = 1, kmax
      ttreac(1) = ttreac(1) + resp_c(k,1) * delz(k) * 3.15E7 / 1e3
      ttreac(2) = ttreac(2) + resp_c(k,2) * delz(k) * 3.15E7 / 1e3
      ttreac(3) = ttreac(3) + resp_c(k,3) * delz(k) * 3.15E7 / 1e3 + cal_c(k) * delz(k) * 3.15E7 / 1e3
      ttrcal = ttrcal + cal_c(k) * delz(k) * 3.15E7 / 1e3
      ttrorg = ttrorg + ( resp_c(k,1) + resp_c(k,2) + resp_c(k,3)) * delz(k) * 3.15e7 / 1e3
    END DO

! the diffusive fluxes
    difflx(1) =   dcco2 * ( carb(1,1) - carb(2,1) ) * pore(2) * delz(2) * 3.15e7 / 1e3
    difflx(2) =   dchco3 * ( carb(1,2) - carb(2,2) ) * pore(2) * delz(2) * 3.15e7 / 1e3
    difflx(3) =   dcco3 * ( carb(1,3) - carb(2,3) ) * pore(2) * delz(2) * 3.15e7 / 1e3

    ttrtc = ttreac(1) + ttreac(3)
    ttral = ttreac(3) * 2
    diftc = difflx(1) + difflx(2) + difflx(3)
    difal = difflx(2) + difflx(3) * 2.

  END SUBROUTINE diag


! *************************************************************************************************
! *** gaussj **************************************************************************************
! *************************************************************************************************
  
  SUBROUTINE gaussj(a,n,b,m)

    INTEGER,INTENT(in)::n,m
    REAL,INTENT(inout),DIMENSION(:,:)::a
    REAL,INTENT(inout),DIMENSION(:,:)::b

    INTEGER::i,j,k,l,ll,irow,icol
    INTEGER,DIMENSION(nmax)::indxr,indxc
    REAL::big,dum,pivinv
    REAL,DIMENSION(nmax)::ipiv
       
! numerical recipes, pages 28-29
    irow = 0
    icol = 0
    
    DO j=1,n
      ipiv(j)=0
    END DO
    DO i=1,n
      big=0.
      DO j=1,n
        IF(ipiv(j).NE.1) THEN
          DO k=1,n
            IF(ipiv(k).EQ.0) THEN
              IF(ABS(a(j,k)).ge.big) THEN
                big=ABS(a(j,k))
                irow=j
                icol=k
              ENDIF
            ELSE IF(ipiv(k).GT.1) THEN
              ipiv(k) = 1.0
              error_Archer = .TRUE.
            ENDIF
          END DO
        ENDIF
      END DO
      ipiv(icol)=ipiv(icol)+1
      IF(irow.NE.icol) THEN
        DO l=1,n
          dum=a(irow,l)
          a(irow,l)=a(icol,l)
          a(icol,l)=dum
        END DO
        DO l=1,m
          dum=b(irow,l)
          b(irow,l)=b(icol,l)
          b(icol,l)=dum
        END DO
      ENDIF
      indxr(i)=irow
      indxc(i)=icol
      IF(a(icol,icol).EQ.0.) then
         a(icol,icol) = 1.0
         error_Archer = .TRUE.
      end IF
      pivinv=1./a(icol,icol)
      a(icol,icol)=1.
      DO l=1,n
        a(icol,l)=a(icol,l)*pivinv
      END DO
      DO l=1,m
        b(icol,l)=b(icol,l)*pivinv
      END DO
      DO ll=1,n
        IF(ll.NE.icol) THEN
          dum=a(ll,icol)
          a(ll,icol)=0.
          DO l=1,n
            a(ll,l)=a(ll,l)-a(icol,l)*dum
          END DO
          DO l=1,m
            b(ll,l)=b(ll,l)-b(icol,l)*dum
          END DO
        ENDIF
      END DO
    END DO
    DO l=n,1,-1
      IF(indxr(l).NE.indxc(l)) THEN
        DO k=1,n
          dum=a(k,indxr(l))
          a(k,indxr(l))=a(k,indxc(l))
          a(k,indxc(l))=dum
        END DO
      ENDIF
    END DO
    
  END SUBROUTINE gaussj


! *************************************************************************************************
! *** CALC_DB *************************************************************************************
! *************************************************************************************************

  SUBROUTINE CALC_DB(db,dbpls,dbmin,z,pore,MLD)
    
    REAL,INTENT(in)::mld
    REAL,INTENT(in)::db
    REAL,INTENT(inout),DIMENSION(:)::dbpls,dbmin,z,pore

    INTEGER::I

    Z(KMAX+1) = MLD +1
    
    DO I=2, kmax
      IF(I.EQ.2) THEN
        DBPLS(I) = DB * 2 / ( (DELZ(I) + DELZ(I+1)) * DELZ(I) ) * ( 1-PORE(I)+1-PORE(I+1) )/( 1-PORE(I) )
        DBMIN(I) = 0.
      ELSE IF(Z(I).LE.MLD) THEN
        DBPLS(I) = DB * 2 / ( (DELZ(I) + DELZ(I+1)) * DELZ(I) ) * ( 1-PORE(I)+1-PORE(I+1) )/( 1-PORE(I) )
        DBMIN(I) = DB * 2 / ( (DELZ(I) + DELZ(I-1)) * DELZ(I) ) * ( 1-PORE(I)+1-PORE(I-1) )/( 1-PORE(I) )
      ELSE IF(Z(I-1).LE.MLD) THEN
        DBPLS(I) = 0.
        DBMIN(I) = DB * 2 / ( (DELZ(I) + DELZ(I-1)) * DELZ(I) ) * ( 1-PORE(I)+1-PORE(I-1) )/( 1-PORE(I) )
      ELSE
        DBPLS(I) = 0.
        DBMIN(I) = 0.
      END IF
    END DO
    DBPLS(kmax) = 0.

  END SUBROUTINE CALC_DB


! *************************************************************************************************
! *** pore_2_form *********************************************************************************
! *************************************************************************************************
  
  SUBROUTINE pore_2_form(pore,form,expb)

    REAL,INTENT(inout)::expb
    REAL,INTENT(inout),dimension(:)::pore,form

    INTEGER::k

    DO k = 1, kmax
      form(k) = pore(k)**expb
    END DO

  END SUBROUTINE pore_2_form


! *************************************************************************************************
! *** sldcon **************************************************************************************
! *************************************************************************************************

  SUBROUTINE sldcon(temp_ml,temp_gg,molwt,pore) 

    REAL,INTENT(in)::molwt
    REAL,INTENT(inout),dimension(:)::temp_ml,temp_gg,pore

    INTEGER::k

! update the solid concentration accounts
    DO k=2,kmax
      temp_ml(k) = temp_gg(k)*2.5*(1-pore(k))*1000/molwt
    END DO

  END SUBROUTINE sldcon


! *************************************************************************************************
! *** sldfrc **************************************************************************************
! *************************************************************************************************
  
  SUBROUTINE sldfrc(temp_ml,temp_gg,molwt,pore)

    REAL,INTENT(inout)::molwt
    REAL,INTENT(inout),dimension(:)::temp_ml,temp_gg,pore

    INTEGER::i

! Update the solid wt. pct. accounts
    DO i=2,kmax
      temp_gg(i) = temp_ml(i)*molwt/(2.5*(1-pore(i))*1000)
    END DO

  END SUBROUTINE sldfrc

  
  ! ********************************************************************************************************************************
  ! SET UP POROSITY PROFILE
  ! NOTE: sub-surface porosity is calculated from the MASS fraction of CaCO3,
  !       whereas the sediment core model works on a VOLUME fraction basis (the difference is relatively small)
  SUBROUTINE set_pore(calgg,z,pore)
    IMPLICIT NONE
    ! dummy arguments
    REAL,INTENT(in)::calgg
    REAL,INTENT(inout),DIMENSION(:)::z,pore
    ! local variables
    INTEGER::k
    REAL::pore_max,exp_pore

    ! *** calculate local constants
    ! Zeebe and Zachos [2007] sub-surface porosity constant
    pore_max = fun_calc_sed_poros(calgg)
    ! Archer [1996] porosity scale depth
    exp_pore = 0.25*calgg + 3. *(1-calgg)

    ! *** calculate porosity profile
    pore(1) = 0.
    DO k = 2, kmax
       ! Archer porosity profile
       pore(k) = EXP(-z(k)/exp_pore) * (1.-pore_max) + pore_max
    ENDDO

  END SUBROUTINE set_pore
  ! ********************************************************************************************************************************
  

END MODULE sedgem_box_archer1991_sedflx
