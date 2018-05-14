! ************************************************************************
! * module legmod - direct and indirect Legendre transformation routines *
! * E. Kirk 20-Feb-2009 tested for T21 - T341 resolutions 32 & 64 bit    *
! ************************************************************************

module legmod
! ************************
! * Legendre Polynomials *
! ************************
use pumamod, only:NTRU,NTP1,NCSP,NESP,NLON,NLPP,NLAT,NHOR,NLEV,gwd,sid,plavor
      implicit none

real :: qi(NCSP,NLPP) ! P(m,n) = Associated Legendre Polynomials
real :: qj(NCSP,NLPP) ! Q(m,n) = Used for d/d(mu)
real :: qc(NCSP,NLPP) ! P(m,n) * gwd              used in fc2sp
real :: qe(NCSP,NLPP) ! Q(mn,) * gwd / cos2       used in mktend
real :: qm(NCSP,NLPP) ! P(m,n) * gwd / cos2 * m   used in mktend
real :: qq(NCSP,NLPP) ! P(m,n) * gwd / cos2 * n * (n+1) / 2  "
real :: qu(NCSP,NLPP) ! P(m,n) / (n*(n+1)) * m    used in dv2uv
real :: qv(NCSP,NLPP) ! Q(m,n) / (n*(n+1))        used in dv2uv
end module legmod

! =================
! SUBROUTINE LEGINI
! =================

subroutine legini
use legmod
implicit none

integer :: jlat ! Latitude
integer :: lm
integer :: m
integer :: n

real (kind=8) :: amsq
real (kind=8) :: z1
real (kind=8) :: z2
real (kind=8) :: z3
real (kind=8) :: f1m
real (kind=8) :: f2m
real (kind=8) :: znn1
real (kind=8) :: zsin    ! sin
real (kind=8) :: zcsq    ! cos2
real (kind=8) :: zcos    ! cos
real (kind=8) :: zgwd    ! gw
real (kind=8) :: zgwdcsq ! gw / cos2

real (kind=8) :: zpli(NCSP)
real (kind=8) :: zpld(NCSP)

do jlat = 1 , NLPP

! set p(0,0) and p(0,1)

   zgwd    = gwd(jlat)            ! gaussian weight - from inigau
   zsin    = sid(jlat)            ! sin(phi) - from inigau
   zcsq    = 1.0_8 - zsin * zsin  ! cos(phi) squared
   zgwdcsq = zgwd / zcsq          ! weight / cos squared
   zcos    = sqrt(zcsq)           ! cos(phi)
   f1m     = sqrt(1.5_8)
   zpli(1) = sqrt(0.5_8)
   zpli(2) = f1m * zsin
   zpld(1) = 0.0
   lm      = 2

! loop over wavenumbers

   do m = 0 , NTRU
      if (m > 0) then
         lm  = lm + 1
         f2m = -f1m * sqrt(zcsq / (m+m))
         f1m =  f2m * sqrt(m+m + 3.0_8)
         zpli(lm) = f2m
         if (lm < NCSP) then
            lm = lm + 1
            zpli(lm  ) =       f1m * zsin
            zpld(lm-1) =  -m * f2m * zsin
         endif ! (lm < NCSP)
      endif ! (m > 0)

      amsq = m * m

      do n = m+2 , NTRU
         lm = lm + 1
         z1 = sqrt(((n-1)*(n-1) - amsq) / (4*(n-1)*(n-1)-1))
         z2 = zsin * zpli(lm-1) - z1 * zpli(lm-2)
         zpli(lm  ) = z2 * sqrt((4*n*n-1) / (n*n-amsq))
         zpld(lm-1) = (1-n) * z2 + n * z1 * zpli(lm-2)
      enddo ! n

      if (lm < NCSP) then ! mode (m,NTRU)
         z3 = sqrt((NTRU*NTRU-amsq) / (4*NTRU*NTRU-1))
         zpld(lm)=-NTRU*zsin*zpli(lm) + (NTRU+NTRU+1)*zpli(lm-1)*z3
      else                ! mode (NTRU,NTRU)
         zpld(lm)=-NTRU*zsin*zpli(lm)
      endif
   enddo ! m

   lm = 0
   do m = 0 , NTRU
      do n = m , NTRU
           lm = lm + 1
           znn1 = 0.0
           if (n > 0) znn1 = 1.0_8 / (n*(n+1))
           qi(lm,jlat) = zpli(lm)
           qj(lm,jlat) = zpld(lm)
           qc(lm,jlat) = zpli(lm) * zgwd
           qu(lm,jlat) = zpli(lm) * znn1 * m
           qv(lm,jlat) = zpld(lm) * znn1
           qe(lm,jlat) = zpld(lm) * zgwdcsq
           qq(lm,jlat) = zpli(lm) * zgwdcsq * n * (n+1) * 0.5_8
           qm(lm,jlat) = zpli(lm) * zgwdcsq * m
      enddo ! n
   enddo ! m
enddo ! jlat
return
end


! ================
! SUBROUTINE FC2SP
! ================

subroutine fc2sp(fc,sp)
use legmod
implicit none
real, intent(in ) :: fc(2,NLON/2,NLPP)
real, intent(out) :: sp(2,NESP/2)

integer :: l ! Index for latitude
integer :: m ! Index for zonal wavenumber
integer :: n ! Index for total wavenumber
integer :: w ! Index for spherical harmonic

sp(:,:) = 0.0

if (NLPP < NLAT) then  ! Universal (parallel executable) version
!----------------------------------------------------------------------
  do l = 1 , NLPP
    w = 1
    do m = 1 , NTP1
      do n = m , NTP1
        sp(1,w) = sp(1,w) + qc(w,l) * fc(1,m,l)
        sp(2,w) = sp(2,w) + qc(w,l) * fc(2,m,l)
        w = w + 1
      enddo ! n
    enddo ! m
  enddo ! l
else                   ! Single CPU version (symmetry conserving)
!----------------------------------------------------------------------
  do l = 1 , NLAT/2
    w = 1
    do m = 1 , NTP1
      do n = m , NTP1
        if (mod(m+n,2) == 0) then ! Symmetric modes
          sp(1,w) = sp(1,w) + qc(w,l) * (fc(1,m,l) + fc(1,m,NLAT+1-l))
          sp(2,w) = sp(2,w) + qc(w,l) * (fc(2,m,l) + fc(2,m,NLAT+1-l))
        else                      ! Antisymmetric modes
          sp(1,w) = sp(1,w) + qc(w,l) * (fc(1,m,l) - fc(1,m,NLAT+1-l))
          sp(2,w) = sp(2,w) + qc(w,l) * (fc(2,m,l) - fc(2,m,NLAT+1-l))
        endif
        w = w + 1
      enddo ! n
    enddo ! m
  enddo ! l
!----------------------------------------------------------------------
endif ! parallel ?
return
end


! ================
! SUBROUTINE SP2FC
! ================

subroutine sp2fc(sp,fc) ! Spectral to Fourier
use legmod
implicit none

real :: sp(2,NCSP)        ! Coefficients of spherical harmonics
real :: fc(2,NLON/2,NLPP) ! Fourier coefficients

integer :: l ! Loop index for latitude
integer :: m ! Loop index for zonal wavenumber m
integer :: n ! Loop index for total wavenumber n
integer :: w ! Loop index for spectral mode

fc(:,:,:) = 0.0

do l = 1 , NLPP
   w = 1  
   do m = 1 , NTP1
      do n = m , NTP1
         fc(1,m,l) = fc(1,m,l) + qi(w,l) * sp(1,w)
         fc(2,m,l) = fc(2,m,l) + qi(w,l) * sp(2,w)
         w = w + 1
      enddo ! n
   enddo ! m
enddo ! l
return
end


! ===================
! SUBROUTINE SP2FCDMU
! ===================

subroutine sp2fcdmu(sp,fc) ! Spectral to Fourier d/dmu
use legmod
implicit none

real :: sp(2,NCSP)        ! Coefficients of spherical harmonics
real :: fc(2,NLON/2,NLPP) ! Fourier coefficients

integer :: l ! Loop index for latitude
integer :: m ! Loop index for zonal wavenumber m
integer :: n ! Loop index for total wavenumber n
integer :: w ! Loop index for spectral mode

fc(:,:,:) = 0.0

do l = 1 , NLPP
   w = 1  
   do m = 1 , NTP1
      do n = m , NTP1
         fc(1,m,l) = fc(1,m,l) + qj(w,l) * sp(1,w)
         fc(2,m,l) = fc(2,m,l) + qj(w,l) * sp(2,w)
         w = w + 1
      enddo ! n
   enddo ! m
enddo ! l
return
end


! ================
! SUBROUTINE SP3FC
! ================

subroutine sp3fc
use pumamod, only:NLEV,sd,st,sz,gd,gt,gz
implicit none
integer :: v ! Loop index for level

do v = 1 , NLEV
   call sp2fc(sd(1,v),gd(1,v))
   call sp2fc(st(1,v),gt(1,v))
   call sp2fc(sz(1,v),gz(1,v))
enddo
return
end


! ================
! SUBROUTINE DV2UV
! ================

subroutine dv2uv(pd,pz,pu,pv)
use legmod
implicit none

real :: pd(2,NESP/2,NLEV)
real :: pz(2,NESP/2,NLEV)
real :: pu(2,NLON/2,NLPP,NLEV)
real :: pv(2,NLON/2,NLPP,NLEV)
real :: zsave

integer :: l ! Loop index for latitude
integer :: m ! Loop index for zonal wavenumber m
integer :: n ! Loop index for total wavenumber n
integer :: v ! Loop index for level
integer :: w ! Loop index for spectral mode

pu(:,:,:,:) = 0.0
pv(:,:,:,:) = 0.0

do v = 1 , NLEV
  zsave = pz(1,2,v)
  pz(1,2,v) = zsave - plavor
  do l = 1 , NLPP
    w = 1
    do m = 1 , NTP1
      do n = m , NTP1
        pu(1,m,l,v)=pu(1,m,l,v)+qv(w,l)*pz(1,w,v)+qu(w,l)*pd(2,w,v)
        pu(2,m,l,v)=pu(2,m,l,v)+qv(w,l)*pz(2,w,v)-qu(w,l)*pd(1,w,v)
        pv(1,m,l,v)=pv(1,m,l,v)+qu(w,l)*pz(2,w,v)-qv(w,l)*pd(1,w,v)
        pv(2,m,l,v)=pv(2,m,l,v)-qu(w,l)*pz(1,w,v)-qv(w,l)*pd(2,w,v)
        w = w + 1
      enddo ! n
    enddo ! m
  enddo ! l
  pz(1,2,v) = zsave
enddo ! jv
return
end


! ================
! SUBROUTINE UV2DV
! ================

subroutine uv2dv(pu,pv,pd,pz)
use legmod
implicit none

real :: pd(2,NESP/2,NLEV)
real :: pz(2,NESP/2,NLEV)
real :: pu(2,NLON/2,NLPP,NLEV)
real :: pv(2,NLON/2,NLPP,NLEV)

integer :: k ! Loop index for southern latitude
integer :: l ! Loop index for latitude
integer :: m ! Loop index for zonal wavenumber m
integer :: n ! Loop index for total wavenumber n
integer :: v ! Loop index for level
integer :: w ! Loop index for spectral mode

pd(:,:,:) = 0.0
pz(:,:,:) = 0.0

if (NLPP < NLAT) then  ! Universal (parallel executable) version
!----------------------------------------------------------------------
do v = 1 , NLEV
  do l = 1 , NLPP
    w = 1
    do m = 1 , NTP1
      do n = m , NTP1
        pz(1,w,v) = pz(1,w,v)+qe(w,l)*pu(1,m,l,v)-qm(w,l)*pv(2,m,l,v)
        pz(2,w,v) = pz(2,w,v)+qe(w,l)*pu(2,m,l,v)+qm(w,l)*pv(1,m,l,v)
        pd(1,w,v) = pd(1,w,v)-qe(w,l)*pv(1,m,l,v)-qm(w,l)*pu(2,m,l,v)
        pd(2,w,v) = pd(2,w,v)-qe(w,l)*pv(2,m,l,v)+qm(w,l)*pu(1,m,l,v)
        w = w + 1
      enddo ! n
    enddo ! m
  enddo ! l
enddo ! v
else                   ! Single CPU version (symmetry conserving)
!----------------------------------------------------------------------
do v = 1 , NLEV
  do l = 1 , NLAT/2
    k = NLAT+1-l
    w = 1
    do m = 1 , NTP1
      do n = m , NTP1
        if (mod(m+n,2) == 0) then ! symmetric -----------------
          pz(1,w,v) = pz(1,w,v) + qe(w,l) * (pu(1,m,l,v)-pu(1,m,k,v)) &
                                - qm(w,l) * (pv(2,m,l,v)+pv(2,m,k,v))
          pz(2,w,v) = pz(2,w,v) + qe(w,l) * (pu(2,m,l,v)-pu(2,m,k,v)) &
                                + qm(w,l) * (pv(1,m,l,v)+pv(1,m,k,v))
          pd(1,w,v) = pd(1,w,v) - qe(w,l) * (pv(1,m,l,v)-pv(1,m,k,v)) &
                                - qm(w,l) * (pu(2,m,l,v)+pu(2,m,k,v))
          pd(2,w,v) = pd(2,w,v) - qe(w,l) * (pv(2,m,l,v)-pv(2,m,k,v)) &
                                + qm(w,l) * (pu(1,m,l,v)+pu(1,m,k,v))
        else ! ---------------- antisymmetric -----------------
          pz(1,w,v) = pz(1,w,v) + qe(w,l) * (pu(1,m,l,v)+pu(1,m,k,v)) &
                                - qm(w,l) * (pv(2,m,l,v)-pv(2,m,k,v))
          pz(2,w,v) = pz(2,w,v) + qe(w,l) * (pu(2,m,l,v)+pu(2,m,k,v)) &
                                + qm(w,l) * (pv(1,m,l,v)-pv(1,m,k,v))
          pd(1,w,v) = pd(1,w,v) - qe(w,l) * (pv(1,m,l,v)+pv(1,m,k,v)) &
                                - qm(w,l) * (pu(2,m,l,v)-pu(2,m,k,v))
          pd(2,w,v) = pd(2,w,v) - qe(w,l) * (pv(2,m,l,v)+pv(2,m,k,v)) &
                                + qm(w,l) * (pu(1,m,l,v)-pu(1,m,k,v))
        endif
        w = w + 1
      enddo ! n
    enddo ! m
  enddo ! l
enddo ! v
!----------------------------------------------------------------------
endif ! symmetric?
return
end 


! ================
! SUBROUTINE QTEND
! ================

subroutine qtend(q,qn,uq,vq)
use legmod
implicit none

real, intent(in) :: qn(2,NLON/2,NLPP,NLEV)
real, intent(in) :: uq(2,NLON/2,NLPP,NLEV)
real, intent(in) :: vq(2,NLON/2,NLPP,NLEV)

real, intent(out) :: q(2,NESP/2,NLEV)

integer :: k ! Loop index for southern latitude
integer :: l ! Loop index for latitude
integer :: m ! Loop index for zonal wavenumber m
integer :: n ! Loop index for total wavenumber n
integer :: v ! Loop index for level
integer :: w ! Loop index for spectral mode

q(:,:,:) = 0.0

if (NLPP < NLAT) then  ! Universal (parallel executable) version
!----------------------------------------------------------------------
do v = 1 , NLEV
 do l = 1 , NLPP
  w = 1
  do m = 1 , NTP1
   do n = m , NTP1
    q(1,w,v)=q(1,w,v)+qe(w,l)*vq(1,m,l,v)+qc(w,l)*qn(1,m,l,v)+qm(w,l)*uq(2,m,l,v)
    q(2,w,v)=q(2,w,v)+qe(w,l)*vq(2,m,l,v)+qc(w,l)*qn(2,m,l,v)-qm(w,l)*uq(1,m,l,v)
    w = w + 1
   enddo ! n
  enddo ! m
 enddo ! l
enddo ! v
else                   ! Single CPU version (symmetry conserving)
!----------------------------------------------------------------------
do v = 1 , NLEV
 do l = 1 , NLAT/2
  k = NLAT+1-l
  w = 1
  do m = 1 , NTP1
   do n = m , NTP1
    if (mod(m+n,2) == 0) then ! symmetric -----------------
      q(1,w,v)=q(1,w,v)+qe(w,l)*(vq(1,m,l,v)-vq(1,m,k,v)) &
                       +qc(w,l)*(qn(1,m,l,v)+qn(1,m,k,v)) &
                       +qm(w,l)*(uq(2,m,l,v)+uq(2,m,k,v))
      q(2,w,v)=q(2,w,v)+qe(w,l)*(vq(2,m,l,v)-vq(2,m,k,v)) &
                       +qc(w,l)*(qn(2,m,l,v)+qn(2,m,k,v)) &
                       -qm(w,l)*(uq(1,m,l,v)+uq(1,m,k,v))
    else ! ---------------- antisymmetric -----------------
      q(1,w,v)=q(1,w,v)+qe(w,l)*(vq(1,m,l,v)+vq(1,m,k,v)) &
                       +qc(w,l)*(qn(1,m,l,v)-qn(1,m,k,v)) &
                       +qm(w,l)*(uq(2,m,l,v)-uq(2,m,k,v))
      q(2,w,v)=q(2,w,v)+qe(w,l)*(vq(2,m,l,v)+vq(2,m,k,v)) &
                       +qc(w,l)*(qn(2,m,l,v)-qn(2,m,k,v)) &
                       -qm(w,l)*(uq(1,m,l,v)-uq(1,m,k,v))
    endif
    w = w + 1
   enddo ! n
  enddo ! m
 enddo ! l
enddo ! v
!----------------------------------------------------------------------
endif ! symmetric?
return
end

! =================
! SUBROUTINE MKTEND
! =================

subroutine mktend(d,t,z,tn,fu,fv,ke,ut,vt)
use legmod
implicit none

real, intent(in) :: tn(2,NLON/2,NLPP,NLEV)
real, intent(in) :: fu(2,NLON/2,NLPP,NLEV)
real, intent(in) :: fv(2,NLON/2,NLPP,NLEV)
real, intent(in) :: ke(2,NLON/2,NLPP,NLEV)
real, intent(in) :: ut(2,NLON/2,NLPP,NLEV)
real, intent(in) :: vt(2,NLON/2,NLPP,NLEV)

real, intent(out) :: d(2,NESP/2,NLEV)
real, intent(out) :: t(2,NESP/2,NLEV)
real, intent(out) :: z(2,NESP/2,NLEV)

integer :: k ! Loop index for southern latitude
integer :: l ! Loop index for latitude
integer :: m ! Loop index for zonal wavenumber m
integer :: n ! Loop index for total wavenumber n
integer :: v ! Loop index for level
integer :: w ! Loop index for spectral mode

d(:,:,:) = 0.0
t(:,:,:) = 0.0
z(:,:,:) = 0.0

if (NLPP < NLAT) then  ! Universal (parallel executable) version
!----------------------------------------------------------------------
do v = 1 , NLEV
 do l = 1 , NLPP
  w = 1
  do m = 1 , NTP1
   do n = m , NTP1
    d(1,w,v)=d(1,w,v)+qq(w,l)*ke(1,m,l,v)-qe(w,l)*fv(1,m,l,v)-qm(w,l)*fu(2,m,l,v)
    d(2,w,v)=d(2,w,v)+qq(w,l)*ke(2,m,l,v)-qe(w,l)*fv(2,m,l,v)+qm(w,l)*fu(1,m,l,v)
    t(1,w,v)=t(1,w,v)+qe(w,l)*vt(1,m,l,v)+qc(w,l)*tn(1,m,l,v)+qm(w,l)*ut(2,m,l,v)
    t(2,w,v)=t(2,w,v)+qe(w,l)*vt(2,m,l,v)+qc(w,l)*tn(2,m,l,v)-qm(w,l)*ut(1,m,l,v)
    z(1,w,v)=z(1,w,v)+qe(w,l)*fu(1,m,l,v)-qm(w,l)*fv(2,m,l,v)
    z(2,w,v)=z(2,w,v)+qe(w,l)*fu(2,m,l,v)+qm(w,l)*fv(1,m,l,v)
    w = w + 1
   enddo ! n
  enddo ! m
 enddo ! l
enddo ! v
else                   ! Single CPU version (symmetry conserving)
!----------------------------------------------------------------------
do v = 1 , NLEV
 do l = 1 , NLAT/2
  k = NLAT+1-l
  w = 1
  do m = 1 , NTP1
   do n = m , NTP1
    if (mod(m+n,2) == 0) then ! symmetric -----------------
      d(1,w,v)=d(1,w,v)+qq(w,l)*(ke(1,m,l,v)+ke(1,m,k,v)) &
                       -qe(w,l)*(fv(1,m,l,v)-fv(1,m,k,v)) &
                       -qm(w,l)*(fu(2,m,l,v)+fu(2,m,k,v))
      d(2,w,v)=d(2,w,v)+qq(w,l)*(ke(2,m,l,v)+ke(2,m,k,v)) &
                       -qe(w,l)*(fv(2,m,l,v)-fv(2,m,k,v)) &
                       +qm(w,l)*(fu(1,m,l,v)+fu(1,m,k,v))
      t(1,w,v)=t(1,w,v)+qe(w,l)*(vt(1,m,l,v)-vt(1,m,k,v)) &
                       +qc(w,l)*(tn(1,m,l,v)+tn(1,m,k,v)) &
                       +qm(w,l)*(ut(2,m,l,v)+ut(2,m,k,v))
      t(2,w,v)=t(2,w,v)+qe(w,l)*(vt(2,m,l,v)-vt(2,m,k,v)) &
                       +qc(w,l)*(tn(2,m,l,v)+tn(2,m,k,v)) &
                       -qm(w,l)*(ut(1,m,l,v)+ut(1,m,k,v))
      z(1,w,v)=z(1,w,v)+qe(w,l)*(fu(1,m,l,v)-fu(1,m,k,v)) &
                       -qm(w,l)*(fv(2,m,l,v)+fv(2,m,k,v))
      z(2,w,v)=z(2,w,v)+qe(w,l)*(fu(2,m,l,v)-fu(2,m,k,v)) &
                       +qm(w,l)*(fv(1,m,l,v)+fv(1,m,k,v))
    else ! ---------------- antisymmetric -----------------
      d(1,w,v)=d(1,w,v)+qq(w,l)*(ke(1,m,l,v)-ke(1,m,k,v)) &
                       -qe(w,l)*(fv(1,m,l,v)+fv(1,m,k,v)) &
                       -qm(w,l)*(fu(2,m,l,v)-fu(2,m,k,v))
      d(2,w,v)=d(2,w,v)+qq(w,l)*(ke(2,m,l,v)-ke(2,m,k,v)) &
                       -qe(w,l)*(fv(2,m,l,v)+fv(2,m,k,v)) &
                       +qm(w,l)*(fu(1,m,l,v)-fu(1,m,k,v))
      t(1,w,v)=t(1,w,v)+qe(w,l)*(vt(1,m,l,v)+vt(1,m,k,v)) &
                       +qc(w,l)*(tn(1,m,l,v)-tn(1,m,k,v)) &
                       +qm(w,l)*(ut(2,m,l,v)-ut(2,m,k,v))
      t(2,w,v)=t(2,w,v)+qe(w,l)*(vt(2,m,l,v)+vt(2,m,k,v)) &
                       +qc(w,l)*(tn(2,m,l,v)-tn(2,m,k,v)) &
                       -qm(w,l)*(ut(1,m,l,v)-ut(1,m,k,v))
      z(1,w,v)=z(1,w,v)+qe(w,l)*(fu(1,m,l,v)+fu(1,m,k,v)) &
                       -qm(w,l)*(fv(2,m,l,v)-fv(2,m,k,v))
      z(2,w,v)=z(2,w,v)+qe(w,l)*(fu(2,m,l,v)+fu(2,m,k,v)) &
                       +qm(w,l)*(fv(1,m,l,v)-fv(1,m,k,v))
    endif
    w = w + 1
   enddo ! n
  enddo ! m
 enddo ! l
enddo ! v
!----------------------------------------------------------------------
endif ! symmetric?
return
end


! ================
! SUBROUTINE SP2FL
! ================

subroutine sp2fl(psp,pfc,klev)
use legmod
implicit none

real,    intent(in ) :: psp(NESP,klev)
real,    intent(out) :: pfc(NHOR,klev)
integer, intent(in ) :: klev

integer :: jlev

do jlev = 1,klev
   call sp2fc(psp(1,jlev),pfc(1,jlev))
enddo

return
end 


! ==================
! SUBROUTINE INVLEGA
! ==================

subroutine invlega
use pumamod
implicit none

integer :: jlev

call dv2uv(sd,sz,gu,gv)

do jlev = 1,NLEV
  call sp2fc(sd(1,jlev),gd(1,jlev))
  call sp2fc(st(1,jlev),gt(1,jlev))
  call sp2fc(sz(1,jlev),gz(1,jlev))
  if (nqspec == 1) call sp2fc(sq(1,jlev),gq(1,jlev))
enddo

call sp2fc(sp,gp)
call sp2fcdmu(sp,gpj)

return
end


! ==================
! SUBROUTINE INVLEGD
! ==================

subroutine invlegd
use pumamod
implicit none

integer :: jlev

call dv2uv(sd,sz,gu,gv)

do jlev = 1,NLEV
  if (nqspec == 1) call sp2fc(sq(1,jlev),gq(1,jlev))
  call sp2fc(st(1,jlev),gt(1,jlev))
enddo

call sp2fc(sp,gp)

return
end

