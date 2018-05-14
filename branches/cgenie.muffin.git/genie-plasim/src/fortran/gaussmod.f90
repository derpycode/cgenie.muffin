! =================
! SUBROUTINE INIGAU
! =================

subroutine inigau_plasim(klat,pz0,pzw)        ! pz0 & pzw are (kind=8) reals !!!
implicit none
integer                  :: klat       ! Number of Gaussian latitudes
real (kind=8)            :: pz0(klat)  ! Gaussian abscissas
real (kind=8)            :: pzw(klat)  ! Gaussian weights
integer                  :: jlat       ! Latitudinal loop index
integer                  :: jiter      ! Iteration loop index
integer      , parameter :: NITER = 50 ! Maximum # of iterations
real (kind=8), parameter :: PI    =  3.14159265358979_8
real (kind=8), parameter :: ZEPS  =  1.0e-16 ! Convergence criterion
real (kind=8) :: z0,z1,z2,z3,z4,z5
real (kind=8) :: ql,qld
! Compute Gaussian abscissas & weights

z0 = PI / (2*klat+1)
z1 = 1.0_8 / (klat*klat*8)
z4 = 2.0_8 / (klat*klat)

do jlat = 1 , klat/2
   z2 = z0 * (2*jlat - 0.5_8)
   z2 = cos(z2 + z1 / tan(z2))
   do jiter = 1 , NITER
      z3 = ql(klat,z2) * qld(klat,z2)
      z2 = z2 - z3
      if (abs(z3) < ZEPS) exit ! converged
   enddo ! jiter
   z5 = ql(klat-1,z2) / sqrt(klat - 0.5_8)
   pz0(jlat) = z2
   pzw(jlat) = z4 * (1.0_8 - z2 * z2) / (z5 * z5)
   pz0(klat-jlat+1) = -z2
   pzw(klat-jlat+1) = pzw(jlat)
enddo ! jlat

return
end subroutine inigau_plasim

! ===========
! FUNCTION QL
! ===========

real (kind=8) function ql(k,p)
implicit none
integer      , intent(IN) :: k
real (kind=8), intent(IN) :: p
real (kind=8) :: z0,z1,z2,z3,z4
integer :: j
z0 = acos(p)
z1 = 1.0
z2 = 0.0
do j = k , 0 , -2
   z3 = z1 * cos(z0 * j)
   z2 = z2 + z3
   z4 = (k-j+1) * (k+j) * 0.5_8
   z1 = z1 * z4 / (z4 + (j-1))
enddo ! j
if (mod(k,2) == 0) z2 = z2 - 0.5_8 * z3

z0 = sqrt(2.0_8)
do j = 1 ,k
   z0 = z0 * sqrt(1.0_8 - 0.25_8 / (j*j))
enddo ! j
ql = z0 * z2
return
end function ql

! ============
! FUNCTION QLD
! ============

real (kind=8) function qld(k,p)
implicit none
integer      , intent(IN) :: k
real (kind=8), intent(IN) :: p
real (kind=8) :: z
real (kind=8) :: ql

z = p * ql(k,p) - sqrt((k + k + 1.0_8) / (k + k - 1.0_8)) * ql(k-1,p)
qld = (p * p - 1.0_8) / (k * z)

return
end function qld
