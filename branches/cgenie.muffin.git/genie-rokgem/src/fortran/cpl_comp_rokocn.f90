

! ******************************************************************************************************************************** !
! cpl_comp_rokocn.f90
! rokgem interface rokd compositional integrator
! ******************************************************************************************************************************** !

! NOTE: 'surface' refers to ocean surface for flux passing to biogem
! ******************************************************************************************************************************** !
! COUPLE rokgem rock composition
SUBROUTINE cpl_comp_rokocn()
!SUBROUTINE cpl_comp_rokocn(     &
!     & dum_n_rok,               &
!     & dum_n_i_rok,dum_n_j_rok, &
!     & dum_n_i_ocn,dum_n_j_ocn, &
!     & dum_sfcrok,              &
!     & dum_sfcrok1              &
!     & )
!  IMPLICIT NONE
  ! dummy arguments
!  integer,intent(in)::dum_n_rok
!  integer,intent(in)::dum_n_i_rok,dum_n_j_rok
!  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
!  real,dimension(dum_n_rok,dum_n_i_rok,dum_n_j_rok),intent(in)::dum_sfcrok     ! rock-surface tracer composition; rok grid
!  real,dimension(dum_n_rok,dum_n_i_ocn,dum_n_j_ocn),intent(out)::dum_sfcrok1 ! rock-surface tracer composition; ocn grid
  ! 
  ! ANY DIFFERENCE BETWEEN OCEAN AND rock GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
  ! NOTE: do not copy the first 2 tracers (SAT and humidity) as these values are set directly by the EMBM
!  dum_sfcrok1(3:dum_n_rok,:,:) = dum_sfcrok(3:dum_n_rok,:,:)
  ! 
end SUBROUTINE cpl_comp_rokocn
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE EMBM TRACERS
!SUBROUTINE cpl_comp_rokEMBM(    &
!     & dum_n_rok,               &
!     & dum_n_i_rok,dum_n_j_rok, &
!     & dum_n_i_ocn,dum_n_j_ocn, &
!     & dum_t,                   &
!     & dum_q,                   &
!     & dum_sfcrok1)
!  IMPLICIT NONE
  ! dummy arguments
!  integer,intent(in)::dum_n_rok
!  integer,intent(in)::dum_n_i_rok,dum_n_j_rok
!  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
!  real,dimension(dum_n_i_rok,dum_n_j_rok),intent(in)::dum_t
!  real,dimension(dum_n_i_rok,dum_n_j_rok),intent(in)::dum_q
!  real,dimension(dum_n_rok,dum_n_i_ocn,dum_n_j_ocn),intent(out)::dum_sfcrok1 ! rock-surface tracer composition; ocn grid
  ! 
  ! ANY DIFFERENCE BETWEEN OCEAN AND rock GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
!  dum_sfcrok1(1,:,:) = dum_t(:,:)
!  dum_sfcrok1(2,:,:) = dum_q(:,:)
  ! 
!end SUBROUTINE cpl_comp_rokEMBM
! ******************************************************************************************************************************** !
