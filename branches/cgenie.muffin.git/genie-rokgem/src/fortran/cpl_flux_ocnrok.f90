

! ******************************************************************************************************************************** !
! cpl_flux_ocnrok.f90
! rokgem interface flux integrator
! ******************************************************************************************************************************** !

! Edited from original atchem/cpl_flux_ocnatm.f90 
!  -> changed instances of atm->rok (for rocks on land)
! Only want runoff flux from atm to ocean (no flux from land to atm)
! maybe don't need this as should be dumping solute fluxes in coastal cells rather than doing
! a cell to cell flux matching 2 grids!
! ******************************************************************************************************************************** !
! COUPLE fluxes surface (embm) -> rokgem
SUBROUTINE cpl_flux_ocnrok()
!SUBROUTINE cpl_flux_ocnrok(     &
!     & dum_dts,                 &
!     & dum_n_rok,               &
!     & dum_n_i_rok,dum_n_j_rok, &
!     & dum_n_i_ocn,dum_n_j_ocn, &
!     & dum_sfxrok1,             &
!     & dum_sfxsumrok            &
!     & )
!  IMPLICIT NONE
  ! dummy arguments
!  real,intent(in)::dum_dts
!  integer,intent(in)::dum_n_rok
!  integer,intent(in)::dum_n_i_rok,dum_n_j_rok
!  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
!  real,dimension(dum_n_rok,dum_n_i_ocn,dum_n_j_ocn),intent(in)::dum_sfxrok1   ! surface(embm)->rocks fluxes; ocn grid
!  real,dimension(dum_n_rok,dum_n_i_rok,dum_n_j_rok),intent(out)::dum_sfxsumrok ! surface(embm)->rocks fluxes; integrated, rokgem grid
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ROKGEM GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! integrate flux to rokgem <dum_sfxrok1> (mol m-2 s-1)
  ! running total <dum_sfxsumrok> is in units of (mol m-2)
!  dum_sfxsumrok(:,:,:) = dum_sfxsumrok(:,:,:) + dum_dts*dum_sfxrok1(:,:,:)
  ! zero flux - put in initialise_rokgem when in use
!  dum_sfxrok1(:,:,:) = 0.0
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
END SUBROUTINE cpl_flux_ocnrok
! ******************************************************************************************************************************** !


