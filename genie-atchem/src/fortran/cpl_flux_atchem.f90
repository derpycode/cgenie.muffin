
! ******************************************************************************************************************************** !
! cpl_flux_atchem.f90
! AtCheM interface flux integrator
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE AtChem fluxes
SUBROUTINE cpl_flux_ocnatm(     &
     & dum_dts,                 &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_sfxatm1,             &
     & dum_sfxsumatm            &
     & )
  USE atchem_lib
  IMPLICIT NONE
  ! dummy arguments
  real,intent(in)::dum_dts
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_sfxatm1   ! atmosphere-surface fluxes; ocn grid
  real,dimension(dum_n_atm,dum_n_i_atm,dum_n_j_atm),intent(inout)::dum_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '*** COUPLE AtChem fluxes ***'
  IF (ctrl_debug_lvl1) print*, '    >>>'
  ! ---------------------------------------------------------- !
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! integrate flux to atmosphere <dum_sfxatm1> (mol m-2 s-1)
  ! running total <dum_sfxsumatm> is in units of (mol m-2)
  dum_sfxsumatm(:,:,:) = dum_sfxsumatm(:,:,:) + dum_dts*dum_sfxatm1(:,:,:)
  ! zero flux
  dum_sfxatm1(:,:,:) = 0.0
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '    <<<'
  ! ---------------------------------------------------------- !
END SUBROUTINE cpl_flux_ocnatm
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE AtChem fluxes
SUBROUTINE cpl_flux_lndatm(     &
     & dum_dts,                 &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_n_i_lnd,dum_n_j_lnd, &
     & dum_sfxatm_lnd,          &
     & dum_sfxsumatm            &
     & )
  USE atchem_lib
  IMPLICIT NONE
  ! dummy arguments
  real,intent(in)::dum_dts
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  integer,intent(in)::dum_n_i_lnd,dum_n_j_lnd
  real,dimension(dum_n_atm,dum_n_i_lnd,dum_n_j_lnd),intent(inout)::dum_sfxatm_lnd   ! atmosphere-surface fluxes; lnd grid
  real,dimension(dum_n_atm,dum_n_i_atm,dum_n_j_atm),intent(inout)::dum_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '*** COUPLE AtChem fluxes ***'
  IF (ctrl_debug_lvl1) print*, '    >>>'
  ! ---------------------------------------------------------- !
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! integrate flux to atmosphere <dum_sfxatm_lnd> (mol m-2 s-1)
  ! running total <dum_sfxsumatm> is in units of (mol m-2)
  dum_sfxsumatm(:,:,:) = dum_sfxsumatm(:,:,:) + dum_dts*dum_sfxatm_lnd(:,:,:)
  ! zero flux
  dum_sfxatm_lnd(:,:,:) = 0.0
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '    <<<'
  ! ---------------------------------------------------------- !
END SUBROUTINE cpl_flux_lndatm
! ******************************************************************************************************************************** !

