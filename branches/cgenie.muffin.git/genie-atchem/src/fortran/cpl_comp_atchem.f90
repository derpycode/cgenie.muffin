! ******************************************************************************************************************************** !
! cpl_comp_atchem.f90
! Tracer field transfers
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: ATM->GEM(GENIE)
SUBROUTINE cpl_comp_atmgem(     &
     & dum_dts,                 &
     & dum_n_atm,               &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_genie_atm1           &
     & )
  use genie_global
  use atchem_lib
  IMPLICIT NONE
  ! dummy arguments
  real,intent(in)::dum_dts
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_genie_atm1 !
  ! local variables
  integer::l,ia
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! create time-averaged tracer array
  ! NOTE: currently, the GENIE arrays are defiend with the max number of atm tracers (not selected number)
  DO l=1,n_l_atm
     ia = conv_iselected_ia(l)
     !!!dum_genie_atm(l,:,:) = atm(ia,:,:)
  end do
  dum_genie_atm1(:,:,:) = dum_genie_atm1(:,:,:) + dum_dts*atm(:,:,:)/conv_yr_s
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
end SUBROUTINE cpl_comp_atmgem
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: GEM(GENIE)->ATM
SUBROUTINE cpl_comp_gematm(     &
     & dum_n_atm,               &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_genie_datm1          &
     & )
  use genie_global
  use atchem_lib
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_genie_datm1 !
  ! local variables
  integer::l,ia
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! update tracer array
  ! NOTE: currently, the GENIE arrays are defiend with the max number of atm tracers (not selected number)
  ! NOTE: <dum_genie_atm1> is passed as an ANOMALY
  DO l=1,n_l_atm
     ia = conv_iselected_ia(l)
     !!!atm(ia,:,:) = dum_genie_datm1(l,:,:)
  end do
  atm(:,:,:) = atm(:,:,:) + dum_genie_datm1(:,:,:)
  ! reset anomaly array
  dum_genie_datm1(:,:,:) = 0.0
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
end SUBROUTINE cpl_comp_gematm
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE AtChem atmospheric composition
SUBROUTINE cpl_comp_atmocn(     &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_sfcatm,              &
     & dum_sfcatm1              &
     & )
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
  real,dimension(dum_n_atm,dum_n_i_atm,dum_n_j_atm),intent(in)::dum_sfcatm     ! atmosphere-surface tracer composition; atm grid
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_sfcatm1 ! atmosphere-surface tracer composition; ocn grid
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
  ! NOTE: OLD: do not copy the first 2 tracers (SAT and humidity) as these values are set directly by the EMBM
  ! NOTE: now ... copy them to ensure that atm temeprsture is available in atchem
!!!  dum_sfcatm1(3:dum_n_atm,:,:) = dum_sfcatm(3:dum_n_atm,:,:)
  dum_sfcatm1(1:dum_n_atm,:,:) = dum_sfcatm(1:dum_n_atm,:,:)
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
end SUBROUTINE cpl_comp_atmocn
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE EMBM TRACERS
SUBROUTINE cpl_comp_EMBMatm(       &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_t,                   &
     & dum_q,                   &
     & dum_sfcatm)
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  real,dimension(dum_n_i_atm,dum_n_j_atm),intent(in)::dum_t
  real,dimension(dum_n_i_atm,dum_n_j_atm),intent(in)::dum_q
  real,dimension(dum_n_atm,dum_n_i_atm,dum_n_j_atm),intent(inout)::dum_sfcatm ! atmosphere-surface tracer composition; ocn grid
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
  ! NOTE: temperature is degrees C
  dum_sfcatm(1,:,:) = dum_t(:,:)
  dum_sfcatm(2,:,:) = dum_q(:,:)
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
end SUBROUTINE cpl_comp_EMBMatm
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE EMBM TRACERS (OLD)
SUBROUTINE cpl_comp_EMBM(       &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_t,                   &
     & dum_q,                   &
     & dum_sfcatm1)
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
  real,dimension(dum_n_i_atm,dum_n_j_atm),intent(in)::dum_t
  real,dimension(dum_n_i_atm,dum_n_j_atm),intent(in)::dum_q
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_sfcatm1 ! atmosphere-surface tracer composition; ocn grid
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
  ! NOTE: temperature is degrees C
  dum_sfcatm1(1,:,:) = dum_t(:,:)
  dum_sfcatm1(2,:,:) = dum_q(:,:)
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
end SUBROUTINE cpl_comp_EMBM
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE AtChem atmospheric composition
SUBROUTINE cpl_comp_atmlnd(     &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_n_i_lnd,dum_n_j_lnd, &
     & dum_sfcatm,              &
     & dum_sfcatm_lnd           &
     & )
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  integer,intent(in)::dum_n_i_lnd,dum_n_j_lnd
  real,dimension(dum_n_atm,dum_n_i_atm,dum_n_j_atm),intent(in)::dum_sfcatm        ! atmosphere-surface tracer composition; atm grid
  real,dimension(dum_n_atm,dum_n_i_lnd,dum_n_j_lnd),intent(inout)::dum_sfcatm_lnd ! atmosphere-surface tracer composition; lnd grid
  ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
  ! NOTE: do not copy the first 2 tracers (SAT and humidity) as these values are set directly by the EMBM
  dum_sfcatm_lnd(3:dum_n_atm,:,:) = dum_sfcatm(3:dum_n_atm,:,:)
  ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
end SUBROUTINE cpl_comp_atmlnd
! ******************************************************************************************************************************** !

