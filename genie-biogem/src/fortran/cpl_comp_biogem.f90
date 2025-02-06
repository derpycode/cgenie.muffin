! ******************************************************************************************************************************** !
! cpl_comp_biogem.f90
! Tracer field transfers
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: OCN->GEM(GENIE)
SUBROUTINE cpl_comp_ocngem(                 &
     & dum_dts,                             &
     & dum_n_ocn,                           &
     & dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn, &
     & dum_genie_ocn                        &
     & )
  use genie_global
  use biogem_lib
  IMPLICIT NONE
  ! dummy arguments
  real,intent(in)::dum_dts
  integer,intent(in)::dum_n_ocn
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn
  real,dimension(dum_n_ocn,dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn),intent(out)::dum_genie_ocn !
  ! local variables
  integer::l,io
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '*** COUPLE TRACER FIELDS: OCN->GEM(GENIE) ***'
  IF (ctrl_debug_lvl1) print*, '    >>>'
  ! ---------------------------------------------------------- !
  ! copy tracer array
  ! NOTE: currently, the GENIE arrays are defiend with the max number of ocn tracers (not selected number)
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
!!!dum_genie_ocn(l,:,:,:) = ocn(io,:,:,:)
  end do
  dum_genie_ocn(:,:,:,:) = dum_genie_ocn(:,:,:,:) + dum_dts*ocn(:,:,:,:)/conv_yr_s
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '    <<<'
  ! ---------------------------------------------------------- !
end SUBROUTINE cpl_comp_ocngem
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: GEM(GENIE)->OCN
SUBROUTINE cpl_comp_gemocn(                 &
     & dum_n_ocn,                           &
     & dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn, &
     & dum_genie_docn                       &
     & )
  use genie_global
  use biogem_lib
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_ocn
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn
  real,dimension(dum_n_ocn,dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn),intent(inout)::dum_genie_docn !
  ! local variables
  integer::l,io
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '*** COUPLE TRACER FIELDS: GEM(GENIE)->OCN ***'
  IF (ctrl_debug_lvl1) print*, '    >>>'
  ! ---------------------------------------------------------- !
  ! update tracer array
  ! NOTE: currently, the GENIE arrays are defiend with the max number of ocn tracers (not selected number)
  ! NOTE: <dum_genie_docn> is passed in as an ANOMALY
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
!!!ocn(io,:,:,:) = dum_genie_ocn(l,:,:,:)
  end do
  ocn(:,:,:,:) = ocn(:,:,:,:) + dum_genie_docn(:,:,:,:)
  ! reset anomoly array
  dum_genie_docn(:,:,:,:) = 0.0
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '    <<<'
  ! ---------------------------------------------------------- !
end SUBROUTINE cpl_comp_gemocn
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: GEM(GENIE)->ATM
SUBROUTINE cpl_comp_gematm1(    &
     & dum_n_atm,               &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_genie_datm1,         &
     & dum_sfcatm1              &
     & )
  use genie_global
  use biogem_lib
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_genie_datm1 ! atm anomoly; ocn grid
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_sfcatm1 ! atmosphere-surface tracer composition; ocn grid
  ! local variables
  integer::l,ia
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '*** COUPLE TRACER FIELDS: GEM(GENIE)->ATM ***'
  IF (ctrl_debug_lvl1) print*, '    >>>'
  ! ---------------------------------------------------------- !
  ! update atm interface tracer array
  ! NOTE: currently, the GENIE arrays are defiend with the max number of atm tracers (not selected number)
  ! NOTE: <dum_genie_datm1> is passed in as an ANOMALY
  DO l=1,n_l_atm
     ia = conv_iselected_ia(l)
!!!atm(ia,:,:) = dum_genie_atm(l,:,:)
  end do
  dum_sfcatm1(:,:,:) = dum_sfcatm1(:,:,:) + dum_genie_datm1(:,:,:)
  ! reset anomoly array
  dum_genie_datm1(:,:,:) = 0.0
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '    <<<'
  ! ---------------------------------------------------------- !
end SUBROUTINE cpl_comp_gematm1
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER CONVERSION MATRIX: OCN->SED
SUBROUTINE cpl_conv_ocnsed(      &
     & dum_n_ocn,dum_n_sed,      &
     & dum_n_i_sed,dum_n_j_sed,  &
     & dum_sfcsumocn,            &
     & dum_sfcsumconv            &
     & )
  use biogem_box
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_ocn,dum_n_sed
  integer,intent(in)::dum_n_i_sed,dum_n_j_sed                                          ! 
  real,dimension(dum_n_ocn,dum_n_i_sed,dum_n_j_sed),intent(in)::dum_sfcsumocn          !
  real,dimension(dum_n_ocn,dum_n_sed,dum_n_i_sed,dum_n_j_sed),intent(out)::dum_sfcsumconv !
  ! local variables
  integer::i,j,l                                                 ! 
  real,dimension(1:n_l_ocn)::loc_vocn                            !
  real,dimension(n_l_ocn,n_l_sed)::loc_conv_ls_lo                !
  ! ---------------------------------------------------------- !
  ! INITIALIZE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  dum_sfcsumconv = 0.0
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '*** COUPLE TRACER CONVERSION MATRIX: OCN->SED ***'
  IF (ctrl_debug_lvl1) print*, '    >>>'
  ! ---------------------------------------------------------- !
  DO i=1,dum_n_i_sed
     DO j=1,dum_n_j_sed
        DO l=1,n_l_ocn
           loc_vocn(l) = dum_sfcsumocn(l2io(l),i,j)
        end DO
        call sub_box_remin_redfield(loc_vocn(:),loc_conv_ls_lo(:,:))
        dum_sfcsumconv(:,:,i,j) = fun_conv_lslo2sedocn(loc_conv_ls_lo(:,:))
     end DO
  end DO
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '    <<<'
  ! ---------------------------------------------------------- !
end SUBROUTINE cpl_conv_ocnsed
! ******************************************************************************************************************************** !

