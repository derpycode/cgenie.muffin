! ******************************************************************************************************************************** !
! cpl_comp_gemlite.f90
! GEMlite tracer field transfers
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: ATM,OCN->GEMLITE
SUBROUTINE cpl_comp_gemglt(               &
     & dum_n_atm,dum_n_ocn,                 &
     & dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn, &
     & dum_genie_atm1,                      &
     & dum_genie_ocn                        &
     & )
  use genie_global
  use gemlite_lib
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm,dum_n_ocn
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_genie_atm1 !
  real,dimension(dum_n_ocn,dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn),intent(inout)::dum_genie_ocn !
  ! local variables
  integer::l,ia,io
  ! copy tracer array
  DO l=1,n_l_atm
     ia = conv_iselected_ia(l)
     atm(l,:,:) = dum_genie_atm1(ia,:,:)
  end do
  !atm = dum_genie_atm
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     ocn(l,:,:,:) = dum_genie_ocn(io,:,:,:)
  end do
  !ocn = dum_genie_ocn
  ! reset composition arrays
  dum_genie_atm1(:,:,:)  = 0.0
  dum_genie_ocn(:,:,:,:) = 0.0
end SUBROUTINE cpl_comp_gemglt
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: GEMLITE->OCN,ATM
SUBROUTINE cpl_comp_gltgem_d(               &
     & dum_n_atm,dum_n_ocn,                 &
     & dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn, &
     & dum_genie_datm1,                     &
     & dum_genie_docn                       &
     & )
  use genie_global
  use gemlite_lib
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm,dum_n_ocn
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(out)::dum_genie_datm1 !
  real,dimension(dum_n_ocn,dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn),intent(out)::dum_genie_docn !
  ! local variables
  integer::l,ia,io
  ! initialize arrays
  dum_genie_datm1(:,:,:)  = 0.0
  dum_genie_docn(:,:,:,:) = 0.0
  ! copy tracer anomaly arrays
  DO l=1,n_l_atm
     ia = conv_iselected_ia(l)
     dum_genie_datm1(ia,:,:) = datm(l,:,:)
  end do
  !dum_genie_atm = atm
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     dum_genie_docn(io,:,:,:) = docn(l,:,:,:)
  end do
  !dum_genie_ocn = ocn
  ! reset composition anomaly arrays
  datm(:,:,:)   = 0.0
  docn(:,:,:,:) = 0.0
end SUBROUTINE cpl_comp_gltgem_d
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: GEMLITE->OCN,ATM (SUMMED)
SUBROUTINE cpl_comp_gltgem_dsum(            &
     & dum_n_atm,dum_n_ocn,                 &
     & dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn, &
     & dum_genie_datm1,                     &
     & dum_genie_docn                       &
     & )
  use genie_global
  use gemlite_lib
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm,dum_n_ocn
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(out)::dum_genie_datm1 !
  real,dimension(dum_n_ocn,dum_n_i_ocn,dum_n_j_ocn,dum_n_k_ocn),intent(out)::dum_genie_docn !
  ! local variables
  integer::l,ia,io
  ! initialize receiving arrays
  dum_genie_datm1(:,:,:)  = 0.0
  dum_genie_docn(:,:,:,:) = 0.0
  ! copy tracer anomaly arrays
  ! NOTE: do not re-set integrated composition anomaly arrays yet ...
  DO l=1,n_l_atm
     ia = conv_iselected_ia(l)
     dum_genie_datm1(ia,:,:) = datm_sum(l,:,:)
  end do
  !dum_genie_atm = atm
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     dum_genie_docn(io,:,:,:) = docn_sum(l,:,:,:)
  end do
  !dum_genie_ocn = ocn
  ! reset composition anomaly arrays
  datm_sum(:,:,:)   = 0.0
  docn_sum(:,:,:,:) = 0.0
end SUBROUTINE cpl_comp_gltgem_dsum
! ******************************************************************************************************************************** !

