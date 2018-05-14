
! ******************************************************************************************************************************** !
! GOLDlite LOOP SUBROUTINE
subroutine goldlite( &
     & dum_ts,       &
     & dum_ts1       &
     & )
  use gem_cmn
  use gem_util
  USE goldlite_lib
  USE goldlite_box
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  real,intent(inout),dimension(n_c,n_i,n_j,n_k)::dum_ts                 ! 
  real,intent(inout),dimension(n_c,n_i,n_j,n_k)::dum_ts1                ! 
  ! LOCAL VARIABLES
!!!

print*,'> START goldlite'

  ! MAIN
  if (opt_colorpattern /= 0) then
     ! diagnose transport
     call calc_coltrans(n_c,n_l,dum_ts(n_c,:,:,(n_k - par_nk_sur + 1):n_k))
     ! copy (reset) color pattern to <ts> and <ts1> arrays
print*,'>'
     dum_ts(:,:,:,:)  = color_init(:,:,:,:)
     dum_ts1(:,:,:,:) = color_init(:,:,:,:)
print*,'<'
  end if
print*,'< END goldlite'
end subroutine goldlite
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! GOLDlite LOOP SUBROUTINE
subroutine goldlite_applycoltrans( &
     & dum_p,                    &
     & dum_tracer,               &
     & dum_dtracer               &
     & )
  use gem_cmn
  use gem_util
  USE goldlite_lib
  USE goldlite_box
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  integer,intent(in)::dum_p                                                    ! number of plankton tracers
  real,intent(in),dimension(dum_p,n_i,n_j,par_nk_sur)::dum_tracer              ! 
  real,intent(inout),dimension(dum_p,n_i,n_j,par_nk_sur)::dum_dtracer          ! 
  ! LOCAL VARIABLES   
  integer::i,j,k,n                                                             ! local loop counting variables
  integer::loc_i,loc_j

print*,'hello'

  ! initialize tracer anomoly?????????????????

  ! transport tracer form origination to destination cell
  ! loop through locations of tracer *origination* cells
  DO i=1,n_i
     DO j=1,n_j
        do k=1,par_nk_sur
           do n=1,par_n_color_ftrans_max
              ! set location of cell tracers are being transported to (*destination* cells)
              loc_i = int(color_ftrans(n,i,j,1))
              loc_j = int(color_ftrans(n,i,j,2))
              dum_dtracer(:,loc_i,loc_j,k) =  dum_dtracer(:,loc_i,loc_j,k) + color_ftrans(n,i,j,3)*dum_tracer(:,i,j,k)
              dum_dtracer(:,i,j,k) =  dum_dtracer(:,i,j,k) - color_ftrans(n,i,j,3)*dum_tracer(:,i,j,k)
           end do
        end do
     end do
  end do

end subroutine goldlite_applycoltrans
! ******************************************************************************************************************************** !

