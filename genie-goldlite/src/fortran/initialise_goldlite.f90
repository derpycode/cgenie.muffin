
! ******************************************************************************************************************************** !
! SETUP GOLDlite
SUBROUTINE initialise_goldlite( &
     & dum_ts,                  &
     & dum_ts1                  &
     & )
  USE goldlite_lib
  USE goldlite_box
  USE goldlite_data
  USE genie_util, ONLY: check_iostat
  ! DUMMY ARGUMENTS
  real,intent(inout),dimension(1:n_c,n_i,n_j,n_k)::dum_ts               ! 
  real,intent(inout),dimension(1:n_c,n_i,n_j,n_k)::dum_ts1              ! 
  ! local variables
!!!

  print*,'======================================================='
  print*,' >>> Initialising GOLDlite ocean biogeochem. module ...'

  ! *** load goin information ***
  call sub_load_goin_goldlite()

  ! *** dimension tracer arrays ***
  ! NOTE: check for problems allocating array space
  ! misc
  ALLOCATE(color_init(n_c,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(color_init_c(n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(color_ftrans(par_n_color_ftrans_max,n_i,n_j,3),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)

  ! *** INITIALIZE GOLDlite ***
  ! set color array
  call sub_set_colorarray(n_c,n_l)
  ! copy color pattern to <ts> and <ts1> arrays
  dum_ts(:,:,:,:) = color_init(:,:,:,:)
  dum_ts1(:,:,:,:) = color_init(:,:,:,:)

  print*,' <<< Initialisation complete'
  print*,'======================================================='

  return

END SUBROUTINE initialise_goldlite
! ******************************************************************************************************************************** !

