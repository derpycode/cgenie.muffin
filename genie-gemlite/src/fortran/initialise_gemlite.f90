
! ******************************************************************************************************************************** !
! SETUP GEMlite
SUBROUTINE initialise_gemlite( &
     & dum_dsc,                &
     & dum_k1,                 &
     & dum_dz,                 &
     & dum_dza,                &
     & dum_sv,                 &
     & dum_sfxsumsed1,         &
     & dum_sfxsumrok1_gem,     &
     & dum_sfxsumatm1_gem      &
     & )
  USE gemlite_lib
  USE gemlite_data
  USE genie_util, ONLY: check_iostat
  ! dummy arguments
  real,INTENT(in)::dum_dsc                                              ! 
  integer,DIMENSION(n_i,n_j),INTENT(in)::dum_k1                         !
  REAL,DIMENSION(n_k),INTENT(in)::dum_dz,dum_dza                        ! 
  REAL,DIMENSION(0:n_j),INTENT(in)::dum_sv                              ! 
  real,DIMENSION(n_sed,n_i,n_j),intent(inout)::dum_sfxsumsed1           ! 
  REAL,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxsumrok1_gem       ! 
  REAL,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfxsumatm1_gem       ! 
  ! local variables
!!!

  print*,'======================================================='
  print*,' >>> Initialising GEMlite ocean biogeochem. module ...'

  ! *** load goin information ***
  call sub_load_goin_gemlite()

  ! *** dimension tracer arrays ***
  ! NOTE: check for problems allocating array space
  ! tracer arrays
  ALLOCATE(atm(n_l_atm,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(datm(n_l_atm,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(datm_sum(n_l_atm,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(ocn(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(docn(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(docn_sum(n_l_ocn,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! 'physics'
  ALLOCATE(phys_atm_A(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(phys_atm_V(n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(phys_ocn_Dmid(n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(phys_ocn_A(n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(phys_ocn_V(n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(phys_ocn_mask(n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(phys_ocnatm_seaice (n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! carbonate chemsitrty [SURFACE ONLY]
  ALLOCATE(carb(n_carb,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(carbconst(n_carbconst,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(carbalk(n_carbalk,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(carbisor(n_carbisor,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)

  ! *** INITIALIZE GEMlite ***
  ! initialize dynamically-allocated arrays (those not done elsewhere)
  atm(:,:,:)        = 0.0                                               !
  datm(:,:,:)       = 0.0                                               !
  datm_sum(:,:,:)   = 0.0                                               !
  ocn(:,:,:,:)      = 0.0                                               !
  docn(:,:,:,:)     = 0.0                                               !
  docn_sum(:,:,:,:) = 0.0                                               !
  carb(:,:,:)       = 0.0                                               !
  carbconst(:,:,:)  = 0.0                                               !
  carbalk(:,:,:)    = 0.0                                               !
  carbisor(:,:,:)   = 0.0                                               !

  ! *** copy GOLDSTEIn parameters ***
  ! copy dimensional scale factors for ocean
  goldstein_dsc    = dum_dsc
  ! copy ocean bottom index grid
  goldstein_k1(:,:) = dum_k1(:,:)
  ! miscellaneous
  goldstein_dz(:)  = dum_dz(:)
  goldstein_dza(:) = dum_dza(:)
  goldstein_sv(:)  = dum_sv(:)

  ! *** set-up interfacing ***
  ! initialize interface arrays
  dum_sfxsumsed1(:,:,:) = 0.0
  dum_sfxsumrok1_gem(:,:,:) = 0.0
  dum_sfxsumatm1_gem(:,:,:) = 0.0

  ! *** initialize GEMlite ***
  ! initialize atmosphere and ocean grids & physics
  CALL sub_init_phys_ocn()

  print*,' <<< Initialisation complete'
  print*,'======================================================='

  return

END SUBROUTINE initialise_gemlite
! ******************************************************************************************************************************** !
