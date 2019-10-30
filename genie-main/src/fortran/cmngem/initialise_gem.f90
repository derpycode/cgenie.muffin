! ******************************************************************************************************************************** !
! SETUP GEM
SUBROUTINE initialise_gem()
  USE gem_util
  USE gem_data
  ! local variables
  integer::ia,io,is                                              ! tracer counter

  print*,'======================================================='
  print*,' >>> Initialising GEM geochemistry module ...'

  ! *** load goin information ***
  call sub_load_goin_gem()

  ! *** initialize GeM ***
  ! initialize tracer definitions
  CALL sub_init_tracer_atm()
  CALL sub_init_tracer_ocn()
  CALL sub_init_tracer_sed()
  ! initialize air-sea gas exchange coefficiencts
  ! define Schmidt Number coefficients
  call sub_def_schmidtnumber()
  ! define Bunsen Solubility Coefficient coefficients
  call sub_def_bunsencoefficient()
  ! define radioactive tracer decay coefficients
  call sub_def_tracer_decay()

  ! ---------------------------------------------------------- !
  !  INITIALIZE ARRAYS
  ! ---------------------------------------------------------- !
  ! NOTE: check for problems allocating array space
  !- --------------------------------------------------------- ! 
  ALLOCATE(conv_ls_lo(n_l_ocn,n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_lD_lP(n_l_sed,n_l_ocn),STAT=alloc_error)
  ALLOCATE(conv_lP_lD(n_l_ocn,n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_lRD_lP(n_l_sed,n_l_ocn),STAT=alloc_error)
  ALLOCATE(conv_lP_lRD(n_l_ocn,n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_O(n_l_ocn,n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_N(n_l_ocn,n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_S(n_l_ocn,n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_meth(n_l_ocn,n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_i(0:n_l_ocn,0:n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_lD_lP_i(0:n_l_sed,0:n_l_ocn),STAT=alloc_error)
  ALLOCATE(conv_lP_lD_i(0:n_l_ocn,0:n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_lRD_lP_i(0:n_l_sed,0:n_l_ocn),STAT=alloc_error)
  ALLOCATE(conv_lP_lRD_i(0:n_l_ocn,0:n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_i_O(0:n_l_ocn,0:n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_i_N(0:n_l_ocn,0:n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_i_S(0:n_l_ocn,0:n_l_sed),STAT=alloc_error)
  ALLOCATE(conv_ls_lo_i_meth(0:n_l_ocn,0:n_l_sed),STAT=alloc_error)
  !- --------------------------------------------------------- ! 

  if (ctrl_debug_init > 0) then
     print*,'--- TRACER SELECTION ---'
     print*,'Selected atmosphere tracers:                        : '
     do ia=1,n_atm
        if (atm_select(ia)) print*,ia,' = ',trim(string_longname_atm(ia))
     end do
     print*,'Selected ocean tracers:                             : '
     do io=1,n_ocn
        if (ocn_select(io)) print*,io,' = ',trim(string_longname_ocn(io))
     end do
     print*,'Selected sediment tracers:                          : '
     do is=1,n_sed
        if (sed_select(is)) print*,is,' = ',trim(string_longname_sed(is))
     end do
  end if

  print*,' <<< Initialisation complete'
  print*,'======================================================='

end SUBROUTINE initialise_gem
! ******************************************************************************************************************************** !
