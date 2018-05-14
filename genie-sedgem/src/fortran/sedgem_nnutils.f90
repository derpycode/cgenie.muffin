! ******************************************************************************************************************************** !
! sedgem_nnutils.f90
! NETCDF UTILITIES MODULE
! ******************************************************************************************************************************** !

module sedgem_nnutils


  use genie_control
  USE gem_cmn
  IMPLICIT NONE
  SAVE


  ! ****************************************************************************************************************************** !
  ! PARAMETERS AND VARIABLES
  ! ****************************************************************************************************************************** !


  ! ***  ***
  INTEGER,PARAMETER::nl = 2  ! number of layers
  INTEGER,PARAMETER::par_nn_input   = 5
  INTEGER,PARAMETER::par_nn_neurons = 12
  INTEGER,PARAMETER::par_nn_target  = 1
  INTEGER,dimension(2)::par_nn_ifuna  = (/3, 1/)  ! 1:linear, 2:logsig, 3:tansig
  real,dimension(2)::par_nn_funcoa = (/1, 1/) ! coefficients for functions

  ! *** ***
  ! NOTE: order; D(m), dco3(mol kg-1), o2(mol kg-1), calpc (dim-less fraction), rainorg (mol cm-2 yr-1)
  real,dimension(5)::nn_min = (/    0.0, -100.0e-6, 200.0e-6, 0.0,  0.0e-6/)
  real,dimension(5)::nn_max = (/10000.0,  100.0e-6, 200.0e-6, 1.0, 49.0e-6/)
  real,allocatable,target:: &
       a0(:), &
       w1(:,:),b1(:),a1(:),dadn1(:),dw1(:,:),db1(:),s1(:), &
       w2(:,:),b2(:),a2(:),dadn2(:),dw2(:,:),db2(:),s2(:), &
       w3(:,:),b3(:),a3(:),dadn3(:),dw3(:,:),db3(:),s3(:)
  real,pointer::w(:,:),wp(:,:),b(:),dadn(:),dw(:,:),db(:),s(:),am(:),sp(:)
  real,allocatable::p(:),t(:)
  integer::nn_current_layer,nn_ifun
  real::nn_funco
  real::nn_mint,nn_maxt
  real,dimension(par_nn_input)::nn_minp,nn_maxp
  real,pointer::nn_a(:)


contains
  
  
  ! ****************************************************************************************************************************** !
  ! 
  function fun_nn_parnorm(dum_x,dum_min,dum_max,dum_isize)
    ! dummy arguments
    integer::dum_isize
    real,dimension(dum_isize),intent(in)::dum_x,dum_min,dum_max
    ! result variable
    real,dimension(dum_isize)::fun_nn_parnorm
    ! local variables
    integer::i
    real,dimension(dum_isize)::loc_parnorm
    !
    do i=1,dum_isize
       if (dum_max(i) - dum_min(i) < const_real_nullsmall) then
          loc_parnorm(i) = 1.0
       else
          loc_parnorm(i) = 2.0*(dum_x(i) - dum_min(i))/(dum_max(i) - dum_min(i)) - 1.0
          if (loc_parnorm(i) < -1.0) loc_parnorm(i) = -1.0
          if (loc_parnorm(i) > 1.0)  loc_parnorm(i) = 1.0
       end if
    end do
    ! return value
    fun_nn_parnorm(:) = loc_parnorm(:)
  end function fun_nn_parnorm
  ! ****************************************************************************************************************************** !
  
  
  ! ****************************************************************************************************************************** !
  ! 
  function fun_nn_premn(dum_x,dum_min,dum_max,dum_isize)
    ! dummy arguments
    integer::dum_isize
    real,dimension(dum_isize),intent(in)::dum_x,dum_min,dum_max
    ! result variable
    real,dimension(dum_isize)::fun_nn_premn
    ! return value
    fun_nn_premn = 2.0*(dum_x - dum_min)/(dum_max - dum_min) - 1.0
  end function fun_nn_premn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  function fun_nn_posmn(dum_x,dum_mint,dum_maxt)
    ! result variable
    real::fun_nn_posmn
    ! dummy arguments
    real,intent(in)::dum_x,dum_mint,dum_maxt
    ! return value
    fun_nn_posmn = 0.5*(dum_x + 1.0)*(dum_maxt - dum_mint) + dum_mint
  end function fun_nn_posmn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! 
  subroutine sub_nn_allocate_network
    USE genie_util, ONLY: check_iostat
    ! local variables
    INTEGER::i,j,alloc_stat
    ! 
    i=par_nn_input
    allocate(a0(i))
    j=i
    i=par_nn_neurons
    allocate(w1(i,j),b1(i),dw1(i,j),db1(i),a1(i),dadn1(i),s1(i))
    j=i
    i=par_nn_target
    allocate(w2(i,j),b2(i),dw2(i,j),db2(i),a2(i),dadn2(i),s2(i),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    allocate(p(par_nn_input),t(par_nn_target),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
  end subroutine sub_nn_allocate_network
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  subroutine sub_nn_point_layer(dum_m)
    ! dummy arguments
    integer dum_m
    !
    if (dum_m.eq.1) then
       am=>a0
       nn_a=>a1
       dadn=>dadn1
       s=>s1
       sp=>s2
       w=>w1
       dw=>dw1
       b=>b1
       db=>db1
       wp=>w2
       nn_ifun=par_nn_ifuna(1)
       nn_funco=par_nn_funcoa(1)
    else if (dum_m.eq.2) then
       am=>a1
       nn_a=>a2
       dadn=>dadn2
       s=>s2
       sp=>s3
       w=>w2
       dw=>dw2
       b=>b2
       db=>db2
       wp=>w3
       nn_ifun=par_nn_ifuna(2)
       nn_funco=par_nn_funcoa(2)
    else if (dum_m.eq.3) then
       am=>a2
       nn_a=>a3
       dadn=>dadn3
       s=>s3
       w=>w3
       dw=>dw3
       b=>b3
       db=>db3
       nullify(sp)
       nullify(wp)
    else
       print *,' m not in range, cannot repoint. m=',dum_m,' ,nl=',nl
       stop
    endif
    nn_current_layer = dum_m
  end subroutine sub_nn_point_layer
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  subroutine sub_nn_forward_model(dum_p)
    ! dummy arguments
    real,optional::dum_p(:)
    ! local variables
    integer::m
    !
    if (present(dum_p)) a0 = dum_p
    do m=1,nl
       call sub_nn_point_layer(m)
       call fun_nn_f(dadn,nn_a,matmul(w,am)+b,nn_funco,nn_ifun)
    enddo
  end subroutine sub_nn_forward_model
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  !
  subroutine fun_nn_f(dum_dadn,dum_a,dum_n,dum_coef,dum_ifun)
    ! dummy arguments
    integer::dum_ifun
    real::dum_dadn(:),dum_a(:),dum_n(:),dum_coef
    ! local variables
    integer::i
    !
    select case(dum_ifun)
    case(1) ! linear transfer function
       forall (i=1:size(dum_n,1))
          dum_a(i)=dum_n(i)*dum_coef
          dum_dadn(i)=dum_coef
       end forall
    case(2) ! logsig transfer function
       do i=1,size(dum_n,1)
          dum_a(i)=1./(1.+exp(-dum_coef*dum_n(i)))
          dum_dadn(i)=dum_coef*dum_a(i)*(1-dum_a(i))!derivative from logsig
       enddo
    case(3) ! tansig transfer function
       do i=1,size(dum_n,1)
          dum_a(i)=2./(1.+exp(-2*dum_n(i)))-1.
          dum_dadn(i)=1.-dum_a(i)**2 !derivative from tansig
       enddo
    case default
       print *, 'cannot find a function for ',dum_ifun,' ...stopping'
         stop
      endselect
    end subroutine fun_nn_f
  ! ****************************************************************************************************************************** !


end module sedgem_nnutils

