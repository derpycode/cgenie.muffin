! ******************************************************************************************************************************** !
! goldlite_box.f90
! 
! MISCELLANEOUS ROUTINES
! ******************************************************************************************************************************** !


MODULE goldlite_box


  USE goldlite_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! SET COLOR TRACER ARRAY
  subroutine sub_set_colorarray( &
       & dum_nc,                 &
       & dum_nk                  &
       & )
    ! DUMMY ARGUMENTS
    integer,intent(in)::dum_nc                                          ! number of color tracers
    integer,intent(in)::dum_nk                                          ! number of surface levels
    ! LOCAL VARIABLES
    integer::i,j                                                        !
    integer::loc_c                                                      !
    ! initialize color array
    if (opt_colorpattern /= 0) color_init(:,:,:,:) = 0.0
    ! set color pattern
    DO i=1,n_i
       DO j=1,n_j
          select CASE (opt_colorpattern)
          case (0)
             ! *nothing*
          case (1)
             loc_c = mod(2*j - 1 + (mod(i - 1,dum_nc)),dum_nc) + 1
             color_init(loc_c,i,j,(n_k - dum_nk + 1):n_k) = 1.0
             color_init_c(i,j,(n_k - dum_nk + 1):n_k) = loc_c
          case (2)
             loc_c = mod(2*j - 1 + (mod(i - 1,dum_nc)),dum_nc)
             color_init(1,i,j,(n_k - dum_nk + 1):n_k) = real(loc_c)/real(dum_nc - 1)
          case default
!!!
          end select
       end do
    end do
  end subroutine sub_set_colorarray
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DIAGNOSE TRANSPORT
  subroutine calc_coltrans( &
       & dum_nc,            &
       & dum_nk,            &
       & dum_tracer         &
       & )
    ! DUMMY ARGUMENTS
    integer,intent(in)::dum_nc                                          ! number of color tracers
    integer,intent(in)::dum_nk                                          ! number of surface levels
    real,intent(in),dimension(dum_nc,n_i,n_j,dum_nk)::dum_tracer        ! 
    ! LOCAL VARIABLES
    integer::n,c                                                        !
    integer::i,j,i2,j2                                                     !
    integer::loc_i,loc_j                                                        !

print*,'> START calc_coltrans'

    ! BLAH
    DO i=1,n_i
       DO j=1,n_j
          ! first determine original 'color' of cell
          ! NOTE: base this on template color pattern array

          do c=1,dum_nc
             if (color_init(c,i,j,n_k) == 1.0) then

                ! create surrounding transport matrix
                n = 0
                do i2=i-1,i+1
                   if (i2 < 1) then
                      loc_i = n_i
                   elseif (i2 > n_i)then
                      loc_i = 1
                   else
                      loc_i = i2
                   endif
                   do j2=j-1,j+1
                      if (j2 < 1) then
                         exit
                      elseif (j2 > n_j)then
                         exit
                      else
                         loc_j = j2
                      endif
                      if ((abs(i - loc_i) + abs(j - loc_j)) == 1) then

                         n = n + 1

                         If (n > par_n_color_ftrans_max) then
                            ! help!
                            print*,'help!'
                            stop
                         endif

                         if ((loc_j >= 1) .AND. (loc_j <= n_j)) then

!!!print*,n,i,j,loc_i,loc_j,color_init(c,i,j,n_k)
!!!print*,dum_tracer(c,loc_i,loc_j,1)

                            color_ftrans(n,i,j,1) = real(loc_i)
                            color_ftrans(n,i,j,2) = real(loc_j)
                            color_ftrans(n,i,j,3) = dum_tracer(c,loc_i,loc_j,1)/color_init(c,i,j,n_k)

                         else
                            ! do nothing (off grid)
                         end if

                      end if

                   end do
                end do

             end if
          end do

       end do
    end do

print*,'< END calc_coltrans'

  end subroutine calc_coltrans
  ! ****************************************************************************************************************************** !


!!$  ! ****************************************************************************************************************************** !
!!$  ! 
!!$  subroutine calc_applycoltrans( &
!!$       & dum_p,                  &
!!$       & dum_tracer              &
!!$       & dum_dtracer             &
!!$       & )
!!$    ! DUMMY ARGUMENTS
!!$    integer::intent(in)::dum_p
!!$    real,intent(in),dimension(dum_p,n_i,n_j,n_k)::dum_tracer                     ! 
!!$    real,intent(inout),dimension(dum_p,n_i,n_j,n_k)::dum_dtracer                 ! 
!!$    ! LOCAL VARIABLES   
!!$    integer::i,j                                                                 ! 
!!$    integer::loc_i,loc_j
!!$
!!$    ! initialize tracer anomoly?????????????????
!!$    
!!$    ! transport tracer form origination to destination cell
!!$    ! loop through locations of tracer *origination* cells
!!$    DO i=1,n_i
!!$       DO j=1,n_j
!!$          do n=1,par_n_color_ftrans_max
!!$             ! set location of cell tracers are being transported to (*destination* cells)
!!$             loc_i =  color_ftrans(n,i,j,1)
!!$             loc_j =  color_ftrans(n,i,j,2)
!!$             dum_dtracer(:,loc_i,loc_j) =  dum_dtracer(:,loc_i,loc_j) + color_ftrans(n,i,j,3)*dum_tracer(:,i,j)
!!$             dum_dtracer(:,i,j,) =  dum_dtracer(:,i,j,) - color_ftrans(n,i,j,3)*dum_tracer(:,i,j)
!!$          end do
!!$       end do
!!$    end do
!!$
!!$  end subroutine calc_applycoltrans
!!$  ! ****************************************************************************************************************************** !


END MODULE goldlite_box
