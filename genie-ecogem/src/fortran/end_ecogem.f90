
! ******************************************************************************************************************************** !
! END GEMlite
SUBROUTINE end_ecogem()
  USE ecogem_lib
  use ecogem_data_netCDF
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  integer::loc_iou 
  integer        :: ii,io,jp
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  print*,'======================================================='
  print*,' >>> Initialising ECOGEM module shutdown ...'


  goto 101
  ! ---------------------------------------------------------- !
  ! SAVE DATA: 2-D netCDF
  ! ---------------------------------------------------------- !
  if (ctrl_debug_end > 0) print*,'WRITE 2D netCDF OUTPUT'
  call sub_update_netcdf(const_real_zero,2)
  call sub_save_netcdf_2d()
  call sub_closefile(ncout2d_iou)
  ncout2d_ntrec = ncout2d_ntrec + 1
  ! ---------------------------------------------------------- !
  ! SAVE DATA: 3-D netCDF
  ! ---------------------------------------------------------- !
  if (ctrl_debug_end > 0) print*,'WRITE 3D netCDF OUTPUT'
  call sub_update_netcdf(const_real_zero,3)
  call sub_save_netcdf_3d()
  call sub_closefile(ncout3d_iou)
  ncout3d_ntrec = ncout3d_ntrec + 1
101 continue


  ! ---------------------------------------------------------- !
  ! CLOSE ASCII FILES !---------------------------------------- !
  ! ---------------------------------------------------------- !
  !  ! close nutrient files
  !  do ii=1,iimax
  !    close(100+ii)
  !  enddo
  !  ! close biomass files
  !  do io=1,iomax+iChl
  !    do jp=1,npmax
  !      close(1000*io+jp)
  !    enddo
  !  enddo
  !  close(10000)
  ! -------------------------------------------------------- !
  ! END
  ! -------------------------------------------------------- !

  print*,' <<< Shutdown complete'
  print*,'======================================================='
END SUBROUTINE end_ecogem
! ******************************************************************************************************************************** !

