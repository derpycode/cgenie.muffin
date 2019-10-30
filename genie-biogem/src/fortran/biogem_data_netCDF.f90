! ******************************************************************************************************************************** !
! biogem_data_netCDF.f90
! BioGEochemical Model
! DATA LOADING/SAVING ROUTINES
! ******************************************************************************************************************************** !


MODULE biogem_data_netCDF


  USE gem_netcdf
  USE biogem_lib
  USE biogem_box
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! SAVE NETCDF RESTART DATA
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_netCDF_ncrstsave(dum_name,dum_yr,dum_iou)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    character(LEN=*),INTENT(IN)::dum_name                      !
    REAL,INTENT(in)::dum_yr                                    !
    INTEGER,INTENT(OUT)::dum_iou                               !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::io,is,l
    integer::loc_ntrec,loc_iou
    integer::loc_id_lonm,loc_id_latm,loc_id_lon_e,loc_id_lat_e
    integer::loc_id_zt,loc_id_zt_e
    integer,dimension(1:2)::loc_it_1
    integer,dimension(1:3)::loc_it_2
    integer,dimension(1:4)::loc_it_3
    character(127)::loc_title,loc_timunit
    character(7)::loc_string_year
    real::loc_c0,loc_c1
    real,dimension(0:n_i)::loc_lon_e
    real,dimension(0:n_j)::loc_lat_e
    real,dimension(0:n_k)::loc_zt_e
    real,dimension(n_i,n_j,n_k)::loc_ijk,loc_ijk_mask
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_c0 = 0.0
    loc_c1 = 1.0
    ! -------------------------------------------------------- !
    ! WRITE TO FILE
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! open file
    call sub_opennew(dum_name,loc_iou)
    ! -------------------------------------------------------- ! start definitions
    call sub_redef(loc_iou)
    ! -------------------------------------------------------- ! set global attributes
    loc_string_year = fun_conv_num_char_n(8,int(dum_yr))
    loc_title = 'BIOGEM restart @ year '//loc_string_year
    call sub_putglobal(loc_iou,dum_name,loc_title,string_ncrunid,loc_timunit)
    ! -------------------------------------------------------- ! define dimensions
    call sub_defdim ('lon',loc_iou,n_i,loc_id_lonm)
    call sub_defdim ('lat',loc_iou,n_j,loc_id_latm)
    call sub_defdim ('lon_edges',loc_iou,n_i+1,loc_id_lon_e)
    call sub_defdim ('lat_edges',loc_iou,n_j+1,loc_id_lat_e)
    call sub_defdim ('zt',loc_iou, n_k,loc_id_zt)
    call sub_defdim ('zt_edges',loc_iou,n_k+1,loc_id_zt_e)
    ! -------------------------------------------------------- ! define 1d data (t)
    loc_it_1(1) = loc_id_lonm
    call sub_defvar ('lon',loc_iou,1,loc_it_1,loc_c0,loc_c0,'X','D','longitude of the t grid','longitude','degrees_east')
    loc_it_1(1) = loc_id_latm
    call sub_defvar ('lat',loc_iou,1,loc_it_1,loc_c0,loc_c0,'Y','D','latitude of the t grid','latitude','degrees_north')
    loc_it_1(1) = loc_id_lon_e
    call sub_defvar ('lon_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D','longitude of t grid edges',' ','degrees')
    loc_it_1(1) = loc_id_lat_e
    call sub_defvar ('lat_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D','latitude of t grid edges',' ','degrees')
    loc_it_1(1) = loc_id_zt
    call sub_defvar ('zt',loc_iou,1,loc_it_1,loc_c0,loc_c0,'Z','D','depth of z grid',' ','cm')
    loc_it_1(1) = loc_id_zt_e
    call sub_defvar ('zt_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D','depth of z grid edges',' ','m')
    loc_it_2(1) = loc_id_lonm
    loc_it_2(2) = loc_id_latm
    loc_it_3(1) = loc_id_lonm
    loc_it_3(2) = loc_id_latm
    loc_it_3(3) = loc_id_zt
    ! -------------------------------------------------------- ! define (3D) tracer variables -- dissolved
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       call sub_defvar('ocn_'//trim(string_ocn(io)),loc_iou,3,loc_it_3,loc_c0,loc_c0,' ','F', &
            & string_longname_ocn(io),'Ocean tracer - '//trim(string_ocn(io)),' ')
    end do
    ! -------------------------------------------------------- ! define (3D) tracer variables -- particulate
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       call sub_defvar('bio_part_'//trim(string_sed(is)),loc_iou,3,loc_it_3,loc_c0,loc_c0,' ','F', &
            & string_longname_sed(is),'Particulate tracer - '//trim(string_sed(is)),' ')
    end do
    ! -------------------------------------------------------- ! end definitions
    call sub_enddef (loc_iou)
    call sub_sync(loc_iou)
    ! -------------------------------------------------------- !
    loc_ntrec = 1
    ! -------------------------------------------------------- ! write 1D variables
    call sub_putvar1d ('lon',loc_iou,n_i,loc_ntrec,n_i,phys_ocn(ipo_lon,:,1,n_k),loc_c1,loc_c0)
    call edge_maker (1,loc_lon_e,phys_ocn(ipo_lon,:,1,n_k),phys_ocn(ipo_lone,:,1,n_k),phys_ocn(ipo_dlon,:,1,n_k),n_i)
    call sub_putvar1d ('lon_edges',loc_iou,n_i+1,loc_ntrec,n_i+1,loc_lon_e,loc_c1,loc_c0)
    call sub_putvar1d ('lat',loc_iou,n_j,loc_ntrec,n_j,phys_ocn(ipo_lat,1,:,n_k),loc_c1,loc_c0)
    call edge_maker (1,loc_lat_e,phys_ocn(ipo_lat,1,:,n_k),phys_ocn(ipo_latn,1,:,n_k),phys_ocn(ipo_dlat,1,:,n_k),n_j)
    call sub_putvar1d ('lat_edges',loc_iou, n_j+1,loc_ntrec,n_j+1,loc_lat_e,loc_c1,loc_c0)
    call sub_putvar1d ('zt',loc_iou,n_k,loc_ntrec,n_k,phys_ocn(ipo_Dmid,1,1,n_k:1:-1),loc_c1,loc_c0)
    call edge_maker (1,loc_zt_e,phys_ocn(ipo_Dmid,1,1,n_k:1:-1),phys_ocn(ipo_Dbot,1,1,n_k:1:-1), &
         & phys_ocn(ipo_dD,1,1,n_k:1:-1),n_k)
    loc_zt_e(0)=0.0
    call sub_putvar1d ('zt_edges',loc_iou,n_k+1,loc_ntrec,n_k+1,loc_zt_e,loc_c1,loc_c0)
    ! -------------------------------------------------------- ! write (3D) tracer variables -- dissolved
    loc_ijk_mask(:,:,:) = phys_ocn(ipo_mask_ocn,:,:,:)
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       loc_ijk(:,:,:) = ocn(io,:,:,:)
       call sub_putvar3d('ocn_'//trim(string_ocn(io)),loc_iou,n_i,n_j,n_k,loc_ntrec, &
            & loc_ijk(:,:,n_k:1:-1),loc_ijk_mask(:,:,n_k:1:-1))
    end do
    ! -------------------------------------------------------- ! write (3D) tracer variables -- particulate
    loc_ijk_mask(:,:,:) = phys_ocn(ipo_mask_ocn,:,:,:)
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ijk(:,:,:) = bio_part(is,:,:,:)
       call sub_putvar3d('bio_part_'//trim(string_sed(is)),loc_iou,n_i,n_j,n_k,loc_ntrec, &
            & loc_ijk(:,:,n_k:1:-1),loc_ijk_mask(:,:,n_k:1:-1))
    end do
    ! -------------------------------------------------------- ! close file and return IOU
    call sub_closefile(loc_iou)
    dum_iou = loc_iou
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_netCDF_ncrstsave
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE netCDF
  SUBROUTINE sub_init_netcdf(dum_name,dum_iou,dum_dd)
    !-----------------------------------------------------------------------
    !       dummy arguments
    !-----------------------------------------------------------------------
    character(LEN=*),INTENT(IN) :: dum_name
    INTEGER,INTENT(OUT):: dum_iou
    INTEGER,INTENT(IN)::dum_dd
    !-----------------------------------------------------------------------
    !       define local variables
    !-----------------------------------------------------------------------
    character(255) :: loc_title, loc_timunit
    real           :: loc_c0, loc_c1
    integer        :: loc_it(6), loc_id_time, loc_id_lonm, loc_id_latp, loc_id_ztp
    integer        :: loc_id_lonps, loc_id_latps
    integer        :: loc_id_latm, loc_id_zt, loc_id_lon_e, loc_id_xu, loc_id_yu
    integer        :: loc_id_lat_e, loc_id_zt_e, loc_id_xu_e, loc_id_yu_e
    integer        :: loc_id_misc
    integer::loc_id_latp_e,loc_id_ztp_e
    integer::loc_id_lonps_e,loc_id_latps_e
    !-----------------------------------------------------------------------
    !       initialize local variables
    !-----------------------------------------------------------------------
    loc_c0 = 0.
    loc_c1 = 1.
    !-----------------------------------------------------------------------
    !       open file
    !-----------------------------------------------------------------------
    call sub_opennew (dum_name, dum_iou)
    !-----------------------------------------------------------------------
    !       start definitions
    !-----------------------------------------------------------------------
    call sub_redef (dum_iou)
    !-----------------------------------------------------------------------
    !       set global attributes
    !-----------------------------------------------------------------------
    loc_title = 'Time averaged integrals'
    write (loc_timunit,'(a)') 'Year mid-point'
    call sub_putglobal (dum_iou, dum_name, loc_title, string_ncrunid, loc_timunit)
    !-----------------------------------------------------------------------
    !       define dimensions
    !-----------------------------------------------------------------------
    call sub_defdim ('time', dum_iou, const_integer_zero, loc_id_time)
    call sub_defdim ('xu', dum_iou, n_i, loc_id_xu)
    call sub_defdim ('lon', dum_iou, n_i, loc_id_lonm)
    call sub_defdim ('lat', dum_iou, n_j, loc_id_latm)
    call sub_defdim ('zt', dum_iou, n_k, loc_id_zt)
    call sub_defdim ('yu', dum_iou, n_j, loc_id_yu)
    call sub_defdim ('lon_edges', dum_iou, n_i+1, loc_id_lon_e)
    call sub_defdim ('lat_edges', dum_iou, n_j+1, loc_id_lat_e)
    call sub_defdim ('zt_edges', dum_iou, n_k+1, loc_id_zt_e)
    call sub_defdim ('xu_edges', dum_iou, n_i+1, loc_id_xu_e)
    call sub_defdim ('yu_edges', dum_iou, n_j+1, loc_id_yu_e)
    call sub_defdim ('lat_moc', dum_iou, n_j+1, loc_id_latp)
    call sub_defdim ('zt_moc', dum_iou, n_k+1, loc_id_ztp)
    call sub_defdim ('lat_moc_edges', dum_iou, n_j+2, loc_id_latp_e)
    call sub_defdim ('zt_moc_edges', dum_iou, n_k+2, loc_id_ztp_e)
    call sub_defdim ('lon_psi', dum_iou, n_i, loc_id_lonps)
    call sub_defdim ('lat_psi', dum_iou, n_j+1, loc_id_latps)
    call sub_defdim ('lon_psi_edges', dum_iou, n_i+1, loc_id_lonps_e)
    call sub_defdim ('lat_psi_edges', dum_iou, n_j+2, loc_id_latps_e)
    call sub_defdim ('para', dum_iou, const_integer_one, loc_id_misc)
    !-----------------------------------------------------------------------
    !       define 1d data (t)
    !-----------------------------------------------------------------------
    loc_it(1) = loc_id_time
    call sub_defvar ('time', dum_iou, 1, loc_it, loc_c0, loc_c0, 'T', 'D' &
         &, 'Year', 'time', trim(loc_timunit))
    call sub_defvar ('year', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','year', ' ',' ')
    !-----------------------------------------------------------------------
    !       define 1d data (x, y or z)
    !-----------------------------------------------------------------------
    loc_it(1) = loc_id_lonm
    call sub_defvar ('lon', dum_iou, 1, loc_it, loc_c0, loc_c0, 'X', 'D' , &
         &'longitude of the t grid', 'longitude', 'degrees_east')
    loc_it(1) = loc_id_latm
    call sub_defvar ('lat', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
         &'latitude of the t grid', 'latitude', 'degrees_north')
    loc_it(1) = loc_id_zt
    call sub_defvar ('zt', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Z', 'D' , &
         &'z-level mid depth', 'depth', 'm')
    loc_it(1) = loc_id_xu
    call sub_defvar ('xu', dum_iou, 1, loc_it, loc_c0, loc_c0, 'X', 'D' , &
         &'longitude of the u grid', 'longitude', 'degrees_east')
    loc_it(1) = loc_id_yu
    call sub_defvar ('yu', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
         &'latitude of the u grid', 'latitude', 'degrees_north')
    loc_it(1) = loc_id_lon_e
    call sub_defvar ('lon_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
         &'longitude of t grid edges', ' ', 'degrees')
    loc_it(1) = loc_id_lat_e
    call sub_defvar ('lat_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
         &'latitude of t grid edges', ' ', 'degrees')
    loc_it(1) = loc_id_zt_e
    call sub_defvar ('zt_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
         &'depth of t grid edges', ' ', 'm')
    loc_it(1) = loc_id_xu_e
    call sub_defvar ('xu_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
         &'longitude of u grid edges', ' ', 'degrees')
    loc_it(1) = loc_id_yu_e
    call sub_defvar ('yu_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
         &'latitude of u grid edges', ' ', 'degrees')
    SELECT CASE (dum_dd)
    CASE (2)
       ! MOC
       loc_it(1) = loc_id_latp
       call sub_defvar ('lat_moc', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
            &'latitude of moc grid', 'latitude', 'degrees_north')
       loc_it(1) = loc_id_ztp
       call sub_defvar ('zt_moc', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Z', 'D' , &
            &'depth of moc grid', 'depth', 'm')
       call sub_putatttext ('zt_moc', dum_iou, 'positive', 'down')
       loc_it(1) = loc_id_latp_e
       call sub_defvar ('lat_moc_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
            &'latitude of moc grid edges', 'latitude', 'degrees_north')
       loc_it(1) = loc_id_ztp_e
       call sub_defvar ('zt_moc_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Z', 'D' , &
            &'depth of moc grid edges', 'depth', 'm')
       ! PSI
       loc_it(1) = loc_id_lonps
       call sub_defvar ('lon_psi', dum_iou, 1, loc_it, loc_c0, loc_c0, 'X', 'D' , &
            &'longitude of psi grid', 'longitude', 'degrees_east')
       loc_it(1) = loc_id_latps
       call sub_defvar ('lat_psi', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
            &'latitude of psi grid', 'latitude', 'degrees_north')
       loc_it(1) = loc_id_lonps_e
       call sub_defvar ('lon_psi_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, 'X', 'D' , &
            &'longitude of psi grid edges', 'longitude', 'degrees_east')
       loc_it(1) = loc_id_latps_e
       call sub_defvar ('lat_psi_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
            &'latitude of psi grid edges', 'latitude', 'degrees_north')
    end select
    !-----------------------------------------------------------------------
    !       define basic 2d/3d data (x,y)
    !-----------------------------------------------------------------------
    SELECT CASE (dum_dd)
    CASE (2)
       loc_it(1) = loc_id_lonm
       loc_it(2) = loc_id_latm
       call sub_defvar('grid_level',dum_iou,2,loc_it,loc_c0,100.0,' ','I', &
            & 'grid definition','model_level_number','n/a')
       call sub_defvar ('grid_mask', dum_iou, 2, loc_it, loc_c0, 100.0, ' ', 'F', &
            &'land-sea mask', ' ' ,'n/a')
       call sub_defvar ('grid_topo', dum_iou, 2, loc_it, loc_c0, 100000., ' ', 'F', &
            &'ocean depth ', ' ' ,'m')
       call sub_defvar ('grid_area', dum_iou, 2, loc_it, loc_c0, 0.5099044E+15, ' ', 'F', &
            &'grid area ', ' ' ,'m2')
    end select
    SELECT CASE (dum_dd)
    CASE (3,4)
       loc_it(1) = loc_id_lonm
       loc_it(2) = loc_id_latm
       loc_it(3) = loc_id_zt
       call sub_defvar('grid_level',dum_iou,2,loc_it,loc_c0,100.0,' ','I', &
            & 'grid definition','model_level_number','n/a')
       call sub_defvar ('grid_mask', dum_iou, 2, loc_it, loc_c0, 100.0, ' ', 'F', &
            &'land-sea mask', ' ' ,'n/a')
       call sub_defvar ('grid_topo', dum_iou, 2, loc_it, loc_c0, 100000., ' ', 'F', &
            &'ocean depth ', ' ' ,'m')
       call sub_defvar ('grid_area', dum_iou, 2, loc_it, loc_c0, 0.5099044E+15, ' ', 'F', &
            &'grid area ', ' ' ,'m2')
       call sub_defvar('grid_mask_3d',dum_iou,3,loc_it,loc_c0, 1.,' ','F', &
            & 'ocean mask',' ','n/a')
    end select
    !-----------------------------------------------------------------------
    call sub_enddef (dum_iou)
    call sub_closefile (dum_iou)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_init_netcdf
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_init_netcdf_TM(dum_wet_grid)
    !-----------------------------------------------------------------------
    !       dummy arguments
    !-----------------------------------------------------------------------
    integer::dum_wet_grid
    !-----------------------------------------------------------------------
    !       define local variables
    !-----------------------------------------------------------------------
    integer::ncid,status
    integer::coo_dimid,index_dimid,var_id

    ! open file
    status=nf90_create(TRIM(par_outdir_name)//'transport_matrix_COO.nc',nf90_clobber,ncid)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    ! dimensions
    status=nf90_def_dim(ncid,'coo',nf90_unlimited,coo_dimid)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    status=nf90_def_dim(ncid,'index',dum_wet_grid,index_dimid)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    ! define variables
    status=nf90_def_var(ncid,'coo_val',nf90_double,(/coo_dimid/),var_id)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    status=nf90_def_var(ncid,'coo_row',nf90_int,(/coo_dimid/),var_id)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    status=nf90_def_var(ncid,'coo_col',nf90_int,(/coo_dimid/),var_id)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    status=nf90_def_var(ncid,'coo_avg_n',nf90_byte,(/coo_dimid/),var_id)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    status=nf90_def_var(ncid,'index_i',nf90_int,(/index_dimid/),var_id)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    status=nf90_def_var(ncid,'index_j',nf90_int,(/index_dimid/),var_id)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    status=nf90_def_var(ncid,'index_k',nf90_int,(/index_dimid/),var_id)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    !end definition
    status=nf90_enddef(ncid)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    ! close file
    status=nf90_close(ncid)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

  END SUBROUTINE sub_init_netcdf_TM
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_save_netcdf_TM(dum_TM_flag,dum_start,dum_val,dum_col,dum_row,dum_avg_n,dum_i,dum_j,dum_k)

    integer::dum_start
    real::dum_val
    integer::dum_TM_flag
    integer::dum_col,dum_row,dum_avg_n,dum_i,dum_j,dum_k

    integer::ncid,status,nc_record_count,dimid
    character(len=100)::name

    ! open file
    status=nf90_open(TRIM(par_outdir_name)//'transport_matrix_COO.nc',nf90_write,ncid)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

    select case (dum_TM_flag)
    case(1) ! write TM data

      ! coo is unlimited dimension that we have to append to
      ! find the current length of coo:
      status=nf90_inq_dimid(ncid, 'coo', dimid)
      if(status /= nf90_NoErr) print*,trim(nf90_strerror(status)//',coo')

      status=nf90_inquire_dimension(ncid, dimid, name, nc_record_count)
      if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

      nc_record_count=nc_record_count+1 ! start count for appending data

      call sub_putvars ('coo_val', ncid, nc_record_count, dum_val,1.0,0.0)

      call sub_putvarIs ('coo_col', ncid, nc_record_count, dum_col,1.0,0.0)

      call sub_putvarIs ('coo_row', ncid, nc_record_count, dum_row,1.0,0.0)

      call sub_putvarIs ('coo_avg_n', ncid, nc_record_count, dum_avg_n,1.0,0.0)

    case(0) ! write index data

      call sub_putvarIs ('index_i', ncid, dum_start, dum_i,1.0,0.0)

      call sub_putvarIs ('index_j', ncid, dum_start, dum_j,1.0,0.0)

      call sub_putvarIs ('index_k', ncid, dum_start, dum_k,1.0,0.0)
    end select

    ! close file
    status=nf90_close(ncid)
    if(status /= nf90_NoErr) print*,trim(nf90_strerror(status))

  END SUBROUTINE sub_save_netcdf_TM
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_save_netcdf(dum_yr,dum_dd)
    !-----------------------------------------------------------------------
    !       dummy arguments
    !-----------------------------------------------------------------------
    INTEGER,INTENT(IN)::dum_dd
    REAL,INTENT(in):: dum_yr
    !-----------------------------------------------------------------------
    !       local variables
    !-----------------------------------------------------------------------
    character(255) :: loc_name
    real           :: loc_c0, loc_c1
    integer        :: i, j, k, loc_i, loc_iou, loc_ntrec
    real,dimension(n_i,n_j) :: loc_mask_ALL,loc_data
    real,dimension(n_i+1) :: loc_lon_e, loc_xu_e
    real,dimension(n_j+1) :: loc_lat_e, loc_yu_e
    real,dimension(0:n_k) :: loc_zt_e, loc_help
    REAL,DIMENSION(0:n_k+1)::loc_grid_dz, loc_tmp_k
    real,dimension(0:n_j+1)::loc_lat_moc_e
    REAL,DIMENSION(0:n_k+1)::loc_zt_moc_e
    logical :: loc_defined
    !-----------------------------------------------------------------------
    !       initialize local variables
    !-----------------------------------------------------------------------
    loc_c0 = 0.
    loc_c1 = 1.
    !
    loc_mask_ALL(:,:)  = 1.0
    loc_data(:,:)      = 0.0
    loc_lon_e(:)       = 0.0
    loc_xu_e(:)        = 0.0
    loc_lat_e(:)       = 0.0
    loc_yu_e(:)        = 0.0
    loc_zt_e(:)        = 0.0
    loc_help(:)        = 0.0
    loc_grid_dz(:)     = 0.0
    loc_tmp_k(:)       = 0.0
    loc_lat_moc_e(:)   = 0.0
    loc_zt_moc_e(:)    = 0.0
    ! set local netCDF variables
    SELECT CASE (dum_dd)
    CASE (-1)
       loc_name = string_ncrst
       loc_iou = ncrst_iou
       loc_ntrec = ncrst_ntrec
    CASE (2)
       loc_name = string_ncout2d
       loc_iou = ncout2d_iou
       loc_ntrec = ncout2d_ntrec
    CASE (3)
       loc_name = string_ncout3d
       loc_iou = ncout3d_iou
       loc_ntrec = ncout3d_ntrec
    CASE (4)
       loc_name = string_ncout3dsig
       loc_iou = ncout3dsig_iou
       loc_ntrec = ncout3dsig_ntrec
    CASE DEFAULT
       CALL sub_report_error( &
            & 'biogem_data_netCDF','sub_save_netcdf', &
            & 'illegal netCDF dimension', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    end select
    ! open file and get latest record number
    loc_defined = .true.
    loc_i = 0
    if (loc_ntrec .eq. 0) then
       loc_defined = .false.
       loc_i = 1
    end if
    call sub_opennext (loc_name, dum_yr, loc_i, loc_ntrec, loc_iou)
    ! write time data
    call sub_putvars  ('time', loc_iou, loc_ntrec, dum_yr, loc_c1, loc_c0)
    call sub_putvarIs  ('year', loc_iou, loc_ntrec, nint(dum_yr), loc_c1, loc_c0)
    if(.not. loc_defined) then
       ! write 1d data: x
       call sub_putvar1d ('lon', loc_iou, n_i, loc_ntrec, n_i, &
            & phys_ocn(ipo_lon,:,1,1), loc_c1, loc_c0)
       call edge_maker (1, loc_lon_e, phys_ocn(ipo_lon,:,1,1), &
            & phys_ocn(ipo_lone,:,1,1), phys_ocn(ipo_dlon,:,1,1), n_i)
       call sub_putvar1d ('lon_edges', loc_iou, n_i+1, loc_ntrec, n_i+1, &
            & loc_lon_e, loc_c1, loc_c0)
       call sub_putvar1d ('xu', loc_iou, n_i, loc_ntrec, n_i, &
            & loc_lon_e(1:n_i), loc_c1, loc_c0)
       call edge_maker (2, loc_xu_e, phys_ocn(ipo_lon,:,1,1), &
            & phys_ocn(ipo_lone,:,1,1), phys_ocn(ipo_dlon,:,1,1), n_i)
       call sub_putvar1d ('xu_edges', loc_iou, n_i+1, loc_ntrec, n_i+1, &
            & loc_xu_e, loc_c1, loc_c0)
       ! write 1d data: y
       call sub_putvar1d ('lat', loc_iou, n_j, loc_ntrec, n_j, &
            & phys_ocn(ipo_lat,1,:,1), loc_c1, loc_c0)
       call edge_maker (1, loc_lat_e, phys_ocn(ipo_lat,1,:,1), &
            & phys_ocn(ipo_latn,1,:,1), phys_ocn(ipo_dlat,1,:,1), n_j)
       call sub_putvar1d ('lat_edges', loc_iou, n_j+1, loc_ntrec, n_j+1, &
            & loc_lat_e, loc_c1, loc_c0)
       call sub_putvar1d ('yu', loc_iou, n_j, loc_ntrec, n_j, &
            & loc_lat_e(1:n_j), loc_c1, loc_c0)
       call edge_maker (2, loc_yu_e, phys_ocn(ipo_lat,1,:,1), &
            & phys_ocn(ipo_latn,1,:,1), phys_ocn(ipo_dlat,1,:,1), n_j)
       call sub_putvar1d ('yu_edges', loc_iou, n_j+1, loc_ntrec, n_j+1, &
            & loc_yu_e, loc_c1, loc_c0)
       ! write 1d data: z
       call sub_putvar1d ('zt', loc_iou, n_k, loc_ntrec, n_k, &
            & phys_ocn(ipo_Dmid,1,1,n_k:1:-1), loc_c1, loc_c0)
       loc_help(1:n_k) = phys_ocn(ipo_dD,1,1,n_k:1:-1)
       call edge_maker (1, loc_zt_e, phys_ocn(ipo_Dmid,1,1,n_k:1:-1), &
            & phys_ocn(ipo_Dbot,1,1,n_k:1:-1), loc_help , n_k)
       loc_zt_e(0)=0.0
       call sub_putvar1d ('zt_edges', loc_iou, n_k+1, loc_ntrec, n_k+1, &
            & loc_zt_e, loc_c1, loc_c0)
       !
       SELECT CASE (dum_dd)
       CASE (2)
          ! MOC
          call sub_putvar1d ('lat_moc', loc_iou, n_j+1, loc_ntrec, n_j+1, &
               & (180.0/const_pi) * ASIN(goldstein_sv(:)), loc_c1, loc_c0)
          loc_grid_dz(1:n_k) = goldstein_dz(:)
          DO k=n_k,0,-1
             loc_tmp_k(n_k-k) = SUM(goldstein_dsc * loc_grid_dz(k+1:n_k+1))
          ENDDO
          call sub_putvar1d ('zt_moc', loc_iou, n_k+1, loc_ntrec, n_k+1, &
               & loc_tmp_k, loc_c1, loc_c0)
          loc_lat_moc_e(0) = phys_ocn(ipo_lat,1,1,1) - (phys_ocn(ipo_latn,1,1,1) + 90.0)
          loc_lat_moc_e(1:n_j) = phys_ocn(ipo_lat,1,1:n_j,1)
          loc_lat_moc_e(n_j+1) = phys_ocn(ipo_lat,1,n_j,1) + (phys_ocn(ipo_latn,1,n_j,1) - phys_ocn(ipo_latn,1,n_j-1,1))
          call sub_putvar1d ('lat_moc_edges', loc_iou, n_j+2, loc_ntrec, n_j+2, &
               & loc_lat_moc_e(:), loc_c1, loc_c0)
          loc_zt_moc_e(0) = 0.0
          loc_zt_moc_e(1:n_k) = phys_ocn(ipo_Dmid,1,1,n_k:1:-1)
          loc_zt_moc_e(n_k+1) = goldstein_dsc
          call sub_putvar1d ('zt_moc_edges', loc_iou, n_k+2, loc_ntrec, n_k+2, &
               & loc_zt_moc_e(:), loc_c1, loc_c0)
          ! PSI
          call sub_putvar1d('lon_psi',loc_iou,n_i,loc_ntrec,n_i,loc_lon_e(2:n_i+1),loc_c1,loc_c0)
          call sub_putvar1d('lat_psi',loc_iou,n_j+1,loc_ntrec,n_j+1,loc_lat_e(1:n_j+1),loc_c1,loc_c0)
          call sub_putvar1d('lon_psi_edges',loc_iou,n_i+1,loc_ntrec,n_i+1,loc_xu_e(:),loc_c1,loc_c0)
          call sub_putvar1d('lat_psi_edges',loc_iou,n_j+2,loc_ntrec,n_j+2,loc_lat_moc_e(:),loc_c1,loc_c0)
       end select
       ! set maximum ocean depth
       do i=1,n_i
          do j=1,n_j
             if(phys_ocn(ipo_mask_ocn,i,j,n_k) == 1.0) then
                loc_data(i,j) = phys_ocn(ipo_Dbot,i,j,goldstein_k1(i,j))
             else
                loc_data(i,j) = 0.0
             end if
          end do
       end do
       ! write 2D grid data
       SELECT CASE (dum_dd)
       CASE (2)
          call sub_putvar2dI ('grid_level', loc_iou, n_i, n_j, loc_ntrec, &
               & goldstein_k1(:,:))
          call sub_putvar2d ('grid_mask', loc_iou, n_i, n_j, loc_ntrec, &
               & phys_ocn(ipo_mask_ocn,:,:,n_k), phys_ocn(ipo_mask_ocn,:,:,n_k))
          call sub_putvar2d ('grid_topo', loc_iou, n_i, n_j, loc_ntrec, &
               & loc_data(:,:), phys_ocn(ipo_mask_ocn,:,:,n_k))
          call sub_putvar2d ('grid_area', loc_iou, n_i, n_j, loc_ntrec, &
               & phys_ocnatm(ipoa_A,:,:), loc_mask_ALL)
       end select
       ! write 3D grid data
       SELECT CASE (dum_dd)
       CASE (3,4)
          call sub_putvar2dI ('grid_level', loc_iou, n_i, n_j, loc_ntrec, &
               & goldstein_k1(:,:))
          call sub_putvar2d ('grid_mask', loc_iou, n_i, n_j, loc_ntrec, &
               & phys_ocn(ipo_mask_ocn,:,:,n_k), phys_ocn(ipo_mask_ocn,:,:,n_k))
          call sub_putvar2d ('grid_topo', loc_iou, n_i, n_j, loc_ntrec, &
               & loc_data(:,:), phys_ocn(ipo_mask_ocn,:,:,n_k))
          call sub_putvar2d ('grid_area', loc_iou, n_i, n_j, loc_ntrec, &
               & phys_ocn(ipo_A,:,:,n_k), phys_ocn(ipo_mask_ocn,:,:,n_k))
          call sub_putvar3d('grid_mask_3d',loc_iou,n_i,n_j,n_k,loc_ntrec, &
               & phys_ocn(ipo_mask_ocn,:,:,n_k:1:-1),phys_ocn(ipo_mask_ocn,:,:,n_k:1:-1))
       end select
    end if
    !-----------------------------------------------------------------------
    ! update record number
    SELECT CASE (dum_dd)
    CASE (-1)
       ncrst_ntrec = loc_ntrec
    CASE (2)
       ncout2d_ntrec = loc_ntrec
    CASE (3)
       ncout3d_ntrec = loc_ntrec
    CASE (4)
       ncout3dsig_ntrec = loc_ntrec
    end select
    !
    call sub_sync(loc_iou)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! MISC 3-D FIELDS
  ! NOTE: called from sub_save_netcdf_3d
  ! NOTE: fields will be saved if the 'miscellaneous' time-slice save option is selected (biogem_config.par)
  ! NOTE: calls to sub_adddef_netcdf(dum_ncid,dum_dino,dum_tname,dum_tlname,dum_unit,dum_min,dum_max)
  !       consist of the following passed parameters:
  !       dum_ncid   = unit
  !       dum_dino   = number dimensions of data (4 in this case - 2 for lat/lon, one for depth, and one of time)
  !       dum_tname  = name of variable to be defined
  !       dum_tlname = long name
  !       dum_unit   = data units
  !       dum_min    = minimum range (default real)
  !       dum_max    = maximum range (default real)
  SUBROUTINE sub_save_netcdf_3d_USER()
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    integer::i,j,k,io,id
    INTEGER::loc_iou,loc_ntrec
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk,loc_mask
    CHARACTER(len=255)::loc_name
    CHARACTER(len=255)::loc_unitsname
    real::loc_tot,loc_frac,loc_standard
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou   = ncout3d_iou
    loc_ntrec = ncout3d_ntrec
    loc_mask(:,:,:) = phys_ocn(ipo_mask_ocn,:,:,:)
    !-----------------------------------------------------------------------
    !       pH FIELD
    !-----------------------------------------------------------------------
    if (ctrl_data_save_slice_diag_geochem) then
       loc_unitsname = 'pH units (SWS)'
       IF (opt_select(iopt_select_carbchem)) THEN
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_ijk(i,j,k) = -LOG10(int_carb_timeslice(ic_H,i,j,k)/int_t_timeslice)
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_pH','ocean pH', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_pH',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end if
    end if
    !-----------------------------------------------------------------------
    !       CaCO3:POC 'rain ratio'
    !-----------------------------------------------------------------------
    IF (ctrl_data_save_slice_bio) THEN
       loc_unitsname = 'n/a'
       IF (sed_select(is_CaCO3) .AND. sed_select(is_POC)) THEN
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   if (int_bio_settle_timeslice(is_POC,i,j,k) > const_real_nullsmall) then
                      loc_ijk(i,j,k) = int_bio_settle_timeslice(is_CaCO3,i,j,k)/int_bio_settle_timeslice(is_POC,i,j,k)
                   end if
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_rCaCO3toPOC','CaCO3 to POC rain ratio', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_rCaCO3toPOC',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
    end if
    !-----------------------------------------------------------------------
    !       opal:POC 'rain ratio'
    !-----------------------------------------------------------------------
    IF (ctrl_data_save_slice_bio) THEN
       loc_unitsname = 'n/a'
       IF (sed_select(is_opal) .AND. sed_select(is_POC)) THEN
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   if (int_bio_settle_timeslice(is_POC,i,j,k) > const_real_nullsmall) then
                      loc_ijk(i,j,k) = int_bio_settle_timeslice(is_opal,i,j,k)/int_bio_settle_timeslice(is_POC,i,j,k)
                   end if
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_ropaltoPOC','opal to POC rain ratio', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_ropaltoPOC',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
    end if
    !-----------------------------------------------------------------------
    !       Particulate flux fractions
    !-----------------------------------------------------------------------
    ! NOTE: divied by real(int_t_timeslice_count) because the flux arrays has been integrated (rather than averaged)
    IF (ctrl_data_save_slice_bio) THEN
       loc_unitsname = 'n/a'
       IF (sed_select(is_POC_frac2)) THEN
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_ijk(i,j,k) = int_bio_settle_timeslice(is_POC_frac2,i,j,k)/real(int_t_timeslice_count)
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_frac2_POC','POC fraction #2', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_frac2_POC',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
       IF (sed_select(is_CaCO3_frac2)) THEN
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_ijk(i,j,k) = int_bio_settle_timeslice(is_CaCO3_frac2,i,j,k)/real(int_t_timeslice_count)
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_frac2_CaCO3','CaCO3 fraction #2', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_frac2_CaCO3',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
       IF (sed_select(is_opal_frac2)) THEN
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_ijk(i,j,k) = int_bio_settle_timeslice(is_opal_frac2,i,j,k)/real(int_t_timeslice_count)
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_frac2_opal','opal fraction #2', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_frac2_opal',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
    end if
    !-----------------------------------------------------------------------
    !       Cd trace metal ratios (in sea-water)
    !-----------------------------------------------------------------------
    IF (ctrl_data_save_slice_ocn .AND. ctrl_data_save_slice_diag_proxy) THEN
       IF (ocn_select(io_Cd) .AND. ocn_select(io_Ca)) THEN
          loc_unitsname = 'nmol kg-1 (mmol kg-1)-1'
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   if (int_ocn_timeslice(io_Ca,i,j,k) > const_real_nullsmall) then
                      loc_ijk(i,j,k) = 1.0E6*int_ocn_timeslice(io_Cd,i,j,k)/int_ocn_timeslice(io_Ca,i,j,k)
                   end if
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_rCdtoCa','Cd:Ca trace metal ratio (ocean)', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_rCdtoCa',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
       IF (ocn_select(io_Cd) .AND. ocn_select(io_PO4)) THEN
          loc_unitsname = 'nmol kg-1 (umol kg-1)-1'
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   if (int_ocn_timeslice(io_PO4,i,j,k) > const_real_nullsmall) then
                      loc_ijk(i,j,k) = 1.0E3*int_ocn_timeslice(io_Cd,i,j,k)/int_ocn_timeslice(io_PO4,i,j,k)
                   end if
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_rCdtoPO4','Cd:PO4 trace metal ratio (ocean)', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_rCdtoPO4',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
       IF (ocn_select(io_Cd) .AND. ocn_select(io_PO4) .AND. ocn_select(io_Ca)) THEN
          loc_unitsname = 'umol kg-1 (mmol kg-1)-1'
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   if (int_ocn_timeslice(io_Ca,i,j,k) > const_real_nullsmall) then
                      loc_ijk(i,j,k) = 1.0E3*int_ocn_timeslice(io_PO4,i,j,k)/int_ocn_timeslice(io_Ca,i,j,k)
                   end if
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_rPO4toCa','PO4:Ca ratio (ocean)', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_rPO4toCa',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
    end if
    !-----------------------------------------------------------------------
    !       Fe speciation
    !-----------------------------------------------------------------------
    IF (ctrl_data_save_slice_ocn) THEN
       IF (ocn_select(io_Fe) .AND. ocn_select(io_FeL)) THEN
          loc_unitsname = 'nmol kg-1'
          loc_ijk(:,:,:) = 1.0E9*(int_ocn_timeslice(io_Fe,:,:,:) + int_ocn_timeslice(io_FeL,:,:,:))/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,4,'misc_FeT','Total dissolved iron concentration', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_FeT',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
          loc_ijk(:,:,:) = 1.0E9*(int_ocn_timeslice(io_L,:,:,:) + int_ocn_timeslice(io_FeL,:,:,:))/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,4,'misc_LT','Total Fe-binding ligand concentration', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_LT',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
    end if
    !----------------------------------------------------------------
    !       Fe SPECIATION [complete]
    !----------------------------------------------------------------
    If ( &
         & (ctrl_data_save_slice_ocn .AND. ctrl_data_save_slice_diag_geochem) &
         & .AND. &
         & (ocn_select(io_Fe) .OR. ocn_select(io_TDFe)) &
         & ) then
       DO id=1,n_diag_Fe
          loc_unitsname = 'mol kg-1'
          loc_ijk(:,:,:) = int_diag_Fe_timeslice(id,:,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,4,'diag_Fe_'//trim(string_diag_Fe(id)), &
               & 'water-column Fe speciation - '//trim(string_diag_Fe(id)), &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('diag_Fe_'//trim(string_diag_Fe(id)),loc_iou, &
               & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end DO
    end If
    !!-----------------------------------------------------------------------
    !!       misc 'physics'
    !!-----------------------------------------------------------------------
    !! NOTE: save selected ocean 'physics' as an alternative to having to select to save ALL of the ocean physical properties
    !loc_ijk(:,:,:) = int_phys_ocn_timeslice(ipo_rho,:,:,:)/int_t_timeslice
    !call sub_adddef_netcdf(loc_iou,4,'misc_rho','ocean density','kg m-3',const_real_zero,const_real_zero)
    !call sub_putvar3d_g('misc_rho',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
    !loc_ijk(:,:,:) = int_phys_ocn_timeslice(ipo_A,:,:,:)/int_t_timeslice
    !call sub_adddef_netcdf(loc_iou,4,'grid_A','ocean cell surface area','m2',const_real_zero,const_real_zero)
    !call sub_putvar3d_g('grid_A',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
    !loc_ijk(:,:,:) = int_phys_ocn_timeslice(ipo_dD,:,:,:)/int_t_timeslice
    !call sub_adddef_netcdf(loc_iou,4,'grid_dD','ocean cell thickness','m2',const_real_zero,const_real_zero)
    !call sub_putvar3d_g('grid_dD',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
    !-----------------------------------------------------------------------
    !       ocean carbonate system isotopic properties
    !-----------------------------------------------------------------------
    If (ctrl_data_save_slice_carb) then
       if (ocn_select(io_DIC_13C)) then
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_ijk(i,j,k) = fun_calc_isotope_delta( &
                        & int_carb_timeslice(ic_conc_CO2,i,j,k), &
                        & int_carbisor_timeslice(ici_CO2_r13C,i,j,k)*int_carb_timeslice(ic_conc_CO2,i,j,k), &
                        & const_standards(11), &
                        & .FALSE., &
                        & const_real_null &
                        & )
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'carb_d13C_CO2','d13C of CO2(aq)','o/oo',const_real_zero,const_real_zero)
          call sub_putvar3d_g('carb_d13C_CO2',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_ijk(i,j,k) = fun_calc_isotope_delta( &
                        & int_carb_timeslice(ic_conc_HCO3,i,j,k), &
                        & int_carbisor_timeslice(ici_HCO3_r13C,i,j,k)*int_carb_timeslice(ic_conc_HCO3,i,j,k), &
                        & const_standards(11), &
                        & .FALSE., &
                        & const_real_null &
                        & )
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'carb_d13C_HCO3','d13C of HCO3-','o/oo',const_real_zero,const_real_zero)
          call sub_putvar3d_g('carb_d13C_HCO3',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_ijk(i,j,k) = fun_calc_isotope_delta( &
                        & int_carb_timeslice(ic_conc_CO3,i,j,k), &
                        & int_carbisor_timeslice(ici_CO3_r13C,i,j,k)*int_carb_timeslice(ic_conc_CO3,i,j,k), &
                        & const_standards(11), &
                        & .FALSE., &
                        & const_real_null &
                        & )
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'carb_d13C_CO32','d13C of CO32-','o/oo',const_real_zero,const_real_zero)
          call sub_putvar3d_g('carb_d13C_CO32',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end if
    end If
    If (ctrl_data_save_slice_ocn .AND. ctrl_data_save_slice_diag_tracer) then
       !-----------------------------------------------------------------------
       !       N-star
       !-----------------------------------------------------------------------
       IF (ocn_select(io_PO4) .AND. ocn_select(io_NO3)) THEN
          loc_unitsname = 'umol kg-1'
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_ijk(i,j,k) =                                                           &
                        & 1.0E6*                                                              &
                        & (                                                                   &
                        &   int_ocn_timeslice(io_NO3,i,j,k) -                                 &
                        &   bio_part_red(is_POP,is_PON,i,j)*int_ocn_timeslice(io_PO4,i,j,k) + &
                        &   par_bio_Nstar_offset                                              &
                        & )                                                                   &
                        & /int_t_timeslice
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_Nstar','N-star', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_Nstar',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
       !-----------------------------------------------------------------------
       !       P-star
       !-----------------------------------------------------------------------
       IF (ocn_select(io_PO4) .AND. ocn_select(io_NO3)) THEN
          loc_unitsname = 'umol kg-1'
          loc_ijk(:,:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   if (bio_part_red(is_POP,is_PON,i,j) > const_real_nullsmall) then
                      loc_ijk(i,j,k) =                                                           &
                           & 1.0E6*                                                              &
                           & (                                                                   &
                           &   int_ocn_timeslice(io_PO4,i,j,k) -                                 &
                           &   int_ocn_timeslice(io_NO3,i,j,k)/bio_part_red(is_POP,is_PON,i,j)   &
                           & )                                                                   &
                           & /int_t_timeslice
                   end if
                END DO
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,4,'misc_Pstar','P-star', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('misc_Pstar',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end IF
    end If
    If (ctrl_data_save_slice_ocn) then
       !-----------------------------------------------------------------------
       ! Preformed nutrients and things
       !-----------------------------------------------------------------------
       if (ctrl_bio_preformed) then
             do io=io_col0,io_col9
                if (ocn_select(io)) then
                   loc_ijk(:,:,:) = const_real_null
                   DO i=1,n_i
                      DO j=1,n_j
                         DO k=goldstein_k1(i,j),n_k
                            select case (io)
                            CASE (io_col0:io_col6)
                               loc_ijk(i,j,k) = int_ocn_timeslice(io,i,j,k)/int_t_timeslice
                            CASE (io_col7)
                               loc_tot  = int_ocn_timeslice(io_col0,i,j,k)/int_t_timeslice
                               loc_frac = int_ocn_timeslice(io_col7,i,j,k)/int_t_timeslice
                               loc_standard = const_standards(ocn_type(io_DIC_13C))
                               loc_ijk(i,j,k) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                            case default
                               ! NOTHING DOING
                            end select
                         END DO
                      END DO
                   END DO
                   select case (io)
                   CASE (io_col0:io_col6)
                      loc_unitsname = 'mol kg-1'
                   CASE (io_col7)
                      loc_unitsname = 'o/oo'
                   case default
                      ! NOTHING DOING
                   end select
                   loc_name = 'diag_pre_NULL'
                   select case (io)
                   CASE (io_col0)
                      if (ocn_select(io_DIC)) loc_name = 'diag_pre_'//trim(string_ocn(io_DIC))
                   CASE (io_col1)
                      if (ocn_select(io_ALK)) loc_name = 'diag_pre_'//trim(string_ocn(io_ALK))
                   CASE (io_col2)
                      if (ocn_select(io_O2)) loc_name = 'diag_pre_'//trim(string_ocn(io_O2))
                   CASE (io_col3)
                      if (ocn_select(io_PO4)) loc_name = 'diag_pre_'//trim(string_ocn(io_PO4))
                   CASE (io_col4)
                      if (ocn_select(io_NO3))loc_name = 'diag_pre_'//trim(string_ocn(io_NO3))
                   CASE (io_col5)
                      if (ocn_select(io_Ca)) loc_name = 'diag_pre_'//trim(string_ocn(io_Ca))
                   CASE (io_col6)
                      if (ocn_select(io_SiO2)) loc_name = 'diag_pre_'//trim(string_ocn(io_SiO2))
                   CASE (io_col7)
                      if (ocn_select(io_DIC_13C)) loc_name = 'diag_pre_'//trim(string_ocn(io_DIC_13C))
                   case default
                      ! NOTHING DOING
                   end select
                   call sub_adddef_netcdf(loc_iou,4,trim(loc_name),'Preformed tracer', &
                        & trim(loc_unitsname),const_real_zero,const_real_zero)
                   call sub_putvar3d_g(trim(loc_name),loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
                end if
             end do
       end if
       !
    end If
    ! ### INSERT CODE TO SAVE ADDITIONAL 3-D DATA FIELDS ######################################################################### !
    !
    ! ############################################################################################################################ !
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_3d_USER
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CDR-MIP 3-D FIELDS
  SUBROUTINE sub_save_netcdf_3d_cdrmip()
    !-----------------------------------------------------------------------
    ! DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    integer::i,j,k
    INTEGER::loc_iou,loc_ntrec
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk,loc_mask
    CHARACTER(len=255)::loc_unitsname
    !-----------------------------------------------------------------------
    ! INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou         = ncout3d_iou
    loc_ntrec       = ncout3d_ntrec
    loc_mask(:,:,:) = phys_ocn(ipo_mask_ocn,:,:,:)
    !-----------------------------------------------------------------------
    ! pH
    !-----------------------------------------------------------------------
    loc_unitsname = 'pH units (SWS)'
    loc_ijk(:,:,:) = const_real_zero
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             loc_ijk(i,j,k) = -LOG10(int_carb_timeslice(ic_H,i,j,k)/int_t_timeslice)
          END DO
       END DO
    END DO
    call sub_adddef_netcdf(loc_iou,4,'cdrmip_pH','ocean pH', &
         & trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar3d_g('cdrmip_pH',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_3d_cdrmip
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! MISC 2-D FIELDS
  ! NOTE: called from sub_save_netcdf_2d
  ! NOTE: fields will be saved if the 'miscellaneous' time-slice save option is selected (biogem_config.par)
  ! NOTE: calls to sub_adddef_netcdf(dum_ncid,dum_dino,dum_tname,dum_tlname,dum_unit,dum_min,dum_max)
  !       consist of the following passed parameters:
  !       dum_ncid   = unit
  !       dum_dino   = number dimensions of data (3 in this case - 2 for lat/lon and one of time)
  !       dum_tname  = name of variable to be defined
  !       dum_tlname = long name
  !       dum_unit   = data units
  !       dum_min    = minimum range (default real)
  !       dum_max    = maximum range (default real)
  SUBROUTINE sub_save_netcdf_2d_USER()
    USE genie_util, ONLY: check_unit, check_iostat
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    integer::i,j
    integer::io,is,ib
    integer::l,loc_m,loc_tot_m
    INTEGER::loc_iou,loc_ntrec
    real,DIMENSION(n_i,n_j)::loc_ij,loc_mask_surf
    real,DIMENSION(n_sed,n_i,n_j)::loc_isij
    CHARACTER(len=255)::loc_unitsname
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou   = ncout2d_iou
    loc_ntrec = ncout2d_ntrec
    loc_mask_surf(:,:) = phys_ocnatm(ipoa_mask_ocn,:,:)
    !-----------------------------------------------------------------------
    !       pH FIELD
    !-----------------------------------------------------------------------
    if (ctrl_data_save_slice_diag_geochem) then
       loc_unitsname = 'pH units (SWS)'
       IF (opt_select(iopt_select_carbchem)) THEN
          loc_ij(:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   loc_ij(i,j) = -LOG10(int_carb_timeslice(ic_H,i,j,n_k)/int_t_timeslice)
                END if
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_pH','ocean pH', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_pH',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end if
    end if
    !-----------------------------------------------------------------------
    !       P:C export cellular quotient ratio
    !-----------------------------------------------------------------------
    if (ctrl_data_save_slice_bio .AND. ctrl_data_save_slice_diag_bio) then
       IF (sed_select(is_POP) .AND. sed_select(is_POC)) THEN
          ! P/C
          loc_unitsname = 'n/a'
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   if (int_bio_settle_timeslice(is_POP,i,j,n_k) > const_real_nullsmall) then
                      loc_ij(i,j) = int_bio_settle_timeslice(is_POC,i,j,n_k)/int_bio_settle_timeslice(is_POP,i,j,n_k)
                   end if
                end IF
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_rPOCtoPOP','average POM export C/P cellular ratio', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_rPOCtoPOP',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          ! C/P
          loc_unitsname = 'o/oo'
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   if (int_bio_settle_timeslice(is_POC,i,j,n_k) > const_real_nullsmall) then
                      loc_ij(i,j) = 1.0E3*int_bio_settle_timeslice(is_POP,i,j,n_k)/int_bio_settle_timeslice(is_POC,i,j,n_k)
                   end if
                end IF
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_rPOPtoPOC','average POM export P/C cellular ratio', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_rPOPtoPOC',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end IF
    end if
    !-----------------------------------------------------------------------
    !       CaCO3:POC surface ocean export 'rain ratio'
    !-----------------------------------------------------------------------
    IF (ctrl_data_save_slice_bio) THEN
       loc_unitsname = 'n/a'
       IF (sed_select(is_CaCO3) .AND. sed_select(is_POC)) THEN
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   if (int_bio_settle_timeslice(is_POC,i,j,n_k) > const_real_nullsmall) then
                      loc_ij(i,j) = int_bio_settle_timeslice(is_CaCO3,i,j,n_k)/int_bio_settle_timeslice(is_POC,i,j,n_k)
                   else
                      loc_ij(i,j) = 0.0
                   end if
                end IF
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_rCaCO3toPOC','CaCO3 to POC export rain ratio', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_rCaCO3toPOC',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end if
    end IF
    !-----------------------------------------------------------------------
    !       opal:POC surface ocean export 'rain ratio'
    !-----------------------------------------------------------------------
    IF (ctrl_data_save_slice_bio) THEN
       loc_unitsname = 'n/a'
       IF (sed_select(is_opal) .AND. sed_select(is_POC)) THEN
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   if (int_bio_settle_timeslice(is_POC,i,j,n_k) > const_real_nullsmall) then
                      loc_ij(i,j) = int_bio_settle_timeslice(is_opal,i,j,n_k)/int_bio_settle_timeslice(is_POC,i,j,n_k)
                   else
                      loc_ij(i,j) = 0.0
                   end if
                end IF
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_ropaltoPOC','opal to POC export rain ratio', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_ropaltoPOC',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end if
    end IF
    !-----------------------------------------------------------------------
    !       POC frac2 surface ocean export ratio
    !-----------------------------------------------------------------------
    if (ctrl_data_save_slice_bio .AND. ctrl_data_save_slice_diag_bio) then
       loc_unitsname = 'n/a'
       loc_ij(:,:) = const_real_null
       DO i=1,n_i
          DO j=1,n_j
             IF (n_k >= goldstein_k1(i,j)) THEN
                if (int_bio_settle_timeslice(is_POC,i,j,n_k) > const_real_nullsmall) then
                   loc_ij(i,j) = int_bio_part_timeslice(is_POC_frac2,i,j,n_k)/int_t_timeslice
                end if
             end IF
          END DO
       END DO
       call sub_adddef_netcdf(loc_iou,3,'misc_sur_rPOC2POC','Recalcitrant to labile POC export ratio', &
            & trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('misc_sur_rPOC2POC',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
    end IF
    !-----------------------------------------------------------------------
    !       Cd particulate surface ocean export trace metal ratios
    !-----------------------------------------------------------------------
    If (ctrl_data_save_slice_bio .AND. ctrl_data_save_slice_diag_proxy) then
       IF (ocn_select(io_Cd)) THEN
          loc_unitsname = 'nmol kg-1 (umol kg-1)-1'
          IF (sed_select(is_POCd) .AND. sed_select(is_POC)) THEN
             loc_ij(:,:) = const_real_null
             DO i=1,n_i
                DO j=1,n_j
                   IF (n_k >= goldstein_k1(i,j)) THEN
                      if (int_bio_settle_timeslice(is_POC,i,j,n_k) > const_real_nullsmall) then
                         loc_ij(i,j) = 1.0E3*int_bio_settle_timeslice(is_POCd,i,j,n_k)/int_bio_settle_timeslice(is_POC,i,j,n_k)
                      end if
                   end IF
                END DO
             END DO
             call sub_adddef_netcdf(loc_iou,3,'misc_sur_rCdtoC_POM','Cd to C organic matter export ratio', &
                  & trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('misc_sur_rCdtoC_POM',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          end IF
          IF (sed_select(is_POCd) .AND. sed_select(is_POP)) THEN
             loc_unitsname = 'nmol kg-1 (umol kg-1)-1'
             loc_ij(:,:) = const_real_null
             DO i=1,n_i
                DO j=1,n_j
                   IF (n_k >= goldstein_k1(i,j)) THEN
                      if (int_bio_settle_timeslice(is_POP,i,j,n_k) > const_real_nullsmall) then
                         loc_ij(i,j) = 1.0E3*int_bio_settle_timeslice(is_POCd,i,j,n_k)/int_bio_settle_timeslice(is_POP,i,j,n_k)
                      end if
                   end IF
                END DO
             END DO
             call sub_adddef_netcdf(loc_iou,3,'misc_sur_rCdtoP_POM','Cd to P organic matter export ratio', &
                  & trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('misc_sur_rCdtoP_POM',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          end IF
          IF (sed_select(is_CdCO3) .AND. sed_select(is_CaCO3)) THEN
             loc_unitsname = 'nmol kg-1 (mmol kg-1)-1'
             loc_ij(:,:) = const_real_null
             DO i=1,n_i
                DO j=1,n_j
                   IF (n_k >= goldstein_k1(i,j)) THEN
                      if (int_bio_settle_timeslice(is_CaCO3,i,j,n_k) > const_real_nullsmall) then
                         loc_ij(i,j) = 1.0E6*int_bio_settle_timeslice(is_CdCO3,i,j,n_k)/int_bio_settle_timeslice(is_CaCO3,i,j,n_k)
                      end if
                   end IF
                END DO
             END DO
             call sub_adddef_netcdf(loc_iou,3,'misc_sur_rCdtoCa_carbonate','Cd:Ca trace metal ratio (carbonate)', &
                  & trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('misc_sur_rCdtoCa_carbonate',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          end IF
       end IF
    end if
    !-----------------------------------------------------------------------
    !       Fe diagnostics
    !-----------------------------------------------------------------------
    If (ctrl_data_save_slice_ocn .AND. ctrl_data_save_slice_diag_geochem) then
       IF (ocn_select(io_Fe)) THEN
          ! total aeolian Fe flux (mass)
          loc_unitsname = 'mg Fe m-2 yr-1'
          loc_ij(:,:) = conv_mol_mmol*par_det_Fe_frac*conv_det_mol_g* &
               & int_phys_ocn_timeslice(ipo_rA,:,:,n_k)*int_bio_settle_timeslice(is_det,:,:,n_k)/(int_t_timeslice**2)
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_fFetot_g','Total aeolian iron flux to surface', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_fFetot_g',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          ! total aeolian Fe flux (moles)
          loc_unitsname = 'mmol Fe m-2 yr-1'
          loc_ij(:,:) = conv_Fe_g_mol*loc_ij(:,:)
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_fFetot_mol','Total aeolian iron flux to surface', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_fFetot_mol',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          ! solulablized aeolian Fe flux
          loc_unitsname = 'umol Fe m-2 yr-1'
          loc_ij(:,:) = conv_mol_umol*conv_mmol_mol*int_phys_ocnatm_timeslice(ipoa_solFe,:,:)*loc_ij(:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_fFe_mol','Dissolved aeolian iron flux to surface', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_fFe_mol',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          ! particulate Fe loss
          loc_unitsname = 'umol Fe m-2 yr-1'
          loc_ij(:,:) = conv_mol_umol* &
               & int_phys_ocn_timeslice(ipo_rA,:,:,n_k)*int_bio_settle_timeslice(is_POFe,:,:,n_k)/(int_t_timeslice**2)
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_fpartFe','Particulate organic matter iron loss from surface', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_fpartFe',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          ! total scavenged Fe loss
          loc_unitsname = 'umol Fe m-2 yr-1'
          loc_ij(:,:) = (conv_mol_umol*int_phys_ocn_timeslice(ipo_rA,:,:,n_k)/(int_t_timeslice**2))* &
               & ( &
               &   int_bio_settle_timeslice(is_POM_Fe,:,:,n_k)   + &
               &   int_bio_settle_timeslice(is_CaCO3_Fe,:,:,n_k) + &
               &   int_bio_settle_timeslice(is_opal_Fe,:,:,n_k) + &
               &   int_bio_settle_timeslice(is_det_Fe,:,:,n_k) &
               & )
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_fscavFetot','Total scavenged iron loss from surface', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_fscavFetot',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          ! solubility (%)
          loc_unitsname = '%'
          loc_ij(:,:) = 100.0*int_phys_ocnatm_timeslice(ipoa_solFe,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_Fe_sol','Aeolian iron solubility', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_Fe_sol',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end if
    end if
    !-----------------------------------------------------------------------
    !       Fe:C export cellular quotient ratio
    !-----------------------------------------------------------------------
    if (ctrl_data_save_slice_bio .AND. ctrl_data_save_slice_diag_bio) then
       IF (sed_select(is_POFe) .AND. sed_select(is_POC)) THEN
          ! C/Fe
          loc_unitsname = 'n/a'
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   if (int_bio_settle_timeslice(is_POFe,i,j,n_k) > const_real_nullsmall) then
                      loc_ij(i,j) = int_bio_settle_timeslice(is_POC,i,j,n_k)/int_bio_settle_timeslice(is_POFe,i,j,n_k)
                   end if
                end IF
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_rPOCtoPOFe','average POM export C/Fe cellular ratio', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_rPOCtoPOFe',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          ! Fe/C
          loc_unitsname = '10^3 o/oo'
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   if (int_bio_settle_timeslice(is_POC,i,j,n_k) > const_real_nullsmall) then
                      loc_ij(i,j) = 1.0E6*int_bio_settle_timeslice(is_POFe,i,j,n_k)/int_bio_settle_timeslice(is_POC,i,j,n_k)
                   end if
                end IF
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_rPOFetoPOC','average POM export Fe/C cellular ratio', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_rPOFetoPOC',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end IF
    end if
    !-----------------------------------------------------------------------
    !       Biological productivity controls
    !-----------------------------------------------------------------------
    if (ctrl_data_save_slice_bio .AND. ctrl_data_save_slice_diag_bio) then
       DO ib=1,n_diag_bio
          select case (ib)
          CASE (idiag_bio_dPO4,idiag_bio_dPO4_1,idiag_bio_dPO4_2)
             loc_unitsname = 'mol kg-1 yr-1'
          CASE (idiag_bio_N2fixation,idiag_bio_NH4assim)
             loc_unitsname = 'mol kg-1 yr-1'
          case default
             loc_unitsname = 'n/a'
          end select
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                If (goldstein_k1(i,j) <= n_k) then
                   loc_ij(i,j) = int_diag_bio_timeslice(ib,i,j)/int_t_timeslice
                end If
             end DO
          end DO
          select case (ib)
          CASE (idiag_bio_CaCO3toPOC_nsp,idiag_bio_opaltoPOC_sp,idiag_bio_fspPOC)
             ! correct for the number of sub-slices to create an average
             loc_ij(:,:) = loc_ij(:,:)/real(int_t_timeslice_count)
          end select
          call sub_adddef_netcdf(loc_iou,3,'bio_diag_'//trim(string_diag_bio(ib)), &
               & 'biological productivity control - '//trim(string_diag_bio(ib)), &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('bio_diag_'//trim(string_diag_bio(ib)),loc_iou, &
               & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end DO
       ! nutrient limitation
       ! NOTE: -1.0 equates to dominance of PO4 limitation, +1.0 to dominance of Fe limitation
       !       a value of ~0.0 represents nutrient replete conditions OR ~equal limitation
       if ( ocn_select(io_PO4) .AND. (ocn_select(io_Fe) .OR. ocn_select(io_TDFe)) ) then
          loc_unitsname = 'n/a'
          loc_ij(:,:) = ( int_diag_bio_timeslice(idiag_bio_kFe,:,:) - int_diag_bio_timeslice(idiag_bio_kPO4,:,:) )/ &
               & int_t_timeslice
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_PO4Felimbalance','occurrence of Fe vs. PO4 limitation condition', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_PO4Felimbalance',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end if
    end if
    !-----------------------------------------------------------------------
    ! ECOGEM diagnostics
    !-----------------------------------------------------------------------
    if (ctrl_data_save_slice_bio .AND. ctrl_data_save_slice_diag_bio .AND. flag_ecogem) then
       ! calculate POM equivalnt of DOM
       loc_isij(:,:,:) = 0.0
       DO i=1,n_i
          DO j=1,n_j
             If (goldstein_k1(i,j) <= n_k) then
                DO l=3,n_l_ocn
                   io = conv_iselected_io(l)
                   loc_tot_m = conv_DOM_POM_i(0,io)
                   do loc_m=1,loc_tot_m
                      is = conv_DOM_POM_i(loc_m,io)
                      loc_isij(is,i,j) = loc_isij(is,i,j) + conv_DOM_POM(is,io)*int_diag_ecogem_remin(io,i,j)
                   end do
                end do
             end If
          end DO
       end DO
       ! calculate DOM ratio (replace values in same local array)
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          DO i=1,n_i
             DO j=1,n_j
                If (goldstein_k1(i,j) <= n_k) then
                   if ((loc_isij(is,i,j)+int_diag_ecogem_part(is,i,j)) > const_real_nullsmall) then
                      loc_isij(is,i,j) = loc_isij(is,i,j)/(loc_isij(is,i,j)+int_diag_ecogem_part(is,i,j))
                   else
                      loc_isij(is,i,j) = 0.0
                   end if
                end If
             end DO
          end DO
          call sub_adddef_netcdf(loc_iou,3,'eco_diag_DOMfract_'//trim(string_sed(is)), &
               & 'ECOGEM dissolved matter production fraction - '//trim(string_sed(is)), &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('eco_diag_DOMfract_'//trim(string_sed(is)),loc_iou, &
               & n_i,n_j,loc_ntrec,loc_isij(is,:,:),loc_mask_surf)
       end do
    end if
    !-----------------------------------------------------------------------
    ! nutrient availablity diagnostics
    !-----------------------------------------------------------------------
    if (ctrl_data_save_slice_bio .AND. ctrl_data_save_slice_diag_bio) then
       if ( ocn_select(io_PO4) .AND. ocn_select(io_SiO2) ) then
          loc_unitsname = 'n/a'
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   loc_ij(i,j) = int_ocn_timeslice(io_SiO2,i,j,n_k)/int_t_timeslice - &
                        & par_bio_red_POP_PON*int_ocn_timeslice(io_PO4,i,j,n_k)/int_t_timeslice
                end IF
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_sur_SiSTAR','Si Star', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_sur_SiSTAR',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end if
    end if
    ! ### INSERT CODE TO SAVE ADDITIONAL 2-D DATA FIELDS ######################################################################### !
    !
    ! ############################################################################################################################ !
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_2d_USER
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CDR-MIP 2-D FIELDS
  SUBROUTINE sub_save_netcdf_2d_cdrmip()
    !-----------------------------------------------------------------------
    ! DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    integer::i,j
    INTEGER::loc_iou,loc_ntrec
    real,DIMENSION(n_i,n_j)::loc_ij,loc_mask_surf,loc_mask_surf_ALL
    CHARACTER(len=255)::loc_unitsname
    !-----------------------------------------------------------------------
    ! INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou                = ncout2d_iou
    loc_ntrec              = ncout2d_ntrec
    loc_mask_surf(:,:)     = phys_ocnatm(ipoa_mask_ocn,:,:)
    loc_mask_surf_ALL(:,:) = 1.0
    !-----------------------------------------------------------------------
    ! WRITE DATA
    !-----------------------------------------------------------------------
    loc_ij(:,:) = const_real_zero
    DO i=1,n_i
       DO j=1,n_j
          loc_unitsname = 'uatm'
          loc_ij(i,j) = int_sfcatm1_timeslice(ia_pCO2,i,j)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,3,'cdrmip_pco2','atmospheric pCO2', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('cdrmip_pco2',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf_ALL)
       end do
    end do
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_2d_cdrmip
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** save time-slice data ***
  SUBROUTINE sub_save_netcdf_2d()
    !-----------------------------------------------------------------------
    !       local variables
    !-----------------------------------------------------------------------
    INTEGER::l,i,j,ia,io,is
    integer::ib,id,ip,ic
    integer::loc_k1
    integer::loc_iou,loc_ntrec
    CHARACTER(len=255)::loc_unitsname
    real,DIMENSION(n_i,n_j)::loc_ij,loc_mask_surf,loc_mask_surf_ALL
    real::loc_tot,loc_frac,loc_standard
    real::loc_d13C,loc_d14C
    !-----------------------------------------------------------------------
    !       initialize local variables
    !-----------------------------------------------------------------------
    loc_iou   = ncout2d_iou
    loc_ntrec = ncout2d_ntrec
    loc_mask_surf(:,:)     = phys_ocnatm(ipoa_mask_ocn,:,:)
    loc_mask_surf_ALL(:,:) = 1.0
    !-----------------------------------------------------------------------
    !              <ocnatm_*>
    !       save ocean-atmosphere interface tracer data field
    !-----------------------------------------------------------------------
    If (ctrl_data_save_slice_ocnatm) then
       DO l=1,n_l_atm
          ia = conv_iselected_ia(l)
          loc_ij(:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                SELECT CASE (atm_type(ia))
                CASE (0,1)
                   loc_ij(i,j) = int_sfcatm1_timeslice(ia,i,j)/int_t_timeslice
                case (n_itype_min:n_itype_max)
                   loc_tot  = int_sfcatm1_timeslice(atm_dep(ia),i,j)/int_t_timeslice
                   loc_frac = int_sfcatm1_timeslice(ia,i,j)/int_t_timeslice
                   loc_standard = const_standards(atm_type(ia))
                   loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                END SELECT
             end DO
          end DO
          SELECT CASE (atm_type(ia))
          CASE (0,1,n_itype_min:n_itype_max)
             call sub_adddef_netcdf(loc_iou,3,'atm_'//trim(string_atm_tname(l)), &
                  & trim(string_atm_tlname(l)),trim(string_atm_unit(l)),atm_mima(l,1),atm_mima(l,2))
             call sub_putvar2d('atm_'//trim(string_atm(ia)),loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask_surf_ALL)
          END SELECT
       END DO
    end if
    !-----------------------------------------------------------------------
    !       save air-sea gas exchange flux data
    !-----------------------------------------------------------------------
    If (ctrl_data_save_slice_fairsea .OR. ctrl_data_save_slice_ocnatm) then
       CALL sub_save_netcdf_flux_seaair()
    end if
    !-----------------------------------------------------------------------
    !       save atmospheric 14C data in dumb-ass D14C units
    !-----------------------------------------------------------------------
    If (ctrl_data_save_slice_ocnatm) then
       IF (atm_select(ia_pCO2_13C) .AND. atm_select(ia_pCO2_14C)) THEN
          loc_ij(:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                loc_tot  = int_sfcatm1_timeslice(ia_pCO2,i,j)/int_t_timeslice
                loc_frac = int_sfcatm1_timeslice(ia_pCO2_13C,i,j)/int_t_timeslice
                loc_standard = const_standards(atm_type(ia_pCO2_13C))
                loc_d13C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                loc_frac = int_sfcatm1_timeslice(ia_pCO2_14C,i,j)/int_t_timeslice
                loc_standard = const_standards(atm_type(ia_pCO2_14C))
                loc_d14C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                loc_ij(i,j) = fun_convert_delta14CtoD14C(loc_d13C,loc_d14C)
                loc_unitsname = 'o/oo'
             end do
          end do
          call sub_adddef_netcdf(loc_iou,3,'atm_pCO2_D14C', &
               & ' atmospheric pCO2 D14C',trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('atm_pCO2_D14C',loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask_surf_ALL)
       end if
    end if
    !----------------------------------------------------------------
    !              <misc_*>
    !       save miscellaneous data
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_misc .AND. ctrl_data_save_slice_carb) then
       IF (opt_select(iopt_select_carbchem)) THEN
          ! air-sea delta pCO2
          loc_ij(:,:) = const_real_zero
          loc_ij(:,:) = phys_ocn(ipo_mask_ocn,:,:,n_k) * &
               & (int_carb_timeslice(ic_fug_CO2,:,:,n_k) - int_sfcatm1_timeslice(ia_pCO2,:,:))/int_t_timeslice
          loc_unitsname = 'atm'
          call sub_adddef_netcdf(loc_iou,3,'atm_dpCO2','air-sea pCO2 diff',trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('atm_dpCO2',loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask_surf)
       end if
    END IF
    !----------------------------------------------------------------
    !              <phys_*>
    !       save physical ocean data
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_misc) then
       ! (1) overturning stream-function
       CALL sub_save_netcdf_goldstein_opsi()
       ! (2) surface wind speed
       loc_unitsname = 'm s-1'
       call sub_adddef_netcdf(loc_iou,3,'phys_wspeed','gas transfer windspeed',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_wspeed',loc_iou,n_i,n_j,loc_ntrec, &
            & int_phys_ocnatm_timeslice(ipoa_wspeed,:,:)/int_t_timeslice,loc_mask_surf)
       ! (3) fractional sea-ice cover
       loc_unitsname = 'n/a'
       call sub_adddef_netcdf(loc_iou,3,'phys_seaice','sea-ice cover (%)',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_seaice',loc_iou,n_i,n_j,loc_ntrec, &
            & 100.0*int_phys_ocnatm_timeslice(ipoa_seaice,:,:)/int_t_timeslice,loc_mask_surf)
       ! (4) sea-ice thickness
       loc_unitsname = 'm'
       call sub_adddef_netcdf (loc_iou,3,'phys_seaice_th','sea-ice thickness',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_seaice_th',loc_iou,n_i,n_j,loc_ntrec, &
            & int_phys_ocnatm_timeslice(ipoa_seaice_th,:,:)/int_t_timeslice,loc_mask_surf)
       ! (5) solar forcing
       loc_unitsname = 'W m-2'
       call sub_adddef_netcdf(loc_iou,3,'phys_solfor','solar forcing',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_solfor',loc_iou,n_i,n_j,loc_ntrec, &
            & int_phys_ocnatm_timeslice(ipoa_solfor,:,:)/int_t_timeslice,loc_mask_surf_ALL)
       ! (6) incident sw radiation
       loc_unitsname = 'W m-2'
       call sub_adddef_netcdf(loc_iou,3,'phys_fxsw','incident sw radiation',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_fxsw',loc_iou,n_i,n_j,loc_ntrec, &
            & int_phys_ocnatm_timeslice(ipoa_fxsw,:,:)/int_t_timeslice,loc_mask_surf_ALL)
       ! (7) wind stress
       loc_unitsname = 'N/m-2'
       call sub_adddef_netcdf (loc_iou,3,'phys_tau_u','wind stress (u)',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_tau_u',loc_iou,n_i,n_j,loc_ntrec, &
            & int_phys_ocnatm_timeslice(ipoa_tau_u,:,:)/int_t_timeslice,loc_mask_surf)
       loc_unitsname = 'N/m-2'
       call sub_adddef_netcdf(loc_iou,3,'phys_tau_v','wind stress (v)',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_tau_v',loc_iou,n_i,n_j,loc_ntrec, &
            & int_phys_ocnatm_timeslice(ipoa_tau_v,:,:)/int_t_timeslice,loc_mask_surf)
       ! (8) convective 'cost'
       loc_unitsname = '???'
       call sub_adddef_netcdf(loc_iou,3,'phys_cost','convective cost',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_cost',loc_iou,n_i,n_j,loc_ntrec, &
            & int_phys_ocnatm_timeslice(ipoa_cost,:,:)/int_t_timeslice,loc_mask_surf)
       ! (9) air-sea gas exchange coefficient
       if (opt_select(iopt_select_ocnatm_CO2) .AND. ctrl_data_save_slice_carb) then
          loc_unitsname = 'mol m-2 yr-1 uatm-1'
          call sub_adddef_netcdf(loc_iou,3,'phys_KCO2','air-sea gas ex. coef.',trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('phys_KCO2',loc_iou,n_i,n_j,loc_ntrec, &
               & int_phys_ocnatm_timeslice(ipoa_KCO2,:,:)/int_t_timeslice,loc_mask_surf)
       end if
       ! (10) MLD
       loc_unitsname = 'm'
       call sub_adddef_netcdf(loc_iou,3,'phys_MLD','mixed layer depth',trim(loc_unitsname),const_real_zero,const_real_zero)
       call sub_putvar2d('phys_MLD',loc_iou,n_i,n_j,loc_ntrec, &
            & int_phys_ocnatm_timeslice(ipoa_mld,:,:)/int_t_timeslice,loc_mask_surf)
    end if
    !----------------------------------------------------------------
    !       FULL ATMOSPEHRE 'PHYSICS'
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_phys_atm) then
       loc_ij(:,:) = const_real_zero
       DO ip=1,n_phys_ocnatm
          loc_ij(:,:) = int_phys_ocnatm_timeslice(ip,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,3,'phys_ocnatm_'//trim(string_phys_ocnatm(ip)), &
               & 'full atmosphere physical properties - '//trim(string_phys_ocnatm(ip)),' ',const_real_zero,const_real_zero)
          call sub_putvar2d('phys_ocnatm_'//trim(string_phys_ocnatm(ip)),loc_iou, &
               & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf_ALL)
       END DO
    end if
    !----------------------------------------------------------------
    !              <diag_*>
    !       save diagnostics data
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_diag_geochem .AND. flag_rokgem) then
       loc_unitsname = 'mol kg-1 yr-1'
       DO ib=1,n_diag_bio
          loc_ij(:,:) = int_diag_bio_timeslice(ib,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,3,'diag_'//trim(string_diag_bio(ib)), &
               & 'biological transformation rate - '//trim(string_diag_bio(ib)),trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('diag_'//trim(string_diag_bio(ib)),loc_iou, &
               & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end DO
       DO l=3,n_l_ocn
          io = conv_iselected_io(l)
          loc_ij(:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                SELECT CASE (ocn_type(io))
                CASE (1)
                   loc_ij(i,j) = int_diag_weather_timeslice(io,i,j)
                   loc_unitsname = 'mol yr-1'
                case (n_itype_min:n_itype_max)
                   loc_tot  = int_diag_weather_timeslice(ocn_dep(io),i,j)
                   loc_frac = int_diag_weather_timeslice(io,i,j)
                   loc_standard = const_standards(ocn_type(io))
                   loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                   loc_unitsname = 'o/oo'
                end SELECT
             end DO
          end DO
          SELECT CASE (ocn_type(io))
          CASE (1,n_itype_min:n_itype_max)
             call sub_adddef_netcdf(loc_iou,3,'diag_weather_'//trim(string_ocn(io)), &
                  & 'weathering flux - '//trim(string_ocn(io)),trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('diag_weather_'//trim(string_ocn(io)),loc_iou, &
                  & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          end SELECT
       END DO
    end If
    !----------------------------------------------------------------
    !       WATER-COLUMN INTEGRATED TRACER INVENTORIES
    !----------------------------------------------------------------
    If ((ctrl_data_save_slice_ocn .AND. ctrl_data_save_slice_diag_tracer) .OR. (par_data_save_level == 10)) then
       loc_unitsname = 'mol m-2'
       DO l=3,n_l_ocn
          io = conv_iselected_io(l)
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                If (goldstein_k1(i,j) <= n_k) then
                   loc_ij(i,j) = &
                        & sum(phys_ocn(ipo_M,i,j,:)*int_ocn_timeslice(io,i,j,:))* &
                        & phys_ocn(ipo_rA,i,j,n_k)/int_t_timeslice
                end If
             end DO
          end DO
          SELECT CASE (ocn_type(io))
          CASE (1)
             call sub_adddef_netcdf(loc_iou,3,'ocn_int_'//trim(string_ocn(io)), &
                  & trim(string_ocn(io))//' water-column integrated tracer inventory', &
                  & trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('ocn_int_'//trim(string_ocn(io)),loc_iou, &
                  & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
          END SELECT
       end DO
    end If
    !----------------------------------------------------------------
    ! WATER-COLUMN INTEGRATED PRODUCTION/OXIDATION RATE -- OLD
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_diag_geochem .AND. ctrl_data_save_slice_diag_redox_old) then
       loc_unitsname = 'mol m-2 yr-1'
       DO id=1,n_diag_geochem
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                If (goldstein_k1(i,j) <= n_k) then
                   loc_ij(i,j) = &
                        & sum(phys_ocn(ipo_M,i,j,:)*int_diag_geochem_timeslice(id,i,j,:))* &
                        & phys_ocn(ipo_rA,i,j,n_k)/int_t_timeslice
                end If
             end DO
          end DO
          call sub_adddef_netcdf(loc_iou,3,'diag_int_'//trim(string_diag_geochem(id)), &
               & 'water-column integrated production/oxidation rate - '//trim(string_diag_geochem(id)), &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('diag_int_'//trim(string_diag_geochem(id)),loc_iou, &
               & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end DO
    end If
    !----------------------------------------------------------------
    ! WATER-COLUMN INTEGRATED REDOX TRANSFORMATIONS
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_diag_geochem) then
       loc_unitsname = 'mol m-2 yr-1'
       DO id=1,n_diag_redox
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                If (goldstein_k1(i,j) <= n_k) then
                   loc_ij(i,j) = &
                        & sum(phys_ocn(ipo_M,i,j,:)*int_diag_redox_timeslice(id,i,j,:))* &
                        & phys_ocn(ipo_rA,i,j,n_k)/int_t_timeslice
                end If
             end DO
          end DO
          call sub_adddef_netcdf(loc_iou,3,'redox_int_'//trim(string_diag_redox(id)), &
               & 'water-column integrated redox transformation rate - '//trim(string_diag_redox(id)), &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('redox_int_'//trim(string_diag_redox(id)),loc_iou, &
               & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
       end DO
    end If
    !----------------------------------------------------------------
    !       OCEAN SURFACE DATA
    !----------------------------------------------------------------
    ! NOTE: exclude dissolved organic matter tracers
    If ((ctrl_data_save_slice_ocn .AND. ctrl_data_save_slice_diag_proxy) .OR. ctrl_data_save_slice_sur) then
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          is = maxval(maxloc(abs(conv_DOM_POM(:,io))))-1
          if (is == 0) then
             loc_ij(:,:) = const_real_zero
             DO i=1,n_i
                DO j=1,n_j
                   loc_k1 = goldstein_k1(i,j)
                   IF (n_k >= loc_k1) THEN
                      SELECT CASE (ocn_type(io))
                      CASE (0)
                         if (io == io_T) then
                            loc_ij(i,j) = int_ocn_timeslice(io,i,j,n_k)/int_t_timeslice - const_zeroC
                         else
                            loc_ij(i,j) = int_ocn_timeslice(io,i,j,n_k)/int_t_timeslice
                         end if
                      CASE (1)
                         loc_ij(i,j) = int_ocn_timeslice(io,i,j,n_k)/int_t_timeslice
                      case (n_itype_min:n_itype_max)
                         loc_tot  = int_ocn_timeslice(ocn_dep(io),i,j,n_k)
                         loc_frac = int_ocn_timeslice(io,i,j,n_k)
                         loc_standard = const_standards(ocn_type(io))
                         loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                      end SELECT
                   end IF
                end DO
             end DO
             SELECT CASE (ocn_type(io))
             CASE (0)
                If (io == io_T) then
                   loc_unitsname = 'degrees C'
                else
                   loc_unitsname = 'o/oo'
                end If
             CASE (1)
                loc_unitsname = 'mol kg-1'
             case (n_itype_min:n_itype_max)
                loc_unitsname = 'o/oo'
             end SELECT
             SELECT CASE (ocn_type(io))
             CASE (0,1,n_itype_min:n_itype_max)
                call sub_adddef_netcdf(loc_iou, 3, 'ocn_sur_'//trim(string_ocn(io)), &
                     & 'surface-water '//trim(string_ocn(io)), trim(loc_unitsname),const_real_zero,const_real_zero)
                call sub_putvar2d('ocn_sur_'//trim(string_ocn(io)),loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask_surf)
             end SELECT
          end if
       END DO
    end if
    !----------------------------------------------------------------
    !       OCEAN FLOOR DATA
    !----------------------------------------------------------------
    ! NOTE: exclude dissolved organic matter tracers
    If ((ctrl_data_save_slice_ocn .AND. ctrl_data_save_slice_diag_proxy) .OR. ctrl_data_save_slice_sur) then
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          is = maxval(maxloc(abs(conv_DOM_POM(:,io))))-1
          if (is == 0) then
             loc_ij(:,:) = const_real_zero
             DO i=1,n_i
                DO j=1,n_j
                   loc_k1 = goldstein_k1(i,j)
                   IF (n_k >= loc_k1) THEN
                      SELECT CASE (ocn_type(io))
                      CASE (0)
                         if (io == io_T) then
                            loc_ij(i,j) = int_ocn_timeslice(io,i,j,loc_k1)/int_t_timeslice - const_zeroC
                         else
                            loc_ij(i,j) = int_ocn_timeslice(io,i,j,loc_k1)/int_t_timeslice
                         end if
                      CASE (1)
                         loc_ij(i,j) = int_ocn_timeslice(io,i,j,loc_k1)/int_t_timeslice
                      case (n_itype_min:n_itype_max)
                         loc_tot  = int_ocn_timeslice(ocn_dep(io),i,j,loc_k1)
                         loc_frac = int_ocn_timeslice(io,i,j,loc_k1)
                         loc_standard = const_standards(ocn_type(io))
                         loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                      end SELECT
                   end IF
                end DO
             end DO
             SELECT CASE (ocn_type(io))
             CASE (0)
                If (io == io_T) then
                   loc_unitsname = 'degrees C'
                else
                   loc_unitsname = 'o/oo'
                end If
             CASE (1)
                loc_unitsname = 'mol kg-1'
             case (n_itype_min:n_itype_max)
                loc_unitsname = 'o/oo'
             end SELECT
             SELECT CASE (ocn_type(io))
             CASE (0,1,n_itype_min:n_itype_max)
                call sub_adddef_netcdf(loc_iou, 3, 'ocn_ben_'//trim(string_ocn(io)), &
                     & 'bottom-water '//trim(string_ocn(io)), trim(loc_unitsname),const_real_zero,const_real_zero)
                call sub_putvar2d('ocn_ben_'//trim(string_ocn(io)),loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask_surf)
             end SELECT
          end if
       END DO
    end if
    !----------------------------------------------------------------
    !       ocean surface carbonate chemistry data
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_sur) then
       IF (opt_select(iopt_select_carbchem)) THEN
          loc_ij(:,:) = const_real_zero
          DO ic=1,n_carb
             DO i=1,n_i
                DO j=1,n_j
                   loc_k1 = goldstein_k1(i,j)
                   IF (n_k >= loc_k1) THEN
                      SELECT CASE (ic)
                      CASE (ic_ohm_cal,ic_ohm_arg)
                         loc_ij(i,j) = int_carb_timeslice(ic,i,j,n_k)/int_t_timeslice
                      case default
                         loc_ij(i,j) = int_carb_timeslice(ic,i,j,n_k)/int_t_timeslice
                      end SELECT
                   end IF
                end DO
             end DO
             SELECT CASE (ic)
             CASE (ic_ohm_cal,ic_ohm_arg)
                loc_unitsname = ' '
             case default
                loc_unitsname = 'mol kg-1'
             end SELECT
             call sub_adddef_netcdf(loc_iou,3,'carb_sur_'//trim(string_carb(ic)), &
                  & 'surface '//trim(string_carb(ic)),trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('carb_sur_'//trim(string_carb(ic)),loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask_surf)
          END DO
       end if
    end if
    !----------------------------------------------------------------
    !       PARTICULATE FLUXES
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_bio) then
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_ij(:,:) = const_real_zero
          !---------------------------------------------------------- flux density
          DO i=1,n_i
             DO j=1,n_j
                loc_k1 = goldstein_k1(i,j)
                IF (n_k >= loc_k1) THEN
                   SELECT CASE (sed_type(is))
                   CASE (par_sed_type_bio,par_sed_type_abio, &
                        & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
                        & par_sed_type_scavenged)
                      loc_ij(i,j) = int_bio_settle_timeslice(is,i,j,n_k)*phys_ocn(ipo_rA,i,j,n_k)/int_t_timeslice
                      loc_unitsname = 'mol m-2 yr-1'
                   case (n_itype_min:n_itype_max)
                      loc_tot  = int_bio_settle_timeslice(sed_dep(is),i,j,n_k)*phys_ocn(ipo_rA,i,j,n_k)/int_t_timeslice
                      loc_frac = int_bio_settle_timeslice(is,i,j,n_k)*phys_ocn(ipo_rA,i,j,n_k)/int_t_timeslice
                      loc_standard = const_standards(sed_type(is))
                      loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                      loc_unitsname = 'o/oo'
                   CASE (par_sed_type_frac)
                      loc_ij(i,j) = int_bio_settle_timeslice(is,i,j,n_k)
                      loc_unitsname = 'n/a'
                   end SELECT
                end if
             end do
          end do
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged,n_itype_min:n_itype_max, &
               & par_sed_type_frac)
             call sub_adddef_netcdf(loc_iou,3,'bio_export_'//trim(string_sed(is)), &
                  & 'biological export - '//trim(string_sed(is)),loc_unitsname,const_real_zero,const_real_zero)
             call sub_putvar2d('bio_export_'//trim(string_sed(is)),loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask_surf)
          end SELECT
       end do
    end if
    !-----------------------------------------------------------------------
    !       save ocn-sed interface data
    !-----------------------------------------------------------------------
    CALL sub_save_netcdf_2d_sed()
    !----------------------------------------------------------------
    !       USED-DEFINED (MISC) FIELDS
    !----------------------------------------------------------------
    CALL sub_save_netcdf_2d_USER()
    !--------------------------------------------------------- !
    ! CDR-MIP
    !--------------------------------------------------------- !
    if (ctrl_data_save_slice_cdrmip) CALL sub_save_netcdf_2d_cdrmip()
    !--------------------------------------------------------- !
  END SUBROUTINE sub_save_netcdf_2d
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** save time-slice data ***
  SUBROUTINE sub_save_netcdf_2d_sed()
    !-----------------------------------------------------------------------
    !       local variables
    !-----------------------------------------------------------------------
    INTEGER::l,i,j,io,is,ic
    integer::loc_k1
    integer::loc_iou,loc_ntrec
    CHARACTER(len=255)::loc_unitsname
    real,DIMENSION(n_i,n_j)::loc_ij,loc_ij_1,loc_ij_2,loc_sed_mask
    real::loc_tot,loc_frac,loc_standard
    !-----------------------------------------------------------------------
    !       initialize local variables
    !-----------------------------------------------------------------------
    loc_iou   = ncout2d_iou
    loc_ntrec = ncout2d_ntrec
    loc_sed_mask(:,:) = phys_ocn(ipo_mask_ocn,:,:,n_k)
    !----------------------------------------------------------------
    !       save ocn->sed interface flux data
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_focnsed) then
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_ij(:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                SELECT CASE (sed_type(is))
                CASE (par_sed_type_bio,par_sed_type_abio, &
                     & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
                     & par_sed_type_scavenged)
                   loc_ij(i,j) = int_focnsed_timeslice(is,i,j)
                   loc_unitsname = 'mol yr-1'
                CASE (par_sed_type_age)
                   if (int_focnsed_timeslice(sed_dep(is),i,j) > 0.0) then
                      loc_ij(i,j) = int_focnsed_timeslice(is,i,j)/int_focnsed_timeslice(sed_dep(is),i,j)
                      loc_unitsname = 'years'
                   end if
                case (n_itype_min:n_itype_max)
                   loc_tot  = int_focnsed_timeslice(sed_dep(is),i,j)
                   loc_frac = int_focnsed_timeslice(is,i,j)
                   loc_standard = const_standards(sed_type(is))
                   loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_real_null)
                   loc_unitsname = 'o/oo'
                end SELECT
             end DO
          end DO
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged,par_sed_type_age,n_itype_min:n_itype_max)
             call sub_adddef_netcdf(loc_iou,3,'focnsed_'//trim(string_sed(is)), &
                  & trim(string_sed(is))//' ocean->sediment flux',trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('focnsed_'//trim(string_sed(is)),loc_iou,n_i,n_j, &
                  & loc_ntrec, loc_ij, loc_sed_mask)
          end SELECT
       END DO
    end if
    !----------------------------------------------------------------
    !       save sed->ocn interface flux data
    !----------------------------------------------------------------
    ! NOTE: exclude dissolved organic matter tracers
    If (ctrl_data_save_slice_fsedocn) then
       DO l=3,n_l_ocn
          io = conv_iselected_io(l)
          is = maxval(maxloc(abs(conv_DOM_POM(:,io))))-1
          if (is == 0) then
             loc_ij(:,:) = const_real_zero
             DO i=1,n_i
                DO j=1,n_j
                   SELECT CASE (ocn_type(io))
                   CASE (1)
                      loc_ij(i,j) = int_fsedocn_timeslice(io,i,j)
                      loc_unitsname = 'mol yr-1'
                   case (n_itype_min:n_itype_max)
                      loc_tot  = int_fsedocn_timeslice(ocn_dep(io),i,j)
                      loc_frac = int_fsedocn_timeslice(io,i,j)
                      loc_standard = const_standards(ocn_type(io))
                      loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_real_null)
                      loc_unitsname = 'o/oo'
                   end SELECT
                end DO
             end DO
             SELECT CASE (ocn_type(io))
             CASE (1,n_itype_min:n_itype_max)
                call sub_adddef_netcdf(loc_iou,3,'fsedocn_'//trim(string_ocn(io)), &
                     & trim(string_ocn(io))//' sediment->ocean flux',trim(loc_unitsname),const_real_zero,const_real_zero)
                call sub_putvar2d('fsedocn_'//trim(string_ocn(io)),loc_iou,n_i,n_j, &
                     & loc_ntrec,loc_ij,loc_sed_mask)
             end SELECT
          end if
       END DO
    end if
    !----------------------------------------------------------------
    !       save core-top sediment composition data
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_ocnsed) then
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio)
             loc_unitsname = 'wt%'
          CASE (par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det,par_sed_type_scavenged)
             loc_unitsname = 'ppm'
          CASE (n_itype_min:n_itype_max)
             loc_unitsname = 'o/oo'
          CASE (par_sed_type_age)
             loc_unitsname = 'years'
          end SELECT
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged,par_sed_type_age,n_itype_min:n_itype_max)
             call sub_adddef_netcdf(loc_iou,3,'sed_'//trim(string_sed(is)), &
                  & 'sediment core-top '//trim(string_sed(is)),trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('sed_'//trim(string_sed(is)),loc_iou,n_i,n_j, &
                  & loc_ntrec,int_sfcsed1_timeslice(is,:,:)/int_t_timeslice,loc_sed_mask)
          end SELECT
       END DO
    end if
    !----------------------------------------------------------------
    !       save overlying ocean dissolved tracer data
    !----------------------------------------------------------------
    ! NOTE: exclude dissolved organic matter tracers
    If (ctrl_data_save_slice_diag_proxy) then
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          is = maxval(maxloc(abs(conv_DOM_POM(:,io))))-1
          if (is == 0) then
             loc_ij(:,:) = const_real_zero
             DO i=1,n_i
                DO j=1,n_j
                   loc_k1 = goldstein_k1(i,j)
                   IF (n_k >= loc_k1) THEN
                      SELECT CASE (ocn_type(io))
                      CASE (0)
                         if (io == io_T) then
                            loc_ij(i,j) = int_ocn_timeslice(io,i,j,loc_k1)/int_t_timeslice - const_zeroC
                         else
                            loc_ij(i,j) = int_ocn_timeslice(io,i,j,loc_k1)/int_t_timeslice
                         end if
                      CASE (1)
                         loc_ij(i,j) = int_ocn_timeslice(io,i,j,loc_k1)/int_t_timeslice
                      case (n_itype_min:n_itype_max)
                         loc_tot  = int_ocn_timeslice(ocn_dep(io),i,j,loc_k1)
                         loc_frac = int_ocn_timeslice(io,i,j,loc_k1)
                         loc_standard = const_standards(ocn_type(io))
                         loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                      end SELECT
                   end IF
                end DO
             end DO
             SELECT CASE (ocn_type(io))
             CASE (0)
                If (io == io_T) then
                   loc_unitsname = 'degrees C'
                else
                   loc_unitsname = 'o/oo'
                end If
             CASE (1)
                loc_unitsname = 'mol kg-1'
             case (n_itype_min:n_itype_max)
                loc_unitsname = 'o/oo'
             end SELECT
             SELECT CASE (ocn_type(io))
             CASE (0,1,n_itype_min:n_itype_max)
                call sub_adddef_netcdf(loc_iou, 3, 'ocn_ben_'//trim(string_ocn(io)), &
                     & 'bottom-water '//trim(string_ocn(io)), trim(loc_unitsname),const_real_zero,const_real_zero)
                call sub_putvar2d('ocn_ben_'//trim(string_ocn(io)),loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_sed_mask)
             end SELECT
          end if
       END DO
    end if
    !----------------------------------------------------------------
    !       save planktic-benthic difference
    !----------------------------------------------------------------
    ! NOTE: exclude dissolved organic matter tracers
    If (ctrl_data_save_slice_diag_proxy) then
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          is = maxval(maxloc(abs(conv_DOM_POM(:,io))))-1
          if (is == 0) then
             loc_ij(:,:) = const_real_zero
             loc_ij_2(:,:) = const_real_zero
             loc_ij_2(:,:) = const_real_zero
             DO i=1,n_i
                DO j=1,n_j
                   loc_k1 = goldstein_k1(i,j)
                   IF (n_k >= loc_k1) THEN
                      SELECT CASE (ocn_type(io))
                      CASE (0)
                         loc_ij_1(i,j) = int_ocn_timeslice(io,i,j,n_k)/int_t_timeslice
                         loc_ij_2(i,j) = int_ocn_timeslice(io,i,j,loc_k1)/int_t_timeslice
                      CASE (1)
                         loc_ij_1(i,j) = int_ocn_timeslice(io,i,j,n_k)/int_t_timeslice
                         loc_ij_2(i,j) = int_ocn_timeslice(io,i,j,loc_k1)/int_t_timeslice
                      case (n_itype_min:n_itype_max)
                         loc_tot  = int_ocn_timeslice(ocn_dep(io),i,j,n_k)
                         loc_frac = int_ocn_timeslice(io,i,j,n_k)
                         loc_standard = const_standards(ocn_type(io))
                         loc_ij_1(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_real_null)
                         loc_tot  = int_ocn_timeslice(ocn_dep(io),i,j,loc_k1)
                         loc_frac = int_ocn_timeslice(io,i,j,loc_k1)
                         loc_standard = const_standards(ocn_type(io))
                         loc_ij_2(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_real_null)
                      end SELECT
                   end IF
                end DO
             end DO
             loc_ij(:,:) = loc_ij_1(:,:) - loc_ij_2(:,:)
             SELECT CASE (ocn_type(io))
             CASE (0)
                If (io == io_T) loc_unitsname = 'degrees C'
                If (io == io_S) loc_unitsname = 'o/oo'
             CASE (1)
                loc_unitsname = 'mol kg-1'
             case (n_itype_min:n_itype_max)
                loc_unitsname = 'o/oo'
             end SELECT
             SELECT CASE (ocn_type(io))
             CASE (0,1,n_itype_min:n_itype_max)
                call sub_adddef_netcdf(loc_iou, 3,'ocn_D_'//trim(string_ocn(io)), &
                     & 'planktic-benthic difference '//trim(string_ocn(io)), trim(loc_unitsname),const_real_zero,const_real_zero)
                call sub_putvar2d('ocn_D_'//trim(string_ocn(io)),loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_sed_mask)
             end SELECT
          end if
       END DO
    end if
    !-----------------------------------------------------------------------
    !       save overlying Cd trace metal ratios
    !-----------------------------------------------------------------------
    If (ctrl_data_save_slice_diag_proxy) then
       loc_unitsname = 'nmol kg-1 (mmol kg-1)-1'
       IF (ocn_select(io_Cd) .AND. ocn_select(io_Ca)) THEN
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                loc_k1 = goldstein_k1(i,j)
                IF (n_k >= loc_k1) THEN
                   if (int_ocn_timeslice(io_Ca,i,j,loc_k1) > const_real_nullsmall) then
                      loc_ij(i,j) = 1.0E6*int_ocn_timeslice(io_Cd,i,j,loc_k1)/int_ocn_timeslice(io_Ca,i,j,loc_k1)
                   end if
                end IF
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_rCdtoCa','Cd:Ca trace metal ratio (ocean)', &
               & trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_rCdtoCa',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_sed_mask)
       end IF
    end if
    !----------------------------------------------------------------
    !       save overlying ocean carbonate chemistry data
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_carb .AND. (ctrl_data_save_slice_sur .OR. ctrl_data_save_slice_ocnsed)) then
       IF (opt_select(iopt_select_carbchem)) THEN
          loc_ij(:,:) = const_real_zero
          DO ic=1,n_carb
             DO i=1,n_i
                DO j=1,n_j
                   loc_k1 = goldstein_k1(i,j)
                   IF (n_k >= loc_k1) THEN
                      SELECT CASE (ic)
                      CASE (ic_ohm_cal,ic_ohm_arg)
                         loc_ij(i,j) = int_carb_timeslice(ic,i,j,loc_k1)/int_t_timeslice
                      case default
                         loc_ij(i,j) = int_carb_timeslice(ic,i,j,loc_k1)/int_t_timeslice
                      end SELECT
                   end IF
                end DO
             end DO
             SELECT CASE (ic)
             CASE (ic_ohm_cal,ic_ohm_arg)
                loc_unitsname = ' '
             case default
                loc_unitsname = 'mol kg-1'
             end SELECT
             call sub_adddef_netcdf(loc_iou,3,'carb_ben_'//trim(string_carb(ic)), &
                  & 'bottom-water '//trim(string_carb(ic)),trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar2d('carb_ben_'//trim(string_carb(ic)),loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_sed_mask)
          END DO
       end if
    end if
    !----------------------------------------------------------------
    !       particulate flux fractions
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_focnsed) then
       loc_unitsname = 'n/a'
       IF (sed_select(is_POC_frac2)) THEN
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                loc_k1 = goldstein_k1(i,j)
                IF (n_k >= loc_k1) THEN
                   loc_ij(i,j) = int_bio_settle_timeslice(is_POC_frac2,i,j,loc_k1)/real(int_t_timeslice_count)
                END if
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_focnsed_frac2_POC', &
               & 'POC fraction #2',trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_focnsed_frac2_POC',loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_sed_mask)
       end IF
       IF (sed_select(is_CaCO3_frac2)) THEN
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                loc_k1 = goldstein_k1(i,j)
                IF (n_k >= loc_k1) THEN
                   loc_ij(i,j) = int_bio_settle_timeslice(is_CaCO3_frac2,i,j,loc_k1)/real(int_t_timeslice_count)
                END if
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_focnsed_frac2_CaCO3', &
               & 'CaCO3 fraction #2',trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_focnsed_frac2_CaCO3',loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_sed_mask)
       end IF
       IF (sed_select(is_opal_frac2)) THEN
          loc_ij(:,:) = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                loc_k1 = goldstein_k1(i,j)
                IF (n_k >= loc_k1) THEN
                   loc_ij(i,j) = int_bio_settle_timeslice(is_opal_frac2,i,j,loc_k1)/real(int_t_timeslice_count)
                END if
             END DO
          END DO
          call sub_adddef_netcdf(loc_iou,3,'misc_focnsed_frac2_opal', &
               & 'opal fraction #2',trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar2d('misc_focnsed_frac2_opal',loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_sed_mask)
       end IF
    end IF
  end SUBROUTINE sub_save_netcdf_2d_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** save ocean-atmopshere flux data ***
  SUBROUTINE sub_save_netcdf_flux_seaair()
    !-----------------------------------------------------------------------
    !       local variables
    !-----------------------------------------------------------------------
    INTEGER::l,ia
    integer::loc_iou,loc_ntrec
    CHARACTER(len=255)::loc_unitsname
    real,DIMENSION(n_i,n_j)::loc_ij,loc_mask
    !-----------------------------------------------------------------------
    !       initialize local variables
    !-----------------------------------------------------------------------
    loc_iou = ncout2d_iou
    loc_ntrec = ncout2d_ntrec
    !----------------------------------------------------------------
    !                  <fseaair_*>
    !       save flux density data
    !----------------------------------------------------------------
    ! NOTE: use atmospheric grid point physics array to avoid the zero
    !       area values of dry grid points in the (ocean) physics array
    ! NOTE: a positive value of the array represents net ocean to atmosphere transfer
    loc_mask = phys_ocnatm(ipoa_mask_ocn,:,:)
    loc_ij(:,:) = 0.0
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       SELECT CASE (atm_type(ia))
       CASE (1)
          loc_ij(:,:) = (int_diag_airsea_timeslice(ia,:,:)/phys_ocnatm(ipoa_A,:,:))/int_t_timeslice
          loc_unitsname = 'mol m-2 yr-1'
          call sub_adddef_netcdf(                                                                                     &
               & loc_iou,3,'fseaair_'//trim(string_atm(ia)),trim(string_atm(ia))//                                    &
               & ': net sea->air gas exchange flux density',                                                          &
               & trim(loc_unitsname),const_real_zero,const_real_zero                                                  &
               & )
          call sub_putvar2d ('fseaair_'//trim(string_atm(ia)),loc_iou,n_i,n_j, &
               & loc_ntrec,loc_ij,loc_mask)
       end SELECT
    END DO
    !----------------------------------------------------------------
    !                  <misc_Fseaair_*>
    !       save derived flux data
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_carb .AND. ctrl_data_save_slice_diag_geochem) then
       loc_ij(:,:) = int_diag_airsea_timeslice(ia_pCO2,:,:)/int_t_timeslice
       loc_unitsname = 'mol yr-1'
       call sub_adddef_netcdf(                                                    &
            & loc_iou,3,'Fseaair_pCO2','pCO2 net sea->air gas exchange flux per grid point', &
            & trim(loc_unitsname),const_real_zero,const_real_zero                 &
            & )
       call sub_putvar2d('Fseaair_pCO2',loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask)
       loc_ij(:,:) = (int_diag_airsea_timeslice(ia_pCO2,:,:)/int_t_timeslice) * &
            & (5.0/phys_ocnatm(ipoa_dlon,:,:))*(4.0/phys_ocnatm(ipoa_dlat,:,:))
       loc_unitsname = 'mol (5x4)-1 yr-1'
       call sub_adddef_netcdf(                                                                &
            & loc_iou,3,'Fseaair_pCO2_grid','pCO2 net sea->air gas exchange flux per 5 x 4 grid', &
            & trim(loc_unitsname),const_real_zero,const_real_zero                             &
            & )
       call sub_putvar2d('Fseaair_pCO2_grid',loc_iou,n_i,n_j,loc_ntrec,loc_ij,loc_mask)
    end if
    !----------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_flux_seaair
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** save time-slice data ***
  SUBROUTINE sub_save_netcdf_3d(dum_t)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    INTEGER::l,i,j,k,io,is,ip,ic,icc,loc_iou,loc_ntrec
    integer::id
    CHARACTER(len=255)::loc_unitsname
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk,loc_mask,loc_sed_mask
    real::loc_ocn_mean_S
    real::loc_tot,loc_frac,loc_standard
    real::loc_d13C,loc_d14C
    !----------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !----------------------------------------------------------------
    loc_iou = ncout3d_iou
    loc_ntrec = ncout3d_ntrec
    loc_mask = phys_ocn(ipo_mask_ocn,:,:,:)
    !----------------------------------------------------------------
    !       SAVE OCEAN TRACER FIELD
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_ocn) then
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   SELECT CASE (ocn_type(io))
                   CASE (0)
                      if (io == io_T) then
                         loc_ijk(i,j,k) = int_ocn_timeslice(io,i,j,k)/int_t_timeslice - const_zeroC
                      else
                         loc_ijk(i,j,k) = int_ocn_timeslice(io,i,j,k)/int_t_timeslice
                      end if
                   CASE (1)
                      loc_ijk(i,j,k) = int_ocn_timeslice(io,i,j,k)/int_t_timeslice
                   case (n_itype_min:n_itype_max)
                      loc_tot  = int_ocn_timeslice(ocn_dep(io),i,j,k)/int_t_timeslice
                      loc_frac = int_ocn_timeslice(io,i,j,k)/int_t_timeslice
                      loc_standard = const_standards(ocn_type(io))
                      loc_ijk(i,j,k) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                   END SELECT
                end do
             end do
          end do
          SELECT CASE (ocn_type(io))
          CASE (0,1,n_itype_min:n_itype_max)
             call sub_adddef_netcdf(loc_iou,4,'ocn_'//trim(string_ocn_tname(l)), &
                  & trim(string_ocn_tlname(l)),trim(string_ocn_unit(l)),ocn_mima(l,1),ocn_mima(l,2))
             call sub_putvar3d_g('ocn_'//trim(string_ocn(io)),loc_iou,n_i,n_j,n_k, &
                  & loc_ntrec,loc_ijk(:,:,:),loc_mask)
          END SELECT
       END DO
       ! radiocarbon in DELTA notation
       IF (ocn_select(io_DIC_13C) .AND. ocn_select(io_DIC_14C)) THEN
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   loc_tot  = int_ocn_timeslice(io_DIC,i,j,k)/int_t_timeslice
                   loc_frac = int_ocn_timeslice(io_DIC_13C,i,j,k)/int_t_timeslice
                   loc_standard = const_standards(ocn_type(io_DIC_13C))
                   loc_d13C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                   loc_frac = int_ocn_timeslice(io_DIC_14C,i,j,k)/int_t_timeslice
                   loc_standard = const_standards(ocn_type(io_DIC_14C))
                   loc_d14C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                   loc_ijk(i,j,k) = fun_convert_delta14CtoD14C(loc_d13C,loc_d14C)
                   loc_unitsname = 'o/oo'
                end do
             end do
          end do
          call sub_adddef_netcdf(loc_iou,4,'ocn_DIC_D14C', &
               & ' oceanic D14C (big delta)',trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('ocn_DIC_D14C',loc_iou,n_i,n_j,n_k, &
               & loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end if
       ! color tracer ratios -- as an age tracer
       IF (ctrl_force_ocn_age) then
          CALL sub_save_netcdf_ocn_col_extra(dum_t)
       end IF
    end if
    !----------------------------------------------------------------
    !       SAVE SALINITY-NORMALIZED OCEAN TRACER FIELD
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_ocn .AND. ctrl_data_save_derived) then
       loc_ocn_mean_S = SUM(int_ocn_timeslice(io_S,:,:,:)*phys_ocn(ipo_M,:,:,:))/SUM(phys_ocn(ipo_M,:,:,:))
       DO l=3,n_l_ocn
          io = conv_iselected_io(l)
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   SELECT CASE (ocn_type(io))
                   CASE (0,1)
                      loc_ijk(i,j,k) = int_ocn_timeslice(io,i,j,k)* &
                           & (loc_ocn_mean_S/int_ocn_timeslice(io_S,i,j,k))/int_t_timeslice
                      loc_unitsname = 'mol kg-1'
                   END SELECT
                end DO
             end DO
          end DO
          SELECT CASE (ocn_type(io))
          CASE (0,1)
             call sub_adddef_netcdf(loc_iou,4,'ocn_'//trim(string_ocn(io))//'_Snorm', &
                  & trim(string_ocn(io))//' normalized by salinity',trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar3d_g('ocn_'//trim(string_ocn(io))//'_Snorm',loc_iou, &
                  & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
          END SELECT
       END DO
    end if
    !----------------------------------------------------------------
    !       SAVE TRACER INVENTORIES
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_ocn .AND. ctrl_data_save_derived) then
       DO l=3,n_l_ocn
          io = conv_iselected_io(l)
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   SELECT CASE (ocn_type(io))
                   CASE (1,n_itype_min:n_itype_max)
                      loc_ijk(i,j,k) = phys_ocn(ipo_M,i,j,k)*int_ocn_timeslice(io,i,j,k)/int_t_timeslice
                      loc_unitsname = 'mol'
                   END SELECT
                end DO
             end DO
          end DO
          SELECT CASE (ocn_type(io))
          CASE (1,n_itype_min:n_itype_max)
             call sub_adddef_netcdf(loc_iou,4,'ocn_'//trim(string_ocn(io))//'_tot', &
                  & trim(string_ocn(io))//' volume integrated inventory',trim(loc_unitsname),const_real_zero,const_real_zero)
             call sub_putvar3d_g('ocn_'//trim(string_ocn(io))//'_tot',loc_iou, &
                  & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
          END SELECT
       END DO
    end If
    !----------------------------------------------------------------
    !       REMINERALIZATION FIELD
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_bio .AND. ctrl_data_save_derived) then
       DO l=3,n_l_ocn
          io = conv_iselected_io(l)
          is = maxval(maxloc(abs(conv_DOM_POM(:,io))))-1
          if (is == 0) then
             loc_ijk(:,:,:) = const_real_zero
             DO i=1,n_i
                DO j=1,n_j
                   DO k=goldstein_k1(i,j),n_k
                      SELECT CASE (ocn_type(io))
                      CASE (0,1)
                         loc_ijk(i,j,k) = int_bio_remin_timeslice(io,i,j,k)/int_t_timeslice
                         loc_unitsname = 'mol yr-1'
                      case (n_itype_min:n_itype_max)
                         loc_tot  = int_bio_remin_timeslice(ocn_dep(io),i,j,k)/int_t_timeslice
                         loc_frac = int_bio_remin_timeslice(io,i,j,k)/int_t_timeslice
                         loc_standard = const_standards(ocn_type(io))
                         loc_ijk(i,j,k) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                         loc_unitsname = 'o/oo'
                      END SELECT
                   end do
                end do
             end do
             SELECT CASE (ocn_type(io))
             CASE (1,n_itype_min:n_itype_max)
                call sub_adddef_netcdf(loc_iou,4,'ocn_'//trim(string_ocn(io))//'_remin', &
                     & 'remineralization flux - '//trim(string_ocn(io)), trim(loc_unitsname),const_real_zero,const_real_zero)
                call sub_putvar3d_g('ocn_'//trim(string_ocn(io))//'_remin',loc_iou, &
                     & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
             END SELECT
          end if
       END DO
    end if
    !----------------------------------------------------------------
    !       OCEAN 'PHYSICS'
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_phys_ocn) then
       loc_ijk(:,:,:) = const_real_zero
       DO ip=1,n_phys_ocn
          loc_ijk(:,:,:) = int_phys_ocn_timeslice(ip,:,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,4,'phys_ocn_'//trim(string_phys_ocn(ip)), &
               & 'ocean physical properties - '//trim(string_phys_ocn(ip)),' ',const_real_zero,const_real_zero)
          call sub_putvar3d_g('phys_ocn_'//trim(string_phys_ocn(ip)),loc_iou, &
               & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       END DO
    end if
    !----------------------------------------------------------------
    !       CARBONATE CHEMISTRY FIELD
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_carb) then
       loc_ijk(:,:,:) = const_real_zero
       DO ic=1,n_carb
          loc_ijk(:,:,:) = int_carb_timeslice(ic,:,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,4,'carb_'//trim(string_carb(ic)), &
               & 'carbonate chemistry properties - '//trim(string_carb(ic)),' ',const_real_zero,const_real_zero)
          call sub_putvar3d_g('carb_'//trim(string_carb(ic)),loc_iou, &
               & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       END DO
    end if
    !----------------------------------------------------------------
    !       CARBONATE CHEMISTRY CONSTANTS (YAWN)
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_carbconst) then
       DO icc=1,n_carbconst
          loc_ijk(:,:,:) = const_real_zero
          loc_ijk(:,:,:) = int_carbconst_timeslice(icc,:,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,4, 'carb_const_'//trim(string_carbconst(icc)), &
               & 'carbonate chemistry dissociation constants - '//trim(string_carbconst(icc)),' ',const_real_zero,const_real_zero)
          call sub_putvar3d_g('carb_const_'//trim(string_carbconst(icc)),loc_iou, &
               & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       END DO
    end if
    !----------------------------------------------------------------
    !       PARTICULATE CONCENTRATION FIELD
    !----------------------------------------------------------------
    loc_sed_mask = loc_mask
    If (ctrl_data_save_slice_bio .AND. ctrl_data_save_derived) then
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   SELECT CASE (sed_type(is))
                   CASE (par_sed_type_bio,par_sed_type_abio, &
                        & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
                        & par_sed_type_scavenged)
                      loc_ijk(i,j,k) = int_bio_part_timeslice(is,i,j,k)/int_t_timeslice
                      loc_unitsname = 'mol kg-1'
                   case (n_itype_min:n_itype_max)
                      loc_tot  = int_bio_part_timeslice(sed_dep(is),i,j,k)/int_t_timeslice
                      loc_frac = int_bio_part_timeslice(is,i,j,k)/int_t_timeslice
                      loc_standard = const_standards(sed_type(is))
                      loc_ijk(i,j,k) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                      loc_unitsname = 'o/oo'
                   CASE (par_sed_type_frac)
                      loc_ijk(i,j,k) = int_bio_part_timeslice(is,i,j,k)
                      loc_unitsname = 'n/a'
                   END SELECT
                end do
             end do
          end do
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged,n_itype_min:n_itype_max, &
               & par_sed_type_frac)
             call sub_adddef_netcdf(loc_iou,4,'bio_part_'//trim(string_sed(is)), &
                  & 'particulate density - '//trim(string_sed(is)),loc_unitsname,const_real_zero,const_real_zero)
             call sub_putvar3d_g('bio_part_'//trim(string_sed(is)),loc_iou, &
                  & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_sed_mask)
          end SELECT
       END DO
    end if
    !----------------------------------------------------------------
    !       PARTICULATE FLUXES
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_bio) then
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_ijk(:,:,:) = const_real_zero
          !---------------------------------------------------------- flux density
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   SELECT CASE (sed_type(is))
                   CASE (par_sed_type_bio,par_sed_type_abio, &
                        & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
                        & par_sed_type_scavenged)
                      loc_ijk(i,j,k) = int_bio_settle_timeslice(is,i,j,k)*phys_ocn(ipo_rA,i,j,k)/int_t_timeslice
                      loc_unitsname = 'mol m-2 yr-1'
                   case (n_itype_min:n_itype_max)
                      loc_tot  = int_bio_settle_timeslice(sed_dep(is),i,j,k)*phys_ocn(ipo_rA,i,j,k)/int_t_timeslice
                      loc_frac = int_bio_settle_timeslice(is,i,j,k)*phys_ocn(ipo_rA,i,j,k)/int_t_timeslice
                      loc_standard = const_standards(sed_type(is))
                      loc_ijk(i,j,k) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                      loc_unitsname = 'o/oo'
                   CASE (par_sed_type_frac)
                      loc_ijk(i,j,k) = int_bio_settle_timeslice(is,i,j,k)
                      loc_unitsname = 'n/a'
                   end SELECT
                end do
             end do
          end do
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged,n_itype_min:n_itype_max, &
               & par_sed_type_frac)
             call sub_adddef_netcdf(loc_iou,4,'bio_fpart_'//trim(string_sed(is)), &
                  & 'particulate flux (density) - '//trim(string_sed(is)),loc_unitsname,const_real_zero,const_real_zero)
             call sub_putvar3d_g('bio_fpart_'//trim(string_sed(is)), loc_iou, &
                  & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_sed_mask)
          end SELECT
          !---------------------------------------------------------- total flux (per grid cell)
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   SELECT CASE (sed_type(is))
                   CASE (par_sed_type_bio,par_sed_type_abio, &
                        & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
                        & par_sed_type_scavenged)
                      loc_ijk(i,j,k) = int_bio_settle_timeslice(is,i,j,k)/int_t_timeslice
                      loc_unitsname = 'mol yr-1'
                   CASE (par_sed_type_frac)
                      loc_ijk(i,j,k) = int_bio_settle_timeslice(is,i,j,k)
                      loc_unitsname = 'n/a'
                   end SELECT
                end do
             end do
          end do
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged, &
               & par_sed_type_frac)
             call sub_adddef_netcdf(loc_iou,4,'bio_fparttot_'//trim(string_sed(is)), &
                  & 'particulate flux (total) - '//trim(string_sed(is)),loc_unitsname,const_real_zero,const_real_zero)
             call sub_putvar3d_g('bio_fparttot_'//trim(string_sed(is)), loc_iou, &
                  & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_sed_mask)
          end SELECT
          !---------------------------------------------------------- normalized flux
          DO i=1,n_i
             DO j=1,n_j
                if (int_bio_settle_timeslice(is,i,j,n_k) > 0.0) then
                   loc_ijk(i,j,1:n_k-1) = int_bio_settle_timeslice(is,i,j,1:n_k-1)/int_bio_settle_timeslice(is,i,j,n_k)
                   loc_ijk(i,j,n_k)     = 1.0
                else
                   loc_ijk(i,j,:)       = 0.0
                end if
             end do
          end do
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_det)
             call sub_adddef_netcdf(loc_iou,4,'bio_fpartnorm_'//trim(string_sed(is)), &
                  & 'export-normalized particulate flux - '//trim(string_sed(is)),'n/a',const_real_zero,const_real_zero)
             call sub_putvar3d_g('bio_fpartnorm_'//trim(string_sed(is)),loc_iou, &
                  & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_sed_mask)
          end SELECT
       END DO
    end if
    !----------------------------------------------------------------
    ! GEOCHEMICAL DIAGNOSTICS -- OLD
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_diag_geochem .AND. ctrl_data_save_slice_diag_redox_old) then
       loc_unitsname = 'mol kg-1 yr-1'
       DO id=1,n_diag_geochem
          loc_ijk(:,:,:) = int_diag_geochem_timeslice(id,:,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,4,'diag_'//trim(string_diag_geochem(id)), &
               & 'production/oxidation rate - '//trim(string_diag_geochem(id)),trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('diag_'//trim(string_diag_geochem(id)),loc_iou, &
               & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end DO
    end If
    !----------------------------------------------------------------
    ! GEOCHEMICAL DIAGNOSTICS
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_diag_geochem) then
       loc_unitsname = 'mol kg-1 yr-1'
       DO id=1,n_diag_redox
          loc_ijk(:,:,:) = int_diag_redox_timeslice(id,:,:,:)/int_t_timeslice
          call sub_adddef_netcdf(loc_iou,4,'redox_'//trim(string_diag_redox(id)), &
               & 'redox transformation rate - '//trim(string_diag_redox(id)),trim(loc_unitsname),const_real_zero,const_real_zero)
          call sub_putvar3d_g('redox_'//trim(string_diag_redox(id)),loc_iou, &
               & n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
       end DO
    end If
    !----------------------------------------------------------------
    !       OCEAN VELOCITY FIELD
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_misc) CALL sub_save_netcdf_goldstein_u()
    !----------------------------------------------------------------
    !----------------------------------------------------------------
    !       USED-DEFINED (MISC) FIELDS
    !----------------------------------------------------------------
    CALL sub_save_netcdf_3d_USER()
    !--------------------------------------------------------- !
    ! CDR-MIP
    !--------------------------------------------------------- !
    if (ctrl_data_save_slice_cdrmip) CALL sub_save_netcdf_3d_cdrmip()
    !--------------------------------------------------------- !
  END SUBROUTINE sub_save_netcdf_3d
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! TIME-SERIES 3-D FIELDS
  SUBROUTINE sub_save_netcdf_3d_sig()
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    INTEGER::l,i,j,k,loc_iou,loc_ntrec
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk,loc_mask
    real::loc_tot,loc_frac,loc_standard
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou   = ncout3dsig_iou
    loc_ntrec = ncout3dsig_ntrec
    loc_mask  = phys_ocn(ipo_mask_ocn,:,:,:)
    !----------------------------------------------------------------
    !       SAVE OCEAN TRACER FIELD
    !----------------------------------------------------------------
    If (ctrl_data_save_slice_ocn) then
       DO l=1,n_l_ocn
          loc_ijk(:,:,:) = const_real_zero
          DO i=1,n_i
             DO j=1,n_j
                DO k=goldstein_k1(i,j),n_k
                   SELECT CASE (ocn_type(l2io(l)))
                   CASE (0)
                      if (l == io2l(io_T)) then
                         loc_ijk(i,j,k) = int_misc_3D_sig(l,i,j,k)/int_t_sig - const_zeroC
                      else
                         loc_ijk(i,j,k) = int_misc_3D_sig(l,i,j,k)/int_t_sig
                      end if
                   CASE (1)
                      loc_ijk(i,j,k) = int_misc_3D_sig(l,i,j,k)/int_t_sig
                   case (n_itype_min:n_itype_max)
                      loc_tot  = int_misc_3D_sig(io2l(ocn_dep(l2io(l))),i,j,k)/int_t_sig
                      loc_frac = int_misc_3D_sig(l,i,j,k)/int_t_sig
                      loc_standard = const_standards(ocn_type(l2io(l)))
                      loc_ijk(i,j,k) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                   END SELECT
                end do
             end do
          end do
          SELECT CASE (ocn_type(l2io(l)))
          CASE (0,1,n_itype_min:n_itype_max)
             call sub_adddef_netcdf(loc_iou,4,'ocn_'//trim(string_ocn_tname(l)), &
                  & trim(string_ocn_tlname(l)),trim(string_ocn_unit(l)),ocn_mima(l,1),ocn_mima(l,2))
             call sub_putvar3d_g('ocn_'//trim(string_ocn(l2io(l))),loc_iou,n_i,n_j,n_k, &
                  & loc_ntrec,loc_ijk(:,:,:),loc_mask)
          END SELECT
       END DO
    end if
    !----------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_3d_sig
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** save streamfunction data ***
  SUBROUTINE sub_save_netcdf_goldstein_opsi()
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    INTEGER::loc_iou, loc_ntrec
    REAL::loc_scale
    real,DIMENSION(0:n_j,0:n_k)::loc_mask,loc_tmp_jk
    real,DIMENSION(1:n_i,0:n_j)::loc_mask_surf,loc_tmp_ij
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou = ncout2d_iou
    loc_ntrec = ncout2d_ntrec
    loc_scale = goldstein_dsc*goldstein_usc*const_rEarth*1.0E-6
    !-----------------------------------------------------------------------
    !       WRITE MOC
    !-----------------------------------------------------------------------
    ! NOTE: flip the data in the vertical to match the axes ...
    ! global
    loc_tmp_jk(:,:) = loc_scale*int_opsi_timeslice(:,:)/int_t_timeslice
    loc_tmp_jk(:,n_k:0:-1) = loc_tmp_jk(:,0:n_k:1)
    loc_mask = const_real_one
    where(abs(loc_tmp_jk) < const_real_nullsmall)
       loc_mask = const_real_zero
    endwhere
    call sub_adddef_netcdf_moc(loc_iou,'phys_opsi','Global streamfunction','Sv',const_real_zero,const_real_zero)
    call sub_putvar2d('phys_opsi',loc_iou,n_j+1,n_k+1,loc_ntrec,loc_tmp_jk,loc_mask)
    ! Atlantic & Pacific -- modern topos only
    select case (fname_topo)
    case ('worbe2', 'worjh2', 'worjh4', 'worlg2', 'worlg4', 'wv2jh2', 'wv3jh2', 'worri4')
       ! Atlantic
       loc_tmp_jk(:,:) = loc_scale*int_opsia_timeslice(:,:)/int_t_timeslice
       loc_tmp_jk(:,n_k:0:-1) = loc_tmp_jk(:,0:n_k:1)
       loc_mask = const_real_one
       where(abs(loc_tmp_jk) < const_real_nullsmall)
          loc_mask = const_real_zero
       endwhere
       call sub_adddef_netcdf_moc(loc_iou,'phys_opsia','Atlantic streamfunction','Sv',const_real_zero,const_real_zero)
       call sub_putvar2d('phys_opsia',loc_iou,n_j+1,n_k+1,loc_ntrec,loc_tmp_jk,loc_mask)
       ! Pacific
       loc_tmp_jk(:,:) = loc_scale*int_opsip_timeslice(:,:)/int_t_timeslice
       loc_tmp_jk(:,n_k:0:-1) = loc_tmp_jk(:,0:n_k:1)
       loc_mask = const_real_one
       where(abs(loc_tmp_jk) < const_real_nullsmall)
          loc_mask = const_real_zero
       endwhere
       call sub_adddef_netcdf_moc(loc_iou,'phys_opsip','Pacific streamfunction','Sv',const_real_zero,const_real_zero)
       call sub_putvar2d('phys_opsip',loc_iou,n_j+1,n_k+1,loc_ntrec,loc_tmp_jk,loc_mask)
    end select
    !-----------------------------------------------------------------------
    !       WRITE PSI
    !-----------------------------------------------------------------------
    loc_tmp_ij(:,:)    = int_psi_timeslice(1:n_i,0:n_j)/int_t_timeslice
    loc_mask_surf(:,:) = const_real_one
    call sub_adddef_netcdf_psi(loc_iou,'phys_psi','Barotropic streamfunction','Sv',const_real_zero,const_real_zero)
    call sub_putvar2d('phys_psi',loc_iou,n_i,n_j+1,loc_ntrec,loc_tmp_ij,loc_mask_surf)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_goldstein_opsi
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** save velocity field data ***
  SUBROUTINE sub_save_netcdf_goldstein_u()
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk,loc_mask
    real::loc_c100
    INTEGER::loc_iou,loc_ntrec
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou = ncout3d_iou
    loc_ntrec = ncout3d_ntrec
    loc_c100 = 100.0
    !-----------------------------------------------------------------------
    !       WRITE OCEAN CIRCULATION FIELDS
    !-----------------------------------------------------------------------
    ! NOTE: scale to give velocity components in units of (m s-1);
    !       for the horizontal velocity components, the scale factor is usc (= 0.05) [Edwards and Shepherd, 2002]
    !       for the vertical velocity component, the overall scale factor is usc*dsc/rsc
    !       (= 0.05*5000.0/6.36e6) [Edwards and Shepherd, 2002]
    loc_mask = phys_ocn(ipo_mask_ocn,:,:,:)
    !
    loc_ijk(:,:,:) = goldstein_usc*int_u_timeslice(1,:,:,:)/int_t_timeslice
    call sub_adddef_netcdf(loc_iou,4,'phys_u','ocean velocity - u','m/s',-loc_c100,loc_c100)
    call sub_putvar3d_g('phys_u',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
    !
    loc_ijk(:,:,:) = goldstein_usc*int_u_timeslice(2,:,:,:)/int_t_timeslice
    call sub_adddef_netcdf(loc_iou,4,'phys_v','ocean velocity - v', 'm/s',-loc_c100,loc_c100)
    call sub_putvar3d_g('phys_v',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
    !
    loc_ijk(:,:,:) = (goldstein_usc*goldstein_dsc/const_rEarth)*int_u_timeslice(3,:,:,:)/int_t_timeslice
    call sub_adddef_netcdf(loc_iou,4,'phys_w','ocean velocity - w', 'm/s',-loc_c100,loc_c100)
    call sub_putvar3d_g('phys_w',loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_goldstein_u
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE DERIVED COLOR TRACER FIELDS
  SUBROUTINE sub_save_netcdf_ocn_col_extra(dum_t)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    ! local variables
    INTEGER::i,j,k
    INTEGER::loc_iou,loc_ntrec
    REAL,DIMENSION(n_i,n_j,n_k)::loc_colage,loc_mask
    REAL,DIMENSION(n_i,n_j,n_k)::loc_colbminusr,loc_colboverr
    REAL,DIMENSION(n_i,n_j,n_k)::loc_colroverrplusb,loc_colboverrplusb
    real::loc_c0
    !-----------------------------------------------------------------------
    !       SET LOCAL
    !-----------------------------------------------------------------------
    !----------------------------------------------------------------------- ! local constants
    loc_iou = ncout3d_iou
    loc_ntrec = ncout3d_ntrec
    loc_c0 = 0.0
    !----------------------------------------------------------------------- ! initialize arrays
    loc_colage         = 0.0
    loc_colbminusr     = 0.0
    loc_colboverr      = 0.0
    loc_colroverrplusb = 0.0
    loc_colboverrplusb = 0.0
    loc_mask           = phys_ocn(ipo_mask_ocn,:,:,:)
    !-----------------------------------------------------------------------
    !       CALCULATE DERIVED COLOR TRACER PROPERTIES
    !-----------------------------------------------------------------------
    DO i=1,n_i
       DO j=1,n_j
          DO k=1,n_k
             IF (k >= goldstein_k1(i,j)) THEN
                loc_colbminusr(i,j,k) = int_ocn_timeslice(io_colb,i,j,k) - int_ocn_timeslice(io_colr,i,j,k)
                IF(int_ocn_timeslice(io_colr,i,j,k) > const_real_nullsmall) THEN
                   loc_colboverr(i,j,k) = int_ocn_timeslice(io_colb,i,j,k)/int_ocn_timeslice(io_colr,i,j,k)
                   loc_colage(i,j,k) = int_ocn_timeslice(io_colb,i,j,k)/int_ocn_timeslice(io_colr,i,j,k)
                ENDIF
                IF((int_ocn_timeslice(io_colr,i,j,k) + int_ocn_timeslice(io_colb,i,j,k)) > const_real_nullsmall) THEN
                   loc_colroverrplusb(i,j,k) = &
                        & int_ocn_timeslice(io_colr,i,j,k)/(int_ocn_timeslice(io_colr,i,j,k) + int_ocn_timeslice(io_colb,i,j,k))
                   loc_colboverrplusb(i,j,k) = &
                        & int_ocn_timeslice(io_colb,i,j,k)/(int_ocn_timeslice(io_colr,i,j,k) + int_ocn_timeslice(io_colb,i,j,k))
                ENDIF
             ENDIF
          END DO
       END DO
    END DO
    !-----------------------------------------------------------------------
    ! WRITE DATA
    !-----------------------------------------------------------------------
    if (ctrl_force_ocn_age) then
       call sub_adddef_netcdf(loc_iou,4,'misc_col_age','color tracers; total age','(yrs)',loc_c0,loc_c0)
       call sub_putvar3d_g('misc_col_age',loc_iou,n_i,n_j,n_k,loc_ntrec, &
            & loc_colage(:,:,:),loc_mask)
       call sub_adddef_netcdf(loc_iou,4,'misc_col_Dage','color tracers; ventilation age','(yrs)',loc_c0,loc_c0)
       call sub_putvar3d_g('misc_col_Dage',loc_iou,n_i,n_j,n_k,loc_ntrec, &
            & loc_colage(:,:,:)-dum_t,loc_mask)
    else
       call sub_adddef_netcdf(loc_iou,4,'misc_bMINUSr','color tracers; [b] minus [r]','mol kg-1',loc_c0,loc_c0)
       call sub_putvar3d_g ('misc_bMINUSr',loc_iou,n_i,n_j,n_k,loc_ntrec, &
            & loc_colbminusr(:,:,:)/int_t_timeslice,loc_mask)
       call sub_adddef_netcdf(loc_iou,4,'misc_bOVERr','color tracers; [b] / [r]','n/a (ratio)',loc_c0,loc_c0)
       call sub_putvar3d_g('misc_bOVERr',loc_iou,n_i,n_j,n_k,loc_ntrec, &
            & loc_colboverr(:,:,:),loc_mask)
       call sub_adddef_netcdf(loc_iou,4,'misc_rOVERrPLUSb','color tracers; [r] / ([r] + [b])','n/a (ratio)',loc_c0,loc_c0)
       call sub_putvar3d_g('misc_rOVERrPLUSb',loc_iou,n_i,n_j,n_k,loc_ntrec, &
            & loc_colroverrplusb(:,:,:),loc_mask)
       call sub_adddef_netcdf(loc_iou,4,'misc_bOVERrPLUSb','color tracers; [b] / ([r] + [b])','n/a (ratio)',loc_c0,loc_c0)
       call sub_putvar3d_g('misc_bOVERrPLUSb',loc_iou,n_i,n_j,n_k,loc_ntrec, &
            & loc_colboverrplusb(:,:,:),loc_mask)
    endif
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_ocn_col_extra
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** save run-time data ***
  SUBROUTINE sub_save_netcdf_runtime(dum_t)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    ! local variables
    INTEGER :: l,io,ia,is,ic
    REAL :: loc_t
    real :: loc_opsi_scale
    real :: loc_ocn_tot_M, loc_ocn_tot_A
    real :: loc_sig
    real :: loc_tot, loc_frac, loc_standard
    real :: loc_d13C, loc_d14C
    real :: loc_c0, loc_c1, loc_cr1e15, loc_cr1e18
    integer        :: loc_ntrec, loc_iou, loc_id_time, loc_lid
    character(120) :: loc_title, loc_timunit

    loc_c0 = 0.
    loc_c1 = 1.
    loc_cr1e15 = 1.e-15
    loc_cr1e18 = 1.e-18

    ! *** set-up local constants ***
    ! calculate local opsi conversion constant
    loc_opsi_scale = goldstein_dsc*goldstein_usc*const_rEarth*1.0E-6
    ! translate internal (BioGeM) model time to user-defined timescale
    IF (ctrl_misc_t_BP) THEN
       loc_t = dum_t + par_misc_t_end
    ELSE
       loc_t = par_misc_t_end - dum_t
    END IF
    ! total ocean mass
    loc_ocn_tot_M = sum(phys_ocn(ipo_M,:,:,:))
    ! ocean surface area
    loc_ocn_tot_A = sum(phys_ocn(ipo_A,:,:,n_k))

    !-----------------------------------------------------------------------
    !     open file and get latest record number
    !-----------------------------------------------------------------------

    call sub_opennext (string_nctsint, dum_t, 0, loc_ntrec, loc_iou)

    if ( loc_ntrec .le. 1 ) then

       !-----------------------------------------------------------------------
       !       start definitions
       !-----------------------------------------------------------------------
       call sub_redef (loc_iou)

       !-----------------------------------------------------------------------
       !       set global attributes
       !-----------------------------------------------------------------------
       loc_title = 'Time Averaged Integrals'
       write (loc_timunit,'(a,F12.2)') 'equal_month_year since 0000-01-01 00:00:00'
       call sub_putglobal (loc_iou, string_nctsi, loc_title, string_ncrunid, loc_timunit)

       !-----------------------------------------------------------------------
       !       define dimensions
       !-----------------------------------------------------------------------
       call sub_defdim ('time', loc_iou, 0, loc_id_time)
       loc_lid = loc_id_time

       !-----------------------------------------------------------------------
       !       define 1d data (t)
       !-----------------------------------------------------------------------
       call sub_defvar_scalar('time',loc_iou,loc_lid,loc_c0,loc_c0,'T','D','Year','time',trim(loc_timunit))
       call sub_defvar_scalar('year',loc_iou,loc_lid,loc_c0,loc_c0,' ','F','year',' ',' ')
       call sub_defvar_scalar('month',loc_iou,loc_lid,loc_c0,loc_c0,' ','F','month',' ',' ')
       call sub_defvar_scalar('day',loc_iou,loc_lid,loc_c0,loc_c0,' ','F','day',' ',' ')
       call sub_defvar_scalar('hour',loc_iou,loc_lid,loc_c0,loc_c0,' ','F','hour',' ',' ')
       call sub_defvar_scalar('minute',loc_iou,loc_lid,loc_c0,loc_c0,' ','F','minute',' ',' ')
       call sub_defvar_scalar('second',loc_iou,loc_lid,loc_c0,loc_c0,' ','F','second',' ',' ')

       !-----------------------------------------------------------------------
       !       end definitions
       !-----------------------------------------------------------------------
       call sub_enddef (loc_iou)
       if (loc_ntrec .eq. 0) loc_ntrec = 1

    endif
    !-----------------------------------------------------------------------
    !     write 1d data (t)
    !-----------------------------------------------------------------------
    call sub_putvars ('time', loc_iou, loc_ntrec, dum_t, loc_c1, loc_c0)

    ! ------------------------------------------------------------------
    !                  <sig_ocn_*>
    ! save ocean tracer data
    ! ------------------------------------------------------------------
    !
    ! NOTE: write data both as the total inventory, and as the
    !       equivalent mean concentration
    IF (ctrl_data_save_sig_ocn) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          SELECT CASE (ocn_type(io))
          CASE (0,1)
             loc_sig = int_ocn_sig(io)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_ocn_tname(l))//'_tot', &
                  & trim(string_ocn_tlname(l))//'_tot', string_ocn_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_ocn_tname(l))//'_tot', loc_iou, loc_ntrec, &
                  & loc_ocn_tot_M*loc_sig*loc_cr1e15, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_ocn_tname(l))//'_conc', &
                  & trim(string_ocn_tlname(l))//'_conc', string_ocn_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_ocn_tname(l))//'_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)

          case (n_itype_min:n_itype_max)
             loc_tot  = int_ocn_sig(ocn_dep(io))/int_t_sig
             loc_frac = int_ocn_sig(io)/int_t_sig
             loc_standard = const_standards(ocn_type(io))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_ocn_tname(l))//'_tot', &
                  & trim(string_ocn_tlname(l))//'_tot', string_ocn_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_ocn_tname(l))//'_tot', loc_iou, loc_ntrec, &
                  & loc_ocn_tot_M*loc_sig*loc_cr1e18, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_ocn_tname(l))//'_conc', &
                  & trim(string_ocn_tlname(l))//'_conc', string_ocn_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_ocn_tname(l))//'_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)

          END SELECT
       END DO
    END IF

    ! ------------------------------------------------------------------
    !                  <sig_ocn_sur_*>
    ! save ocean  surface tracer data
    ! ------------------------------------------------------------------

    IF (ctrl_data_save_sig_ocn_sur) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          SELECT CASE (ocn_type(io))
          CASE (0,1)
             loc_sig = int_ocn_sur_sig(io)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_ocn_tname(l))//'_ocn_sur_tot', &
                  & trim(string_ocn_tlname(l))//'_ocn_sur_tot', string_ocn_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_ocn_tname(l))//'_ocn_sur_tot', loc_iou, loc_ntrec, &
                  & SUM(phys_ocn(ipo_M,:,:,n_k))*loc_sig*loc_cr1e15, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_ocn_tname(l))//'_onc_sur_conc', &
                  & trim(string_ocn_tlname(l))//'_onc_sur_conc', string_ocn_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_ocn_tname(l))//'_onc_sur_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)

          case (n_itype_min:n_itype_max)
             loc_tot  = int_ocn_sur_sig(ocn_dep(io))/int_t_sig
             loc_frac = int_ocn_sur_sig(io)/int_t_sig
             loc_standard = const_standards(ocn_type(io))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_ocn_tname(l))//'_ocn_sur_tot', &
                  & trim(string_ocn_tlname(l))//'_ocn_sur_tot', string_ocn_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_ocn_tname(l))//'_ocn_sur_tot', loc_iou, loc_ntrec, &
                  & SUM(phys_ocn(ipo_M,:,:,n_k))*loc_sig*loc_cr1e15, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_ocn_tname(l))//'_ocn_sur_conc', &
                  & trim(string_ocn_tlname(l))//'_ocn_sur_conc', string_ocn_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_ocn_tname(l))//'_ocn_sur_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)

          END SELECT
       END DO
    end if

    ! ------------------------------------------------------------------
    !                  <sig_carb_sur_*>
    ! save ocean surface carbonate chemistry data
    ! ------------------------------------------------------------------

    IF (ctrl_data_save_sig_carb_sur) THEN
       DO ic=1,n_carb
          loc_sig = int_carb_sur_sig(ic)/int_t_sig
          SELECT CASE (ic)
          CASE (ic_H)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(ic))//'_carb_sur_tot', &
                  & trim(string_sed_tlname(ic))//'_carb_sur_tot', string_sed_unit(ic), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(ic))//'_carb_sur_tot', loc_iou, loc_ntrec, &
                  & -log10(loc_sig)*loc_cr1e15, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(ic))//'_carb_sur_conc', &
                  & trim(string_sed_tlname(ic))//'_carb_sur_conc', string_sed_unit(ic), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(ic))//'_carb_sur_conc', loc_iou,loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)

          case default
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(ic))//'_carb_sur_conc', &
                  & trim(string_sed_tlname(ic))//'_carb_sur_conc', string_sed_unit(ic), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(ic))//'_carb_sur_conc', loc_iou,loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          end SELECT
       END DO
    end if

    ! ------------------------------------------------------------------
    !                  <sig_ocnatm_*>
    ! save atmosphere tracer data
    ! ------------------------------------------------------------------

    ! NOTE: write data both as the total inventory, and as the equivalent
    !       mean partial pressure simple conversion factor from atm to mol is used
    IF (ctrl_data_save_sig_ocnatm) THEN
       DO l=3,n_l_atm
          ia = conv_iselected_ia(l)
          SELECT CASE (atm_type(ia))
          CASE (1)
             loc_sig = int_ocnatm_sig(ia)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_atm_tname(l))//'_ocnatm_tot', &
                  & trim(string_atm_tlname(l))//'_ocnatm_tot', string_atm_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_atm_tname(l))//'_ocnatm_tot', loc_iou, loc_ntrec, &
                  & conv_atm_mol*loc_sig*loc_cr1e15, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_atm_tname(l))//'_ocnatm_conc', &
                  & trim(string_atm_tlname(l))//'_ocnatm_conc', string_atm_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_atm_tname(l))//'_ocnatm_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_ocnatm_sig(atm_dep(ia))/int_t_sig
             loc_frac = int_ocnatm_sig(ia)/int_t_sig
             loc_standard = const_standards(atm_type(ia))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_atm_tname(l))//'_ocnatm_tot', &
                  & trim(string_atm_tlname(l))//'_ocnatm_tot', string_atm_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_atm_tname(l))//'_ocnatm_tot', loc_iou, loc_ntrec, &
                  & conv_atm_mol*loc_frac, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_atm_tname(l))//'_ocnatm_conc', &
                  & trim(string_atm_tlname(l))//'_ocnatm_conc', string_atm_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_atm_tname(l))//'_ocnatm_conc', loc_iou, &
                  & loc_ntrec, loc_sig, loc_c1, loc_c0)

          end SELECT
       END DO
    END IF

    ! ------------------------------------------------------------------
    !                  <sig_fexport_*>
    ! save export flux data
    ! ------------------------------------------------------------------

    ! NOTE: write data both as mole and mass flux
    IF (ctrl_data_save_sig_fexport) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_sig = int_fexport_sig(is)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fexport_tot', &
                  & trim(string_sed_tlname(l))//'_fexport_tot', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fexport_tot', loc_iou, loc_ntrec, &
                  & loc_sig/loc_ocn_tot_A, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fexport_conc', &
                  & trim(string_sed_tlname(l))//'_fexport_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fexport_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          CASE (par_sed_type_age)
             if (int_fexport_sig(sed_dep(is)) > const_real_nullsmall) then
                loc_sig = int_fexport_sig(is)/int_t_sig
             else
                loc_sig = 0.0
             endif
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fexport_conc', &
                  & trim(string_sed_tlname(l))//'_fexport_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fexport_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_fexport_sig(sed_dep(is))/int_t_sig
             loc_frac = int_fexport_sig(is)/int_t_sig
             loc_standard = const_standards(sed_type(is))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fexport_tot', &
                  & trim(string_sed_tlname(l))//'_fexport_tot', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fexport_tot', loc_iou, loc_ntrec, &
                  & loc_frac, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fexport_conc', &
                  & trim(string_sed_tlname(l))//'_fexport_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fexport_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)

          end SELECT
       END DO
    END IF

    ! ------------------------------------------------------------------
    !                  <sig_focnatm_*>
    ! save ocean-atmopshere flux data
    ! ------------------------------------------------------------------

    ! NOTE: write data both as the total flux, and as the equivalent mean
    !       flux density
    IF (ctrl_data_save_sig_focnatm) THEN
       DO l=3,n_l_atm
          ia = conv_iselected_ia(l)
          SELECT CASE (atm_type(ia))
          CASE (1)
             loc_sig = int_focnatm_sig(ia)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_atm_tname(l))//'_focnatm_tot', &
                  & trim(string_atm_tlname(l))//'_focnatm_tot', string_atm_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_atm_tname(l))//'_focnatm_tot', loc_iou, loc_ntrec, &
                  & loc_sig*loc_cr1e15/loc_ocn_tot_A, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_atm_tname(l))//'_focnatm_conc', &
                  & trim(string_atm_tlname(l))//'_focnatm_conc', string_atm_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_atm_tname(l))//'_focnatm_conc', loc_iou, loc_ntrec, &
                  & loc_sig*loc_cr1e15, loc_c1, loc_c0)
          end SELECT
       END DO
    END IF

    ! ------------------------------------------------------------------
    !                  <sig_focnsed_*>
    ! save ocean-sediment flux data
    ! ------------------------------------------------------------------

    ! NOTE: write data both as the total flux, and as the equivalent mean
    !       flux density the surface ocean area is used as a proxy for the
    !       ocean bottom area
    IF (ctrl_data_save_sig_focnsed) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_sig = int_focnsed_sig(is)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_focnsed_tot', &
                  & trim(string_sed_tlname(l))//'_focnsed_tot', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_focnsed_tot', loc_iou, loc_ntrec, &
                  & loc_sig/loc_ocn_tot_A, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_focnsed_conc', &
                  & trim(string_sed_tlname(l))//'_focnsed_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_focnsed_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          CASE (par_sed_type_age)
             if (int_focnsed_sig(sed_dep(is)) > const_real_nullsmall) then
                loc_sig = int_focnsed_sig(is)/int_t_sig
             else
                loc_sig = 0.0
             end if
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_focnsed_conc', &
                  & trim(string_sed_tlname(l))//'_focnsed_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_focnsed_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_focnsed_sig(sed_dep(is))/int_t_sig
             loc_frac = int_focnsed_sig(is)/int_t_sig
             loc_standard = const_standards(sed_type(is))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_focnsed_tot', &
                  & trim(string_sed_tlname(l))//'_focnsed_tot', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_focnsed_tot', loc_iou, loc_ntrec, &
                  & loc_frac, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_focnsed_conc', &
                  & trim(string_sed_tlname(l))//'_focnsed_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_focnsed_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)

          end SELECT
       END DO
    end IF

    ! ------------------------------------------------------------------
    !                  <sig_fsedocn_*>
    ! save sediment->ocean flux data
    ! ------------------------------------------------------------------

    ! NOTE: write data both as the total flux, and as the equivalent mean
    !       flux density the surface ocean area is used as a proxy for the
    !       ocean bottom area
    IF (ctrl_data_save_sig_fsedocn) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          SELECT CASE (ocn_type(io))
          CASE (1)
             loc_sig = int_fsedocn_sig(io)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fsedocn_tot', &
                  & trim(string_sed_tlname(l))//'_fsedocn_tot', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fsedocn_tot', loc_iou, loc_ntrec, &
                  & loc_sig/loc_ocn_tot_A, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fsedocn_conc', &
                  & trim(string_sed_tlname(l))//'_fsedocn_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fsedocn_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_fsedocn_sig(ocn_dep(io))/int_t_sig
             loc_frac = int_fsedocn_sig(io)/int_t_sig
             loc_standard = const_standards(ocn_type(io))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fsedocn_tot', &
                  & trim(string_sed_tlname(l))//'_fsedocn_tot', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fsedocn_tot', loc_iou, loc_ntrec, &
                  & loc_frac, loc_c1, loc_c0)
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_fsedocn_conc', &
                  & trim(string_sed_tlname(l))//'_fsedocn_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_fsedocn_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)

          end SELECT
       END DO
    END IF

    ! ------------------------------------------------------------------
    !                  <sig_ocnsed_*>
    ! save sediment (core-top) composition data
    ! ------------------------------------------------------------------

    ! NOTE: the data placed on the sediment composition interface array
    !       has already had the necessary type conversions made
    !       (i.e., isotopes in per mill, solid tracers as mass (or volume) fraction, etc)
    IF (ctrl_data_save_sig_ocnsed) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_sig = int_ocnsed_sig(is)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_ocnsed_conc', &
                  & trim(string_sed_tlname(l))//'_ocnsed_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_ocnsed_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          CASE (par_sed_type_age,n_itype_min:n_itype_max)
             loc_sig = int_ocnsed_sig(is)/int_t_sig
             call sub_adddef_netcdf (loc_iou, 1, trim(string_sed_tname(l))//'_ocnsed_conc', &
                  & trim(string_sed_tlname(l))//'_ocnsed_conc', string_sed_unit(l), loc_c1, loc_c0)
             call sub_putvars ( trim(string_sed_tname(l))//'_ocnsed_conc', loc_iou, loc_ntrec, &
                  & loc_sig, loc_c1, loc_c0)
          end SELECT
       END DO
    END IF

    ! ------------------------------------------------------------------
    !                  <sig_misc_*>
    ! save miscellaneous data (if requested)
    ! ------------------------------------------------------------------

    IF (ctrl_data_save_sig_misc) THEN

       call sub_adddef_netcdf (loc_iou, 1, 'THC_min', 'min of thermohaline circulation', 'Sv', loc_c1, loc_c0)
       call sub_putvars ( 'THC_min', loc_iou, loc_ntrec, &
            & loc_opsi_scale*int_misc_opsia_min_sig/int_t_sig, loc_c1, loc_c0)
       call sub_adddef_netcdf (loc_iou, 1, 'THC_max', 'max of thermohaline circulation', 'Sv', loc_c1, loc_c0)
       call sub_putvars ( 'THC_max', loc_iou, loc_ntrec, &
            & loc_opsi_scale*int_misc_opsia_max_sig/int_t_sig, loc_c1, loc_c0)

       ! atmospheric CO2 D14C
       IF (atm_select(ia_pCO2_14C)) THEN
          loc_tot  = int_ocnatm_sig(atm_dep(ia_pCO2))/int_t_sig
          loc_frac = int_ocnatm_sig(ia_pCO2_13C)/int_t_sig
          loc_standard = const_standards(atm_type(ia_pCO2_13C))
          loc_d13C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
          loc_frac = int_ocnatm_sig(ia_pCO2_14C)/int_t_sig
          loc_standard = const_standards(atm_type(ia_pCO2_14C))
          loc_d14C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
          loc_sig = fun_convert_delta14CtoD14C(loc_d13C,loc_d14C)
          call sub_adddef_netcdf (loc_iou, 1, 'pCO2_14C_atm', 'pCO2_14C_atm', '1', loc_c1, loc_c0)
          call sub_putvars ( 'pCO2_14C_atm', loc_iou, loc_ntrec, &
               & loc_sig, loc_c1, loc_c0)
       end IF
       IF (ctrl_data_save_sig_carb_sur) THEN
          call sub_adddef_netcdf (loc_iou, 1, 'int_carb_sur', 'int_carb_sur', '1', loc_c1, loc_c0)
          call sub_putvars ( 'int_carb_sur', loc_iou, loc_ntrec, &
               & -log10(int_carb_sur_sig(ic_H)/int_t_sig), loc_c1, loc_c0)
       end IF
    end if

    call sub_closefile (loc_iou)

  END SUBROUTINE sub_save_netcdf_runtime
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WRITE RUN-TIME DIAGNOSTICS
  SUBROUTINE sub_save_netcdf_tsi(dum_ntrec, dum_yr, dum_ocn_tot_M, dum_ocn_tot_A, &
       & dum_ocnatm_tot_A, dum_opsi_scale, dum_opsia, dum_sfcatm1)
    ! dummy arguments
    REAL,    INTENT(in)::dum_yr
    REAL,    INTENT(in)::dum_ocn_tot_M
    REAL,    INTENT(in)::dum_ocn_tot_A
    REAL,    INTENT(in)::dum_ocnatm_tot_A
    REAL,    INTENT(in)::dum_opsi_scale
    INTEGER,INTENT(OUT)::dum_ntrec
    REAL,DIMENSION(0:n_j,0:n_k),INTENT(in)::dum_opsia
    REAL,DIMENSION(0:n_atm,n_i,n_j),INTENT(in)::dum_sfcatm1
    ! local variables
    real           :: loc_tot, loc_frac, loc_standard, loc_tmp
    logical        :: loc_defined
    character(255) :: loc_title, loc_timunit
    integer        :: loc_iou, loc_id_time, loc_lid
    real           :: loc_c0, loc_c1, loc_c100

    loc_c0 = 0.
    loc_c1 = 1.
    loc_c100 = 100.

    loc_tot  = SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia_pCO2,:,:))/dum_ocnatm_tot_A
    loc_frac = SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia_pCO2_13C,:,:))/dum_ocnatm_tot_A
    loc_standard = const_standards(atm_type(ia_pCO2_13C))
    if (loc_frac < const_real_nullsmall) then
       loc_frac = fun_calc_isotope_fraction(0.0,loc_standard)*loc_tot
    end if
    !-----------------------------------------------------------------------
    !     open file and get latest record number
    !-----------------------------------------------------------------------
    loc_defined = .true.
    if (dum_yr .eq. 1) then
       loc_defined = .false.
       dum_ntrec = 0
    end if
    call sub_opennext (string_nctsi, dum_yr, 0, dum_ntrec, loc_iou)

    if ( dum_ntrec .eq. 1 ) then

       !-----------------------------------------------------------------------
       !       start definitions
       !-----------------------------------------------------------------------
       call sub_redef (loc_iou)

       !-----------------------------------------------------------------------
       !       set global attributes
       !-----------------------------------------------------------------------
       loc_title = 'Run-time diagnostics'
       write (loc_timunit,'(a,F12.2)') 'equal_month_year since 0000-01-01 00:00:00'
       call sub_putglobal (loc_iou, string_nctsi, loc_title, string_ncrunid, loc_timunit)

       !-----------------------------------------------------------------------
       !       define dimensions
       !-----------------------------------------------------------------------
       call sub_defdim ('time', loc_iou, 0, loc_id_time)
       loc_lid = loc_id_time

       !-----------------------------------------------------------------------
       !       define 1d data (t)
       !-----------------------------------------------------------------------
       call sub_defvar_scalar('time',loc_iou,loc_lid,loc_c0,loc_c0,'T','D','Year','time',trim(loc_timunit))
       call sub_defvar_scalar('pCO2',loc_iou,loc_lid,loc_c0,loc_c100,' ','F','Atmospheric pCO2',' ','uatm')
       call sub_defvar_scalar('d13CO2',loc_iou,loc_lid,-loc_c100,loc_c100,' ','F','Atmospheric c13C',' ','o/oo')
       call sub_defvar_scalar('AMO',loc_iou,loc_lid,loc_c0,loc_c100,' ','F','Atlantic meridional overturning',' ','Sv')
       call sub_defvar_scalar('sea_ice_cover',loc_iou,loc_lid,-loc_c1,loc_c100,' ','F','Global sea ice cover',' ','%')
       call sub_defvar_scalar('_surT',loc_iou,loc_lid,loc_c0,loc_c100,' ','F','surface temperature',' ','degrees C')
       call sub_defvar_scalar('_surS',loc_iou,loc_lid,loc_c0,loc_c100,' ','F','surface salinity',' ','PSU')
       call sub_defvar_scalar('dic',loc_iou,loc_lid,loc_c0,loc_c100,' ','F','Global DIC',' ','umol kg-1')
       call sub_defvar_scalar('alk',loc_iou,loc_lid,loc_c0,loc_c100,' ','F','Global ALK',' ','umol kg-1')
       call sub_defvar_scalar('_surWc',loc_iou,loc_lid,loc_c0,loc_c100,' ','F','surface saturation (calcite)',' ','n/a')
       call sub_defvar_scalar('_surWa',loc_iou,loc_lid,loc_c0,loc_c100,' ','F','surface saturation (aragonite)',' ','n/a')
       !-----------------------------------------------------------------------
       !       end definitions
       !-----------------------------------------------------------------------
       call sub_enddef (loc_iou)
       if (dum_ntrec .eq. 0) dum_ntrec = 1

    endif

    !-----------------------------------------------------------------------
    !     write 1d data (t)
    !-----------------------------------------------------------------------
    call sub_putvars ('time', loc_iou, dum_ntrec, dum_yr, loc_c1, loc_c0)
    loc_tmp = conv_mol_umol*SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia_pCO2,:,:))/dum_ocnatm_tot_A
    call sub_putvars ('pCO2', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
    call sub_putvars ('d13CO2', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = dum_opsi_scale*maxval(dum_opsia(:,:))
    call sub_putvars ('AMO', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = 100.0*(loc_c1/SUM(phys_ocn(ipo_A,:,:,n_k)))* &
         &SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:))
    call sub_putvars ('sea_ice_cover', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = SUM((loc_c1 - phys_ocnatm(ipoa_seaice,:,:))* &
         &phys_ocn(ipo_A,:,:,n_k)*ocn(io_T,:,:,n_k))/dum_ocn_tot_A - const_zeroC
    call sub_putvars ('_surT', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = SUM((loc_c1 - phys_ocnatm(ipoa_seaice,:,:))* &
         &phys_ocn(ipo_A,:,:,n_k)*ocn(io_S,:,:,n_k))/dum_ocn_tot_A
    call sub_putvars ('_surS', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_DIC,:,:,:))/dum_ocn_tot_M
    call sub_putvars ('DIC', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_ALK,:,:,:))/dum_ocn_tot_M
    call sub_putvars ('ALK', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = SUM((loc_c1 - phys_ocnatm(ipoa_seaice,:,:))* &
         &phys_ocn(ipo_A,:,:,n_k)*carb(ic_ohm_cal,:,:,n_k))/dum_ocn_tot_A
    call sub_putvars ('_surWc', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)
    loc_tmp = SUM((loc_c1 - phys_ocnatm(ipoa_seaice,:,:))* &
         &phys_ocn(ipo_A,:,:,n_k)*carb(ic_ohm_arg,:,:,n_k))/dum_ocn_tot_A
    call sub_putvars ('_surWa', loc_iou, dum_ntrec, loc_tmp, loc_c1, loc_c0)

    !-----------------------------------------------------------------------
    !     close the file
    !-----------------------------------------------------------------------
    call sub_closefile (loc_iou)

  END SUBROUTINE sub_save_netcdf_tsi
  ! ****************************************************************************************************************************** !


END MODULE biogem_data_netCDF
