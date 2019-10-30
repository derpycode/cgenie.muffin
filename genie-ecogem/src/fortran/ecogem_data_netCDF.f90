! ******************************************************************************************************************************** !
! ecogem_data_netCDF.f90
! 
! DATA LOADING/SAVING ROUTINES
! ******************************************************************************************************************************** !


MODULE ecogem_data_netCDF

  USE gem_netcdf
  USE ecogem_lib
  USE ecogem_box
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
    integer::io,jp
    integer::loc_ntrec,loc_iou
    integer::loc_id_lon,loc_id_lat,loc_id_lon_e,loc_id_lat_e
    integer::loc_id_depth,loc_id_depth_e
    integer,dimension(1:2)::loc_it_1
    integer,dimension(1:3)::loc_it_2
    integer,dimension(1:4)::loc_it_3
    character(127)::loc_title,loc_timunit
    character(127)::shrtstrng,longstrng,vardesc
    character(7)::loc_string_year
    real::loc_c0,loc_c1
    real,dimension(1:n_i)::loc_lon
    real,dimension(1:n_j)::loc_lat
    real,dimension(1:n_k)::loc_depth
    real,dimension(0:n_i)::loc_lon_e
    real,dimension(0:n_j)::loc_lat_e
    real,dimension(0:n_k)::loc_depth_e
    real,dimension(n_i,n_j)::loc_ij,loc_ij_mask
    real,dimension(n_i,n_j,n_k)::loc_ijk,loc_ijk_mask
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_c0 = 0.0
    loc_c1 = 1.0
    ! -------------------------------------------------------- !
    ! GET GRID DEFINITIONS
    ! -------------------------------------------------------- !
    loc_lon(1:n_i)   = fun_get_grid_lon(n_i)
    loc_lat(1:n_j)   = fun_get_grid_lat(n_j)
    loc_lon_e(0:n_i) = fun_get_grid_lone(n_i)
    loc_lat_e(0:n_j) = fun_get_grid_late(n_j)
    loc_depth(1:n_k) = fun_get_grid_z(n_k,goldstein_dsc)
    loc_depth_e(0:n_k) = fun_get_grid_ze(n_k,goldstein_dsc)
    ! -------------------------------------------------------- !
    ! WRITE TO FILE
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! open file 
    call sub_opennew(dum_name,loc_iou)
    ! -------------------------------------------------------- ! start definitions
    call sub_redef(loc_iou)
    ! -------------------------------------------------------- ! set global attributes
    loc_string_year = fun_conv_num_char_n(8,int(dum_yr))
    loc_title = 'ECOGEM restart @ year '//loc_string_year
    call sub_putglobal(loc_iou,dum_name,loc_title,string_ncrunid,loc_timunit)
    ! -------------------------------------------------------- ! define dimensions
    call sub_defdim ('lon',loc_iou,n_i,loc_id_lon)
    call sub_defdim ('lat',loc_iou,n_j,loc_id_lat)
    call sub_defdim ('zt',loc_iou,n_k,loc_id_depth)
    call sub_defdim ('lon_edges',loc_iou,n_i+1,loc_id_lon_e)
    call sub_defdim ('lat_edges',loc_iou,n_j+1,loc_id_lat_e)
    call sub_defdim ('zt_edges',loc_iou,n_k+1,loc_id_depth_e)
    ! -------------------------------------------------------- ! define 1d data (t)
    loc_it_1(1) = loc_id_lon
    call sub_defvar ('lon',loc_iou,1,loc_it_1,loc_c0,loc_c0,'X','D','longitude of the t grid','longitude','degrees_east')
    loc_it_1(1) = loc_id_lat
    call sub_defvar ('lat',loc_iou,1,loc_it_1,loc_c0,loc_c0,'Y','D','latitude of the t grid','latitude','degrees_north')
    loc_it_1(1) = loc_id_lon_e
    call sub_defvar ('lon_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D','longitude of t grid edges',' ','degrees')
    loc_it_1(1) = loc_id_lat_e
    call sub_defvar ('lat_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D','latitude of t grid edges',' ','degrees')
    loc_it_1(1) = loc_id_depth
    call sub_defvar ('zt',loc_iou,1,loc_it_1,loc_c0,loc_c0,'Z','D','depth of z grid',' ','m')
    loc_it_1(1) = loc_id_depth_e
    call sub_defvar ('zt_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D','depth of z grid edges',' ','m')
    loc_it_2(1) = loc_id_lon
    loc_it_2(2) = loc_id_lat
    loc_it_3(1) = loc_id_lon
    loc_it_3(2) = loc_id_lat
    loc_it_3(3) = loc_id_depth
    ! -------------------------------------------------------- ! define 2D and 3D variables
    DO io=1,iomax+iChl
       DO jp=1,npmax
          if ((io.le.iomax).or.(autotrophy(jp).gt.0.0)) then ! do not output non-existent chlorophyll for non-autotroph
             ! Create description strings
             write (shrtstrng, "(A1,A,A1,I3.3)") "_",trim(adjustl(quotastrng(io))),'_',jp
             write (longstrng, "(A11,A,A2,I3.3)") ": Plankton ",trim(adjustl(quotastrng(io))),' #',jp
             write (vardesc, "(A2,A,A14,F7.2,A8,A,A2,A,A1)") &
                  & ': ',trim(adjustl(quotastrng(io))),' biomass of...',diameter(jp), ' micron ',&
                  & trim(adjustl(pft(jp))),' (',trim(adjustl(quotaunits(io))),')'
             ! define (2D) variables
             call sub_defvar('eco2D'//shrtstrng,loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F','eco2D'//longstrng,'eco2D'//vardesc,' ')
             ! define (3D) variables
             call sub_defvar('eco3D'//shrtstrng,loc_iou,3,loc_it_3,loc_c0,loc_c0,' ','F','eco3D'//longstrng,'eco3D'//vardesc,' ')
          endif ! end if not zooplankton chlorophyll
       end do
    end do
    ! -------------------------------------------------------- ! end definitions
    call sub_enddef(loc_iou)
    call sub_sync(loc_iou)
    ! -------------------------------------------------------- ! 
    loc_ntrec = 1
    ! -------------------------------------------------------- ! write 1D variables
    call sub_putvar1d ('lon',loc_iou,n_i,loc_ntrec,n_i,loc_lon,loc_c1,loc_c0)
    call sub_putvar1d ('lon_edges',loc_iou,n_i+1,loc_ntrec,n_i+1,loc_lon_e,loc_c1,loc_c0)
    call sub_putvar1d ('lat',loc_iou,n_j,loc_ntrec,n_j,loc_lat,loc_c1,loc_c0)
    call sub_putvar1d ('lat_edges',loc_iou,n_j+1,loc_ntrec,n_j+1,loc_lat_e,loc_c1,loc_c0)
    call sub_putvar1d ('zt',loc_iou,n_k,loc_ntrec,n_k,loc_depth,loc_c1,loc_c0)
    call sub_putvar1d ('zt_edges',loc_iou,n_k+1,loc_ntrec,n_k+1,loc_depth_e,loc_c1,loc_c0)
    ! -------------------------------------------------------- ! write 2D and 3D plankton variables
    loc_ij_mask(:,:)    =  real(wet_mask_ij)
    loc_ijk_mask(:,:,:) =  real(wet_mask_ijk)
    DO io=1,iomax+iChl
       DO jp=1,npmax
          if ((io.le.iomax).or.(autotrophy(jp).gt.0.0)) then ! do not output non-existent chlorophyll for non-autotroph
             ! Create description strings
             write (shrtstrng, "(A1,A,A1,I3.3)") "_",trim(adjustl(quotastrng(io))),'_',jp
             ! write 2D variables
             loc_ij(:,:) = plankton(io,jp,:,:,n_k)
             call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_ij_mask(:,:))
             ! write 3D variables
             loc_ijk(:,:,:) = plankton(io,jp,:,:,:)
             call sub_putvar3d('eco3D'//shrtstrng,loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_ijk_mask(:,:,:))        
          endif ! end if not zooplankton chlorophyll
       end do
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
    integer::loc_err,loc_id
    character(255) :: loc_title,loc_timunit
    real           :: loc_c0,loc_c1
    integer        :: loc_it(6),loc_id_time
    integer        :: loc_id_lonm,loc_id_latm,loc_id_zt
    integer        :: loc_id_lon_e,loc_id_lat_e,loc_id_zt_e
    integer        :: loc_id_misc
    !-----------------------------------------------------------------------
    !       initialize local variables 
    !-----------------------------------------------------------------------
    loc_c0 = 0.
    loc_c1 = 1.
    !-----------------------------------------------------------------------
    !       open file 
    !-----------------------------------------------------------------------
    call sub_opennew(dum_name,dum_iou)
    !-----------------------------------------------------------------------
    !       start definitions
    !-----------------------------------------------------------------------
    call sub_redef(dum_iou)
    !-----------------------------------------------------------------------
    !       set global attributes
    !-----------------------------------------------------------------------
!!$    loc_title = 'Time averaged integrals'
!!$    write(loc_timunit,'(a)') 'Year mid-point'
    loc_title = 'Instantaneous state'
    write(loc_timunit,'(a)') 'Experiment end'
    call sub_putglobal(dum_iou,dum_name,loc_title,string_ncrunid,loc_timunit)
    !-----------------------------------------------------------------------
    !       define dimensions
    !-----------------------------------------------------------------------
    call sub_defdim('time',dum_iou,const_integer_zero,loc_id_time)
    call sub_defdim('lon',dum_iou,n_i,loc_id_lonm)
    call sub_defdim('lat',dum_iou,n_j,loc_id_latm)
    call sub_defdim('zt',dum_iou,n_k,loc_id_zt)
    call sub_defdim('lon_edges',dum_iou,n_i+1,loc_id_lon_e)
    call sub_defdim('lat_edges',dum_iou,n_j+1,loc_id_lat_e)
    call sub_defdim('zt_edges',dum_iou,n_k+1,loc_id_zt_e)
    !-----------------------------------------------------------------------
    !       define 1d data (t)
    !-----------------------------------------------------------------------
    loc_it(1) = loc_id_time
    call sub_defvar('time',dum_iou,1,loc_it(1),loc_c0,loc_c0,'T','D' &
         &, 'Year','time',trim(loc_timunit))
    call sub_defvar('year',dum_iou,1,loc_it(1),loc_c0,loc_c0,' ','F','year',' ',' ')
    !-----------------------------------------------------------------------
    !       define 1d data (x, y or z)
    !-----------------------------------------------------------------------
    loc_it(1) = loc_id_lonm
    call sub_defvar('lon', dum_iou, 1, loc_it(1), loc_c0, loc_c0, 'X', 'D' , &
         & 'longitude of the t grid', 'longitude', 'degrees_east')
    loc_it(1) = loc_id_latm
    call sub_defvar('lat', dum_iou, 1, loc_it(1), loc_c0, loc_c0, 'Y', 'D' , &
         & 'latitude of the t grid', 'latitude', 'degrees_north')
    loc_it(1) = loc_id_zt
    call sub_defvar('zt', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Z', 'D' , &
         &'z-level mid depth', 'depth', 'm')
    loc_it(1) = loc_id_lon_e
    call sub_defvar('lon_edges', dum_iou, 1, loc_it(1), loc_c0, loc_c0, ' ', 'D' , &
         & 'longitude of t grid edges', ' ', 'degrees')
    loc_it(1) = loc_id_lat_e
    call sub_defvar('lat_edges', dum_iou, 1, loc_it(1), loc_c0, loc_c0, ' ', 'D' , &
         & 'latitude of t grid edges', ' ', 'degrees')
    loc_it(1) = loc_id_zt_e
    call sub_defvar('zt_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
         &'depth of t grid edges', ' ', 'm')
    !-----------------------------------------------------------------------
    !       define basic 2d/3d data (x,y)
    !-----------------------------------------------------------------------
    SELECT CASE (dum_dd)
    CASE (2,3)
       loc_it(1) = loc_id_lonm
       loc_it(2) = loc_id_latm
       call sub_defvar('grid_level',dum_iou,2,loc_it(1:2),loc_c0,100.0,' ','I', &
            & 'grid definition','model_level_number','n/a')
       call sub_defvar('grid_mask',dum_iou,2,loc_it(1:2),loc_c0,100.0,' ','F', &
            & 'land-sea mask',' ','n/a')
       call sub_defvar('grid_topo',dum_iou,2,loc_it(1:2),loc_c0,5000.,' ','F', &
            & 'ocean depth ',' ','m')
    end select
    !-----------------------------------------------------------------------
    call sub_enddef(dum_iou)
    call sub_closefile(dum_iou)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_init_netcdf
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! INITIALIZE netCDF
  SUBROUTINE sub_init_netcdf_tser(dum_name,dum_iou)
    !-----------------------------------------------------------------------
    !       dummy arguments
    !-----------------------------------------------------------------------
    character(LEN=*),INTENT(IN) :: dum_name
    INTEGER,INTENT(OUT):: dum_iou
    !-----------------------------------------------------------------------
    !       define local variables
    !-----------------------------------------------------------------------
    integer::loc_err,loc_id
    character(255) :: loc_title,loc_timunit
    real           :: loc_c0,loc_c1
    integer        :: loc_it(6),loc_id_time,loc_id_site
    integer        :: loc_id_lonm,loc_id_latm,loc_id_zt
    integer        :: loc_id_lon_e,loc_id_lat_e,loc_id_zt_e
    integer        :: loc_id_misc
    !-----------------------------------------------------------------------
    !       initialize local variables 
    !-----------------------------------------------------------------------
    loc_c0 = 0.
    loc_c1 = 1.
    !-----------------------------------------------------------------------
    !       open file 
    !-----------------------------------------------------------------------
    call sub_opennew(dum_name,dum_iou)
    !-----------------------------------------------------------------------
    !       start definitions
    !-----------------------------------------------------------------------
    call sub_redef(dum_iou)
    !-----------------------------------------------------------------------
    !       set global attributes
    !-----------------------------------------------------------------------

    loc_title = 'Instantaneous state'
    write(loc_timunit,'(a)') 'GEnIE timestep end'
    call sub_putglobal(dum_iou,dum_name,loc_title,string_ncrunid,loc_timunit)
    !-----------------------------------------------------------------------
    !       define dimensions
    !-----------------------------------------------------------------------
    call sub_defdim('time',dum_iou,const_integer_zero,loc_id_time)
    !-----------------------------------------------------------------------
    !       define 1d data (t)
    !-----------------------------------------------------------------------
    loc_it(1) = loc_id_time
    call sub_defvar('time',dum_iou,1,loc_it(1),loc_c0,loc_c0,'T','D','Year','time',trim(loc_timunit))

    !-----------------------------------------------------------------------
    call sub_enddef(dum_iou)
    call sub_closefile(dum_iou)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_init_netcdf_tser
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_update_netcdf(dum_yr,dum_dd)
    !-----------------------------------------------------------------------
    !       dummy arguments
    !-----------------------------------------------------------------------
    REAL,INTENT(in)::dum_yr
    INTEGER,INTENT(IN)::dum_dd
    !-----------------------------------------------------------------------
    !       local variables
    !-----------------------------------------------------------------------
    real::loc_c0,loc_c1
    logical::loc_defined
    character(255)::loc_name
    integer::i, j, k, loc_i, loc_iou, loc_ntrec
    real,dimension(1:n_i)::loc_lon
    real,dimension(1:n_j)::loc_lat
    real,dimension(0:n_i)::loc_lon_e
    real,dimension(0:n_j)::loc_lat_e
    real,dimension(1:n_k)::loc_depth
    real,dimension(0:n_k)::loc_depth_e
    real,dimension(n_i,n_j)::loc_ij,loc_mask

    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- ! set default valid data limits
    loc_c0 = 0.0
    loc_c1 = 1.0
    !--------------------------------------------------------- ! determine netCDF file dimension; set record number
    SELECT CASE (dum_dd)
    CASE (2)
       loc_name  = string_ncout2d
       loc_iou   = ncout2d_iou
       loc_ntrec = ncout2d_ntrec
    CASE (3)
       loc_name  = string_ncout3d
       loc_iou   = ncout3d_iou
       loc_ntrec = ncout3d_ntrec
    CASE DEFAULT
       CALL sub_report_error( &
            & 'ecogem_data_netCDF','sub_save_netcdf', &
            & 'illegal netCDF dimension', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )       
    end select
    !--------------------------------------------------------- ! test for new netCDF file (record # == 0 )
    loc_defined = .true.
    loc_i = 0
    if (loc_ntrec .eq. 0) then 
       loc_defined = .false.
       loc_i = 1
    end if
    ! -------------------------------------------------------- !
    ! GET GRID DEFINITIONS
    ! -------------------------------------------------------- !
    loc_lon(1:n_i) = fun_get_grid_lon(n_i)
    loc_lat(1:n_j) = fun_get_grid_lat(n_j)
    loc_lon_e(0:n_i) = fun_get_grid_lone(n_i)
    loc_lat_e(0:n_j) = fun_get_grid_late(n_j)
    loc_depth(1:n_k) = fun_get_grid_z(n_k,goldstein_dsc)
    loc_depth_e(0:n_k) = fun_get_grid_ze(n_k,goldstein_dsc)
    !--------------------------------------------------------- !
    ! WRITE DATA
    !--------------------------------------------------------- ! open file
    call sub_opennext(loc_name,dum_yr,loc_i,loc_ntrec,loc_iou)
    !--------------------------------------------------------- ! write time data
    call sub_putvars('time', loc_iou, loc_ntrec, dum_yr, loc_c1, loc_c0)
    call sub_putvarIs('year', loc_iou, loc_ntrec, nint(dum_yr), loc_c1, loc_c0)
    !--------------------------------------------------------- ! 
    if (.not. loc_defined) then
       ! ----------------------------------------------------- ! write 1D variables
       call sub_putvar1d('lon',loc_iou,n_i,loc_ntrec,n_i,loc_lon,loc_c1,loc_c0)
       call sub_putvar1d('lat',loc_iou,n_j,loc_ntrec,n_j,loc_lat,loc_c1,loc_c0)
       call sub_putvar1d('lon_edges',loc_iou,n_i+1,loc_ntrec,n_i+1,loc_lon_e,loc_c1,loc_c0)
       call sub_putvar1d('lat_edges',loc_iou,n_j+1,loc_ntrec,n_j+1,loc_lat_e,loc_c1,loc_c0)
       call sub_putvar1d('zt',loc_iou,n_k,loc_ntrec,n_k,loc_depth,loc_c1,loc_c0)
       call sub_putvar1d('zt_edges',loc_iou,n_k+1,loc_ntrec,n_k+1,loc_depth_e,loc_c1,loc_c0)
       ! ----------------------------------------------------- ! write (2D) tracer variables
       loc_mask(:,:) = 1.0                                     ! *** NEED DATA ***
       loc_ij(:,:) = 1.0                                       ! *** NEED DATA ***
       call sub_putvar2d('grid_level',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))
       loc_ij(:,:) = loc_mask(:,:)                             ! *** NEED DATA ***
       call sub_putvar2d('grid_mask',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))
       loc_ij(:,:) = 5000.0                                    ! *** NEED DATA ***
       call sub_putvar2d('grid_topo',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))
    end if
    !--------------------------------------------------------- ! 
    ! END
    !--------------------------------------------------------- ! 
    ! update record number
    SELECT CASE (dum_dd)
    CASE (2)
       ncout2d_ntrec = loc_ntrec
    CASE (3)
       ncout3d_ntrec = loc_ntrec
    CASE DEFAULT
       !
    end select
    !
    call sub_sync(loc_iou)
    !--------------------------------------------------------- !
  END SUBROUTINE sub_update_netcdf
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_update_netcdf_tser(k)
    !-----------------------------------------------------------------------
    !       dummy arguments
    !-----------------------------------------------------------------------
    INTEGER,INTENT(in)::k
    !-----------------------------------------------------------------------
    !       local variables
    !-----------------------------------------------------------------------
    real::loc_c0,loc_c1
    logical::loc_defined
    character(255)::loc_name
    integer::i, j, loc_i, loc_iou, loc_ntrec

    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- ! set default valid data limits
    loc_c0 = 0.0
    loc_c1 = 1.0
    !--------------------------------------------------------- ! determine netCDF file dimension; set record number

    loc_name  = TRIM(par_outdir_name)//'timeseries_'//trim(tser_name(k))//'.nc'
    loc_iou   = ncout1d_iou(k)
    loc_ntrec = ncout1d_ntrec(k)

    !--------------------------------------------------------- ! test for new netCDF file (record # == 0 )
    loc_defined = .true.
    loc_i = 0
    if (loc_ntrec .eq. 0) then 
       loc_defined = .false.
       loc_i = 1
    end if
    !--------------------------------------------------------- !
    ! WRITE DATA
    !--------------------------------------------------------- ! open file
    call sub_opennext(loc_name,0.0,loc_i,loc_ntrec,loc_iou)
    call sub_putvar1d('time', loc_iou,48,loc_ntrec,48,time_tser/48.0,loc_c1,loc_c0)

    !--------------------------------------------------------- ! 
    ! END
    !--------------------------------------------------------- ! 
    ! update record number
    ncout1d_ntrec(k) = loc_ntrec

    !
    call sub_sync(loc_iou)
    !--------------------------------------------------------- !
  END SUBROUTINE sub_update_netcdf_tser
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! 2-D FIELDS
  ! NOTE: calls to sub_adddef_netcdf(dum_ncid,dum_dino,dum_tname,dum_tlname,dum_unit,dum_min,dum_max)
  !       consist of the following passed parameters:
  !       dum_ncid   = unit
  !       dum_dino   = number dimensions of data (3 in this case - 2 for lat/lon and one of time)
  !       dum_tname  = name of variable to be defined
  !       dum_tlname = long name
  !       dum_unit   = data units
  !       dum_min    = minimum range (default real)
  !       dum_max    = maximum range (default real)
  SUBROUTINE sub_save_netcdf_2d()
    USE genie_util, ONLY: check_unit, check_iostat
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    INTEGER::loc_iou,loc_ntrec,io,jp,pj,ii,ko
    real,DIMENSION(n_i,n_j)::loc_ij,loc2_ij,loc_mask
    CHARACTER(len=255)::loc_unitsname
    CHARACTER(len=255)::shrtstrng,longstrng,diamtr
    real::loc_c0,loc_c1
    real::totalplankton(iomax+iChl,n_i,n_j)
    real::totalfluxes(iomax,n_i,n_j)
    real::weightedmean(n_i,n_j)
    real::weighteddev(n_i,n_j)
    real::minsize(n_i,n_j),maxsize(n_i,n_j)
    real::shannon(n_i,n_j),simpson(n_i,n_j),nthresh(n_i,n_j),berger(n_i,n_j)
    real::picochl(n_i,n_j),nanochl(n_i,n_j),microchl(n_i,n_j)
    real::nphyto
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou   = ncout2d_iou
    loc_ntrec = ncout2d_ntrec
    loc_mask(:,:) = real(wet_mask_ij)

    !-----------------------------------------------------------------------
    ! POPULATION AND COMMUNITY BIOMASS
    !-----------------------------------------------------------------------
    loc_c0 = 0.0
    loc_c1 = 1.0
    totalplankton = 0.0
    totalfluxes = 0.0
    DO io=1,iomax+iChl
       DO jp=1,npmax
          if ((io.le.iomax).or.(autotrophy(jp).gt.0.0)) then ! do not output non-existent chlorophyll for non-autotroph
             ! Create description strings
             write (diamtr,"(F10.2)") diameter(jp)
             write (shrtstrng, "(A10,A,A1,I3.3)") "_Plankton_",trim(adjustl(quotastrng(io))),'_',jp   
             write (longstrng, "(A,A18,I3.3,A2,A,A8,A,A1)") trim(adjustl(quotastrng(io))),' Biomass - Popn. #',jp,' (',trim(adjustl(diamtr)),' micron ',trim(pft(jp)),')'
             ! write population biomasses and sum for community
             loc_ij(:,:) = int_plankton_timeslice(io,jp,:,:,n_k)
             call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
             call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))  
             totalplankton(io,:,:) = totalplankton(io,:,:) + int_plankton_timeslice(io,jp,:,:,n_k)   
             if (io.le.iomax) then
                totalfluxes(io,:,:) = totalfluxes(io,:,:) + int_uptake_timeslice(io,jp,:,:,n_k)   
             endif
          endif ! end if not zooplankton chlorophyll
          if ((io.ne.iCarb).and.(io.le.iomax).and.(autotrophy(jp).gt.0.0)) then ! limiting factors for autotroph nutrient quotas only
             ! quota limitation status 
             loc_ij(:,:) = int_gamma_timeslice(io,jp,:,:,n_k)
             write (shrtstrng, "(A8,A,A1,I3.3)") "_xGamma_",trim(adjustl(quotastrng(io))),'_',jp   
             write (longstrng, "(A,A21,I3.3,A2,A,A8,A,A1)") trim(adjustl(quotastrng(io))),' Limitation - Popn. #',jp,' (',trim(adjustl(diamtr)),' micron ',trim(pft(jp)),')'
             call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
             call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))
          endif
          ! Write plankton export rate - Fanny/Maria - Aug19
          if (io.le.iomax) then
             loc_ij(:,:) = int_export_timeslice(io,jp,:,:,n_k)
             write (shrtstrng, "(A8,A,A1,I3.3)") "_Export_",trim(adjustl(quotastrng(io))),'_',jp   
             write (longstrng, "(A,A17,I3.3,A2,A,A8,A,A1)") trim(adjustl(quotastrng(io))),' Export - Popn. #',jp,' (',trim(adjustl(diamtr)),' micron ',trim(pft(jp)),')'
             call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,trim(quotaunits(io))//' d^-1',loc_c0,loc_c0)
             call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))  
          endif
       end do
       ! Write community total biomasses and inorganic resource fluxes
       write (shrtstrng, "(A10,A,A6)") "_Plankton_",trim(adjustl(quotastrng(io))),"_Total" 
       write (longstrng, "(A,A16)") trim(adjustl(quotastrng(io))) ," Biomass - Total" 
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,totalplankton(io,:,:),loc_mask(:,:))   
       if (io.le.iomax) then
          write (shrtstrng, "(A15,A)") "_Uptake_Fluxes_",trim(adjustl(quotastrng(io)))  
          write (longstrng, "(A14,A)") "Uptake Fluxes ",trim(adjustl(quotastrng(io)))  
          call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,trim(quotaunits(io))//' d^-1',loc_c0,loc_c0)
          call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,totalfluxes(io,:,:),loc_mask(:,:)) 
       endif
    end do
    ! temperature limitation status
    loc_ij(:,:) = int_gamma_timeslice(iomax+1,1,:,:,n_k)
    write (shrtstrng, "(A9)") "_xGamma_T"   
    write (longstrng, "(A22)") 'Temperature Limitation'
    call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
    call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))
    ! Zoo food limitation status - Maria May 2019
    DO jp=1,npmax
       loc_ij(:,:) = int_zoogamma_timeslice(jp,:,:,n_k)
       write (shrtstrng, "(A12,I3.3)") "_Gamma_Food_",jp
       write (longstrng, "(A37,I3.3)") 'Zooplankton food limitation - Popn. #',jp
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))
    end do

    DO ii=1,iimax
       ! Create description strings
       write (shrtstrng, "(A11,A)") "_Nutrients_",trim(adjustl(rsrcstrng(ii)))
       write (longstrng, "(A10,A)") "Nutrients ",trim(adjustl(rsrcstrng(ii)))    
       ! write 2D variables
       loc_ij(:,:) = int_nutrient_timeslice(ii,:,:,n_k)
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'mmol '//trim(rsrcstrng(ii))//' m^-3',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask(:,:))     
    end do

    !-----------------------------------------------------------------------
    ! COMMUNITY SIZE & DIVERSITY METRICS (AUTOTROPHS ONLY)
    !-----------------------------------------------------------------------
    ! Size metrics ... total plankton, phytoplankton and zooplankton
    ! Carbon biomass weighted mean size
    weightedmean(:,:)    = 0.0
    totalplankton(:,:,:) = 0.0
    do jp=1,npmax
       if (autotrophy(jp).gt.0.0) then ! AUTOTROPHIC plankton ONLY
          ! Carbon biomass weighted geometric mean of size
          weightedmean(:,:)        = weightedmean(:,:)        + int_plankton_timeslice(iCarb,jp,:,:,n_k) * logesd(jp)
          totalplankton(iCarb,:,:) = totalplankton(iCarb,:,:) + int_plankton_timeslice(iCarb,jp,:,:,n_k)
       endif
    enddo
    weightedmean(:,:) = weightedmean(:,:) / totalplankton(iCarb,:,:)

    ! Carbon biomass weighted standard deviation of size
    weighteddev(:,:) = 0.0
    nphyto           = 0.0
    nthresh(:,:)     = 0.0
    minsize(:,:)     = maxval(diameter(:))
    maxsize(:,:)     = minval(diameter(:))
    shannon(:,:)     = 0.0
    simpson(:,:)     = 0.0
    berger(:,:)      = 0.0
    picochl(:,:)     = 0.0
    nanochl(:,:)     = 0.0
    microchl(:,:)    = 0.0
    do jp=1,npmax
       if (autotrophy(jp).gt.0.0) then   
          nphyto           = nphyto + 1.0
          ! Carbon biomass weighted geometric standard deviation of size
          weighteddev(:,:) = weighteddev(:,:) + int_plankton_timeslice(iCarb,jp,:,:,n_k) * (logesd(jp)-weightedmean)**2  
          ! ADB threshold diversity index - Population above threshold
          nthresh(:,:)     = nthresh(:,:) + MERGE(1.0,0.0,   int_plankton_timeslice(iCarb,jp,:,:,n_k).gt.(totalplankton(iCarb,:,:)*0.001))
          ! Minimum size - Population above threshold and diameter less than current minimum value
          minsize(:,:)     = MERGE(diameter(jp),minsize(:,:),int_plankton_timeslice(iCarb,jp,:,:,n_k).gt.(totalplankton(iCarb,:,:)*0.001) .and. minsize(:,:).gt.diameter(jp) )
          ! Maximum size - Population above threshold and diameter more than current maximum value
          maxsize(:,:)     = MERGE(diameter(jp),maxsize(:,:),int_plankton_timeslice(iCarb,jp,:,:,n_k).gt.(totalplankton(iCarb,:,:)*0.001) .and. maxsize(:,:).lt.diameter(jp) )
          ! Shannon-Wiener
          shannon(:,:)     = shannon(:,:) -     int_plankton_timeslice(iCarb,jp,:,:,n_k)/totalplankton(iCarb,:,:) &
               & * log(int_plankton_timeslice(iCarb,jp,:,:,n_k)/totalplankton(iCarb,:,:))
          ! Gini-Simpson
          simpson(:,:)     = simpson(:,:) +    (int_plankton_timeslice(iCarb,jp,:,:,n_k)/totalplankton(iCarb,:,:)) ** 2
          ! Berger-Parker
          berger(:,:)     = MERGE(int_plankton_timeslice(iCarb,jp,:,:,n_k)/totalplankton(iCarb,:,:),berger(:,:),int_plankton_timeslice(iCarb,jp,:,:,n_k)/totalplankton(iCarb,:,:).gt.berger(:,:))
          ! size fractionated chlorophyll biomass
          if (diameter(jp).lt.2.0) then
             picochl(:,:)    = picochl(:,:)  + int_plankton_timeslice(iChlo,jp,:,:,n_k)
          elseif (diameter(jp).ge.2.0.and.diameter(jp).lt.20.0) then
             nanochl(:,:)    = nanochl(:,:)  + int_plankton_timeslice(iChlo,jp,:,:,n_k)
          elseif (diameter(jp).ge.20.0) then
             microchl(:,:)   = microchl(:,:) + int_plankton_timeslice(iChlo,jp,:,:,n_k)
          endif
       endif
    enddo
    ! take inverse of Gini-Simpson and Berger-Parker diversity indices 
    simpson(:,:) = 1.0 - simpson(:,:)
    berger(:,:)  = 1.0 - berger(:,:)
    !
    weighteddev(:,:) = sqrt( nphyto * weighteddev / ((nphyto-1.0) * totalplankton(iCarb,:,:)) )

    ! only save these variables if Phyto diversity is possible
    if (nphyto.gt.1.0) then
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A10)") "_Size_Mean"  
       write (longstrng, "(A31)") "Geometric Mean of Cell Diameter"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'µm',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,10.0 ** weightedmean(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A11)") "_Size_Stdev"  
       write (longstrng, "(A45)") "Geometric Standard Deviation of Cell Diameter"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'Relative magnitude',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,10.0 ** weighteddev(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A13)") "_Size_Minimum"  
       write (longstrng, "(A21)") "Minimum Cell Diameter"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'µm',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,minsize(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A13)") "_Size_Maximum"  
       write (longstrng, "(A21)") "Maximum Cell Diameter"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'µm',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,maxsize(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A20)") "_Diversity_Threshold"  
       write (longstrng, "(A25)") "Threshold Diversity Index"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'-',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,nthresh(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A18)") "_Diversity_Shannon"  
       write (longstrng, "(A23)") "Shannon Diversity Index"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'-',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,shannon(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A18)") "_Diversity_Simpson"
       write (longstrng, "(A23)") "Simpson Diversity Index"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'-',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,simpson(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A17)") "_Diversity_Berger"
       write (longstrng, "(A22)") "Berger Diversity Index"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'-',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,berger(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A19)") "_Size_Frac_Pico_Chl"  
       write (longstrng, "(A24)") "Pico chlorophyll biomass"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'mg Chl m^-3',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,picochl(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A19)") "_Size_Frac_Nano_Chl"  
       write (longstrng, "(A24)") "Nano chlorophyll biomass"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'mg Chl m^-3',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,nanochl(:,:),loc_mask(:,:))
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A20)") "_Size_Frac_Micro_Chl"  
       write (longstrng, "(A25)") "Micro chlorophyll biomass"
       call sub_adddef_netcdf(loc_iou,3,'eco2D'//shrtstrng,longstrng,'mg Chl m^-3',loc_c0,loc_c0)
       call sub_putvar2d('eco2D'//shrtstrng,loc_iou,n_i,n_j,loc_ntrec,microchl(:,:),loc_mask(:,:))
    endif

    !-----------------------------------------------------------------------
    ! PHYSIOLOGICAL STATUS (RESOURCE AND ABIOTIC LIMITATION FACTORS)
    !-----------------------------------------------------------------------


    ! ### INSERT CODE TO SAVE ADDITIONAL 2-D DATA FIELDS ###################################################################### !
    !
    ! ######################################################################################################################### !
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_2d
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! 3-D FIELDS
  ! NOTE: calls to sub_adddef_netcdf(dum_ncid,dum_dino,dum_tname,dum_tlname,dum_unit,dum_min,dum_max)
  !       consist of the following passed parameters:
  !       dum_ncid   = unit
  !       dum_dino   = number dimensions of data (4 in this case - 3 for lat/lon/depth and one of time)
  !       dum_tname  = name of variable to be defined
  !       dum_tlname = long name
  !       dum_unit   = data units
  !       dum_min    = minimum range (default real)
  !       dum_max    = maximum range (default real)
  SUBROUTINE sub_save_netcdf_3d()
    USE genie_util, ONLY: check_unit, check_iostat
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    INTEGER::loc_iou,loc_ntrec,io,jp,ii
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk,loc_mask
    CHARACTER(len=255)::loc_unitsname
    CHARACTER(len=255)::shrtstrng,longstrng
    real::loc_c0,loc_c1
    real::totalplankton(iomax+iChl,n_i,n_j,n_k)
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou   = ncout3d_iou
    loc_ntrec = ncout3d_ntrec
    loc_mask(:,:,:) = real(wet_mask_ijk)
    !-----------------------------------------------------------------------
    !       EXAMPLE 'DATA'
    !-----------------------------------------------------------------------
    loc_unitsname = 'n/a'
    loc_c0 = 0.0
    loc_c1 = 1.0
    DO io=1,iomax+iChl
       DO jp=1,npmax
          if ((io.le.iomax).or.(autotrophy(jp).gt.0.0)) then ! do not output non-existent chlorophyll for non-autotroph
             ! Create description strings
             write (shrtstrng, "(A10,A,A1,I3.3)") "_Plankton_",trim(adjustl(quotastrng(io))),'_',jp   
             write (longstrng, "(A9,A,A2,I3.3)") "Plankton ",trim(adjustl(quotastrng(io))),' #',jp     
             ! write 3D variables
             loc_ijk(:,:,:) = int_plankton_timeslice(io,jp,:,:,:)
             call sub_adddef_netcdf(loc_iou,4,'eco3D'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
             call sub_putvar3d('eco3D'//shrtstrng,loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask(:,:,:)) 
             totalplankton(io,:,:,:) = totalplankton(io,:,:,:) + int_plankton_timeslice(io,jp,:,:,:)       
          endif ! end if not zooplankton chlorophyll
       end do
       ! Write community total biomasses
       write (shrtstrng, "(A16,A)") "_Total_Plankton_",trim(adjustl(quotastrng(io)))  
       write (longstrng, "(A15,A)") "Total Plankton ",trim(adjustl(quotastrng(io)))  
       call sub_adddef_netcdf(loc_iou,4,'eco3D'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
       call sub_putvar3d('eco3D'//shrtstrng,loc_iou,n_i,n_j,n_k,loc_ntrec,totalplankton(io,:,:,:),loc_mask(:,:,:))  
    end do

    DO ii=1,iimax
       ! Create description strings
       write (shrtstrng, "(A11,A)") "_Nutrients_",trim(adjustl(rsrcstrng(ii)))
       write (longstrng, "(A10,A)") "Nutrients ",trim(adjustl(rsrcstrng(ii)))    
       ! write 3D variables
       loc_ijk(:,:,:) = nutrient(ii,:,:,:)
       call sub_adddef_netcdf(loc_iou,4,'eco3D'//shrtstrng,longstrng,'mmol m^{-3}',loc_c0,loc_c0)
       call sub_putvar3d('eco3D'//shrtstrng,loc_iou,n_i,n_j,n_k,loc_ntrec,loc_ijk(:,:,:),loc_mask(:,:,:))  
    end do
    ! ### INSERT CODE TO SAVE ADDITIONAL 3-D DATA FIELDS ###################################################################### !
    !
    ! ######################################################################################################################### !
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_3d
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WRITE RUN-TIME DIAGNOSTICS
  SUBROUTINE sub_save_netcdf_tseries(k)
    USE genie_util, ONLY: check_unit, check_iostat
    ! 
    INTEGER,INTENT(in)::k
    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    INTEGER::loc_iou,loc_ntrec,io,jp,pj,ii,ko
    real,DIMENSION(48)::loc_ij
    CHARACTER(len=255)::loc_unitsname
    CHARACTER(len=255)::shrtstrng,longstrng,diamtr
    real::loc_c0,loc_c1
    real::totalplankton(iomax+iChl,48)
    real::totalfluxes(iomax,48)
    real::weightedmean(48)
    real::weighteddev(48)
    real::minsize(48),maxsize(48)
    real::shannon(48),simpson(48),nthresh(48),berger(48)
    real::picochl(48),nanochl(48),microchl(48)
    real::nphyto
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou   = ncout1d_iou(k)
    loc_ntrec = ncout1d_ntrec(k)
    !-----------------------------------------------------------------------
    ! POPULATION AND COMMUNITY BIOMASS
    !-----------------------------------------------------------------------
    loc_c0 = 0.0
    loc_c1 = 1.0
    totalplankton = 0.0
    totalfluxes = 0.0
    DO io=1,iomax+iChl
       DO jp=1,npmax
          if ((io.le.iomax).or.(autotrophy(jp).gt.0.0)) then ! do not output non-existent chlorophyll for non-autotroph
             ! Create description strings
             write (diamtr,"(F10.2)") diameter(jp)
             write (shrtstrng, "(A10,A,A1,I3.3)") "_Plankton_",trim(adjustl(quotastrng(io))),'_',jp   
             write (longstrng, "(A,A18,I3.3,A2,A,A8,A,A1)") trim(adjustl(quotastrng(io))),' Biomass - Popn. #',jp,' (',trim(adjustl(diamtr)),' micron ',trim(pft(jp)),')'
             ! write population biomasses and sum for community
             loc_ij(:) = plankton_tser(io,jp,k,:)
             call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
             call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,loc_ij(:),loc_c1,loc_c0) 
             totalplankton(io,:) = totalplankton(io,:) + plankton_tser(io,jp,k,:)   
             if (io.le.iomax) then
                totalfluxes(io,:) = totalfluxes(io,:) + uptake_tser(io,jp,k,:)   
             endif
          endif ! end if not zooplankton chlorophyll
          if ((io.ne.iCarb).and.(io.le.iomax).and.(autotrophy(jp).gt.0.0)) then ! limiting factors for autotroph nutrient quotas only
             ! quota limitation status 
             loc_ij(:) = gamma_tser(io,jp,k,:)
             write (shrtstrng, "(A8,A,A1,I3.3)") "_xGamma_",trim(adjustl(quotastrng(io))),'_',jp   
             write (longstrng, "(A,A21,I3.3,A2,A,A8,A,A1)") trim(adjustl(quotastrng(io))),' Limitation - Popn. #',jp,' (',trim(adjustl(diamtr)),' micron ',trim(pft(jp)),')'
             call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
             call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,loc_ij(:),loc_c1,loc_c0)
          endif
       end do
       ! Write community total biomasses and inorganic resource fluxes
       write (shrtstrng, "(A10,A,A6)") "_Plankton_",trim(adjustl(quotastrng(io))),"_Total" 
       write (longstrng, "(A,A16)") trim(adjustl(quotastrng(io))) ," Biomass - Total" 
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,totalplankton(io,:),loc_c1,loc_c0)   
       if (io.le.iomax) then
          write (shrtstrng, "(A15,A)") "_Uptake_Fluxes_",trim(adjustl(quotastrng(io)))  
          write (longstrng, "(A14,A)") "Uptake Fluxes ",trim(adjustl(quotastrng(io)))  
          call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,trim(quotaunits(io))//' d^-1',loc_c0,loc_c0)
          call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,totalfluxes(io,:),loc_c0,loc_c0) 
       endif
    end do
    ! temperature limitation status
    loc_ij(:) = gamma_tser(iomax+1,1,k,:)
    write (shrtstrng, "(A9)") "_xGamma_T"   
    write (longstrng, "(A22)") 'Temperature Limitation'
    call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,trim(quotaunits(io)),loc_c0,loc_c0)
    call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,loc_ij(:),loc_c1,loc_c0)

    DO ii=1,iimax
       ! Create description strings
       write (shrtstrng, "(A11,A)") "_Nutrients_",trim(adjustl(rsrcstrng(ii)))
       write (longstrng, "(A10,A)") "Nutrients ",trim(adjustl(rsrcstrng(ii)))    
       ! write 1D variables
       loc_ij(:) = nutrient_tser(ii,k,:)
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'mmol '//trim(rsrcstrng(ii))//' m^-3',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,loc_ij(:),loc_c0,loc_c0)     
    end do
    ! 
    !     !-----------------------------------------------------------------------
    !     ! COMMUNITY SIZE & DIVERSITY METRICS (AUTOTROPHS ONLY)
    !     !-----------------------------------------------------------------------
    ! Size metrics ... total plankton, phytoplankton and zooplankton
    ! Carbon biomass weighted mean size
    weightedmean(:)    = 0.0
    totalplankton(iCarb,:) = 0.0
    do jp=1,npmax
       if (autotrophy(jp).gt.0.0) then ! AUTOTROPHIC plankton ONLY
          ! Carbon biomass weighted geometric mean of size
          weightedmean(:)        = weightedmean(:)        + plankton_tser(iCarb,jp,k,:) * logesd(jp)
          totalplankton(iCarb,:) = totalplankton(iCarb,:) + plankton_tser(iCarb,jp,k,:)
       endif
    enddo
    weightedmean(:) = weightedmean(:) / totalplankton(iCarb,:)

    ! Carbon biomass weighted standard deviation of size
    weighteddev(:) = 0.0
    nphyto         = 0.0
    nthresh(:)     = 0.0
    minsize(:)     = maxval(diameter(:))
    maxsize(:)     = minval(diameter(:))
    shannon(:)     = 0.0
    simpson(:)     = 0.0
    berger(:)      = 0.0
    picochl(:)     = 0.0
    nanochl(:)     = 0.0
    microchl(:)    = 0.0
    do jp=1,npmax
       if (autotrophy(jp).gt.0.0) then   
          nphyto           = nphyto + 1.0
          ! Carbon biomass weighted geometric standard deviation of size
          weighteddev(:) = weighteddev(:) + plankton_tser(iCarb,jp,k,:) * (logesd(jp)-weightedmean)**2  
          ! ADB threshold diversity index - Population above threshold
          nthresh(:)     = nthresh(:) + MERGE(1.0,0.0,   plankton_tser(iCarb,jp,k,:).gt.(totalplankton(iCarb,:)*0.001))
          ! Minimum size - Population above threshold and diameter less than current minimum value
          minsize(:)     = MERGE(diameter(jp),minsize(:),plankton_tser(iCarb,jp,k,:).gt.(totalplankton(iCarb,:)*0.001) .and. minsize(:).gt.diameter(jp) )
          ! Maximum size - Population above threshold and diameter more than current maximum value
          maxsize(:)     = MERGE(diameter(jp),maxsize(:),plankton_tser(iCarb,jp,k,:).gt.(totalplankton(iCarb,:)*0.001) .and. maxsize(:).lt.diameter(jp) )
          ! Shannon-Wiener
          shannon(:)     = shannon(:) -     plankton_tser(iCarb,jp,k,:)/totalplankton(iCarb,:) &
               & * log(plankton_tser(iCarb,jp,k,:)/totalplankton(iCarb,:))
          ! Gini-Simpson
          simpson(:)     = simpson(:) +    (plankton_tser(iCarb,jp,k,:)/totalplankton(iCarb,:)) ** 2
          ! Berger-Parker
          berger(:)     = MERGE(plankton_tser(iCarb,jp,k,:)/totalplankton(iCarb,:),berger(:),plankton_tser(iCarb,jp,k,:)/totalplankton(iCarb,:).gt.berger(:))
          ! size fractionated chlorophyll biomass
          if (diameter(jp).lt.2.0) then
             picochl(:)    = picochl(:)  + plankton_tser(iChlo,jp,k,:)
          elseif (diameter(jp).ge.2.0.and.diameter(jp).lt.20.0) then
             nanochl(:)    = nanochl(:)  + plankton_tser(iChlo,jp,k,:)
          elseif (diameter(jp).ge.20.0) then
             microchl(:)   = microchl(:) + plankton_tser(iChlo,jp,k,:)
          endif
       endif
    enddo
    ! take inverse of Gini-Simpson and Berger-Parker diversity indices 
    simpson(:) = 1.0 - simpson(:)
    berger(:)  = 1.0 - berger(:)
    !
    weighteddev(:) = sqrt( nphyto * weighteddev / ((nphyto-1.0) * totalplankton(iCarb,:)) )
    !     
    ! only save these variables if Phyto diversity is possible
    if (nphyto.gt.1.0) then
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A10)") "_Size_Mean"  
       write (longstrng, "(A31)") "Geometric Mean of Cell Diameter"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'µm',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,10.0 ** weightedmean(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A11)") "_Size_Stdev"  
       write (longstrng, "(A45)") "Geometric Standard Deviation of Cell Diameter"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'Relative magnitude',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,10.0 ** weighteddev(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A13)") "_Size_Minimum"  
       write (longstrng, "(A21)") "Minimum Cell Diameter"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'µm',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,minsize(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A13)") "_Size_Maximum"  
       write (longstrng, "(A21)") "Maximum Cell Diameter"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'µm',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,maxsize(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A20)") "_Diversity_Threshold"  
       write (longstrng, "(A25)") "Threshold Diversity Index"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'-',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,nthresh(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A18)") "_Diversity_Shannon"  
       write (longstrng, "(A23)") "Shannon Diversity Index"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'-',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,shannon(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A18)") "_Diversity_Simpson"
       write (longstrng, "(A23)") "Simpson Diversity Index"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'-',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,simpson(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A17)") "_Diversity_Berger"
       write (longstrng, "(A22)") "Berger Diversity Index"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'-',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,berger(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A19)") "_Size_Frac_Pico_Chl"  
       write (longstrng, "(A24)") "Pico chlorophyll biomass"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'mg Chl m^-3',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,picochl(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A19)") "_Size_Frac_Nano_Chl"  
       write (longstrng, "(A24)") "Nano chlorophyll biomass"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'mg Chl m^-3',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,nanochl(:),loc_c0,loc_c0)
       ! Create description strings and write 2D variables
       write (shrtstrng, "(A20)") "_Size_Frac_Micro_Chl"  
       write (longstrng, "(A25)") "Micro chlorophyll biomass"
       call sub_adddef_netcdf(loc_iou,1,'ecoTS'//shrtstrng,longstrng,'mg Chl m^-3',loc_c0,loc_c0)
       call sub_putvar1d('ecoTS'//shrtstrng,loc_iou,48,loc_ntrec,48 ,microchl(:),loc_c0,loc_c0)
    endif

  END SUBROUTINE sub_save_netcdf_tseries
  ! ****************************************************************************************************************************** !


END MODULE ecogem_data_netCDF
