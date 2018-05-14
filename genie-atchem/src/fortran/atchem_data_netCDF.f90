! ******************************************************************************************************************************** !
! atchem_data_netCDF.f90
! ATCHEM netCDF ROUTINES
! ******************************************************************************************************************************** !


MODULE atchem_data_netCDF


  USE gem_netcdf
  USE atchem_lib
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
    integer::ia,l
    integer::loc_ntrec,loc_iou
    integer::loc_id_lonm,loc_id_latm,loc_id_lon_e,loc_id_lat_e
    integer,dimension(1:2)::loc_it_1
    integer,dimension(1:3)::loc_it_2
    character(127)::loc_title,loc_timunit
    character(7)::loc_string_year
    real::loc_c0,loc_c1
    real,dimension(0:n_i)::loc_lon_e
    real,dimension(0:n_j)::loc_lat_e
    real,dimension(n_i,n_j)::loc_ij,loc_ij_mask
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
    loc_title = 'ATCHEM restart @ year '//loc_string_year
    call sub_putglobal(loc_iou,dum_name,loc_title,string_ncrunid,loc_timunit)
    ! -------------------------------------------------------- ! define dimensions
    call sub_defdim ('lon', loc_iou, n_i, loc_id_lonm)
    call sub_defdim ('lat', loc_iou, n_j, loc_id_latm)
    call sub_defdim ('lon_edges', loc_iou, n_i+1, loc_id_lon_e)
    call sub_defdim ('lat_edges', loc_iou, n_j+1, loc_id_lat_e)
    ! -------------------------------------------------------- ! define 1d data (t)
    loc_it_1(1) = loc_id_lonm
    call sub_defvar ('lon',loc_iou,1,loc_it_1,loc_c0,loc_c0,'X','D','longitude of the t grid','longitude','degrees_east')
    loc_it_1(1) = loc_id_latm
    call sub_defvar ('lat',loc_iou,1,loc_it_1,loc_c0,loc_c0,'Y','D','latitude of the t grid','latitude','degrees_north')
    loc_it_1(1) = loc_id_lon_e
    call sub_defvar ('lon_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D' ,'longitude of t grid edges',' ','degrees')
    loc_it_1(1) = loc_id_lat_e
    call sub_defvar ('lat_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D','latitude of t grid edges',' ','degrees')
    loc_it_2(1) = loc_id_lonm
    loc_it_2(2) = loc_id_latm
    ! -------------------------------------------------------- ! define (2D) tracer variables
    DO l=1,n_l_atm
       ia = conv_iselected_ia(l)
       call sub_defvar('atm_'//trim(string_atm(ia)),loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
            & string_longname_atm(ia),'Atmosphere tracer - '//trim(string_atm(ia)),' ')
    end DO
    ! -------------------------------------------------------- ! end definitions
    call sub_enddef (loc_iou)
    call sub_sync(loc_iou)
    ! -------------------------------------------------------- ! 
    loc_ntrec = 1
    ! -------------------------------------------------------- ! write 1D variables
    call sub_putvar1d ('lon',loc_iou,n_i,loc_ntrec,n_i,phys_atm(ipa_lon,:,1),loc_c1,loc_c0)
    call edge_maker (1,loc_lon_e,phys_atm(ipa_lon,:,1),phys_atm(ipa_lone,:,1),phys_atm(ipa_dlon,:,1),n_i)
    call sub_putvar1d ('lon_edges',loc_iou,n_i+1,loc_ntrec,n_i+1,loc_lon_e,loc_c1,loc_c0)
    call sub_putvar1d ('lat',loc_iou,n_j,loc_ntrec,n_j,phys_atm(ipa_lat,1,:),loc_c1,loc_c0)
    call edge_maker (1,loc_lat_e, phys_atm(ipa_lat,1,:),phys_atm(ipa_latn,1,:),phys_atm(ipa_dlat,1,:), n_j)
    call sub_putvar1d ('lat_edges',loc_iou,n_j+1,loc_ntrec,n_j+1,loc_lat_e,loc_c1,loc_c0)
    ! -------------------------------------------------------- ! write (2D) tracer variables
    loc_ij_mask(:,:) = 1.0
    loc_ij(:,:)= 0.0
    DO l=1,n_l_atm
       ia = conv_iselected_ia(l)
       loc_ij(:,:) = atm(ia,:,:)
       call sub_putvar2d('atm_'//trim(string_atm(ia)),loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_ij_mask(:,:))
    end DO
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
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_netCDF_ncrstinit_OLD(dum_name,dum_iou)
    ! ======================================================== !
    !     initialize netcdf restart file
    !
    !     input:
    !       dum_name  = name of variable to be defined
    !       dum_iou   = unit
    !
    ! loosely based on code by: M. Eby
    ! ======================================================== !
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    character(LEN=*),INTENT(IN)::dum_name
    INTEGER,INTENT(OUT)::dum_iou
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    character(255) :: loc_title, loc_timunit
    real           :: loc_c0, loc_c1
    integer        :: loc_it(6), loc_id_time, loc_id_lonm
    integer        :: loc_id_latm, loc_id_lon_e, loc_id_xu, loc_id_yu
    integer        :: loc_id_lat_e, loc_id_xu_e, loc_id_yu_e
    integer        :: loc_id_misc
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_c0 = 0.0
    loc_c1 = 1.0
    ! -------------------------------------------------------- !
    ! WRITE TO FILE
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! open file 
    call sub_opennew(dum_name,dum_iou)
    ! -------------------------------------------------------- ! start definitions
    call sub_redef(dum_iou)
    ! -------------------------------------------------------- ! set global attributes
    loc_title = 'Time averaged integrals'
    write (loc_timunit,'(a)') 'Year mid-point'
    call sub_putglobal(dum_iou,dum_name,loc_title,string_ncrunid,loc_timunit)
    ! -------------------------------------------------------- ! define dimensions
    call sub_defdim ('time', dum_iou, const_integer_zero, loc_id_time)
    call sub_defdim ('lon', dum_iou, n_i, loc_id_lonm)
    call sub_defdim ('lat', dum_iou, n_j, loc_id_latm)
    call sub_defdim ('xu', dum_iou, n_i, loc_id_xu)
    call sub_defdim ('yu', dum_iou, n_j, loc_id_yu)
    call sub_defdim ('lon_edges', dum_iou, n_i+1, loc_id_lon_e)
    call sub_defdim ('lat_edges', dum_iou, n_j+1, loc_id_lat_e)
    call sub_defdim ('xu_edges', dum_iou, n_i+1, loc_id_xu_e)
    call sub_defdim ('yu_edges', dum_iou, n_j+1, loc_id_yu_e)
    call sub_defdim ('para', dum_iou, const_integer_one, loc_id_misc)
    ! -------------------------------------------------------- ! define 1d data (t)
    loc_it(1) = loc_id_time
    call sub_defvar ('time', dum_iou, 1, loc_it, loc_c0, loc_c0, 'T', 'D' &
         &, 'Year', 'time', trim(loc_timunit))
    call sub_defvar ('year', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','year', ' ',' ')
    ! -------------------------------------------------------- ! define 1d data (x, y)
    loc_it(1) = loc_id_lonm
    call sub_defvar ('lon', dum_iou, 1, loc_it, loc_c0, loc_c0, 'X', 'D' , &
         &'longitude of the t grid', 'longitude', 'degrees_east')
    loc_it(1) = loc_id_latm
    call sub_defvar ('lat', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
         &'latitude of the t grid', 'latitude', 'degrees_north')
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
    loc_it(1) = loc_id_xu_e
    call sub_defvar ('xu_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
         &'longitude of u grid edges', ' ', 'degrees')
    loc_it(1) = loc_id_yu_e
    call sub_defvar ('yu_edges', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
         &'latitude of u grid edges', ' ', 'degrees')
    ! -------------------------------------------------------- ! close file
    call sub_enddef (dum_iou)
    call sub_closefile (dum_iou)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_netCDF_ncrstinit_OLD
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CREATE ATCHEM RESTART FILE
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_netCDF_ncrstsave_OLD(dum_yr)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    REAL,INTENT(in)::dum_yr
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    character(255) :: loc_name
    real           :: loc_c0, loc_c1
    integer        :: loc_i, loc_iou, loc_ntrec
    real,dimension(n_i+1) :: loc_lon_e, loc_xu_e
    real,dimension(n_j+1) :: loc_lat_e, loc_yu_e
    logical :: loc_defined
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_c0 = 0.0
    loc_c1 = 1.0
    !
    loc_lon_e(:)       = 0.0
    loc_lat_e(:)       = 0.0
    loc_xu_e(:)        = 0.0
    loc_yu_e(:)        = 0.0
    ! -------------------------------------------------------- ! set local netCDF variables
    loc_name = string_ncrst
    loc_iou = ncrst_iou
    loc_ntrec = ncrst_ntrec
    ! -------------------------------------------------------- ! open file and get latest record number
    loc_defined = .true.
    loc_i = 0
    if (loc_ntrec .eq. 0) then 
       loc_defined = .false.
       loc_i = 1
    end if
    call sub_opennext (loc_name, dum_yr, loc_i, loc_ntrec, loc_iou)
    ! -------------------------------------------------------- ! write 1d data: time
    call sub_putvars  ('time', loc_iou, loc_ntrec, dum_yr, loc_c1, loc_c0)
    call sub_putvarIs  ('year', loc_iou, loc_ntrec, nint(dum_yr), loc_c1, loc_c0)
    if(.not. loc_defined) then
       ! ----------------------------------------------------- ! write 1d data: x
       call sub_putvar1d ('lon', loc_iou, n_i, loc_ntrec, n_i,phys_atm(ipa_lon,:,1), loc_c1, loc_c0)
       call edge_maker (1, loc_lon_e, phys_atm(ipa_lon,:,1),phys_atm(ipa_lone,:,1), phys_atm(ipa_dlon,:,1), n_i)
       call sub_putvar1d ('lon_edges', loc_iou, n_i+1, loc_ntrec, n_i+1,loc_lon_e, loc_c1, loc_c0)
       call sub_putvar1d ('xu', loc_iou, n_i, loc_ntrec, n_i,loc_lon_e(1:n_i), loc_c1, loc_c0)
       call edge_maker (2, loc_xu_e, phys_atm(ipa_lon,:,1),phys_atm(ipa_lone,:,1), phys_atm(ipa_dlon,:,1), n_i)
       call sub_putvar1d ('xu_edges', loc_iou, n_i+1, loc_ntrec, n_i+1,loc_xu_e, loc_c1, loc_c0)
       ! ----------------------------------------------------- ! write 1d data: y
       call sub_putvar1d ('lat', loc_iou, n_j, loc_ntrec, n_j,phys_atm(ipa_lat,1,:), loc_c1, loc_c0)
       call edge_maker (1, loc_lat_e, phys_atm(ipa_lat,1,:),phys_atm(ipa_latn,1,:), phys_atm(ipa_dlat,1,:), n_j)
       call sub_putvar1d ('lat_edges', loc_iou, n_j+1, loc_ntrec, n_j+1,loc_lat_e, loc_c1, loc_c0)
       call sub_putvar1d ('yu', loc_iou, n_j, loc_ntrec, n_j,loc_lat_e(1:n_j), loc_c1, loc_c0)
       call edge_maker (2, loc_yu_e, phys_atm(ipa_lat,1,:),phys_atm(ipa_latn,1,:), phys_atm(ipa_dlat,1,:), n_j)
       call sub_putvar1d ('yu_edges', loc_iou, n_j+1, loc_ntrec, n_j+1,loc_yu_e, loc_c1, loc_c0)
    end if
    ! -------------------------------------------------------- ! 
    ncrst_ntrec = loc_ntrec
    ncrst_iou = loc_iou
    call sub_sync(loc_iou)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_netCDF_ncrstsave_OLD
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WRITE NETCDF RESTART DATA
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_netCDF_ncrstwrite_OLD()
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::l,ia
    INTEGER::loc_iou,loc_ntrec
    CHARACTER(len=255)::loc_unitsname
    real,DIMENSION(1:n_i,1:n_j)::loc_ij,loc_mask
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_iou = ncrst_iou
    loc_ntrec = ncrst_ntrec
    loc_mask(:,:) = 1.0
    loc_unitsname = 'atm'
    ! -------------------------------------------------------- !
    ! WRITE DATA
    ! -------------------------------------------------------- !
    DO l=1,n_l_atm
       ia = conv_iselected_ia(l)
       loc_ij(:,:) = atm(ia,:,:)
       call sub_adddef_netcdf(loc_iou,3,'atm_'//trim(string_atm_tname(l)), &
            & trim(string_atm_tlname(l)),trim(string_atm_unit(l)),atm_mima(l,1),atm_mima(l,2))
       call sub_putvar2d('atm_'//trim(string_atm(ia)),loc_iou,n_i,n_j, &
            & loc_ntrec,loc_ij(:,:),loc_mask(:,:))
    END DO
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_netCDF_ncrstwrite_OLD
  ! ****************************************************************************************************************************** !


END MODULE atchem_data_netCDF
