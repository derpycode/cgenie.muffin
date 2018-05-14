! ******************************************************************************************************************************** !
! sedgem_data_netCDF.f90
! DATA LOADING/SAVING ROUTINES
! ******************************************************************************************************************************** !


MODULE sedgem_data_netCDF

  
  use genie_control
  USE gem_cmn
  USE gem_util
  USE gem_netcdf
  USE sedgem_lib
  USE sedgem_box
  IMPLICIT NONE
  SAVE


CONTAINS
  
  
  ! ****************************************************************************************************************************** !
  ! SAVE NETCDF RESTART DATA
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_netCDF_ncrstsave(dum_name,dum_yr,dum_iou,dum_sfxocn)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    character(LEN=*),INTENT(IN)::dum_name                      ! 
    REAL,INTENT(in)::dum_yr                                    ! 
    INTEGER,INTENT(OUT)::dum_iou                               !
    real,DIMENSION(n_ocn,n_i,n_j),intent(in)::dum_sfxocn       ! sediment dissolution flux interface array
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::i,j,k,io,is,l
    integer::loc_k
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
    real,dimension(0:n_sed_tot)::loc_depth_e
    real,dimension(n_i,n_j)::loc_ij,loc_ij_mask
    real,dimension(n_i,n_j,n_sed_tot)::loc_ijk,loc_ijk_mask
    real,dimension(n_sed_tot)::loc_depth,loc_ddepth
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_c0 = 0.0
    loc_c1 = 1.0
    do k = 1,n_sed_tot
       loc_depth(k) = real(k) - 0.5
       loc_ddepth(k) = 1.0
    end do
    ! -------------------------------------------------------- !
    ! WRITE TO FILE
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! open file 
    call sub_opennew(dum_name,loc_iou)
    ! -------------------------------------------------------- ! start definitions
    call sub_redef(loc_iou)
    ! -------------------------------------------------------- ! set global attributes
    loc_string_year = fun_conv_num_char_n(8,int(dum_yr))
    loc_title = 'SEDGEM restart @ year '//loc_string_year
    call sub_putglobal(loc_iou,dum_name,loc_title,string_ncrunid,loc_timunit)
    ! -------------------------------------------------------- ! define dimensions
    call sub_defdim ('lon',loc_iou,n_i,loc_id_lonm)
    call sub_defdim ('lat',loc_iou,n_j,loc_id_latm)
    call sub_defdim ('lon_edges',loc_iou,n_i+1,loc_id_lon_e)
    call sub_defdim ('lat_edges',loc_iou,n_j+1,loc_id_lat_e)
    call sub_defdim('zt',loc_iou,n_sed_tot,loc_id_zt)
    call sub_defdim('zt_edges',loc_iou,n_sed_tot+1,loc_id_zt_e)
    ! -------------------------------------------------------- ! define 1d data (t)
    loc_it_1(1) = loc_id_lonm
    call sub_defvar ('lon', loc_iou, 1, loc_it_1, loc_c0, loc_c0, 'X', 'D' , &
         &'longitude of the t grid', 'longitude', 'degrees_east')
    loc_it_1(1) = loc_id_latm
    call sub_defvar ('lat', loc_iou, 1, loc_it_1, loc_c0, loc_c0, 'Y', 'D' , &
         &'latitude of the t grid', 'latitude', 'degrees_north')
    loc_it_1(1) = loc_id_lon_e
    call sub_defvar ('lon_edges', loc_iou, 1, loc_it_1, loc_c0, loc_c0, ' ', 'D' , &
         &'longitude of t grid edges', ' ', 'degrees')
    loc_it_1(1) = loc_id_lat_e
    call sub_defvar ('lat_edges', loc_iou, 1, loc_it_1, loc_c0, loc_c0, ' ', 'D' , &
         &'latitude of t grid edges', ' ', 'degrees')
    loc_it_1(1) = loc_id_zt
    call sub_defvar ('zt', loc_iou, 1, loc_it_1, loc_c0, loc_c0, 'Z', 'D' , &
         & 'depth of z grid', ' ', 'cm')
    loc_it_1(1) = loc_id_zt_e
    call sub_defvar ('zt_edges', loc_iou, 1, loc_it_1, loc_c0, loc_c0, ' ', 'D' , &
         & 'depth of z grid edges', ' ', 'cm')
    loc_it_2(1) = loc_id_lonm
    loc_it_2(2) = loc_id_latm
    loc_it_3(1) = loc_id_lonm
    loc_it_3(2) = loc_id_latm
    loc_it_3(3) = loc_id_zt
    ! -------------------------------------------------------- ! define (2D) stack variable
    call sub_defvar('phys_dh',loc_iou,2, &
         & loc_it_2,loc_c0,loc_c0,' ','F', &
         & 'Incomplete sediment stack layer thickness','Incomplete sediment stack layer thickness',' ')
    ! -------------------------------------------------------- ! define (2D) dissolution variables
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       call sub_defvar('fdis_'//trim(string_ocn(io)),loc_iou,2, &
            & loc_it_2,loc_c0,loc_c0,' ','F', &
            & string_longname_ocn(io),'Dissolution flux - '//trim(string_ocn(io)),' ')
    end DO
    ! -------------------------------------------------------- ! define (3D) tracer variables
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       call sub_defvar('sed_'//trim(string_sed(is)),loc_iou,3, &
            & loc_it_3,loc_c0,loc_c0,' ','F', &
            & string_longname_sed(is),'Sediment tracer - '//trim(string_sed(is)),' ')
    end do
    ! -------------------------------------------------------- ! end definitions
    call sub_enddef (loc_iou)
    call sub_sync(loc_iou)
    ! -------------------------------------------------------- ! 
    loc_ntrec = 1
    ! -------------------------------------------------------- ! write 1D variables
    call sub_putvar1d ('lon', loc_iou, n_i, loc_ntrec, n_i, &
         & phys_sed(ips_lon,:,1), loc_c1, loc_c0)
    call edge_maker (1, loc_lon_e, phys_sed(ips_lon,:,1), &
         & phys_sed(ips_lone,:,1), phys_sed(ips_dlon,:,1), n_i)
    call sub_putvar1d ('lon_edges', loc_iou, n_i+1, loc_ntrec, &
         & n_i+1, loc_lon_e, loc_c1, loc_c0)
    call sub_putvar1d ('lat', loc_iou, n_j, loc_ntrec, n_j, &
         & phys_sed(ips_lat,1,:), loc_c1, loc_c0)
    call edge_maker (1, loc_lat_e, phys_sed(ips_lat,1,:), &
         & phys_sed(ips_latn,1,:), phys_sed(ips_dlat,1,:), n_j)
    call sub_putvar1d ('lat_edges', loc_iou, n_j+1, loc_ntrec, &
         & n_j+1, loc_lat_e, loc_c1, loc_c0)
    call sub_putvar1d ('zt', loc_iou, n_sed_tot, loc_ntrec, n_sed_tot, &
         & loc_depth(:), loc_c1, loc_c0)
    call edge_maker (1, loc_depth_e, loc_depth(:), &
         & loc_depth(:)+0.5, loc_ddepth(:), n_sed_tot)
    call sub_putvar1d ('zt_edges', loc_iou, n_sed_tot+1, loc_ntrec, &
         & n_sed_tot+1, loc_depth_e, loc_c1, loc_c0)
    ! -------------------------------------------------------- ! write (2D) top stack layer thickness
    loc_ij_mask(:,:) = phys_sed(ips_mask_sed,:,:)
    loc_ij(:,:)= 0.0
    DO i = 1,n_i
       DO j = 1,n_j
          if (sed_mask(i,j)) then
             loc_ij(i,j) = sed_top_h(i,j) - REAL(INT(sed_top_h(i,j)))
          endif
       end DO
    END DO
    call sub_putvar2d('phys_dh',loc_iou, &
         & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_ij_mask(:,:))
    ! -------------------------------------------------------- ! write (2D) dissolution flux variables
    loc_ij_mask(:,:) = 1.0
    loc_ij(:,:)= 0.0
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       loc_ij(:,:) = dum_sfxocn(conv_iselected_io(l),:,:)
       call sub_putvar2d('fdis_'//trim(string_ocn(io)),loc_iou, &
            & n_i,n_j,loc_ntrec,loc_ij(:,:),loc_ij_mask(:,:))
    end DO
    ! -------------------------------------------------------- ! write (3D) tracer variables
    ! Note that in indexing the netCDF sedcore array, it is assumed that the uppermost stack layer is always empty
    ! => the dimension of the netCDF sedcore array == stack array - 1 + 1 (sedtop layer)
    loc_ijk_mask(:,:,:)= 0.0
    do k = 1,n_sed_tot
       loc_ijk_mask(:,:,k) = phys_sed(ips_mask_sed,:,:)
    enddo
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ijk(:,:,:) = 0.0
       DO i = 1,n_i
          DO j = 1,n_j
             if (sed_mask(i,j)) then
                loc_ijk(i,j,1) = sed_top(is,i,j)
                loc_k = INT(sed_top_h(i,j)) + 1                ! stack number of uppermost (incomplete) layer
                do k = 1,loc_k
                   loc_ijk(i,j,k+1) = sed(is,i,j,(loc_k - k + 1))
                end do
             end if
          end DO
       END DO
       call sub_putvar3d('sed_'//trim(string_sed(is)),loc_iou, &
            & n_i,n_j,n_sed_tot,loc_ntrec,loc_ijk(:,:,:),loc_ijk_mask(:,:,:))
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
  ! SAVE NETCDF RESTART DATA
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_netCDF_sedcoresave(dum_namein,dum_nameout,dum_yr,dum_iou)
    USE genie_util, ONLY: check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    character(LEN=*),INTENT(IN)::dum_namein                    ! 
    character(LEN=*),INTENT(IN)::dum_nameout                   ! 
    REAL,INTENT(in)::dum_yr                                    ! 
    INTEGER,INTENT(OUT)::dum_iou                               !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,ll,m,mm,o,oo
    integer::is
    integer::loc_i,loc_j,loc_ii,loc_jj
    integer::loc_o,loc_oo                                      ! 
    ! -------------------------------------------------------- ! netCDF: out (save)
    integer::loc_ntrec,loc_iou
    integer::loc_id_latm,loc_id_lat_e
    integer::loc_id_zt,loc_id_zt_e
    integer,dimension(1:2)::loc_it_1
    integer,dimension(1:3)::loc_it_2
    character(127)::loc_title,loc_timunit
    character(127)::loc_string,loc_string_longname
    character(7)::loc_string_year
    real::loc_c0,loc_c1
    real,dimension(nv_sedcore)::loc_lat,loc_dlat
    real,dimension(0:nv_sedcore)::loc_lat_e
    real,DIMENSION(:),ALLOCATABLE::loc_depth_e                 ! 
    real,DIMENSION(:),ALLOCATABLE::loc_depth,loc_ddepth        ! 
    real,dimension(:,:),ALLOCATABLE::loc_mk,loc_mk_mask
    integer,dimension(:),ALLOCATABLE::loc_m
    ! -------------------------------------------------------- ! netCDF: in (restart)
    integer::ios                                               !
    integer::loc_ncid
    integer::loc_ndims,loc_nvars
    integer::loc_nv_sedcore_rst
    integer::loc_n_sedcore_tot_rst
    integer::loc_n_sedcore_tracer_rst
    integer,ALLOCATABLE,dimension(:)::loc_dimlen
    integer,ALLOCATABLE,dimension(:,:)::loc_varlen
    integer,ALLOCATABLE,dimension(:)::loc_vdims
    character(20),ALLOCATABLE,dimension(:)::loc_varname
    integer,dimension(:),ALLOCATABLE::loc_m_in
    real,dimension(:,:),ALLOCATABLE::loc_mk_in
    integer,dimension(:),ALLOCATABLE::loc_nv_sed_stack_top_rst
    type(fieldsedcore),DIMENSION(:),ALLOCATABLE::loc_vsedcore_rst
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_layer_rst
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_depth_rst
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_poros_rst     !
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_th_rst        !
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_age_cal_rst   ! 
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_age_det_rst   !
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_age_ash_rst   !
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_age_14C_rst
    ! -------------------------------------------------------- ! sedcore calculations
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_layer
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_depth
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_poros         !
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_th            !
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_age_cal       ! 
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_age_det       ! 
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_age_ash       !
    real,dimension(:,:),ALLOCATABLE::loc_sedcore_age_14C
    integer::loc_n_sedcore_tot
    integer::loc_n_sedcore_store_tot
    integer::loc_n_sed_stack_tot
    integer,dimension(nv_sedcore)::loc_n_sedcore_store_top     ! 
    type(fieldsedcore),DIMENSION(:),ALLOCATABLE::loc_vsedcore  !
    integer,dimension(n_i,n_j)::loc_n_sed_stack_top
    real,dimension(n_i,n_j)::loc_sed_stack_top_th
    real::loc_sed_tot_vol,loc_sed_tot_wt
    integer::loc_ash_max_o
    real::loc_ash_max
    real::loc_ash_max_depth
    real::loc_ash_conv_dbs_age
    real,dimension(n_sed)::loc_sed                             !
    real::loc_tot,loc_frac,loc_standard,loc_delta              !
    real::loc_CaCO3_D14C
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    IF (ctrl_misc_debug4) print*,'*** INITIALIZE LOCAL VARIABLES ***'
    ! -------------------------------------------------------- ! determine number of layers in sedcore store
    ! NOTE: do not add '1' as the height should be an exact multiple of 1.0
    !       (adding the const_real_nullsmall for numerical safety in calculating the integer part)
    do m = 1,nv_sedcore
       loc_n_sedcore_store_top(m) = INT(vsedcore_store(m)%ht + const_real_nullsmall)
    end do
    loc_n_sedcore_store_tot = maxval(loc_n_sedcore_store_top(:))
    ! -------------------------------------------------------- ! determine number of layers in sed stack
    ! NOTE: disable sedcores that have been masked out at runtime (e.g. ones falling within the land grid)
    loc_n_sed_stack_top(:,:) = 0
    loc_sed_stack_top_th(:,:) = 0
    do m=1,nv_sedcore
       loc_i = vsedcore_store(m)%i
       loc_j = vsedcore_store(m)%j
       if (sed_save_mask(loc_i,loc_j)) then
          loc_n_sed_stack_top(loc_i,loc_j) = INT(sed_top_h(loc_i,loc_j)) + 1
          loc_sed_stack_top_th(loc_i,loc_j) = sed_top_h(loc_i,loc_j) - REAL(loc_n_sed_stack_top(loc_i,loc_j) - 1)
       else
          loc_n_sed_stack_top(loc_i,loc_j) = 1
          loc_sed_stack_top_th(loc_i,loc_j) = 0.0
          vsedcore_store(m)%save = .false.
       end if
    end DO
    loc_n_sed_stack_tot = maxval(loc_n_sed_stack_top(:,:))
    ! -------------------------------------------------------- !
    ! LOAD ANY RESTART DATA
    ! -------------------------------------------------------- !
    IF (ctrl_misc_debug4) print*,'*** LOAD ANY RESTART DATA ***'
    ! ----------------------------------------------------- ! check file status
    IF (ctrl_continuing) then
       call check_unit(in,__LINE__,__FILE__)
       OPEN(unit=in,status='old',file=TRIM(dum_namein),form='unformatted',action='read',IOSTAT=ios)
       If (ios /= 0) then
          ! -------------------------------------------------- ! file does not exist (or other error possible)
          CALL sub_report_error( &
               & 'sedgem_data_netCDF','sub_data_netCDF_sedcoresave', &
               & 'You have requested a CONTINUING run, but there is no sedcore restart file', &
               & 'SKIPPING - creating sedcores from scratch', &
               & (/const_real_null/),.false. &
               & )
          loc_n_sedcore_tot_rst = 0
          ctrl_continuing = .false.
       else
          close(unit=in,iostat=ios)
          call check_iostat(alloc_error,__LINE__,__FILE__)
       end if
    end if
    IF (ctrl_continuing) then
       ! -------------------------------------------------------- ! open netCDF file
       call sub_openfile(TRIM(dum_namein),loc_ncid)
       ! -------------------------------------------------------- ! determine number of variables
       call sub_inqdims(TRIM(dum_namein),loc_ncid,loc_ndims,loc_nvars)
       ! -------------------------------------------------------- ! allocate arrays: netCDF
       ALLOCATE(loc_dimlen(loc_ndims),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_varlen(2,loc_nvars),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_vdims(loc_nvars),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_varname(loc_nvars),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ! -------------------------------------------------------- ! get variable dimensions
       call sub_inqvars(loc_ncid,loc_ndims,loc_nvars,loc_dimlen,loc_varname,loc_vdims,loc_varlen)
       loc_nv_sedcore_rst = loc_dimlen(1)
       loc_n_sedcore_tot_rst = loc_dimlen(3)
       loc_n_sedcore_tracer_rst = loc_nvars
       ! -------------------------------------------------------- ! allocate arrays: temp arrays
       ALLOCATE(loc_m_in(1:loc_nv_sedcore_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_mk_in(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ! -------------------------------------------------------- ! allocate arrays: sedcore restart load/save arrays
       ALLOCATE(loc_sedcore_layer_rst(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_sedcore_depth_rst(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_sedcore_poros_rst(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_sedcore_th_rst(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_sedcore_age_cal_rst(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_sedcore_age_det_rst(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_sedcore_age_ash_rst(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_sedcore_age_14C_rst(1:loc_nv_sedcore_rst,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(loc_nv_sed_stack_top_rst(1:loc_nv_sedcore_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ! NOTE: allocate %top even if not used
       ! NOTE: dimension to number of *current* tracers, not restart sedcore number
       ALLOCATE(loc_vsedcore_rst(1:loc_nv_sedcore_rst),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       do mm=1,loc_nv_sedcore_rst
          allocate(loc_vsedcore_rst(mm)%top(1:n_sedcore_tracer),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          allocate(loc_vsedcore_rst(mm)%lay(1:n_sedcore_tracer,1:loc_n_sedcore_tot_rst),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
       end do
       ! -------------------------------------------------------- ! recover (i,j) information & initialize arrays
       loc_sedcore_depth_rst(:,:) = 0.0
       loc_sedcore_poros_rst(:,:) = 0.0
       loc_sedcore_th_rst(:,:) = 0.0
       loc_sedcore_age_cal_rst(:,:) = 0.0
       loc_sedcore_age_det_rst(:,:) = 0.0
       loc_sedcore_age_ash_rst(:,:) = 0.0
       loc_sedcore_age_14C_rst(:,:) = 0.0
       loc_nv_sed_stack_top_rst(:) = 0
       call sub_getvar1d_I(loc_ncid,'grid_i',loc_nv_sedcore_rst,loc_m_in(:))
       loc_vsedcore_rst(:)%i = loc_m_in(:)
       call sub_getvar1d_I(loc_ncid,'grid_j',loc_nv_sedcore_rst,loc_m_in(:))
       loc_vsedcore_rst(:)%j = loc_m_in(:)
       DO mm=1,loc_nv_sedcore_rst
          loc_vsedcore_rst(mm)%ht = 0.0
          loc_vsedcore_rst(mm)%top(:) = 0.0
          loc_vsedcore_rst(mm)%lay(:,:) = 0.0
       end do
       ! load sedcore stack top information (this is the stack top state at the time of restart (run beginning)
       ! and hence allows the layers in the sedcore restart that equate to actively used layers implicitly included in the
       ! current stack or sedcore store, to be omitted) (did anypony get all that?)
       call sub_getvar1d_I(loc_ncid,'grid_nrst',loc_nv_sedcore_rst,loc_m_in(:))
       loc_nv_sed_stack_top_rst(:) = loc_m_in(:)
       ! -------------------------------------------------------- ! load and apply sediment tracers that are selected
       ! only copy tracer variables that are currently selected
       ! NOTE: the sed core restart arrays are on the restart m,k (sedcores x sed layers) grid, not necessarily the current one
       !       BUT the selected sediment tracer dimension ([loc_vsedcore_rst]) is current
       ! NOTE: the k dimension is flipped in sub_getvarijk and sub_getvarijk (WHY, for the love of pony?)
       DO ll=1,loc_n_sedcore_tracer_rst
          DO l=1,n_l_sed
             is = conv_iselected_is(l)
             if (trim(loc_varname(ll)) == 'sed_'//trim(string_sed(is))) then
                loc_mk_in(:,:) = 0.0
                call sub_getvarjk(loc_ncid,'sed_'//trim(string_sed(is)),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
                do mm=1,loc_nv_sedcore_rst
                   loc_vsedcore_rst(mm)%lay(l,:) = loc_mk_in(mm,loc_n_sedcore_tot_rst:1:-1)
                enddo
             endif
          end do
       end do
       DO ll=1,loc_n_sedcore_tracer_rst
          if (trim(loc_varname(ll)) == 'phys_layer') then
             loc_mk_in(:,:) = 0.0
             call sub_getvarjk(loc_ncid,loc_varname(ll),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
             loc_sedcore_layer_rst(:,:) = loc_mk_in(:,loc_n_sedcore_tot_rst:1:-1)
          elseif (trim(loc_varname(ll)) == 'phys_depth') then
             loc_mk_in(:,:) = 0.0
             call sub_getvarjk(loc_ncid,loc_varname(ll),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
             loc_sedcore_depth_rst(:,:) = loc_mk_in(:,loc_n_sedcore_tot_rst:1:-1)
          elseif (trim(loc_varname(ll)) == 'phys_thickness') then
             loc_mk_in(:,:) = 0.0
             call sub_getvarjk(loc_ncid,loc_varname(ll),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
             loc_sedcore_th_rst(:,:) = loc_mk_in(:,loc_n_sedcore_tot_rst:1:-1)
          elseif (trim(loc_varname(ll)) == 'phys_porosity') then
             loc_mk_in(:,:) = 0.0
             call sub_getvarjk(loc_ncid,loc_varname(ll),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
             loc_sedcore_poros_rst(:,:) = loc_mk_in(:,loc_n_sedcore_tot_rst:1:-1)
          elseif (trim(loc_varname(ll)) == 'age_CaCO3') then
             loc_mk_in(:,:) = 0.0
             call sub_getvarjk(loc_ncid,loc_varname(ll),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
             loc_sedcore_age_cal_rst(:,:) = loc_mk_in(:,loc_n_sedcore_tot_rst:1:-1)
          elseif (trim(loc_varname(ll)) == 'age_det') then
             loc_mk_in(:,:) = 0.0
             call sub_getvarjk(loc_ncid,loc_varname(ll),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
             loc_sedcore_age_det_rst(:,:) = loc_mk_in(:,loc_n_sedcore_tot_rst:1:-1)
          elseif (trim(loc_varname(ll)) == 'age_ash') then
             loc_mk_in(:,:) = 0.0
             call sub_getvarjk(loc_ncid,loc_varname(ll),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
             loc_sedcore_age_ash_rst(:,:) = loc_mk_in(:,loc_n_sedcore_tot_rst:1:-1)
          elseif (trim(loc_varname(ll)) == 'age_14C') then
             loc_mk_in(:,:) = 0.0
             call sub_getvarjk(loc_ncid,loc_varname(ll),loc_nv_sedcore_rst,loc_n_sedcore_tot_rst,loc_mk_in(:,:))
             loc_sedcore_age_14C_rst(:,:) = loc_mk_in(:,loc_n_sedcore_tot_rst:1:-1)
          end if
       end do
       ! add current run duration to ages
       ! NOTE: check for non-zero bulk for tied chronologies
       ! NOTE: the underlying bulk composition-weighted ages are *not* updated here
       !       (this can be confusing ... maybe they should be also updated?)
       do mm=1,loc_nv_sedcore_rst
          do oo=1,loc_n_sedcore_tot_rst
             loc_sedcore_age_ash_rst(mm,oo) = loc_sedcore_age_ash_rst(mm,oo) + par_misc_t_runtime
             if (loc_vsedcore_rst(mm)%lay(conv_is_lselected(is_CaCO3),oo) > const_real_nullsmall) then
                loc_sedcore_age_cal_rst(mm,oo) = loc_sedcore_age_cal_rst(mm,oo) + par_misc_t_runtime
                loc_sedcore_age_14C_rst(mm,oo) = loc_sedcore_age_14C_rst(mm,oo) + par_misc_t_runtime
             end if
             if (loc_vsedcore_rst(mm)%lay(conv_is_lselected(is_det),oo) > const_real_nullsmall) then
                loc_sedcore_age_det_rst(mm,oo) = loc_sedcore_age_det_rst(mm,oo) + par_misc_t_runtime
             end if
          end do
       end do
       ! -------------------------------------------------------- ! deallocate arrays (except loc_vsedcore_rst)
       DEALLOCATE(loc_m_in,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_mk_in,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       deALLOCATE(loc_dimlen,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       deALLOCATE(loc_varlen,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       deALLOCATE(loc_vdims,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       deALLOCATE(loc_varname,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ! -------------------------------------------------------- ! close file
       call sub_closefile(loc_ncid)
    else
       loc_n_sedcore_tot_rst = 0
    END IF
    ! -------------------------------------------------------- !
    ! CONSTRUCT SEDCORE ARRAY FOR WRITING
    ! -------------------------------------------------------- !
    IF (ctrl_misc_debug4) print*,'*** CONSTRUCT SEDCORE ARRAY FOR WRITING ***'
    ! -------------------------------------------------------- ! set number of layers required
    ! NOTE: 1 (additional) layer is required for the sedtop
    ! NOTE: include sedcore restart, BUT, remove maximum # discarded layers
    IF (ctrl_continuing) then
       loc_n_sedcore_tot = loc_n_sedcore_store_tot + loc_n_sed_stack_tot + 1 + &
            & loc_n_sedcore_tot_rst - maxval(loc_nv_sed_stack_top_rst(:))
    else
       loc_n_sedcore_tot = loc_n_sedcore_store_tot + loc_n_sed_stack_tot + 1
    end if
    ! -------------------------------------------------------- ! dimension local sedcore arrays
    ! NOTE: allocate %top even if not used
    ALLOCATE(loc_vsedcore(1:nv_sedcore),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    do m=1,nv_sedcore
       allocate(loc_vsedcore(m)%top(1:n_sedcore_tracer),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       allocate(loc_vsedcore(m)%lay(1:n_sedcore_tracer,1:loc_n_sedcore_tot),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
    end do
    ! -------------------------------------------------------- ! initialize sedcores
    DO m=1,nv_sedcore
       loc_vsedcore(m)%ht = 0.0
       loc_vsedcore(m)%top(:) = 0.0
       loc_vsedcore(m)%lay(:,:) = 0.0
    end do
    loc_vsedcore(:)%i = vsedcore_store(:)%i
    loc_vsedcore(:)%j = vsedcore_store(:)%j
    ! -------------------------------------------------------- ! populate sedcore output array with current sedstack
    DO m = 1,nv_sedcore
       loc_i = loc_vsedcore(m)%i
       loc_j = loc_vsedcore(m)%j
       loc_o = loc_n_sed_stack_top(loc_i,loc_j)
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          DO o = 1,loc_o
             loc_vsedcore(m)%lay(l,loc_o - o + 2) = sed(is,loc_i,loc_j,o)
          end do
          loc_vsedcore(m)%lay(l,1) = sed_top(is,loc_i,loc_j)
       END DO
    end do
    ! -------------------------------------------------------- ! populate sedcore output array with sedcore store
    ! NOTE: check for zero size of sedcore store at each location
    DO m = 1,nv_sedcore
       if (loc_n_sedcore_store_top(m) > 0) then
          loc_i = loc_vsedcore(m)%i
          loc_j = loc_vsedcore(m)%j
          loc_o = loc_n_sed_stack_top(loc_i,loc_j)
          DO l=1,n_l_sed
             is = conv_iselected_is(l)
             DO o = 1,loc_n_sedcore_store_top(m)
                loc_vsedcore(m)%lay(l,loc_n_sedcore_store_top(m) + loc_o - o + 2) = vsedcore_store(m)%lay(l,o)
             end do
          END DO
       end if
    end do
    ! -------------------------------------------------------- !
    ! CONVERT SEDCORE DATA TO OUTPUT FORMAT
    ! -------------------------------------------------------- !
    IF (ctrl_misc_debug4) print*,'*** CONVERT SEDCORE DATA TO OUTPUT FORMAT ***'
    ! -------------------------------------------------------- ! allocate local arrays
    ALLOCATE(loc_sedcore_layer(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sedcore_depth(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sedcore_poros(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sedcore_th(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sedcore_age_cal(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sedcore_age_det(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sedcore_age_ash(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sedcore_age_14C(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! -------------------------------------------------------- ! initialize local arrays
    loc_sedcore_depth(:,:) = 0.0
    loc_sedcore_poros(:,:) = 0.0
    loc_sedcore_th(:,:) = 0.0
    loc_sedcore_age_cal(:,:) = 0.0
    loc_sedcore_age_det(:,:) = 0.0
    loc_sedcore_age_ash(:,:) = 0.0
    loc_sedcore_age_14C(:,:) = 0.0
    ! -------------------------------------------------------- ! set equivalent (i,j) grid point
    DO m = 1,nv_sedcore
       loc_i = loc_vsedcore(m)%i
       loc_j = loc_vsedcore(m)%j
       ! ----------------------------------------------------- ! (a) calculate sediment depth
       o = 1
       loc_sedcore_depth(m,o) = par_sed_top_th/2.0
       o = 2
       loc_sedcore_depth(m,o) = par_sed_top_th + loc_sed_stack_top_th(loc_i,loc_j)/2.0
       DO o = 3,loc_n_sedcore_tot
          loc_sedcore_depth(m,o) = par_sed_top_th + loc_sed_stack_top_th(loc_i,loc_j) + REAL((o - 3)) + 0.5
       end DO
       ! ----------------------------------------------------- ! (b) calculate porosity and actual layer thickness
       ! NOTE: actual layer thickness is calculated as a reality check
       ! NOTE: currently assume mud porosity the same as pelagic sediments
       o = 1
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_sed(is) = loc_vsedcore(m)%lay(l,o)
       end do
       loc_sed_tot_vol = fun_calc_sed_vol(loc_sed(:))
       if (loc_sed_tot_vol > const_real_nullsmall) then
          if (sed_mask_reef(loc_i,loc_j)) then
             loc_sedcore_poros(m,o) = par_sed_poros_CaCO3_reef
          elseif (sed_mask_muds(loc_i,loc_j)) then
             loc_sedcore_poros(m,o) = fun_calc_sed_poros_nsur(loc_vsedcore(m)%lay(is2l(is_CaCO3),o)/loc_sed_tot_vol,par_sed_top_th)
          else
             loc_sedcore_poros(m,o) = fun_calc_sed_poros_nsur(loc_vsedcore(m)%lay(is2l(is_CaCO3),o)/loc_sed_tot_vol,par_sed_top_th)
          end if
          loc_sedcore_th(m,o) = loc_sed_tot_vol/(1.0 - loc_sedcore_poros(m,o))
       else
          loc_sedcore_poros(m,o) = 1.0
          loc_sedcore_th(m,o)    = 0.0
       end if
       DO o = 2,loc_n_sedcore_tot
          DO l=1,n_l_sed
             is = conv_iselected_is(l)
             loc_sed(is) = loc_vsedcore(m)%lay(l,o)
          end do
          loc_sed_tot_vol = fun_calc_sed_vol(loc_sed(:))
          if (loc_sed_tot_vol > const_real_nullsmall) then
             if (sed_mask_reef(loc_i,loc_j)) then
                loc_sedcore_poros(m,o) = par_sed_poros_CaCO3_reef
             elseif (sed_mask_muds(loc_i,loc_j)) then
                loc_sedcore_poros(m,o) = fun_calc_sed_poros(loc_vsedcore(m)%lay(is2l(is_CaCO3),o)/loc_sed_tot_vol)
             else
                loc_sedcore_poros(m,o) = fun_calc_sed_poros(loc_vsedcore(m)%lay(is2l(is_CaCO3),o)/loc_sed_tot_vol)
             end if
             loc_sedcore_th(m,o) = loc_sed_tot_vol/(1.0 - loc_sedcore_poros(m,o))
          else
             loc_sedcore_poros(m,o) = 1.0
             loc_sedcore_th(m,o)    = 0.0
          end if
       end DO
       ! ----------------------------------------------------- ! (c) calculate numerical internal ages
       DO o = 1,loc_n_sedcore_tot
          if (sed_select(is_CaCO3_age)) then
             IF (loc_vsedcore(m)%lay(is2l(is_CaCO3),o) > const_real_nullsmall) THEN
                loc_sedcore_age_cal(m,o) = loc_vsedcore(m)%lay(is2l(is_CaCO3_age),o)/loc_vsedcore(m)%lay(is2l(is_CaCO3),o)
             ELSE
                loc_sedcore_age_cal(m,o) = 0.0
             ENDIF
          end if
          if (sed_select(is_det_age)) then
             IF (loc_vsedcore(m)%lay(is2l(is_det),o) > const_real_nullsmall) THEN
                loc_sedcore_age_det(m,o) = loc_vsedcore(m)%lay(is2l(is_det_age),o)/loc_vsedcore(m)%lay(is2l(is_det),o)
             ELSE
                loc_sedcore_age_det(m,o) = 0.0
             ENDIF
          end if
       ENDDO
       ! ----------------------------------------------------- ! (d) normailze solid sediment components
       ! NOTE: treat stable isotopes in a same manner
       ! NOTE: as a first step, calculate total mass of solid components in the sediment sub-layer
       ! NOTE: un-do ash tracer conversion (leaving as cm3 per layer)
       DO o = 1,loc_n_sedcore_tot
          loc_sed(:) = 0.0
          DO l=1,n_l_sed
             is = conv_iselected_is(l)
             loc_sed(is) = loc_vsedcore(m)%lay(l,o)
          end do
          IF (ctrl_data_save_wtfrac) THEN
             loc_sed_tot_wt = fun_calc_sed_mass(loc_sed(:))
             IF (loc_sed_tot_wt > const_real_nullsmall) THEN
                loc_sed(:) = conv_sed_cm3_g(:)*loc_sed(:)/loc_sed_tot_wt
                loc_sed(is_ash) = loc_sed_tot_wt*loc_sed(is_ash)/conv_sed_cm3_g(is_ash)
             end IF
          else
             loc_sed_tot_vol = fun_calc_sed_vol(loc_sed(:))
             IF (loc_sed_tot_vol > const_real_nullsmall) THEN
                loc_sed(:) = loc_sed(:)/loc_sed_tot_vol
                loc_sed(is_ash) = loc_sed_tot_vol*loc_sed(is_ash)
             end IF
          end if
          DO l=1,n_l_sed
             is = conv_iselected_is(l)
             loc_vsedcore(m)%lay(l,o) = loc_sed(is)
          end do
       END DO
       ! ----------------------------------------------------- ! (e) produce stratigraphic marker age scale
       ! NOTE: this assumes that the maximum ash volume fraction represents the ash impulse deposition age
       !       and that the sediment ages inbetween this depth and the surface can be linearly interpolated
       ! NOTE: sediment deeper then the ash maximum is aged by linear extrapolation
       ! NOTE: first, the ash maximum must be found
       ! NOTE: once the first maximum has been passed then stop searching,
       !       because there may be other maxima deeper down ...
       ! NOTE: search only frmo layer #3 onwards (becasue ash corrected for layer thickness)
       loc_ash_max = 0.0
       loc_ash_max_o = 0
       DO o = 3,loc_n_sedcore_tot
          IF (loc_vsedcore(m)%lay(is2l(is_ash),o) > (loc_ash_max + const_real_nullsmall)) THEN
             loc_ash_max   = loc_vsedcore(m)%lay(is2l(is_ash),o)
             loc_ash_max_o = o
          ENDIF
          IF (loc_vsedcore(m)%lay(is2l(is_ash),o) < (loc_ash_max - const_real_nullsmall)) exit
       END DO
       ! calculate ash maximum depth
       SELECT CASE (loc_ash_max_o)
       CASE (0)
          loc_ash_max_depth = par_sed_top_th/2.0
       CASE (1)
          loc_ash_max_depth = par_sed_top_th + loc_sed_stack_top_th(loc_i,loc_j)/2.0
       CASE default
          loc_ash_max_depth = par_sed_top_th + loc_sed_stack_top_th(loc_i,loc_j) + REAL((loc_ash_max_o - 2)) + 0.5
       END SELECT
       ! calculate linear age-depth relation
       loc_ash_conv_dbs_age = par_misc_t_runtime/loc_ash_max_depth
       ! generate age scale
       o = 1
       loc_sedcore_age_ash(m,o) = loc_ash_conv_dbs_age*(par_sed_top_th/2.0)
       o = 2
       loc_sedcore_age_ash(m,o) = loc_ash_conv_dbs_age*(par_sed_top_th + loc_sed_stack_top_th(loc_i,loc_j)/2.0)
       DO o = 3,loc_n_sedcore_tot
          loc_sedcore_age_ash(m,o) = loc_ash_conv_dbs_age * &
               (par_sed_top_th + loc_sed_stack_top_th(loc_i,loc_j) + REAL((o - 2)) + 0.5)
       END DO
       ! ----------------------------------------------------- ! (f) calculate isotopic values in 'per mil' units
       ! NOTE: filter the result to remove the 'null' value when a delta cannot be calculated
       !       because this will screw up writing in the ASCII format later
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          SELECT CASE (sed_type(is))
          case (n_itype_min:n_itype_max)
             DO o = 1,loc_n_sedcore_tot
                loc_tot  = loc_vsedcore(m)%lay(is2l(sed_dep(is)),o)
                loc_frac = loc_vsedcore(m)%lay(l,o)
                loc_standard = const_standards(sed_type(is))
                loc_delta = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                If (loc_delta == const_real_null) then
                   loc_vsedcore(m)%lay(l,o) = const_real_zero
                else
                   loc_vsedcore(m)%lay(l,o) = loc_delta
                end If
             end DO
          end SELECT
       end do
       ! ----------------------------------------------------- ! (g) normalize trace elements to bulk
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          SELECT CASE (sed_type(is))
          case (par_sed_type_CaCO3)
             DO o = 1,loc_n_sedcore_tot
                IF (loc_vsedcore(m)%lay(is2l(sed_dep(is)),o) > const_real_nullsmall) THEN
                   loc_vsedcore(m)%lay(l,o) = loc_vsedcore(m)%lay(l,o)/loc_vsedcore(m)%lay(is2l(sed_dep(is)),o)
                else
                   loc_vsedcore(m)%lay(l,o) = 0.0
                end IF
             end DO
          end SELECT
       end do
       ! ----------------------------------------------------- ! (h) convert mass or volume fraction to % units
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          SELECT CASE (sed_type(is))
          case (par_sed_type_bio,par_sed_type_abio)
             DO o = 1,loc_n_sedcore_tot
                if (l /= is2l(is_ash)) then
                   loc_vsedcore(m)%lay(l,o) = 100.0*loc_vsedcore(m)%lay(l,o)
                end if
             end DO
          end SELECT
       end do
       ! ----------------------------------------------------- ! (i) calculate D14C and radiocarbon age
       ! NOTE: this will be saved regardless of whether 14C is a included tracer in the model or not ...
       loc_sedcore_age_14C(m,:) = const_real_zero
       if (sed_select(is_CaCO3_14C)) then
          DO o = 1,loc_n_sedcore_tot
             IF (loc_vsedcore(m)%lay(is2l(is_CaCO3),o) > const_real_nullsmall) THEN
                loc_CaCO3_D14C = fun_convert_delta14CtoD14C( &
                     &   loc_vsedcore(m)%lay(is2l(is_CaCO3_13C),o), &
                     &   loc_vsedcore(m)%lay(is2l(is_CaCO3_14C),o) &
                     & )
                loc_sedcore_age_14C(m,o) = &
                     & fun_convert_D14Ctoage(loc_CaCO3_D14C)
             end if
          end DO
       end if
       ! ----------------------------------------------------- ! (j) assign a layer number
       DO o = 1,loc_n_sedcore_tot
          loc_sedcore_layer(:,o) = real(o)
       end DO
       ! ----------------------------------------------------- !
    end DO
    ! -------------------------------------------------------- !
    ! ADD SEDCORE RESTART DATA
    ! -------------------------------------------------------- !
    ! NOTE: increment layer number of restart sedcore layers
    !       also: increase depth of restart sedcore layers
    IF (ctrl_misc_debug4) print*,'*** ADD SEDCORE RESTART DATA ***'
    IF (ctrl_continuing) then
       if (loc_n_sedcore_tot_rst > 0) then
          DO mm=1,loc_nv_sedcore_rst
             if (vsedcore_store(mm)%save) then
                loc_ii = loc_vsedcore_rst(mm)%i
                loc_jj = loc_vsedcore_rst(mm)%j
                do m = 1,nv_sedcore
                   loc_i = loc_vsedcore(m)%i
                   loc_j = loc_vsedcore(m)%j
                   if (loc_ii == loc_i .AND. loc_jj == loc_j) then
                      loc_o = loc_n_sedcore_store_top(m) + loc_n_sed_stack_top(loc_i,loc_j) + 1
                      loc_oo = loc_nv_sed_stack_top_rst(mm)
                      loc_vsedcore(m)%lay(1:n_sedcore_tracer,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_vsedcore_rst(mm)%lay(1:n_sedcore_tracer,loc_oo + 1:loc_n_sedcore_tot_rst) 
                      loc_sedcore_layer(m,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_sedcore_layer_rst(mm,loc_oo + 1:loc_n_sedcore_tot_rst) + &
                           & (loc_o - loc_oo)
                      loc_sedcore_depth(m,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_sedcore_depth_rst(mm,loc_oo + 1:loc_n_sedcore_tot_rst) + &
                           & real(loc_o - loc_oo) + loc_sed_stack_top_th(loc_i,loc_j) - (loc_sedcore_depth_rst(mm,2) - 1.0)/0.5
                      loc_sedcore_th(m,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_sedcore_th_rst(mm,loc_oo + 1:loc_n_sedcore_tot_rst)        
                      loc_sedcore_poros(m,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_sedcore_poros_rst(mm,loc_oo + 1:loc_n_sedcore_tot_rst) 
                      loc_sedcore_age_cal(m,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_sedcore_age_cal_rst(mm,loc_oo + 1:loc_n_sedcore_tot_rst)   
                      loc_sedcore_age_det(m,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_sedcore_age_det_rst(mm,loc_oo + 1:loc_n_sedcore_tot_rst)   
                      loc_sedcore_age_ash(m,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_sedcore_age_ash_rst(mm,loc_oo + 1:loc_n_sedcore_tot_rst)  
                      loc_sedcore_age_14C(m,loc_o + 1:loc_o + (loc_n_sedcore_tot_rst - loc_oo)) = &
                           & loc_sedcore_age_14C_rst(mm,loc_oo + 1:loc_n_sedcore_tot_rst)
                   end if
                end do
             end if
          end do
       end if
    endif
    ! -------------------------------------------------------- !
    ! WRITE TO FILE
    ! -------------------------------------------------------- !
    IF (ctrl_misc_debug4) print*,'*** WRITE TO FILE ***'
    ! -------------------------------------------------------- ! allocate local grid arrays
    ALLOCATE(loc_depth(1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_ddepth(1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_depth_e(0:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_mk(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_mk_mask(1:nv_sedcore,1:loc_n_sedcore_tot),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_m(1:nv_sedcore),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! -------------------------------------------------------- ! generate grid and other local variables
    loc_c0 = 0.0
    loc_c1 = 1.0
    loc_string = 'sed_dh'
    loc_string_longname = 'Incomplete sediment stack layer thickness'
    do m = 1,nv_sedcore
       loc_lat(m) = 180.0*(real(m) - 0.5)/real(nv_sedcore) - 90.0
       loc_dlat(m) = 180.0/real(nv_sedcore)
    end do
    do o = 1,loc_n_sedcore_tot
       loc_depth(o) = real(o) - 0.5
       loc_ddepth(o) = 1.0
    end do
    ! -------------------------------------------------------- ! open file 
    call sub_opennew(dum_nameout,loc_iou)
    ! -------------------------------------------------------- ! start definitions
    call sub_redef(loc_iou)
    ! -------------------------------------------------------- ! set global attributes
    loc_string_year = fun_conv_num_char_n(8,int(dum_yr))
    loc_title = 'SEDCORES @ year '//loc_string_year
    call sub_putglobal(loc_iou,dum_nameout,loc_title,string_ncrunid,loc_timunit)
    ! -------------------------------------------------------- ! define dimensions
    call sub_defdim('lat',loc_iou,nv_sedcore,loc_id_latm)
    call sub_defdim('lat_edges',loc_iou,nv_sedcore+1,loc_id_lat_e)
    call sub_defdim('zt',loc_iou,loc_n_sedcore_tot,loc_id_zt)
    call sub_defdim('zt_edges',loc_iou,loc_n_sedcore_tot+1,loc_id_zt_e)
    ! -------------------------------------------------------- ! define 1d data
    loc_it_1(1) = loc_id_latm
    call sub_defvar ('lat', loc_iou, 1, loc_it_1, loc_c0, loc_c0, 'X', 'D' , &
         &'latitude of the t grid', 'latitude', 'degrees_north')
    loc_it_1(1) = loc_id_lat_e
    call sub_defvar ('lat_edges', loc_iou, 1, loc_it_1, loc_c0, loc_c0, ' ', 'D' , &
         &'latitude of t grid edges', ' ', 'degrees')
    loc_it_1(1) = loc_id_zt
    call sub_defvar ('zt', loc_iou, 1, loc_it_1, loc_c0, loc_c0, 'Z', 'D' , &
         & 'depth of z grid', ' ', 'cm')
    loc_it_1(1) = loc_id_zt_e
    call sub_defvar ('zt_edges', loc_iou, 1, loc_it_1, loc_c0, loc_c0, ' ', 'D' , &
         & 'depth of z grid edges', ' ', 'cm')
    loc_it_2(1) = loc_id_latm
    loc_it_2(2) = loc_id_zt
    ! -------------------------------------------------------- ! define (1D)  variables
    call sub_defvar('grid_i',loc_iou,1,loc_it_2(1),loc_c0,loc_c0,' ','F', &
         & 'i grid coordinate','i grid coordinate',' ')
    call sub_defvar('grid_j',loc_iou,1,loc_it_2(1),loc_c0,loc_c0,' ','F', &
         & 'j grid coordinate','j grid coordinate',' ')
    call sub_defvar('grid_nrst',loc_iou,1,loc_it_2(1),loc_c0,loc_c0,' ','F', &
         & '# of layers in restart','# of layers in restart',' ')
    ! -------------------------------------------------------- ! define (2D) tracer variables
    call sub_defvar('phys_layer',loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
         & 'Sediment layer number (1 == surface)','Sediment layer number (1 == surface)',' ')
    call sub_defvar('phys_depth',loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
         & 'Sediment layer depth below surface (cm)','Sediment layer depth below surface (cm)',' ')
    call sub_defvar('phys_thickness',loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
         & 'Sediment layer thickness (cm)','Sediment layer thickness (cm)',' ')
    call sub_defvar('phys_porosity',loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
         & 'Sediment layer porosity (cm3 cm-3)','Sediment layer porosity (cm3 cm-3)',' ')
    call sub_defvar('age_CaCO3',loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
         & 'Age from sediment CaCO3 (yr)','Age from sediment CaCO3 (yr)',' ')
    call sub_defvar('age_det',loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
         & 'Age from sediment detrital (yr)','Age from sediment detrital (yr)',' ')
    call sub_defvar('age_ash',loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
         & 'Age by linear sedimentation rate (yr)','Age by linear sedimentation rate (yr)',' ')
    if (sed_select(is_CaCO3_14C)) then
       call sub_defvar('age_14C',loc_iou,2,loc_it_2,loc_c0,loc_c0,' ','F', &
            & 'Radiocarbon age (yr)','Radioncarbon age (yr)',' ')
    end if
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       call sub_defvar('sed_'//trim(string_sed(is)),loc_iou,2, &
            & loc_it_2,loc_c0,loc_c0,' ','F', &
            & string_longname_sed(is),'Sediment tracer - '//trim(string_sed(is)),' ')
    end do
    ! -------------------------------------------------------- ! end definitions
    call sub_enddef (loc_iou)
    call sub_sync(loc_iou)
    ! -------------------------------------------------------- ! 
    loc_ntrec = 1
    ! -------------------------------------------------------- ! write 1D variables: 'latitude'/sed depth
    call sub_putvar1d ('lat',loc_iou,nv_sedcore,loc_ntrec,nv_sedcore, &
         & loc_lat(:),loc_c1,loc_c0)
    call edge_maker (1,loc_lat_e(:),loc_lat(:), &
         & loc_lat(:)+0.5*loc_dlat(:),loc_dlat(:),nv_sedcore)
    call sub_putvar1d ('lat_edges',loc_iou,nv_sedcore+1,loc_ntrec, &
         & nv_sedcore+1,loc_lat_e,loc_c1,loc_c0)
    call sub_putvar1d ('zt',loc_iou,n_j,loc_ntrec,loc_n_sedcore_tot, &
         & loc_depth(:),loc_c1, loc_c0)
    call edge_maker (1,loc_depth_e(:),loc_depth(:), &
         & loc_depth(:)+0.5,loc_ddepth(:),loc_n_sedcore_tot)
    call sub_putvar1d ('zt_edges',loc_iou,loc_n_sedcore_tot+1,loc_ntrec, &
         & loc_n_sedcore_tot+1,loc_depth_e,loc_c1,loc_c0)
    ! -------------------------------------------------------- ! write 1D variables: grid (i,j)
    loc_m(:) = 0
    DO m = 1,nv_sedcore
       loc_m(m) = loc_vsedcore(m)%i
    end DO
    call sub_putvar1d_I('grid_i',loc_iou,nv_sedcore,loc_ntrec,nv_sedcore,loc_m(:))
    loc_m(:) = 0
    DO m = 1,nv_sedcore
       loc_m(m) = loc_vsedcore(m)%j
    end DO
    call sub_putvar1d_I('grid_j',loc_iou,nv_sedcore,loc_ntrec,nv_sedcore,loc_m(:))
    ! NOTE: add 1 because the sedtop layer is not included in the count
    loc_m(:) = 0
    do m=1,nv_sedcore
       loc_i = vsedcore_store(m)%i
       loc_j = vsedcore_store(m)%j
       loc_m(m) = loc_n_sed_stack_top(loc_i,loc_j) + 1
    end DO
    call sub_putvar1d_I('grid_nrst',loc_iou,nv_sedcore,loc_ntrec,nv_sedcore,loc_m(:))
    ! -------------------------------------------------------- ! write (2D) tracer variables: create mask
    loc_mk_mask(:,:) = 0.0
    loc_o = 0
    DO m = 1,nv_sedcore
       loc_i = loc_vsedcore(m)%i
       loc_j = loc_vsedcore(m)%j
       loc_o = loc_n_sedcore_store_top(m) + loc_n_sed_stack_top(loc_i,loc_j) + 1
       IF (ctrl_continuing) then
          DO mm=1,loc_nv_sedcore_rst
             loc_ii = loc_vsedcore_rst(mm)%i
             loc_jj = loc_vsedcore_rst(mm)%j
             if (loc_i == loc_ii .AND. loc_j == loc_jj) then
                loc_o = loc_o + loc_n_sedcore_tot_rst - loc_nv_sed_stack_top_rst(mm)
             end if
          end DO
       end IF
       loc_mk_mask(m,1:loc_o) = 1.0
    end DO
    ! -------------------------------------------------------- ! write (2D) tracer variables: sedcore physics tracers
    loc_mk(:,:) = loc_sedcore_layer(:,:)
    call sub_putvar2d('phys_layer',loc_iou,nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    loc_mk(:,:) = loc_sedcore_depth(:,:)
    call sub_putvar2d('phys_depth',loc_iou,nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    loc_mk(:,:) = loc_sedcore_th(:,:)
    call sub_putvar2d('phys_thickness',loc_iou,nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    loc_mk(:,:) = loc_sedcore_poros(:,:)
    call sub_putvar2d('phys_porosity',loc_iou,nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    loc_mk(:,:) = loc_sedcore_age_cal(:,:)
    call sub_putvar2d('age_CaCO3',loc_iou,nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    loc_mk(:,:) = loc_sedcore_age_det(:,:)
    call sub_putvar2d('age_det',loc_iou,nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    loc_mk(:,:) = loc_sedcore_age_ash(:,:)
    call sub_putvar2d('age_ash',loc_iou,nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    if (sed_select(is_CaCO3_14C)) then
       loc_mk(:,:) = loc_sedcore_age_14C(:,:)
       call sub_putvar2d('age_14C',loc_iou,nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    end if
    ! -------------------------------------------------------- ! write (2D) tracer variables: sediment tracers
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_mk(:,:) = 0.0
       DO m = 1,nv_sedcore
          DO o = 1,loc_n_sedcore_tot
             loc_mk(m,o) = loc_vsedcore(m)%lay(l,o)
          end do
       END DO
       call sub_putvar2d('sed_'//trim(string_sed(is)),loc_iou, &
            & nv_sedcore,loc_n_sedcore_tot,loc_ntrec,loc_mk(:,:),loc_mk_mask(:,:))
    end do
    ! -------------------------------------------------------- ! close file and return IOU
    call sub_closefile(loc_iou)
    dum_iou = loc_iou
    ! -------------------------------------------------------- !
    ! DEALLOCATE REMAINING LOCAL ARRAYS
    ! -------------------------------------------------------- !
    IF (ctrl_misc_debug4) print*,'*** DEALLOCATE REMAINING LOCAL ARRAYS ***'
    ! -------------------------------------------------------- ! deallocate arrays: sedcore restart in/out
    IF (ctrl_continuing .AND. loc_n_sedcore_tot_rst > 0) then
       DEALLOCATE(loc_vsedcore_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_sedcore_layer_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_sedcore_depth_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_sedcore_poros_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_sedcore_th_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_sedcore_age_cal_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_sedcore_age_det_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_sedcore_age_ash_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_sedcore_age_14C_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       DEALLOCATE(loc_nv_sed_stack_top_rst,STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
    end IF
    ! -------------------------------------------------------- ! deallocate arrays: netCDF arrays save
    DEALLOCATE(loc_vsedcore,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_depth,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_ddepth,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_depth_e,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_mk,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_mk_mask,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_m,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! -------------------------------------------------------- ! deallocate arrays: sedcore calculation arrays
    DEALLOCATE(loc_sedcore_layer,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_sedcore_depth,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_sedcore_poros,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_sedcore_th,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_sedcore_age_cal,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_sedcore_age_det,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_sedcore_age_ash,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    DEALLOCATE(loc_sedcore_age_14C,STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_netCDF_sedcoresave
  ! ****************************************************************************************************************************** !


  SUBROUTINE sub_save_netcdf (dum_yr)

    real,         intent(in) :: dum_yr
    character(120) :: loc_title, loc_timunit
    real           :: loc_c0, loc_c1, loc_c10, loc_c100, loc_c500, loc_c1e3, loc_c1e6
    real           :: loc_c6e3, loc_dat
    integer        :: loc_it(6), loc_i, loc_id_time, loc_id_lonm, loc_id_misc
    integer        :: loc_id_latm, loc_id_lon_e
    integer        :: loc_id_lat_e
    real,dimension(0:n_i) :: loc_lon_e
    real,dimension(0:n_j) :: loc_lat_e
    real,dimension(n_i,n_j) :: loc_mask
    logical :: loc_defined

    loc_c0 = 0.
    loc_c1 = 1.
    loc_c10 = 10.
    loc_c100 = 100.
    loc_c500 = 500.
    loc_c1e3 = 1.e3
    loc_c6e3 = 6.e3
    loc_c1e6 = 1.e6

    !-----------------------------------------------------------------------
    !     open file and get latest record number
    !-----------------------------------------------------------------------

    loc_defined = .true.
    loc_i = 0
    if (ntrec_sout .eq. 0) then 
       loc_defined = .false.
       loc_i = 1
    end if
    !print*,'filename = ',string_ncout2d
    call sub_opennext (string_ncout2d, dum_yr, loc_i, ntrec_sout, ntrec_siou)

    if (.not. loc_defined ) then
      
     IF (ctrl_timeseries_output) THEN
       !-----------------------------------------------------------------------       
       !       open file        
       !-----------------------------------------------------------------------       
       call sub_opennew (string_ncout2d, ntrec_siou)
     ENDIF
     
       !-----------------------------------------------------------------------
       !       start definitions
       !-----------------------------------------------------------------------
       call sub_redef (ntrec_siou)

       !-----------------------------------------------------------------------
       !       set global attributes
       !-----------------------------------------------------------------------
       loc_title = '2-D surface sediment and bottom-water properties'
       write (loc_timunit,'(a,F12.2)') 'equal_month_year since 0000-01-01 00:00:00'
       call sub_putglobal (ntrec_siou, string_ncout2d, loc_title, string_ncrunid, loc_timunit)

       !-----------------------------------------------------------------------
       !       define dimensions
       !-----------------------------------------------------------------------
       call sub_defdim('time',ntrec_siou,const_integer_zero,loc_id_time)
       call sub_defdim('para',ntrec_siou,const_integer_one,loc_id_misc)
       call sub_defdim ('lon', ntrec_siou, n_i, loc_id_lonm)
       call sub_defdim ('lat', ntrec_siou, n_j, loc_id_latm)
       call sub_defdim ('lon_edges', ntrec_siou, n_i+1, loc_id_lon_e)
       call sub_defdim ('lat_edges', ntrec_siou, n_j+1, loc_id_lat_e)

       loc_it(1) = loc_id_time
       call sub_defvar ('time', ntrec_siou, 1, loc_it, loc_c0, loc_c0, 'T', 'D' &
            &, 'Year', 'time', trim(loc_timunit))
       call sub_defvar ('year', ntrec_siou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','year', ' ',' ')
       !-----------------------------------------------------------------------
       !       define 1d data (x, y or z)
       !-----------------------------------------------------------------------
       loc_it(1) = loc_id_lonm
       call sub_defvar ('lon', ntrec_siou, 1, loc_it, loc_c0, loc_c0, 'X', 'D' , &
            &'longitude of the t grid', 'longitude', 'degrees_east')
       loc_it(1) = loc_id_latm
       call sub_defvar ('lat', ntrec_siou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
            &'latitude of the t grid', 'latitude', 'degrees_north')
       loc_it(1) = loc_id_lon_e
       call sub_defvar ('lon_edges', ntrec_siou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
            &'longitude of t grid edges', ' ', 'degrees')
       loc_it(1) = loc_id_lat_e
       call sub_defvar ('lat_edges', ntrec_siou, 1, loc_it, loc_c0, loc_c0, ' ', 'D' , &
            &'latitude of t grid edges', ' ', 'degrees')

       loc_it(1) = loc_id_time
       call sub_defvar ('month', ntrec_siou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','month', ' ',' ')
       call sub_defvar ('day', ntrec_siou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','day', ' ',' ')
       call sub_defvar ('hour', ntrec_siou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','hour', ' ',' ')
       call sub_defvar ('minute', ntrec_siou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','minute', ' ',' ')
       call sub_defvar ('second', ntrec_siou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','second', ' ',' ')


       !-----------------------------------------------------------------------
       !       define 2d data (x,y)
       !-----------------------------------------------------------------------
       loc_it(1) = loc_id_lonm
       loc_it(2) = loc_id_latm
       call sub_defvar ('grid_mask', ntrec_siou, 2, loc_it, loc_c0, loc_c100, ' ', 'D', &
            &'sediment mask', ' ' ,'n/a')
       call sub_defvar ('grid_topo', ntrec_siou, 2, loc_it, -loc_c6e3, loc_c0, ' ', 'D', &
            &'sediment topography', ' ' ,'m')
       !-----------------------------------------------------------------------
       !       end definitions
       !-----------------------------------------------------------------------
       call sub_enddef (ntrec_siou)

       !-----------------------------------------------------------------------
       !       write 1d data (x, y or z)
       !-----------------------------------------------------------------------

       call sub_putvars  ('time', ntrec_siou, ntrec_sout, dum_yr, loc_c1, loc_c0)
       call sub_putvarIs ('year', ntrec_siou, ntrec_sout, floor(dum_yr), loc_c1, loc_c0)
       loc_dat = 1.
       call sub_putvars  ('month', ntrec_siou, ntrec_sout, loc_dat, loc_c1, loc_c0)
       call sub_putvars  ('day', ntrec_siou, ntrec_sout, loc_dat, loc_c1, loc_c0)
       loc_dat = 0.
       call sub_putvars  ('hour', ntrec_siou, ntrec_sout, loc_dat, loc_c1, loc_c0)
       call sub_putvars  ('minute', ntrec_siou, ntrec_sout, loc_dat, loc_c1, loc_c0)
       call sub_putvars  ('second', ntrec_siou, ntrec_sout, loc_dat, loc_c1, loc_c0)
       call sub_putvar1d ('lon', ntrec_siou, n_i, ntrec_sout, n_i, phys_sed(ips_lon,:,1), loc_c1, loc_c0)
       call edge_maker (1, loc_lon_e, phys_sed(ips_lon,:,1), &
            & phys_sed(ips_lone,:,1), phys_sed(ips_dlon,:,1), n_i)
       call sub_putvar1d ('lon_edges', ntrec_siou, n_i+1, ntrec_sout, n_i+1, loc_lon_e, loc_c1, loc_c0)
       call sub_putvar1d ('lat', ntrec_siou, n_j, ntrec_sout, n_j, phys_sed(ips_lat,1,:), loc_c1, loc_c0)
       call edge_maker (1, loc_lat_e, phys_sed(ips_lat,1,:), &
            & phys_sed(ips_latn,1,:), phys_sed(ips_dlat,1,:), n_j)
       call sub_putvar1d ('lat_edges', ntrec_siou, n_j+1, ntrec_sout, n_j+1, loc_lat_e, loc_c1, loc_c0)
       !-----------------------------------------------------------------------
       !       write 2d data (x,y)
       !-----------------------------------------------------------------------
       loc_mask = phys_sed(ips_mask_sed,:,:)
       call sub_putvar2d ('grid_mask', ntrec_siou, n_i, n_j, ntrec_sout, &
            & phys_sed(ips_mask_sed,:,:), loc_mask)
       call sub_putvar2d ('grid_topo', ntrec_siou, n_i, n_j, ntrec_sout, &
            & -phys_sed(ips_mask_sed,:,:)*phys_sed(ips_D,:,:), loc_mask)
    else
      call sub_putvars  ('time', ntrec_siou, ntrec_sout, dum_yr, loc_c1, loc_c0)
      call sub_putvarIs ('year', ntrec_siou, ntrec_sout, floor(dum_yr), loc_c1, loc_c0)
    end if

    call sub_sync(ntrec_siou)

  END SUBROUTINE sub_save_netcdf


  SUBROUTINE sub_save_netcdf_sed2d(dum_dtyr,dum_sfcsumocn)
    ! dummy valiables
    real,INTENT(in)::dum_dtyr
    real,DIMENSION(n_ocn,n_i,n_j),intent(in)::dum_sfcsumocn
    ! local variables
    INTEGER::i,j,l,io,is,ic,idiag
    CHARACTER(len=255)::loc_unitsname
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_sed_coretop
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_sed_burial
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_sed_preservation
    REAL,DIMENSION(n_i,n_j)::loc_ij,loc_mask
    REAL,DIMENSION(n_i,n_j)::loc_mask_reef,loc_mask_muds       ! 
    REAL,DIMENSION(n_i,n_j)::loc_mask_dsea                     ! 
    real::loc_tot,loc_frac,loc_standard                        ! 
    real::loc_c0,loc_c1                                        ! 

    ! *** INITIALIZE ***
    ! initialize local variables
    loc_c0 = 0.
    loc_c1 = 1.
    io = 0
    ! initialize local masks
    loc_mask(:,:)      = phys_sed(ips_mask_sed,:,:)
    loc_mask_reef(:,:) = phys_sed(ips_mask_sed_reef,:,:)
    loc_mask_muds(:,:) = phys_sed(ips_mask_sed_muds,:,:)
    loc_mask_dsea(:,:) = phys_sed(ips_mask_sed,:,:)*(1.0 - loc_mask_reef(:,:))*(1.0 - loc_mask_muds(:,:))
    ! calculate core-top sediment composition data
    loc_sed_coretop(:,:,:) = fun_sed_coretop()
    ! calculate local sediment preservation
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       DO i=1,n_i
          DO j=1,n_j
             loc_sed_burial(is,i,j) = sed_fsed(is,i,j) - sed_fdis(is,i,j)
             IF (sed_fsed(is,i,j) > 0.0) THEN
                loc_sed_preservation(is,i,j) = 100.0*(sed_fsed(is,i,j) - sed_fdis(is,i,j))/sed_fsed(is,i,j)
             else
                loc_sed_preservation(is,i,j) = 0.0
             end if
          end do
       end do
    end do

    ! SAVE *ALL* DISSOLVED DATA
    ! ocean interface tracer data field
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (ocn_type(io))
             CASE (0)
                if (io == io_T) then 
                   loc_ij(i,j) = dum_sfcsumocn(io,i,j) - const_zeroC
                else
                   loc_ij(i,j) = dum_sfcsumocn(io,i,j)                   
                end if
                If (io == io_T) loc_unitsname = 'C'
                If (io == io_S) loc_unitsname = 'o/oo'
             CASE (1)
                loc_ij(i,j) = dum_sfcsumocn(io,i,j)
                loc_unitsname = 'mol kg-1'
             case (n_itype_min:n_itype_max)
                loc_tot  = dum_sfcsumocn(ocn_dep(io),i,j)
                loc_frac = dum_sfcsumocn(io,i,j)
                loc_standard = const_standards(ocn_type(io))
                loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                loc_unitsname = 'o/oo'
             end SELECT
          end do
       end do
       call sub_adddef_netcdf(ntrec_siou,3,'ocn_'//trim(string_ocn(io)), &
            & 'overlying ocean tracer properties - '//trim(string_ocn(io)), &
            & trim(loc_unitsname),ocn_mima(io2l(io),1),ocn_mima(io2l(io),2))
       call sub_putvar2d('ocn_'//trim(string_ocn(io)),ntrec_siou,n_i, n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    END DO
    ! carbonate chemistry data field
    DO ic=1,n_carb
       SELECT CASE (ocn_type(io))
       CASE (ic_H,ic_ohm_cal,ic_ohm_arg)
          loc_unitsname = 'n/a'
       case default
          loc_unitsname = 'mol kg-1'
       end SELECT
       call sub_adddef_netcdf(ntrec_siou,3,'carb_'//trim(string_carb(ic)), &
            & 'overlying ocean carbonate chemistry - '//trim(string_carb(ic)),trim(loc_unitsname),loc_c0,loc_c0)
       call sub_putvar2d('carb_'//trim(string_carb(ic)),ntrec_siou,n_i,n_j,ntrec_sout,sed_carb(ic,:,:),loc_mask)
    END DO
    ! benthic/pelagic dissolved tracer exchange fluxes
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (ocn_type(io))
             CASE (1)
                loc_ij(i,j) = sedocn_fnet(io,i,j)/dum_dtyr
                loc_unitsname = 'mol cm-2 yr-1'
             case (n_itype_min:n_itype_max)
                loc_tot  = sedocn_fnet(ocn_dep(io),i,j)
                loc_frac = sedocn_fnet(io,i,j)
                loc_standard = const_standards(ocn_type(io))
                loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                loc_unitsname = 'o/oo'
             end SELECT
          end do
       end do
       call sub_adddef_netcdf(ntrec_siou,3,'sedocn_fnet_'//trim(string_ocn(io)), &
            & 'benthic interface exchange flux - '//trim(string_ocn(io)), &
            & trim(loc_unitsname),loc_c0,loc_c0)
       call sub_putvar2d('sedocn_fnet_'//trim(string_ocn(io)),ntrec_siou,n_i, n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    END DO

    ! SAVE *ALL* SOLID SEDIMENT FLUXES
    ! interface flux data -- rain flux
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(is))
             CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
                loc_ij(i,j) = sed_fsed(is,i,j)/dum_dtyr
                loc_unitsname = 'mol cm-2 yr-1'
             case (n_itype_min:n_itype_max)
                loc_tot  = sed_fsed(sed_dep(is),i,j)
                loc_frac = sed_fsed(is,i,j)
                loc_standard = const_standards(sed_type(is))
                loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
                loc_unitsname = 'o/oo'
             CASE (par_sed_type_frac)
                loc_ij(i,j) = sed_fsed(is,i,j)
                loc_unitsname = 'n/a'
             END SELECT
          end do
       end do
       SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & n_itype_min:n_itype_max, &
               & par_sed_type_frac)
            call sub_adddef_netcdf(ntrec_siou,3,'fsed_'//trim(string_sed(is)), &
               & 'sediment rain flux - '//trim(string_sed(is)), &
               & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
          call sub_putvar2d('fsed_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
       END SELECT
    END DO
    ! interface flux data -- dissolution flux
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(is))
             CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
                loc_ij(i,j) = sed_fdis(is,i,j)/dum_dtyr
                loc_unitsname = 'mol cm-2 yr-1'
             case (n_itype_min:n_itype_max)
                loc_tot  = sed_fsed(sed_dep(is),i,j)
                loc_frac = sed_fsed(is,i,j)
                loc_standard = const_standards(sed_type(is))
                loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_real_null)
                loc_unitsname = 'o/oo'
             END SELECT
          end do
       end do
       SELECT CASE (sed_type(is))
       CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & n_itype_min:n_itype_max)
          call sub_adddef_netcdf(ntrec_siou,3,'fdis_'//trim(string_sed(is)), &
               & 'sediment dissolution flux - '//trim(string_sed(is)), &
               & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
          call sub_putvar2d('fdis_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
       END SELECT
    END DO
    ! interface flux data -- burial flux
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(is))
             CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
                loc_ij(i,j) = loc_sed_burial(is,i,j)/dum_dtyr
                loc_unitsname = 'mol cm-2 yr-1'
             case (n_itype_min:n_itype_max)
                loc_tot  = loc_sed_burial(sed_dep(is),i,j)
                loc_frac = loc_sed_burial(is,i,j)
                loc_standard = const_standards(sed_type(is))
                loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_real_null)
                loc_unitsname = 'o/oo'
             END SELECT
          end do
       end do
       SELECT CASE (sed_type(is))
       CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & n_itype_min:n_itype_max)
          call sub_adddef_netcdf(ntrec_siou,3,'fburial_'//trim(string_sed(is)), &
               & 'sediment burial flux - '//trim(string_sed(is)), &
               & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
          call sub_putvar2d('fburial_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
       END SELECT
    END DO
    ! interface flux data -- % preservation
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(is))
             CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
                loc_ij(i,j) = loc_sed_preservation(is,i,j)
                loc_unitsname = '%'
             END SELECT
          end do
       end do
       SELECT CASE (sed_type(is))
       CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
          call sub_adddef_netcdf(ntrec_siou,3,'fpres_'//trim(string_sed(is)), &
               & 'sediment burial (preservation) flux - '//trim(string_sed(is)), &
               & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
          call sub_putvar2d('fpres_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
       end SELECT
    END DO
    ! save interface flux data - dust (log10)
    IF (sed_select(is_det)) THEN
       loc_ij(:,:) = const_real_zero
       loc_unitsname = 'mol cm-2 yr-1'
       ! log10 data
       DO i=1,n_i
          DO j=1,n_j
             IF (sed_fsed(is_det,i,j) > 0.0) THEN
                loc_ij(i,j) = log10(sed_fsed(is_det,i,j)/dum_dtyr)
             else
                loc_ij(i,j) = const_real_null
             end if
          end do
       end do
       call sub_adddef_netcdf(ntrec_siou,3,'fsed_'//trim(string_sed(is_det))//'_log10', &
            & 'sediment rain flux - detrital material (log10)',trim(loc_unitsname),loc_c0,loc_c0)
       call sub_putvar2d('fsed_'//trim(string_sed(is_det))//'_log10',ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    END IF
    ! CaCO3:POC 'rain ratio'
    IF (sed_select(is_CaCO3) .AND. sed_select(is_POC)) THEN
       loc_unitsname = 'n/a'
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             if (sed_fsed(is_POC,i,j) > const_real_nullsmall) then
                loc_ij(i,j) = sed_fsed(is_CaCO3,i,j)/sed_fsed(is_POC,i,j)
             end if
          END DO
       END DO
       call sub_adddef_netcdf(ntrec_siou,3,'fsed_CaCO3toPOC', &
            & 'sediment rain flux - CaCO3 to POC rain ratio',trim(loc_unitsname),loc_c0,loc_c0)
       call sub_putvar2d('fsed_CaCO3toPOC',ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    end if
    ! diagnostics
    IF (sed_select(is_POC) .AND. par_sed_diagen_Corgopt == 'huelse2016') THEN
       DO idiag=1,n_diag_sed
          loc_unitsname = 'n/a'
          loc_ij(:,:) = sed_diag(idiag,:,:)
          call sub_adddef_netcdf(ntrec_siou,3,trim(string_diag_sed(idiag)), &
               & trim(string_diag_sed(idiag)),trim(loc_unitsname),loc_c0,loc_c0)
          call sub_putvar2d(trim(string_diag_sed(idiag)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
       end do
    end if

    ! SAVE DEEP-SEA SEDIMENT DATA
    ! core-top data
    DO is=1,n_sed
       IF (sed_select(is)) THEN
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
             loc_unitsname = 'wt%'
          CASE (n_itype_min:n_itype_max)
             loc_unitsname = 'o/oo'
          CASE (par_sed_type_age)
             loc_unitsname = 'years'
          END SELECT
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio,par_sed_type_age, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
                & n_itype_min:n_itype_max)
             call sub_adddef_netcdf(ntrec_siou,3,'sed_'//trim(string_sed(is)), &
                  & 'surface sediment composition - '//trim(string_sed(is)), &
                  & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
             call sub_putvar2d('sed_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_sed_coretop(is,:,:),loc_mask_dsea)
          end SELECT
       END IF
    END DO

    ! SAVE REEF SEDIMENTS
    ! bulk composition
    is = is_CaCO3
    loc_unitsname = 'wt%'
    loc_ij(:,:) = loc_sed_coretop(is,:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'reef_sed_'//trim(string_sed(is)), &
         & 'reef composition - '//trim(string_sed(is)), &
         & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
    call sub_putvar2d('reef_sed_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask_reef)
    is = is_CaCO3_13C
    loc_unitsname = 'o/oo'
    loc_ij(:,:) = loc_sed_coretop(is,:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'reef_sed_'//trim(string_sed(is)), &
         & 'reef composition - '//trim(string_sed(is)), &
         & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
    call sub_putvar2d('reef_sed_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask_reef)
    ! precipitation
    is = is_CaCO3
    loc_unitsname = 'mol cm-2 yr-1'
    loc_ij(:,:) = sed_fsed(is,:,:)/dum_dtyr
    call sub_adddef_netcdf(ntrec_siou,3,'reef_fsed_'//trim(string_sed(is)), &
         & 'reef precipitation rate - '//trim(string_sed(is)), &
         & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
    call sub_putvar2d('reef_fsed_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask_reef)
    is = is_CaCO3_13C
    loc_unitsname = 'o/oo'
    DO i=1,n_i
       DO j=1,n_j
          loc_tot  = sed_fsed(sed_dep(is),i,j)/dum_dtyr
          loc_frac = sed_fsed(is,i,j)/dum_dtyr
          loc_standard = const_standards(sed_type(is))
          loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_real_null)
       end do
    end do
    call sub_adddef_netcdf(ntrec_siou,3,'reef_fsed_'//trim(string_sed(is)), &
         & 'reef precipitation rate - '//trim(string_sed(is)), &
         & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
    call sub_putvar2d('reef_fsed_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask_reef)

    ! SAVE SHALLOW WATER SEDIMENTS
    ! bulk composition
    is = is_POC
    loc_unitsname = 'wt%'
    loc_ij(:,:) = loc_sed_coretop(is,:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'muds_sed_'//trim(string_sed(is)), &
         & 'shallow water sediment composition - '//trim(string_sed(is)), &
         & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
    call sub_putvar2d('muds_sed_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask_muds)
    is = is_POC_13C
    loc_unitsname = 'o/oo'
    loc_ij(:,:) = loc_sed_coretop(is,:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'muds_sed_'//trim(string_sed(is)), &
         & 'shallow water sediment composition - '//trim(string_sed(is)), &
         & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
    call sub_putvar2d('muds_sed_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask_muds)

    ! MISCELLANEOUS FIELDS
    ! 
    loc_unitsname = 'cm3 cm-3'
    loc_ij(:,:) = phys_sed(ips_poros,:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'misc_sed_porosity', &
         & 'sediment surface porosity',trim(loc_unitsname),loc_c0,loc_c0)
    call sub_putvar2d('misc_sed_porosity',ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    ! 
    loc_unitsname = 'cm2 yr-1'
    loc_ij(:,:) = phys_sed(ips_mix_k0,:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'misc_sed_max_k', &
         & 'maximum (surface) sediment bioturbation mixing rate',trim(loc_unitsname),loc_c0,loc_c0)
    call sub_putvar2d('misc_sed_max_k',ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    ! 
    loc_unitsname = 'n/a'
    loc_ij(:,:) = loc_mask_dsea(:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'grid_mask_dsea','deep sea sediments mask',trim(loc_unitsname),loc_c0,loc_c0)
    call sub_putvar2d('grid_mask_dsea',ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    ! 
    loc_unitsname = 'n/a'
    loc_ij(:,:) = loc_mask_reef(:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'grid_mask_reef','reef area mask',trim(loc_unitsname),loc_c0,loc_c0)
    call sub_putvar2d('grid_mask_reef',ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    ! 
    loc_unitsname = 'n/a'
    loc_ij(:,:) = loc_mask_muds(:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'grid_mask_muds','shallow sediments mask',trim(loc_unitsname),loc_c0,loc_c0)
    call sub_putvar2d('grid_mask_muds',ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask)
    ! 
    loc_unitsname = 'm'
    loc_ij(:,:) = loc_mask_reef(:,:)*phys_sed(ips_D,:,:)
    call sub_adddef_netcdf(ntrec_siou,3,'grid_topo_reef','reef topography',trim(loc_unitsname),loc_c0,loc_c0)
    call sub_putvar2d('grid_topo_reef',ntrec_siou,n_i,n_j,ntrec_sout,loc_ij(:,:),loc_mask_reef)
    ! 
    DO i=1,n_i
       DO j=1,n_j
          IF (sed_save_mask(i,j)) THEN
             loc_mask(i,j) = 1.0
          else
             loc_mask(i,j) = 0.0
          end if
       end do
    end do
    loc_unitsname = 'n/a'
    call sub_adddef_netcdf(ntrec_siou,3,'grid_mask_sedcore','sediment core locations',trim(loc_unitsname),loc_c0,loc_c0)
    call sub_putvar2d('grid_mask_sedcore',ntrec_siou,n_i,n_j,ntrec_sout,loc_mask(:,:),loc_mask)
    loc_unitsname = 'm'
    call sub_adddef_netcdf(ntrec_siou,3,'grid_topo_sedcore','sediment core topo',trim(loc_unitsname),loc_c0,loc_c0)
    call sub_putvar2d('grid_topo_sedcore',ntrec_siou,n_i,n_j,ntrec_sout,loc_mask(:,:)*phys_sed(ips_D,:,:),loc_mask)
    ! core-top data @ sedcore locations
    DO is=1,n_sed
       IF (sed_select(is)) THEN
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
             loc_unitsname = 'wt%'
          CASE (n_itype_min:n_itype_max)
             loc_unitsname = 'o/oo'
          CASE (par_sed_type_age)
             loc_unitsname = 'years'
          END SELECT
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio,par_sed_type_age, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & n_itype_min:n_itype_max)
             loc_ij(:,:) = loc_sed_coretop(is,:,:)
             call sub_adddef_netcdf(ntrec_siou,3,'sedcore_'//trim(string_sed(is)), &
                  & 'surface sediment composition - '//trim(string_sed(is)), &
                  & trim(loc_unitsname),sed_mima(is2l(is),1),sed_mima(is2l(is),2))
             call sub_putvar2d('sedcore_'//trim(string_sed(is)),ntrec_siou,n_i,n_j,ntrec_sout,loc_mask(:,:)*loc_ij(:,:),loc_mask)
          end SELECT
       END IF
    END DO

  end SUBROUTINE sub_save_netcdf_sed2d


END MODULE sedgem_data_netCDF
