!
! File: rokgem_data_netCDF.f90
! 
! Description: this module contains the subroutines for generating netCDF output from rokgem.
!
! Uses:
!
! - <gem_netcdf.f90>
! - <rokgem_lib.f90>
! - <rokgem_box.f90>


MODULE rokgem_data_netCDF

  USE gem_netcdf
  USE rokgem_lib
  USE rokgem_data
  IMPLICIT NONE
  SAVE


CONTAINS


  SUBROUTINE rokgem_netcdf(dum_temp,dum_CO2,dum_runoff,dum_photo,dum_respveg,dum_loc_P,&
                          & dum_force_flux_weather_a_land,dum_force_flux_weather_o_land,&
                          & dum_force_flux_weather_o_ocean)

       ! dummy variables
       !(NOTE: output for temp, runoff and productivity is that used in rokgem calculations, so may have been calibrated if option set)
       REAL,INTENT(in)                 :: dum_temp(n_i,n_j)                                        ! temperature (originally from atmosphere composition interface array - subject to calibration)
       REAL,INTENT(in)                 :: dum_CO2(n_i,n_j)                                         ! CO2 (from atmosphere composition interface array)
       REAL,INTENT(in)                 :: dum_runoff(n_i,n_j)                                      ! runoff (originally from EMBM - subject to calibration)
       REAL,INTENT(in)                 :: dum_photo(n_i,n_j)                                       ! photosythesis from land veg module (ENTS)
       REAL,INTENT(in)                 :: dum_respveg(n_i,n_j)                                     ! vegetation respiration from land veg module (ENTS) (subject to calibration)
       REAL,INTENT(in)                 :: dum_loc_P(n_i,n_j)                                       ! Productivity as calculated for rokgem from ENTS inputs
       REAL,INTENT(in)                 :: dum_force_flux_weather_a_land(n_atm,n_i,n_j)             ! fluxes shared over land (atmosphere variables)
       REAL,INTENT(in)                 :: dum_force_flux_weather_o_land(n_ocn,n_i,n_j)             ! fluxes shared over land (ocean variables)
       REAL,INTENT(in)                 :: dum_force_flux_weather_o_ocean(n_ocn,n_i,n_j)            ! fluxes into coastal positions in ocean (ocean variables)     

           ! re-open netcdf file
           call sub_save_netcdf(output_years_2d(output_counter_2d))
           CALL sub_save_netcdf_2d_rg(dum_temp,dum_CO2,dum_runoff,dum_photo,dum_respveg,dum_loc_P, &                      
                    & dum_force_flux_weather_a_land,dum_force_flux_weather_o_land,dum_force_flux_weather_o_ocean)

           ! close netcdf file and update record number
           call sub_closefile(ncout2d_iou_rg)
           ncout2d_ntrec_rg = ncout2d_ntrec_rg + 1

   END SUBROUTINE rokgem_netcdf


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
  !
  ! NOTE: calls tosub_putvar2d (dum_name, dum_ncid, dum_la, dum_lb, dum_is, dum_din, dum_mask)
  !       dum_name = name of variable to be written
  !       dum_ncid = iou unit
  !       dum_la   = length of data
  !       dum_lb   = length of data
  !       dum_is   = starting point for write
  !       dum_din  = data to be written (default real)
  !       dum_mask = topography mask for printing 

  SUBROUTINE sub_save_netcdf_2d_rg(dum_temp,dum_CO2,dum_runoff,dum_photo,dum_respveg,dum_loc_P,&
                                  & dum_force_flux_weather_a_land,dum_force_flux_weather_o_land,&
                                  & dum_force_flux_weather_o_ocean)

       ! dummy variables
       !(NOTE: output for temp, runoff and productivity is that used in rokgem calculations, so may have been calibrated if option set)
       REAL,INTENT(in)                 :: dum_temp(n_i,n_j)                                        ! temperature (originally from atmosphere composition interface array - subject to calibration)
       REAL,INTENT(in)                 :: dum_CO2(n_i,n_j)                                         ! CO2 (from atmosphere composition interface array)
       REAL,INTENT(in)                 :: dum_runoff(n_i,n_j)                                      ! runoff (originally from EMBM - subject to calibration)
       REAL,INTENT(in)                 :: dum_photo(n_i,n_j)                                       ! photosythesis from land veg module (ENTS)
       REAL,INTENT(in)                 :: dum_respveg(n_i,n_j)                                     ! vegetation respiration from land veg module (ENTS) (subject to calibration)
       REAL,INTENT(in)                 :: dum_loc_P(n_i,n_j)                                       ! Productivity as calculated for rokgem from ENTS inputs
       REAL,INTENT(in)                 :: dum_force_flux_weather_a_land(n_atm,n_i,n_j)             ! fluxes shared over land (atmosphere variables)
       REAL,INTENT(in)                 :: dum_force_flux_weather_o_land(n_ocn,n_i,n_j)             ! fluxes shared over land (ocean variables)
       REAL,INTENT(in)                 :: dum_force_flux_weather_o_ocean(n_ocn,n_i,n_j)            ! fluxes into coastal positions in ocean (ocean variables)     

    !-----------------------------------------------------------------------
    !       DEFINE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    integer::i,j
    INTEGER::loc_iou,loc_ntrec
    real,DIMENSION(n_i,n_j)::loc_ij,loc_mask_surf,loc_mask_ocean
    CHARACTER(len=255)::loc_unitsname
    !-----------------------------------------------------------------------
    !       INITIALIZE LOCAL VARIABLES
    !-----------------------------------------------------------------------
    loc_iou   = ncout2d_iou_rg
    loc_ntrec = ncout2d_ntrec_rg
    loc_mask_surf(:,:) = 1.0
    loc_mask_ocean(:,:) = 0.0
    where ( landmask(:,:) .eq. 0 )
          loc_mask_surf = 0.0
          loc_mask_ocean = 1.0
    endwhere
    !-----------------------------------------------------------------------

    ! note - in biogem an average over a specified timeslice is taken, here the array is just taken at one fixed point in time

    ! surface temperature
    loc_unitsname = 'deg C'
    DO i=1,n_i
       DO j=1,n_j
          loc_ij(i,j) = dum_temp(i,j)
       END DO
    END DO
    call sub_adddef_netcdf(loc_iou,3,'sur_temp','Surface Temperature',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('sur_temp',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)

    ! atmospheric pCO2
    loc_unitsname = 'atm'
    DO i=1,n_i
       DO j=1,n_j
          loc_ij(i,j) = dum_CO2(i,j)
       END DO
    END DO
    call sub_adddef_netcdf(loc_iou,3,'pCO2','atmospheric pCO2',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('pCO2',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)

    ! runoff
    loc_unitsname = 'mm/yr'
    loc_ij(:,:) = dum_runoff(:,:)*conv_yr_s
    call sub_adddef_netcdf(loc_iou,3,'runoff','Run-off',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('runoff',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)

    ! photosynthesis
    loc_unitsname = 'GtC/yr'
    loc_ij(:,:) = dum_photo(:,:)
    call sub_adddef_netcdf(loc_iou,3,'photo','Photosynthesis',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('photo',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)

    ! resp veg
    loc_unitsname = 'GtC/yr'
    loc_ij(:,:) = dum_respveg(:,:)
    call sub_adddef_netcdf(loc_iou,3,'resp_veg','Vegetation Respiration',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('resp_veg',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)

    ! productivity
    loc_unitsname = 'GtC/yr'
    loc_ij(:,:) = dum_loc_P(:,:)
    call sub_adddef_netcdf(loc_iou,3,'productivity','land surface productivity',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('productivity',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)

    ! weathering flux from atmosphere - land
    ! CO2
    loc_unitsname = 'mol CO2 / yr'
    loc_ij(:,:) = dum_force_flux_weather_a_land(ia_PCO2,:,:)
    call sub_adddef_netcdf(loc_iou,3,'CO2_land','CO2 weathering flux - land',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('CO2_land',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)

    ! weathering flux to ocean - land
    ! Alkalinity
    loc_unitsname = 'mol ALK / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_land(io_ALK,:,:)
    call sub_adddef_netcdf(loc_iou,3,'ALK_land','Alkalinity weathering flux - land',trim(loc_unitsname),const_real_zero, &
         & const_real_zero)
    call sub_putvar2d('ALK_land',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
    ! DIC
    loc_unitsname = 'mol DIC / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_land(io_DIC,:,:)
    call sub_adddef_netcdf(loc_iou,3,'DIC_land','DIC weathering flux - land',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('DIC_land',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
    ! Ca
    loc_unitsname = 'mol Ca / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_land(io_Ca,:,:)
    call sub_adddef_netcdf(loc_iou,3,'Ca_land','Ca weathering flux - land',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('Ca_land',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
    ! Alkalinity
    loc_unitsname = 'mol DIC_13C / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_land(io_DIC_13C,:,:)
    call sub_adddef_netcdf(loc_iou,3,'DIC_13C_land','DIC_13C weathering flux - land',trim(loc_unitsname),const_real_zero, &
         & const_real_zero)
    call sub_putvar2d('DIC_13C_land',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)
    ! Alkalinity
    loc_unitsname = 'mol DIC_14C / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_land(io_DIC_14C,:,:)
    call sub_adddef_netcdf(loc_iou,3,'DIC_14C_land','DIC_14C weathering flux - land',trim(loc_unitsname),const_real_zero, &
         & const_real_zero)
    call sub_putvar2d('DIC_14C_land',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_surf)

    ! weathering flux to ocean - ocean
    ! Alkalinity
    loc_unitsname = 'mol ALK / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_ocean(io_ALK,:,:)
    call sub_adddef_netcdf(loc_iou,3,'ALK_ocean','Alkalinity weathering flux - ocean',trim(loc_unitsname),const_real_zero, &
         const_real_zero)
    call sub_putvar2d('ALK_ocean',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_ocean)
    ! DIC
    loc_unitsname = 'mol DIC / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_ocean(io_DIC,:,:)
    call sub_adddef_netcdf(loc_iou,3,'DIC_ocean','DIC weathering flux - ocean',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('DIC_ocean',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_ocean)
    ! Ca
    loc_unitsname = 'mol Ca / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_ocean(io_Ca,:,:)
    call sub_adddef_netcdf(loc_iou,3,'Ca_ocean','Ca weathering flux -ocean',trim(loc_unitsname),const_real_zero,const_real_zero)
    call sub_putvar2d('Ca_ocean',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_ocean)
    ! Alkalinity
    loc_unitsname = 'mol DIC_13C / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_ocean(io_DIC_13C,:,:)
    call sub_adddef_netcdf(loc_iou,3,'DIC_13C_ocean','DIC_13C weathering flux - ocean',trim(loc_unitsname),const_real_zero, &
         & const_real_zero)
    call sub_putvar2d('DIC_13C_ocean',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_ocean)
    ! Alkalinity
    loc_unitsname = 'mol DIC_14C / yr'
    loc_ij(:,:) = dum_force_flux_weather_o_ocean(io_DIC_14C,:,:)
    call sub_adddef_netcdf(loc_iou,3,'DIC_14C_ocean','DIC_14C weathering flux - ocean',trim(loc_unitsname),const_real_zero, &
         & const_real_zero)
    call sub_putvar2d('DIC_14C_ocean',loc_iou,n_i,n_j,loc_ntrec,loc_ij(:,:),loc_mask_ocean)

    ! ### INSERT CODE TO SAVE ADDITIONAL 2-D DATA FIELDS ######################################################################### !
    !
    ! ############################################################################################################################ !
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf_2d_rg
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE netCDF
  SUBROUTINE sub_init_netcdf_rg(dum_name, dum_iou)
    !-----------------------------------------------------------------------
    !       dummy arguments
    !-----------------------------------------------------------------------
    character(LEN=*),INTENT(IN) :: dum_name
    INTEGER,INTENT(OUT):: dum_iou
    !-----------------------------------------------------------------------
    !       define local variables
    !-----------------------------------------------------------------------
    character(255) :: loc_title, loc_timunit
    real           :: loc_c0, loc_c1, loc_c10, loc_c100, loc_c500, loc_c1e3, loc_c1e6
    integer        :: loc_it(6), loc_id_time, loc_id_lonm, loc_id_latp
    integer        :: loc_id_latm, loc_id_lon_e, loc_id_xu, loc_id_yu
    integer        :: loc_id_lat_e, loc_id_xu_e, loc_id_yu_e
    integer        :: loc_id_misc
    !-----------------------------------------------------------------------
    !       initialize local variables 
    !-----------------------------------------------------------------------
    loc_c0 = 0.
    loc_c1 = 1.
    loc_c10 = 10.
    loc_c100 = 100.
    loc_c500 = 500.
    loc_c1e3 = 1.e3
    loc_c1e6 = 1.e6
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
    loc_title = 'Time Averaged Integrals'
    write (loc_timunit,'(a)') 'equal month years'
    call sub_putglobal (dum_iou, dum_name, loc_title, string_ncrunid_rg, loc_timunit)
    !-----------------------------------------------------------------------
    !       define dimensions
    !-----------------------------------------------------------------------
    call sub_defdim ('time', dum_iou, const_integer_zero, loc_id_time)
    call sub_defdim ('xu', dum_iou, n_i, loc_id_xu)
    call sub_defdim ('lon', dum_iou, n_i, loc_id_lonm)
    call sub_defdim ('lat', dum_iou, n_j, loc_id_latm)
    call sub_defdim ('yu', dum_iou, n_j, loc_id_yu)
    call sub_defdim ('lon_edges', dum_iou, n_i+1, loc_id_lon_e)
    call sub_defdim ('lat_edges', dum_iou, n_j+1, loc_id_lat_e)
    call sub_defdim ('xu_edges', dum_iou, n_i+1, loc_id_xu_e)
    call sub_defdim ('yu_edges', dum_iou, n_j+1, loc_id_yu_e)
    call sub_defdim ('lat_moc', dum_iou, n_j+1, loc_id_latp)
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
         &'longitude of the t grid', 'grid_longitude', 'degrees_east')
    loc_it(1) = loc_id_latm
    call sub_defvar ('lat', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
         &'latitude of the t grid', 'grid_latitude', 'degrees_north')
    loc_it(1) = loc_id_xu
    call sub_defvar ('xu', dum_iou, 1, loc_it, loc_c0, loc_c0, 'X', 'D' , &
         &'longitude of the u grid', 'grid_longitude', 'degrees_east')
    loc_it(1) = loc_id_yu
    call sub_defvar ('yu', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
         &'latitude of the u grid', 'grid_latitude', 'degrees_north')
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
    loc_it(1) = loc_id_latp
    call sub_defvar ('lat_moc', dum_iou, 1, loc_it, loc_c0, loc_c0, 'Y', 'D' , &
         &'latitude of moc grid', 'grid_latitude', 'degrees_north')
    loc_it(1) = loc_id_time
    call sub_defvar ('month', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','month', ' ',' ')
    call sub_defvar ('day', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','day', ' ',' ')
    call sub_defvar ('hour', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','hour', ' ',' ')
    call sub_defvar ('minute', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','minute', ' ',' ')
    call sub_defvar ('second', dum_iou, 1, loc_it, loc_c0, loc_c0, ' ', 'F','second', ' ',' ')
    !-----------------------------------------------------------------------
    !       define 2d data (x,y)
    !-----------------------------------------------------------------------
    loc_it(1) = loc_id_lonm
    loc_it(2) = loc_id_latm
    call sub_defvar ('grid_mask', dum_iou, 2, loc_it, loc_c0, loc_c100, ' ', 'F', &
         &'land-sea mask', ' ' ,'n/a')

!!$    !-----------------------------------------------------------------------
!!$       !-----------------------------------------------------------------------
!!$       !       define 3d data (x,y,t)
!!$       !-----------------------------------------------------------------------
!!$       loc_it(1) = loc_id_lonm
!!$       loc_it(2) = loc_id_latm
!!$       loc_it(3) = loc_id_time
!!$       if (opt_data(iopt_data_save_slice_ocnatm)) then
!!$         do n = 3,n_l_atm
!!$           call sub_defvar ('atm_'//string_atm_tname(n), dum_iou, 3, &
!!$               & loc_it, atm_mima(n,1), atm_mima(n,2), ' ', 'F', &
!!$               & string_atm_tlname(n), ' ', string_atm_unit(n))
!!$         enddo
!!$       end if
!!$       !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    !       end definitions
    !-----------------------------------------------------------------------
    call sub_enddef (dum_iou)
    call sub_closefile (dum_iou)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_init_netcdf_rg
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_save_netcdf(dum_yr)
    !-----------------------------------------------------------------------
    !       dummy arguments
    !-----------------------------------------------------------------------
    REAL,INTENT(in):: dum_yr
    !-----------------------------------------------------------------------
    !       local variables
    !-----------------------------------------------------------------------
    character(255) :: loc_name
    real           :: loc_c0, loc_c1, loc_c10, loc_c500, loc_c1e3, loc_c1e6
    integer        :: loc_i, loc_iou, loc_ntrec
    real,dimension(n_i,n_j) :: loc_mask_surf, loc_help2d
    real,dimension(n_i+1) :: loc_lon_e, loc_xu_e
    real,dimension(n_j+1) :: loc_lat_e, loc_yu_e
    logical :: loc_defined
    !-----------------------------------------------------------------------
    !       initialize local variables 
    !-----------------------------------------------------------------------
    loc_c0 = 0.
    loc_c1 = 1.
    loc_c10 = 10.
    loc_c500 = 500.
    loc_c1e3 = 1.e3
    loc_c1e6 = 1.e6
    !
    loc_mask_surf(:,:) = 0.0
    loc_help2d(:,:)    = 0.0
    loc_lon_e(:)       = 0.0
    loc_xu_e(:)        = 0.0
    loc_lat_e(:)       = 0.0
    loc_yu_e(:)        = 0.0
    ! 
    loc_name = string_ncout2d_rg
    !print*,'loc_name = ',string_ncout2d_rg
    loc_iou = ncout2d_iou_rg
    !print*,'loc_iou = ',ncout2d_iou_rg
    loc_ntrec = ncout2d_ntrec_rg
    !print*,'loc_ntrec = ',ncout2d_ntrec_rg
    !-----------------------------------------------------------------------
    !       open file and get latest record number
    !-----------------------------------------------------------------------
    loc_defined = .true.
    loc_i = 0
    if (loc_ntrec .eq. 0) then 
       loc_defined = .false.
       loc_i = 1
    end if
    call sub_opennext (loc_name, dum_yr, loc_i, loc_ntrec, loc_iou)
    !-----------------------------------------------------------------------
    !       write 1d data (x, y or z)
    !-----------------------------------------------------------------------
    call sub_putvars  ('time', loc_iou, loc_ntrec, dum_yr, loc_c1, loc_c0)
    call sub_putvarIs  ('year', loc_iou, loc_ntrec, nint(dum_yr), loc_c1, loc_c0)
    ! 
    if(.not. loc_defined) then
       
       call sub_putvar1d ('lon', loc_iou, n_i, loc_ntrec, n_i, &
            & phys_rok(ipr_lon,:,1), loc_c1, loc_c0)
       call edge_maker (1, loc_lon_e, phys_rok(ipr_lon,:,1), &
            & phys_rok(ipr_lone,:,1), phys_rok(ipr_dlon,:,1), n_i)
       call sub_putvar1d ('lon_edges', loc_iou, n_i+1, loc_ntrec, n_i+1, &
            & loc_lon_e, loc_c1, loc_c0)
       call sub_putvar1d ('xu', loc_iou, n_i, loc_ntrec, n_i, &
            & loc_lon_e(1:n_i), loc_c1, loc_c0)
       call edge_maker (2, loc_xu_e, phys_rok(ipr_lon,:,1), &
            & phys_rok(ipr_lone,:,1), phys_rok(ipr_dlon,:,1), n_i)
       call sub_putvar1d ('xu_edges', loc_iou, n_i+1, loc_ntrec, n_i+1, &
            & loc_xu_e, loc_c1, loc_c0)
       
       call sub_putvar1d ('lat', loc_iou, n_j, loc_ntrec, n_j, &
            & phys_rok(ipr_lat,1,:), loc_c1, loc_c0)
       call edge_maker (1, loc_lat_e, phys_rok(ipr_lat,1,:), &
            & phys_rok(ipr_latn,1,:), phys_rok(ipr_dlat,1,:), n_j)
       call sub_putvar1d ('lat_edges', loc_iou, n_j+1, loc_ntrec, n_j+1, &
            & loc_lat_e, loc_c1, loc_c0)
       call sub_putvar1d ('yu', loc_iou, n_j, loc_ntrec, n_j, &
            & loc_lat_e(1:n_j), loc_c1, loc_c0)
       call edge_maker (2, loc_yu_e, phys_rok(ipr_lat,1,:), &
            & phys_rok(ipr_latn,1,:), phys_rok(ipr_dlat,1,:), n_j)
       call sub_putvar1d ('yu_edges', loc_iou, n_j+1, loc_ntrec, n_j+1, &
            & loc_yu_e, loc_c1, loc_c0)
      
       !-----------------------------------------------------------------------
       !       write 2d data (x,y)
       !-----------------------------------------------------------------------
       loc_mask_surf(:,:) = 1.0
       where ( landmask(:,:) .eq. 0 )
          loc_mask_surf = 0.0
       endwhere
       call sub_putvar2d ('grid_mask', loc_iou, n_i, n_j, loc_ntrec, &
          & loc_mask_surf(:,:), loc_mask_surf)

    end if
    !-----------------------------------------------------------------------

       ncout2d_ntrec_rg = loc_ntrec
       ncout2d_iou_rg = loc_iou

    ! 
    call sub_sync(loc_iou)
    !-----------------------------------------------------------------------
  END SUBROUTINE sub_save_netcdf
  ! ****************************************************************************************************************************** !

END MODULE rokgem_data_netCDF
