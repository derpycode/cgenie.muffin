! $Id$

module write_netcdf

contains

  ! Atmos test write routine for 'knowngood's
  subroutine buildtest_atm_write()

    use genie_control, only: outputdir_name,ilon1_atm,ilat1_atm,BUFSIZ
    use genie_global,  only: surf_tstarinst_atm,netlong_atm,alon1_atm, &
         alat1_atm,koverall

    ! locals
    character(len=BUFSIZ)                          :: fname
    character(len=BUFSIZ)                          :: koverallstring
    character(len=BUFSIZ),allocatable,dimension(:) :: varnames
    real,allocatable,dimension(:,:,:)              :: data

    ! construct filename
    write(koverallstring,'(i6.6)') koverall
    fname=trim(outputdir_name)//'/genie_buildtest_atm_' &
         //trim(koverallstring)//'_.nc'

    allocate(data(ilon1_atm,ilat1_atm,2))
    allocate(varnames(2))
    data(:,:,1) = surf_tstarinst_atm
    varnames(1) = 'surf_tstarinst_atm'
    data(:,:,2) = netlong_atm
    varnames(2) = 'netlong_atm'
    
    call write_netcdf_genie(fname,data,varnames,alon1_atm,alat1_atm)
    deallocate(data)
    deallocate(varnames)
    
  end subroutine buildtest_atm_write

  ! Ocean test write routine for 'knowngood's
  subroutine buildtest_ocn_write()

    use genie_control, only: outputdir_name,ilon1_ocn,ilat1_ocn,BUFSIZ
    use genie_global,  only: tstar_ocn,netlong_ocn,alon1_ocn,alat1_ocn, &
         koverall

    ! locals
    character(len=BUFSIZ)                          :: fname
    character(len=BUFSIZ)                          :: koverallstring
    character(len=BUFSIZ),allocatable,dimension(:) :: varnames
    real,allocatable,dimension(:,:,:)              :: data

    ! construct filename
    write(koverallstring,'(i6.6)') koverall
    fname=trim(outputdir_name)//'/genie_buildtest_ocn_' &
         //trim(koverallstring)//'_.nc'

    allocate(data(ilon1_ocn,ilat1_ocn,2))
    allocate(varnames(2))
    data(:,:,1) = tstar_ocn
    varnames(1) = 'tstar_ocn'
    data(:,:,2) = netlong_ocn
    varnames(2) = 'netlong_ocn'
    
    call write_netcdf_genie(fname,data,varnames,alon1_ocn,alat1_ocn)
    deallocate(data)
    deallocate(varnames)
    
  end subroutine buildtest_ocn_write

  subroutine write_netcdf_genie(filename,data1_in,invarnames,alon1_in,alat1_in)

!     ***********************************************************
!     djl, 14/11/2003
!     subroutine to enable instantaneous easy output of 
!     grads/ferret-friendly single-level netcdf data 
!
!     this version for the genie grid (ocean or atmosphere).  
!     (see also write_netcdf_igcm.f and write_netcdf_std.f in genie-igcm3)
!
!     [filename    = output file name]
!     [data1       = data to be output]
!     [invarnames  = name to be given to the netcdf variable]
!     [alon1,alat1 = values of longitudes and latitudes]
!     [nx,ny       = number of longitudes and latitudes]
!     
!     dagw, 17/11/2004
!     made into an f90 module 
!     ***********************************************************

!!!    use precision
    use genie_control

    implicit none

    include 'netcdf.inc'

    ! locals
    integer                                           :: nx,ny  !nx-lon,ny-lat
    integer                                           :: ndim,nvar
    integer, dimension(nall)                          :: natts,nattsvar,vdims,ndims
    integer, dimension(nmaxdims,nall)                 :: vadims
    character(len=BUFSIZ), dimension(nall,nfiles)     :: dimname,varname
    character(len=BUFSIZ), dimension(2,nmaxdims,nall) :: attdimname,attvarname
    integer, dimension(nall,nfiles)                   :: iddim,idvar

    !     GENIE GRID:
    real, intent(in), dimension(:)             :: alon1_in
    real, intent(in), dimension(:)             :: alat1_in
    real, allocatable, dimension(:) :: alon1,alat1

    !     DATA:
    real, intent(in), dimension(:,:,:)             :: data1_in
    real, allocatable, dimension(:,:,:) :: data1

    character(len=*), intent(in)               :: filename
    character(len=*), intent(in), dimension(:) :: invarnames

    !     NETCDF + AWI STUFF: 
    integer :: ncid,loc_dim

    !     LOOPING:
    integer :: i,j,k

    integer, dimension(3) :: dataShape
    integer, dimension(1) :: varNamesShape,lonShape,latShape

    ! determine extent of longititude and latitude arrays
    lonShape = shape(alon1_in)
    latShape = shape(alat1_in)
    nx = lonShape(1)
    ny = latShape(1)

    ! determine extent of data and varaible name arrays
    dataShape = shape(data1_in)
    varNamesShape = shape(invarnames)

    ! test for consistency
    ! NB **ASSUME** THAT DATA ARRAY IS (LON:LAT:NUMVARS) 

    if (dataShape(1) .ne. nx) then
       print*,"Error:",__LINE__,__FILE__,"Longitude inconsistency"
       stop
    else if (dataShape(2) .ne. ny) then
       print*,"Error:",__LINE__,__FILE__,"Latitude inconsistency"
       stop
    else if (dataShape(3) .ne. varNamesShape(1)) then
       print*,"Error:",__LINE__,__FILE__,"Num vars inconsistency"
       stop
    end if

    nvar = varNamesShape(1)

    ! allocate internal storage
    allocate(alon1(nx))
    allocate(alat1(ny))
    allocate(data1(nx,ny,nvar))

    ! copy values--reasons of floating pt resolution
    do k=1,nvar
       do j=1,ny
          do i=1,nx          
             data1(i,j,k)=data1_in(i,j,k)
          enddo
       enddo
    enddo
    
    do i=1,nx
       alon1(i)=alon1_in(i)
    enddo
      
    do j=1,ny
       alat1(j)=alat1_in(j)
    enddo

    ! Set up dimension information
    ! NB **ASSUME** DATA ON SAME LON-LAT GRID ONLY
    ! COPIED FROM INITIALISE_ATMOS.F
    ndim=2
    dimname(1,1)='longitude'
    ndims(1)=nx
    natts(1)=2
    attdimname(1,1,1)='long_name'
    attdimname(2,1,1)='longitude'
    attdimname(1,2,1)='units'
    attdimname(2,2,1)='degrees east'
    
    dimname(2,1)='latitude'
    ndims(2)=ny
    natts(2)=2
    attdimname(1,1,2)='long_name'
    attdimname(2,1,2)='latitude'
    attdimname(1,2,2)='units'
    attdimname(2,2,2)='degrees north'
    
    ! Setup variable information
    do k=1,nvar
       varname(k,1)=trim(invarnames(k))
       vdims(k)=2
       vadims(1,k)=loc_dim('longitude',dimname,nall)
       vadims(2,k)=loc_dim('latitude',dimname,nall)
       nattsvar(k)=2
       attvarname(1,1,k)='long_name'
       attvarname(2,1,k)='data from the model'
       attvarname(1,2,k)='units'
       attvarname(2,2,k)='no units'
    enddo
    
    call ininc(trim(filename),       &
         nmaxdims,ndim,nvar,         &
         natts,nattsvar,             &
         vdims,vadims,ndims,         &
         dimname(1,1),varname(1,1),  &
         attdimname,attvarname,      &
         ncid,iddim(1,1),idvar(1,1))

    call writedim(ncid,iddim(1,1),alon1)
    call writedim(ncid,iddim(2,1),alat1)
    
    do k=1,nvar
       call writevar(ncid,                                 &
            idvar(loc_dim(invarnames(k),varname(1,1),nall),1), &
            data1(:,:,k) )
    enddo

    call closenc(ncid)
    
    deallocate(alon1)
    deallocate(alat1)
    deallocate(data1)
    
    return
  end subroutine write_netcdf_genie
end module write_netcdf
