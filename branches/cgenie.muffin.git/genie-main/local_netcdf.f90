!----------------------------------------------------------------------
!>
!> Module: local_netcdf
!>
!> Interface module talking to the NetCDF library
!> 
!----------------------------------------------------------------------
module local_netcdf

  use netcdf

  ! These interface blocks create 'procedure overloading' in
  ! OOP terminology, or 'generic procedures' in Fortran90-speak.
  interface defineDims
     module procedure defineNocoordDims
     module procedure defineRealDims
     module procedure defineIntegerDims
     module procedure defineRealRecordDims
  end interface

  interface defineVars
     module procedure defineReal1dVars
     module procedure defineReal2dVars
     module procedure defineReal3dVars
     module procedure defineInteger1dVars
     module procedure defineInteger2dVars
     module procedure defineInteger3dVars
     module procedure defineReal1dRecordVars
     module procedure defineReal2dRecordVars
     module procedure defineReal3dRecordVars
     module procedure defineInteger1dRecordVars
     module procedure defineInteger2dRecordVars
     module procedure defineInteger3dRecordVars
  end interface

  interface dimVars
     module procedure dimReal1dVars
     module procedure dimReal2dVars
     module procedure dimReal3dVars
     module procedure dimInteger1dVars
     module procedure dimInteger2dVars
     module procedure dimInteger3dVars
     module procedure dimReal1dRecordVars
     module procedure dimReal2dRecordVars
     module procedure dimReal3dRecordVars
     module procedure dimInteger1dRecordVars
     module procedure dimInteger2dRecordVars
     module procedure dimInteger3dRecordVars
  end interface

  interface writeVars
     module procedure writeReal1dVars
     module procedure writeReal2dVars
     module procedure writeReal3dVars
     module procedure writeInteger1dVars
     module procedure writeInteger2dVars
     module procedure writeInteger3dVars
  end interface

  ! lookup information about variables by using its name
  interface lookupVars
     module procedure lookupReal1dVars
     module procedure lookupReal2dVars
     module procedure lookupReal3dVars
     module procedure lookupInteger1dVars
     module procedure lookupInteger2dVars
     module procedure lookupInteger3dVars
     module procedure lookupReal1dRecordVars
     module procedure lookupReal2dRecordVars
     module procedure lookupReal3dRecordVars
     module procedure lookupInteger1dRecordVars
     module procedure lookupInteger2dRecordVars
     module procedure lookupInteger3dRecordVars
  end interface

  ! append variables as new entries at the end of record dimensions (if there is one)
  interface appendVars
     module procedure appendReal1dVars
     module procedure appendReal2dVars
     module procedure appendReal3dVars
     module procedure appendInteger1dVars
     module procedure appendInteger2dVars
     module procedure appendInteger3dVars
  end interface

  ! Declaration of user-derived data types.
  ! Attributes for variables
  type basicAttInfo
     character(len=nf90_max_name)         :: long_name
     character(len=nf90_max_name)         :: standard_name
     character(len=nf90_max_name)         :: units
     real                                 :: missing_value = -9.99e9
  end type basicAttInfo

  type dimInfo
     character(len=nf90_max_name)         :: name                     ! dimension name
     integer                              :: len                      ! length of dimension
     integer                              :: id                       ! id assigned by lib call
  end type dimInfo

  type realDimInfo
     character(len=nf90_max_name)         :: name                     ! dimension name
     integer                              :: len                      ! length of dimension
     integer                              :: id                       ! id assigned by lib call
     logical                              :: coordsDefine = .true.    ! define coordinates?
     logical                              :: boundsDefine = .false.   ! define boundaries? only rectangular grids supported
     type(basicAttInfo)                   :: basicAtts                ! attributes for the coordinates
     real,pointer,dimension(:)            :: coords                   ! coordinates
     real,pointer,dimension(:)            :: boundsLower              ! lower and upper boundaries of the coordinates
     real,pointer,dimension(:)            :: boundsUpper              ! lower and upper boundaries of the coordinates
  end type realDimInfo

  type integerDimInfo
     character(len=nf90_max_name)         :: name                     ! dimension name
     integer                              :: len                      ! length of dimension
     integer                              :: id                       ! id assigned by lib call
     logical                              :: coordsDefine = .true.    ! define coordinates?
     logical                              :: boundsDefine = .false.   ! define boundaries? only rectangular grids supported
     type(basicAttInfo)                   :: basicAtts                ! attributes for the coordinates
     integer,pointer,dimension(:)            :: coords                   ! coordinates
     integer,pointer,dimension(:)            :: boundsLower              ! lower and upper boundaries of the coordinates
     integer,pointer,dimension(:)            :: boundsUpper              ! lower and upper boundaries of the coordinates
  end type integerDimInfo

  type recordDimInfo
     character(len=nf90_max_name)         :: name                     ! dimension name
     integer                              :: len=NF90_UNLIMITED       ! unlimited length => record dimensions
     integer                              :: id                       ! id assigned by lib call
  end type recordDimInfo

  type realRecordDimInfo
     character(len=nf90_max_name)         :: name                     ! dimension name
     integer                              :: len=NF90_UNLIMITED       ! unlimited length => record dimensions
     integer                              :: id                       ! id assigned by lib call
     logical                              :: coordsDefine = .true.    ! define coordinates?
     logical                              :: boundsDefine = .false.   ! define boundaries? only rectangular grids supported
     type(basicAttInfo)                   :: basicAtts                ! attributes for the coordinates
  end type realRecordDimInfo

  type integerRecordDimInfo
     character(len=nf90_max_name)         :: name                     ! dimension name
     integer                              :: len=NF90_UNLIMITED       ! unlimited length => record dimensions
     integer                              :: id                       ! id assigned by lib call
     logical                              :: coordsDefine = .true.    ! define coordinates?
     logical                              :: boundsDefine = .false.   ! define boundaries? only rectangular grids supported
     type(basicAttInfo)                   :: basicAtts                ! attributes for the coordinates
  end type integerRecordDimInfo

  type real1dVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype =  NF90_FLOAT 
     integer                              :: ndims = 1 ! num associated dims
     integer,dimension(1)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(1)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(1) :: dimnames ! names of assoc dims
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:)            :: data
  end type real1dVar

  type real2dVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_FLOAT
     integer                              :: ndims = 2 ! num associated dims
     integer,dimension(2)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(2)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(2) :: dimnames ! names of assoc dims
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:,:)          :: data
  end type real2dVar

  type real3dVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_FLOAT
     integer                              :: ndims = 3 ! num associated dims
     integer,dimension(3)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(3)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(3) :: dimnames ! names of assoc dims
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:,:,:)        :: data
  end type real3dVar

  type integer1dVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype =  NF90_INT 
     integer                              :: ndims = 1 ! num associated dims
     integer,dimension(1)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(1)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(1) :: dimnames ! names of assoc dims
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     integer,pointer,dimension(:)            :: data
  end type integer1dVar

  type integer2dVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_INT
     integer                              :: ndims = 2 ! num associated dims
     integer,dimension(2)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(2)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(2) :: dimnames ! names of assoc dims
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     integer,pointer,dimension(:,:)          :: data
  end type integer2dVar

  type integer3dVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_INT
     integer                              :: ndims = 3 ! num associated dims
     integer,dimension(3)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(3)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(3) :: dimnames ! names of assoc dims
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     integer,pointer,dimension(:,:,:)        :: data
  end type integer3dVar

  type real1dRecordVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_FLOAT
     integer                              :: ndims = 2 ! num associated dims
     integer,dimension(2)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(2)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(2) :: dimnames ! names of assoc dims
     integer                              :: recordDimIndex = 0 ! Index to record dimension for arrays dimIDs, dimLens
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:)            :: data
  end type real1dRecordVar

  type real2dRecordVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_FLOAT
     integer                              :: ndims = 3 ! num associated dims
     integer,dimension(3)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(3)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(3) :: dimnames ! names of assoc dims
     integer                              :: recordDimIndex = 0 ! Index to record dimension for arrays dimIDs, dimLens
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:,:)          :: data
  end type real2dRecordVar

  type real3dRecordVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_FLOAT
     integer                              :: ndims = 4 ! num associated dims
     integer,dimension(4)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(4)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(4) :: dimnames ! names of assoc dims
     integer                              :: recordDimIndex = 0 ! Index to record dimension for arrays dimIDs, dimLens
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:,:,:)        :: data
  end type real3dRecordVar

  type integer1dRecordVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_INT
     integer                              :: ndims = 2 ! num associated dims
     integer,dimension(2)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(2)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(2) :: dimnames ! names of assoc dims
     integer                              :: recordDimIndex = 0 ! Index to record dimension for arrays dimIDs, dimLens
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:)            :: data
  end type integer1dRecordVar

  type integer2dRecordVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_INT
     integer                              :: ndims = 3 ! num associated dims
     integer,dimension(3)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(3)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(3) :: dimnames ! names of assoc dims
     integer                              :: recordDimIndex = 0 ! Index to record dimension for arrays dimIDs, dimLens
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:,:)          :: data
  end type integer2dRecordVar

  type integer3dRecordVar
     character(len=nf90_max_name)         :: name   ! variable name
     integer                              :: xtype  = NF90_INT
     integer                              :: ndims = 4 ! num associated dims
     integer,dimension(4)                 :: dimIDs ! IDs of assoc dims
     integer,dimension(4)                 :: dimLens ! lengths of assoc dims
     character(len=nf90_max_name),dimension(4) :: dimnames ! names of assoc dims
     integer                              :: recordDimIndex = 0 ! Index to record dimension for arrays dimIDs, dimLens
     integer                              :: nAtts  ! num associated atts
     integer                              :: id     ! id assigned by lib call
     type(basicAttInfo)                   :: basicAtts
     real,pointer,dimension(:,:,:)        :: data
  end type integer3dRecordVar

contains

  !----------------------------------------------------------------------
  !
  ! Subroutine: createNetCDF
  ! Opens a new dataset & returns dataset id
  !
  !----------------------------------------------------------------------

  subroutine createNetCDF(filename,ncid,clobber)

    implicit none

    ! args
    character(len=*),intent(in)   :: filename
    integer,intent(out)           :: ncid
    logical,optional              :: clobber
    ! locals
    integer                       :: status

    ! create the dataset
    ! NF90_NOCLOBBER: do not overwrite an existing dataset with same filename
    ! NF90_CLOBBER also available
    if (present(clobber).and.(clobber)) then
       status = nf90_create(trim(filename),NF90_CLOBBER,ncid)
    else
    status = nf90_create(trim(filename),NF90_NOCLOBBER,ncid)
    endif
    if (status /= NF90_NOERR) call handle_nc_err(status)
    ! write global attributes
    status = nf90_put_att(ncid,NF90_GLOBAL,"title", "Grid ENabled Integrated Earth system modelling (GENIE) model output")
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,NF90_GLOBAL,"institution", "__INSTITUTION__")
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,NF90_GLOBAL,"source", &
         & "Grid ENabled Integrated Earth system modelling (GENIE) framework version: __VERSION__")
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,NF90_GLOBAL,"history", "__TIMESTAMP__: Creation of dataset")
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,NF90_GLOBAL,"references", "http://www.genie.ac.uk")
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,NF90_GLOBAL,"comment", "")
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,NF90_GLOBAL,"Conventions", "CF-1.4")
    if (status /= NF90_NOERR) call handle_nc_err(status)

  end subroutine createNetCDF

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineNocoordDims
  !>
  !> Defines dimensions without coordinates in an opened dataset
  !> 
  !-----------------------------------------------------------------------  ! 

  subroutine defineNocoordDims(ncid,dims)

    implicit none

    ! args
    integer,intent(in)                           :: ncid
    type(DimInfo),intent(inout),dimension(:)     :: dims

    ! locals
    integer                                      :: status
    integer                                      :: ii

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(dims)

       ! defined dimensions
       ! dims(ii)%id is given a value by the nf90_def_dim function
       status = nf90_def_dim(ncid,dims(ii)%name,dims(ii)%len,dims(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)

    enddo

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

  end subroutine defineNocoordDims

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineRealDims
  !>
  !> Defines dimensions with real-valued coordinates in an opened dataset
  !> 
  !-----------------------------------------------------------------------

  subroutine defineRealDims(ncid,dims)

    implicit none

    ! args
    integer,intent(in)                           :: ncid
    type(realDimInfo),intent(inout),dimension(:) :: dims

    ! locals
    integer                                      :: status,varid,dimidBoundaries,varidBoundaries
    integer                                      :: ii

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(dims)

       ! defined dimensions
       ! dims(ii)%id is given a value by the nf90_def_dim function
       status = nf90_def_dim(ncid,dims(ii)%name,dims(ii)%len,dims(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       if (dims(ii)%coordsDefine) then
          ! define and write coordinates as 1-dimensional variables
          status = nf90_def_var(ncid,dims(ii)%name, &
               NF90_FLOAT, dims(ii)%id,varid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          ! Add appropriate attributes to the coordinate entry
          status = nf90_put_att(ncid,varid,"units", &
               dims(ii)%basicAtts%units)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_att(ncid,varid,"long_name", &
               dims(ii)%basicAtts%long_name)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_att(ncid,varid,"standard_name", &
               dims(ii)%basicAtts%standard_name)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          
          ! define and write coordinate boundaries if requested
          if (dims(ii)%boundsDefine) then
             ! unless already there, create dimension referring to the
             ! upper and lower boundaries, respectively
             ! no associated coordinates needed
             status = nf90_inq_dimid(ncid,"nv",dimidBoundaries)
             ! if non-existing, try to create the dimension
             if (status == NF90_EBADDIM) then
                status = nf90_def_dim(ncid,"nv",2,dimidBoundaries)
                if (status /= NF90_NOERR) call handle_nc_err(status)
             endif
             ! write boundary coordinates to dataset
             ! no attributes required, CF-compliance implies
             ! attributes from the coordinate variable to be used for
             ! the boundaries as well
             status = nf90_def_var(ncid,trim(dims(ii)%name)//"_bnds", &
                  NF90_FLOAT,(/ dimidBoundaries, dims(ii)%id /),varidBoundaries)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             ! add attribute to coordinate variable to reference to
             ! boundaries variable
             status = nf90_put_att(ncid,varid,"bounds", &
                  trim(dims(ii)%name)//"_bnds")
             if (status /= NF90_NOERR) call handle_nc_err(status)

          endif

       endif

    enddo

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(dims)
       
       if (dims(ii)%coordsDefine) then
          
          ! write the coordinates to the dataset
          status = nf90_put_var(ncid,varid,dims(ii)%coords)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          
          ! define and write coordinate boundaries if requested
          if (dims(ii)%boundsDefine) then
             
             status = nf90_put_var(ncid,varidBoundaries,dims(ii)%boundsLower,(/ 1, 1 /), (/ 1, dims(ii)%len, 1 /))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             status = nf90_put_var(ncid,varidBoundaries,dims(ii)%boundsUpper,(/ 2, 1 /), (/ 1, dims(ii)%len /))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             
          endif
          
       endif
       
    end do

  end subroutine defineRealDims

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineIntegerDims
  !>
  !> Defines dimensions with integer-valued coordinates in an opened
  !> dataset
  !> 
  !-----------------------------------------------------------------------

  subroutine defineIntegerDims(ncid,dims)

    implicit none

    ! args
    integer,intent(in)                              :: ncid
    type(integerDimInfo),intent(inout),dimension(:) :: dims

    ! locals
    integer                                      :: status,varid,dimidBoundaries,varidBoundaries
    integer                                      :: ii

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(dims)

       ! defined dimensions
       ! dims(ii)%id is given a value by the nf90_def_dim function
       status = nf90_def_dim(ncid,dims(ii)%name,dims(ii)%len,dims(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)

       if (dims(ii)%coordsDefine) then
          ! define and write coordinates as 1-dimensional variables
          status = nf90_def_var(ncid,dims(ii)%name, &
               NF90_FLOAT, dims(ii)%id,varid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          ! Add appropriate attributes to the coordinate entry
          status = nf90_put_att(ncid,varid,"units", &
               dims(ii)%basicAtts%units)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_att(ncid,varid,"long_name", &
               dims(ii)%basicAtts%long_name)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_att(ncid,varid,"standard_name", &
               dims(ii)%basicAtts%standard_name)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          
          ! define and write coordinate boundaries if requested
          if (dims(ii)%boundsDefine) then
             ! unless already there, create dimension referring to the
             ! upper and lower boundaries, respectively
             ! no associated coordinates needed
             status = nf90_inq_dimid(ncid,"nv",dimidBoundaries)
             ! if non-existing, try to create the dimension
             if (status == NF90_EBADDIM) then
                status = nf90_def_dim(ncid,"nv",2,dimidBoundaries)
                if (status /= NF90_NOERR) call handle_nc_err(status)
             endif
             if (status /= NF90_NOERR) call handle_nc_err(status)
             ! write boundary coordinates to dataset
             ! no attributes required, CF-compliance implies
             ! attributes from the coordinate variable to be used for
             ! the boundaries as well
             status = nf90_def_var(ncid,trim(dims(ii)%name)//"_bnds", &
                  NF90_FLOAT,(/ dimidBoundaries, dims(ii)%id /),varidBoundaries)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             ! add attribute to coordinate variable to reference to
             ! boundaries variable
             status = nf90_put_att(ncid,varid,"bounds", &
                  trim(dims(ii)%name)//"_bnds")
             if (status /= NF90_NOERR) call handle_nc_err(status)

          endif

       endif

    enddo

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(dims)
       
       if (dims(ii)%coordsDefine) then
          
          ! write the coordinates to the dataset
          status = nf90_put_var(ncid,varid,dims(ii)%coords)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          
          ! define and write coordinate boundaries if requested
          if (dims(ii)%boundsDefine) then
             
             status = nf90_put_var(ncid,varidBoundaries,dims(ii)%boundsLower,(/ 1, 1 /), (/ 1, dims(ii)%len, 1 /))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             status = nf90_put_var(ncid,varidBoundaries,dims(ii)%boundsUpper,(/ 2, 1 /), (/ 1, dims(ii)%len /))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             
          endif
          
       endif
       
    end do

  end subroutine defineIntegerDims

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineRealRecordDims
  !>
  !> Defines dimensions with integer-valued coordinates in an opened
  !> dataset
  !> 
  !-----------------------------------------------------------------------

  subroutine defineRealRecordDims(ncid,dims)

    implicit none

    ! args
    integer,intent(in)                                 :: ncid
    type(realRecordDimInfo),intent(inout),dimension(:) :: dims

    ! locals
    integer                                            :: status,varid,dimidBoundaries,varidBoundaries
    integer                                            :: ii

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(dims)

       ! defined dimensions
       ! dims(ii)%id is given a value by the nf90_def_dim function
       status = nf90_def_dim(ncid,dims(ii)%name,dims(ii)%len,dims(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)

       if (dims(ii)%coordsDefine) then
          ! define and write coordinates as 1-dimensional variables
          status = nf90_def_var(ncid,dims(ii)%name, &
               NF90_FLOAT, dims(ii)%id,varid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          ! Add appropriate attributes to the coordinate entry
          status = nf90_put_att(ncid,varid,"units", &
               dims(ii)%basicAtts%units)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_att(ncid,varid,"long_name", &
               dims(ii)%basicAtts%long_name)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_att(ncid,varid,"standard_name", &
               dims(ii)%basicAtts%standard_name)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          
          ! define and write coordinate boundaries if requested
          if (dims(ii)%boundsDefine) then
             ! unless already there, create dimension referring to the
             ! upper and lower boundaries, respectively
             ! no associated coordinates needed
             status = nf90_inq_dimid(ncid,"nv",dimidBoundaries)
             ! if non-existing, try to create the dimension
             if (status == NF90_EBADDIM) then
                status = nf90_def_dim(ncid,"nv",2,dimidBoundaries)
                if (status /= NF90_NOERR) call handle_nc_err(status)
             endif
             if (status /= NF90_NOERR) call handle_nc_err(status)
             ! write boundary coordinates to dataset
             ! no attributes required, CF-compliance implies
             ! attributes from the coordinate variable to be used for
             ! the boundaries as well
             status = nf90_def_var(ncid,trim(dims(ii)%name)//"_bnds", &
                  NF90_FLOAT,(/ dimidBoundaries, dims(ii)%id /),varidBoundaries)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             ! add attribute to coordinate variable to reference to
             ! boundaries variable
             status = nf90_put_att(ncid,varid,"bounds", &
                  trim(dims(ii)%name)//"_bnds")
             if (status /= NF90_NOERR) call handle_nc_err(status)

          endif

       endif

    enddo

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

  end subroutine defineRealRecordDims

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineReal1dVars
  !>
  !> Defines a set of 'flat' variables (1D, real valued) in a open dataset
  !>
  !-----------------------------------------------------------------------

  subroutine defineReal1dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real1dVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                        :: ii
    integer                                        :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineReal1dVars


  !----------------------------------------------------------------------
  !
  ! Subroutine: defineReal2dVars
  !
  ! Defines a set of 'flat' variables (2D, real valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineReal2dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real2dVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                        :: ii
    integer                                        :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineReal2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineReal3dVars
  !
  ! Defines a set of 'flat' variables (3D, real valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineReal3dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real3dVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                        :: ii
    integer                                        :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineReal3dVars

  !-----------------------------------------------------------------------
  !>
  !> Subroutine: defineInteger1dVars
  !>
  !> Defines a set of 'flat' variables (1D, integer valued) in a open dataset
  !>
  !-----------------------------------------------------------------------

  subroutine defineInteger1dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer1dVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                        :: ii
    integer                                        :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineInteger1dVars


  !----------------------------------------------------------------------
  !
  ! Subroutine: defineInteger2dVars
  !
  ! Defines a set of 'flat' variables (2D, integer valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineInteger2dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer2dVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                        :: ii
    integer                                        :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineInteger2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineInteger3dVars
  !
  ! Defines a set of 'flat' variables (3D, integer valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineInteger3dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer3dVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                        :: ii
    integer                                        :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineInteger3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineReal1dRecordVars
  !
  ! Defines a set of 'flat' variables (1D, real valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineReal1dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(real1dRecordVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                          :: ii
    integer                                          :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineReal1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineReal2dRecordVars
  !
  ! Defines a set of 'flat' variables (2D, real valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineReal2dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(real2dRecordVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                          :: ii
    integer                                          :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineReal2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineReal3dRecordVars
  !
  ! Defines a set of 'flat' variables (3D, real valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineReal3dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(real3dRecordVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                          :: ii
    integer                                          :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineReal3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineInteger1dRecordVars
  !
  ! Defines a set of 'flat' variables (1D, real valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineInteger1dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(integer1dRecordVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                          :: ii
    integer                                          :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineInteger1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineInteger2dRecordVars
  !
  ! Defines a set of 'flat' variables (2D, real valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineInteger2dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(integer2dRecordVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                          :: ii
    integer                                          :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineInteger2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: defineInteger3dRecordVars
  !
  ! Defines a set of 'flat' variables (3D, real valued) in a open dataset
  !
  !----------------------------------------------------------------------

  subroutine defineInteger3dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(integer3dRecordVar),intent(inout),dimension(:) :: vars

    ! locals
    integer                                          :: ii
    integer                                          :: status

    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    do ii=1,size(vars)
       ! nf90_def_var sets value for vars(ii)%id
       status = nf90_def_var(ncid,vars(ii)%name, &
            vars(ii)%xtype, vars(ii)%dimIDs, vars(ii)%id)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       call addVarAttributes(ncid,vars(ii)%id,vars(ii)%basicAtts)
    end do

    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

    call lookupVars(ncid,vars)

  end subroutine defineInteger3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: addVarAttributes
  !
  ! Internal subroutine, common functionality for all define*Vars
  ! functions
  !
  !----------------------------------------------------------------------

  subroutine addVarAttributes(ncid,varid,basicAtts)
    
    integer,intent(in) :: ncid,varid

    type(basicAttInfo),intent(in) :: basicAtts

    integer :: status

    ! Write any appropriate attributes to the dataset
    ! e.g. units here
    status = nf90_put_att(ncid,varid,"units", &
         basicAtts%units)
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,varid,"long_name", &
         basicAtts%long_name)
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,varid,"standard_name", &
         basicAtts%standard_name)
    if (status /= NF90_NOERR) call handle_nc_err(status)
    status = nf90_put_att(ncid,varid,"missing_value", &
         basicAtts%missing_value)
    if (status /= NF90_NOERR) call handle_nc_err(status)
    
  end subroutine addVarAttributes

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimReal1dVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimReal1dVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real1dVar),intent(inout),dimension(:)     :: vars
    character(len=*),intent(in),dimension(1)       :: dimnames

    ! locals
    integer                                        :: ii,dimid
    integer                                        :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid

    end do

  end subroutine dimReal1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimReal2dVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimReal2dVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real2dVar),intent(inout),dimension(:)     :: vars
    character(len=*),intent(in),dimension(2)       :: dimnames

    ! locals
    integer                                        :: ii,dimid
    integer                                        :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid

    end do

  end subroutine dimReal2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimReal3dVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimReal3dVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real3dVar),intent(inout),dimension(:)     :: vars
    character(len=*),intent(in),dimension(3)       :: dimnames

    ! locals
    integer                                        :: ii,dimid
    integer                                        :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(3)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid

    end do

  end subroutine dimReal3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimInteger1dVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimInteger1dVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer1dVar),intent(inout),dimension(:)     :: vars
    character(len=*),intent(in),dimension(1)       :: dimnames

    ! locals
    integer                                        :: ii,dimid
    integer                                        :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid

    end do

  end subroutine dimInteger1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimInteger2dVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimInteger2dVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer2dVar),intent(inout),dimension(:)     :: vars
    character(len=*),intent(in),dimension(2)       :: dimnames

    ! locals
    integer                                        :: ii,dimid
    integer                                        :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid

    end do

  end subroutine dimInteger2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimInteger3dVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimInteger3dVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer3dVar),intent(inout),dimension(:)     :: vars
    character(len=*),intent(in),dimension(3)       :: dimnames

    ! locals
    integer                                        :: ii,dimid
    integer                                        :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(3)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid

    end do

  end subroutine dimInteger3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimReal1dRecordVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimReal1dRecordVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(real1dRecordVar),intent(inout),dimension(:) :: vars
    character(len=*),intent(in),dimension(2)         :: dimnames

    ! locals
    integer                                          :: ii,dimid
    integer                                          :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid

    end do

  end subroutine dimReal1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimReal2dRecordVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimReal2dRecordVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(real2dRecordVar),intent(inout),dimension(:) :: vars
    character(len=*),intent(in),dimension(3)         :: dimnames

    ! locals
    integer                                          :: ii,dimid
    integer                                          :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(3)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid

    end do

  end subroutine dimReal2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimReal3dRecordVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimReal3dRecordVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(real3dRecordVar),intent(inout),dimension(:) :: vars
    character(len=*),intent(in),dimension(4)         :: dimnames

    ! locals
    integer                                          :: ii,dimid
    integer                                          :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(3)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(4)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(4) = dimid

    end do

  end subroutine dimReal3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimInteger1dRecordVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimInteger1dRecordVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(integer1dRecordVar),intent(inout),dimension(:) :: vars
    character(len=*),intent(in),dimension(2)         :: dimnames

    ! locals
    integer                                          :: ii,dimid
    integer                                          :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid

    end do

  end subroutine dimInteger1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimInteger2dRecordVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimInteger2dRecordVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(integer2dRecordVar),intent(inout),dimension(:) :: vars
    character(len=*),intent(in),dimension(3)         :: dimnames

    ! locals
    integer                                          :: ii,dimid
    integer                                          :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(3)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid

    end do

  end subroutine dimInteger2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: dimInteger3dRecordVars
  !
  ! 
  !
  !----------------------------------------------------------------------

  subroutine dimInteger3dRecordVars(ncid,vars,dimnames)

    implicit none

    ! args
    integer,intent(in)                               :: ncid
    type(integer3dRecordVar),intent(inout),dimension(:) :: vars
    character(len=*),intent(in),dimension(4)         :: dimnames

    ! locals
    integer                                          :: ii,dimid
    integer                                          :: status

    do ii=1,size(vars)

       status = nf90_inq_dimid(ncid,trim(dimnames(1)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(1) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(2)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(2) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(3)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(3) = dimid
       status = nf90_inq_dimid(ncid,trim(dimnames(4)),dimid)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       vars(ii)%dimIDs(4) = dimid

    end do

  end subroutine dimInteger3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupReal1dVars
  !
  ! Lookup information about variables (1D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupReal1dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(real1dVar),dimension(:),intent(inout)     :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii,jj
    integer                                        :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_FLOAT).and.(xtype /= NF90_DOUBLE)) call handle_err('Wrong variable type!')
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)          
          enddo
       endif
    end do

  end subroutine lookupReal1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupReal2dVars
  !
  ! Lookup information about variables (2D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupReal2dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(real2dVar),dimension(:),intent(inout)     :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii,jj
    integer                                        :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_FLOAT).and.(xtype /= NF90_DOUBLE)) call handle_err('Wrong variable type!')
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)          
          enddo
       endif
    end do

  end subroutine lookupReal2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupReal3dVars
  !
  ! Lookup information about variables (3D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupReal3dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(real3dVar),dimension(:),intent(inout)    :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii,jj
    integer                                        :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_FLOAT).and.(xtype /= NF90_DOUBLE)) call handle_err('Wrong variable type!')
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)          
          enddo
       endif
    end do

  end subroutine lookupReal3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupInteger1dVars
  !
  ! Lookup information about variables (1D, 'flat', integer-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupInteger1dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(integer1dVar),dimension(:),intent(inout)     :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii,jj
    integer                                        :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_INT).and.(xtype /= NF90_SHORT)) call handle_err('Wrong variable type!')
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)          
          enddo
       endif
    end do

  end subroutine lookupInteger1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupInteger2dVars
  !
  ! Lookup information about variables (2D, 'flat', integer-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupInteger2dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(integer2dVar),dimension(:),intent(inout)  :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii,jj
    integer                                        :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_INT).and.(xtype /= NF90_SHORT)) call handle_err('Wrong variable type!')
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)          
          enddo
       endif
    end do

  end subroutine lookupInteger2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupInteger3dVars
  !
  ! Lookup information about variables (3D, 'flat', integer-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupInteger3dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(integer3dVar),dimension(:),intent(inout)    :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii,jj
    integer                                        :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status) 
          if ((xtype /= NF90_INT).and.(xtype /= NF90_SHORT)) call handle_err('Wrong variable type!')
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)          
          enddo
       endif
    end do

  end subroutine lookupInteger3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupReal1dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupReal1dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid    
    type(real1dRecordVar),dimension(:),intent(inout) :: vars

    ! locals
    integer                                          :: status
    integer                                          :: ii,jj
    integer                                          :: recdimid
    integer                                          :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status) 
          if ((xtype /= NF90_FLOAT).and.(xtype /= NF90_DOUBLE)) call handle_err('Wrong variable type!')
          status=nf90_inquire(ncid,unlimitedDimId=recdimid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (recdimid == -1) call handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             if (vars(ii)%dimIDs(jj).eq.recdimid) then
                vars(ii)%recordDimIndex = jj
             endif
          enddo
       endif
    end do

  end subroutine lookupReal1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupReal2dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupReal2dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid    
    type(real2dRecordVar),dimension(:),intent(inout) :: vars

    ! locals
    integer                                          :: status
    integer                                          :: ii,jj
    integer                                          :: recdimid
    integer                                          :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_FLOAT).and.(xtype /= NF90_DOUBLE)) call handle_err('Wrong variable type!')
          status=nf90_inquire(ncid,unlimitedDimId=recdimid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (recdimid == -1) call handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             if (vars(ii)%dimIDs(jj).eq.recdimid) then
                vars(ii)%recordDimIndex = jj
             endif
          enddo
       endif
    end do

  end subroutine lookupReal2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupReal3dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupReal3dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid    
    type(real3dRecordVar),dimension(:),intent(inout) :: vars

    ! locals
    integer                                          :: status
    integer                                          :: ii,jj
    integer                                          :: recdimid
    integer                                          :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_FLOAT).and.(xtype /= NF90_DOUBLE)) call handle_err('Wrong variable type!')
          status=nf90_inquire(ncid,unlimitedDimId=recdimid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (recdimid == -1) call handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             if (vars(ii)%dimIDs(jj).eq.recdimid) then
                vars(ii)%recordDimIndex = jj
             endif
          enddo
       endif
    end do

  end subroutine lookupReal3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupInteger1dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupInteger1dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid    
    type(integer1dRecordVar),dimension(:),intent(inout) :: vars

    ! locals
    integer                                          :: status
    integer                                          :: ii,jj
    integer                                          :: recdimid
    integer                                          :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_INT).and.(xtype /= NF90_SHORT)) call handle_err('Wrong variable type!')
          status=nf90_inquire(ncid,unlimitedDimId=recdimid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (recdimid == -1) call handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             if (vars(ii)%dimIDs(jj).eq.recdimid) then
                vars(ii)%recordDimIndex = jj
             endif
          enddo
       endif
    end do

  end subroutine lookupInteger1dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupInteger2dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupInteger2dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid    
    type(integer2dRecordVar),dimension(:),intent(inout) :: vars

    ! locals
    integer                                          :: status
    integer                                          :: ii,jj
    integer                                          :: recdimid
    integer                                          :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_INT).and.(xtype /= NF90_SHORT)) call handle_err('Wrong variable type!')
          status=nf90_inquire(ncid,unlimitedDimId=recdimid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (recdimid == -1) call handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             if (vars(ii)%dimIDs(jj).eq.recdimid) then
                vars(ii)%recordDimIndex = jj
             endif
          enddo
       endif
    end do

  end subroutine lookupInteger2dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: lookupInteger3dRecordVars
  !
  ! Lookup information about variables (2D, 'flat', real-valued)
  ! sets vars(n)%id to -99 if variable not found
  !
  !----------------------------------------------------------------------

  subroutine lookupInteger3dRecordVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                               :: ncid    
    type(integer3dRecordVar),dimension(:),intent(inout) :: vars

    ! locals
    integer                                          :: status
    integer                                          :: ii,jj
    integer                                          :: recdimid
    integer                                          :: xtype


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_inq_varid(ncid,vars(ii)%name,vars(ii)%id)
       if (status == NF90_ENOTVAR) then
          vars(ii)%id = -99
       else
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_inquire_variable(ncid,vars(ii)%id,vars(ii)%name,xtype,vars(ii)%ndims,vars(ii)%dimIDs)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if ((xtype /= NF90_INT).and.(xtype /= NF90_SHORT)) call handle_err('Wrong variable type!')
          status=nf90_inquire(ncid,unlimitedDimId=recdimid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (recdimid == -1) call handle_err('No record dimension found!')
          vars(ii)%recordDimIndex = 0
          do jj=1,vars(ii)%ndims
             status = nf90_inquire_dimension(ncid,vars(ii)%dimIDs(jj),vars(ii)%dimnames(jj),vars(ii)%dimLens(jj))
             if (status /= NF90_NOERR) call handle_nc_err(status)
             if (vars(ii)%dimIDs(jj).eq.recdimid) then
                vars(ii)%recordDimIndex = jj
             endif
          enddo
       endif
    end do

  end subroutine lookupInteger3dRecordVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeReal1dVars
  !
  ! Write set of variables (1D, 'flat', real-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  subroutine writeReal1dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(real1dVar),dimension(:),intent(in)        :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data)
       if (status /= NF90_NOERR) call handle_nc_err(status)
    end do

  end subroutine writeReal1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeReal2dVars
  !
  ! Write set of variables (2D, 'flat', real-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  subroutine writeReal2dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(real2dVar),dimension(:),intent(in)    :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii

    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data)
       if (status /= NF90_NOERR) call handle_nc_err(status)
    end do

  end subroutine writeReal2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeReal3dVars
  !
  ! Write set of variables (3D, 'flat', real-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  subroutine writeReal3dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(real3dVar),dimension(:),intent(in)    :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii

    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data)
       if (status /= NF90_NOERR) call handle_nc_err(status)
    end do

  end subroutine writeReal3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeInteger1dVars
  !
  ! Write set of variables (1D, 'flat', integer-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  subroutine writeInteger1dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(integer1dVar),dimension(:),intent(in)        :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii


    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data)
       if (status /= NF90_NOERR) call handle_nc_err(status)
    end do

  end subroutine writeInteger1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeInteger2dVars
  !
  ! Write set of variables (2D, 'flat', integer-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  subroutine writeInteger2dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(integer2dVar),dimension(:),intent(in)    :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii

    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data)
       if (status /= NF90_NOERR) call handle_nc_err(status)
    end do

  end subroutine writeInteger2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: writeInteger3dVars
  !
  ! Write set of variables (3D, 'flat', integer-valued) to a setup dataset
  !
  !----------------------------------------------------------------------

  subroutine writeInteger3dVars(ncid,vars)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    
    type(integer3dVar),dimension(:),intent(in)    :: vars

    ! locals
    integer                                        :: status
    integer                                        :: ii

    ! write the values to the dataset
    do ii=1,size(vars)
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data)
       if (status /= NF90_NOERR) call handle_nc_err(status)
    end do

  end subroutine writeInteger3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendReal1dVars
  !
  ! Append set of variables (1D, 'flat', real-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  subroutine appendReal1dVars(ncid,vars,recordCoord,recordCoordBounds,offset)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real1dRecordVar),dimension(:),intent(in)  :: vars
    real,intent(in),optional                       :: recordCoord
    real,intent(in),dimension(2),optional          :: recordCoordBounds
    integer,intent(in),optional                    :: offset

    ! locals
    integer                                        :: status
    integer                                        :: ii
    integer                                        :: recdimlen
    integer                                        :: recvarid
    integer,dimension(2)                           :: startoffset

    ! write the values to the dataset
    do ii=1,size(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       if (present(offset)) recdimlen=recdimlen-1+offset
       select case (vars(ii)%recordDimIndex)
       case (0)
          call handle_err("Variable not defined along a record dimension!")
       case (1)
          startoffset=(/recdimlen,1/)
       case (2)
          startoffset=(/1,recdimlen/)
       end select
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data,startoffset)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       if (present(recordCoord)) then
          status = nf90_inq_varid(ncid,vars(ii)%dimnames(vars(ii)%recordDimIndex),recvarid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_var(ncid,recvarid,recordCoord,(/ recdimlen /))
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (present(recordCoordBounds)) then
             status = nf90_inq_varid(ncid,trim(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds",recvarid)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             status = nf90_put_var(ncid,recvarid,recordCoordBounds,(/ 1,recdimlen /))
             if (status /= NF90_NOERR) call handle_nc_err(status)       
          endif
       endif
    end do

  end subroutine appendReal1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendReal2dVars
  !
  ! Append set of variables (2D, 'flat', real-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  subroutine appendReal2dVars(ncid,vars,recordCoord,recordCoordBounds,offset)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real2dRecordVar),dimension(:),intent(in)  :: vars
    real,intent(in),optional                       :: recordCoord
    real,intent(in),dimension(2),optional          :: recordCoordBounds
    integer,intent(in),optional                    :: offset

    ! locals
    integer                                        :: status
    integer                                        :: ii
    integer                                        :: recdimlen
    integer                                        :: recvarid
    integer,dimension(3)                           :: startoffset

    ! write the values to the dataset
    do ii=1,size(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       if (present(offset)) recdimlen=recdimlen-1+offset
       select case (vars(ii)%recordDimIndex)
       case (0)
          call handle_err("Variable not defined along a record dimension!")
       case (1)
          startoffset=(/recdimlen,1,1/)
       case (2)
          startoffset=(/1,recdimlen,1/)
       case (3)
          startoffset=(/1,1,recdimlen/)
       end select
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data,startoffset)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       if (present(recordCoord)) then
          status = nf90_inq_varid(ncid,vars(ii)%dimnames(vars(ii)%recordDimIndex),recvarid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_var(ncid,recvarid,recordCoord,(/ recdimlen /))
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (present(recordCoordBounds)) then
             status = nf90_inq_varid(ncid,trim(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds",recvarid)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             status = nf90_put_var(ncid,recvarid,recordCoordBounds,(/ 1,recdimlen /))
             if (status /= NF90_NOERR) call handle_nc_err(status)       
          endif
       endif
    end do

  end subroutine appendReal2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendReal3dVars
  !
  ! Append set of variables (3D, 'flat', real-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  subroutine appendReal3dVars(ncid,vars,recordCoord,recordCoordBounds,offset)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(real3dRecordVar),dimension(:),intent(in)  :: vars
    real,intent(in),optional                       :: recordCoord
    real,intent(in),dimension(2),optional          :: recordCoordBounds
    integer,intent(in),optional                    :: offset

    ! locals
    integer                                        :: status
    integer                                        :: ii
    integer                                        :: recdimlen
    integer                                        :: recvarid
    integer,dimension(4)                           :: startoffset

    ! write the values to the dataset
    do ii=1,size(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       if (present(offset)) recdimlen=recdimlen-1+offset
       select case (vars(ii)%recordDimIndex)
       case (0)
          call handle_err("Variable not defined along a record dimension!")
       case (1)
          startoffset=(/recdimlen,1,1,1/)
       case (2)
          startoffset=(/1,recdimlen,1,1/)
       case (3)
          startoffset=(/1,1,recdimlen,1/)
       case (4)
          startoffset=(/1,1,1,recdimlen/)
       end select
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data,startoffset)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       if (present(recordCoord)) then
          status = nf90_inq_varid(ncid,vars(ii)%dimnames(vars(ii)%recordDimIndex),recvarid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_var(ncid,recvarid,recordCoord,(/ recdimlen /))
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (present(recordCoordBounds)) then
             status = nf90_inq_varid(ncid,trim(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds",recvarid)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             status = nf90_put_var(ncid,recvarid,recordCoordBounds,(/ 1,recdimlen /))
             if (status /= NF90_NOERR) call handle_nc_err(status)       
          endif
       endif
    end do

  end subroutine appendReal3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendInteger1dVars
  !
  ! Append set of variables (1D, 'flat', real-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  subroutine appendInteger1dVars(ncid,vars,recordCoord,recordCoordBounds,offset)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer1dRecordVar),dimension(:),intent(in)  :: vars
    real,intent(in),optional                       :: recordCoord
    real,intent(in),dimension(2),optional          :: recordCoordBounds
    integer,intent(in),optional                    :: offset

    ! locals
    integer                                        :: status
    integer                                        :: ii
    integer                                        :: recdimlen
    integer                                        :: recvarid
    integer,dimension(2)                           :: startoffset

    ! write the values to the dataset
    do ii=1,size(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       if (present(offset)) recdimlen=recdimlen-1+offset
       select case (vars(ii)%recordDimIndex)
       case (0)
          call handle_err("Variable not defined along a record dimension!")
       case (1)
          startoffset=(/recdimlen,1/)
       case (2)
          startoffset=(/1,recdimlen/)
       end select
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data,startoffset)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       if (present(recordCoord)) then
          status = nf90_inq_varid(ncid,vars(ii)%dimnames(vars(ii)%recordDimIndex),recvarid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_var(ncid,recvarid,recordCoord,(/ recdimlen /))
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (present(recordCoordBounds)) then
             status = nf90_inq_varid(ncid,trim(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds",recvarid)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             status = nf90_put_var(ncid,recvarid,recordCoordBounds,(/ 1,recdimlen /))
             if (status /= NF90_NOERR) call handle_nc_err(status)       
          endif
       endif
    end do

  end subroutine appendInteger1dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendInteger2dVars
  !
  ! Append set of variables (2D, 'flat', real-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  subroutine appendInteger2dVars(ncid,vars,recordCoord,recordCoordBounds,offset)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer2dRecordVar),dimension(:),intent(in)  :: vars
    real,intent(in),optional                       :: recordCoord
    real,intent(in),dimension(2),optional          :: recordCoordBounds
    integer,intent(in),optional                    :: offset

    ! locals
    integer                                        :: status
    integer                                        :: ii
    integer                                        :: recdimlen
    integer                                        :: recvarid
    integer,dimension(3)                           :: startoffset

    ! write the values to the dataset
    do ii=1,size(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       if (present(offset)) recdimlen=recdimlen-1+offset
       select case (vars(ii)%recordDimIndex)
       case (0)
          call handle_err("Variable not defined along a record dimension!")
       case (1)
          startoffset=(/recdimlen,1,1/)
       case (2)
          startoffset=(/1,recdimlen,1/)
       case (3)
          startoffset=(/1,1,recdimlen/)
       end select
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data,startoffset)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       if (present(recordCoord)) then
          status = nf90_inq_varid(ncid,vars(ii)%dimnames(vars(ii)%recordDimIndex),recvarid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_var(ncid,recvarid,recordCoord,(/ recdimlen /))
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (present(recordCoordBounds)) then
             status = nf90_inq_varid(ncid,trim(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds",recvarid)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             status = nf90_put_var(ncid,recvarid,recordCoordBounds,(/ 1,recdimlen /))
             if (status /= NF90_NOERR) call handle_nc_err(status)       
          endif
       endif
    end do

  end subroutine appendInteger2dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: appendInteger3dVars
  !
  ! Append set of variables (3D, 'flat', real-valued)
  ! at the end (+offset) of the record dimension
  !----------------------------------------------------------------------

  subroutine appendInteger3dVars(ncid,vars,recordCoord,recordCoordBounds,offset)

    implicit none

    ! args
    integer,intent(in)                             :: ncid
    type(integer3dRecordVar),dimension(:),intent(in)  :: vars
    real,intent(in),optional                       :: recordCoord
    real,intent(in),dimension(2),optional          :: recordCoordBounds
    integer,intent(in),optional                    :: offset

    ! locals
    integer                                        :: status
    integer                                        :: ii
    integer                                        :: recdimlen
    integer                                        :: recvarid
    integer,dimension(4)                           :: startoffset

    ! write the values to the dataset
    do ii=1,size(vars)
       recdimlen = vars(ii)%dimLens(vars(ii)%recordDimIndex)+1
       if (present(offset)) recdimlen=recdimlen-1+offset
       select case (vars(ii)%recordDimIndex)
       case (0)
          call handle_err("Variable not defined along a record dimension!")
       case (1)
          startoffset=(/recdimlen,1,1,1/)
       case (2)
          startoffset=(/1,recdimlen,1,1/)
       case (3)
          startoffset=(/1,1,recdimlen,1/)
       case (4)
          startoffset=(/1,1,1,recdimlen/)
       end select
       status = nf90_put_var(ncid,vars(ii)%id,vars(ii)%data,startoffset)
       if (status /= NF90_NOERR) call handle_nc_err(status)
       if (present(recordCoord)) then
          status = nf90_inq_varid(ncid,vars(ii)%dimnames(vars(ii)%recordDimIndex),recvarid)
          if (status /= NF90_NOERR) call handle_nc_err(status)
          status = nf90_put_var(ncid,recvarid,recordCoord,(/ recdimlen /))
          if (status /= NF90_NOERR) call handle_nc_err(status)
          if (present(recordCoordBounds)) then
             status = nf90_inq_varid(ncid,trim(vars(ii)%dimnames(vars(ii)%recordDimIndex))//"_bnds",recvarid)
             if (status /= NF90_NOERR) call handle_nc_err(status)
             status = nf90_put_var(ncid,recvarid,recordCoordBounds,(/ 1,recdimlen /))
             if (status /= NF90_NOERR) call handle_nc_err(status)       
          endif
       endif
    end do

  end subroutine appendInteger3dVars

  !----------------------------------------------------------------------
  !
  ! Subroutine: openNetCDFRead
  !
  ! Opens an existing dataset for reading & returns dataset id
  !
  !----------------------------------------------------------------------

  subroutine openNetCDFRead(filename,ncid)

    implicit none

    ! args
    character(len=*),intent(in) :: filename
    integer,intent(out)           :: ncid
    ! locals
    integer                       :: status

    ! create the dataset
    ! NF90_NOWRITE: open dataset for reading only
    ! NF90_WRITE (write only)
    ! NF90_SHARE (read and write) also available
    status = nf90_open(trim(filename),NF90_NOWRITE,ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

  end subroutine openNetCDFRead

  !----------------------------------------------------------------------
  !
  ! Subroutine: openNetCDFWrite
  !
  ! Opens an existing dataset for writing & returns dataset id
  !
  !----------------------------------------------------------------------

  subroutine openNetCDFWrite(filename,ncid)

    implicit none

    ! args
    character(len=*),intent(in) :: filename
    integer,intent(out)           :: ncid
    ! locals
    integer                       :: status

    ! create the dataset
    ! NF90_NOWRITE: open dataset for reading only
    ! NF90_WRITE (write only)
    ! NF90_SHARE (read and write) also available
    status = nf90_open(trim(filename),NF90_WRITE,ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

  end subroutine openNetCDFWrite

  !----------------------------------------------------------------------
  !
  ! Subroutine: reDef
  !
  ! Reenter 'definition mode'
  !
  !----------------------------------------------------------------------

  subroutine reDef(ncid)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    

    ! locals
    integer                                        :: status
    
    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

  end subroutine reDef

  !----------------------------------------------------------------------
  !
  ! Subroutine: endDef
  !
  ! End of 'definition mode', enter 'data mode' for an open dataset
  !
  !----------------------------------------------------------------------

  subroutine endDef(ncid)

    implicit none

    ! args
    integer,intent(in)                             :: ncid    

    ! locals
    integer                                        :: status
    
    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

  end subroutine endDef

  !----------------------------------------------------------------------
  !
  ! Subroutine: closeNetCDF
  !
  ! Close an open dataset with given id
  !
  !----------------------------------------------------------------------

  subroutine closeNetCDF(ncid)

    implicit none

    ! args
    integer,intent(in)            :: ncid
    ! locals
    integer                       :: status

    ! close the dataset
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) call handle_nc_err(status)

  end subroutine closeNetCDF

  !----------------------------------------------------------------------
  !
  ! Subroutine: handle_err
  !
  ! simple error handler
  !
  !----------------------------------------------------------------------

  subroutine handle_err(message)
    
    implicit none

    character(len=*),intent(in) :: message

    write (*,*) trim(message)
    stop "Stopped"

  end subroutine handle_err

  !----------------------------------------------------------------------
  !
  ! Subroutine: handle_nc_err
  !
  ! simple error handler
  !
  !----------------------------------------------------------------------

  subroutine handle_nc_err(status)
    
    implicit none
    
    integer,intent(in) :: status

    if(status /= nf90_noerr) then
       write (*,*) trim(nf90_strerror(status))
       stop "Stopped"
    end if
  end subroutine handle_nc_err

end module local_netcdf
