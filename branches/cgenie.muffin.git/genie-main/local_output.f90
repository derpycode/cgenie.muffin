 
!----------------------------------------------------------------------
!>
!> Module: local_output
!>
!> Output routines: The routines are generally stateless (access via
!> filename of the output file. These routines use the NetCDF routines
!> provided by the module "local_netcdf".
!> 
!----------------------------------------------------------------------
module local_output

  use genie_util, only: message,die

  use local_netcdf

  PRIVATE

  PUBLIC :: resetOutput,defineDimension,defineRecordDimension
  PUBLIC :: writeReal2dVariable,writeInteger2dVariable,writeReal2dRecordVariable
  PUBLIC :: writeReal3dVariable,writeReal3dRecordVariable

contains

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: openInput
  !>
  !> opens output file, returns handle to output file
  !> 
  !----------------------------------------------------------------------
  subroutine openInput(filename,ID)

    character(len=*),intent(in) :: filename

    integer :: ID

    call message("Opening input file in read-only mode!",3)
    call openNetCDFRead(filename,ID)

  end subroutine openInput

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: openOutput
  !>
  !> opens output file, returns handle to output file
  !> 
  !----------------------------------------------------------------------
  subroutine openOutput(filename,ID,readonly,reset)

    character(len=*),intent(in) :: filename

    integer :: ID

    logical,optional,intent(in) :: readonly
    logical,optional,intent(in) :: reset

    logical :: exists

    if (present(readonly).and.(readonly)) then
       call message("Opening existing output file in read-only mode!",3)
       call openNetCDFRead(filename,ID)
    else
       inquire(file=filename,exist=exists)
       if (.not.exists) then
          call message("Creating new output file: "//filename,3)
          call createNetCDF(filename,ID)
          call endDef(ID)
       else
          if (present(reset).and.(reset)) then
             call message("Resetting existing output file: "//filename,3)
             call createNetCDF(filename,ID,.true.)
          else
             call message("Opening existing output file: "//filename,3)
             call openNetCDFWrite(filename,ID)
          endif
       endif
    endif

  end subroutine openOutput

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: closeInOutput
  !>
  !> closes input/output file
  !> 
  !----------------------------------------------------------------------
  subroutine closeInOutput(ID)

    integer :: ID

    call closeNetCDF(ID)

  end subroutine closeInOutput

  !----------------------------------------------------------------------
  !>
  !> Subroutine: resetOutput
  !>
  !> resets (or creates) output file
  !> 
  !----------------------------------------------------------------------
  subroutine resetOutput(filename)

    character(len=*),intent(in) :: filename

    integer ID

    call openOutput(filename,ID,.false.,.true.)
    call closeInOutput(ID)

  end subroutine resetOutput

  !----------------------------------------------------------------------
  !>
  !> Subroutine: defineDimension
  !>
  !> defines Dimension
  !> 
  !----------------------------------------------------------------------

  subroutine defineDimension(filename,dimName,dimValues,dimBoundariesLower,dimBoundariesUpper,dimLongname,dimStandardname,dimUnits)

    character(len=*),intent(in) :: filename,dimName

    real,dimension(:),intent(in) :: dimValues,dimBoundariesLower,dimBoundariesUpper

    character(len=*),intent(in),optional :: dimLongname,dimStandardname,dimUnits

    integer :: ID,status,dimlen

    type(realDimInfo),pointer,dimension(:) :: dimNetCDF

    dimLen = size(dimValues)

    call openOutput(filename,ID)

    allocate(dimNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    allocate(dimNetCDF(1)%coords(1:dimlen),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    allocate(dimNetCDF(1)%boundsLower(dimlen),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    allocate(dimNetCDF(1)%boundsUpper(dimlen),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif

    dimNetCDF(1)%boundsDefine = .true.
    dimNetCDF(1)%name = dimName
    dimNetCDF(1)%len = dimLen
    dimNetCDF(1)%basicAtts%long_name = dimLongname
    dimNetCDF(1)%basicAtts%standard_name = dimStandardname
    dimNetCDF(1)%basicAtts%units = dimUnits
    dimNetCDF(1)%coords(:) = dimValues(:)
    dimNetCDF(1)%boundsLower(:) = dimBoundariesLower(:)
    dimNetCDF(1)%boundsUpper(:) = dimBoundariesUpper(:)

    print *, filename
    call defineDims(ID,dimNetCDF)
    print *, filename

    call closeInOutput(ID)

    deallocate(dimNetCDF(1)%coords)
    deallocate(dimNetCDF(1)%boundsLower)
    deallocate(dimNetCDF(1)%boundsUpper)
    deallocate(dimNetCDF)

  end subroutine defineDimension

  !----------------------------------------------------------------------
  !>
  !> Subroutine: defineRecordDimension
  !>
  !> defines record Dimension
  !> 
  !----------------------------------------------------------------------

  subroutine defineRecordDimension(filename,dimName,dimLongname,dimStandardname,dimUnits)

    character(len=*),intent(in) :: filename,dimName

    character(len=*),intent(in),optional :: dimLongname,dimStandardname,dimUnits

    integer :: ID,status

    type(realRecordDimInfo),pointer,dimension(:) :: dimNetCDF

    call openOutput(filename,ID)

    allocate(dimNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif

    dimNetCDF(1)%coordsDefine = .true.
    dimNetCDF(1)%boundsDefine = .true.
    dimNetCDF(1)%name = dimName
    dimNetCDF(1)%basicAtts%long_name = dimLongname
    dimNetCDF(1)%basicAtts%standard_name = dimStandardname
    dimNetCDF(1)%basicAtts%units = dimUnits

    call defineDims(ID,dimNetCDF)

    call closeInOutput(ID)

    deallocate(dimNetCDF)

  end subroutine defineRecordDimension

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal2dVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal2dVariable(filename,varName,varDimName1,varDimName2,varValues,varLongname,varStandardname,varUnits,varMissingValue)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in) :: varDimName1,varDimName2
    character(len=nf90_max_name),dimension(2) :: varDimNames

    real,dimension(:,:),intent(in) :: varValues

    character(len=*),intent(in),optional :: varLongname,varStandardname,varUnits

    real,intent(in),optional :: varMissingValue

    type(real2dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(2) :: arraySize

    call openOutput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:) = varValues(:,:)

    call lookupVars(ID,varNetCDF)

    if (varNetCDF(1)%id < 0) then

       varDimNames(1)=varDimName1
       varDimNames(2)=varDimName2
       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    call writeVars(ID,varNetCDF)

    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal2dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeInteger2dVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeInteger2dVariable(filename,varName,varDimName1,varDimName2,varValues,varLongname,varStandardname,varUnits,varMissingValue)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in) :: varDimName1,varDimName2

    integer,dimension(:,:),intent(in) :: varValues

    character(len=*),intent(in),optional :: varLongname,varStandardname,varUnits

    integer,intent(in),optional :: varMissingValue

    type(integer2dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(2) :: arraySize

    call openOutput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:) = varValues(:,:)

    call lookupInteger2dVars(ID,varNetCDF)

    if (varNetCDF(1)%id < 0) then

       call dimInteger2dVars(ID,varNetCDF,(/varDimName1,varDimName2/))

       call defineInteger2dVars(ID,varNetCDF)

    endif

    call writeInteger2dVars(ID,varNetCDF)

    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeInteger2dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal2dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal2dRecordVariable(filename,varName,varDimName1,varDimName2,varDimName3,varValues,recordCoord,recordCoordBounds,varLongname,varStandardname,varUnits,varMissingValue,offset)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in) :: varDimName1,varDimName2,varDimName3
    character(len=nf90_max_name),dimension(3) :: varDimNames

    real,dimension(:,:),intent(in) :: varValues

    real,intent(in) :: recordCoord
    real,intent(in),dimension(2) :: recordCoordBounds

    character(len=*),intent(in),optional :: varLongname,varStandardname,varUnits

    real,intent(in),optional :: varMissingValue

    integer,intent(in),optional                    :: offset

    type(real2dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(2) :: arraySize

    call openOutput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:) = varValues(:,:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       varDimNames(1)=varDimName1
       varDimNames(2)=varDimName2
       varDimNames(3)=varDimName3
       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    if (present(offset)) then
       call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds,offset=offset)
    else
       call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds)
    endif
    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal2dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal3dVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal3dVariable(filename,varName,varDimName1,varDimName2,varDimName3,varValues,varLongname,varStandardname,varUnits,varMissingValue)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in) :: varDimName1,varDimName2,varDimName3
    character(len=nf90_max_name),dimension(3) :: varDimNames

    real,dimension(:,:,:),intent(in) :: varValues

    character(len=*),intent(in),optional :: varLongname,varStandardname,varUnits

    real,intent(in),optional :: varMissingValue

    type(real3dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(3) :: arraySize

    call openOutput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2),arraySize(3)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:,:) = varValues(:,:,:)

    call lookupVars(ID,varNetCDF)

    if (varNetCDF(1)%id < 0) then

       varDimNames(1)=varDimName1
       varDimNames(2)=varDimName2
       varDimNames(3)=varDimName3
       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    call writeVars(ID,varNetCDF)

    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal3dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal3dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal3dRecordVariable(filename,varName,varDimName1,varDimName2,varDimName3,varDimName4,varValues,recordCoord,recordCoordBounds,varLongname,varStandardname,varUnits,varMissingValue,offset)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in) :: varDimName1,varDimName2,varDimName3,varDimName4
    character(len=nf90_max_name),dimension(4) :: varDimNames

    real,dimension(:,:,:),intent(in) :: varValues

    real,intent(in) :: recordCoord
    real,intent(in),dimension(2) :: recordCoordBounds

    character(len=*),intent(in),optional :: varLongname,varStandardname,varUnits

    real,intent(in),optional :: varMissingValue

    integer,intent(in),optional                    :: offset

    type(real3dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(3) :: arraySize

    call openOutput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2),arraySize(3)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = varName
    varNetCDF(1)%basicAtts%long_name = varLongname
    varNetCDF(1)%basicAtts%standard_name = varStandardname
    varNetCDF(1)%basicAtts%units = varUnits
    varNetCDF(1)%basicAtts%missing_value = varMissingValue
    varNetCDF(1)%data(:,:,:) = varValues(:,:,:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       varDimNames(1)=varDimName1
       varDimNames(2)=varDimName2
       varDimNames(3)=varDimName3
       varDimNames(4)=varDimName4
       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    if (present(offset)) then
       call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds,offset=offset)
    else
       call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds)
    endif
    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal3dRecordVariable

end module local_output
