
! Compare any variables common to two NetCDF files.
!
! Optional tolerances:-
!
!   Absolute: if the difference between values A and B is less than
!             the absolute tolerance, then it is small enough to
!             ignore.
!
!   Relative: if the difference between values A and B is less than or
!             equal to the given number of Ulps, then they are close
!             enough.

PROGRAM nccompare
  USE netcdf
  IMPLICIT NONE

  CHARACTER(LEN=1024) :: fname1, fname2
  INTEGER :: max_ulps
  REAL :: min_abs
  LOGICAL :: verbose = .FALSE., silent = .FALSE., zerocheck = .FALSE.
  INTEGER :: nc1, nc2
  INTEGER :: nvars1, nvars2
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: varnames1, varnames2

  INTEGER :: ivar1, ivar2
  LOGICAL :: err = .FALSE.


  ! Parse command line.
  max_ulps = 0
  min_abs = TINY(0.0)
  CALL parse_options()

  ! Verify the threshold options, if supplied
  IF (max_ulps < 0) THEN
     PRINT *, 'Error: Cannot specify a -ve relative tolerance'
     CALL EXIT(1)
  END IF
  IF (min_abs < 0.0) THEN
     PRINT *, 'Error: Cannot specify a -ve absolute tolerance'
     CALL EXIT(1)
  END IF

  ! Load the two files and their variable names.
  CALL open_nc(fname1, nc1, nvars1, varnames1)
  CALL open_nc(fname2, nc2, nvars2, varnames2)

  ! Determine which variables are in both files and compare them.
  DO ivar1 = 1, nvars1
     DO ivar2 = 1, nvars2
        IF (varnames1(ivar1) == varnames2(ivar2)) &
             & err = err .OR. .NOT. compare_variables(ivar1, ivar2)
     END DO
  END DO

  IF (err) THEN
    IF (.NOT. silent) PRINT 100, TRIM(fname1), TRIM(fname2)
100 FORMAT ('Files ', A, ' and ', A, ' differ')
    CALL EXIT(1)
 ELSE
    IF (.NOT. silent) PRINT 110, TRIM(fname1), TRIM(fname2)
110 FORMAT ('Files ', A, ' and ', A, ' match')
 END IF

CONTAINS


  SUBROUTINE parse_options()
    IMPLICIT NONE

    INTEGER :: nopt, iopt
    LOGICAL :: have_fname1, have_fname2
    CHARACTER(LEN=1024) :: opt

    have_fname1 = .FALSE. ; have_fname2 = .FALSE.
    nopt = COMMAND_ARGUMENT_COUNT()

    iopt = 1
    DO WHILE (iopt <= nopt)
       CALL GET_COMMAND_ARGUMENT(iopt, opt)
       SELECT CASE (TRIM(opt))
       CASE ('-v')
          verbose = .TRUE.
       CASE ('-s')
          silent = .TRUE.
       CASE ('-z')
          zerocheck = .TRUE.
       CASE ('-r')
          iopt = iopt + 1
          IF (iopt > nopt) CALL usage()
          CALL GET_COMMAND_ARGUMENT(iopt, opt)
          READ (opt, *) max_ulps
       CASE ('-a')
          iopt = iopt + 1
          IF (iopt > nopt) CALL usage()
          CALL GET_COMMAND_ARGUMENT(iopt, opt)
          READ (opt, *) min_abs
       CASE DEFAULT
          IF (.NOT. have_fname1) THEN
             fname1 = opt
             have_fname1 = .TRUE.
          ELSE IF (.NOT. have_fname2) THEN
             fname2 = opt
             have_fname2 = .TRUE.
          ELSE
             CALL usage()
          END IF
       END SELECT
       iopt = iopt + 1
    END DO

    IF (silent) VERBOSE = .FALSE.
  END SUBROUTINE parse_options


  SUBROUTINE open_nc(fname, nc, nvars, varnames)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(OUT) :: nc
    INTEGER, INTENT(OUT) :: nvars
    CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: varnames

    INTEGER :: ivar

    CALL nc_check(NF90_OPEN(fname, NF90_NOWRITE, nc), &
         & 'could not open file "' // TRIM(fname) // '"')
    CALL nc_check(NF90_INQUIRE(nc, NVARIABLES=nvars))

    IF (zerocheck .AND. nvars == 0) THEN
       PRINT *, 'File "' // TRIM(fname) // '" contains no variables'
       CALL EXIT(1)
    END IF

    ALLOCATE(varnames(nvars))

    DO ivar = 1, nvars
       CALL nc_check(NF90_INQUIRE_VARIABLE(nc, ivar, NAME=varnames(ivar)))
    END DO
  END SUBROUTINE open_nc


  FUNCTION compare_variables(ivar1, ivar2)
    IMPLICIT NONE
    LOGICAL :: compare_variables
    INTEGER, INTENT(IN) :: ivar1, ivar2

    compare_variables = .TRUE.
    IF (verbose) PRINT 100, TRIM(varnames1(ivar1))
100 FORMAT ('Comparing variable ', A)
    SELECT CASE (TRIM(do_comparison(ivar1, ivar2)))
    CASE ('ERRDIMNUM')
       compare_variables = .FALSE.
       CALL compare_error(ivar1, 'numbers of dimensions')
    CASE ('ERRTYPE')
       compare_variables = .FALSE.
       CALL compare_error(ivar1, 'types')
    CASE ('ERRSIZE')
       compare_variables = .FALSE.
       CALL compare_error(ivar1, 'sizes')
    CASE ('ERRVALS')
       compare_variables = .FALSE.
       CALL compare_error(ivar1, 'values')
    END SELECT
  END FUNCTION compare_variables


  FUNCTION do_comparison(ivar1, ivar2)
    IMPLICIT NONE
    CHARACTER(LEN=10) :: do_comparison
    INTEGER, INTENT(IN) :: ivar1, ivar2

    INTEGER :: ndims1, ndims2, xtype1, xtype2
    INTEGER, DIMENSION(:), ALLOCATABLE :: dims1, dims2
    INTEGER, DIMENSION(:), ALLOCATABLE :: dimlens1, dimlens2
    INTEGER, DIMENSION(:), ALLOCATABLE :: starts
    INTEGER :: nvals1, nvals2
    INTEGER :: idim
    REAL, DIMENSION(:), ALLOCATABLE :: vals1, vals2
    REAL :: max_absdiff
    INTEGER :: max_reldiff

    do_comparison = 'OK'

    ! First check types and dimensions
    CALL nc_check(NF90_INQUIRE_VARIABLE(nc1, ivar1, NDIMS=ndims1, XTYPE=xtype1))
    CALL nc_check(NF90_INQUIRE_VARIABLE(nc2, ivar2, NDIMS=ndims2, XTYPE=xtype2))
    IF (ndims1 /= ndims2) THEN
       do_comparison = 'ERRDIMNUM'
       RETURN
    END IF
    IF (xtype1 /= xtype2) THEN
       do_comparison = 'ERRTYPE'
       RETURN
    END IF

    ! Get dimension information.
    ALLOCATE(dims1(ndims1))
    ALLOCATE(dims2(ndims2))
    CALL nc_check(NF90_INQUIRE_VARIABLE(nc1, ivar1, DIMIDS=dims1))
    CALL nc_check(NF90_INQUIRE_VARIABLE(nc2, ivar2, DIMIDS=dims2))

    ! Calculate total variable sizes.
    nvals1 = 1 ; nvals2 = 1
    ALLOCATE(dimlens1(ndims1))
    ALLOCATE(dimlens2(ndims2))
    DO idim = 1, ndims1
       CALL nc_check(NF90_INQUIRE_DIMENSION(nc1, dims1(idim), &
            & LEN=dimlens1(idim)))
       CALL nc_check(NF90_INQUIRE_DIMENSION(nc2, dims2(idim), &
            & LEN=dimlens2(idim)))
    END DO
    DEALLOCATE(dims1)
    DEALLOCATE(dims2)
    nvals1 = PRODUCT(dimlens1)
    nvals2 = PRODUCT(dimlens2)
    IF (nvals1 /= nvals2) THEN
       do_comparison = 'ERRSIZE'
       DEALLOCATE(dimlens1) ; DEALLOCATE(dimlens2)
       RETURN
    END IF

    ! Get all the values.
    ALLOCATE(starts(ndims1))
    starts(:) = 1
    ALLOCATE(vals1(nvals1))
    ALLOCATE(vals2(nvals2))
    CALL nc_check(NF90_GET_VAR(nc1, ivar1, vals1, START=starts, COUNT=dimlens1))
    CALL nc_check(NF90_GET_VAR(nc2, ivar2, vals2, START=starts, COUNT=dimlens2))
    DEALLOCATE(starts)
    DEALLOCATE(dimlens1)
    DEALLOCATE(dimlens2)

    ! Calculate absolute and relative differences.
    max_absdiff = MAXVAL(ABS(vals1 - vals2))
    max_reldiff = MAXVAL(float_compare(vals1, vals2))
    DEALLOCATE(vals1)
    DEALLOCATE(vals2)

    ! Check.
    IF (min_abs > TINY(0.0) .AND. max_absdiff > min_abs) THEN
       IF (max_ulps > 0 .AND. max_reldiff < max_ulps) THEN
          PRINT 100, max_absdiff, max_reldiff, max_ulps
100       FORMAT ('Max. abs. diff. = ', G13.6, ' but max. rel. diff. = ', &
               & I0, ' < ', I0)
       ELSE
          do_comparison = 'ERRVALS'
       END IF
    END IF
  END FUNCTION do_comparison


  IMPURE ELEMENTAL FUNCTION float_compare(x, y)
    IMPLICIT NONE
    INTEGER :: float_compare
    REAL, INTENT(IN) :: x, y

    REAL(KIND=4) :: x4, y4
        
    x4 = REAL(x, KIND=4)
    y4 = REAL(y, KIND=4)
    float_compare = ABS((TRANSFER(real(z'80000000', KIND=4), 1) - TRANSFER(x4, 1)) - &
                      & (TRANSFER(real(z'80000000', KIND=4), 1) - TRANSFER(y4, 1)))
  END FUNCTION float_compare


  SUBROUTINE usage()
    IMPLICIT NONE

    PRINT *, 'Usage: nccompare [options] <NCfileA> <NCfileB>'
    PRINT *, 'Options:'
    PRINT *, '  -v        Verbose mode'
    PRINT *, '  -s        Silent mode'
    PRINT *, '  -z        Fail for empty files'
    PRINT *, '  -r <val>  Relative tolerance threshold in ulps'
    PRINT *, '  -a <val>  Absolute tolerance threshold--ignore smaller numbers'
    CALL EXIT(1)
  END SUBROUTINE usage


  SUBROUTINE nc_check(status, msg)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: status
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: msg

    IF (status /= NF90_NOERR) THEN
       IF (PRESENT(msg)) PRINT *, 'Error:', msg
       PRINT *, 'NetCDF status: ', NF90_STRERROR(status)
       CALL EXIT(1)
    END IF
  END SUBROUTINE nc_check

  SUBROUTINE compare_error(ivar, msg)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ivar
    CHARACTER(LEN=*), INTENT(IN) :: msg

    IF (verbose) PRINT 100, TRIM(msg), TRIM(varnames1(ivar))
100 FORMAT ('**ERROR: Differing ', A, ' in variable ', A)
  END SUBROUTINE compare_error

END PROGRAM nccompare
