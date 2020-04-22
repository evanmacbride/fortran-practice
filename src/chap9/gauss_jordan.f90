SUBROUTINE simul(a, b, ndim, n, error)
IMPLICIT NONE
INTEGER, INTENT(IN) :: ndim
REAL, INTENT(INOUT), DIMENSION(ndim, ndim) :: a
REAL, INTENT(INOUT), DIMENSION(ndim) :: b
INTEGER, INTENT(IN) :: n
INTEGER, INTENT(OUT) :: error
REAL, PARAMETER :: EPSILON = 1.0E-6
REAL :: factor, temp
INTEGER :: irow, ipeak, jrow, kcol

! Process n times to get all equations...
mainloop: DO irow = 1, n
  ! Find peak pivot for column irow in rows irow to n
  ipeak = irow
  max_pivot: DO jrow = irow + 1, n
    IF (ABS(a(jrow,irow)) > ABS(a(ipeak,irow))) THEN
      ipeak = jrow
    END IF
  END DO max_pivot

  ! Check for singular equations.
  singular: IF(ABS(a(ipeak,irow)) < EPSILON) THEN
    error = 1
    RETURN
  END IF singular

  ! Otherwise, if ipeak /= irow, swap equations irow & ipeak
  swap_eqn: IF (ipeak /= irow) THEN
    DO kcol = 1, n
      temp           = a(ipeak, kcol)
      a(ipeak, kcol) = a(irow, kcol)
      a(irow, kcol)  = temp
    END DO
    temp     = b(ipeak)
    b(ipeak) = b(irow)
    b(irow)  = temp
  END IF swap_eqn

  ! Multiply equation irow by -a(jrow, irow)/a(irow, irow)
  ! and add it to eqn jrow (for all eqns except irow itself).
  eliminate: DO jrow = 1, n
    IF (jrow /= irow) THEN
      factor = -a(jrow, irow) / a(irow, irow)
      DO kcol = 1, n
        a(jrow, kcol) = a(irow, kcol) * factor + a(jrow, kcol)
      END DO
      b(jrow) = b(irow) * factor + b(jrow)
    END IF
  END DO eliminate
END DO mainloop

! End of mainloop over all equations. All off-diagonal
! terms are now zero. To get the final answer, we must
! divide each equation by the coefficient of its on-diagonal
! term.
divide: DO irow = 1, n
  b(irow)       = b(irow) / a(irow, irow)
  a(irow, irow) = 1
END DO divide

! Set error flag to 0 and return
error = 0
END SUBROUTINE simul

PROGRAM gauss_jordan
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 10
REAL, DIMENSION(MAX_SIZE, MAX_SIZE) :: a
REAL, DIMENSION(MAX_SIZE) :: b
INTEGER :: error
CHARACTER(len=20) :: file_name
INTEGER :: i, j, n, istat
CHARACTER(len=80) :: msg

WRITE(*, "('Enter the file name containing the eqns:')")
READ(*, '(A20)') file_name

! Open input data file. Status is OLD because the input data must
! already exist.
OPEN (UNIT=1, FILE=file_name, STATUS='OLD', ACTION='READ', &
      IOSTAT=istat, IOMSG=msg)

! Was the OPEN successful?
fileopen: IF(istat == 0) THEN
  ! The file was opened successfully, so read the number of
  ! equations in the system.
  READ(1, *) n

  ! If the number of equations is <= MAX_SIZE, read them in
  ! and process them.
  size_ok: IF (n <= MAX_SIZE) THEN
    DO i = 1, n
      READ(1, *) (a(i, j), j = 1, n), b(i)
    END DO

    ! Display coefficients.
    WRITE(*, "(/,'Coefficients before call:')")
    DO i = 1, n
      WRITE(*, "(1X,7F11.4)") (a(i, j), j = 1, n), b(i)
    END DO

    ! Solve equations
    CALL simul(a, b, MAX_SIZE, n, error)

    ! Check for error.
    error_check: IF (error /= 0) THEN
      WRITE(*, 1010)
      1010 FORMAT(/'Zero pivot encountered!', &
                  //'There is no unique solution to this system.')
    ELSE error_check
      ! No errors. Display coefficients.
      WRITE(*, "(/,'Coefficients after call:')")
      DO i = 1, n
        WRITE(*, "(1X,7F11.4)") (a(i, j), j = 1, n), b(i)
      END DO
      ! Write the final answer.
      WRITE(*, "(/,'The solutions are:')")
      DO i = 1, n
        WRITE(*,"(2X,'X(',I2,') = ',F16.6)") i, b(i)
      END DO
    END IF error_check
  END IF size_ok
ELSE fileopen
  ! Else file open failed. Tell user.
  WRITE(*, 1020) msg
  1020 FORMAT('File open failed: ', A)
END IF fileopen
END PROGRAM gauss_jordan
