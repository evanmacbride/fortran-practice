MODULE trend_line
IMPLICIT NONE
REAL :: x_sum = 0., x2_sum = 0., x3_sum = 0., x4_sum = 0., y_sum = 0., &
  xy_sum = 0., x2y_sum = 0.
CONTAINS
  SUBROUTINE cumul_trend_sums(x, y)
  IMPLICIT NONE
  REAL, INTENT(IN) :: x, y
  x_sum   = x_sum + x
  x2_sum  = x2_sum + x**2
  x3_sum  = x3_sum + x**3
  x4_sum  = x4_sum + x**4
  y_sum   = y_sum + y
  xy_sum  = xy_sum + x * y
  x2y_sum = x2y_sum + y * x**2
  END SUBROUTINE cumul_trend_sums

  SUBROUTINE build_matrix(n, a, b)
  REAL, DIMENSION(3,3), INTENT(OUT) :: a
  REAL, DIMENSION(3), INTENT(OUT) :: b
  INTEGER, INTENT(IN) :: n
  a(1,1) = REAL(n)
  a(1,2) = x_sum
  a(1,3) = x2_sum
  a(2,1) = x_sum
  a(2,2) = x2_sum
  a(2,3) = x3_sum
  a(3,1) = x2_sum
  a(3,2) = x3_sum
  a(3,3) = x4_sum

  b(1) = y_sum
  b(2) = xy_sum
  b(3) = x2y_sum
  END SUBROUTINE build_matrix

END MODULE trend_line

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

divide: DO irow = 1, n
  b(irow)       = b(irow) / a(irow, irow)
  a(irow, irow) = 1
END DO divide

! Set error flag to 0 and return
error = 0
END SUBROUTINE simul

PROGRAM least_squares_2nd_ord
USE trend_line
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 1000
REAL, DIMENSION(3, 3) :: a
REAL, DIMENSION(3) :: b
REAL :: x, y
INTEGER :: error
CHARACTER(len=20) :: file_name
INTEGER :: i, j, n, istat
CHARACTER(len=80) :: msg

WRITE(*, "('Enter the file name containing the data:')")
READ(*, '(A20)') file_name

! Open input data file. Status is OLD because the input data must
! already exist.
OPEN (UNIT=1, FILE=file_name, STATUS='OLD', ACTION='READ', &
      IOSTAT=istat, IOMSG=msg)

! Was the OPEN successful?
fileopen: IF(istat == 0) THEN
  ! The file was opened successfully, so read the number of
  ! measurements in the file.
  READ(1, *) n
  ! If the number of equ is <= MAX_SIZE, read them in
  ! and process them.
  size_ok: IF (n <= MAX_SIZE) THEN
    DO i = 1, n
      x = 0.
      y = 0.
      READ(1, *) x, y
      CALL cumul_trend_sums(x, y)
    END DO
    CALL build_matrix(n, a, b)
    ! Display coefficients.
    WRITE(*, "(/,'Coefficients before call:')")
    DO i = 1, 3
      WRITE(*, "(1X,7F11.4)") (a(i, j), j = 1, 3), b(i)
    END DO

    ! Solve equations
    CALL simul(a, b, 3, 3, error)

    ! Check for error.
    error_check: IF (error /= 0) THEN
      WRITE(*, 1010)
      1010 FORMAT(/'Zero pivot encountered!', &
                  //'There is no unique solution to this system.')
    ELSE error_check
      ! No errors. Display coefficients.
      WRITE(*, "(/,'Coefficients after call:')")
      DO i = 1, 3
        WRITE(*, "(1X,7F11.4)") (a(i, j), j = 1, 3), b(i)
      END DO
      ! Write the final answer.
      WRITE(*, "(/,'The solutions are:')")
      DO i = 1, 3
        WRITE(*,"(2X,'X(',I2,') = ',F16.6)") i, b(i)
      END DO
    END IF error_check
  END IF size_ok
ELSE fileopen
  ! Else file open failed. Tell user.
  WRITE(*, 1020) msg
  1020 FORMAT('File open failed: ', A)
END IF fileopen

END PROGRAM least_squares_2nd_ord
