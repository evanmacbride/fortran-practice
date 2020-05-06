MODULE trend_line
IMPLICIT NONE
INTEGER, SAVE :: n
REAL, DIMENSION(:), ALLOCATABLE :: x_sum
REAL, DIMENSION(:), ALLOCATABLE :: xy_sum
!LOGICAL :: initialized = .FALSE.
CONTAINS
  SUBROUTINE cumul_trend_sums(x, y, k)
  IMPLICIT NONE
  REAL, INTENT(IN) :: x, y
  INTEGER, INTENT(IN) :: k
  INTEGER :: i

  IF (.not. ALLOCATED(x_sum) .and. .not. ALLOCATED(xy_sum)) THEN
    ALLOCATE(x_sum(0:2*n),xy_sum(0:n))
  END IF

  IF (k == 1) THEN
    x_sum = 0.
    xy_sum = 0.
  END IF

  DO i = 0, 2 * n
    x_sum(i) = x_sum(i) + x**i
  END DO
  DO i = 0, n
    xy_sum(i) = xy_sum(i) + (x**i) * y
  END DO
  END SUBROUTINE cumul_trend_sums

  SUBROUTINE build_matrix(a, b)
  REAL, DIMENSION(n,n), INTENT(OUT) :: a
  REAL, DIMENSION(n), INTENT(OUT) :: b
  INTEGER :: i, j, k

  DO i = 1, n + 1
    DO j = 1, n + 1
      k = i + j - 2
      a(i, j) = x_sum(k)
    END DO
  END DO
  b = xy_sum
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

PROGRAM higher_ord
USE trend_line
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 1000
REAL, DIMENSION(3, 3) :: a
REAL, DIMENSION(3) :: b
REAL :: x, y
INTEGER :: error
CHARACTER(len=20) :: file_name
INTEGER :: i, j, istat
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
      READ(1, *) x, y
      CALL cumul_trend_sums(x, y, i)
    END DO
    CALL build_matrix(a, b)
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

END PROGRAM higher_ord
