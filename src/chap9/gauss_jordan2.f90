SUBROUTINE simul2 ( a, b, soln, ndim, n, error )
IMPLICIT NONE
! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: ndim
! Dimension of arrays a and b
REAL, INTENT(IN), DIMENSION(ndim,ndim) :: a
! Array of coefficients (N x N).
! This array is of size ndim x
! ndim, but only N x N of the
! coefficients are being used.
REAL, INTENT(IN), DIMENSION(ndim) :: b
! Input: Right-hand side of eqns.
REAL, INTENT(OUT), DIMENSION(ndim) :: soln
! Output: Solution vector.
INTEGER, INTENT(IN) :: n
! Number of equations to solve.
INTEGER, INTENT(OUT) :: error
REAL, PARAMETER :: EPSILON = 1.0E-6 ! A "small" number for comparison
! when determining singular eqns
! Data dictionary: declare local variable types & definitions
REAL, DIMENSION(n,n) :: a1
! Copy of "a" which will be
! destroyed during the solution
REAL :: factor
! Factor to multiply eqn irow by
! before adding to eqn jrow
INTEGER :: irow
! Number of the equation currently
! being processed
INTEGER :: ipeak
! Pointer to equation containing
! maximum pivot value
INTEGER :: jrow
! Number of the equation compared
! to the current equation
REAL :: temp
! Scratch value

REAL, DIMENSION(n) :: temp1
! Scratch array
! Make copies of arrays "a" and "b" for local use
a1 = a(1:n,1:n)
soln = b(1:n)
! Process N times to get all equations...
mainloop: DO irow = 1, n
! Find peak pivot for column irow in rows irow to N
ipeak = irow
max_pivot: DO jrow = irow+1, n
IF (ABS(a1(jrow,irow)) > ABS(a1(ipeak,irow))) THEN
ipeak = jrow
END IF
END DO max_pivot
! Check for singular equations.
singular: IF ( ABS(a1(ipeak,irow)) < EPSILON ) THEN
error = 1
RETURN
END IF singular
! Otherwise, if ipeak /= irow, swap equations irow & ipeak
swap_eqn: IF ( ipeak /= irow ) THEN
temp1 = a1(ipeak,1:n)
a1(ipeak,1:n) = a1(irow,1:n)
! Swap rows in a
a1(irow,1:n) = temp1
temp = soln(ipeak)
soln(ipeak) = soln(irow)
! Swap rows in b
soln(irow) = temp
END IF swap_eqn

! Multiply equation irow by -a1(jrow,irow)/a1(irow,irow),
! and add it to Eqn jrow (for all eqns except irow itself).
eliminate: DO jrow = 1, n
IF ( jrow /= irow ) THEN
factor = -a1(jrow,irow)/a1(irow,irow)
a1(jrow,:) = a1(irow,1:n)*factor + a1(jrow,1:n)
soln(jrow) = soln(irow)*factor + soln(jrow)
END IF
END DO eliminate
END DO mainloop
! End of main loop over all equations. All off-diagonal terms
! are now zero. To get the final answer, we must divide
! each equation by the coefficient of its on-diagonal term.
divide: DO irow = 1, n
soln(irow) = soln(irow) / a1(irow,irow)
a1(irow,irow) = 1.
END DO divide
! Set error flag to 0 and return.
error = 0
END SUBROUTINE simul2

PROGRAM gauss_jordan2
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 10
REAL, DIMENSION(MAX_SIZE, MAX_SIZE) :: a
REAL, DIMENSION(MAX_SIZE) :: b
REAL, DIMENSION(MAX_SIZE) :: soln
INTEGER :: error
CHARACTER(len=20) :: file_name
INTEGER :: i, j, n, istat
!DO i = 1, n
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
    READ(1, *) (a(i, j), j = 1, n), b(i)
    !END DO

    ! Display coefficients.
    WRITE(*, "(/,'Coefficients before call:')")
    DO i = 1, n
      WRITE(*, "(1X,7F11.4)") (a(i, j), j = 1, n), b(i)
    END DO

    ! Solve equations
    CALL simul2(a, b, soln, MAX_SIZE, n, error)

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
        WRITE(*,"(2X,'X(',I2,') = ',F16.6)") i, soln(i)
      END DO
    END IF error_check
  END IF size_ok
ELSE fileopen
  ! Else file open failed. Tell user.
  WRITE(*, 1020) msg
  1020 FORMAT('File open failed: ', A)
END IF fileopen
END PROGRAM gauss_jordan2
