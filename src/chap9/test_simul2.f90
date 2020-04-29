MODULE solvers
! This module contains simultaneous equation solvers.
INTERFACE
  MODULE SUBROUTINE simul(a, b, ndim, n, error)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ndim
  REAL, INTENT(INOUT), DIMENSION(ndim, ndim) :: a
  REAL, INTENT(INOUT), DIMENSION(ndim) :: b
  INTEGER, INTENT(IN) :: n
  INTEGER, INTENT(OUT) :: error
  END SUBROUTINE simul
END INTERFACE
END MODULE solvers

SUBMODULE(solvers) solvers_exec
CONTAINS
  MODULE PROCEDURE simul
  REAL, PARAMETER :: EPSILON = 1.0E-6
  REAL :: factor
  INTEGER :: irow, ipeak, jrow, kcol
  REAL :: temp

  mainloop: DO irow = 1, n
    ipeak = irow
    max_pivot: DO jrow = irow + 1, n
      IF (ABS(a(jrow, irow)) > ABS(a(ipeak, irow))) THEN
        ipeak = jrow
      END IF
    END DO max_pivot

    singular: IF (ABS(a(ipeak, irow)) < EPSILON) THEN
      error = 1
      RETURN
    END IF singular

    swap_eqn: IF(ipeak /= irow) THEN
      DO kcol = 1, n
        temp           = a(ipeak, kcol)
        a(ipeak, kcol) = a(irow, kcol)
        a(irow, kcol)  = temp
      END DO
      temp     = b(ipeak)
      b(ipeak) = b(irow)
      b(irow)  = temp
    END IF swap_eqn

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
    b(irow) = b(irow) / a(irow, irow)
    a(irow, irow) = 1.
  END DO divide
  error = 0
  END PROCEDURE simul
END SUBMODULE solvers_exec

PROGRAM test_simul2
USE solvers
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 10
REAL, DIMENSION(MAX_SIZE, MAX_SIZE) :: a
REAL, DIMENSION(MAX_SIZE) :: b
INTEGER :: error
CHARACTER(len=20) :: file_name
INTEGER :: i, j
CHARACTER(len=80) :: msg
INTEGER :: n, istat

WRITE(*, "('Enter the file name containing the equations: ')")
READ(*, '(A20)') file_name

END PROGRAM test_simul2
