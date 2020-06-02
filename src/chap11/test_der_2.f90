MODULE tools
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
CONTAINS
  SUBROUTINE derivative(func, x, step_size, result)
  IMPLICIT NONE
  INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
  REAL(KIND=DBL), EXTERNAL :: func
  REAL(KIND=DBL), INTENT(IN) :: x
  REAL(KIND=DBL), INTENT(IN) :: step_size
  REAL(KIND=DBL), INTENT(OUT) :: result
  ! Calculate average rate of change at x over step_size
  result = (func(x + step_size) - func(x)) / step_size
  END SUBROUTINE derivative

  REAL(KIND=DBL) FUNCTION f1(x)
  IMPLICIT NONE
  REAL(KIND=DBL), INTENT(IN) :: x
  f1 = x**2 + 1._DBL
  END FUNCTION f1

  REAL(KIND=DBL) FUNCTION f2(x)
  IMPLICIT NONE
  REAL(KIND=DBL), INTENT(IN) :: x
  f2 = 10._DBL * SIN(20._DBL * x)
  END FUNCTION f2
END MODULE tools

PROGRAM test_der_2
USE tools
IMPLICIT NONE
REAL(KIND=DBL) :: x, step_size, result
x = 0._DBL
step_size = 0.000001_DBL
CALL derivative(f2, x, step_size, result)
WRITE(*, *) result
END PROGRAM test_der_2
