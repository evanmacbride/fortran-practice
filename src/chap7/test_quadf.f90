REAL FUNCTION quadf(x, a, b, c)
IMPLICIT NONE
  REAL, INTENT(IN) :: x, a, b, c
  quadf = a * x**2 + b * x + c
END FUNCTION quadf

PROGRAM test_quadf
IMPLICIT NONE
  REAL :: quadf, a, b, c, x
  WRITE(*, *) "Enter coefficients a, b, and c for a quadratic equation:"
  READ(*, *) a, b, c
  WRITE(*, *) "Enter an x value to evualate the equation:"
  READ(*, *) x
  WRITE(*, 100) 'quadf(', x, ') = ', quadf(x, a, b, c)
  100 FORMAT (A, F10.4, a, F12.4)
END PROGRAM test_quadf
