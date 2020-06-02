SUBROUTINE quad(a, b, c, x1, x2)
IMPLICIT NONE
REAL, INTENT(IN) :: a, b, c
COMPLEX, INTENT(OUT) :: x1, x2
COMPLEX :: discriminant
discriminant = CMPLX(b**2 - 4. * a * c, 0)
x1 = (- b + SQRT(discriminant)) / 2. * a
x2 = (- b - SQRT(discriminant)) / 2. * a
END SUBROUTINE quad

PROGRAM quadratic
IMPLICIT NONE
REAL :: a, b, c
COMPLEX :: x1, x2
READ(*, *) a, b, c
CALL quad(a, b, c, x1, x2)
WRITE(*, *) x1, x2
END PROGRAM quadratic
