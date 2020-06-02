! This program fails because return kind is not specified for f1 & f2.

REAL FUNCTION f1(x)
IMPLICIT NONE
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
REAL(KIND=DBL), INTENT(IN) :: x

f1 = 10._DBL * SIN(20._DBL * x)
END FUNCTION f1

REAL FUNCTION f2(x)
IMPLICIT NONE
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
REAL(KIND=DBL), INTENT(IN) :: x
f2 = 77._DBL
END FUNCTION f2

! Placeholder for derivative subroutine
SUBROUTINE derivative(func, x, step_size, result)
IMPLICIT NONE
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
REAL(KIND=DBL), EXTERNAL :: func
REAL(KIND=DBL), INTENT(IN) :: x
REAL(KIND=DBL), INTENT(IN) :: step_size
REAL(KIND=DBL), INTENT(OUT) :: result

result = func(x)
END SUBROUTINE derivative

! Placeholder for derivative function
REAL FUNCTION deriv_func(func, x, step_size)
IMPLICIT NONE
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
REAL(KIND=DBL), EXTERNAL :: func
REAL(KIND=DBL), INTENT(IN) :: x
REAL(KIND=DBL), INTENT(IN) :: step_size

deriv_func = func(x)
END FUNCTION deriv_func

PROGRAM test_derivative
IMPLICIT NONE
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
REAL(KIND=DBL), EXTERNAL :: f2
REAL :: deriv_func
REAL(KIND=DBL) :: x
REAL(KIND=DBL) :: step_size
REAL(KIND=DBL) :: result
step_size = 1._DBL
x = 2._DBL
result = 0._DBL

WRITE(*, '(F20.12)') deriv_func(f2, x, step_size)
END PROGRAM test_derivative
