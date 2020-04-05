MODULE funcs
IMPLICIT NONE
  INTEGER :: i, prod = 1
  CONTAINS
  INTEGER FUNCTION factorial(k)
  IMPLICIT NONE
    INTEGER, INTENT(IN) :: k
    IF (k .eq. 0) THEN
      factorial = 1
    ELSE
      DO i = 1, k
        prod = prod * (i)
      END DO
    END IF
    factorial = prod
  END FUNCTION factorial
END MODULE funcs

PROGRAM cars_passing
IMPLICIT NONE
  INTEGER :: factorial
  REAL :: poisson
  REAL :: t = 1.0, lambda = 1.6
  INTEGER :: i
  !WRITE(*, *) factorial(0), factorial(1), factorial(5)
  WRITE(*, '(F6.4, A, F6.4, A)') lambda, " cars pass in ", t, " minutes."
  DO i = 0, 5
    WRITE(*, '(A, I2, A, F14.10)') "Probability of ", i, " cars passing:", poisson(i, t, lambda)
  END DO
END PROGRAM cars_passing

REAL FUNCTION poisson(k, t, lambda)
USE funcs
IMPLICIT NONE
  INTEGER, INTENT(IN) :: k
  REAL, INTENT(IN) :: t, lambda
  REAL, PARAMETER :: E = 2.71828
  poisson = (E**(-lambda * t)) * (((lambda * t)**k) / REAL(factorial(k)))
END FUNCTION poisson
