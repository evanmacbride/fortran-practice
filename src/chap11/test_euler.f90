MODULE constants
IMPLICIT NONE
REAL, PARAMETER :: E = 2.71828
END MODULE constants

COMPLEX FUNCTION euler(theta)
USE constants
IMPLICIT NONE
REAL, INTENT(IN) :: theta
euler = COS(theta) + CMPLX(0.,1.) * SIN(theta)
END FUNCTION euler

PROGRAM test_euler
USE constants
IMPLICIT NONE
REAL :: theta
COMPLEX :: euler
WRITE(*, *) "Enter a value for theta: "
READ(*, *) theta
WRITE(*, 100) "euler(", theta, ")     =", euler(theta)
WRITE(*, 100) "CEXP(e^i *", theta, ") =", CEXP(CMPLX(0,1.) * theta)
100 FORMAT (A,F8.5,A,"(",F16.12,", ",F16.12,")")
END PROGRAM test_euler
