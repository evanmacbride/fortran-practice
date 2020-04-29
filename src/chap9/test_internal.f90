PROGRAM test_internal
IMPLICIT NONE
REAL, PARAMETER :: PI = 3.141592
REAL :: theta ! Angle in degrees
WRITE(*, *) 'Enter the desired angle in degrees: '
READ(*, *) theta
WRITE(*, '(A,F10.4)') 'The secant is ', secant(theta)
CONTAINS
  REAL FUNCTION secant(angle_in_degrees)
  REAL :: angle_in_degrees
  secant = 1. / COS(angle_in_degrees * PI / 180.)
  END FUNCTION secant
END PROGRAM test_internal
