PROGRAM rect_polar
IMPLICIT NONE
  REAL, DIMENSION(2) :: rect, polar
  REAL, PARAMETER :: PI = 3.1416

  WRITE(*, *) "Enter the magnitude:"
  READ(*, *) polar(1)
  WRITE(*, *) "Enter the angle in degrees:"
  READ(*, *) polar(2)

  rect(1) = polar(1) * COS(polar(2) * PI / 180.0)
  rect(2) = polar(1) * SIN(polar(2) * PI / 180.0)

  WRITE(*, '(A,F7.4,A,F7.4)') "Rectangular coordinates are ", rect(1), ", ", rect(2)

END PROGRAM rect_polar
