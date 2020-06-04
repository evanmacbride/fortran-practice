! Define types for Cartesian geometry, and define functions for distance and
! calculating slope and intercept.
MODULE cartesian_tools
IMPLICIT NONE

TYPE :: point
  REAL :: x
  REAL :: y
END TYPE point

TYPE :: line
  REAL :: m
  REAL :: b
END TYPE line
CONTAINS
  REAL FUNCTION dist(p1, p2)
  IMPLICIT NONE
  TYPE (point), INTENT(IN) :: p1, p2
  dist = SQRT((p2%x - p1%x)**2 + (p2%y - p1%y)**2)
  END FUNCTION dist

  TYPE (line) FUNCTION connect(p1, p2)
  IMPLICIT NONE
  TYPE (point), INTENT(IN) :: p1, p2
  REAL :: m, b
  IF (p1%x == p2%x .AND. p1%y == p2%y) THEN
    connect = line(0, 0)
  ELSE
    m = (p2%y - p1%y) / (p2%x - p1%x)
    b = p1%y - m * p1%x
    connect = line(m, b)
  END IF
  END FUNCTION
END MODULE cartesian_tools

PROGRAM cartesian
USE cartesian_tools
IMPLICIT NONE
TYPE (point) :: p1, p2, p3
TYPE (line) :: p_line

p1 = point(0.,0.)
p2 = point(1.,2.)
p3 = p2
WRITE(*, *) dist(p1, p2)
WRITE(*, *) dist(p2, p3)
WRITE(*, *) connect(p1, p2)
WRITE(*, *) connect(p2, p3)

END PROGRAM cartesian
