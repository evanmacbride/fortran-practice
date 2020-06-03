PROGRAM test_type_extension
IMPLICIT NONE

TYPE :: point
  REAL :: x
  REAL :: y
END TYPE point

TYPE, EXTENDS(point) :: point3d
  REAL :: z
END TYPE point3d

TYPE (point3d) :: my_point
my_point%x = 1.
my_point%y = 2.
my_point%z = 3.

WRITE(*, *) 'my_point = ', my_point
END PROGRAM test_type_extension
