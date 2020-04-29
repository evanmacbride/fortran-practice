MODULE trig
IMPLICIT NONE
REAL, PARAMETER :: PI = 3.141592
CONTAINS
  ELEMENTAL REAL FUNCTION sine_degrees(a)
  REAL, INTENT(IN) :: a
  sine_degrees = SIN(a * PI / 180.)
  END FUNCTION sine_degrees

  ELEMENTAL REAL FUNCTION cosine_degrees(a)
  REAL, INTENT(IN) :: a
  cosine_degrees = COS(a * PI / 180.)
  END FUNCTION cosine_degrees

  ELEMENTAL REAL FUNCTION tangent_degrees(a)
  REAL, INTENT(IN) :: a
  tangent_degrees = TAN(a * PI / 180.)
  END FUNCTION tangent_degrees
END MODULE trig

PROGRAM elem_trig
USE trig
IMPLICIT NONE
REAL, DIMENSION(3,3) :: x
INTEGER :: i, j
x = RESHAPE([85.,45.,30.,60.,60.,-85.,0.,45.,-30.], SHAPE(x))
WRITE(*, *) 'INPUT (DEGREES)'
WRITE(*, '(3F9.4)') ((x(i, j), i = 1, 3), j = 1, 3)
WRITE(*, *) 'SINE'
WRITE(*, '(3F9.4)') ((sine_degrees(x(i, j)), i = 1, 3), j = 1, 3)
WRITE(*, *) 'COSINE'
WRITE(*, '(3F9.4)') ((cosine_degrees(x(i, j)), i = 1, 3), j = 1, 3)
WRITE(*, *) 'TANGENT'
WRITE(*, '(3F9.4)') ((tangent_degrees(x(i, j)), i = 1, 3), j = 1, 3)
END PROGRAM elem_trig
