MODULE vector_module
IMPLICIT NONE
TYPE :: vector
  REAL :: x
  REAL :: y
END TYPE vector

CONTAINS
  TYPE (vector) FUNCTION vector_add(v1, v2)
  IMPLICIT NONE
  TYPE (vector), INTENT(IN) :: v1, v2
  vector_add = vector(v1%x + v2%x, v1%y + v2%y)
  END FUNCTION vector_add

  TYPE (vector) FUNCTION vector_sub(v1, v2)
  IMPLICIT NONE
  TYPE (vector), INTENT(IN) :: v1, v2
  vector_sub = vector(v1%x - v2%x, v1%y - v2%y)
  END FUNCTION vector_sub
END MODULE vector_module

PROGRAM test_vectors
USE vector_module
IMPLICIT NONE
TYPE (vector) :: a, b, c
a = vector(1., 2.)
b = vector(6., 9.)
c = vector_add(a, b)
WRITE(*, *) c
c = vector_sub(a, b)
WRITE(*, *) c
END PROGRAM test_vectors
