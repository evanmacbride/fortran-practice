MODULE vector_module
IMPLICIT NONE
TYPE :: vector
  REAL :: x
  REAL :: y
CONTAINS
  PROCEDURE, PASS :: add
  PROCEDURE, PASS :: sub
END TYPE vector

CONTAINS
  TYPE (vector) FUNCTION add(this, v)
  IMPLICIT NONE
  CLASS (vector) :: this, v
  add = vector(this%x + v%x, this%y + v%y)
  END FUNCTION add

  TYPE (vector) FUNCTION sub(this, v)
  IMPLICIT NONE
  CLASS (vector) :: this, v
  sub = vector(this%x - v%x, this%y - v%y)
  END FUNCTION sub

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

PROGRAM test_bound_vectors
USE vector_module
IMPLICIT NONE
TYPE (vector) :: a, b, c
a = vector(1., 2.)
b = vector(6., 9.)
!c = vector_add(a, b)
c = a%add(b)
WRITE(*, *) c
!c = vector_sub(a, b)
c = a%sub(b)
WRITE(*, *) c
END PROGRAM test_bound_vectors
