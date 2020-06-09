! Extend operators for a user-defined vector type. Create new operator .DOT. to
! calculate dot product. Set module to PRIVATE, and set specific elements of
! module to PUBLIC.
MODULE vector_operators
IMPLICIT NONE
PRIVATE
PUBLIC :: vector, ASSIGNMENT(=), OPERATOR(+), OPERATOR(-), OPERATOR(*), &
          OPERATOR(/), OPERATOR(.DOT.), signal_me
TYPE :: vector
  !PRIVATE
  REAL :: x
  REAL :: y
  REAL :: z
END TYPE vector
CHARACTER(LEN=20), PUBLIC :: hello = "HELLO WORLD"
INTEGER, PUBLIC, PROTECTED :: signal = 29

INTERFACE OPERATOR(+)
  MODULE PROCEDURE add_vectors
  MODULE PROCEDURE add_vector_and_scalar
END INTERFACE

INTERFACE OPERATOR(-)
  MODULE PROCEDURE sub_vectors
END INTERFACE

INTERFACE OPERATOR(*)
  MODULE PROCEDURE mult_by_scalar
END INTERFACE

INTERFACE OPERATOR(/)
  MODULE PROCEDURE div_by_scalar
END INTERFACE

INTERFACE OPERATOR(.DOT.)
  MODULE PROCEDURE dot_product
END INTERFACE

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE array_to_vector
  MODULE PROCEDURE vector_to_array
END INTERFACE

CONTAINS
  INTEGER FUNCTION signal_me()
  IMPLICIT NONE
  signal = 33
  signal_me = signal
  END FUNCTION signal_me

  TYPE(vector) FUNCTION add_vectors(v1, v2)
  IMPLICIT NONE
  TYPE(vector), INTENT(IN) :: v1, v2
  add_vectors = vector(v1%x + v2%x, v1%y + v2%y, v1%z + v2%z)
  END FUNCTION add_vectors

  TYPE(vector) FUNCTION sub_vectors(v1, v2)
  IMPLICIT NONE
  TYPE(vector), INTENT(IN) :: v1, v2
  sub_vectors = vector(v1%x - v2%x, v1%y - v2%y, v1%z - v2%z)
  END FUNCTION sub_vectors

  TYPE(vector) FUNCTION mult_by_scalar(v, s)
  IMPLICIT NONE
  TYPE(vector), INTENT(IN) :: v
  REAL, INTENT(IN) :: s
  mult_by_scalar = vector(v%x * s, v%y * s, v%z * s)
  END FUNCTION mult_by_scalar

  TYPE(vector) FUNCTION div_by_scalar(v, s)
  IMPLICIT NONE
  TYPE(vector), INTENT(IN) :: v
  REAL, INTENT(IN) :: s
  div_by_scalar = vector(v%x / s, v%y / s, v%z / s)
  END FUNCTION div_by_scalar

  REAL FUNCTION dot_product(v1, v2)
  IMPLICIT NONE
  TYPE(vector), INTENT(IN) :: v1, v2
  dot_product = v1%x * v2%x + v1%y * v2%y + v1%z * v2%z
  END FUNCTION dot_product

  SUBROUTINE vector_to_array(vec, arr)
  IMPLICIT NONE
  REAL, DIMENSION(3), INTENT(IN) :: arr
  TYPE(vector), INTENT(OUT) :: vec
  vec = vector(arr(1), arr(2), arr(3))
  END SUBROUTINE vector_to_array

  SUBROUTINE array_to_vector(arr, vec)
  IMPLICIT NONE
  TYPE(vector), INTENT(IN) :: vec
  REAL, DIMENSION(3), INTENT(OUT) :: arr
  arr(1) = vec%x
  arr(2) = vec%y
  arr(3) = vec%z
  END SUBROUTINE array_to_vector

  TYPE(vector) FUNCTION add_vector_and_scalar(v, s)
  IMPLICIT NONE
  TYPE(vector), INTENT(IN) :: v
  REAL, INTENT(IN) :: s
  add_vector_and_scalar = vector(v%x + s, v%y + s, v%z + s)
  END FUNCTION add_vector_and_scalar

END MODULE vector_operators

PROGRAM test_vector_operators
USE vector_operators
IMPLICIT NONE
TYPE(vector) :: u, v
REAL :: s
REAL, DIMENSION(3) :: a

a = [7., 8., 9.]
s = 2
!greet = hello
!u = vector(1., 2., 3.)
!v = vector(4., 5., 4.)
u = [1., 2., 3.]
v = [4., 5., 4.]
WRITE(*, *) u + v
WRITE(*, *) u * s
WRITE(*, *) v / 3.
WRITE(*, *) u .DOT. v
v = a
WRITE(*, *) v
a = u
WRITE(*, *) a
!signal = 55
WRITE(*, *) signal_me()
END PROGRAM test_vector_operators
