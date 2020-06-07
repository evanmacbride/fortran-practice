MODULE generic_procedure_module
IMPLICIT NONE
! Declare type vector
TYPE :: vector
  REAL :: x
  REAL :: y
CONTAINS
  GENERIC :: add => vector_plus_vector, vector_plus_scalar
  PROCEDURE, PASS :: vector_plus_scalar
  PROCEDURE, PASS :: vector_plus_vector
END TYPE vector

CONTAINS
  TYPE (vector) FUNCTION vector_plus_vector(this, v2)
  IMPLICIT NONE
  CLASS(vector), INTENT(IN) :: this
  CLASS(vector), INTENT(IN) :: v2
  vector_plus_vector%x = this%x + v2%x
  vector_plus_vector%y = this%y + v2%y
  END FUNCTION vector_plus_vector

  TYPE (vector) FUNCTION vector_plus_scalar(this, scalar)
  IMPLICIT NONE
  CLASS(vector), INTENT(IN) :: this
  REAL, INTENT(IN) :: scalar
  vector_plus_scalar%x = this%x + scalar
  vector_plus_scalar%y = this%y + scalar
  END FUNCTION vector_plus_scalar
END MODULE generic_procedure_module

PROGRAM test_generic_procedures
USE generic_procedure_module
IMPLICIT NONE
TYPE(vector) :: v1
TYPE(vector) :: v2
REAL :: s

WRITE(*, *) 'ENTER FIRST VECTOR (X, Y):'
READ(*, *) v1%x, v1%y
WRITE(*, *) 'ENTER SECOND VECTOR (X, Y):'
READ(*, *) v2%x, v2%y
WRITE(*, *) 'ENTER A SCALAR:'
READ(*, *) s
WRITE(*, 1000) v1%add(v2)
1000 FORMAT ('The sum of the vectors is (',F8.2,',',F8.2,')')
WRITE(*, 1010) v1%add(s)
1010 FORMAT ('The sum of the vector and the scalar is (',F8.2,',',F8.2,')')
END PROGRAM test_generic_procedures
