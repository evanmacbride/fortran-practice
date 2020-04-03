MODULE shared_data
IMPLICIT NONE
SAVE

  INTEGER, PARAMETER :: num_vals = 5
  REAL, DIMENSION(num_vals) :: values
END MODULE

PROGRAM test_module
USE shared_data
IMPLICIT NONE

  REAL, PARAMETER :: PI = 3.141592
  values = PI * [1., 2., 3., 4., 5.]

  CALL sub1
END PROGRAM test_module

SUBROUTINE sub1
USE shared_data
IMPLICIT NONE
  WRITE(*, *) values
END SUBROUTINE sub1
