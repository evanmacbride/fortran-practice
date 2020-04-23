MODULE test_module
CONTAINS
  SUBROUTINE test_array(array)
  IMPLICIT NONE
  REAL, DIMENSION(:,:) :: array
  INTEGER :: i1, i2, j1, j2
  ! Get details about array
  i1 = LBOUND(array,1)
  i2 = UBOUND(array,1)
  j1 = LBOUND(array,1)
  j2 = UBOUND(array,2)
  WRITE(*, 100) i1, i2, j1, j2
  100 FORMAT ('The bounds are: (',I2,':',I2','I2,':',I2,')')
  WRITE(*, 110) SHAPE(array)
  110 FORMAT ('The shape is:    ',2I4)
  WRITE(*, 120) SIZE(array)
  120 FORMAT ('The size is:     ',I4)
  END SUBROUTINE test_array
END MODULE test_module

PROGRAM assumed_shape
USE test_module
IMPLICIT NONE

REAL, DIMENSION(-5:5,-5:5) :: a = 0
REAL, DIMENSION(10,2) :: b = 1

WRITE(*, *) 'Calling test_array with array a:'
CALL test_array(a)
WRITE(*, *) 'Calling test_array with array b:'
CALL test_array(b)
END PROGRAM assumed_shape
