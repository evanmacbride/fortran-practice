SUBROUTINE sort(a, n)
IMPLICIT NONE
REAL, DIMENSION(:), INTENT(INOUT) :: a
INTEGER, INTENT(IN) :: n
END SUBROUTINE sort

PROGRAM interface_example
IMPLICIT NONE
INTERFACE
  SUBROUTINE sort(a, n)
  IMPLICIT NONE
  REAL, DIMENSION(:), INTENT(INOUT) :: a
  INTEGER, INTENT(IN) :: n
  END SUBROUTINE sort
END INTERFACE

REAL, DIMENSION(6) :: array = [1., 5., 3., 2., 6., 4.]
INTEGER :: nvals = 6
CALL sort(N=nvals, A=array)
WRITE(*, *) array
END PROGRAM interface_example
