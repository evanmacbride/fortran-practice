PROGRAM test_allocatable_arrays
IMPLICIT NONE

REAL, DIMENSION(:), ALLOCATABLE :: arr1
REAL, DIMENSION(8) :: arr2 = [1., 2., 3., 4., 5., 6., 7., 8.]
REAL, DIMENSION(3) :: arr3 = [1., -2., 3.]

arr1 = 2. * arr3
WRITE(*, *) arr1
arr1 = arr2(1:8:2)
WRITE(*, *) arr1
arr1 = 2. * arr2(1:4)
WRITE(*, *) arr1

END PROGRAM test_allocatable_arrays
