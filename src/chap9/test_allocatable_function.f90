MODULE test_module
! Purpose:
!   To illustrate the use of allocatable function
!   return values.
!
CONTAINS
  FUNCTION test_alloc_fun(n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  REAL, ALLOCATABLE, DIMENSION(:) :: test_alloc_fun
  INTEGER :: i, istat

  ! Get the status of the array
  IF (ALLOCATED(test_alloc_fun)) THEN
    WRITE(*, '(A)') 'Array is allocated'
  ELSE
    WRITE(*, '(A)') 'Array is NOT allocated'
  END IF

  ! Allocate as an n element vector
  ALLOCATE(test_alloc_fun(n), STAT=istat)

  ! Initialize data
  DO i = 1, n
    test_alloc_fun(i) = 6 - i
  END DO

  ! Display contents of array on exit
  WRITE(*, '(A,20F4.1)') 'Array on exit = ', test_alloc_fun

  ! Return to caller
  END FUNCTION test_alloc_fun
END MODULE test_module

PROGRAM test_allocatable_function
! Purpose:
!   To illustrate the use of allocatable function
!   return values
USE test_module
IMPLICIT NONE

INTEGER :: n = 5
REAL, DIMENSION(:), ALLOCATABLE :: res

! Call function and display results
res = test_alloc_fun(n)
WRITE(*, '(A,20F4.1)') 'Function return = ', res
END PROGRAM test_allocatable_function
