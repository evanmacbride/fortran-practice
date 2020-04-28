MODULE test_module
! Purpose:
!   To illustrate the use of allocatable arguments
!   in a subroutine.
!
CONTAINS
  SUBROUTINE test_alloc(array)
  IMPLICIT NONE
  REAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: array
  INTEGER :: i, istat

  ! Get the status of this array
  IF (ALLOCATED(array)) THEN
    WRITE(*, '(A)') 'Sub:  the array is allocated'
    WRITE(*, '(A,6F4.1)') 'Sub:  Array on entry = ', array
  ELSE
    WRITE(*, '(A)') 'Sub:  the array is not allocated'
  END IF

  ! Deallocate the array
  IF (ALLOCATED(array)) THEN
    DEALLOCATE(array, STAT=istat)
  END IF

  ! Reallocate as a 5 element vector
  ALLOCATE(array(5), STAT=istat)

  ! Save data
  DO i = 1, 5
    array(i) = 6 - i
  END DO

  ! Display the contents of array on exit
  WRITE(*, '(A,6F4.1)') 'Sub:  Array on exit = ', array

  ! Return to caller
  END SUBROUTINE test_alloc
END MODULE test_module

PROGRAM test_allocatable_arguments
!
! Purpose:
!   To illustrate the use of allocatable arguments
!   in a subroutine.
!
USE test_module
IMPLICIT NONE

! Declare local variables
REAL, ALLOCATABLE, DIMENSION(:) :: a
INTEGER :: istat

! Allocate the array initially
!ALLOCATE(a(6), STAT=istat)

! Initialize array
!a = [1., 2., 3., 4., 5., 6.]
IF (ALLOCATED(a)) THEN
  ! Display a before call
  WRITE(*, '(A,6F4.1)') 'Main: Array before a call = ', a
ELSE
  WRITE(*, '(A)') 'Main: Array is not allocated'
END IF

! Call subroutine
CALL test_alloc(a)

! Display a after a call
WRITE(*, '(A, 6F4.1)') 'Main: Array a after call = ', a
END PROGRAM test_allocatable_arguments
