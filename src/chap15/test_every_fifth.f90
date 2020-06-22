MODULE funcs
CONTAINS
  FUNCTION every_fifth(ptr_array) RESULT(ptr_fifth)
  IMPLICIT NONE
  INTEGER, DIMENSION(:), POINTER :: ptr_array
  INTEGER, DIMENSION(:), POINTER :: ptr_fifth
  INTEGER :: low    ! Array lower bound
  INTEGER :: high   ! Array upper bound

  low = LBOUND(ptr_array,1)
  high = UBOUND(ptr_array,1)
  ptr_fifth => ptr_array(low:high:5)
  END FUNCTION every_fifth
END MODULE funcs

PROGRAM test_every_fifth
USE funcs
IMPLICIT NONE
!INTERFACE
!  FUNCTION every_fifth(ptr_array) RESULT(ptr_fifth)
!  INTEGER, DIMENSION(:), POINTER :: ptr_array
!  INTEGER, DIMENSION(:), POINTER :: ptr_fifth
!  END FUNCTION
!END INTERFACE
INTEGER, DIMENSION(:), POINTER :: arr
INTEGER, DIMENSION(:), POINTER :: fifths
INTEGER :: i, istat
!INTEGER, DIMENSION(:), POINTER :: every_fifth

ALLOCATE(arr(50),STAT=istat)
arr = [(i * 2, i = 1, 50)]
WRITE(*, '(5I4)') (arr(i), i = 1, 50)
fifths => every_fifth(arr)
WRITE(*, '(5I4)') (fifths(i), i = 1, 10)
END PROGRAM test_every_fifth
