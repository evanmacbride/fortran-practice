PROGRAM bad_ptr
IMPLICIT NONE
INTEGER :: i, istat
INTEGER, DIMENSION(:), POINTER :: ptr1, ptr2
! Allocate and initialize memory
ALLOCATE(ptr1(1:10), STAT=istat)
ptr1 = [(i, i = 1, 10)]
ptr2 => ptr1
! Check associated status of ptrs.
WRITE(*, '(A,2L5)') ' Are ptr1, ptr2 associated? ', &
                    ASSOCIATED(ptr1), ASSOCIATED(ptr2)
WRITE(*, '(A,10I3)') ' ptr1 = ', ptr1
WRITE(*, '(A,10I3)') ' ptr2 = ', ptr2
! Now deallocate memory associated with ptr1
DEALLOCATE(ptr1, STAT=istat)
! Check associated status of ptrs.
WRITE(*, '(A,2L5)') ' Are ptr1, ptr2 associated? ', &
                    ASSOCIATED(ptr1), ASSOCIATED(ptr2)
! Write out memory associate with ptr2
WRITE(*, '(A10I3)') ' ptr2 = ', ptr2
ALLOCATE(ptr1(1:2), STAT=istat)   ! Reallocate ptr1
ptr1 = [21, 22]
WRITE(*,'(A,10I3)') ' ptr1 = ', ptr1
WRITE(*,'(A,10I3)') ' ptr2 = ', ptr2
END PROGRAM bad_ptr
