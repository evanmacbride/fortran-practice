PROGRAM ex15_12
IMPLICIT NONE
INTEGER :: i, istat
INTEGER, DIMENSION(:), POINTER :: ptr1, ptr2
ALLOCATE(ptr1(1:10), STAT=istat)
ptr1 = [(i, i = 1, 10)]
ptr2 => ptr1
WRITE(*, '(A,10I3)') ' ptr1 = ', ptr1
WRITE(*, '(A,10I3)') ' ptr2 = ', ptr2
DEALLOCATE(ptr1, STAT=istat)
NULLIFY(ptr2)
ALLOCATE(ptr1(1:3), STAT=istat)
ptr1 = [-2, 0, 2]
ptr2 => ptr1
WRITE(*, '(A,10I3)') ' ptr1 = ', ptr1
WRITE(*, '(A,10I3)') ' ptr2 = ', ptr2
END PROGRAM ex15_12
