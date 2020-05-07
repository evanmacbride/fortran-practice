PROGRAM sort4
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 10 ! Max number to sort
CHARACTER(len=20), DIMENSION(MAX_SIZE) :: a
LOGICAL :: exceed = .FALSE.
CHARACTER(len=20) :: filename
INTEGER :: i
CHARACTER(len=80) :: msg
INTEGER :: nvals = 0
INTEGER :: status
CHARACTER(len=20) :: temp

WRITE(*, *) "Enter the file name with the data to be sorted:"
READ(*, '(A20)') filename
OPEN(UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
     IOSTAT=status, IOMSG=msg)
fileopen: IF (status == 0) THEN
  DO
    READ(9, *, IOSTAT=status) temp
    IF (status /= 0) EXIT
    nvals = nvals + 1
    size: IF (nvals <= MAX_SIZE) THEN
      a(nvals) = temp
      !WRITE(*, *) temp
    ELSE
      exceed = .TRUE.
    END IF size
  END DO

  toobig: IF (exceed) THEN
    WRITE(*, 1000) nvals, MAX_SIZE
    1000 FORMAT (' Maximum array size exceeded: ', I6, ' > ', I6)
  ELSE
    CALL sortc(a, nvals)
    WRITE(*, *) 'The sorted output data values are: '
    WRITE(*, '(4X,A)') (a(i), i = 1, nvals)
  END IF toobig
ELSE fileopen
  WRITE(*, 1050) TRIM(msg)
  1050 FORMAT ('File open failed. Error: ', A)
END IF fileopen
END PROGRAM sort4

SUBROUTINE sortc(array, n)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
CHARACTER(len=20), DIMENSION(n), INTENT(INOUT) :: array
INTEGER :: i, iptr, j
CHARACTER(len=20) :: temp


outer: DO i = 1, n - 1
  iptr = i
  inner: DO j = i + 1, n
    minval: IF(array(j) < array(iptr)) THEN
      !WRITE(*, *) array(j), array(iptr)
      iptr = j
    END IF minval
  END DO inner
  swap: IF (i /= iptr) THEN
    temp        = array(i)
    array(i)    = array(iptr)
    array(iptr) = temp
  END IF swap
END DO outer
!WRITE(*, '(4X,A)') (array(i), i = 1, n)
END SUBROUTINE sortc
