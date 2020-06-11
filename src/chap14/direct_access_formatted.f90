PROGRAM direct_access_formatted
IMPLICIT NONE
INTEGER :: i
INTEGER :: irec
CHARACTER(len=40) :: line

OPEN(UNIT=8, FILE='dirio.fmt', ACCESS='DIRECT', &
     FORM='FORMATTED', STATUS='REPLACE', RECL=40)
DO i = 1, 100
  WRITE(8, '(A,I3,A)', REC=i) 'This is record ', i, '.'
END DO
WRITE(*, '(A)', ADVANCE='NO') 'Which record would you like to see? '
READ(*, '(I3)') irec
READ(8, '(A)', REC=irec) line
WRITE(*, '(A,/,5X,A)') ' The record is: ', line
END PROGRAM direct_access_formatted
