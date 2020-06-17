PROGRAM form_unform
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 100000
REAL, DIMENSION(MAX_SIZE) :: arr
CHARACTER, DIMENSION(MAX_SIZE) :: word_arr = 'H'
REAL, DIMENSION(10) :: line
CHARACTER, DIMENSION(10) :: word_line
INTEGER, DIMENSION(10) :: int_line
INTEGER :: i, j, istat
CHARACTER(len=80) :: msg

DO i = 1,  MAX_SIZE
  arr(i) = 10**7 - (RAND() * 2 * 10**7)
END DO

OPEN(7, FILE='fmt.dat', STATUS='REPLACE', ACTION='WRITE', &
     IOSTAT=istat, IOMSG=msg)
WRITE(7, '(10ES14.6)') (arr(i), i = 1, MAX_SIZE)
OPEN(8, FILE='unf.dat', STATUS='REPLACE', ACTION='WRITE', &
     FORM='UNFORMATTED', IOSTAT=istat, IOMSG=msg)
DO i = 1, MAX_SIZE / 10
  !WRITE(8) (arr(j), j = 1 * i, 1 * i + 10)
  WRITE(8) (word_arr(j), j = 1 * i, 1 * i + 10)
END DO
CLOSE(7)
CLOSE(8)

OPEN(8, FILE='unf.dat', STATUS='OLD', ACTION='READ', &
     FORM='UNFORMATTED', IOSTAT=istat, IOMSG=msg)
OPEN(7, FILE='fmt.dat', STATUS='REPLACE', ACTION='WRITE', &
     IOSTAT=istat, IOMSG=msg)
DO i = 1, MAX_SIZE / 10
  !READ(8) line
  !WRITE(7, '(10ES14.6)') (line(j), j = 1, 10)
  READ(8) word_line
  WRITE(7, '(10I14)') (int_line(j), j = 1, 10)
END DO

END PROGRAM form_unform
