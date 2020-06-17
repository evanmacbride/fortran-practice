PROGRAM seq_direct
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_SIZE = 1000
REAL, DIMENSION(MAX_SIZE) :: arr
REAL :: val
CHARACTER, DIMENSION(MAX_SIZE) :: word_arr = 'H'
REAL, DIMENSION(10) :: line
CHARACTER, DIMENSION(10) :: word_line
INTEGER, DIMENSION(10) :: int_line
INTEGER :: i, j, istat, length_unf
CHARACTER(len=80) :: msg

DO i = 1,  MAX_SIZE
  arr(i) = 10**6 - (RAND() * 2 * 10**6)
END DO

OPEN(7, FILE='fmt_seq.dat', STATUS='REPLACE', ACTION='WRITE', &
     IOSTAT=istat, IOMSG=msg)
IF (istat /= 0) THEN
  WRITE(*, *) 'Error opening fmt_seq.dat ', msg
END IF
WRITE(7, '(ES14.6)') (arr(i), i = 1, MAX_SIZE)

OPEN(9, FILE='fmt_dir.dat', STATUS='REPLACE', ACTION='WRITE', ACCESS='DIRECT', &
     FORM='FORMATTED', RECL=14, IOSTAT=istat, IOMSG=msg)
IF (istat /= 0) THEN
 WRITE(*, *) 'Error opening fmt_dir.dat ', msg
END IF
DO i = 1, MAX_SIZE
  WRITE(9, '(ES14.6)', REC=i) arr(i)
END DO

! Get the length required to store a value in the array in an unformatted file.
INQUIRE(IOLENGTH=length_unf) arr(1)
OPEN(8, FILE='unf_dir.dat', STATUS='REPLACE', ACTION='WRITE', ACCESS='DIRECT', &
     FORM='UNFORMATTED', RECL=length_unf, IOSTAT=istat, IOMSG=msg)
IF (istat /= 0) THEN
 WRITE(*, *) 'Error opening unf_dir.dat ', msg
END IF
DO i = 1, MAX_SIZE
  WRITE(8, REC=i) arr(i)
END DO
CLOSE(7)
CLOSE(8)
CLOSE(9)
OPEN(9, FILE='fmt_dir.dat', STATUS='OLD', ACTION='READ', ACCESS='DIRECT', &
     FORM='FORMATTED', RECL=14, IOSTAT=istat, IOMSG=msg)
OPEN(8, FILE='unf_dir.dat', STATUS='OLD', ACTION='READ', ACCESS='DIRECT', &
     FORM='UNFORMATTED', RECL=length_unf, IOSTAT=istat, IOMSG=msg)
READ(8,REC=500) val
WRITE(*, *) val
READ(9,'(ES14.6)',REC=500) val
WRITE(*, *) val
!!!!
CLOSE(8)
CLOSE(9)
END PROGRAM seq_direct
