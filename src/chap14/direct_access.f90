MODULE timer
IMPLICIT NONE
INTEGER, PARAMETER :: LNG = SELECTED_INT_KIND(15)
INTEGER(KIND=LNG), SAVE :: begin_timer, count_rate, count_max
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13,r=200)
CONTAINS
  SUBROUTINE set_timer()
  IMPLICIT NONE
  CALL SYSTEM_CLOCK(begin_timer, count_rate, count_max)
  END SUBROUTINE set_timer

  ! Return elapsed time in seconds.
  SUBROUTINE elapsed_time(elapsed)
  IMPLICIT NONE
  REAL(KIND=DBL), INTENT(OUT) :: elapsed
  INTEGER(KIND=LNG) :: now
  CALL SYSTEM_CLOCK(now)
  elapsed = (now - begin_timer) / REAL(count_rate, DBL)
  END SUBROUTINE elapsed_time
END MODULE timer


PROGRAM direct_access
USE timer
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_RECORDS = 50000
INTEGER, PARAMETER :: NUMBER_OF_READS = 50000
INTEGER :: i, j
INTEGER :: length_fmt = 84
INTEGER :: length_unf
INTEGER :: irec
REAL(KIND=DBL) :: time_fmt, time_unf, value
REAL(KIND=DBL), DIMENSION(4) :: values
INQUIRE(IOLENGTH=length_unf) values
WRITE(*, '(A,I2)') ' The unformatted record length is ', length_unf
WRITE(*, '(A,I2)') ' The formatted record length is ', length_fmt
OPEN(UNIT=8, FILE='dirio.unf', ACCESS='DIRECT', FORM='UNFORMATTED', &
     STATUS='REPLACE', RECL=length_unf)
OPEN(UNIT=9, FILE='dirio.fmt', ACCESS='DIRECT', FORM='FORMATTED', &
     STATUS='REPLACE', RECL=length_fmt)

DO i = 1, MAX_RECORDS
  DO j = 1, 4
    value = RAND()
    values(j) = 30._DBL * value
  END DO
  WRITE(8, REC=i) values
  WRITE(9, '(4ES21.14)', REC=i) values
END DO
CALL set_timer()
DO i = 1, NUMBER_OF_READS
  value = RAND()
  irec = (MAX_RECORDS-1) * value + 1
  READ(8, REC=irec) values
END DO
CALL elapsed_time(time_unf)
CALL set_timer()
DO i = 1, NUMBER_OF_READS
  value = RAND()
  irec = (MAX_RECORDS-1) * value + 1
  READ(9, '(4ES21.14)', REC=irec) values
END DO
CALL elapsed_time(time_fmt)
WRITE(*, '(A,F6.2)') ' Time for unformatted file = ', time_unf
WRITE(*, '(A,F6.2)') ' Time for formatted file   = ', time_fmt
END PROGRAM direct_access
