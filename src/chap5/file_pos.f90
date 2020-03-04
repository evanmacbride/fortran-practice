PROGRAM file_pos
IMPLICIT NONE
  REAL :: r
  INTEGER :: status, line, index
  CHARACTER :: msg
  OPEN(UNIT=1, FILE='temp.dat', STATUS='NEW', ACTION='READWRITE', IOSTAT=status, IOMSG=msg)
  WRITE(*, *) "Enter nonnegative real numbers to store in a temporary file."
  WRITE(*, *) "Enter a negative real number to stop."
  DO
    READ(*, '(F3.1)') r
    IF ((r .lt. 0.0) .OR. (status .ne. 0)) THEN
      EXIT
    ELSE
      WRITE(1, *, IOSTAT=status) r
    END IF
  END DO
  REWIND(1)

  WRITE(*, *) "Enter a line number to review."
  READ(*, *) line
  DO index = 1, line
    READ(1, *, IOSTAT=status) r
  END DO
  WRITE(*, *) r
  CLOSE(1)
END PROGRAM file_pos
