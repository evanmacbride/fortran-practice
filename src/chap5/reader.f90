PROGRAM reader
IMPLICIT NONE
  INTEGER :: status, num_lines = 0
  CHARACTER(85) :: temp, msg = ''

  OPEN(UNIT = 1, FILE='num_table.f90',STATUS='OLD',ACTION='READ',IOSTAT=status, IOMSG=msg)

  DO
    READ(1, '(85A)',IOSTAT=status) temp
    IF (status .ne. 0) THEN
      EXIT
    ELSE
      num_lines = num_lines + 1
    END IF
    WRITE(*, *) TRIM(temp)
  END DO
  WRITE(*, '(I4,1X,A)') num_lines, "LINES READ"
  CLOSE(1)
END PROGRAM reader
