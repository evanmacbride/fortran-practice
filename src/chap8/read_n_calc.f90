PROGRAM read_n_calc
IMPLICIT NONE
REAL, ALLOCATABLE, DIMENSION(:,:) :: data
CHARACTER(LEN=20) :: filename
INTEGER :: status
INTEGER :: rows, cols, i, j

WRITE(*, *) "Enter the filename to read and process:"
READ(*, *) filename

OPEN(1, FILE=filename, STATUS='OLD', ACTION='READ', IOSTAT=status)
READ(1, *) rows, cols
ALLOCATE(data(1:rows, 1:cols))

DO i = 1, rows
  DO j = 1, cols
    READ(1, '(3F8.1)', IOSTAT=status) data(i, j)
    IF (status .lt. 0) THEN
      EXIT
    END IF
  END DO
END DO
DO i = 1, rows
  DO j = 1, cols
    WRITE(*, *) data(i, j)
  END DO
END DO

WRITE(*, *) SUM(data(1, 1:cols))
DEALLOCATE(data)
END PROGRAM read_n_calc
