! Write a 100x100 matrix of random real numbers to a file.
PROGRAM write_data
IMPLICIT NONE
  INTEGER, PARAMETER :: ROWS = 100, COLS = 100
  INTEGER, DIMENSION(8) :: time
  INTEGER :: i, j
  CHARACTER(LEN=3) :: col_str
  CHARACTER(LEN=9) :: fmt

  CALL DATE_AND_TIME(VALUES=time)
  CALL SRAND(time(8))

  WRITE(col_str, '(I3)') COLS
  col_str = TRIM(col_str)
  fmt = '(' // col_str // 'F8.1)'

  OPEN (1, FILE='data.txt', STATUS='NEW', ACTION='WRITE')
  WRITE(1,'(2I5)') ROWS, COLS
  WRITE (1, fmt) ((RAND() * 1000. - 500., j=1,COLS), i=1,ROWS)

END PROGRAM write_data
