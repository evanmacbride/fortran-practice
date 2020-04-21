! Write 2 matrices of random real numbers to 2 separate files.
! The matrices must be compatible for matrix multiplication.
PROGRAM create_matrix_files
IMPLICIT NONE
  INTEGER, DIMENSION(8) :: time
  INTEGER :: rows, cols, i, j
  CHARACTER(LEN=3) :: col_str, row_str
  CHARACTER(LEN=9) :: col_fmt, row_fmt
  REAL, PARAMETER :: VAL_MAX = 199.9

  WRITE(*, *) "Enter dimensions of first matrix."
  READ(*, *) rows, cols
  WRITE(*, *) "Creating two matrices."
  WRITE(*, '("Matrix A:",I6,1X,"x",I6)') rows, cols
  WRITE(*, '("Matrix B:",I6,1X,"x",I6)') cols, rows

  CALL DATE_AND_TIME(VALUES=time)
  CALL SRAND(time(8))

  WRITE(col_str, '(I3)') cols
  col_str = TRIM(col_str)
  col_fmt = '(' // col_str // 'F8.1)'
  WRITE(row_str, '(I3)') rows
  row_str = TRIM(row_str)
  row_fmt = '(' // row_str // 'F8.1)'

  OPEN (1, FILE='matrixA.dat', STATUS='UNKNOWN', ACTION='WRITE')
  WRITE (1, col_fmt) ((RAND() * VAL_MAX - (VAL_MAX / 2.), j=1,cols), i=1,rows)
  OPEN (2, FILE='matrixB.dat', STATUS='UNKNOWN', ACTION='WRITE')
  WRITE (2, row_fmt) ((RAND() * VAL_MAX - (VAL_MAX / 2.), j=1,rows), i=1,cols)
  CLOSE(1)
  CLOSE(2)

END PROGRAM create_matrix_files
