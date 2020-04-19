PROGRAM matrix_mult
IMPLICIT NONE
INTEGER, PARAMETER :: COL_WIDTH = 8
REAL, ALLOCATABLE, DIMENSION(:,:) :: matrixA, matrixB
INTEGER :: i, j, cols = 0, rows = 0, status
CHARACTER(LEN=800) :: line
CHARACTER(LEN=3) :: col_str
CHARACTER(LEN=9) :: col_fmt

OPEN (1, FILE='matrixA.dat', STATUS='OLD', ACTION='READ')
DO
  READ(1, '(A)', IOSTAT=status) line
  IF (status .lt. 0) THEN
    EXIT
  ELSE
    rows = rows + 1
  END IF
END DO
cols = LEN_TRIM(line) / COL_WIDTH
WRITE(*, *) rows, cols

WRITE(col_str, '(I3)') cols
col_str = TRIM(col_str)
col_fmt = '(' // col_str // 'F8.1)'

ALLOCATE(matrixA(1:rows, 1:cols))
REWIND(1)
READ(1, col_fmt) ((matrixA(i, j), j=1, cols), i=1, rows)

WRITE(*, col_fmt) ((matrixA(i, j), j=1, cols), i=1, rows)
WRITE(*, '(F8.1)') matrixA(2, 4)

CLOSE(1)
END PROGRAM matrix_mult
