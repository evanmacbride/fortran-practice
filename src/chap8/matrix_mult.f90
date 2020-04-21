MODULE matrix_maintenance
IMPLICIT NONE
INTEGER, PARAMETER :: COL_WIDTH = 8

CONTAINS
  SUBROUTINE LOAD_MATRIX(filename, arr, r, c, fmt)
  IMPLICIT NONE
  CHARACTER(*), INTENT(IN) :: filename
  REAL, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: arr
  INTEGER, INTENT(OUT) :: r, c
  CHARACTER(*) :: fmt
  INTEGER :: i, j, status
  CHARACTER(LEN=800) :: line
  CHARACTER(LEN=3) :: col_str

  OPEN (1, FILE=filename, STATUS='OLD', ACTION='READ')
  DO
    READ(1, '(A)', IOSTAT=status) line
    IF (status .lt. 0) THEN
      EXIT
    ELSE
      r = r + 1
    END IF
  END DO
  c = LEN_TRIM(line) / COL_WIDTH

  ! Convert the integer c to a string
  WRITE(col_str, '(I3)') c
  col_str = TRIM(col_str)
  fmt = '(' // col_str // 'F8.1)'

  ALLOCATE(arr(1:r, 1:c))
  REWIND(1)
  READ(1, fmt) ((arr(i, j), j=1, c), i=1, r)
  END SUBROUTINE LOAD_MATRIX
END MODULE matrix_maintenance

PROGRAM matrix_mult
USE matrix_maintenance
IMPLICIT NONE

REAL, ALLOCATABLE, DIMENSION(:,:) :: matrixA, matrixB, matrixC
REAL :: temp
INTEGER :: i, j, k, l, colsA, rowsA, colsB, rowsB
CHARACTER(LEN=9) :: col_fmtA, col_fmtB

rowsA = 0
colsA = 0
CALL LOAD_MATRIX("matrixA.dat", matrixA, rowsA, colsA, col_fmtA)
WRITE(*, *) "MATRIX A:"
WRITE(*, col_fmtA) ((matrixA(i, j), j=1, colsA), i=1, rowsA)

rowsB = 0
colsB = 0
CALL LOAD_MATRIX("matrixB.dat", matrixB, rowsB, colsB, col_fmtB)
WRITE(*, *) "MATRIX B:"
WRITE(*, col_fmtB) ((matrixB(i, j), j=1, colsB), i=1, rowsB)

IF (colsA .eq. rowsB) THEN
  ! Call matrix multiplication function.
  ALLOCATE(matrixC(1:rowsA, 1:colsB))
  DO i = 1, rowsA
    DO j = 1, colsB
      temp = 0
      DO k = 1, colsA
        temp = temp + matrixA(i, k) * matrixB(k, j)
      END DO
      matrixC(i, j) = temp
    END DO
  END DO
END IF

WRITE(*, *) "MATRIX C:"
WRITE(*, '(3F14.3)') ((matrixC(i, j), j = 1, colsB), i = 1, rowsA)
matrixC = 0.
WRITE(*, *) "MATRIX C:"
WRITE(*, '(3F14.3)') ((matrixC(i, j), j = 1, colsB), i = 1, rowsA)
matrixC = MATMUL(matrixA, matrixB)
WRITE(*, *) "MATRIX C:"
WRITE(*, '(3F14.3)') ((matrixC(i, j), j = 1, colsB), i = 1, rowsA)

CLOSE(1)
END PROGRAM matrix_mult
