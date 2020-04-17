PROGRAM read_n_calc
IMPLICIT NONE
REAL, ALLOCATABLE, DIMENSION(:,:) :: data
CHARACTER(LEN=20) :: filename
INTEGER :: status
INTEGER :: rows, cols, i, j
CHARACTER(LEN=10) :: row_str, col_str
CHARACTER(LEN=40) :: row_sum_fmt, col_sum_fmt

WRITE(*, *) "Enter the filename to read and process:"
READ(*, *) filename

OPEN(1, FILE=filename, STATUS='OLD', ACTION='READ', IOSTAT=status)
READ(1, *) rows, cols
ALLOCATE(data(1:rows, 1:cols))

WRITE(row_str, '(I3)') rows
WRITE(col_str, '(I3)') cols
row_str = TRIM(row_str)
col_str = TRIM(col_str)
row_sum_fmt = '(' // row_str // '(A, I4, 2X, A, F12.1, A))'
col_sum_fmt = '(' // col_str // '(A, I4, 2X, A, F12.1, A))'
row_sum_fmt = TRIM(row_sum_fmt)
col_sum_fmt = TRIM(col_sum_fmt)

DO i = 1, rows
  READ(1, '(' // col_str // 'F8.1)', IOSTAT=status) (data(i, j), j = 1, cols)
  IF (status .lt. 0) THEN
    EXIT
  END IF
END DO

WRITE(*, row_sum_fmt) ("Row:", i, "Sum:", SUM(data(i, 1:cols)), NEW_LINE('A'), i = 1, rows)
WRITE(*, col_sum_fmt) ("Col:", i, "Sum:", SUM(data(1:rows, i)), NEW_LINE('A'), i = 1, cols)
!WRITE(*, row_sum_fmt) ("Row:", i, "Sum:", SUM(data(i, 1:cols)), NEW_LINE('A'), i = 1, rows)
!WRITE(*, col_sum_fmt) ("Col:", i, "Sum:", SUM(data(1:rows, i)), i = 1, cols)

DEALLOCATE(data)
END PROGRAM read_n_calc
