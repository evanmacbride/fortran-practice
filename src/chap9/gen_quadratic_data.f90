! Write a 100x100 matrix of random real numbers to a file.
PROGRAM gen_quadratic_data
IMPLICIT NONE
  INTEGER, PARAMETER :: MAX = 50!, COLS = 100
  INTEGER, DIMENSION(8) :: time
  INTEGER :: i!, j
  REAL :: x0, x1, x2, x, y, rand_range = 4.
  x0 = 3.
  x1 = -4.
  x2 = 1.
  !CHARACTER(LEN=3) :: col_str
  !CHARACTER(LEN=9) :: fmt

  CALL DATE_AND_TIME(VALUES=time)
  CALL SRAND(time(8))

  !WRITE(col_str, '(I3)') COLS
  !col_str = TRIM(col_str)
  !fmt = '(' // col_str // 'F8.1)'

  OPEN (1, FILE='data.txt', STATUS='UNKNOWN', ACTION='WRITE')
  WRITE(1,'(2I5)') MAX + 1!, COLS
  DO i = 0, MAX
    x = i / 10.
    y = x2 * x**2 + x1 * x + x0
    y = y + (RAND() * rand_range - rand_range / 2.)
    WRITE(1, '(2F9.3)') x, y
  END DO
  CLOSE(1)
END PROGRAM gen_quadratic_data
