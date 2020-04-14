PROGRAM read_generators
IMPLICIT NONE
  INTEGER, PARAMETER :: ROWS = 4, COLS = 6

  REAL, DIMENSION(COLS) :: get_col_totals

  REAL, DIMENSION(ROWS, COLS) :: gens
  REAL, DIMENSION(ROWS) :: avgs
  REAL, DIMENSION(COLS) :: totals
  REAL :: g_sum, t_sum
  INTEGER :: i, j
  OPEN (1, FILE='gendat.txt', STATUS='OLD', ACTION='READ')
  READ(1, *) gens

  ! Get the total power for each time
  DO i = 1, COLS
    t_sum = 0.0
    DO j = 1, ROWS
      t_sum = t_sum + gens(j, i)
    END DO
    totals(i) = t_sum
  END DO

  ! Get average power of each generator
  DO i = 1, ROWS
    g_sum = 0.0
    DO j = 1, COLS
      g_sum = g_sum + gens(i, j)
    END DO
    avgs(i) = g_sum / COLS
  END DO
  WRITE(*, '(A, 6F8.2)') "TOTALS:", totals
  WRITE(*, '(A, 4F8.2)') "AVGS:  ", avgs
  !WRITE(*, '(6F5.1)') ((gens(i, j), j=1,COLS), i=1,ROWS)
  !WRITE(*, *) get_col_totals(ROWS, COLS, gens)
END PROGRAM read_generators

FUNCTION get_col_totals(r, c, arr)
  INTEGER, INTENT(IN) :: r, c
  REAL, DIMENSION(r, c), INTENT(IN) :: arr
  REAL, DIMENSION(c) :: totals
  REAL, DIMENSION(c) :: get_col_totals
  totals = 1
  get_col_totals = totals
END FUNCTION get_col_totals
