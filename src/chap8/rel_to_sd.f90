! Adapt rel_max to show if a cell is more than one std dev less
! or greater than the local mean. Mark the corresponding cell in
! the map as blank (" ") for less, "X" for greater, and "." for
! within one std dev of the local mean.
PROGRAM rel_to_sd
IMPLICIT NONE
REAL, DIMENSION(:,:), ALLOCATABLE :: matrix
INTEGER, DIMENSION(1:2) :: loc
CHARACTER, DIMENSION(:,:), ALLOCATABLE :: max_map
INTEGER :: rows, cols, i, j, k, l
REAL :: std_dev, sum_x, sum_x2, temp, x_bar

OPEN (1, FILE="matrix.dat", STATUS='OLD', ACTION='READ')
READ(1, *) rows, cols
ALLOCATE(matrix(1:rows, 1:cols), max_map(1:rows, 1:cols))
READ(1, *) matrix
WRITE(*, *) "DATA:"
WRITE(*, '(10F12.1)') ((matrix(i, j), j = 1, cols), i = 1, rows)

max_map = '.'
DO i = 2, rows - 1
  DO j = 2, cols - 1
    !loc = MAXLOC(matrix(i-1:i+1,j-1:j+1))
    !IF ((loc(1) .eq. 2) .and. (loc(2)) .eq. 2) THEN
    !  max_map(i, j) = 'X'
    !END IF
    sum_x = 0.
    sum_x2 = 0.
    sum_x = SUM(matrix(i-1:i+1,j-1:j+1))
    FORALL(k=i-1:i+1, l=j-1:j+1)
      sum_x2 = sum_x2 + matrix(k, l)
    END FORALL
    x_bar = sum_x / 9.
    std_dev = SQRT((9. * sum_x2 - sum_x**2) &
            / (9. * 8.))
    IF (matrix(i, j) .lt. (x_bar - std_dev)) THEN
      max_map(i, j) = ' '
    ELSE IF (matrix(i, j) .gt. (x_bar + std_dev)) THEN
      max_map(i, j) = 'X'
    END IF
  END DO
END DO

WRITE(*, *) "MAXIMA LOCATIONS:"
WRITE(*, '(10A12)') ((max_map(i, j), j = 1, cols), i = 1, rows)
END PROGRAM rel_to_sd
