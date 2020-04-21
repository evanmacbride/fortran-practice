! Adapt rel_max to show if a cell is more than one std dev less
! or greater than the local mean. Mark the corresponding cell in
! the map as blank (" ") for less, "X" for greater, and "." for
! within one std dev of the local mean.
PROGRAM rel_max
IMPLICIT NONE
REAL, DIMENSION(:,:), ALLOCATABLE :: matrix
INTEGER, DIMENSION(1:2) :: loc
CHARACTER, DIMENSION(:,:), ALLOCATABLE :: max_map
INTEGER :: rows, cols, i, j

OPEN (1, FILE="matrix.dat", STATUS='OLD', ACTION='READ')
READ(1, *) rows, cols
ALLOCATE(matrix(1:rows, 1:cols), max_map(1:rows, 1:cols))
READ(1, *) matrix
WRITE(*, *) "DATA:"
WRITE(*, '(10F12.1)') ((matrix(i, j), j = 1, cols), i = 1, rows)

max_map = '_'
DO i = 2, rows - 1
  DO j = 2, cols - 1
    loc = MAXLOC(matrix(i-1:i+1,j-1:j+1))
    !WRITE(*, *) loc
    !WRITE(*, '(8F12.1)') MAXVAL(matrix(i-1:i+1,j-1:j+1))
    !WRITE(*, *) i, loc(1),  j .eq. loc(2)
    IF ((loc(1) .eq. 2) .and. (loc(2)) .eq. 2) THEN
      max_map(i, j) = 'X'
    END IF
  END DO
END DO

WRITE(*, *) "MAXIMA LOCATIONS:"
WRITE(*, '(10A12)') ((max_map(i, j), j = 1, cols), i = 1, rows)
END PROGRAM rel_max
