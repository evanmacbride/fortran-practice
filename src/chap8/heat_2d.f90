PROGRAM heat_2d
IMPLICIT NONE
REAL, PARAMETER :: COOL = 20., HOT = 100., DEFAULT = 50.
REAL, PARAMETER :: LIMIT = 1.0E-32
INTEGER, PARAMETER :: MAX_ROWS = 100, MAX_COLS = 100
REAL, DIMENSION(MAX_ROWS,MAX_COLS) :: plate
REAL :: prev, new
INTEGER :: i, j, steps, same
LOGICAL :: done

! Initialize values
plate = DEFAULT
plate(1,1:MAX_COLS) = COOL
plate(MAX_ROWS,1:MAX_COLS) = COOL
plate(1:MAX_ROWS,1) = COOL
plate(1:MAX_ROWS,MAX_COLS) = COOL
plate(3,8) = HOT
WRITE(*, *) "INITIAL TEMPS (C):"
!WRITE(*, '(10F11.3)') ((plate(i, j), j = 1, MAX_COLS), i = 1, MAX_ROWS)

done = .false.
steps = 0
DO
  IF (done) THEN
    EXIT
  END IF
  same = 0
  DO i = 2, MAX_ROWS - 1
    IF (done) THEN
      EXIT
    END IF
    DO j = 2, MAX_COLS - 1
      IF (i .eq. 3 .and. j .eq. 8) THEN
        CYCLE
      ELSE
        prev = plate(i, j)
        new = 0.25 * (plate(i+1, j) + plate(i-1, j) + plate(i, j+1) + plate(i, j-1))
        plate(i, j) = new
        !WRITE(*, *) prev, new, i, j
        IF (ABS(new - prev) .le. LIMIT) THEN
          same = same + 1
          !WRITE(*, *) prev, new, i, j
          !done = .true.
        !  EXIT
        END IF
      END  IF
    END DO
  END DO
  steps = steps + 1
  IF (same .ge. (MAX_ROWS - 2) * (MAX_COLS - 2) - 1) THEN
    done = .true.
  END IF
END DO

WRITE(*, '(I6,1X,A)') steps, "STEPS. FINAL TEMPS (C):"
!WRITE(*, '(10F11.3)') ((plate(i, j), j = 1, MAX_COLS), i = 1, MAX_ROWS)

END PROGRAM heat_2d
