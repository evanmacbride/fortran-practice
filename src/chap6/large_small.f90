PROGRAM large_small
IMPLICIT NONE
  REAL, DIMENSION(100) :: values
  REAL :: largest = 0, smallest = 0
  INTEGER :: index, finish

  WRITE(*, *) "How many numbers will be entered?"
  READ(*, *) finish

  DO index = 1, finish
    WRITE(*, *) "Enter a number:"
    READ(*, *) values(index)
  END DO

  DO index = 1, finish
    IF (values(index) > largest) THEN
      largest = values(index)
    ELSE IF (values(index) < smallest) THEN
      smallest = values(index)
    END IF
  END DO

  WRITE(*, 100) largest, smallest
  100 FORMAT ("Largest: ", F11.4, 2X, "Smallest: ", F11.4)
END PROGRAM large_small
