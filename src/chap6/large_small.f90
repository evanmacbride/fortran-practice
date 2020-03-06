PROGRAM large_small
IMPLICIT NONE
  INTEGER, PARAMETER :: ARR_SIZE = 100
  REAL, DIMENSION(ARR_SIZE) :: input_vals
  REAL :: largest = 0, smallest = 0
  INTEGER :: index, num_vals

  WRITE(*, *) "How many numbers will be entered?"
  READ(*, *) num_vals

  DO index = 1, num_vals
    WRITE(*, *) "Enter a number:"
    READ(*, *) input_vals(index)
  END DO

  DO index = 1, num_vals
    IF (input_vals(index) > largest) THEN
      largest = input_vals(index)
    ELSE IF (input_vals(index) < smallest) THEN
      smallest = input_vals(index)
    END IF
  END DO

  DO index = 1, num_vals
    IF (input_vals(index) == largest) THEN
      WRITE(*, 100) input_vals(index), "LARGEST"
    ELSE IF (input_vals(index) == smallest) THEN
      WRITE(*, 100) input_vals(index), "SMALLEST"
    ELSE
      WRITE(*, 100) input_vals(index)
    END IF
    100 FORMAT (2X, F11.4, 2X, A8)
  END DO

END PROGRAM large_small
