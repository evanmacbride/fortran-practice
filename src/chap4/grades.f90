PROGRAM grades
IMPLICIT NONE
  REAL :: x = 0.0, n = 0.0, sum_x = 0.0, sum_x2 = 0.0, avg, std_dev
  DO
    WRITE(*, *) "Enter a numerical grade (0 - 100) to continue."
    WRITE(*, *) "Enter a negative number to quit."
    READ(*, *) x
    IF (x < 0.0) THEN
      EXIT
    END IF
    sum_x = sum_x + x
    sum_x2 = sum_x2 + x**2
    n = n + 1.0
  END DO

  avg = sum_x / n
  std_dev = SQRT((n*sum_x2 - sum_x**2) / (n * (n - 1)))

  WRITE(*, *) "Average: ", avg, "Std. Dev.: ", std_dev
END PROGRAM grades
