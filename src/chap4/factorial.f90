PROGRAM factorial
IMPLICIT NONE
  INTEGER :: index, n, result = 1 ! User input n and output result for factorial function
  WRITE(*, *) "Enter a non-negative integer to compute its factorial:"
  READ(*, *) n
  IF (n .lt. 0) THEN
    WRITE(*, *) "You must enter a non-negative integer."
  ELSE
    DO index = 0, n - 1
      result = result * (n - index)
    END DO
    WRITE(*, *) "Factorial of ", n, " = ", result
  END IF
END PROGRAM factorial
