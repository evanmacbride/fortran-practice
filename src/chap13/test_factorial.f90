RECURSIVE SUBROUTINE factorial(n, result)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
INTEGER, INTENT(OUT) :: result
INTEGER :: temp

IF (n >= 1) THEN
  CALL factorial(n-1, temp)
  result = n * temp
ELSE
  result = 1
END IF
END SUBROUTINE factorial

RECURSIVE FUNCTION fact(n) RESULT(answer)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
INTEGER :: answer
IF (n >= 1) THEN
  answer = n * fact(n-1)
ELSE
  answer = 1
END IF
END FUNCTION fact

PROGRAM test_factorial
IMPLICIT NONE
INTEGER :: i, facto
INTEGER :: fact
WRITE(*, *) "Enter an integer to calculate its factorial:"
READ(*, *) i
CALL factorial(i, facto)
WRITE(*, '(I0,A,I0)') i, "! = ", facto
WRITE(*, '(I0,A,I0)') i, "! = ", fact(i)
END PROGRAM test_factorial
