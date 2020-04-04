REAL FUNCTION sinc(x)
IMPLICIT NONE
  REAL, INTENT(IN) :: x
  REAL, PARAMETER :: EPSILON = 1.0E-30
  IF (ABS(x) > EPSILON) THEN
    sinc = SIN(x) / x
  ELSE
    sinc = 1
  END IF
END FUNCTION sinc

PROGRAM test_sinc
IMPLICIT NONE
  REAL :: x, sinc
  INTEGER :: i, num_results, offset, adj

  WRITE(*, *) "We will calculate sinc over a range of positive and negative values."
  WRITE(*, *) "How many results would you like to see?"
  READ(*, *) num_results

  offset = num_results / 2
  IF (MOD(num_results, 2) .eq. 0) THEN
    adj = 0
  ELSE
    adj = 1
  END IF

  DO i = 1, num_results
    WRITE(*, '(F9.6)') sinc(REAL(i - adj - offset))
  END DO
  !WRITE(*, 100) "sinc(", x, ") = ", sinc(x)
  !100 FORMAT (A, F12.6, A, F12.6)
END PROGRAM test_sinc
