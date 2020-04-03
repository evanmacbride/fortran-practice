PROGRAM bounds
IMPLICIT NONE
  INTEGER :: i
  REAL, DIMENSION(5) :: a = 0

  CALL sub1(a, 5, 6)

  DO i = 1, 6
    WRITE(*, 100) i, a(i)
    100 FORMAT ('A(',I1, ') = ', F6.2)
  END DO

END PROGRAM bounds

SUBROUTINE sub1(a, ndim, n)
IMPLICIT NONE
  INTEGER, INTENT(IN) :: ndim
  REAL, INTENT(OUT), DIMENSION(ndim) :: a
  INTEGER, INTENT(IN) :: n
  INTEGER :: i

  DO i = 1, n
    a(i) = i
  END DO

END SUBROUTINE sub1
