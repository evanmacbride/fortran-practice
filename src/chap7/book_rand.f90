MODULE ran001
IMPLICIT NONE
SAVE
  INTEGER :: n = 9876
END MODULE ran001

PROGRAM book_rand
  USE ran001
  IMPLICIT NONE
  INTEGER :: i, j, iseed
  REAL :: r, avg, sum

  WRITE(*, *) "Ten random numbers produced by random0:"
  DO i = 1, 10
    CALL random0(r)
    WRITE(*, 100) r
  END DO

  DO i = 1, 5
    WRITE(*, *) "Seed random0 with an integer: "
    READ(*, *) iseed
    CALL seed(iseed)
    avg = 0.0
    sum = 0.0
    DO j = 1, 10000
      CALL random0(r)
      sum = sum + r
    END DO
    avg = sum / 10000.0
    WRITE(*, 100) avg
    100 FORMAT (F12.6)
  END DO
END PROGRAM book_rand

SUBROUTINE random0(ran)
USE ran001
IMPLICIT NONE
  REAL, INTENT(OUT) :: ran
  n = MOD(8121 * n + 28411, 134456)
  ran = REAL(n) / 134456.
END SUBROUTINE random0

SUBROUTINE seed(iseed)
USE ran001
IMPLICIT NONE
  INTEGER, INTENT(IN) :: iseed
  n = ABS(iseed)
END SUBROUTINE seed
