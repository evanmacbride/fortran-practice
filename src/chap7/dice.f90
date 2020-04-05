MODULE ran001
IMPLICIT NONE
SAVE
  INTEGER :: n = 9876
END MODULE ran001

PROGRAM dice
IMPLICIT NONE
  INTEGER :: i, iseed, sum = 0, roll
  INTEGER :: roll_die
  REAL :: r, avg
  WRITE(*, *) "Enter a number to seed the random generator:"
  READ(*, *) iseed
  WRITE(*, *) "Rolling the dice..."

  CALL seed(iseed)
  DO i = 1, 10000
    roll = roll_die()
    WRITE(*, '(I4)') roll
    sum = sum + roll
  END DO
  WRITE(*, *) "Roll average: ", REAL(sum) / 10000.0

END PROGRAM dice

INTEGER FUNCTION roll_die()
IMPLICIT NONE
  REAL :: r
  CALL random0(r)
  roll_die = CEILING(r * 6)
END FUNCTION roll_die

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
