SUBROUTINE throw(dice_arr, sides, ndice)
IMPLICIT NONE
INTEGER, DIMENSION(2), INTENT(OUT) :: dice_arr
INTEGER, INTENT(IN) :: sides, ndice
INTEGER :: i

DO i = 1, ndice
  dice_arr(i) = die()
END DO

CONTAINS
  INTEGER FUNCTION die()
  die = CEILING(RAND() * sides)
  END FUNCTION die
END SUBROUTINE throw

PROGRAM dice_thrower
IMPLICIT NONE
INTEGER, DIMENSION(3) :: dice
INTEGER :: i
DO i = 1, 10
  CALL throw(dice, 12, 3)
  WRITE(*, '(3I3)') dice(1), dice(2), dice(3)
END DO
END PROGRAM dice_thrower
