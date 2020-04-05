! Do I need to have a module that will let the program and the function share
! a random number? I think this is the same problem I had with my hangman game.

!MODULE share_rand
!IMPLICIT NONE
!SAVE
!  REAL :: r
!END MODULE share_rand

INTEGER FUNCTION roll_die()
!USE share_rand
IMPLICIT NONE
  !INTEGER, INTENT(IN) :: j
  !r = RAND()
  roll_die = CEILING(RAND() * 6)
  !roll_die = j
END FUNCTION roll_die

PROGRAM dice
!USE share_rand
IMPLICIT NONE
  INTEGER :: i, iseed, throw
  INTEGER :: roll_die
  REAL :: r
  WRITE(*, *) "Enter a number to seed the random generator:"
  READ(*, *) iseed

  CALL SRAND(iseed)
  DO i = 1, 10
    r = RAND()
    WRITE(*, '(I4)') CEILING(r * 6)
  END DO

  CALL SRAND(iseed)
  DO i = 1, 10
    !throw = roll_die(2)
    WRITE(*, '(I4)') roll_die()
  END DO
END PROGRAM dice
