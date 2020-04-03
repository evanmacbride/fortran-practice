MODULE my_random
IMPLICIT NONE
SAVE
  INTEGER, PARAMETER :: A = 8121, B = 28411, C = 134456
  INTEGER :: rand_int
  REAL :: rand_real

END MODULE my_random

PROGRAM random_test
USE my_random
IMPLICIT NONE
  INTEGER :: i
  rand_int = 1
  DO i = 1, 10
    CALL rando
    WRITE(*, 100) rand_real
    100 FORMAT (F10.4)
  END DO
END PROGRAM random_test

SUBROUTINE rando
USE my_random
IMPLICIT NONE
  rand_int = MOD(A * rand_int + B, C)
  rand_real = REAL(rand_int) / REAL(C)

END SUBROUTINE rando
