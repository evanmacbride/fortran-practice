PROGRAM timer_test
IMPLICIT NONE
  INTEGER(KIND=8) :: begin_timer, end_timer
  INTEGER :: i
  REAL :: x

  CALL SYSTEM_CLOCK(begin_timer)
  DO i = 1, 100000000
    x = SQRT(REAL(i**2 - 1))
  END DO
  CALL SYSTEM_CLOCK(end_timer)
  WRITE(*, '(F14.6)') REAL(end_timer - begin_timer) / 100000000.
END PROGRAM timer_test
