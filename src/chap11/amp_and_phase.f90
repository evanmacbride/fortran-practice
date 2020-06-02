SUBROUTINE calc_amp_phase(c, amp, theta)
COMPLEX, INTENT(IN) :: c
REAL, INTENT(OUT) :: amp, theta
REAL :: r, i
r = REAL(c)
i = AIMAG(c)
amp = CABS(c)
theta = ATAN2D(r, i)
END SUBROUTINE calc_amp_phase

PROGRAM amp_and_phase
IMPLICIT NONE
COMPLEX :: c
REAL :: amp, theta
WRITE(*, *) "Enter a complex number: "
READ(*, *) c
CALL calc_amp_phase(c, amp, theta)
WRITE(*, *) c, amp, theta
END PROGRAM amp_and_phase
