INCLUDE 'calc_hypotenuse.f90'

PROGRAM test_hypotenuse
IMPLICIT NONE
  REAL :: s1, s2, hypo
  WRITE(*, *) "Testing subroutine calc_hypotenuse."
  WRITE(*, *) "Enter length of side 1: "
  READ(*, *) s1
  WRITE(*, *) "Enter length of side 2:"
  READ(*, *) s2
  CALL calc_hypotenuse(s1, s2, hypo)
  WRITE(*, 1000) hypo
  1000 FORMAT ('The length of the hypotenuse is ', F10.4)

END PROGRAM test_hypotenuse
