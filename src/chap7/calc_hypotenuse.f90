SUBROUTINE calc_hypotenuse(side_1, side_2, hypotenuse)
IMPLICIT NONE
  REAL, INTENT(IN) :: side_1, side_2
  REAL, INTENT(OUT) :: hypotenuse
  REAL :: temp

  temp = side_1**2 + side_2**2
  hypotenuse = SQRT(temp)

END SUBROUTINE calc_hypotenuse
