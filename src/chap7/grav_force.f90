PROGRAM grav_force
IMPLICIT NONE
  REAL :: calc_grav
  WRITE(*, *) calc_grav(5.98E24, 1000., 38000000.)

END PROGRAM grav_force

REAL FUNCTION calc_grav(m1, m2, r)
IMPLICIT NONE
  REAL, INTENT(IN) :: m1, m2, r
  REAL, PARAMETER :: G = 6.672E-11
  calc_grav = (G * m1 * m2) / r**2
END FUNCTION calc_grav
