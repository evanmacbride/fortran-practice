MODULE polar_tools
IMPLICIT NONE

TYPE :: polar
  REAL :: mag
  REAL :: angle
CONTAINS
  PROCEDURE, PASS :: mult
  PROCEDURE, PASS :: div
END TYPE polar

CONTAINS
  TYPE (polar) FUNCTION mult(this, p)
  IMPLICIT NONE
  CLASS (polar), INTENT(IN) :: this, p
  mult = polar(this%mag * p%mag, this%angle + p%angle)
  END FUNCTION mult

  TYPE (polar) FUNCTION div(this, p)
  IMPLICIT NONE
  CLASS (polar), INTENT(IN) :: this, p
  div = polar((this%mag / p%mag), this%angle - p%angle)
  END FUNCTION div
END MODULE polar_tools

PROGRAM bound_polar
USE polar_tools
IMPLICIT NONE
TYPE (polar) :: p1, p2
p1 = polar(5., 3.)
p2 = polar(0.25, 2.)
WRITE(*, *) p1%mult(p2)
WRITE(*, *) p1%div(p2)
WRITE(*, *) p2%div(p1)
WRITE(*, *) p1%div(p1)
END PROGRAM bound_polar
