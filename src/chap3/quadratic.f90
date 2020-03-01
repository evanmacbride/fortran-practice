PROGRAM quadratic
IMPLICIT NONE
  REAL :: a, b, c ! The coefficients of a quadratic equation ax**2 + bx + c = 0
  REAL :: discriminant
  REAL :: rootA, rootB
  REAL :: real_part, imag_part
  WRITE(*, *) "Enter the coefficients a, b, c of a quadratic equation."
  READ(*, *) a, b, c
  discriminant = b**2 - 4.0 * a * c

  WRITE(*, *) "Coefficients: ", a, b, c
  WRITE(*, *) "Discriminant: ", discriminant
  IF (discriminant < 0.0) THEN
    real_part = (-b) / (2.0 * a)
    imag_part = SQRT(ABS(discriminant)) / (2.0 * a)
    WRITE(*, *) "The equation has complex roots."
    WRITE(*, *) "x = ", real_part, " + i", imag_part
    WRITE(*, *) "x = ", real_part, " - i", imag_part
  ELSE IF (discriminant > 0.0) THEN
    rootA = (-b + SQRT(discriminant)) / (2.0 * a)
    rootB = (-b - SQRT(discriminant)) / (2.0 * a)
    WRITE(*, *) "The equation has two distinct real roots."
    WRITE(*, *) "x = ", rootA
    WRITE(*, *) "x = ", rootB
  ELSE
    rootA = (-b) / (2.0 * a)
    WRITE(*, *) "The equation has two identical real roots."
    WRITE(*, *) "x = ", rootA
  END IF

END PROGRAM quadratic
