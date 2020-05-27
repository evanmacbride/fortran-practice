PROGRAM kinds
IMPLICIT NONE
WRITE(*, '("The KIND for single precision is", I2)') KIND(0.0)
WRITE(*, '("The KIND for double precision is", I2)') KIND(0.0D0)
END PROGRAM kinds
