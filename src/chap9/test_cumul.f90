MODULE funcs
CONTAINS
  IMPURE ELEMENTAL REAL FUNCTION cumul(a, sum)
  IMPLICIT NONE
  REAL, INTENT(IN) :: a
  REAL, INTENT(INOUT) :: sum
  sum = sum + a
  cumul = sum
  END FUNCTION
END MODULE funcs

PROGRAM test_cumul
USE funcs
IMPLICIT NONE
REAL, DIMENSION(5) :: a, b
REAL :: sum
INTEGER :: i
sum = 0.
a = [1., 2., 3., 4., 5.]
b = cumul(a, sum)
WRITE(*, '(5F6.1)') a
WRITE(*, '(5F6.1)') b
END PROGRAM test_cumul
