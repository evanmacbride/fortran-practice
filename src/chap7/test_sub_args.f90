SUBROUTINE sub_args(x, y, sub, result)
IMPLICIT NONE
  EXTERNAL :: sub
  REAL, INTENT(IN) :: x, y
  REAL, INTENT(OUT) :: result
  CALL sub(x, y, result)
END SUBROUTINE sub_args

PROGRAM test_sub_args
IMPLICIT NONE
  EXTERNAL :: sum, prod
  REAL :: x, y, result

  WRITE(*, *) "Enter x:"
  READ(*, *) x
  WRITE(*, *) "Enter y:"
  READ(*, *) y

  CALL sub_args(x, y, prod, result)
  WRITE(*, *) "The product is ", result
  CALL sub_args(x, y, sum, result)
  WRITE(*, *) "The sum is ", result
END PROGRAM test_sub_args

SUBROUTINE prod(x, y, result)
IMPLICIT NONE
  REAL, INTENT(IN) :: x, y
  REAL, INTENT(OUT) :: result
  result = x * y
END SUBROUTINE prod

SUBROUTINE sum(x, y, result)
IMPLICIT NONE
  REAL, INTENT(IN) :: x, y
  REAL, INTENT(OUT) :: result
  result = x + y
END SUBROUTINE sum
