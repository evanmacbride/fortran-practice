MODULE tools
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
CONTAINS
  ! Sum n calls of func(x)
  REAL(KIND=DBL) FUNCTION sum_func(func, x, n)
  IMPLICIT NONE
  REAL(KIND=DBL), EXTERNAL :: func
  REAL(KIND=DBL), INTENT(IN) :: x
  INTEGER, INTENT(IN) :: n
  INTEGER :: i
  REAL(KIND=DBL) :: sum
  sum = 0._DBL
  DO i = 1, n
    sum = sum + func(x)
  END DO
  sum_func = sum
  END FUNCTION sum_func

  REAL(KIND=DBL) FUNCTION inc(x)
  IMPLICIT NONE
  INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
  REAL(KIND=DBL), INTENT(IN) :: x
  inc = x + 1._DBL
  END FUNCTION inc
END MODULE tools

!REAL FUNCTION inc(x)
!IMPLICIT NONE
!INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
!REAL(KIND=DBL), INTENT(IN) :: x
!inc = x + 1._DBL
!END FUNCTION inc

! Sum n calls of func(x)
SUBROUTINE sum_func_sub(func, x, n, result)
IMPLICIT NONE
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
REAL(KIND=DBL), EXTERNAL :: func
REAL(KIND=DBL), INTENT(IN) :: x
INTEGER, INTENT(IN) :: n
REAL(KIND=DBL), INTENT(OUT) :: result
INTEGER :: i
REAL(KIND=DBL) :: sum
sum = 0._DBL
DO i = 1, n
  sum = sum + func(x)
END DO
result = sum
END SUBROUTINE sum_func_sub

PROGRAM passing_funcs
USE tools
IMPLICIT NONE
!REAL, EXTERNAL :: inc
REAL(KIND=DBL) :: a, result
a = 1._DBL
WRITE(*, *) "func: ", sum_func(inc, a, 10)
CALL sum_func_sub(inc, a, 10, result)
WRITE(*, *) "sub: ", result
END PROGRAM passing_funcs
