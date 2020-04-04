REAL FUNCTION ave_value(func, first_value, last_value, n)
IMPLICIT NONE
  REAL, EXTERNAL :: func
  REAL, INTENT(IN) :: first_value, last_value
  INTEGER, INTENT(IN) :: n
  REAL :: delta, sum
  INTEGER :: i

  delta = (last_value - first_value) / REAL(n - 1)
  sum = 0
  DO i = 1, n
    sum = sum + func(REAL(i - 1) * delta)
  END DO
  ave_value = sum / REAL(n)
END FUNCTION ave_value

PROGRAM test_ave_value
IMPLICIT NONE
  REAL :: ave_value
  REAL, EXTERNAL :: my_function
  REAL :: ave
  ave = ave_value(my_function, 0.0, 1.0, 1000001)
  WRITE(*, 1000) 'my_function', ave
  1000 FORMAT ('The average value of ', A, ' between 0.0 and 1.0 is ', &
               F16.8,'.')
END PROGRAM test_ave_value

REAL FUNCTION my_function(x)
IMPLICIT NONE
  REAL, INTENT(IN) :: x
  my_function = SIN(X)**2
END FUNCTION my_function
