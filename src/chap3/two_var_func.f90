PROGRAM two_var_func
IMPLICIT NONE
  REAL:: x, y ! User inputs for f(x,y)
  REAL :: result ! The output of f(x,y)
  WRITE(*, *) "Enter real number values for the variables x and y."
  READ(*, *) x, y

  IF ((x .GE. 0) .AND. (y .GE. 0)) THEN
    result = x + y
    WRITE(*, *) "f(x,y) = x + y"
  ELSE IF ((x .GE. 0) .AND. (y .LT. 0)) THEN
    result = x + y**2
    WRITE(*, *) "f(x,y) = x + y^2"
  ELSE IF ((x .LT. 0) .AND. (y .GE. 0)) THEN
    result = x**2 + y
    WRITE(*, *) "f(x,y) = x^2 + y"
  ELSE IF ((x .LT. 0) .AND. (y .LT. 0)) THEN
    result = x**2 + y**2
    WRITE(*, *) "f(x,y) = x^2 + y^2"
  END IF

  WRITE(*, *) "x = ", x, " y = ", y
  WRITE(*, *) "f(x,y) = ", result
  
END PROGRAM two_var_func
