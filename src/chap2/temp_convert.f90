PROGRAM temp_convert
IMPLICIT NONE
  REAL :: temp_f, temp_k
  WRITE(*, *) "Please enter the temperature in Fahrenheit"
  READ(*, *)  temp_f
  temp_k = (5. / 9.) * (temp_f - 32.) + 273.15
  WRITE(*, *) temp_f, " degrees Fahrenheit is ", temp_k, " degrees Kelvin"
END PROGRAM temp_convert
