PROGRAM cases
IMPLICIT NONE
  INTEGER :: temp ! Temperature in degrees Celsius
  WRITE(*, *) "Enter the temperature in degrees Celsius"
  READ(*, *) temp
  WRITE(*, *) temp, " degrees Celsius:"
  weather: SELECT CASE (temp)
  CASE(:-1)
    WRITE(*, *) "Below freezin'"
  CASE(0)
    WRITE(*, *) "Exactly freezin'"
  CASE DEFAULT
    WRITE(*, *) "Probably not freezin'"
  END SELECT weather
END PROGRAM cases
