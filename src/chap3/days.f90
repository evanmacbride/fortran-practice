PROGRAM days
IMPLICIT NONE
  CHARACTER(LEN = 11) :: day  ! User input for day of the week
  !CHARACTER(LEN = 9) :: weekdays(5)
  !CHARACTER(LEN = 8) :: weekends(2)
  CHARACTER(LEN = 11) :: type ! Type of day (i.e. weekday or weekend)

  !weekdays = (/ "Monday   ", "Tuesday  ", "Wednesday", "Thursday ", "Friday   "/)
  !weekends = (/ "Saturday", "Sunday  "/)
  WRITE(*, *) "What day of the week is it today?"
  READ(*, *) day
  SELECT CASE (day)
  CASE("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    type = "Weekday"
  CASE("Saturday","Sunday")
    type = "Weekend"
  CASE DEFAULT
    type = "Invalid day"
  END SELECT
  WRITE(*, *) day, " is a ", type
  !WRITE(*, *) weekdays

END PROGRAM days
