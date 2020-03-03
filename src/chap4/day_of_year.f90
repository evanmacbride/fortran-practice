PROGRAM day_of_year
IMPLICIT NONE
  INTEGER :: year, month, day ! User inputs
  INTEGER :: index
  INTEGER :: month_lengths(12)
  LOGICAL :: is_leap
  INTEGER :: result = 0

  WRITE(*, *) "Enter today's numerical date using format YYYY M D"
  READ(*, *) year, month, day
  IF (MOD(year, 400) .eq. 0) THEN
    is_leap = .true.
  ELSE IF (MOD(year, 100) .eq. 0) THEN
    is_leap = .false.
  ELSE IF (MOD(year, 4) .eq. 0) THEN
    is_leap = .true.
  ELSE
    is_leap = .false.
  END IF

  IF (is_leap) THEN
    month_lengths = (/ 31,29,31,30,31,30,31,31,30,31,30,31 /)
  ELSE
    month_lengths = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  END IF

  result = day
  DO index = 1, month - 1
      result = result + month_lengths(index)
  END DO
  WRITE(*, *) result
END PROGRAM day_of_year
