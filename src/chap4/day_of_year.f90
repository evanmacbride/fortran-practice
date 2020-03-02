PROGRAM day_of_year
IMPLICIT NONE
  INTEGER :: year, month, day ! User inputs
  LOGICAL :: is_leap
  INTEGER :: result = 0
  WRITE(*, *) "Enter today's date using format 2020 11 30"
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
  WRITE(*, *) is_leap
END PROGRAM day_of_year
