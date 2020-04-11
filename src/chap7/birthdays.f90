PROGRAM birthdays
IMPLICIT NONE
  INTEGER :: assign_birthdays
  LOGICAL :: find_match
  REAL :: get_probability

  INTEGER, PARAMETER :: MAX_PEOPLE = 65
  INTEGER, DIMENSION(MAX_PEOPLE) :: birthday_list
  INTEGER :: i
  REAL :: prob

  !WRITE(*, '(2X,A,2X,A)') "People", "Shared B-Day Prob"
  !WRITE(*, '(2X,A,2X,A)') "------", "-----------------"
  DO i = 2, MAX_PEOPLE
    prob = get_probability(birthday_list, i)
    WRITE(*, 100) i, prob
    100 FORMAT (I8, 2X, F17.6)
  END DO

END PROGRAM birthdays

SUBROUTINE assign_birthdays(arr, arr_size)
IMPLICIT NONE
  INTEGER, INTENT(IN) :: arr_size
  INTEGER, DIMENSION(arr_size), INTENT(INOUT) :: arr
  INTEGER :: i

  DO i = 1, arr_size
    arr(i) = CEILING(RAND() * 365)
  END DO
END SUBROUTINE assign_birthdays

LOGICAL FUNCTION find_match(arr, arr_size)
IMPLICIT NONE
  INTEGER, INTENT(IN) :: arr_size
  INTEGER, DIMENSION(arr_size), INTENT(IN) :: arr
  INTEGER :: i, j

  DO i = 1, arr_size
    DO j = i + 1, arr_size
      IF (arr(j) .eq. arr(i)) THEN
        find_match = .true.
        RETURN
      END IF
    END DO
  END DO
  find_match = .false.
END FUNCTION find_match

REAL FUNCTION get_probability(arr, arr_size)
IMPLICIT NONE
  LOGICAL :: find_match

  INTEGER, INTENT(IN) :: arr_size
  INTEGER, DIMENSION(arr_size), INTENT(INOUT) :: arr

  INTEGER, PARAMETER :: NUM_TESTS = 10000
  LOGICAL :: found_match = .false.
  INTEGER :: i, j, matches

  matches = 0
  DO i = 1, NUM_TESTS
    CALL assign_birthdays(arr, arr_size)
    found_match = find_match(arr, arr_size)
    IF (found_match) THEN
      matches = matches + 1
    END IF
  END DO
  !WRITE(*, *) matches
  get_probability = REAL(matches) / REAL(NUM_TESTS)
END FUNCTION get_probability
