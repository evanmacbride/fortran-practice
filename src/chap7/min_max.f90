PROGRAM min_max
IMPLICIT NONE
  REAL, EXTERNAL :: func_a, func_b
  REAL :: start, finish, xmin, ymin, xmax, ymax
  INTEGER :: steps
  WRITE(*, *) "Enter start and end values:"
  READ(*, *) start, finish
  WRITE(*, *) "Enter the number of steps:"
  READ(*, *) steps

  CALL find_min_max(start, finish, steps, func_a, xmin, ymin, xmax, ymax)
  WRITE(*, '(A,F10.4,A,F10.4)') "func_a min: ", ymin, " at ", xmin
  WRITE(*, '(A,F10.4,A,F10.4)') "func_a max: ", ymax, " at ", xmax
  CALL find_min_max(start, finish, steps, func_b, xmin, ymin, xmax, ymax)
  WRITE(*, '(A,F10.4,A,F10.4)') "func_b min: ", ymin, " at ", xmin
  WRITE(*, '(A,F10.4,A,F10.4)') "func_b max: ", ymax, " at ", xmax
END PROGRAM min_max

SUBROUTINE find_min_max(first_val, last_val, num_steps, func, xmin, ymin, xmax, ymax)
IMPLICIT NONE
  REAL, INTENT(IN) :: first_val, last_val
  INTEGER, INTENT(IN) :: num_steps
  REAL, EXTERNAL :: func
  REAL, INTENT(OUT) :: xmin, ymin, xmax, ymax
  REAL :: delta, x, y
  INTEGER :: i

  x = first_val
  y = func(x)
  xmin = x
  xmax = x
  ymin = y
  ymax = y
  delta = (last_val - first_val) / REAL(num_steps - 1)
  DO i = 1, num_steps - 1
    x = first_val + i * delta
    y = func(x)
    IF (y .le. ymin) THEN
      ymin = y
      xmin = x
    ELSE IF (y .ge. ymax) THEN
      ymax = y
      xmax = x
    END IF
  END DO
END SUBROUTINE find_min_max

REAL FUNCTION func_a(r)
IMPLICIT NONE
  REAL, INTENT(IN) :: r
  func_a = r**3 - 5 * r**2 + 5 * r + 2
END FUNCTION func_a

REAL FUNCTION func_b(r)
IMPLICIT NONE
  REAL, INTENT(IN) :: r
  func_b = r**2 / 2 - r
END FUNCTION func_b
