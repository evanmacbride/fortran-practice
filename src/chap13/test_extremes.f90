MODULE procs
CONTAINS
  SUBROUTINE extremes(a, n, maxval, pos_maxval, minval, pos_minval)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(IN), DIMENSION(n) :: a
  REAL, INTENT(OUT), OPTIONAL :: maxval
  INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval
  REAL, INTENT(OUT), OPTIONAL :: minval
  INTEGER, INTENT(OUT), OPTIONAL :: pos_minval
  INTEGER :: i
  REAL :: real_max
  INTEGER :: pos_max
  REAL :: real_min
  INTEGER :: pos_min

  real_max = a(1)
  pos_max = 1
  real_min = a(1)
  pos_min = 1
  DO i = 2, n
    IF (a(i) > real_max) THEN
      real_max = a(i)
      pos_max = i
    ELSE IF (a(i) < real_min) THEN
      real_min = a(i)
      pos_min = i
    END IF
  END DO

  IF (PRESENT(maxval)) THEN
    maxval = real_max
  END IF
  IF (PRESENT(pos_maxval)) THEN
    pos_maxval = pos_max
  END IF
  IF (PRESENT(minval)) THEN
    minval =  real_min
  END IF
  IF (PRESENT(pos_minval)) THEN
    pos_minval = pos_min
  END IF

  END SUBROUTINE extremes
END MODULE procs

PROGRAM test_extremes
USE procs
IMPLICIT NONE
REAL, DIMENSION(5) :: arr
REAL :: arr_max, arr_min
INTEGER :: maxp, minp
arr = [3.,-0.1,17.5,0.,-4.5]
CALL extremes(arr, SIZE(arr), pos_maxval=maxp, minval=arr_min, maxval=arr_max, &
              pos_minval=minp)
WRITE(*, *) maxp, arr_max, minp, arr_min
END PROGRAM test_extremes
