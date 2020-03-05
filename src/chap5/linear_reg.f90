PROGRAM linear_reg
IMPLICIT NONE
  REAL :: x, y, x_sum = 0, y_sum = 0, xy_sum = 0, x2_sum = 0, x_mean, y_mean, m, b
  INTEGER :: num_records = 0, status
  CHARACTER(80) :: msg
  OPEN(UNIT=1, FILE='coords.dat', STATUS='OLD', ACTION='READ', IOSTAT=status, IOMSG=msg)

  DO
    READ(1, *, IOSTAT=status) x, y
    IF (status .ne. 0) THEN
      EXIT
    ELSE
      x_sum = x_sum + x
      y_sum = y_sum + y
      xy_sum = xy_sum + (x * y)
      x2_sum = x2_sum + (x**2)
      num_records = num_records + 1
    END IF
  END DO

  x_mean = x_sum / REAL(num_records)
  y_mean = y_sum / REAL(num_records)
  m = (xy_sum - x_sum * y_mean) / (x2_sum - x_sum * x_mean)
  b = y_mean - m * x_mean
  WRITE(*, '(A,F9.7,A,F9.7)') "y = ",m, "x + ",b

END PROGRAM linear_reg
