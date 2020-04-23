SUBROUTINE running_avg(x, avg, std_dev, nvals, reset)
IMPLICIT NONE

REAL, INTENT(IN) :: x
REAL, INTENT(OUT) :: avg, std_dev
INTEGER, INTENT(OUT) :: nvals
LOGICAL, INTENT(IN) :: reset
INTEGER, SAVE :: n
REAL, SAVE :: sum_x, sum_x2

calc_sums: IF(reset) THEN
  n       = 0
  sum_x   = 0.
  sum_x2  = 0.
  avg     = 0.
  std_dev = 0.
  nvals   = 0
ELSE
  n      = n + 1
  sum_x  = sum_x + x
  sum_x2 = sum_x2 + x**2

  avg = sum_x / REAL(n)

  IF (n >= 2) THEN
    std_dev = SQRT((REAL(n) * sum_x2 - sum_x**2) &
            / (REAL(n) * REAL(n-1)))
  ELSE
    std_dev = 0.
  END IF

  nvals = n
END IF calc_sums
END SUBROUTINE running_avg

PROGRAM test_running_avg
IMPLICIT NONE
INTEGER :: istat, nvals
REAL :: avg, std_dev, x
CHARACTER(len=80) :: msg
CHARACTER(len=20) :: file_name

CALL running_avg(0., avg, std_dev, nvals, .TRUE.)
WRITE(*, *) 'Enter the file name to read data from:'
READ(*, '(A20)') file_name
OPEN(UNIT=21, FILE=file_name, STATUS='OLD', ACTION='READ', &
     IOSTAT=istat, IOMSG=msg)

openok: IF (istat == 0) THEN
  calc: DO
    READ(21, *, IOSTAT=istat) x
    IF (istat /= 0) EXIT
    CALL running_avg(x, avg, std_dev, nvals, .FALSE.)
    WRITE(*, 1020) 'VALUE = ',x,' Avg = ',avg, &
                   '  Std_Dev = ', std_dev, &
                   '  Nvals = ', nvals
    1020 FORMAT (3(A,F10.4),A,I6)
  END DO calc
ELSE openok
  WRITE(*, 1030) msg
  1030 FORMAT ('File open failed: ', A)
END IF openok
END PROGRAM test_running_avg
