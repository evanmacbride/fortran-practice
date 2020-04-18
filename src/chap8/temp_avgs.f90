PROGRAM temp_avgs
IMPLICIT NONE
CHARACTER(LEN=80) :: msg
INTEGER :: status, i, j
REAL, DIMENSION(6, 6) :: t

OPEN(UNIT=1, FILE="temps.txt", STATUS='OLD', ACTION='READ', &
     IOSTAT=status, IOMSG=msg)
READ(1, *) t
WRITE(*, *) "Temperature Readings:"
WRITE(*, '(6F6.1)') ((t(i, j), j=1, 6), i=1, 6)
WRITE(*, *) "Latitute Averages:"
WRITE(*, '(6F6.1)') (SUM(t(i,1:6)) / 6., i = 1, 6)
WRITE(*, *) "Longitute Averages:"
WRITE(*, '(6F6.1)') (SUM(t(1:6,i)) / 6., i = 1, 6)
CLOSE(1)
END PROGRAM temp_avgs
