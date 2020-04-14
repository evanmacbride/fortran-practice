PROGRAM stats_5
IMPLICIT NONE
REAL, ALLOCATABLE, DIMENSION(:) :: a
CHARACTER(len=20) :: filename
INTEGER :: i, iptr, j, status
REAL :: median, std_dev, sum_x = 0., sum_x2 = 0., temp, x_bar
CHARACTER(len=80) :: msg
INTEGER :: nvals = 0

WRITE(*, 1000)
1000 FORMAT ('Enter the filename with the data to be sorted:')
READ(*, '(A20)') filename

OPEN(UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
     IOSTAT=status, IOMSG=msg)

fileopen: IF (status == 0) THEN
  DO
    READ(9, *, IOSTAT=status) temp
    IF (status /= 0) EXIT
    nvals = nvals + 1
  END DO

  WRITE(*, '(A,I12)') 'Allocating a: size = ', nvals
  ALLOCATE (a(nvals), STAT=status)

  allocate_ok: IF (status == 0) THEN
    REWIND(UNIT=9)
    READ(9, *) a
    outer: DO i = 1, nvals - 1
      iptr = i
      inner: DO j = i + 1, nvals
        minval: IF (a(j) < a(iptr)) THEN
          iptr = j
        END IF minval
      END DO inner

      swap: IF (i /= iptr) THEN
        temp = a(i)
        a(i) = a(iptr)
        a(iptr) = temp
      END IF swap
    END DO outer

    sums: DO i = 1, nvals
      sum_x = sum_x + a(i)
      sum_x2 = sum_x2 + a(i)**2
    END DO sums

    enough: IF (nvals < 2) THEN
      WRITE(*, *) 'At least 2 values must be entered.'
    ELSE
      x_bar = sum_x / REAL(nvals)
      std_dev = SQRT((REAL(nvals) * sum_x2 - sum_x**2) &
              / (REAL(nvals) * real(nvals - 1)))
      even: IF (MOD(nvals, 2) == 0) THEN
        median = (a(nvals/2) + a(nvals/2+1)) / 2.
      ELSE
        median = a(nvals/2+1)
      END IF even
      WRITE(*, '(A,F8.3)') 'MEAN:    ', x_bar
      WRITE(*, '(A,F8.3)') 'MEDIAN:  ', median
      WRITE(*, '(A,F8.3)') 'STD_DEV: ', std_dev
      WRITE(*, '(A,I8)')   'N:       ', nvals
    END IF enough
    DEALLOCATE(a, STAT=status)
  END IF allocate_ok
ELSE fileopen
  WRITE(*, 1050) TRIM(msg)
  1050 FORMAT ('File open failed. Status: ', A)
END IF fileopen
END PROGRAM stats_5
