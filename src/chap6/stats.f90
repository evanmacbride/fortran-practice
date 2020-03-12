PROGRAM stats
IMPLICIT NONE
  INTEGER, PARAMETER :: MAX_SIZE = 32
  INTEGER, DIMENSION(MAX_SIZE) :: numbers
  INTEGER :: arr_size, smallest, start, swap, status, index = 1, diff = 0
  REAL :: median, mean, std_dev, x_sum = 0.0, diff2_sum = 0.0
  CHARACTER(LEN = 85) :: msg

  OPEN(UNIT=1, FILE='numbers.txt', STATUS='OLD', ACTION='READ', IOSTAT=status, IOMSG=msg)

  ! Read data from file and load it into numbers array. Calculate mean and
  ! std_dev in the process
  DO
    READ(1,*,IOSTAT=status) numbers(index)
    x_sum = x_sum + numbers(index)
    IF (status .ne. 0) THEN
      ! Get upper bound of array
      arr_size = index - 1
      EXIT
    END IF
    index = index + 1
  END DO

  mean = x_sum / arr_size
  WRITE(*, *) "Mean: ", mean

  DO index = 1, arr_size
    diff = numbers(index) - mean
    diff2_sum = diff2_sum + diff**2
  END DO

  std_dev = SQRT(diff2_sum/(arr_size - 1))
  WRITE(*, *) "Standard Deviation: ", std_dev

  ! Find smallest and swap it with the value stored in start
  outer: DO start = 1, arr_size
    smallest = start
    find_smallest: DO index = start, arr_size
      IF (numbers(index) < numbers(smallest)) THEN
        smallest = index
      END IF
    END DO find_smallest
    IF (start .ne. smallest) THEN
      swap = numbers(start)
      numbers(start) = numbers(smallest)
      numbers(smallest) = swap
    END IF
  END DO outer

  ! Calculate the median and print it
  IF (MODULO(arr_size,2) .ne. 0) THEN
    median = REAL(numbers(index/2))
    WRITE(*, *) "Median: ", INT(median)
  ELSE
    median = REAL(numbers(index/2) + numbers(index/2 + 1)) / 2.0
    WRITE(*, *) "Near median: ", numbers(index/2), numbers(index/2 + 1)
    WRITE(*, *) "Median: ", median
  END IF


  CLOSE(1)

END PROGRAM stats
