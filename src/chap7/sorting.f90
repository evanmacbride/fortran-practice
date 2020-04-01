PROGRAM sort
IMPLICIT NONE
  INTEGER, PARAMETER :: MAX_SIZE = 32
  INTEGER, DIMENSION(MAX_SIZE) :: numbers
  INTEGER :: arr_size, smallest, start, swap, status, index = 1
  CHARACTER(LEN = 85) :: msg

  OPEN(UNIT=1, FILE='numbers.txt', STATUS='OLD', ACTION='READ', IOSTAT=status, IOMSG=msg)

  ! Read data from file and load it into numbers array
  DO
    READ(1,*,IOSTAT=status) numbers(index)
    IF (status .ne. 0) THEN
      ! Get upper bound of array
      arr_size = index - 1
      EXIT
    END IF
    index = index + 1
  END DO

  CALL sort_algorithm(arr_size, numbers)

  ! Print out array
  WRITE(*, 100) (numbers(index), index = 1, arr_size)
  100 FORMAT (1I4)

  CLOSE(1)

END PROGRAM sort

SUBROUTINE sort_algorithm(size, nums)
IMPLICIT NONE
  INTEGER, INTENT(IN) :: size
  INTEGER, DIMENSION(size), INTENT(INOUT) :: nums
  INTEGER :: start, index, swap, smallest

  ! Find smallest and swap it with the value stored in start
  outer: DO start = 1, size
    smallest = start
    find_smallest: DO index = start, size
      IF (nums(index) < nums(smallest)) THEN
        smallest = index
      END IF
    END DO find_smallest
    IF (start .ne. smallest) THEN
      swap = nums(start)
      nums(start) = nums(smallest)
      nums(smallest) = swap
    END IF
  END DO outer

END SUBROUTINE sort_algorithm
