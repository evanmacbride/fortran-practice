MODULE types
TYPE :: customer
  CHARACTER(len=12) :: first
  CHARACTER :: middle
  CHARACTER(len=12) :: last
  CHARACTER(len=26) :: street
  CHARACTER(len=12) :: city
  CHARACTER(len=2) :: state
  INTEGER :: zip
END TYPE customer
END MODULE types

SUBROUTINE name_sort(arr, num)
USE types
IMPLICIT NONE
TYPE (customer), DIMENSION(num), INTENT(INOUT) :: arr
INTEGER, INTENT(IN) :: num
INTEGER :: index, start, smallest
TYPE (customer) :: swap
! Find smallest and swap it with the value stored in start
outer: DO start = 1, num
  smallest = start
  find_smallest: DO index = start, num
    IF (LLT(arr(index)%last,arr(smallest)%last)) THEN
      smallest = index
    END IF
  END DO find_smallest
  IF (start /= smallest) THEN
    swap = arr(start)
    arr(start) = arr(smallest)
    arr(smallest) = swap
  END IF
END DO outer
END SUBROUTINE name_sort

SUBROUTINE city_sort(arr, num)
USE types
IMPLICIT NONE
TYPE (customer), DIMENSION(num), INTENT(INOUT) :: arr
INTEGER, INTENT(IN) :: num
INTEGER :: index, start, smallest
TYPE (customer) :: swap
! Find smallest and swap it with the value stored in start
outer: DO start = 1, num
  smallest = start
  find_smallest: DO index = start, num
    IF (LLT(arr(index)%city,arr(smallest)%city)) THEN
      smallest = index
    END IF
  END DO find_smallest
  IF (start /= smallest) THEN
    swap = arr(start)
    arr(start) = arr(smallest)
    arr(smallest) = swap
  END IF
END DO outer
END SUBROUTINE city_sort

SUBROUTINE zip_sort(arr, num)
USE types
IMPLICIT NONE
TYPE (customer), DIMENSION(num), INTENT(INOUT) :: arr
INTEGER, INTENT(IN) :: num
INTEGER :: index, start, smallest
TYPE (customer) :: swap
! Find smallest and swap it with the value stored in start
outer: DO start = 1, num
  smallest = start
  find_smallest: DO index = start, num
    IF (arr(index)%zip < arr(smallest)%zip) THEN
      smallest = index
    END IF
  END DO find_smallest
  IF (start /= smallest) THEN
    swap = arr(start)
    arr(start) = arr(smallest)
    arr(smallest) = swap
  END IF
END DO outer
END SUBROUTINE zip_sort

LOGICAL FUNCTION lt_name(a, b)
USE types
IMPLICIT NONE
TYPE (customer), INTENT(IN) :: a, b
lt_name = LLT(a%last, b%last)
END FUNCTION lt_name

LOGICAL FUNCTION lt_city(a, b)
USE types
IMPLICIT NONE
TYPE (customer), INTENT(IN) :: a, b
lt_city = LLT(a%city, b%city)
END FUNCTION lt_city

LOGICAL FUNCTION lt_zip(a, b)
USE types
IMPLICIT NONE
TYPE (customer), INTENT(IN) :: a, b
lt_zip = a%zip < b%zip
END FUNCTION lt_zip

SUBROUTINE sort_database(arr, num, lt_func)
USE types
IMPLICIT NONE
TYPE (customer), DIMENSION(num), INTENT(INOUT) :: arr
LOGICAL, EXTERNAL :: lt_func
INTEGER, INTENT(IN) :: num
INTEGER :: index, start, smallest
TYPE (customer) :: swap
! Find smallest and swap it with the value stored in start
outer: DO start = 1, num
  smallest = start
  find_smallest: DO index = start, num
    IF (lt_func(arr(index), arr(smallest))) THEN
      smallest = index
    END IF
  END DO find_smallest
  IF (start /= smallest) THEN
    swap = arr(start)
    arr(start) = arr(smallest)
    arr(smallest) = swap
  END IF
END DO outer
END SUBROUTINE sort_database

PROGRAM customer_database
USE types
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_CUSTOMERS = 100
LOGICAL, EXTERNAL :: lt_name, lt_city, lt_zip
TYPE (customer), DIMENSION(MAX_CUSTOMERS) :: customers
INTEGER :: status, i, total, choice
CHARACTER(len=80) :: msg
CHARACTER(len=16) :: filename

WRITE(*, *) "Enter filename for customer database: "
READ(*, *) filename
OPEN(UNIT=1,FILE=filename,STATUS='OLD',ACTION='READ',IOSTAT=status,IOMSG=msg)

total = 0
fileopen: IF (status /= 0) THEN
  WRITE(*, *) msg
ELSE
  DO i = 1, MAX_CUSTOMERS
    READ(1, 100, IOSTAT=status) customers(i)
    100 FORMAT (A12,A1,A12,A26,A12,A3,1X,I5)
    IF (status /= 0) THEN
      !WRITE(*, *) "Exiting on line: ", i
      total = i - 1
      EXIT
    END IF
  END DO

  WRITE(*, '(/,A)') "Choose sorting option:"
  WRITE(*, *) "1                - Sort by customer last name"
  WRITE(*, *) "2                - Sort by customer city"
  WRITE(*, *) "3                - Sort by customer zip code"
  WRITE(*, *) "Any other number - Do not sort"
  READ(*, *) choice
  SELECT CASE (choice)
  CASE (1)
    !CALL name_sort(customers,total)
    CALL sort_database(customers, total, lt_name)
  CASE (2)
    CALL sort_database(customers, total, lt_city)
  CASE (3)
    CALL sort_database(customers, total, lt_zip)
  CASE DEFAULT

  END SELECT
  !CALL name_sort(customers,total)
  WRITE(*, 100) (customers(i), i=1,total)
END IF fileopen
END PROGRAM customer_database
