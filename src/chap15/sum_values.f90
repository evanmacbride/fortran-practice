MODULE my_sub
IMPLICIT NONE
CONTAINS
  SUBROUTINE running_sum(sum, value)
  IMPLICIT NONE
  !REAL, POINTER :: sum, value
  REAL :: sum, value
  !ALLOCATE(sum)
  sum = sum + value
  END SUBROUTINE running_sum
END MODULE my_sub

PROGRAM sum_values
USE my_sub
IMPLICIT NONE
INTEGER :: istat
!REAL, POINTER :: sum, value
REAL :: sum, value
!ALLOCATE(sum, value, STAT=istat)
WRITE(*, *) 'Enter values to add: '
DO
  READ(*, *, IOSTAT=istat) value
  IF (istat /= 0) EXIT
  CALL running_sum(sum, value)
  WRITE(*, *) ' The sum is ', sum
END DO
END PROGRAM sum_values
