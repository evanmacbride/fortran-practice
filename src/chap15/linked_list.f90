PROGRAM linked_list
IMPLICIT NONE
TYPE :: real_value
  REAL :: value
  TYPE (real_value), POINTER :: p
END TYPE real_value

TYPE (real_value), POINTER :: head => NULL()
CHARACTER(len=20) :: filename
INTEGER :: nvals = 0
TYPE (real_value), POINTER :: ptr => NULL()
TYPE (real_value), POINTER :: tail => NULL()
INTEGER :: istat
CHARACTER(len=80) :: msg
REAL :: temp

! Get the name of the file containing the input data.
WRITE(*, *) 'Enter the file name with the data to be read: '
READ(*, '(A20)') filename
! Open the input data file.
OPEN(UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
     IOSTAT=istat, IOMSG=msg)
! Was the OPEN successful?
fileopen: IF (istat == 0) THEN
  ! The file was opened successfully, so read the data from
  ! it, and store it in the linked list.

  input: DO
    READ(9, *, IOSTAT=istat) temp
    IF(istat /= 0) EXIT
    nvals = nvals + 1
    IF (.NOT. ASSOCIATED(head)) THEN
      ALLOCATE(head, STAT=istat)
      tail => head
      NULLIFY(tail%p)
      tail%value = temp
    ELSE
      ALLOCATE (tail%p, STAT=istat)
      tail => tail%p
      NULLIFY(tail%p)
      tail%value = temp
    END IF
  END DO input
  ! Now, write out the data
  ptr => head
  output: DO
    IF (.NOT. ASSOCIATED(ptr)) EXIT
    WRITE(*, '(F10.4)') ptr%value
    ptr => ptr%p
  END DO output
ELSE fileopen
  ! Else file open failed. Tell user.
  WRITE(*, '(A,I6)') 'File open failed. Status: ', istat
  WRITE(*, *) msg
END IF fileopen
END PROGRAM linked_list
