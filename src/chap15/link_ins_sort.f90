MODULE tools
IMPLICIT NONE
TYPE :: node
  REAL :: value
  TYPE (node), POINTER :: prev
  TYPE (node), POINTER :: next
END TYPE node
CONTAINS
  RECURSIVE SUBROUTINE insert_sorted(ptr, val, tail)
  TYPE (node), INTENT(INOUT), POINTER :: ptr
  TYPE (node), INTENT(INOUT), POINTER :: tail
  REAL, INTENT(IN) :: val
  TYPE (node), POINTER :: temp
  IF (.NOT. ASSOCIATED(ptr)) THEN
    ALLOCATE(ptr)
    ptr%prev => NULL()
    ptr%next => NULL()
    ptr%value = val
    tail => ptr
  ELSE IF (val <= ptr%value) THEN
    ALLOCATE(temp)
    temp%value = val
    IF (.NOT. ASSOCIATED(ptr%prev)) THEN
      temp%prev => NULL()
      temp%next => ptr
      ptr%prev => temp
      ptr => temp
    ELSE
      temp%prev => ptr%prev
      ptr%prev => temp
      temp%next => ptr
      temp%prev%next => temp
    END IF
  ELSE IF (.NOT. ASSOCIATED(ptr%next)) THEN
    ALLOCATE(temp)
    temp%value = val
    temp%next => NULL()
    temp%prev => ptr
    ptr%next => temp
    tail => temp
  ELSE
    CALL insert_sorted(ptr%next, val, tail)
  END IF
  END SUBROUTINE insert_sorted
END MODULE tools

PROGRAM link_ins_sort
USE tools
IMPLICIT NONE
TYPE (node), POINTER :: head => NULL()
CHARACTER(len=20) :: filename
INTEGER :: nvals = 0
TYPE (node), POINTER :: ptr => NULL()
TYPE (node), POINTER :: tail => NULL()
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
    CALL insert_sorted(head, temp, tail)
  END DO input
  ! Now, write out the data
  ptr => head
  WRITE(*, '(A)') ' Printing list.'
  forward: DO
    IF (.NOT. ASSOCIATED(ptr)) EXIT
    WRITE(*, '(F10.4)') ptr%value
    ptr => ptr%next
  END DO forward
  ptr => tail
  WRITE(*, '(A)') ' Printing list backwards.'
  backward: DO
    IF (.NOT. ASSOCIATED(ptr)) EXIT
    WRITE(*, '(F10.4)') ptr%value
    ptr => ptr%prev
  END DO backward
ELSE fileopen
  ! Else file open failed. Tell user.
  WRITE(*, '(A,I6)') 'File open failed. Status: ', istat
  WRITE(*, *) msg
END IF fileopen
END PROGRAM link_ins_sort
