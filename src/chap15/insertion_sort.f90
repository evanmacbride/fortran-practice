PROGRAM insertion_sort
IMPLICIT NONE
TYPE :: int_value
  INTEGER :: value
  TYPE (int_value), POINTER :: next_value
END TYPE int_value

TYPE (int_value), POINTER :: head => NULL()
CHARACTER(len=20) :: filename
INTEGER :: istat
CHARACTER(len=80) :: msg
INTEGER :: nvals = 0
TYPE (int_value), POINTER :: ptr => NULL()
TYPE (int_value), POINTER :: ptr1 => NULL()
TYPE (int_value), POINTER :: ptr2 => NULL()
TYPE (int_value), POINTER :: tail => NULL()
INTEGER :: temp

! Get the name of the file containing the input data.
WRITE(*, *) 'Enter the file name with the data to be sorted: '
READ(*, '(A20)') filename
! Open input data file.
OPEN(UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
     IOSTAT=istat)
! Was the OPEN successful?
fileopen: IF (istat == 0) THEN
  ! The file was opened successfully, so read the data value
  ! to sort, allocate a variable for it, and locate the proper
  ! point to insert the new value into the list.
  input: DO
    READ(9, *, IOSTAT=istat) temp
    IF (istat /= 0) EXIT input
    nvals = nvals + 1
    ALLOCATE(ptr, STAT=istat)
    ptr%value = temp
    ! Now find out where to put it in the list.
    new: IF (.NOT. ASSOCIATED(head)) THEN
      ! Add at end of list
      head => ptr
      tail => head
      NULLIFY(ptr%next_value)
    ELSE
      ! Values already in list. Check for location.
      front: IF (ptr%value < head%value) THEN
        ! Add at front of list
        ptr%next_value => head
        head => ptr
      ELSE IF (ptr%value >= tail%value) THEN
        ! Add at the end of list
        tail%next_value => ptr
        tail => ptr
        NULLIFY(tail%next_value)
      ELSE
        ! Find place to add value
        ptr1 => head
        ptr2 => ptr1%next_value
        search: DO
          IF ((ptr%value >= ptr1%value) .AND. &
              (ptr%value < ptr2%value)) THEN
            ! Insert value here
            ptr%next_value => ptr2
            ptr1%next_value => ptr
            EXIT search
          END IF
          ptr1 => ptr2
          ptr2 => ptr2%next_value
        END DO search
      END IF front
    END IF new
  END DO input
  ! Now write out the data.
  ptr => head
  output: DO
    IF (.NOT. ASSOCIATED(ptr)) EXIT
    WRITE(*, '(I10)') ptr%value
    ptr => ptr%next_value
  END DO output
ELSE fileopen
  ! Else file open failed. Tell user.
  WRITE(*, '(A,I6)') 'File open failed. Status: ', istat
  WRITE(*, *) msg
END IF fileopen
END PROGRAM insertion_sort
