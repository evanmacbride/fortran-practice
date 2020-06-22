FUNCTION largest(arr) RESULT(value)
IMPLICIT NONE
REAL, DIMENSION(:), INTENT(IN), TARGET :: arr
REAL, POINTER :: value
INTEGER :: i
NULLIFY(value)
ALLOCATE(value)
value => arr(1)
DO i = 2, SIZE(arr)
  IF (arr(i) > value) THEN
    value => arr(i)
  END IF
END DO
END FUNCTION largest

PROGRAM pt_largest
IMPLICIT NONE
INTERFACE
  FUNCTION largest(arr) RESULT(value)
  IMPLICIT NONE
  REAL, DIMENSION(:), INTENT(IN), TARGET :: arr
  REAL, POINTER :: value
  END FUNCTION
END INTERFACE
REAL, DIMENSION(10), TARGET :: arr
REAL, POINTER :: ptr
INTEGER :: i
arr = [(REAL(i), i = 1, 10)]
arr(5) = 17.
ptr => largest(arr)
WRITE(*, *) ptr
END PROGRAM pt_largest
