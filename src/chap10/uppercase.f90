SUBROUTINE upper(str)
IMPLICIT NONE
CHARACTER(len=*), INTENT(INOUT) :: str
INTEGER :: i

DO i = 1, LEN(str)
  IF(LGE(str(i:i), 'a') .AND. LLE(str(i:i), 'z')) THEN
    str(i:i) = ACHAR(IACHAR(str(i:i)) - 32)
  END IF
END DO

END SUBROUTINE upper

PROGRAM uppercase
IMPLICIT NONE
CHARACTER(len=5), DIMENSION(3) :: str
INTEGER :: i, j
str(1) = "helLo"
str(2) = "?O^s~"
str(3) = "choiR"
DO i = 1, 3
CALL upper(str(i))
  DO j = 1, LEN(str)
    WRITE(*, *) str(i)(j:j)
  END DO
END DO
END PROGRAM uppercase
