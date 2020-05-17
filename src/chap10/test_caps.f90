! Test program for subroutine caps, which will capitalize the first
! letter after a word break and convert all other letters to lowercase.

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

SUBROUTINE lower(str)
IMPLICIT NONE
CHARACTER(len=*), INTENT(INOUT) :: str
INTEGER :: i
DO i = 1, LEN(str)
  IF(LGE(str(i:i), 'A') .AND. LLE(str(i:i), 'Z')) THEN
    str(i:i) = ACHAR(IACHAR(str(i:i)) + 32)
  END IF
END DO
END SUBROUTINE lower

SUBROUTINE caps(str)
IMPLICIT NONE
CHARACTER(len=*), INTENT(INOUT) :: str
INTEGER :: i
CALL upper(str(1:1))
! Check to see if preceeding character was non-alphanumeric in the basic
! Latin set. (Since that would probably mark a word break.) If it was,
! call upper. Otherwise, call lower.
DO i = 2, LEN(str)
  IF (LLT(str(i-1:i-1), '0') .OR. &
  (LLT(str(i-1:i-1), 'A') .AND. LGT(str(i-1:i-1), '9')) .OR. &
  (LLT(str(i-1:i-1), 'a') .AND. LGT(str(i-1:i-1), 'Z')) .OR. &
  LGT(str(i-1:i-1), 'z')) THEN
    CALL upper(str(i:i))
  ELSE
    CALL lower(str(i:i))
  END IF
END DO
END SUBROUTINE caps

PROGRAM test_caps
IMPLICIT NONE
CHARACTER(len=80) :: a = "hellO_Kind sir+madmAn"
CHARACTER(len=80) :: b = "complaints@dmV.Gov"
character(len=80) :: c = "Drs.eat 12lbs of:fat&gristle"
CALL caps(a)
CALL caps(b)
CALL caps(c)
WRITE(*, *) a, b, c
END PROGRAM test_caps
