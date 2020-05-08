SUBROUTINE gib(str)
CHARACTER(len=*), INTENT(OUT) :: str
INTEGER :: i

DO i = 1, LEN(str)
  str(i:i) = ACHAR(CEILING(RAND() * 95) + 31)
END DO
END SUBROUTINE gib

PROGRAM gibber
IMPLICIT NONE
CHARACTER(len=16), DIMENSION(16,4) :: strs
INTEGER :: i, j
DO i = 1, 16
  DO j = 1, 4
    CALL gib(strs(i, j))
    !WRITE(*, '(A)') strs(i)
  END DO
END DO
WRITE(*, '(A17,3A18)') ((strs(i, j), i = 1, 16), j = 1, 4)
END PROGRAM gibber
