MODULE format_tools
CONTAINS
  SUBROUTINE form(r, fmt)
  IMPLICIT NONE
  REAL, INTENT(IN) :: r
  CHARACTER(len=:), INTENT(OUT), ALLOCATABLE :: fmt
  IF ((r > 9999999 .OR. r < -999999) .OR. (ABS(r) < 0.01)) THEN
    fmt = "ES12.5"
  ELSE
    fmt = "F12.4"
  END IF
  fmt = '('//fmt//')'
  !99999999.9999

  END SUBROUTINE form
END MODULE format_tools

PROGRAM dynamic_format
USE format_tools
IMPLICIT NONE
CHARACTER(len=:), ALLOCATABLE :: fmt
REAL :: r
!fmt = "hello"
r = 2.1
CALL form(r, fmt)
WRITE(*, fmt) r

r = -0.00005
CALL form(r, fmt)
WRITE(*, fmt) r

r = 9992999
CALL form(r, fmt)
WRITE(*, fmt) r

WRITE(*, *) 1.
END PROGRAM dynamic_format
