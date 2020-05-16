! Create a format for a real number so that it only shows as many decimal places
! as necessary.
! Read the real in as a string and save its length. Then find the index of the
! decimal point, and save that.

SUBROUTINE get_decimal_places(str, places)
IMPLICIT NONE
CHARACTER(len=*), INTENT(IN) :: str ! A string representing a real number
INTEGER, INTENT(OUT) :: places ! The number of decimal places in the string
IF (INDEX(str,'.') == 0) THEN
  places = 0
ELSE
  places = LEN_TRIM(str) - INDEX(str,'.')
END IF
END SUBROUTINE get_decimal_places

SUBROUTINE get_format_fragment(places, frag)
INTEGER, INTENT(IN) :: places
CHARACTER(len=*), INTENT(OUT) :: frag
CHARACTER(len=20) :: p
WRITE(p, '(I0)') places
WRITE(*, *) "Length of p: ", LEN_TRIM(p), " Places: ", places
frag = 'F0.'//TRIM(p)
END SUBROUTINE get_format_fragment

PROGRAM shrink_format
IMPLICIT NONE
REAL :: r
CHARACTER(len=20) :: str, frag, fmt
INTEGER :: places

WRITE(*, *) "Enter a real number: "
READ(*, *) str
CALL get_decimal_places(str, places)
CALL get_format_fragment(places, frag)
fmt = '(A,' // TRIM(frag) // ',A)'
!WRITE(*, *) fmt
WRITE(*, *) "Decimal places required: ", places
READ(str, *) r
WRITE(*, fmt) "Entered: ", r, ". Enjoy."
END PROGRAM shrink_format
