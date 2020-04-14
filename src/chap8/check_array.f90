PROGRAM check_array
IMPLICIT NONE
  REAL, DIMENSION(-5:5,0:3) :: a = 0.
  WRITE(*, 100) SHAPE(a)
  100 FORMAT ('The shape of the array is:            ',7I6)
  WRITE(*, 110) SIZE(a)
  110 FORMAT ('The size of the array is:             ',I6)
  WRITE(*, 120) LBOUND(a)
  120 FORMAT ('The lower bounds of the array are:    ',7I6)
  WRITE(*, 130) UBOUND(a)
  130 FORMAT ('The upper bounds of the array are:    ',7I6)

END PROGRAM check_array
