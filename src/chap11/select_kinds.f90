PROGRAM select_kinds
USE iso_Fortran_env
IMPLICIT NONE
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6, r=37)
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13,r=200)
REAL(kind=SGL) :: var1 = 0.
REAL(kind=DBL) :: var2 = 0._DBL
REAL(kind=REAL32) :: var3 = 1.
REAL(kind=REAL128) :: var4 = 1.
WRITE(*, 100) 'var1', KIND(var1), PRECISION(var1), RANGE(var1)
WRITE(*, 100) 'var2', KIND(var2), PRECISION(var2), RANGE(var2)
WRITE(*, 100) 'var3', KIND(var3), PRECISION(var3), RANGE(var3)
WRITE(*, 100) 'var4', KIND(var4), PRECISION(var4), RANGE(var4)
100 FORMAT (A, ': kind = ', I2, ', precision = ', I2, ', range = ', I4)
END PROGRAM select_kinds
