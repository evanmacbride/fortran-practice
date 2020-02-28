PROGRAM getSize
IMPLICIT NONE
	REAL(KIND = 4) :: a
	REAL(KIND = 8) :: b
	INTEGER(KIND = 2) :: i
	INTEGER(KIND = 4) :: j

	PRINT *, 'Precision of real(4) = ', PRECISION(a)
	PRINT *, 'Precision of real(8) = ', PRECISION(b)

	PRINT *, 'Range of real(4) = ', RANGE(a)
	PRINT *, 'Range of real(8) = ', RANGE(b)

	PRINT *, 'Maximum exponent of real(4) = ', MAXEXPONENT(a)
	PRINT *, 'Maximum exponent of real(8) = ', MAXEXPONENT(b)

	PRINT *, 'Minimum exponent of real(4) = ', MinEXPONENT(a)
	PRINT *, 'Minimum exponent of real(8) = ', MinEXPONENT(b)

	PRINT *, 'Bits in integer(2) = ', BIT_SIZE(i)
	PRINT *, 'Bits in integer(4) = ', BIT_SIZE(j)
END PROGRAM getSize
