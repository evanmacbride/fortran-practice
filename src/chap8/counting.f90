MODULE populate
IMPLICIT NONE
REAL, DIMENSION(-50:50,0:100) :: values
INTEGER :: i, j
CONTAINS
  SUBROUTINE populate_array()
    INTEGER, DIMENSION(8) :: time
    CALL DATE_AND_TIME(VALUES=time)
    CALL SRAND(time(8))
    DO i = -50, 50
      DO j = 0, 100
        IF (RAND() .gt. 0.95) THEN
          values(i, j) = 0.
        ELSE
          values(i, j) = RAND() * 20. - 10.
        END IF
      END DO
    END DO
  END SUBROUTINE populate_array
END MODULE populate

PROGRAM counting
USE populate
IMPLICIT NONE
INTEGER :: pos_count = 0, neg_count = 0, zero_count = 0

CALL populate_array()

DO i = -50, 50
  DO j = 0, 100
    IF (values(i, j) .gt. 0.0) THEN
      pos_count = pos_count + 1
    ELSE IF (values(i, j) .lt. 0.0) THEN
      neg_count = neg_count + 1
    ELSE
      zero_count = zero_count + 1
    END IF
  END DO
END DO

WRITE(*, *) "Counting positive, negative, and zero values in array..."
WRITE(*, *) "Results using DO and IF statements:"
WRITE(*, '(3(A,I5,2X))') "POS: ", pos_count, "NEG: ", neg_count, "ZERO: ", zero_count
WRITE(*, *) "Results using COUNT statements:"
WRITE(*, '(3(A,I5,2X))') "POS: ", COUNT(values > 0.), "NEG: ", COUNT(values < 0.), "ZERO: ", COUNT(values == 0.)
END PROGRAM counting
