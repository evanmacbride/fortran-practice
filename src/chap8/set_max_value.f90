PROGRAM set_max_value
IMPLICIT NONE
INTEGER, DIMENSION(1000,10,30) :: values
INTEGER :: i, j, k

DO i = 1, 1000
  DO j = 1, 10
    DO k = 1, 30
      values(i, j, k) = CEILING(RAND() * 1002.)
    END DO
  END DO
END DO

DO i = 1, 1000
  DO j = 1, 10
    DO k = 1, 30
      IF (values(i, j, k) .gt. 1000) THEN
        values(i, j, k) = 1000
        !WRITE(*, 100) i, j, k, values(i, j, k)
        !100 FORMAT ("POS:",I6,I4,I4,2X,"VAL:",I6)
      END IF
    END DO
  END DO
END DO

DO i = 1, 1000
  DO j = 1, 10
    DO k = 1, 30
      values(i, j, k) = CEILING(RAND() * 1002.)
    END DO
  END DO
END DO

WHERE (values > 1000)
  values = 1000
END WHERE

END PROGRAM set_max_value
