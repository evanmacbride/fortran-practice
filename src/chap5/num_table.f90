PROGRAM num_table
IMPLICIT NONE
INTEGER :: index

WRITE(*, '(1X,A,1X,A9,2A6)') "N", "SQRT(N)", "N**2", "N**3"
WRITE(*, '(1X,A,2X,A,2(2X,A))') "--","-------","----","----"
DO index = 1, 10
  WRITE(*,'(I3,F9.5,2I6)') index, SQRT(REAL(index)), index**2, index**3
END DO

END PROGRAM num_table
