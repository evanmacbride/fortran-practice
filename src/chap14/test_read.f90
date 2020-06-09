PROGRAM test_read
IMPLICIT NONE
INTEGER :: i = 1, j = 2, k = 3
WRITE(*, *) 'Enter i, j, and k: '
READ(*, *) i, j, k
WRITE(*, *) 'i, j, k = ', i, j, k
END PROGRAM test_read
