PROGRAM test_blocks
IMPLICIT NONE
INTEGER :: i, j, k
i = 1
j = 2
k = 3

WRITE(*, *) "BEFORE BLOCK: i, j, k = ", i, j, k
test_block: BLOCK
  INTEGER :: j
  WRITE(*, *) 'In block before DO loop.'
  DO j = 1, 10
    WRITE(*, *) 'In block: i, j, k     = ', i, j, k
    IF (j > 2) EXIT test_block
  END DO
  WRITE(*, *) 'In block after DO loop.'
END BLOCK test_block

WRITE(*, *) 'After block: i, j, k  = ', i, j, k

END PROGRAM test_blocks
