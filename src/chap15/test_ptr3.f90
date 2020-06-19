PROGRAM test_ptr3
IMPLICIT NONE
REAL, POINTER :: p1 => NULL(), p2 => NULL(), p3 => NULL()
REAL, TARGET :: a = 11., b = 12.5, c = 3.141592
WRITE(*, *) ASSOCIATED(p1)
p1 => a
p2 => b
p3 => c
WRITE(*, *) ASSOCIATED(p1)
WRITE(*, *) ASSOCIATED(p1, b)
END PROGRAM test_ptr3
