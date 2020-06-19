PROGRAM test_ptr4
IMPLICIT NONE
REAL, POINTER :: p1 => NULL(), p2 => NULL(), p3 => NULL()
REAL, TARGET :: a = 11., b = 12.5, c
p1 => a
p2 => b
p3 => c
p3 = p1 + p2
WRITE(*, *) 'p3 = ', p3
p2 => p1
p3 = p1 + p2
WRITE(*, *) 'p3 = ', p3
p3 = p1
p3 => a
WRITE(*, *) 'p3 = ', p3
WRITE(*, *) 'a, b, c = ', a, b, c
END PROGRAM test_ptr4
