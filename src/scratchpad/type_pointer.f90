PROGRAM type_pointer
IMPLICIT NONE
TYPE fun
  INTEGER :: int
  CHARACTER :: char
END TYPE fun
INTEGER :: i
INTEGER, PARAMETER :: MAX_SIZE = 30
INTEGER, DIMENSION(MAX_SIZE), TARGET :: nums
INTEGER, POINTER :: p
TYPE(fun), DIMENSION(MAX_SIZE), TARGET :: fun_stuff
TYPE(fun), POINTER :: fsp

p => nums(3)
fsp => fun_stuff(5)

DO i = 1, MAX_SIZE
  fun_stuff(i)%int = i
  fun_stuff(i)%char = ACHAR(MOD(i - 1, 25) + 65)
  nums(i) = 2 * i - 1
END DO

WRITE(*, *) p, fsp%char
p = 12
fsp%char = '@'

DO i = 1, MAX_SIZE
  WRITE(*, '(2I4,A3)') nums(i), fun_stuff(i)%int, fun_stuff(i)%char
END DO

END PROGRAM type_pointer
