PROGRAM linky
IMPLICIT NONE
TYPE node
  CHARACTER(len=16) :: word
  TYPE(node), POINTER :: next
END TYPE node
INTEGER, PARAMETER :: MAX_SIZE = 30
TYPE(node), DIMENSION(MAX_SIZE), TARGET :: word_list
TYPE(node), POINTER :: p, temp
INTEGER :: i

DO i = 1, MAX_SIZE
  word_list(i)%word = ACHAR(MOD(i - 1, 25) + 65) // ACHAR(MOD(-i + 1, 26) + 90)
END DO

DO i = 1, MAX_SIZE
  WRITE(*, *) word_list(i)%word
END DO

DO i = 1, MAX_SIZE - 1
  word_list(i)%next => word_list(i + 1)
END DO

word_list(MAX_SIZE)%next => word_list(i)

WRITE(*, *) "HERE WE GOOOOOOO"
p => word_list(i)
DO i = 1, 10
  WRITE(*, *) p%word
  p = p%next
END DO

END PROGRAM linky
