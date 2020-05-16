MODULE character_subs
CONTAINS
  FUNCTION abc(n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  CHARACTER(len=n) :: abc
  character(len=26) :: alphabet = 'abcdefghijklmnopqrstuvwxyz'
  abc = alphabet(1:n)
  END FUNCTION abc
END MODULE character_subs

PROGRAM test_abc
USE character_subs
IMPLICIT NONE
INTEGER :: n
WRITE(*, *) "Enter string length: "
READ(*, *) n
WRITE(*, *) 'The string is: ', abc(n)
END PROGRAM test_abc
