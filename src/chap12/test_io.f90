PROGRAM test_io
IMPLICIT NONE
TYPE :: person
  CHARACTER(len=14) :: first_name
  CHARACTER :: middle_initial
  CHARACTER(len=14) :: last_name
  CHARACTER(len=14) :: phone
  INTEGER :: age
  CHARACTER :: sex
  CHARACTER(len=11) :: ssn
END TYPE person
TYPE (person) :: john

john = person('John', 'R', 'Jones', '323-6439', 21, 'M', '123-45-6789')
WRITE(*, *) 'Free format: ', john
WRITE(*, 1000) john
1000 FORMAT (' Formatted I/O:',/,4(1X,A,/),1X,I0,/,1X,A,/,1X,A)
END PROGRAM test_io
