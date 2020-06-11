PROGRAM read_namelist
IMPLICIT NONE
INTEGER :: i = 1, j = 2
REAL :: a = -999., b = 0.
CHARACTER(len=12) :: string = 'Test string.'
NAMELIST / mylist / i, j, string, a, b
OPEN(7, FILE='input.nml', DELIM='APOSTROPHE')
WRITE(*, '(A)') 'Namelist file before update: '
WRITE(UNIT=*, NML=mylist)
READ(UNIT=7, NML=mylist)
WRITE(*, '(A)') 'Namelist file after update: '
WRITE(UNIT=*, NML=mylist)
END PROGRAM read_namelist
