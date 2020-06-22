MODULE test_functions
IMPLICIT NONE
CONTAINS
  REAL FUNCTION func1(x)
  IMPLICIT NONE
  REAL, INTENT(IN) :: x
  func1 = x**2 - 2 * x + 4
  END FUNCTION func1

  REAL FUNCTION func2(x)
  IMPLICIT NONE
  REAL, INTENT(IN) :: x
  func2 = EXP(-x / 5) * SIN(2 * x)
  END FUNCTION func2

  REAL FUNCTION func3(x)
  IMPLICIT NONE
  REAL, INTENT(IN) :: x
  func3 = COS(x)
  END FUNCTION func3
END MODULE test_functions

PROGRAM test_function_pointers
USE test_functions
IMPLICIT NONE
INTEGER :: index
PROCEDURE(func1), POINTER :: p
REAL :: x
WRITE(*, *) 'Select a function to associate with the pointer:'
WRITE(*, *) ' 1: func1'
WRITE(*, *) ' 2: func2'
WRITE(*, *) ' 3: func3'
READ(*, *) index
IF ((index < 1) .OR. (index > 3)) THEN
  WRITE(*, *) 'Invalid selection.'
  ERROR STOP 'Bad index'
ELSE
  SELECT CASE(index)
    CASE(1)
      WRITE(*, *) 'func1 selected...'
      p => func1
    CASE(2)
      WRITE(*, *) 'func2 selected...'
      p => func2
    CASE(3)
      WRITE(*, *) 'func3 selected...'
      p => func3
  END SELECT
  WRITE(*, '(A)', ADVANCE='NO') 'Enter x: '
  READ(*, *) x
  WRITE(*, '(A,F13.6)') 'f(x) = ', p(x)
END IF
END PROGRAM test_function_pointers
