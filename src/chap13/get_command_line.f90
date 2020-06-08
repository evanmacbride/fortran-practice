PROGRAM get_command_line
IMPLICIT NONE
INTEGER :: i
CHARACTER(len=128) :: command
CHARACTER(len=80) :: arg

CALL GET_COMMAND_ARGUMENT(0, command)
WRITE(*, '(A,A)') 'Program name is: ', TRIM(command)

DO i = 1, COMMAND_ARGUMENT_COUNT()
  CALL GET_COMMAND_ARGUMENT(i, arg)
  WRITE(*, '(A,I2,A,A)') 'Argument ', i, ' is ', TRIM(arg)
END DO
END PROGRAM get_command_line
