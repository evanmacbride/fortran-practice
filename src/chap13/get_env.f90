PROGRAM get_env
IMPLICIT NONE
INTEGER :: length
INTEGER :: status
CHARACTER(len=80) :: value
CALL GET_ENVIRONMENT_VARIABLE('USER',value,length,status)
WRITE(*, *) 'Get "USER" environment variable:'
WRITE(*, '(A,I6)') 'STATUS: ', status
IF (status <= 0) THEN
  WRITE(*, '(A,A)') 'VALUE: ', TRIM(value)
END IF
END PROGRAM get_env
