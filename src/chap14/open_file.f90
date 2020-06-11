PROGRAM open_file
IMPLICIT NONE
INTEGER :: istat
LOGICAL :: lexist
LOGICAL :: lopen = .FALSE.
CHARACTER(len=20) :: file_name
CHARACTER :: yn

openfile: DO
  WRITE(*, *) 'Enter output file name: '
  READ(*, '(A)') file_name
  INQUIRE(FILE=file_name, EXIST=lexist)
  exists: IF (.NOT. lexist) THEN
    OPEN(UNIT=9, FILE=file_name, STATUS='NEW', ACTION='WRITE', IOSTAT=istat)
    lopen = .TRUE.
  ELSE
    WRITE(*, '(A,G0,A)') 'File ', TRIM(file_name), ' already exists. Overwrite it? (Y/N) '
    READ(*, '(A)') yn
    !CALL UCASE(yn)
    replace: IF (yn == 'Y') THEN
      OPEN(UNIT=9, FILE=file_name, STATUS='REPLACE', ACTION='WRITE', &
           IOSTAT=istat)
      lopen = .TRUE.
    END IF replace
  END IF exists
  IF (lopen) EXIT
END DO openfile
WRITE(9, *) 'This is the output file!'
CLOSE(9, STATUS='KEEP')
END PROGRAM open_file
