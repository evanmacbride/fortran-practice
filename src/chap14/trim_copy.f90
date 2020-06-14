PROGRAM trim_copy
IMPLICIT NONE
CHARACTER(len=32) :: in_name, out_name, close_status
CHARACTER(len=160) :: line
CHARACTER(len=80) :: msg
INTEGER :: istat
LOGICAL :: lexist = .FALSE.
CHARACTER :: choice

WRITE(*, *) "Enter input filename: "
READ(*, *) in_name

OPEN(UNIT=7, FILE=in_name, STATUS='OLD', ACTION='READ', IOSTAT=istat, IOMSG=msg)
!INQUIRE(UNIT=7, EXIST=lexist)
input_open_ok: IF (istat /= 0) THEN
  WRITE(*, *) "Error: Could not open file ", TRIM(in_name)
  WRITE(*, *) msg
ELSE
  WRITE(*, *) "Enter output filename: "
  READ(*, *) out_name
  INQUIRE(FILE=out_name, EXIST=lexist)
  exists: IF (lexist) THEN
    WRITE(*, *) "File ", TRIM(out_name), " already exists.", &
                " Would you like to overwrite it? "
    READ(*, *) choice
    overwrite: IF (choice == 'Y') THEN
      OPEN(UNIT=8, FILE=out_name, STATUS='REPLACE', ACTION='WRITE', &
      IOSTAT=istat, IOMSG=msg)
    ELSE
      WRITE(*, *) "Exiting program."
    END IF overwrite
  ELSE
    OPEN(UNIT=8, FILE=out_name, STATUS='NEW', ACTION='WRITE', &
    IOSTAT=istat, IOMSG=msg)
  END IF exists
  output_open_ok: IF (istat /= 0) THEN
    WRITE(*, *) "Error: Could not open file ", TRIM(in_name)
    WRITE(*, *) msg
  ELSE
    DO
      IF (istat /= 0) EXIT
      READ(7, '(A160)', IOSTAT=istat) line
      IF (istat /= 0) EXIT
      WRITE(8, *, IOSTAT=istat) TRIM(line)
    END DO
    WRITE(*, *) "Copy complete. Would you like to delete ", TRIM(in_name), "?"
    READ(*, *) choice
    IF (choice == 'Y') THEN
      close_status = 'DELETE'
    ELSE
      close_status = 'KEEP'
    END IF
    CLOSE(7, STATUS=close_status)
    CLOSE(8)
  END IF output_open_ok
END IF input_open_ok
END PROGRAM trim_copy
