PROGRAM stock
IMPLICIT NONE
INTEGER, PARAMETER :: LU_DB = 7   ! Unit for the database file
INTEGER, PARAMETER :: LU_M = 8    ! Unit for the message file
INTEGER, PARAMETER :: LU_T = 9    ! Unit for the transaction file
TYPE :: database_record
  INTEGER :: stock_number
  CHARACTER(len=30) :: description
  CHARACTER(len=10) :: vendor
  CHARACTER(len=20) :: vendor_number
  INTEGER :: number_in_stock
  INTEGER :: minimum_quantity
END TYPE database_record

TYPE :: transaction_record
  INTEGER :: stock_number
  INTEGER :: number_in_transaction
END TYPE transaction_record

TYPE(database_record) :: item
TYPE(transaction_record) :: trans
CHARACTER(len=3) :: file_stat
INTEGER :: istat
LOGICAL :: exist
CHARACTER(len=120) :: msg
CHARACTER(len=24) :: db_file = 'stock.db'
CHARACTER(len=24) :: msg_file = 'stock.msg'
CHARACTER(len=24) :: trn_file = 'stock.trn'

OPEN(LU_DB, FILE=db_file, STATUS='OLD', ACCESS='DIRECT', FORM='FORMATTED', &
     RECL=78, IOSTAT=istat, IOMSG=msg)
IF (istat /= 0) THEN
  WRITE(*, 100) db_file, istat
  100 FORMAT (' Open failed on file ', A, '. IOSTAT = ', I6)
  WRITE(*, '(A)') msg
  ERROR STOP 'Database file bad'
END IF

OPEN(LU_T, FILE=trn_file, STATUS='OLD', ACCESS='SEQUENTIAL', IOSTAT=istat, &
     IOMSG=msg)
IF (istat /= 0) THEN
  WRITE(*, 100) trn_file, istat
  WRITE(*, '(A)') msg
  ERROR STOP 'Transaction file bad'
END IF

INQUIRE(FILE=msg_file, EXIST=exist)
IF (exist) THEN
  file_stat = 'OLD'
ELSE
  file_stat = 'NEW'
END IF
OPEN(LU_M, FILE=msg_file, STATUS=file_stat, POSITION='APPEND', &
     ACCESS='SEQUENTIAL', IOSTAT=istat, IOMSG=msg)
IF (istat /= 0) THEN
  WRITE(*, 100) msg_file, istat
  WRITE(*, '(A)') msg
  ERROR STOP 'Message file bad'
END IF

process: DO
  READ(LU_T, *, IOSTAT=istat) trans
  IF (istat /= 0) EXIT
  READ(LU_DB, '(I6,A30,A10,A20,I6,I6)', REC=trans%stock_number, &
       IOSTAT=istat) item
  IF (istat /= 0) THEN
    WRITE(*, '(A,I6,A,I6)') &
      ' Read failed on database file record ', &
        trans%stock_number, ' IOSTAT = ', istat
    ERROR STOP 'Database read failed'
  END IF
  item%number_in_stock = item%number_in_stock + trans%number_in_transaction
  IF (item%number_in_stock < 0) THEN
    WRITE(LU_M, '(A,I6,A)') ' Error: Stock number ', trans%stock_number, &
                            ' has quantity < 0! '
    item%number_in_stock = 0
  END IF

  IF (item%number_in_stock < item%minimum_quantity) THEN
    WRITE(LU_M,110) ' Reorder stock number ', trans%stock_number, &
                    ' from vendor ', item%vendor, ' Description: ', &
                    item%description
    110 FORMAT (A,I6,A,A,/,A,A)
  END IF

  WRITE(LU_DB, '(I6,A30,A10,A20,I6,I6)', REC=trans%stock_number, &
        IOSTAT=istat) item
END DO process
CLOSE(LU_DB)
CLOSE(LU_T)
CLOSE(LU_M)
END PROGRAM stock
