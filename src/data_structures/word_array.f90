PROGRAM word_array
IMPLICIT NONE
CHARACTER(len=80) :: msg
INTEGER :: status
CHARACTER(len=16) :: word ! 45 is the length of the longest dictionary word
CHARACTER(len=16), DIMENSION(5000) :: words
CHARACTER(len=16), DIMENSION(16,900) :: length_dict
INTEGER, DIMENSION(16) :: counters
INTEGER :: i, j
LOGICAL :: done_length

OPEN(UNIT=13,FILE="resources/top5000.txt",STATUS='OLD',ACTION='READ', &
     IOSTAT=status,IOMSG=msg)
fileopen: IF (status /= 0) THEN
  WRITE(*, *) msg
ELSE
  READ(13, '(A)') words
  counters = 1
  length_dict = ''
  DO i = 1, SIZE(words)
    length_dict( LEN_TRIM(words(i)), counters( LEN_TRIM(words(i)) ) ) = words(i)
    counters(LEN_TRIM(words(i))) = counters(LEN_TRIM(words(i))) + 1
  END DO

  ! Print all words in order of length. Do not print empty strings.
  !done_length = .FALSE.
  !DO i = 1, 16
  !  DO j = 1, 900
  !    IF (LEN_TRIM(length_dict(i, j)) == 0) THEN
  !      WRITE(*, *) "No more words of length ", i
  !      done_length = .TRUE.
  !      EXIT
  !    END IF
  !    WRITE(*, *) length_dict(i, j), LEN_TRIM(length_dict(i, j))
  !  END DO
  !  done_length = .FALSE.
  !END DO

  counters = counters - 1
  ! Write random words at each available length between 2 and 14
  CALL SYSTEM_CLOCK(j)
  CALL SRAND(j**2 - j)
  DO i = 2, 14
    IF (counters(i) == 0) THEN
      CYCLE
    END IF
    j =  CEILING(RAND() * counters(i))
    WRITE(*, '(A17,I4)') TRIM(length_dict(i, j)), j
  END DO
END IF fileopen

CLOSE(13)
END PROGRAM word_array
