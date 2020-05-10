MODULE word_help
IMPLICIT NONE
! The number of words similar to the secret word
INTEGER, PARAMETER :: MAX_SIMILAR = 4
! Store words from a word list, organized by length. 900 is the most there is of
! any single word length in the word list being used.
CHARACTER(len=16), DIMENSION(16,900) :: length_dict
! Keep track of how many words there are at each possible length.
INTEGER, DIMENSION(16) :: counters
CONTAINS

  ! Load words from a word list and organize them by length.
  SUBROUTINE load_words()
  IMPLICIT NONE
  CHARACTER(len=80) :: msg
  INTEGER :: status
  CHARACTER(len=16) :: word ! 16 is the length of the longest word in our file
  CHARACTER(len=16), DIMENSION(5000) :: words ! There's 5000 words in our file
  INTEGER :: i
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
    counters = counters - 1
  END IF fileopen
  CLOSE(13)
  END SUBROUTINE load_words

  ! Get a random word from the word list
  SUBROUTINE get_rand_word(w)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(OUT) :: w
  INTEGER :: i, j
  i = LEN_TRIM(w)
  CALL SYSTEM_CLOCK(j)
  CALL SRAND((j + 1)**2)
  j =  CEILING(RAND() * counters(i))
  w = TRIM(length_dict(i, j))
  END SUBROUTINE get_rand_word

  ! Get words that are similar to a given word
  SUBROUTINE get_similar(w, sim)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(IN) :: w
  CHARACTER(len=LEN(w)), DIMENSION(LEN(w)-1), INTENT(OUT) :: sim
  CHARACTER(len=LEN(w)) :: candidate
  INTEGER :: i, j, k, l, same, found
  found = 0
  ! Iterate through words of LEN(w) in length_dict. Add words that have
  ! LEN(w) - i letters in common with w.
  DO i = 1, LEN(w) - 1
    ! Iterate through all available words of a given length. The number of
    ! available words for each length is stored in counters.
    DO j = 1, counters(LEN(w))
      ! If there are LEN(w) - j shared letters at k positions, add word to sim.
      same = 0
      candidate = length_dict(LEN(w),j)
      ! If candidate is already in sim, skip it.
      IF (ANY(sim == candidate)) CYCLE
      DO k = 1, LEN(w)
        IF (w(k:k) == candidate(k:k)) THEN
          same = same + 1
        END IF
      END DO
      ! If there are enough letters in common, add candidate word to sim
      IF (same == LEN(w) - i) THEN
        sim(found + 1) = candidate
        found = found + 1
        WRITE(*, *) candidate, LEN(w) - i
        IF (LEN(w) - i > 1) EXIT
      END IF
      IF (found == MAX_SIMILAR) EXIT
    END DO
    IF (found == MAX_SIMILAR) EXIT
  END DO
  END SUBROUTINE get_similar

END MODULE word_help

PROGRAM password_game
USE word_help
IMPLICIT NONE
INTEGER :: n
CHARACTER(len=:), ALLOCATABLE :: w ! The secret word
CHARACTER(len=:), ALLOCATABLE :: s(:) ! Words that are similar to the secret word

WRITE(*, *) "Enter a word length between 2 and 14"
READ(*, *) n
ALLOCATE(CHARACTER(len=n) :: w)
ALLOCATE(CHARACTER(len=n) :: s(MAX_SIMILAR))
CALL load_words()
CALL get_rand_word(w)
WRITE(*, '(A,I3,A,1X,A)') "Random word of length", n, ":", w
CALL get_similar(w, s)
WRITE(*, *) "Similar: ", s
END PROGRAM password_game
