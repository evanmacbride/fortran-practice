PROGRAM string_comp
IMPLICIT NONE
  CHARACTER(LEN = 63) :: stringA, stringB ! User inputs
  CHARACTER(LEN = 63) :: upperA = "", upperB = ""! Uppercase copies of user inputs
  CHARACTER :: temp
  INTEGER :: index ! Counter
  WRITE(*, *) "Enter the first string to compare: "
  READ(*, *) stringA
  WRITE(*, *) "Enter the second string to compare: "
  READ(*, *) stringB
  WRITE(*, *) "Comparing strings: "
  WRITE(*, *) stringA
  WRITE(*, *) stringB

  IF (LEN_TRIM(stringA) .ne. LEN_TRIM(stringB)) THEN
    WRITE(*, *) "Strings are unequal lengths. Strings are not equal."
  ELSE
    DO index = 1, LEN_TRIM(stringA)
      IF ((stringA(index:index) .ge. 'a') .AND. (stringA(index:index) .le. 'z')) THEN
          temp = ACHAR(IACHAR(stringA(index:index)) - 32)
          upperA(index:index) = temp
      ELSE
        upperA(index:index) = stringA(index:index)
      END IF
      IF ((stringB(index:index) .ge. 'a')  .AND. (stringB(index:index) .le. 'z')) THEN
          temp = ACHAR(IACHAR(stringB(index:index)) - 32)
          upperB(index:index) = temp
      ELSE
        upperB(index:index) = stringB(index:index)
      END IF
    END DO

    WRITE(*, *) "Converting to uppercase: "
    WRITE(*, *) upperA
    WRITE(*, *) upperB
    WRITE(*, *) "Strings equal: ", upperA .eq. upperB
  END IF
END PROGRAM string_comp
