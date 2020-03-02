PROGRAM letter_grades
IMPLICIT NONE
  REAL :: numeric_grade
  CHARACTER :: letter_grade
  WRITE(*, *) "Enter a numerical grade (0 - 100)."
  READ(*, *) numeric_grade

  !multiple_else_if: IF (95.0 < numeric_grade) THEN
  !  letter_grade = "A"
  !ELSE IF (86.0 < numeric_grade) THEN
  !  letter_grade = "B"
  !ELSE IF (76.0 < numeric_grade) THEN
  !  letter_grade = "C"
  !ELSE IF (66.0 < numeric_grade) THEN
  !  letter_grade = "D"
  !ELSE
  !  letter_grade = "F"
  !END IF multiple_else_if

  check_a: IF (95.0 > numeric_grade) THEN
    check_b: IF (86.0 > numeric_grade) THEN
      check_c: IF (76.0 > numeric_grade) THEN
        check_d: IF (66.0 > numeric_grade) THEN
          letter_grade = "F"
        ELSE
          letter_grade = "D"
        END IF check_d
      ELSE
        letter_grade = "C"
      END IF check_c
    ELSE
      letter_grade = "B"
    END IF check_b
  ELSE
    letter_grade = "A"
  END IF check_a

  WRITE(*, *) numeric_grade, letter_grade
END PROGRAM letter_grades
