MODULE generic_maxval
IMPLICIT NONE
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6)
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
INTERFACE maxval
  MODULE PROCEDURE maxval_i
  MODULE PROCEDURE maxval_r
  MODULE PROCEDURE maxval_d
  MODULE PROCEDURE maxval_c
  MODULE PROCEDURE maxval_dc
END INTERFACE maxval

CONTAINS
  SUBROUTINE maxval_i(array, nvals, value_max, pos_maxval)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: nvals
  INTEGER, INTENT(IN), DIMENSION(nvals) :: array
  INTEGER, INTENT(OUT) :: value_max
  INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval
  INTEGER :: i
  INTEGER :: pos_max
  value_max = array(1)
  pos_max = 1
  DO i = 2, nvals
    IF (array(i) > value_max) THEN
      value_max = array(i)
      pos_max = i
    END IF
  END DO
  IF (PRESENT(pos_maxval)) THEN
    pos_maxval = pos_max
  END IF
  END SUBROUTINE maxval_i

  SUBROUTINE maxval_r(array, nvals, value_max, pos_maxval)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: nvals
  REAL(KIND=SGL), INTENT(IN), DIMENSION(nvals) :: array
  REAL(KIND=SGL), INTENT(OUT) :: value_max
  INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval
  INTEGER :: i
  INTEGER :: pos_max
  value_max = array(1)
  pos_max = 1
  DO i = 2, nvals
    IF (array(i) > value_max) THEN
      value_max = array(i)
      pos_max = i
    END IF
  END DO
  IF (PRESENT(pos_maxval)) THEN
    pos_maxval = pos_max
  END IF
  END SUBROUTINE maxval_r
END MODULE generic_maxval

PROGRAM test_maxval
IMPLICIT NONE

END PROGRAM test_maxval
