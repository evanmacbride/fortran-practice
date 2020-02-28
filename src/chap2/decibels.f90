PROGRAM decibels
IMPLICIT NONE
  REAL :: ratio ! In decibels
  REAL :: power_level, reference = 1.0 ! Power levels in mW
  WRITE(*, *) "Enter a power level in mW"
  READ(*, *) power_level
  ratio = 10 * LOG(power_level / reference) / LOG(10.0)
  WRITE(*, *) power_level, " mW against ", reference, " mW reference is ", ratio, " decibels"


END PROGRAM decibels
