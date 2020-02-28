PROGRAM carbon_date
IMPLICIT NONE
  REAL, PARAMETER :: DECAY_CONSTANT = 0.00012097 ! The known decay constant for carbon 14
  REAL :: carbon14_final  ! Percentage of carbon 14 remaining
  REAL :: age             ! Age in years
  WRITE(*, *) "Enter the percent of carbon 14 remaining."
  READ(*, *) carbon14_final
  age = (-1.0 / DECAY_CONSTANT) * LOG(carbon14_final / 100)
  WRITE(*, *) carbon14_final, "% carbon 14 remaining. Age: ", age, " years."
END PROGRAM carbon_date
