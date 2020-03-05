PROGRAM gravity
IMPLICIT NONE
  REAL, PARAMETER :: G = 6.672E-11 ! Gravitational constant in N m**2/kg**2
  REAL, PARAMETER :: M = 5.98E+24 ! Mass of the Earth in kg
  REAL, PARAMETER :: R = 6371.0 ! Mean radius of the Earth in km
  REAL :: h ! Height in km
  REAL :: start_height = 40000.0 ! Starting height
  REAL :: inc = 500.0 ! Height increment in km
  REAL :: result ! Acceleration due to gravity at height h
  INTEGER :: index

  h = start_height
  WRITE(*, 100) "Height (km)","Accel (m/s^2)"
  WRITE(*, 100) "-----------","-------------"
  100 FORMAT (1X,A11,2X,A13)
  DO index = 0, INT(start_height / inc)
    result = -G * (M / (R + h)**2)
    WRITE(*,'(F12.1,2X,ES13.3)') h, result
    h = h - inc
  END DO

END PROGRAM gravity
