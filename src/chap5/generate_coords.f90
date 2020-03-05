PROGRAM generate_coords
IMPLICIT NONE
  REAL :: x = 0, y ! Coordinates to write to a file
  REAL, PARAMETER :: a = 9.81, b = 5.37 ! Acceleration due to gravity in m/s**2 and initial velocity in m/s
  REAL :: inc = 0.1 ! Time increment
  REAL :: noise ! Randomly generated value to add
  REAL, PARAMETER :: DISTORTION = 0.05
  INTEGER :: index, status, lines = 10000
  CHARACTER(LEN = 85) :: msg

  OPEN(UNIT=1, FILE='coords.dat', STATUS='NEW', ACTION='WRITE', IOSTAT=status, IOMSG=msg)
  DO index = 0, lines
    noise = (RAND() * DISTORTION) - (DISTORTION / 2.0)
    x = (x + inc) * (1.0 + noise)
    noise = (RAND() * DISTORTION) - (DISTORTION / 2.0)
    y = (a * x + b) * (1.0 + noise)
    WRITE(1, '(F14.8,1X,F14.8)', IOSTAT=status) x, y
  END DO
  CLOSE(1)
END PROGRAM generate_coords
