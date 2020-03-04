PROGRAM generate_coords
IMPLICIT NONE
  REAL :: x = 0, y ! Coordinates to write to a file
  REAL, PARAMETER :: a = 9.81, b = 2.13 ! Acceleration due to gravity and initial velocity
  REAL :: inc = 0.1 ! Time increment
  REAL :: noise ! Randomly generated value to add
  REAL, PARAMETER :: DISTORTION = 1.05
  INTEGER :: index, status, lines = 100
  CHARACTER(LEN = 85) :: msg

  OPEN(UNIT=1, FILE='coords.dat', STATUS='NEW', ACTION='WRITE', IOSTAT=status, IOMSG=msg)
  DO index = 0, lines
    noise = RAND() * DISTORTION
    noise = noise - (DISTORTION / 2)
    x = (x + inc) * noise
    y = (a * x + b) * noise
    WRITE(1, *, IOSTAT=status) x, y
  END DO
  CLOSE(1)
END PROGRAM generate_coords
