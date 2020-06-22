PROGRAM ex15_16
TYPE :: ptr
  REAL, DIMENSION(:), POINTER :: p
END TYPE ptr
TYPE(ptr), DIMENSION(4) :: p1
REAL, DIMENSION(4), TARGET :: a = [1., 2., 3., 4.]
REAL, DIMENSION(2), TARGET :: b = [5., 6.]
REAL, DIMENSION(3), TARGET :: c = [7., 8., 9.]
REAL, DIMENSION(5), TARGET :: d = [10., 11., 12., 13., 14.]
p1(1)%p => a
p1(2)%p => b
p1(3)%p => c
p1(4)%p => d
WRITE(*, '(F6.1,/)') p1(1)%p(2) + p1(4)%p(4) + p1(3)%p(3)
DO i = 1, 4
  WRITE(*, '(5F6.1)') p1(i)%p
END DO
END PROGRAM ex15_16
