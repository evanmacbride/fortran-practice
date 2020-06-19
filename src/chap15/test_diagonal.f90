SUBROUTINE get_diagonal(ptr_a, ptr_b, error)
IMPLICIT NONE
INTEGER, DIMENSION(:,:), POINTER :: ptr_a ! Pointer to a square array
INTEGER, DIMENSION(:), POINTER :: ptr_b   ! Pointer to the output array
INTEGER, INTENT(OUT) :: error             ! Error flag
INTEGER :: i
INTEGER :: istat
INTEGER, DIMENSION(2) :: l_bound
INTEGER, DIMENSION(2) :: u_bound
INTEGER, DIMENSION(2) :: extent

! Check error conditions
error_1: IF (.NOT. ASSOCIATED(ptr_a)) THEN
  error = 1
ELSE IF (ASSOCIATED(ptr_b)) THEN
  error = 2
ELSE
  ! Check for square array
  l_bound = LBOUND(ptr_a)
  u_bound = UBOUND(ptr_a)
  extent = u_bound - l_bound + 1
  error_3: IF (extent(1) /= extent(2)) THEN
    error = 3
  ELSE
    ! Everything is OK so far. Allocate ptr_b
    ALLOCATE(ptr_b(extent(1)), STAT=istat)
    error_4: IF (istat /= 0) THEN
      error = 4
    ELSE
      ok: DO i = 1, extent(1)
        ptr_b(i) = ptr_a(l_bound(1)+i-1, l_bound(2)+i-1)
      END DO ok
      ! Reset error flag
      error = 0
    END IF error_4
  END IF error_3
END IF error_1
END SUBROUTINE get_diagonal

PROGRAM test_diagonal
IMPLICIT NONE
INTERFACE
  SUBROUTINE get_diagonal(ptr_a, ptr_b, error)
  INTEGER, DIMENSION(:,:), POINTER :: ptr_a
  INTEGER, DIMENSION(:), POINTER :: ptr_b
  INTEGER, INTENT(OUT) :: error
  END SUBROUTINE get_diagonal
END INTERFACE

INTEGER :: i, j, k
INTEGER :: istat
INTEGER, DIMENSION(:,:), POINTER :: ptr_a
INTEGER, DIMENSION(:), POINTER :: ptr_b
INTEGER :: error

! Call get_diagonal with nothing define to see what happens.
CALL get_diagonal(ptr_a, ptr_b, error)
WRITE(*, *) 'No pointers allocated: '
WRITE(*, *) ' Error = ', error
! Allocate both pointers and call the subroutine.
ALLOCATE(ptr_a(10,10), STAT=istat)
ALLOCATE(ptr_b(10), STAT=istat)
CALL get_diagonal(ptr_a, ptr_b, error)
WRITE(*, *) 'Both pointers allocated: '
WRITE(*, *) ' Error = ', error
! Allocate ptr_a only, but with unequal extents
DEALLOCATE(ptr_a, STAT=istat)
DEALLOCATE(ptr_b, STAT=istat)
ALLOCATE(ptr_a(-5:5,10), STAT=istat)
CALL get_diagonal(ptr_a, ptr_b, error)
WRITE(*, *) 'Array on ptr_a not square: '
WRITE(*, *) ' Error = ', error
! Allocate ptr_a only, initialize, and get results.
DEALLOCATE(ptr_a, STAT=istat)
ALLOCATE(ptr_a(-2:2,0:4), STAT=istat)
k = 0
DO j = 0, 4
  DO i = -2, 2
    k = k + 1
    ptr_a(i, j) = k
  END DO
END DO
CALL get_diagonal(ptr_a, ptr_b, error)
WRITE(*, *) 'ptr_a allocated & square; ptr_b not allocated: '
WRITE(*, *) 'Error = ', error
WRITE(*, *) ' Diag = ', ptr_b
END PROGRAM test_diagonal
