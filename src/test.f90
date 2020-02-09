program whereStatement
implicit none

	integer :: a(3, 5), i, j

	do i = 1, 3
		do j = 1, 5
			a(i, j) = j - i
		end do
	end do

	print *, "ARRAY A:"

	do i = lbound(a, 1), ubound(a, 1)
		write(*, *) (a(i, j), j = lbound(a, 2), ubound(a, 2))
	end do

	where (a < 0)
		a = 1
	elsewhere (a == 0)
		a = 5
	elsewhere
		a = 99
	end where

	print *, "NEW ARRAY A:"
	do i = lbound(a, 1), ubound(a, 1)
		write(*, *) (a(i, j), j = lbound(a, 2), ubound(a, 2))
	end do

end program whereStatement
