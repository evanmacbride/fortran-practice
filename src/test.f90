program mainprog
implicit none

	real :: a, b
	a = 2.0
	b = 3.0

	print *, "Before calling swap"
	print *, "a = ", a
	print *, "b = ", b
	call swap(a, b)

	print *, "After calling swap"
	print *, "a = ", a
	print *, "b = ", b

contains
	subroutine swap(x, y)
		real :: x, y, temp
		temp = x
		x = y
		y = temp
	end subroutine swap
end program mainprog
