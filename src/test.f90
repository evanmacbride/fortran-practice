program ifProg
implicit none

	real, dimension(5) :: marks
	real :: avg, sum
	integer :: n
	!character(len = 16) :: name
	marks = (/90.1,48.5,70.3,88.4,51.7/)
	!name = "Buhhh"
	!integer :: bleh
	!bleh = 2
	avg = 0
	sum = 0
	do n = 1, size(marks)
		sum = sum + marks(n)
	end do
	avg = sum / size(marks)
	print *, "AVERAGE: ", avg
	!gr: if (marks > 90.0) then
	!	do n = 1, 10
	!		marks = marks + 1
	!		name = trim(name)//trim(name)
	!		print *, name
	!	end do
	!else
	!	print *, "Complete Failure"
	!end if gr
end program ifProg
