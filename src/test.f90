program ifProg
implicit none

	real :: marks
	marks = 89.99
	gr: if (marks > 90.0) then
		print *, "Grade A"
	else
		print *, "Complete Failure"
	end if gr
end program ifProg
