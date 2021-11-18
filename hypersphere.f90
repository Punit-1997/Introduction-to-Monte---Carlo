!                                         Programe To  illustrate The Monte -Carlo  Programe
!
!  In this programe we will understand how to use monte - carlo method to calculate the integration of hyperspere in general dimension  
!
!
!
	
	program hypersphere
	implicit none
	real :: r,x,sumx2,area
	integer :: i,j,d,n,count=0
	write(*,*)"How many random number do you want ?"
	read(*,*) n
	write(*,*)"What are dimensions of space d & radius r of standard hypersphere ?"
	read(*,*) d,r
	do j=1,n
		sumx2=0.0
		do i=1,d
			call random_number(x)
			sumx2=sumx2+r*r*x*x
		end do
		if(sumx2<=r*r)then
			count=count+1
		end if
	end do
	area=2.0**real(d)*r**d*real(count)/real(n)
	write(*,*)"The volume of hypersphere using monte carlo method is",area
	end program hypersphere
	
