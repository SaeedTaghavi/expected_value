program expectec_val
implicit none
    integer::i,x
    real::sumation,expected_val
    integer::num_occure(6)
sumation=0.0
num_occure=0
    do i=1,100000
        call random_integer(x)
	if (x>6 .or. x<1) then 
	    print*,"random integer is x>6 or x<1"
	end if
	num_occure(x)=num_occure(x)+1
	sumation = sumation + x
	expected_val=sumation/real(i)
	write(1,*)i,expected_val
    end do

    do i=1,6
	write(2,*)i,num_occure(i)
    end do
end program expectec_val


!return random integer in {1,2,3,4,5,6}
subroutine random_integer(x)
implicit none
integer::x
real::ran
    call random_number(ran)
    ran = ran * 6.0
    x=int(ran)+1
end subroutine random_integer

