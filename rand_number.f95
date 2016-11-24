program thomas_algorithm
implicit none
!double precision :: 
integer :: n,i,j
double precision, dimension(:,:), allocatable :: A
double precision, dimension(:), allocatable :: b
print*, "enter the dimension of the square matrix i.e n"
read* , n
allocate (A(n,n))
do j= 0,n
  do i= 0,n
  A(i,j) = 0
  end do
end do

call random_seed()
do i=0,n 
  call random_number (A(i,i)) 
end do

do i = 1,n
    call random_number(b(i))
end do







print* , A
end program thomas_algorithm
