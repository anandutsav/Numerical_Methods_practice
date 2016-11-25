program thomas_algorithm
implicit none 
integer :: n,i,j
double precision, dimension(:,:), allocatable :: A
real, dimension(:), allocatable :: b
real, dimension(:), allocatable :: x
real, dimension(:), allocatable :: d
real :: error
print*, "enter the dimension of the square matrix i.e n"
read* , n
allocate (A(n,n))
allocate (b(n))
allocate (x(n))
allocate (d(n))
do j= 1,n
  do i= 1,n
  A(i,j) = 0
  end do
end do
call random_seed()
do i=1,n 
  call random_number (A(i,i)) 
end do

do i=2,n 
  call random_number (A(i-1,i)) 
end do

do i=2,n 
  call random_number (A(i,i-1)) 
end do

do i = 1,n
    call random_number(b(i))
end do
print* , b

!call matrix_transform(A,b,n)

do i=2,n
  A(i,i)= A(i,i)-((A(i,i-1)/A(i-1,i-1))*A(i-1,i))
  b(i)= b(i)-((A(i,i-1)/A(i-1,i-1))*b(i-1))
end do

x(n) = b(n)/A(n,n)


i=0

do i = n-1,1,-1
x(i) = b(i)-(A(i,i+1)*x(i+1))
end do

do i = 1,n
print*, x(i)
end do
print*,  
print*,  
d= matmul(A, x)
print* , d

do i =1,n
error = abs(d(i)-b(i))
write(*,*)  'error in x is (x1 to xn respectively) is :' , error
end do


end program thomas_algorithm

!module mysubs
 !contains
 !subroutine matrix_transform(A,b,n)
 !implicit none
 !integer :: i,n
 !real, dimension(:,:), allocatable :: A
 !real, dimension(:), allocatable :: b
 !do i=2,n
  !A(i,i)= A(i,i)-((A(i,i-1)/A(i-1,i-1))*A(i-1,i))
  !b(i)= b(i)-((A(i,i-1)/A(i-1,i-1))*b(i-1))
 !end do
 !end subroutine matrix_transform
!end module mysubs

!subroutine error_calc (A,x)
!implicit none
  !real,intent(in), dimension(:,:), allocatable :: A 
  !real,intent(in),dimension(:), allocatable :: x
  !real,intent(out) dimension(:), allocatable :: c
  !integer :: n
  !allocate (A(n,n))
  !allocate (x(n))
  !allocate (c(n))
  !c= matmul(A, x)
  !return
!end subroutine


 


