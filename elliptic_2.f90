program elliptic_2
use thomas
use error
implicit none
real(8), parameter :: pi=4.0d0*datan(1.0d0)
integer div,j
real(8),dimension(:), allocatable :: a,b,c,d,x
real(8),dimension(:), allocatable :: U,l
real(8) :: mesh_s

div = 4
open(unit=11, file='elliptic_2.dat', status='replace', action='write')

do while (div <=100000)

allocate(a(div+1))
allocate(b(div+1))
allocate(c(div+1))
allocate(d(div+1))
allocate(x(div+1))
allocate(l(div+1))
allocate(U(div+1))
mesh_s = pi/div
do j = 1,div+1
    l(j) = (j-1)*mesh_s
end do

do j=1,div+1
    U(j) = cos(l(j))
end do


do j = 1, div
    a(j) = 1
    b(j) = -2
    c(j) = 1
    x(j) = 0
end do

do j = 1,div
    b(j) = -2
    x(j) = 0
end do
c(1) = 2 
b(div+1) = -2*mesh_s  -2
a(div+1) = 2

do j = 1,div
    d(j) = (mesh_s**2) * (-cos(l(j)))
end do
d(div+1) = (mesh_s**2) * (-cos(l(div+1))) + 2 * mesh_s

call thomas_alg(a,b,c,d,div+1,x)
print*, 'div=',div,'error =',rms_error(U,x,div+1)
write(11,*)mesh_s, rms_error(U,x,div+1)

deallocate(a)
deallocate(b)
deallocate(c)
deallocate(d)
deallocate(x)
deallocate(l)
deallocate(U)
div = div * 10
end do

end program elliptic_2
