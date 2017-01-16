program elliptic_1
!use thomas
use error
implicit none
real(kind=16), parameter :: pi=4.0d0*datan(1.0d0)
integer M,j
real(kind=16),dimension(:), allocatable :: a,b,c,d,x
real(kind=16),dimension(:), allocatable :: U,l
real(kind=16) :: mesh_s

div = 2
open(unit=10, file='elliptic.dat', status='replace', action='write')

do while (div <= 100000)

allocate(a(div))
allocate(b(div))
allocate(c(div))
allocate(d(div))
allocate(x(div))
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

d(1) = (mesh_s**2) * (-cos(l(j))) - 1
do j = 2,div-1
    d(j) = (mesh_s**2) * (-cos(l(j)))
end do
d(div) = (mesh_s**2) * (-cos(l(j))) + 1 

call thomas_alg(a,b,c,d,div,x)
print*, 'M=',div,'error =',rms_error(U,x,div)
write(10,*) mesh_s, rms_error(U,x,div)

deallocate(a)
deallocate(b)
deallocate(c)
deallocate(d)
deallocate(x)
deallocate(l)
deallocate(U)
div = div * 10
end do

end program elliptic_1
