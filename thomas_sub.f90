subroutine thomas_alg(a,b,c,d,n,x)
        integer :: i,n
        real(8), dimension(:), allocatable,intent(in) :: a,b,c,d
        real(8), dimension(:), allocatable :: x,b2,d2
        allocate(b2(n))
        allocate(d2(n))
        b2 = b
        d2 = d
        do i = 2,n
            b2(i) = b2(i) - (a(i)/b2(i-1))*c(i-1)
            d2(i) = d2(i) - (a(i)/b2(i-1))*d2(i-1)
        end do
        
        x(n) = d2(n)/b2(n)
        do i = n-1,1,-1
            x(i) = (d2(i) - c(i)*x(i+1))/b2(i)
        end do

    end subroutine

  !  subroutine generate_x(a,b,c,d,n,x)
   !   call random_seed()
    !    do i = 1,n
     !       call random_number(x(i))
      !  end do
       ! do i = 1,n
        !    d(i) = a(i-1)*x(i-1) + b(i)*x(i) + c(i)*x(i+1)
        !end do
    !end subroutine
