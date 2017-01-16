''module error
implicit none
private

public rms_error

contains
    real(8) function rms_error(X,Y,n)
        implicit none
        integer :: i,n
        real(8), dimension(:), allocatable :: X,Y
        real(8) :: res

        res = 0.0
        do i = 1,n
            res = res + (X(i)-Y(i))**2
        end do
        res = sqrt(res/n)
        return

    end function rms_error
end module error

   

