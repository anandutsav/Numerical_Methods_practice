!Newton divided difference program
!Assigned by Prof. Emmanuel Leriche

PROGRAM Newton_DD
IMPLICIT NONE
INTEGER :: i, n, j, k
REAL :: a, p, s
REAL, DIMENSION (20) :: x
REAL, DIMENSION (20,20) :: y
k= 0

write(*,*) 'To find an interpolating polynomial using Newtons divided difference method.' 

write(*,*)'Enter the number of known function values:'
read(*,*)n

write(*,*)'Enter the values of x and the corresponding function values:'

  DO i=1,n
  read(*,*) x(i),y(i,1)    ! storage of given points in the arrays
  END DO 

!write(*,*) 'Enter the value of x for which function value is to be found:'
!read(*,*) a

  DO j= 2, n
  k=k+1
    DO i= 1,n-k
    y(i,j)=(y(i+1,j-1)-y(i,j-1))/(x(i+k)-x(i))
    END DO
  END DO

!write(*,*)'The divided difference table is : '
write(*,*)'The coefficients for the Newton DD polynomial are : '
!write(*,*)
 !DO i=1,n
  !write(*,3)x(i)
  !3 format(1x,F10.3)
    DO j=1,n !n-1=1 changed to n
    write(*,3) y(1,j) !i changed to 1
    3 format(1x,F10.3)   
    END DO
    !write(*,*)
   !write(*,*)
  !END DO

!s=y(1,1)
!DO j=2,n
!p=1
!DO i=1,j-1
!p=p*(a-x(i))
!END DO
!s=s+p*y(1,j)
!END DO
!write(*,4)s
!4 format(1x,'The corresponding function value is:',F10.3)

END PROGRAM Newton_DD
