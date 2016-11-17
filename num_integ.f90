PROGRAM numerical_integration
! A calculation of integral of a function using rectangular, midpoint, trapezoidal, Simpson methods.
! Assigned by Prof. Emmanuel Leriche

IMPLICIT NONE
!declare variable when they appear
INTEGER :: n, i, k
REAl :: a, b, h, x, l, y, m, q, right_area, left_area, area

a=0
b=0
right_area= 0
left_area= 0
area= 0
y= 0
m= 0
n= 0
q= 0

WRITE (*,*) 'integration using numerical methods'

WRITE(*,*) 'Enter the lower bound:' 
READ (*,*) a
WRITE(*,*) 'Enter the upper bound:' 
READ (*,*) b

WRITE (*,*) 'Number of divisons:'
READ (*,*) n
h= (b-a)/n

!IF (r==1) THEN !integration using rectangular method


  
  DO  i = 0, n-1
  l = a + h*i
  y = y + (2+sin(2*sqrt(l)))
  !y = y + (5*l**2+2*l+3)
  !y = y + (l**4-l**3)
  !y = y + (exp(-l**2))
  !y = y + 1/l 
  END DO
  right_area = y*h

  DO  k = 1, n
  m = a + h*k
  q = q + (2+sin(2*sqrt(m)))
  !q = q + 2*(5*m**2+2*m+3)
  !q = q + 2*(m**4-m**3)
  !q = q + 2*(exp(-m**2))
  !q = q + 2/m 
  END DO
  left_area = q*h

WRITE (*,*) ' Area with rectangular method= right, left :', right_area, left_area

y= 0
m= 0
l= 0
!ELSE IF(r==2) THEN !integration using midpoint method
  DO  i = 0, n-1
  l = a + h*(i+.5)
  y = y + (2+sin(2*sqrt(l)))
  !y = y + (5*l**2+2*l+3)
  !y = y + (l**4-l**3)
  !y = y + (exp(-l**2))
  !y = y + 1/l 
  END DO
  area = y*h
WRITE (*,*) ' Area with midpoint method=', area

!ELSE IF (r==3) THEN !integration using trapezoidal method

y= 0
m= 0
l= 0
  DO  i = 1, n-1
  l = a + h*i
  y = y + (2*(2+sin(2*sqrt(l)))) !involves multiplication by 2
  !y = y + (5*l**2+2*l+3)
  !y = y + (l**4-l**3)
  !y = y + (exp(-l**2))
  !y = y + 1/l 
  END DO
area =h/2* (y + (2+sin(2*sqrt(a))) + (2+sin(2*sqrt(b))))
!area = h/2*(y + (5*a**2+2*a+3) +(5*b**2+2*b+3))
!area = h/2*(y + (a**4-a**3) + (b**4-b**3))
!area = h/2*(y + (exp(-a**2)) + (exp(-b**2)))
!area = h/2*(y + 1/a + 1/b)

WRITE (*,*) ' Area with trapezoidal method=', area 

!ELSE IF (r==4) THEN !integration by Simpson's one third method

y= 0
m= 0
l= 0
q= 0 
  DO  i = 0, n-2, 2
  l = (a+h) + (h*i)
  y = y + 4*(2+sin(2*sqrt(l)))
  !y = y + 4*(5*l**2+2*l+3)
  !y = y + 4*(l**4-l**3)
  !y = y + 4*(exp(-l**2))
  !y = y + 4/l 
  END DO
  
  DO  k = 0, n-4, 2
  m = (a+2*h) + (h*k)
  q = q + 2*(2+sin(2*sqrt(m)))
  !q = q + 2*(5*m**2+2*m+3)
  !q = q + 2*(m**4-m**3)
  !q = q + 2*(exp(-m**2))
  !q = q + 2/m 
  END DO
  
area =h/3* (y + q + (2+sin(2*sqrt(a))) + (2+sin(2*sqrt(b))))
!area = h/3*(y + q +(5*a**2+2*a+3) +(5*b**2+2*b+3))
!area = h/3*(y + q + (a**4-a**3) + (b**4-b**3))
!area = h/3*(y + q+ (exp(-a**2)) + (exp(-b**2)))
!area = h/3*(y + q + 1/a + 1/b)

WRITE (*,*) ' Area with Simpson one third method=', area 

!ELSE
WRITE (*,*) 'Please enter 1, 2, 3 or 4'
!END IF
END PROGRAM numerical_integration 
