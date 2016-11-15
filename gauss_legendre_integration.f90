PROGRAM GL_Integration
IMPLICIT NONE
INTEGER :: i
REAL :: a,b,c,d,e,f,l,u,p,q,y

REAL, DIMENSION (2) :: a2
REAL, DIMENSION (2) :: w2
REAL, DIMENSION (3) :: a3
REAL, DIMENSION (3) :: w3
REAL, DIMENSION (4) :: a4
REAL, DIMENSION (4) :: w4
REAL, DIMENSION (5) :: a5
REAL, DIMENSION (5) :: w5
REAL, DIMENSION (6) :: a6
REAL, DIMENSION (6) :: w6
REAL, DIMENSION (7) :: a7
REAL, DIMENSION (7) :: w7

a2 = (/0.577350269,-0.577350269/)
w2 = (/1,1/)
a3 =(/0.774596669,-0.774596669,0.0/)
w3 =(/0.555555556,0.555555556,0.888888889/)
a4 =(/0.861136312,-0.861136312,0.339981044,-0.339981044/)
w4 =(/0.347854845,0.347854845,0.652145155,0.652145155/)
a5 =(/0.906179846,-0.906179846,0.538469310,-0.538469310,0.0/)
w5 =(/0.236926885, 0.236926885,0.478628671,0.478628671,0.568888889/)
a6 =(/0.932469514,-0.932469514,0.661209387,-0.661209387,0.238619186,-0.238619186/)
w6 =(/0.171324492,0.171324492,0.360761573,0.360761573,0.467913935,0.467913935/)
a7 =(/0.949107912,-0.949107912,0.741531186,-0.741531186,0.405845151,-0.405845151,0.0/)
w7 =(/ 0.129484966,0.129484966,0.279705392,0.279705392,0.381830051,0.381830051,0.417959184/)

a=0
b=0
c=0
d=0
e=0
f=0

WRITE(*,*) 'Enter the lower bound:' 
READ (*,*) l
WRITE(*,*) 'Enter the upper bound:' 
READ (*,*) u
p= (u-l)/2
q= (u+l)/2

  DO i= 1,2 
  !a = a + ((p*w2(i))*(2+sin(2*sqrt(q-p*a2(i)))))
  !a = a + ((p*w2(i))*(5*(q-p*a2(i))**2+2*(q-p*a2(i))+3))
  !a = a + ((p*w2(i))*((q-p*a2(i))**4-(q-p*a2(i))**3))
  !a = a + (p*w2(i))*(exp(-(q-p*a2(i))**2))
   a = a + (p*w2(i))/(q-p*a2(i))
  
  END DO

  DO i= 1,3 
  !b = b + ((p*w3(i))*(2+sin(2*sqrt(q-p*a3(i)))))
  !b = b + ((p*w3(i))*(5*(q-p*a3(i))**2+2*(q-p*a3(i))+3))
  !b = b + ((p*w3(i))*((q-p*a3(i))**4-(q-p*a3(i))**3))
  !b = b + (p*w3(i))*(exp(-(q-p*a3(i))**2))
   b = b + (p*w3(i))/(q-p*a3(i))
  
  END DO

  DO i= 1,4 
  !c = c + ((p*w4(i))*(2+sin(2*sqrt(q-p*a4(i)))))
  !c = c + ((p*w4(i))*(5*(q-p*a4(i))**2+2*(q-p*a4(i))+3))
  !c = c + ((p*w4(i))*((q-p*a4(i))**4-(q-p*a4(i))**3))
  !c = c + (p*w4(i))*(exp(-(q-p*a4(i))**2))
  c = c + (p*w4(i))/(q-p*a4(i)) 
  
  END DO

  DO i= 1,5 
  !d = d + ((p*w5(i))*(2+sin(2*sqrt(q-p*a5(i)))))
  !d = d + ((p*w5(i))*(5*(q-p*a5(i))**2+2*(q-p*a5(i))+3))
  !d = d + ((p*w5(i))*((q-p*a5(i))**4-(q-p*a5(i))**3))
  !d = d + (p*w5(i))*(exp(-(q-p*a5(i))**2))
  d= d + (p*w5(i))/(q-p*a5(i))
  
  END DO

  DO i= 1,6
  !e = e + ((p*w6(i))*(2+sin(2*sqrt(q-p*a6(i)))))
  !e = e + ((p*w6(i))*(5*(q-p*a6(i))**2+2*(q-p*a6(i))+3))
  !e = e + ((p*w6(i))*((q-p*a6(i))**4-(q-p*a6(i))**3))
  !e = e + (p*w6(i))*(exp(-(q-p*a6(i))**2))
   e = e + (p*w6(i))/(q-p*a6(i))
  
  END DO

  DO i= 1,7
  !f = f + ((p*w7(i))*(2+sin(2*sqrt(q-p*a7(i)))))
  !f = f + ((p*w7(i))*(5*(q-p*a7(i))**2+2*(q-p*a7(i))+3))
  !f = f + ((p*w7(i))*((q-p*a7(i))**4-(q-p*a7(i))**3))
  !f = f + (p*w7(i))*(exp(-(q-p*a7(i))**2))
  f= f + (p*w7(i))/(q-p*a7(i))
  
  END DO

WRITE(*,*) 'GL approx. with 2 points=' , a
WRITE(*,*) 'GL approx. with 3 points=' , b
WRITE(*,*) 'GL approx. with 4 points=', c
WRITE(*,*) 'GL approx. with 5 points=', d
WRITE(*,*) 'GL approx. with 6 points=', e
WRITE(*,*) 'GL approx. with 7 points=', f

END PROGRAM GL_Integration
