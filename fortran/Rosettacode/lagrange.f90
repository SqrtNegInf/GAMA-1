PROGRAM lagrangian_polynomial
IMPLICIT NONE
REAL::x(4),y(4),s=0.0,p,k
INTEGER::i,j,n

PRINT *, "============================================" 
PRINT *, "Program for Lagrange interpolation method - [BY - www.BottomScience.com]" 
PRINT *, "============================================"

!POINTS
x = (/ 3.35,3.40,3.50,3.60 /)
y = (/ 0.2985,0.294118,0.285714,0.277778 /)

PRINT *,'LAGRANGE INTERPOLATION'

PRINT *,'Number of terms?'
READ(*,*)n

PRINT *,'ENTER THE DATA POINT TO CALCULATE THE VALUE OF POLYNOMIAL'
READ(*,*)k

DO i=1,n
  p=1.0
  DO j=1,n
    IF(i .ne. j) THEN
      p=p*((k-x(j))/(x(i)-x(j)))
  ENDIF
ENDDO

s=s+(p*y(i)) 
ENDDO
PRINT *,"CALCULATED VALUE OF POLYNOMIAL - ",s

END PROGRAM
