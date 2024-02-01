!   Fortran
!u# https://rosettacode.org/wiki/Roots_of_a_function
!c# 2024-01-07 

PROGRAM ROOTS_OF_A_FUNCTION

  IMPLICIT NONE

  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15)
  REAL(dp) :: f1, e, x, step, value
  LOGICAL :: s 

!INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15)
INTEGER :: i=1, limit=100
!REAL(dp) :: d, e, f, x, x1, x2
 REAL(dp) :: f2, d, x1, x2

f1(x) = x*x*x - 3.0_dp*x*x + 2.0_dp*x
f2(x) = x*x*x - 3.0_dp*x*x + 2.0_dp*x

  x = -1.0_dp ; step = 1.0e-6_dp ; e = 1.0e-9_dp

  s = (f1(x) > 0)
  DO WHILE (x < 3.0)
    value = f1(x)
    IF(ABS(value) < e) THEN
      WRITE(*,"(A,F12.9)") "Root found at x =", x
      s = .NOT. s
    ELSE IF ((value > 0) .NEQV. s) THEN
      WRITE(*,"(A,F12.9)") "Root found near x = ", x
      s = .NOT. s
    END IF
    x = x + step
  END DO


! The following approach uses the Secant Method to numerically find one root. Which root is found will depend on the start values x1 and x2 and if these are far from a root this method may not converge.

x1 = -1.0_dp ; x2 = 3.0_dp ; e = 1.0e-15_dp

DO 
  IF (i > limit) THEN
    WRITE(*,*) "Function not converging"
    EXIT
  END IF
  d = (x2 - x1) / (f2(x2) - f2(x1)) * f2(x2)
  IF (ABS(d) < e) THEN
    WRITE(*,"(A,F18.15)") "Root found at x = ", x2
    EXIT    
  END IF
  x1 = x2
  x2 = x2 - d
  i = i + 1
END DO



END PROGRAM ROOTS_OF_A_FUNCTION
