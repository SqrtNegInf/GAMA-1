!   Fortran
!u# https://rosettacode.org/wiki/Factorial
!c# 2024-02-15 

program F

integer :: n = 16

print *, PRODUCT((/(i, i=1,n)/))
print *, RECURSIVE_FACTORIAL(n)

contains

INTEGER RECURSIVE FUNCTION RECURSIVE_FACTORIAL(X) RESULT(ANS)
    INTEGER, INTENT(IN) :: X
    IF (X <= 1) THEN
        ANS = 1
    ELSE
        ANS = X * RECURSIVE_FACTORIAL(X-1)
    END IF
END FUNCTION RECURSIVE_FACTORIAL

end program F
