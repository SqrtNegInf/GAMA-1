MODULE fuzzy_maths
    IMPLICIT NONE
    TYPE fuzzy
      REAL :: value, error
    END TYPE fuzzy
    INTERFACE OPERATOR (+)
      MODULE PROCEDURE fuzzy_plus_fuzzy
    END INTERFACE
  CONTAINS
    FUNCTION fuzzy_plus_fuzzy(first, second) RESULT (sum)
      TYPE(fuzzy), INTENT(IN) :: first, second  ! INTENT(IN) required
      TYPE(fuzzy)             :: sum
      sum%value = first%value + second%value
      sum%error = SQRT(first%error**2 + second%error**2)
    END FUNCTION fuzzy_plus_fuzzy
END MODULE fuzzy_maths

PROGRAM test_fuzzy
USE fuzzy_maths 
! implicit none 
TYPE(fuzzy) a, b, c
  A = fuzzy(15.0, 4.0) ;  b = fuzzy(12.5, 3.0)
  c = a + b
  PRINT *, c
END PROGRAM test_fuzzy
