!   Fortran
!u# https://rosettacode.org/wiki/One-dimensional_cellular_automata
!c# 2024-02-01 

PROGRAM LIFE_1D

  IMPLICIT NONE

  LOGICAL :: cells(20) = (/ .FALSE., .TRUE., .TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .FALSE., .TRUE., .FALSE., &
            &               .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .true., .TRUE., .FALSE., .FALSE. /)
  logical :: cellsp(20)
  INTEGER :: i

  cellsp = .false.

  DO while (any(cells .neqv. cellsp))
     WRITE(*, "(A,I0,A)", ADVANCE = "NO") "Generation ", i, ": "
     CALL Drawgen(cells)
     cellsp = cells
     CALL Nextgen(cells)
  END DO

CONTAINS

  SUBROUTINE Nextgen(cells)
    LOGICAL, INTENT (IN OUT) :: cells(:)
    LOGICAL :: left, centre, right
    INTEGER :: i

    left = .FALSE.
    DO i = 1, SIZE(cells)-1
       centre = cells(i)
       right = cells(i+1)
       IF (left .AND. right) THEN
          cells(i) = .NOT. cells(i)
       ELSE IF (.NOT. left .AND. .NOT. right) THEN
          cells(i) = .FALSE.
       END IF
       left = centre
    END DO
    cells(SIZE(cells)) = left .AND. right
  END SUBROUTINE Nextgen

  SUBROUTINE Drawgen(cells)
    LOGICAL, INTENT (IN OUT) :: cells(:)
    INTEGER :: i

    DO i = 1, SIZE(cells)
       IF (cells(i)) THEN
          WRITE(*, "(A)", ADVANCE = "NO") "#"
       ELSE
          WRITE(*, "(A)", ADVANCE = "NO") "_"
       END IF
    END DO
    WRITE(*,*)
  END SUBROUTINE Drawgen

END PROGRAM LIFE_1D
