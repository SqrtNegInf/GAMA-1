!   Fortran
!u# https://rosettacode.org/wiki/Probabilistic_choice
!c# 2024-01-24 

PROGRAM PROBS

  IMPLICIT NONE

  INTEGER, PARAMETER :: trials = 1000000
  INTEGER :: i, j, probcount(8) = 0
  REAL :: expected(8), mapping(8), rnum
  CHARACTER(6) :: items(8) = (/ "aleph ", "beth  ", "gimel ", "daleth", "he    ", "waw   ", "zayin ", "heth  " /)

  expected(1:7) = (/ (1.0/i, i=5,11) /)
  expected(8) = 1.0 - SUM(expected(1:7))
  mapping(1) = 1.0 / 5.0
  DO i = 2, 7
     mapping(i) = mapping(i-1) + 1.0/(i+4.0)
  END DO
  mapping(8) = 1.0

  DO i = 1, trials
     CALL RANDOM_NUMBER(rnum)
     DO j = 1, 8
        IF (rnum < mapping(j)) THEN
           probcount(j) = probcount(j) + 1
           EXIT
        END IF
     END DO
  END DO

  WRITE(*, "(A,I10)") "Trials:             ", trials
  WRITE(*, "(A,8A10)") "Items:             ", items
  WRITE(*, "(A,8F10.6)") "Target Probability:  ", expected
  WRITE(*, "(A,8F10.6)") "Attained Probability:", REAL(probcount) / REAL(trials)

ENDPROGRAM PROBS


!Trials:                1000000
!Items:                 aleph     beth      gimel     daleth    he        waw       zayin     heth
!Target Probability:    0.200000  0.166667  0.142857  0.125000  0.111111  0.100000  0.090909  0.063456
!Attained Probability:  0.199631  0.166907  0.142488  0.124920  0.110906  0.099943  0.091775  0.063430


