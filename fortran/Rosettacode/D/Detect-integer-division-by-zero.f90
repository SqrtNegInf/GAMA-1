!   Fortran
!u# https://rosettacode.org/wiki/Detect_division_by_zero
!c# 2024-02-01 

! both compilers fail equally well

! Integer division by zero. No detection.

program    rosetta_integer_divbyzero
   implicit none
   integer :: normal,zero,answer
   normal = 1
   zero = 0
   answer = normal/ zero
   write(*,*) answer
end program rosetta_integer_divbyzero
