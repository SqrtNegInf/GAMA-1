!   Fortran
!u# https://rosettacode.org/wiki/Averages/Root_mean_square
!c# 2024-02-02 

program rms
implicit none
integer i
real, dimension(10) :: x = [(i,i=1,10)]

print *,sqrt( sum(x**2)/size(x) )

end program



