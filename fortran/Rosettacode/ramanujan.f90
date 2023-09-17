program ramanujans_constant
implicit none 

integer, parameter :: p = 16
real (kind=p) :: ppi

! equivalent to above
!integer, parameter :: p = selected_real_kind(33, 4931)
!real (kind=p) :: ppi

ppi = 4*atan(1._p)
print *, ppi
print *, exp(ppi * sqrt(163._p))

end program

! 3.14159265358979323846264338327950280
! 262537412640768743.999999999999249212

