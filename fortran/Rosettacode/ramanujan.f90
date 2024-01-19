program ramanujans_constant
implicit none 

! standard
!integer, parameter :: p = 16

! does not suffice [to try, put above  'implicit']
use, intrinsic :: iso_fortran_env, only : p => real128

!integer, parameter :: p = selected_real_kind(33, 4931)  ! 128-bit reals [works]
!integer, parameter :: p = selected_real_kind(15,  307)  !  64-bit reals [fails]
!integer, parameter :: p = selected_real_kind(19)        ! minimum required?

real (kind=p) :: ppi

ppi = 4*atan(1._p)
print *, ppi
print *, exp(ppi * sqrt(163._p))

end program

! 3.14159265358979323846264338327950280
! 262537412640768743.999999999999249212

