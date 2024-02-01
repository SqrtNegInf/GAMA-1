!   Fortran
!u# https://rosettacode.org/wiki/Pick_random_element
!c# 2024-01-06 

program pick_random
  implicit none

  integer :: i, j, n
  integer, allocatable :: seed(:)
  integer :: a(20) = (/ (i, i = 1, 20) /)
  real :: r
  
! j = 47;
! call random_init(.true., .true.) !! not with older 'gofortran'

  call random_number(r)
  write(*,*) r
  write(*,*) a(int(r*size(a)) + 1)

  call random_number(r)
  write(*,*) r
  write(*,*) a(int(r*size(a)) + 1)

  call random_seed(size = n)
  write(*,*) 'n: ', n
  allocate(seed(n))
  call random_seed(get=seed)
  write(*,*) seed
  call random_seed(get=seed)
  write(*,*) seed
end program



