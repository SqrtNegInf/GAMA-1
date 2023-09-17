module test_mod
  implicit none
contains
  elemental real function square(x)
    real, intent(in) :: x
    square = x*x
  end function
end module

program test_prog
  use test_mod
  implicit none

  real, allocatable :: x(:)
  allocate( x(3) )
  x = (/ 1.0, 2.0, 3.0, 4.0 /)

! real, dimension(4) :: x = (/ 1.0, 2.0, 3.0, 4.0 /)

  print *, square(x)
! deallocate( x )
end program
