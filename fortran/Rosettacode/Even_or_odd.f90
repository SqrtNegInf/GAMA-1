!   Fortran
!u# https://rosettacode.org/wiki/Even_or_odd
!c# 2024-01-28 

! Please find the compilation and example run in the comments at the beginning of the FORTRAN 2008 source.  Separating the bit 0 parity module from the main program enables reuse of the even and odd functions.  Even and odd, with scalar and vector interfaces demonstrate the generic function capability of FORTRAN 90. 

module bit0parity

  interface odd
    module procedure odd_scalar, odd_list
  end interface

  interface even
    module procedure even_scalar, even_list
  end interface

contains

  logical function odd_scalar(a)
    implicit none
    integer, intent(in) :: a
    odd_scalar = btest(a, 0)
  end function odd_scalar

  logical function even_scalar(a)
    implicit none
    integer, intent(in) :: a
    even_scalar = .not. odd_scalar(a)
  end function even_scalar

  function odd_list(a) result(rv)
    implicit none
    integer, dimension(:), intent(in) :: a
    logical, dimension(size(a)) :: rv
    rv = btest(a, 0)
  end function odd_list

  function even_list(a) result(rv)
    implicit none
    integer, dimension(:), intent(in) :: a
    logical, dimension(size(a)) :: rv
    rv = .not. odd_list(a)
  end function even_list

end module bit0parity

program oe
  use bit0parity
  implicit none
  integer :: i
  integer, dimension(13) :: j
  write(6,'(a2,2a8)') 'n', 'odd', 'even'
  write(6, '(i2,2l5)') (i, odd_scalar(i), even_scalar(i), i=-6,6)
  do i=-6, 6
    j(i+7) = i
  end do
  write(6, '((13i3),a8/(13l3),a8/(13l3),a8)') j, 'n', odd(j), 'odd', even(j), 'even'
end program oe
