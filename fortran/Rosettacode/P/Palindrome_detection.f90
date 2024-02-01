!   Fortran
!u# https://rosettacode.org/wiki/Palindrome_detection
!c# 2024-01-04 

program palindro

  implicit none

  character(len=*), parameter :: p = "ingirumimusnocteetconsumimurigni"

  print *, is_palindro_r(p)
  print *, is_palindro_r("anothertest")
  print *, is_palindro2(p)
  print *, is_palindro2("test")
  print *, is_palindro(p)
  print *, is_palindro("last test")

contains

! non-recursive
function is_palindro(t)
  logical :: is_palindro
  character(len=*), intent(in) :: t

  integer :: i, l

  l = len(t)
  is_palindro = .false.
  do i=1, l/2
     if ( t(i:i) /= t(l-i+1:l-i+1) ) return
  end do
  is_palindro = .true.
end function is_palindro

! non-recursive 2
function is_palindro2(t) result(isp)
  logical :: isp
  character(len=*), intent(in) :: t

  character(len=len(t)) :: s
  integer :: i

  forall(i=1:len(t)) s(len(t)-i+1:len(t)-i+1) = t(i:i)
  isp = ( s == t )
end function is_palindro2

!Recursive
  recursive function is_palindro_r (t) result (isp)

    implicit none
    character (*), intent (in) :: t
    logical :: isp

    isp = len (t) == 0 .or. t (: 1) == t (len (t) :) .and. is_palindro_r (t (2 : len (t) - 1))

  end function is_palindro_r

end program palindro



