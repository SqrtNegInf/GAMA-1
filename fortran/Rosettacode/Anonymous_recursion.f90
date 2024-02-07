!   Fortran
!u# https://rosettacode.org/wiki/Anonymous_recursion
!c# 2024-02-07 

! only sort-of anon then...
! Since a hidden named function instead of an anonymous one seems to be ok with implementors, here is the Fortran version:

program fibo
implicit none

print *, fib(5)

contains

integer function fib(n)
  integer, intent(in) :: n
  if (n < 0 ) then
    write (*,*) 'Bad argument: fib(',n,')'
    stop
  else
    fib = purefib(n)
  end if
contains
  recursive pure integer function purefib(n) result(f)
    integer, intent(in) :: n
    if (n < 2 ) then
      f = n
    else
      f = purefib(n-1) + purefib(n-2)
    end if
  end function purefib
end function fib

end program fibo
