!   Fortran
!u# https://rosettacode.org/wiki/Greatest_common_divisor
!c# 2024-01-27 

program gcd_examples
implicit none
integer :: i  = 0, u  = 40902, v  = 24140

write (*, '("gcd      (40902, 24140) = ", I0)') gcd (40902, 24140)
write (*, '("gcd_rec  (40902, 24140) = ", I0)') gcd_rec (40902, 24140)
call gcd_iter(i, u, v); write (*, '("gcd_iter (40902, 24140) = ", I0)') i

contains

!Recursive Euclid algorithm
recursive function gcd_rec(u, v) result(gcd)
    integer             :: gcd
    integer, intent(in) :: u, v

    if (mod(u, v) /= 0) then
        gcd = gcd_rec(v, mod(u, v))
    else
        gcd = v
    end if
end function gcd_rec

!Iterative Euclid algorithm
 subroutine gcd_iter(value, u, v)
  integer, intent(out) :: value
  integer, intent(inout) :: u, v
  integer :: t

  do while( v /= 0 )
     t = u
     u = v
     v = mod(t, v)
  enddo
  value = abs(u)
end subroutine gcd_iter

!A different version, and implemented as function
function gcd(v, t)
  integer :: gcd
  integer, intent(in) :: v, t
  integer :: c, b, a

  b = t
  a = v
  do
     c = mod(a, b)
     if ( c == 0) exit
     a = b
     b = c
  end do
  gcd = b ! abs(b)
end function gcd

end program
