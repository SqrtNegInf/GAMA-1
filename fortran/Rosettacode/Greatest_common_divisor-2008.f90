!   Fortran
!u# https://rosettacode.org/wiki/Greatest_common_divisor
!c# 2024-01-27 

!Iterative binary algorithm in Fortran 2008
!Fortran 2008 introduces new intrinsic functions for integer operations that nowadays usually have hardware support, such as TRAILZ to count trailing zeros.

! Stein’s algorithm implemented in Fortran 2008.
! Translated from my implementation for ATS/Postiats.

program test_gcd
  implicit none

! by re-arranging, don't need to dup interface info. (older 'gfortran' doesn't like this)
! interface
!    elemental function gcd (u, v) result (d)
!      integer, intent(in) :: u, v
!      integer :: d
!    end function gcd
! end interface

  write (*, '("gcd (0, 0) = ", I0)') gcd (0, 0)
  write (*, '("gcd (0, 10) = ", I0)') gcd (0, 10)
  write (*, '("gcd (-6, -9) = ", I0)') gcd (-6, -9)
  write (*, '("gcd (64 * 5, -16 * 3) = ", I0)') gcd (64 * 5, -16 * 3)
  write (*, '("gcd (40902, 24140) = ", I0)') gcd (40902, 24140)
  write (*, '("gcd (-40902, 24140) = ", I0)') gcd (-40902, 24140)
  write (*, '("gcd (40902, -24140) = ", I0)') gcd (40902, -24140)
  write (*, '("gcd (-40902, -24140) = ", I0)') gcd (-40902, -24140)
  write (*, '("gcd (24140, 40902) = ", I0)') gcd (24140, 40902)

contains

elemental function gcd (u, v) result (d)
  implicit none
  integer, intent(in) :: u, v
  integer :: d

  integer :: x, y

  ! gcd(x,y) = gcd(u,v), but x and y are non-negative and x <= y.
  x = min (abs (u), abs (v))
  y = max (abs (u), abs (v))

  if (x == 0) then
     d = y
  else
     d = gcd_pos_pos (x, y)
  end if

contains

  elemental function gcd_pos_pos (u, v) result (d)
    integer, intent(in) :: u, v
    integer :: d

    integer :: n
    integer :: x, y
    integer :: p, q

    ! n = the number of common factors of two in u and v.
    n = trailz (ior (u, v))

    ! Remove the common twos from u and v, giving x and y.
    x = ishft (u, -n)
    y = ishft (v, -n)

    ! Make both numbers odd. One of the numbers already was odd.
    ! There is no effect on the value of their gcd.
    x = ishft (x, -trailz (x))
    y = ishft (y, -trailz (y))

    do while (x /= y)
       ! If x > y then swap x and y, renaming them p
       ! and q. Thus p <= q, and gcd(p,q) = gcd(x,y).
       p = min (x, y)
       q = max (x, y)

       x = p                    ! x remains odd.
       q = q - p
       y = ishft (q, -trailz (q)) ! Make y odd again.
    end do

    ! Put the common twos back in.
    d = ishft (x, n)
  end function gcd_pos_pos

end function gcd

end program test_gcd
