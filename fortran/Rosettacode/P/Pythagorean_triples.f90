!   Fortran
!u# https://rosettacode.org/wiki/Pythagorean_triples
!c# 2024-01-18 

module triples
  implicit none

  integer :: max_peri, prim, total
  integer :: u(9,3) = reshape((/ 1, -2, 2,  2, -1, 2,  2, -2, 3, &
                                 1,  2, 2,  2,  1, 2,  2,  2, 3, &
                                -1,  2, 2, -2,  1, 2, -2,  2, 3 /), &
                                (/ 9, 3 /))

contains

recursive subroutine new_tri(in)
  integer, intent(in) :: in(:)
  integer :: i
  integer :: t(3), p

  p = sum(in)
  if (p > max_peri) return

  prim = prim + 1
  total = total + max_peri / p
  do i = 1, 3
    t(1) = sum(u(1:3, i) * in)
    t(2) = sum(u(4:6, i) * in)
    t(3) = sum(u(7:9, i) * in)
    call new_tri(t);
  end do
end subroutine new_tri
end module triples

program Pythagorean
  use triples
  implicit none

  integer :: seed(3) = (/ 3, 4, 5 /)

  max_peri = 10
  do
    total = 0
    prim = 0
    call new_tri(seed)
    write(*, "(a, i10, 2(i10, a))") "Up to", max_peri, total, " triples",  prim, " primitives"
    if(max_peri == 100000000) exit
    max_peri = max_peri * 10
  end do
end program Pythagorean

!Output:Up to         10          0 triples         0 primitives
!Up to        100         17 triples         7 primitives
!Up to       1000        325 triples        70 primitives
!Up to      10000       4858 triples       703 primitives
!Up to     100000      64741 triples      7026 primitives
!Up to    1000000     808950 triples     70229 primitives
!Up to   10000000    9706567 triples    702309 primitives
!Up to  100000000  113236940 triples   7023027 primitives


