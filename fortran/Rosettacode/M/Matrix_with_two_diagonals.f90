!   Fortran
!u# https://rosettacode.org/wiki/Matrix_with_two_diagonals
!c# 2024-01-28 

program prog
    implicit none
    integer :: n = 7, i, j, j1, j2
    integer, dimension(100, 100) :: a = 8

!    a = 0
!    n = 7

    j1 = 1
    j2 = n
    do i = 1, n
!       do j = 1, n
!           a(i, j) = 0.
!       end do
        a(i, j1) = 1
        a(i, j2) = 1
        j1 = j1 + 1
        j2 = j2 - 1
    end do

    do i = 1, n
        print *, (a(i, j), j=1,n)
    end do

end
