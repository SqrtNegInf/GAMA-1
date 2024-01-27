!   Fortran
!u# https://rosettacode.org/wiki/N-queens_problem
!c# 2024-01-27 

program queens
    implicit none
    integer ::       n, m
   !integer :: numq, n, m
    do n = 4, 16
        m = numq(n)
        print *, n, m
    end do

contains

function numq(n)
    implicit none
    integer :: i, n, m, a(n), numq
    logical :: up(2*n - 1), down(2*n - 1)
    do i = 1, n
        a(i) = i
    end do
    up = .true.
    down = .true.
    m = 0
    call sub(1)
    numq = m
contains
    recursive subroutine sub(i)
        integer :: i, j, k, p, q, s
        do k = i, n
            j = a(k)
            p = i + j - 1
            q = i - j + n
            if(up(p) .and. down(q)) then
                if(i == n) then
                    m = m + 1
                else
                    up(p) = .false.
                    down(q) = .false.
                    s = a(i)
                    a(i) = a(k)
                    a(k) = s
                    call sub(i + 1)
                    up(p) = .true.
                    down(q) = .true.
                    s = a(i)
                    a(i) = a(k)
                    a(k) = s
                end if
            end if
        end do
    end subroutine
end function

end program
