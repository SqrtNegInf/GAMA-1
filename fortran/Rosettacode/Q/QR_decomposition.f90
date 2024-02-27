!   Fortran
!u# https://rosettacode.org/wiki/QR_decomposition
!c# 2024-01-18 

! library: LAPACK
! See the documentation for the DGEQRF and DORGQR routines. Here the example matrix is the magic square from Albrecht Dürer's Melencolia I.

program qrtask
    implicit none
    integer, parameter :: n = 4
    real(8) :: durer(n, n) = reshape(dble([ &
        16,  5,  9,  4, &
         3, 10,  6, 15, &
         2, 11,  7, 14, &
        13,  8, 12,  1  &
    ]), [n, n])
    real(8) :: q(n, n), r(n, n), qr(n, n), id(n, n), tau(n)
    integer, parameter :: lwork = 1024
    real(8) :: work(lwork)
    integer :: info, i, j

    q = durer
    call dgeqrf(n, n, q, n, tau, work, lwork, info)

    r = 0d0
    forall (i = 1:n, j = 1:n, j >= i) r(i, j) = q(i, j)

    call dorgqr(n, n, n, q, n, tau, work, lwork, info)

    qr = matmul(q, r)
    id = matmul(q, transpose(q))

    call show(4, durer, "A")
    call show(4, q, "Q")
    call show(4, r, "R")
    call show(4, qr, "Q*R")
    call show(4, id, "Q*Q'")
contains
    subroutine show(n, a, s)
        character(*) :: s
        integer :: n, i
        real(8) :: a(n, n)

        print *, s
        do i = 1, n
            print 1, a(i, :)
          1 format (*(f12.6,:,' '))
        end do
    end subroutine
end program

!Output:
! A
!   16.000000     3.000000     2.000000    13.000000
!    5.000000    10.000000    11.000000     8.000000
!    9.000000     6.000000     7.000000    12.000000
!    4.000000    15.000000    14.000000     1.000000
! Q
!   -0.822951     0.376971     0.361447    -0.223607
!   -0.257172    -0.454102    -0.526929    -0.670820
!   -0.462910    -0.060102    -0.576283     0.670820
!   -0.205738    -0.805029     0.509510     0.223607
! R
!  -19.442222   -10.904103   -10.595497   -18.516402
!    0.000000   -15.846152   -15.932298    -0.258437
!    0.000000     0.000000    -1.974168    -5.922505
!    0.000000     0.000000     0.000000    -0.000000
! Q*R
!   16.000000     3.000000     2.000000    13.000000
!    5.000000    10.000000    11.000000     8.000000
!    9.000000     6.000000     7.000000    12.000000
!    4.000000    15.000000    14.000000     1.000000
! Q*Q'
!    1.000000    -0.000000    -0.000000     0.000000
!   -0.000000     1.000000     0.000000     0.000000
!   -0.000000     0.000000     1.000000    -0.000000
!    0.000000     0.000000    -0.000000     1.000000
!
!
