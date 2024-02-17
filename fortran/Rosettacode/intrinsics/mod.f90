program demo_mod
implicit none

!   print *, mod(-17.5, 5.2d0), modulo(-17.5, 5.2d0)                        ! helps
!   print *, mod(dble(-17.5), dble(5.2)), modulo(dble(-17.5), dble(5.2))    ! does not

   ! basics
    print *, mod( -17,  3 ), modulo( -17,  3 )
    print *, mod(  17, -3 ), modulo(  17, -3 )
    print *, mod(  17,  3 ), modulo(  17,  3 )  ! same
    print *, mod( -17, -3 ), modulo( -17, -3 )  ! same

    print *, mod(-17.5, 5.2), modulo(-17.5, 5.2)
    print *, mod( 17.5,-5.2), modulo( 17.5,-5.2)
    print *, mod( 17.5, 5.2), modulo( 17.5, 5.2)    ! same
    print *, mod(-17.5,-5.2), modulo(-17.5,-5.2)    ! same

  ! with a divisor of 1 the fractional part is returned
    print *, mod(-17.5, 1.0), modulo(-17.5, 1.0)
    print *, mod( 17.5,-1.0), modulo( 17.5,-1.0)
    print *, mod( 17.5, 1.0), modulo( 17.5, 1.0)    ! same
    print *, mod(-17.5,-1.0), modulo(-17.5,-1.0)    ! same

end program demo_mod

!          -2           1
!           2          -1
!           2           2
!          -2          -2

!  -1.90000057       3.29999924
!   1.90000057      -3.29999924
!   1.90000057       1.90000057
!  -1.90000057      -1.90000057

! -0.500000000      0.500000000
!  0.500000000     -0.500000000
!  0.500000000      0.500000000
! -0.500000000     -0.500000000
