!   Fortran
!u# https://rosettacode.org/wiki/Apply_a_callback_to_an_array
!c# 2024-02-02 


!Elemental functions.

module arrCallback
contains
    elemental function cube( x )
        implicit none
        real :: cube
        real, intent(in) :: x
        cube = x * x * x
    end function cube
end module arrCallback

program testAC
    use arrCallback
    implicit none
    integer :: i, j
    real, dimension(3,4) :: b, &
        a = reshape( (/ ((10 * i + j, i = 1, 3), j = 1, 4) /), (/ 3,4 /) )

    do i = 1, 3
        write(*,*) a(i,:)
    end do

    print *, ''

    b = cube( a )  ! Applies CUBE to every member of a,
                   ! and stores each result in the equivalent element of b
    do i = 1, 3
        write(*,*) b(i,:)
    end do
end program testAC
