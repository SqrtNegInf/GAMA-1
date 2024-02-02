!   Fortran
!u# https://rosettacode.org/wiki/100_doors
!c# 2024-02-01 

program doors
    implicit none
    integer, allocatable :: door(:)
    character(6), parameter :: s(0:1) = [character(6) :: "closed", "open"]
    integer :: i, n

!   print "(A)", "Number of doors?"
!    read *, n
    n = 100
    allocate (door(n))
    door = 1
    do i = 1, n
        door(i:n:i) = 1 - door(i:n:i)
        print "(A,G0,2A)", "door ", i, " is ", s(door(i))
    end do
end program
