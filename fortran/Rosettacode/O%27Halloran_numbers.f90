!   Fortran
!u# https://rosettacode.org/wiki/O%27Halloran_numbers
!c# 2024-01-20 

program ohalloran_numbers
    implicit none
    integer, parameter :: max_area = 1000
    integer :: half_max, i, j, k, area
    logical, dimension(max_area) :: areas
    half_max = max_area / 2

    ! Initialize areas with .TRUE.
    areas = .TRUE.

    do i = 1, max_area
        do j = 1, half_max
            if (i * j > half_max) exit
            do k = 1, half_max
                area = 2 * (i * j + i * k + j * k)
                if (area > max_area) exit
                areas(area) = .FALSE. ! Mark as not an O'Halloran number
            end do
        end do
    end do

    ! Print the O'Halloran numbers
    print *, "Even surface areas < NOT ", max_area, " achievable by any regular integer-valued cuboid:"
    do i = 1, max_area
    	if (mod(i, 2) == 0 .AND. areas(i)) then
        	write(*,'(I4,1X)', advance='no') i
    	endif
     end do
     print *, "" ! To add a final newline after the list
end program ohalloran_numbers

!Even surface areas < NOT         1000  achievable by any regular integer-valued cuboid:
!  2    4    8   12   20   36   44   60   84  116  140  156  204  260  380  420  660  924  


