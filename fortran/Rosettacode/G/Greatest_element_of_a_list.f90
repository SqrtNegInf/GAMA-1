!   Fortran
!u# https://rosettacode.org/wiki/Greatest_element_of_a_list
!c# 2024-01-27 

program test_maxval

integer,dimension(5),parameter :: x = [10,100,7,1,2]
real,dimension(5),parameter :: y = [5.0,60.0,1.0,678.0,0.0]

write(*,'(I5)') maxval(x)
write(*,'(F5.1)') maxval(y)

print *,'-----'
!end program test_maxval


!The intrinsic function max accepts any number of arguments. 
!The type of these arguments can be integer, real, character, string of characters or arrays of these.

!program test_max

!  implicit none

  write (*, '(i0)') &
    & max (1, 2, 3)
  write (*, '(f3.1)') &
    & max (1.0, 2.0, 3.0)
  write (*, '(a)') &
    & max ('a', 'b', 'c')
  write (*, '(a)') &
    & max ('abc', 'bca', 'cab')
  write (*, '(i0, 2 (1x, i0))') &
    & max ([1, 8, 6], [7, 5, 3], [4, 2, 9])
  write (*, '(f3.1, 2 (1x, f3.1))') &
    & max ([1.0, 8.0, 6.0], [7.0, 5.0, 3.0], [4.0, 2.0, 9.0])
  write (*, '(a, 2 (1x, a))') &
    & max (['a', 'h', 'f'], ['g', 'e', 'c'], ['d', 'b', 'i'])
  write (*, '(a, 2 (1x, a))') &
    & max (['abc', 'hig', 'fde'], ['ghi', 'efd', 'cab'], ['def', 'bca', 'igh'])

end program 
