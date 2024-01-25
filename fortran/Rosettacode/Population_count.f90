!   Fortran
!u# https://rosettacode.org/wiki/Population_count
!c# 2024-01-24 

! Warning: ‘popcnt’ declared at (1) may shadow the intrinsic of the same name.  
! In order to call the intrinsic, explicit INTRINSIC declarations may be required. [-Wintrinsic-shadow]
! hence popcnt  -> pop_cnt

program population_count
  implicit none

  integer, parameter :: i64 = selected_int_kind(18)
  integer(i64) :: x
  integer :: i, n

  x = 1
  write(*, "(a8)", advance = "no") "3**i :"
  do i = 1, 30
    write(*, "(i3)", advance = "no") pop_cnt(x)
    x = x * 3
  end do

  write(*,*)
  write(*, "(a8)", advance = "no") "Evil :"
  n = 0
  x = 0 
  do while(n < 30)
    if(mod(pop_cnt(x), 2) == 0) then
      n = n + 1
      write(*, "(i3)", advance = "no") x
    end if
    x = x + 1
  end do

  write(*,*)
  write(*, "(a8)", advance = "no") "Odious :"
  n = 0
  x = 0 
  do while(n < 30)
    if(mod(pop_cnt(x), 2) /= 0) then
      n = n + 1
      write(*, "(i3)", advance = "no") x
    end if
    x = x + 1
  end do

contains

integer function pop_cnt(x)
  integer(i64), intent(in) :: x
  integer :: i

  pop_cnt = 0
  do i = 0, 63
    if(btest(x, i)) pop_cnt = pop_cnt + 1
  end do

end function
end program

!  3**i : 1  2  2  4  3  6  6  5  6  8  9 13 10 11 14 15 11 14 14 17 17 20 19 22 16 18 24 30 25 25
!  Evil : 0  3  5  6  9 10 12 15 17 18 20 23 24 27 29 30 33 34 36 39 40 43 45 46 48 51 53 54 57 58
!Odious : 1  2  4  7  8 11 13 14 16 19 21 22 25 26 28 31 32 35 37 38 41 42 44 47 49 50 52 55 56 59

