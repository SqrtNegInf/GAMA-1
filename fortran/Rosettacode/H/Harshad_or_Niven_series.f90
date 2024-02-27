!   Fortran
!u# https://rosettacode.org/wiki/Harshad_or_Niven_series
!c# 2024-01-27 

program Harshad
  integer :: i, h = 0
  do i=1, 20
    call nextHarshad(h)
    write(6, '(i5$)') h
   !write(6, '(i5)', advance='no') h
  enddo
  h = 1000
  call nextHarshad(h)
  write(6, '(i5)') h

contains

  subroutine nextHarshad(h) ! alter input integer h to be the next greater Harshad number.
    integer, intent(inout) :: h
    h = h+1 ! bigger
    do while (.not. isHarshad(h))
      h = h+1
    end do
  end subroutine nextHarshad

  logical function isHarshad(a)
    integer, intent(in) :: a
    integer :: mutable, digit_sum
    isHarshad = .false.
    if (a .lt. 1) return ! false if a < 1
    mutable = a
    digit_sum = 0
    do while (mutable /= 0)
      digit_sum = digit_sum + mod(mutable, 10)
      mutable = mutable / 10
    end do
    isHarshad = 0 .eq. mod(a, digit_sum)
  end function isHarshad

end program Harshad
