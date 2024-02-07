!   Fortran
!u# https://rosettacode.org/wiki/AKS_test_for_primes
!c# 2024-02-07 

! switch to kind=8 to get 'flang' working

program aks
  implicit none

  ! Coefficients of polynomial expansion
  integer(kind=8), dimension(:), allocatable :: coeffs
  integer(kind=8) :: n
  ! Character variable for I/O
  character(len=40) :: tmp

  ! Point #2
  do n = 0, 7
    write(tmp, *) n
    call polynomial_expansion(n, coeffs)
    write(*, fmt='(A)', advance='no') '(x - 1)^'//trim(adjustl(tmp))//' ='
    call print_polynom(coeffs)
  end do

  ! Point #4
  do n = 2, 35
    if (is_prime(n)) write(*, '(I4)', advance='no') n
  end do
  write(*, *)

  ! Point #5
  do n = 2, 124
    if (is_prime(n)) write(*, '(I4)', advance='no') n
  end do
  write(*, *)

  if (allocated(coeffs)) deallocate(coeffs)
contains
  ! Calculate coefficients of (x - 1)^n using binomial theorem
  subroutine polynomial_expansion(n, coeffs)
    integer(kind=8), intent(in) :: n
    integer(kind=8), dimension(:), allocatable, intent(out) :: coeffs
    integer(kind=8) :: i

    if (allocated(coeffs)) deallocate(coeffs)

    allocate(coeffs(n + 1))

    do i = 1, n + 1
      coeffs(i) = binomial(n, i - 1)*(-1)**(n - i - 1)
    end do
  end subroutine

  ! Calculate binomial coefficient using recurrent relation, as calculation
  ! using factorial overflows too quickly.
  function binomial(n, k) result (res)
    integer(kind=8), intent(in) :: n, k
    integer(kind=8) :: res
    integer(kind=8) :: i

    if (k == 0) then
      res = 1
      return
    end if

    res = 1
    do i = 0, k - 1
      res = res*(n - i)/(i + 1)
    end do
  end function

  ! Outputs polynomial with given coefficients
  subroutine print_polynom(coeffs)
    integer(kind=8), dimension(:), allocatable, intent(in) :: coeffs
    integer(kind=4) :: i, p
    character(len=40) :: cbuf, pbuf
    logical(kind=1) :: non_zero

    if (.not. allocated(coeffs)) return

    non_zero = .false.

    do i = 1, size(coeffs)
      if (coeffs(i) .eq. 0) cycle

      p = i - 1
      write(cbuf, '(I40)') abs(coeffs(i))
      write(pbuf, '(I40)') p 

      if (non_zero) then
        if (coeffs(i) .gt. 0) then
          write(*, fmt='(A)', advance='no') ' + '
        else
          write(*, fmt='(A)', advance='no') ' - '
        endif
      else
        if (coeffs(i) .gt. 0) then
          write(*, fmt='(A)', advance='no') '   '
        else
          write(*, fmt='(A)', advance='no') ' - '
        endif
      endif

      if (p .eq. 0) then
        write(*, fmt='(A)', advance='no') trim(adjustl(cbuf))
      elseif (p .eq. 1) then
        if (coeffs(i) .eq. 1) then
          write(*, fmt='(A)', advance='no') 'x'
        else
          write(*, fmt='(A)', advance='no') trim(adjustl(cbuf))//'x'
        end if
      else
        if (coeffs(i) .eq. 1) then
          write(*, fmt='(A)', advance='no') 'x^'//trim(adjustl(pbuf))
        else
          write(*, fmt='(A)', advance='no') &
            trim(adjustl(cbuf))//'x^'//trim(adjustl(pbuf))
        end if
      end if
      non_zero = .true.
    end do

    write(*, *)
  end subroutine

  ! Test if n is prime using AKS test. Point #3.
  function is_prime(n) result (res)
    integer(kind=8), intent (in) :: n
    logical(kind=1) :: res
    integer(kind=8), dimension(:), allocatable :: coeffs
    integer(kind=8) :: i

    call polynomial_expansion(n, coeffs)
    coeffs(1) = coeffs(1) + 1
    coeffs(n + 1) = coeffs(n + 1) - 1

    res = .true.

    do i = 1, n + 1
      res = res .and. (mod(coeffs(i), n) == 0)
    end do

    if (allocated(coeffs)) deallocate(coeffs)
  end function
end program aks

!(x - 1)^0 =   1
!(x - 1)^1 = - 1 + x
!(x - 1)^2 =   1 - 2x + x^2
!(x - 1)^3 = - 1 + 3x - 3x^2 + x^3
!(x - 1)^4 =   1 - 4x + 6x^2 - 4x^3 + x^4
!(x - 1)^5 = - 1 + 5x - 10x^2 + 10x^3 - 5x^4 + x^5
!(x - 1)^6 =   1 - 6x + 15x^2 - 20x^3 + 15x^4 - 6x^5 + x^6
!(x - 1)^7 = - 1 + 7x - 21x^2 + 35x^3 - 35x^4 + 21x^5 - 7x^6 + x^7
!   2   3   5   7  11  13  17  19  23  29  31
!   2   3   5   7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71  73  79  83  89  97 101 103 107 109 113
