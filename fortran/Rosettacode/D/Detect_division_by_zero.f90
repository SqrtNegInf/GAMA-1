!   Fortran
!u# https://rosettacode.org/wiki/Detect_division_by_zero
!c# 2024-02-01 

! works with 'gfortran' only

! Fortran has only floating-point exception handling. Integer exceptions are missing in ISO standard. Gfortran detects some integer explicit exceptions during compilation and is able to generate some run-time checks for integer overflow (with -ftrapv).  Intel ifort does not have integer overflow / division by zero detection.

program  rosetta_divbyzero
   implicit none
   integer, parameter :: rdp = kind(1.d0)
   real(rdp) :: normal,zero

   normal = 1.d0
   zero = 0.d0

   call div_by_zero_check(normal,zero)

 contains

   subroutine  div_by_zero_check(x,y)
      use, intrinsic  :: ieee_exceptions
      use, intrinsic  :: ieee_arithmetic
      implicit none
      real(rdp), intent(in) :: x,y

      real(rdp) :: check
      type(ieee_status_type) :: status_value
      logical :: flag
      flag = .false.
      ! Get the flags
      call ieee_get_status(status_value)
      ! Set the flags quiet
      call ieee_set_flag(ieee_divide_by_zero,.false.)
      write(*,*)"Inf supported? ",ieee_support_inf(check)

      ! Calculation involving exception handling
      check = x/y
      write(*,*)"Is check finite?",ieee_is_finite(check), check

      call ieee_get_flag(ieee_divide_by_zero, flag)
      if (flag) write(*,*)"Warning!  Division by zero detected"

      ! Restore the flags
      call ieee_set_status(status_value)

   end subroutine div_by_zero_check

end program rosetta_divbyzero
