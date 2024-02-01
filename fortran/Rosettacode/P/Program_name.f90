!   Fortran
!u# https://rosettacode.org/wiki/Program_name
!c# 2024-01-24 

module sundry

contains

  subroutine verify_name(required)
    ! name verification reduces the ways an attacker can rename rm as cp.
    character(len=*), intent(in) :: required
    character(len=1024) :: name
    integer :: length, status
    ! I believe get_command_argument is part of the 2003 FORTRAN standard intrinsics.
    call get_command_argument(0, name, length, status)
    if (0 /= status) stop
    if ((len_trim(name)+1) .ne. (index(name, required, back=.true.) + len(required))) stop
    write(6,*) trim(name)//' approved.'
  end subroutine verify_name

end module sundry

program name
  use sundry
  call verify_name('Program_name')
  write(6,*)'program continues...'
end program name

