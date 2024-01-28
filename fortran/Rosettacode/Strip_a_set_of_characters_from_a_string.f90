!   Fortran
!u# https://rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string
!c# 2024-01-25 

program foo
    implicit none
    character(len=9), dimension(3) :: bar = (/'012345678','aaa135aaa','876543210'/)
    character(*), parameter        :: bad = '13579'

    call strip(bar,bad)
    print *, bar

contains

!Note: Since strip is an elemental subroutine, it can be called with arrays of strings as well.
elemental subroutine strip(string,set)
  character(len=*), intent(out) :: string   ! suffices, but less clear
! character(len=*), intent(inout) :: string
  character(len=*), intent(in)    :: set
  integer                         :: old, new, stride
  old = 1; new = 1
  do
    stride = scan( string( old : ), set )
    if ( stride > 0 ) then
      string( new : new+stride-2 ) = string( old : old+stride-2 )
      old = old+stride
      new = new+stride-1
    else
      string( new : ) = string( old : )
      return
    end if
  end do
end subroutine strip


end program foo
