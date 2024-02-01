!   Fortran
!u# https://rosettacode.org/wiki/Reverse_a_string
!c# 2024-01-04 

subroutine reverse_string_1

  CHARACTER(80) :: str = "This is a string"
  CHARACTER :: temp
  INTEGER :: i, length

  WRITE (*,*) str
  length = LEN_TRIM(str) ! Ignores trailing blanks. Use LEN(str) to reverse those as well
  DO i = 1, length/2
     temp = str(i:i)
     str(i:i) = str(length+1-i:length+1-i)
     str(length+1-i:length+1-i) = temp
  END DO
  WRITE(*,*) str

END subroutine reverse_string_1

subroutine reverse_string_2

  implicit none
  character (*), parameter :: string = 'no devil lived on'

  write (*, '(a)') 'a palindrome:'
  write (*, '(a)') reverse (string)

contains

  recursive function reverse (string) result (res)

    implicit none
    character (*), intent (in) :: string
    character (len (string)) :: res

    if (len (string) == 0) then
      res = ''
    else
      res = string (len (string) :) // reverse (string (: len (string) - 1))
    end if

  end function reverse

end subroutine reverse_string_2

! Another shorter implementation (adapted version from stackoverflow question 10605574 how-to-reverse-a-chain-of-character-fortran-90):

 subroutine reverse_string_3
  implicit none
  character (80) :: cadena
  integer :: k, n
  !
  cadena = "abcdefgh"
  n = len_trim (cadena)
  !
  write (*,*) cadena
  forall (k=1:n) cadena (k:k) = cadena (n-k+1:n-k+1)
  write (*,*) cadena
  !
end subroutine reverse_string_3

program examples
    implicit none
    call reverse_string_1
    call reverse_string_2
    call reverse_string_3

end program examples
