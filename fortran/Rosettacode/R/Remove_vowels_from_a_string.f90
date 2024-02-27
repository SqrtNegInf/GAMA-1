!   Fortran
!u# https://rosettacode.org/wiki/Remove_vowels_from_a_string
!c# 2024-02-02 


program remove_vowels
    implicit none

    character(len=*), parameter :: string1="The quick brown fox jumps over the lazy dog."
    character(len=*), parameter :: string2="Fortran programming language"

    call print_no_vowels(string1)
    call print_no_vowels(string2)
contains
    subroutine print_no_vowels(string)
        character(len=*), intent(in)  :: string
        character(len=len(string))    :: temp
        integer                       :: i, j
        temp = ' '
        j = 0

        do i = 1, len(string)
            select case (string(i:i))
            case('A','E','I','O','U','a','e','i','o','u')
                cycle
            case default
                write(*,'(A1)',advance="no") string(i:i)
                j = j + 1
                temp(j:j) = string(i:i)
            end select
        end do
        write(*,*) new_line('A') 
        print *, '|' // temp(1:j) // '|'
    end subroutine print_no_vowels
end program remove_vowels
