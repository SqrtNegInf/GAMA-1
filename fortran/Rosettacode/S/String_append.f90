!   Fortran
!u# https://rosettacode.org/wiki/String_append
!c# 2024-01-25 

! Using deferred length character strings:

program main
 character(len=20) :: str20

 character(len=:),allocatable :: str

 str = 'hello'
 str = str//' world'

 write(*,*) str
 print *, str

!Using pre-allocated character strings:
 str20 = 'String'
 str20(len_trim(str20)+1:) = 'Append'
 print *, str20 

end program main
