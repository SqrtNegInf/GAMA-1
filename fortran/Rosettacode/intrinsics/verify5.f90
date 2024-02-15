program more_verify
implicit none
character(len=*),parameter :: &
! & int='0123456789', &
  & low*(*)   = 'abcdefghijklmnopqrstuvwxyz', &
  & upp='ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
  & blank=' '
! note character variables in an array have to be of the same length
character(len=6) :: strings(3)=["Go    ","right ","home! "]
character(len=2) :: sets(3)=["do","re","me"]

  ! elemental -- you can use arrays for both strings and for sets

   ! check each string from right to left for non-letter/non-blank
   write(*,*)'last non-letter',verify(strings,upp//low//blank,back=.true.)

   ! even BACK can be an array
   ! find last non-uppercase character in "Howdy "
   ! and first non-lowercase in "there "
   write(*,*) verify(strings(1:2),[upp,low],back=[.true.,.false.])

   ! using a null string for a set is not well defined. Avoid it
   write(*,*) 'null',verify("for tran ", "", .true.) ! 8,length of string?
   ! probably what you expected
   write(*,*) 'blank',verify("for tran ", " ", .true.) ! 7,found 'n'

   ! first character in  "Go    " not in "do",
   ! and first letter in "right " not in "ri"
   ! and first letter in "home! " not in "me"
   write(*,*) verify(strings,sets)

end program more_verify
