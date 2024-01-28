!   Fortran
!u# https://rosettacode.org/wiki/Man_or_boy_test
!c# 2024-01-28 

! Fortran 2008 (uses an internal procedure as function argument). Tested with g95 and gfortran 4.6.
! does not work with 'flang', compiles but seg.faults
! with 'gfortran': only runs up to '16' before crashing (depth of recursion?)

module man_or_boy

implicit none

contains

  recursive integer function A(k,x1,x2,x3,x4,x5) result(res)
    integer, intent(in) :: k
    interface
      recursive integer function x1()
      end function
      recursive integer function x2()
      end function
      recursive integer function x3()
      end function
      recursive integer function x4()
      end function
      recursive integer function x5()
      end function
    end interface
    integer :: m
    if ( k <= 0 ) then
      res = x4()+x5()
    else
      m = k
      res = B()
    end if

  contains

    recursive integer function B() result(res)    
      m = m-1
      res = A(m,B,x1,x2,x3,x4)
    end function B

  end function A

  recursive integer function one() result(res)
    res = 1
  end function

  recursive integer function minus_one() result(res)
    res = -1
  end function

  recursive integer function zero() result(res)
    res = 0
  end function

end module man_or_boy

program test
  use man_or_boy
  implicit none
  integer i         ! cannot be named 'a', conflicts with function name
  do i=10,16
      write (*,*) A(i,one,minus_one,minus_one,one,zero)
  end do
end program test



