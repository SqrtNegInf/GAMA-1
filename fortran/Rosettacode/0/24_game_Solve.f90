!   Fortran
!u# https://rosettacode.org/wiki/24_game/Solve
!c# 2024-02-01 

! 'gfortran' defaults to 'wide' spacing
! module moved to precede program, neither compiler works unless so arranged

module helpers

contains

  pure subroutine Insertion_Sort(a)
    integer, intent(inout) :: a(:)
    integer                :: temp, i, j
    do i=2,size(a)
      j = i-1
      temp = a(i)
      do while ( j>=1 .and. a(j)>temp )
        a(j+1) = a(j)
        j = j - 1
      end do
      a(j+1) = temp
    end do
  end subroutine Insertion_Sort

  subroutine nextpermutation(perm,last)
    integer, intent(inout) :: perm(:)
    logical, intent(out)   :: last
    integer :: k,l
    k = largest1()
    last = k == 0
    if ( .not. last ) then    
      l = largest2(k)
      call swap(l,k)
      call reverse(k)
    end if
  contains
    pure integer function largest1()
      integer :: k, max
      max = 0
      do k=1,size(perm)-1
        if ( perm(k) < perm(k+1) ) then
          max = k
        end if
      end do
      largest1 = max
    end function largest1

    pure integer function largest2(k)
      integer, intent(in) :: k
      integer             :: l, max
      max = k+1
      do l=k+2,size(perm)
        if ( perm(k) < perm(l) ) then
          max = l
        end if
      end do
      largest2 = max
    end function largest2

    subroutine swap(l,k)
      integer, intent(in) :: k,l
      integer             :: temp
      temp    = perm(k)
      perm(k) = perm(l)
      perm(l) = temp
    end subroutine swap

    subroutine reverse(k)
      integer, intent(in) :: k
      integer             :: i
      do i=1,(size(perm)-k)/2
        call swap(k+i,size(perm)+1-i)
      end do
    end subroutine reverse

  end subroutine nextpermutation

end module helpers

program solve_24
  use helpers
  implicit none
  real                 :: vector(4), reals(4), p, q, r, s
  integer              :: numbers(4), n, i, j, k, a, b, c, d
  character, parameter :: ops(4) = (/ '+', '-', '*', '/' /)
  logical              :: last
  real,parameter       :: eps = epsilon(1.0)

  do n=1,12
    call random_number(vector)
    reals   = 9 * vector + 1
    numbers = int(reals)
    call Insertion_Sort(numbers)

    permutations: do
      a = numbers(1); b = numbers(2); c = numbers(3); d = numbers(4)
      reals = real(numbers)
      p = reals(1);   q = reals(2);   r = reals(3);   s = reals(4)
      ! combinations of operators:
      do i=1,4
        do j=1,4
          do k=1,4
            if      ( abs(op(op(op(p,i,q),j,r),k,s)-24.0) < eps ) then
              write (*,*) numbers, ' : ', '((',a,ops(i),b,')',ops(j),c,')',ops(k),d
              exit permutations
            else if ( abs(op(op(p,i,op(q,j,r)),k,s)-24.0) < eps ) then
              write (*,*) numbers, ' : ', '(',a,ops(i),'(',b,ops(j),c,'))',ops(k),d
              exit permutations
            else if ( abs(op(p,i,op(op(q,j,r),k,s))-24.0) < eps ) then
              write (*,*) numbers, ' : ', a,ops(i),'((',b,ops(j),c,')',ops(k),d,')'
              exit permutations
            else if ( abs(op(p,i,op(q,j,op(r,k,s)))-24.0) < eps ) then
              write (*,*) numbers, ' : ', a,ops(i),'(',b,ops(j),'(',c,ops(k),d,'))'
              exit permutations
            else if ( abs(op(op(p,i,q),j,op(r,k,s))-24.0) < eps ) then
              write (*,*) numbers, ' : ', '(',a,ops(i),b,')',ops(j),'(',c,ops(k),d,')'
              exit permutations
            end if
          end do
        end do
      end do
      call nextpermutation(numbers,last)  
      if ( last ) then
        write (*,*) numbers, ' : no solution.'
        exit permutations
      end if
    end do permutations

  end do

contains

  pure real function op(x,c,y)
    integer, intent(in) :: c
    real, intent(in)    :: x,y
    select case ( ops(c) )
      case ('+')
        op = x+y
      case ('-')
        op = x-y
      case ('*')
        op = x*y
      case ('/')
        op = x/y
    end select
  end function op

end program solve_24
