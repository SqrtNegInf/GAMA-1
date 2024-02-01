!   Fortran
!u# https://rosettacode.org/wiki/Partition_an_integer_x_into_n_primes
!c# 2024-01-25 

   module primes_module
    implicit none
    integer,allocatable :: p(:)
    integer :: a(0:32), b(0:32)
    integer,private :: sum_primes, number
    contains
!
    subroutine setnum(val)
        implicit none
        integer :: val
        number = val
        return
    end subroutine setnum
!
    subroutine init(thesize)
    implicit none
    integer :: thesize
    !
    allocate(p(thesize))
    p=0
    a=0
    b=0
    return
    end subroutine init
!
    subroutine genp(high) ! Store all primes up to high in the array p
        integer, intent(in) :: high
        integer :: i, numprimes, j,k
        logical*1 :: bk(0:high)
        !
        bk = .false.
        p = 0
        a = 0
        b = 0
        call eratosthenes(bk , high)
        j = 0
        numprimes = count(bk)
        k = 0
        do i = 1,high
            if(bk(i))then
                j = j+1
                p(j) = i
                if(j==numprimes)exit    !No need to loop more, all primes stored
            endif
        end do
        print*,'numprimes',numprimes, i,p(j)
        return
    end subroutine genp

    subroutine getp(z) ! used to update the zth prime number in the sequence of primes that are being used to partition the integer number.
        integer :: z

        if (a(z) == 0)a(z) = a(z-1)
        a(z) = a(z) + 1
        b(z) = p(a(z))
        return
    end subroutine getp

    subroutine part(num_found)
        integer, intent(in) :: num_found
        integer :: i, s, r
        logical :: bu
        a = 0
        do i = 1, num_found
            call getp(i)
        end do
   infinite: do
            sum_primes = 0
            bu = .false.
    nextin:do s = 1, num_found
                sum_primes = sum_primes + b(s)  !Adds the sth prime to sum_primes.
                if (sum_primes > number) then   !If the sum of primes exceeds number:
                    if (s == 1)then
                        exit infinite           !If only one prime has been added, exit the infinite loop.
                    endif
                    a(s:num_found) = 0          ! Resets the indices of the primes from s to num_found
                    do r = s - 1, num_found     ! Gets the next set of primes from s-1 to num_found
                        call getp(r)
                    end do
                    bu = .true.                 ! Sets bu to true and exits the loop over the primes
                    exit nextin
                end if
            end do nextin
            if (.not. bu) then                  ! If bu is false (meaning the sum of primes does not exceed number)
                if (sum_primes == number) exit infinite !We got it so go
                if (sum_primes < number) then
                    call getp(num_found)        ! If the sum of primes is less than number, gets the next prime
                else
                    error stop " Something wrong here!"
                endif
            endif
        end do infinite
        write( *,'(/,a,1x,i0,1x,a,1x,i0,1x,a)',advance='no') "Partition", number, "into", num_found,trim(adjustl(list(num_found)))
    end subroutine part
!
    function list(num_found)
        integer, intent(in) :: num_found
        integer :: i
        character(len=128) :: list
        character(len = 10):: pooka
!
        write(list,'(i0)') b(1)
        if (sum_primes == number) then
            do i = 2, num_found
                pooka = ''
                write(pooka,'(i0)') b(i)
                list = trim(list) // " + " // adjustl(pooka)
            end do
        else
            list = "(not possible)"
        end if
        list = "primes: " // list(:128-8)
    end function list
    !
    subroutine eratosthenes(p , n)
      implicit none
!
! dummy arguments
!
      integer  ::  n
      logical*1 , dimension(0:*)  ::  p
      intent (in) n
      intent (inout) p
!
! local variables
!
      integer  ::  i
      integer  ::  ii
      logical  ::  oddeven
      integer  ::  pr
!
      p(0:n) = .false.
      p(1) = .false.
      p(2) = .true.
      oddeven = .true.
      do i = 3 , n,2
          p(i) = .true.
      end do
      do i = 2 , int(sqrt(float(n)))
         ii = i + i
         if( p(i) )then
            do pr = i*i , n , ii
               p(pr) = .false.
            end do
         end if
      end do
      return
      end subroutine eratosthenes

end module primes_module

program prime_partition
    use primes_module
    implicit none
    integer :: i
    integer :: values(10,2)
    values(1,:) = (/99809,1/)
    values(2,:) = (/18,2/)
    values(3,:) = (/19,3/)
    values(4,:) = (/20,4/)
    values(5,:) = (/2017,24/)
    values(6,:) = (/22699, 1/)
    values(7,:) = (/22699, 2/)
    values(8,:) = (/22699, 3/)
    values(9,:) = (/22699, 4/)
    values(10,:) = (/40355, 3/)
    i = maxval(values(1:10,1))*2
    call init(i)    ! Set up a few basics
    call genp(i)    ! Generate primes up to i
    do i = 1,10
        call setnum( values(i,1))
        call part(values(i,2))
    end do
    write(*,*)
    Stop 'Successfully completed'
end program prime_partition

!Partition 99809 into 1 primes: 99809
!Partition 18 into 2 primes   : 5 + 13
!Partition 19 into 3 primes   : 3 + 5 + 11
!Partition 20 into 4 primes   : (not possible)
!Partition 2017 into 24 primes: 2 + 3 + 5 + 7 + 11 + 13 + 17 + 19 + 23 + 29 + 31 + 37 + 41 + 43 + 47 + 53 + 59 + 61 + 67 + 71 + 73 + 79 + 97 + 1129
!Partition 22699 into 1 primes: 22699
!Partition 22699 into 2 primes: 2 + 22697
!Partition 22699 into 3 primes: 3 + 5 + 22691
!Partition 22699 into 4 primes: 2 + 3 + 43 + 22651
!Partition 40355 into 3 primes: 3 + 139 + 40213
