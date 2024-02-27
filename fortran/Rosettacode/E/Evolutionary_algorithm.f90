!   Fortran
!u# https://rosettacode.org/wiki/Evolutionary_algorithm
!c# 2024-01-27 

! Apple/LLVM doesn't have 'rand' or 'srand' (gfortran extensions)
! tweaked to work with both

     module evolve_routines
     implicit none

     !the target string:
     character(len=*),parameter :: targ = 'METHINKS IT IS LIKE A WEASEL'

     contains

     pure elemental function fitness(member) result(n)

     implicit none
     integer :: n
     character(len=*),intent(in) :: member

     integer :: i

     n=0
     do i=1,len(targ)
         n = n + abs( ichar(targ(i:i)) - ichar(member(i:i))  )
     end do

     end function fitness

!    pure elemental subroutine mutate(member,factor) ! cannot use 'rand' in pure/elemental (didn't need this anyhow)
     subroutine mutate(member,factor)

     implicit none
     character(len=*),intent(inout) :: member   !population member
     real,intent(in) :: factor                  !mutation factor

     integer,parameter :: n_chars = 27    !number of characters in set
     character(len=n_chars),parameter :: chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '

     real    :: rnd_val
     integer :: i,j,n

     n = len(member)

     do i=1,n
        !rnd_val = rand()
         call random_number(rnd_val)
         if (rnd_val<=factor) then   !mutate this element            
            !rnd_val = rand()
             call random_number(rnd_val)
             j = int(rnd_val*n_chars)+1   !an integer between 1 and n_chars
             member(i:i) = chars(j:j)
         end if
     end do

    end subroutine mutate

     end module evolve_routines

     program evolve
     use evolve_routines

     implicit none

     !Tuning parameters:
!    integer,parameter :: seed = 12345             !random number generator seed
     integer,parameter :: max_iter = 10000         !maximum number of iterations
     integer,parameter :: population_size = 200    !size of the population
     real,parameter    :: factor = 0.04            ![0,1] mutation factor
     integer,parameter :: iprint = 5               !print every iprint iterations

     !local variables:
     integer :: i,iter
     integer,dimension(1) :: i_best
     character(len=len(targ)),dimension(population_size) :: population
     integer xx

     !initialize random number generator:
    !call srand(seed)
     call random_seed(size = xx)

     !create initial population:
     ! [the first element of the population will hold the best member]
     population(1) = 'PACQXJB CQPWEYKSVDCIOUPKUOJY'  !initial guess
     iter=0

     write(*,'(A10,A30,A10)') 'iter','best','fitness'
     write(*,'(I10,A30,I10)') iter,population(1),fitness(population(1))

     do 

         iter = iter + 1 !iteration counter

          !write the iteration:
         if (mod(iter,iprint)==0) write(*,'(I10,A30,I10)') iter,population(1),fitness(population(1))

         !check exit conditions:
         if ( iter>max_iter .or. fitness(population(1))==0 ) exit

         !copy best member and mutate:
         population = population(1)    
         do i=2,population_size
             call mutate(population(i),factor)    
         end do

         !select the new best population member:
         ! [the best has the lowest value]
         i_best = minloc(fitness(population))
         population(1) = population(i_best(1))

     end do

     !write the last iteration:
     if (mod(iter,iprint)/=0) write(*,'(I10,A30,I10)') iter,population(1),fitness(population(1))

     if (iter>max_iter) then
         write(*,*) 'No solution found.'
     else
         write(*,*) 'Solution found.'
     end if

     end program evolve
