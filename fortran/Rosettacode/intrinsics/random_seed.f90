program demo_random_seed
implicit none
integer, allocatable :: seed(:)
integer :: n

   call random_seed(size = n)
   print *, 'n: ', n
   allocate(seed(n))
   print *, 's: ', size(seed)
   call random_seed(get=seed)
   write (*, *) seed

end program demo_random_seed
