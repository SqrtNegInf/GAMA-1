!   Fortran
!u# https://rosettacode.org/wiki/Percolation/Mean_run_density
!c# 2024-01-25 

! loosely translated from python.  We do not need to generate and store the entire vector at once.
! compilation: gfortran -Wall -std=f2008 -o thisfile thisfile.f08

program percolation_mean_run_density
  implicit none
  integer :: i, p10, n2, n, t
  real :: p, limit, sim, delta
  data n,p,t/100,0.5,500/
  write(6,'(a3,a5,4a7)')'t','p','n','p(1-p)','sim','delta%'
  do p10=1,10,2
     p = p10/10.0
     limit = p*(1-p)
     write(6,'()')
     do n2=10,15,2
        n = 2**n2
        sim = 0
        do i=1,t
           sim = sim + mean_run_density(n,p)
        end do
        sim = sim/t
        if (limit /= 0) then
           delta = abs(sim-limit)/limit
        else
           delta = sim
        end if
        delta = delta * 100
        write(6,'(i3,f5.2,i7,2f7.3,f5.1)')t,p,n,limit,sim,delta
     end do
  end do

contains

  integer function runs(n, p)
    integer, intent(in) :: n
    real, intent(in) :: p
    real :: harvest
    logical :: q
    integer :: count, i
    count = 0
    q = .false.
    do i=1,n
       call random_number(harvest)
       if (harvest < p) then
          q = .true.
       else
          if (q) count = count+1
          q = .false.
       end if
    end do
    runs = count
  end function runs

  real function mean_run_density(n, p)
    integer, intent(in) :: n
    real, intent(in) :: p
    mean_run_density = real(runs(n,p))/real(n)
  end function mean_run_density

end program percolation_mean_run_density

!  t    p      n p(1-p)    sim  delta%
!
!500 0.10   1024  0.090  0.090  0.2
!500 0.10   4096  0.090  0.090  0.2
!500 0.10  16384  0.090  0.090  0.0
!
!500 0.30   1024  0.210  0.210  0.2
!500 0.30   4096  0.210  0.210  0.0
!500 0.30  16384  0.210  0.210  0.0
!
!500 0.50   1024  0.250  0.250  0.1
!500 0.50   4096  0.250  0.250  0.1
!500 0.50  16384  0.250  0.250  0.1
!
!500 0.70   1024  0.210  0.210  0.1
!500 0.70   4096  0.210  0.210  0.1
!500 0.70  16384  0.210  0.210  0.0
!
!500 0.90   1024  0.090  0.090  0.1
!500 0.90   4096  0.090  0.090  0.4
!500 0.90  16384  0.090  0.090  0.1
