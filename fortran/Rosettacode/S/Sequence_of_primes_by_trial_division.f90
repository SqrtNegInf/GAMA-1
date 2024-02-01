!   Fortran
!u# https://rosettacode.org/wiki/Sequence_of_primes_by_trial_division
!c# 2024-01-29 

! after dealing with funny ha-ha comments, it works!
! not that arithmatic-if is good practice...

! This version was written for an IBM1130, which offered 16-bit integers. Storage size was 32K words. It was written before the revised dogma that one is not a prime number became common. With punched-card input, using only capital letters was normal. This system offered Fortran IV which lacks the many later developments such as if ... then style statements, thus the profusion of statement labels and arithmetic if-statements. In if (expression) negative,zero,positive the sign of the expression is examined to choose the appropriate label to jump to. Labels can only be numbers, alas. There was also no MOD function for determining the remainder.
! This routine does not attempt a calculation of sqrt(n), a time-consuming and also potentially inacurrate scheme. For instance, using Trunc(Log10(n)) + 1 to determine how many digits are needed to print n seems an obvious ad-hoc ploy, and gave 1 for n = 1 to 9, but it also gave 1 for n = 10, because Log10(10) was calculated as 0.9999964 or so (single precision on an IBM 390 system), which truncates to zero.
! Instead, since many successive numbers are to be tested for primality, advantage can be taken of the fact that prime numbers only up to PRIME(LP) need be tried as factors all the way up to N = PRIME(LP + 1)**2 = XP2. This is similar to starting a pass through a Sieve of Eratoshenes at P*P rather than at 2*P. Thus, 5 is the largest factor to try, even beyond 5*5, all the way up to 49, because, if the number were divisible by 7, 7*2 would already have been checked because of 2, 7*3 because of 3, and so on. Only when 7*7 is needed will a further possible factor have to be tried. Likewise, although the last possible factor to try for N up to the integer limit of 32767 is 181 because the square of the next prime (191) exceeds 32767, in order for the method to be able to know this, the PRIME array must have space for this surplus prime. However, it does not know this properly because the square of 191 does exceed 32767 and so its value in XP2 will be incorrect, but this doesn't matter because only equality to XP2 is checked for and there will never be call to try 191 as a factor because 181 suffices up to the integer limit and the iteration will stop by then. Fortunately, 32767 is not divisible by three so that value will not be excluded as a possible candidate for N, and so the search can correctly end after inspecting the largest possible integer - finding it divisible by seven.
! This method avoids considering multiples of two and three, leading to the need to pre-load array PRIME and print the first few values explicitly rather than flounder about with special startup tricks. Even so, in order not to pre-load with 7, and to correctly start the factor testing with 5, the first few primes are found with some wasted effort because 5 is not needed at the start. Storing the primes as found has the obvious advantage of enabling divisions only by prime numbers, but care with the startup is needed to ensure that primes have indeed been stored before they are called for.

!CONCOCTED BY R.N.MCLEAN, APPLIED MATHS COURSE, AUCKLAND UNIVERSITY, MCMLXXI.
      INTEGER ENUFF,PRIME(44)
!CALCULATION SHOWS PRIME(43) = 181, AND PRIME(44) = 191.
      INTEGER N,F,Q,XP2
      INTEGER INC,IP,LP,PP
      INTEGER ALINE(20),LL,I
      DATA ENUFF/44/
      DATA PP/4/
      DATA PRIME(1),PRIME(2),PRIME(3),PRIME(4)/1,2,3,5/
!COPY THE KNOWN PRIMES TO THE OUTPUT LINE.
      DO 1 I = 1,PP
    1   ALINE(I) = PRIME(I)
      LL = PP
      LP = 3
      XP2 = PRIME(LP + 1)**2
      N = 5
      INC = 4
!CONSIDER ANOTHER CANDIDATE. VIA INC, DODGE MULTIPLES OF 2 AND 3.
   10 INC = 6 - INC
      N = N + INC
      IF (N - XP2) 20,11,20
   11 LP = LP + 1
      XP2 = PRIME(LP + 1)**2
      GO TO 40
!CHECK SUCCESSIVE PRIMES AS FACTORS, STARTING WITH PRIME(4) = 5.
   20 IP = 4
   21 F = PRIME(IP)
      Q = N/F
      IF (Q*F - N) 22,40,22
   22 IP = IP + 1
      IF (IP - LP) 21,21,30
!CAUGHT ANOTHER PRIME.
   30 IF (PP - ENUFF) 31,32,32
   31 PP = PP + 1
      PRIME(PP) = N
   32 IF (LL - 20) 35,33,33
   33 WRITE (6,34) (ALINE(I), I = 1,LL)
   34 FORMAT (20I6)
      LL = 0
   35 LL = LL + 1
      ALINE(LL) = N
!COMPLETED?
   40 IF (N - 32767) 10,41,41
   41 WRITE (6,34) (ALINE(I), I = 1,LL)
      END
