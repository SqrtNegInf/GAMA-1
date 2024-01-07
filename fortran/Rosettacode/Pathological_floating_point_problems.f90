!   Fortran
!u# https://rosettacode.org/wiki/Pathological_floating_point_problems
!c# 2024-01-05 


      SUBROUTINE MULLER

       REAL*4 VN,VNL1,VNL2	!The exact precision and dynamic range
       REAL*8 WN,WNL1,WNL2	!Depends on the format's precise usage of bits.
       INTEGER I		!A stepper.
        WRITE (6,1)		!A heading.
    1   FORMAT ("Muller's sequence should converge to six...",/
     1   "  N     Single      Double")
        VNL1 = 2; VN = -4	!Initialise for N = 2.
        WNL1 = 2; WN = -4	!No fractional parts yet.
        DO I = 3,36			!No point going any further.
          VNL2 = VNL1; VNL1 = VN		!Shuffle the values along one place.
          WNL2 = WNL1; WNL1 = WN		!Ready for the next term's calculation.
          VN = 111 - 1130/VNL1 + 3000/(VNL1*VNL2)	!Calculate the next term.
          WN = 111 - 1130/WNL1 + 3000/(WNL1*WNL2)	!In double precision.
          WRITE (6,2) I,VN,WN			!Show both.
    2     FORMAT (I3,F12.7,F21.16)		!With too many fractional digits.
        END DO				!On to the next term.
      END SUBROUTINE MULLER	!That was easy. Too bad the results are wrong.

      SUBROUTINE CBS		!The Chaotic Bank Society.
       INTEGER YEAR	!A stepper.
       REAL*4 V		!The balance.
       REAL*8 W		!In double precision as well.
        V = 1; W = 1		!Initial values, without dozy 1D0 stuff.
        V = EXP(V) - 1		!Actual initial value desired is e - 1..,
        W = EXP(W) - 1		!This relies on double-precision W selecting DEXP.
        WRITE (6,1)		!Here we go.
    1   FORMAT (///"The Chaotic Bank Society in action..."/"Year")
        WRITE (6,2) 0,V,W	!Show the initial deposit.
    2   FORMAT (I3,F16.7,F28.16)
        DO YEAR = 1,25		!Step through some years.
          V = V*YEAR - 1	!The specified procedure.
          W = W*YEAR - 1	!The compiler handles type conversions.
          WRITE (6,2) YEAR,V,W	!The current balance.
        END DO			!On to the following year.
      END SUBROUTINE CBS	!Madness!

      REAL*4 FUNCTION SR4(A,B)	!Siegfried Rump's example function of 1988.
       REAL*4 A,B
        SR4 = 333.75*B**6
     1      + A**2*(11*A**2*B**2 - B**6 - 121*B**4 - 2)
     2      + 5.5*B**8 + A/(2*B)
      END FUNCTION SR4

      REAL*8 FUNCTION SR8(A,B)	!Siegfried Rump's example function.
       REAL*8 A,B
        SR8 = 333.75*B**6	!.75 is exactly represented in binary.
     1      + A**2*(11*A**2*B**2 - B**6 - 121*B**4 - 2)
     2      + 5.5*B**8 + A/(2*B)!.5 is exactly represented in binary.
      END FUNCTION SR8

      PROGRAM POKE
      REAL*4 V	!Some example variables.
      REAL*8 W	!Whose type goes to the inquiry function.
      WRITE (6,1) RADIX(V),DIGITS(V),"single",DIGITS(W),"double"
    1   FORMAT ("Floating-point arithmetic is conducted in base ",I0,/
     1   2(I3," digits for ",A," precision",/))
      WRITE (6,*) "Single precision limit",HUGE(V)
      WRITE (6,*) "Double precision limit",HUGE(W)
      WRITE (6,*)

      CALL MULLER

      CALL CBS

      WRITE (6,10)
   10 FORMAT (///"Evaluation of Siegfried Rump's function of 1988",
     1 " where F(77617,33096) = -0.827396059946821")
      WRITE (6,*) "Single precision:",SR4(77617.0,33096.0)
!     WRITE (6,*) "Double precision:",SR8(77617.0D0,33096.0D0)	!Must match the types.
      END


      SUBROUTINE CBSX	!The Chaotic Bank Society.
       INTEGER YEAR	!A stepper.
       REAL*4 V		!The balance.
       REAL*8 W		!In double precision as well.
       INTEGER NTERM	!Share information with CBSERIES.
       REAL*8 T		!So as to show workings.
        V = 1; W = 1		!Initial values, without dozy 1D0 stuff.
        V = EXP(V) - 1		!Actual initial value desired is e - 1..,
        W = EXP(W) - 1		!This relies on double-precision W selecting DEXP.
        WRITE (6,1)		!Here we go.
    1   FORMAT (///"The Chaotic Bank Society in action...",/,
     *   "Year",9X,"Real*4",22X,"Real*8",12X,"Series summation",
     *   9X,"Last term",2X,"Terms.")
        WRITE (6,2) 0,V,W,CBSERIES(0),T,NTERM	!Show the initial deposit.
    2   FORMAT (I3,F16.7,2F28.16,D18.8,I7)	!Not quite 16-digit precision for REAL*8.
        DO YEAR = 1,25		!Step through some years.
          V = V*YEAR - 1	!The specified procedure.
          W = W*YEAR - 1	!The compiler handles type conversions.
          WRITE (6,2) YEAR,V,W,CBSERIES(YEAR),T,NTERM	!The current balance.
        END DO			!On to the following year.
        CONTAINS		!An alternative.
        REAL*8 FUNCTION CBSERIES(N)	!Calculates for the special deposit of e - 1.
         INTEGER N	!Desire the balance after year N for the deposit of e - 1.
         REAL*8 S	!Via a series summation.
          S = 0			!Start the summation.
          T = 1			!First term is 1/(N + 1)
          I = N			!Second is 1/[(N + 1)*(N + 2)], etc.
          NTERM = 0		!No terms so far.
    3       NTERM = NTERM + 1	!Here we go.
            I = I + 1		!Thus advance to the next divisor, and not divide by zero.
            T = T/I		!Thus not compute the products from scratch each time.
            S = S + T		!Add the term.
            IF (T/S .GE. EPSILON(S)) GO TO 3	!If they're still making a difference, another.
          CBSERIES = S		!Convergence is ever-faster as N increases.
        END FUNCTION CBSERIES	!So this is easy.
      END SUBROUTINE CBSX	!Madness!
