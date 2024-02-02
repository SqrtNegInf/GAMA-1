!   Fortran
!u# https://rosettacode.org/wiki/4-rings_or_4-squares_puzzle
!c# 2024-02-01 

! This uses the facility standardised in F90 whereby DO-loops can have text labels attached (not in the usual label area) so that the END DO statement can have the corresponding label, and any CYCLE statements can use it also. Similarly, the subroutine's END statement bears the name of the subroutine. This is just syntactic decoration. Rather more useful is extended syntax for dealing with arrays and especially the function ANY for making multiple tests without having to enumerate them in the code. To gain this convenience, the EQUIVALENCE statement makes variables A, B, C, D, E, F, and G occupy the same storage as INTEGER V(7), an array. 
! One could abandon the use of the named variables in favour of manipulating the array equivalent, and indeed develop code which performs the nested loops via messing with the array, but for simplicity, the individual variables are used. However, tempting though it is to write a systematic sequence of seven nested DO-loops, the variables are not in fact all independent: some are fixed once others are chosen. Just cycling through all the notional possibilities when one only is in fact possible is a bit too much brute-force-and-ignorance, though other problems with other constraints, may encourage such exhaustive stepping. As a result, the code is more tightly bound to the specific features of the problem. 

! Also standardised in F90 is the $ format code, which specifies that the output line is not to end with the WRITE statement. The problem here is that Fortran does not offer an IF ...FI bracketing construction inside an expression, that would allow something like WRITE(...) FIRST,LAST,IF (UNIQUE) THEN "Distinct values only" ELSE "Repeated values allowed" FI // "."
!  so that the correct alternative will be selected. Further, an array (that would hold those two texts) can't be indexed by a LOGICAL variable, and playing with EQUIVALENCE won't help, because the numerical values revealed thereby for .TRUE. and .FALSE. may not be 1 and 0. And anyway, parameters are not allowed to be accessed via EQUIVALENCE to another variable.
! So, a two-part output, and to reduce the blather, two IF-statements.       

SUBROUTINE FOURSHOW(FIRST,LAST,UNIQUE)

!  Choose values such that A+B = B+C+D = D+E+F = F+G, all being integers in FIRST:LAST...
       INTEGER FIRST,LAST    !The range of allowed values.
       LOGICAL UNIQUE        !Solutions need not have unique values.
       INTEGER A,B,C,D,E,F,G    !Ah, Diophantus of Alexandria.
       INTEGER V(7),S,N        !Assistants.
       EQUIVALENCE (V(1),A),(V(2),B),(V(3),C),     &    !Yes,
                   (V(4),D),(V(5),E),(V(6),F),(V(7),G)    !We're all individuals.
        WRITE (6,1) FIRST,LAST    !Announce: first part.
    1   FORMAT (/,"The Four Rings puzzle, over ",I0," to ",I0,".",$)    !$: An addendum follows.
        IF (UNIQUE) WRITE (6,*) "Distinct values only."    !Save on the THEN ... ELSE ... END IF blather.
        IF (.NOT.UNIQUE) WRITE (6,*) "Repeated values allowed."    !Perhaps the compiler will be smarter.

        N = 0    !No solutions have been found.
      BB:DO B = FIRST,LAST    !Start chugging through the possibilities.
        CC:DO C = FIRST,LAST        !Brute force and ignorance.
             IF (UNIQUE .AND. B.EQ.C) CYCLE CC    !The first constraint shows up.
          DD:DO D = FIRST,LAST        !Start by forming B, C, and D.
               IF (UNIQUE .AND. ANY(V(2:3).EQ.D)) CYCLE DD    !Ignoring A just for now.
               S = B + C + D        !This is the common sum.
               A = S - B        !The value of A is not free from BCD.
               IF (A < FIRST .OR. A > LAST) CYCLE DD    !And it may not be within bounds.
               IF (UNIQUE .AND. ANY(V(2:4).EQ.A)) CYCLE DD    !Or, if required so, unique.
            EE:DO E = FIRST,LAST    !Righto, A,B,C,D are valid. Try an E.
                 IF (UNIQUE .AND. ANY(V(1:4).EQ.E)) CYCLE EE    !Precluded already?
                 F = S - (E + D)        !No. So therefore, F is determined.
                 IF (F < FIRST .OR. F > LAST) CYCLE EE    !Acceptable?
                 IF (UNIQUE .AND. ANY(V(1:5).EQ.F)) CYCLE EE    !And, if required, unique?
                 G = S - F            !Yes! So finally, G is determined.
                 IF (G < FIRST .OR. G > LAST) CYCLE EE    !Acceptable?
                 IF (UNIQUE .AND. ANY(V(1:6).EQ.G)) CYCLE EE    !And, if required, unique?
                 N = N + 1            !Yes! Count a solution set!
                 IF (UNIQUE) WRITE (6,"(7I3)") V    !Show its values.
               END DO EE            !Consder another E.
             END DO DD            !Consider another D.
           END DO CC        !Consider another C.
         END DO BB    !Consider another B.
        WRITE (6,2) N    !Announce the count.
    2   FORMAT (I9," found.")    !Numerous, if no need for distinct values.
      END SUBROUTINE FOURSHOW    !That was fun!

PROGRAM POKE

CALL FOURSHOW(1,7,.TRUE.)
CALL FOURSHOW(3,9,.TRUE.)
CALL FOURSHOW(0,9,.FALSE.)

END PROGRAM POKE
