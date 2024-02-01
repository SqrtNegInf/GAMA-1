!   Fortran
!u# https://rosettacode.org/wiki/Phrase_reversals
!c# 2024-01-29 

! The key here is the ability via F90 to specify an array span, as A(first:last:step) where the step can be negative... This facility is not available for CHARACTER variables where only TEXT(first:last) is available - no step is accommodated and TEXT(last:first) evokes nothing rather than a backwards order as in Python. However, one can have an array of CHARACTER*1 variables, and as an array they can be rolled bidirectionally. For convenience in initialising such an array, EQUIVALENCE(TEXT,ATXT) means that a normal multi-character text literal can be assigned to TEXT via the DATA statement, rather than having to specify the ATXT elements one at a time.
! By identifying the first and last character position of each word in TEXT (or equivalently, their indices in ATXT) and storing them in arrays IST and LST, the i'th word can be fingered via IST(i) to LST(i) and of course a DO-loop can step in either direction. 

! F90 allows a WHILE-loop, and writing something like       DO WHILE (L1.LE.L .AND. ATXT(L1).LE." ")
!         L1 = L1 + 1
!       END DO

! Would be rather more structured and involve fewer GO TOs and their labels, but alas, modern Fortran specifies that there is no specification as to whether or not both terms of an expression such as (A and B) will always be evaluated or instead there will be a shortcut: if A is false then the B term is ignored. And here, the A term checks whether or not L1 is within bounds and if it is not, then the B term should not be evaluated, not just because of the waste of effort but because to do so might involve accessing outside the definition of ATXT. As when the scan chases through the trailing spaces. One could make the array one longer, or rather, L = LEN(TEXT) - 1 for this case but that would be messy, require explanation, be easily forgotten, and, typical ad-hoc testing would be unlikely to detect the mistake.

! Alternatively, a GO TO can be removed from view by using EXIT in its place:       DO L1 = L1,L
!         IF (ATXT(L1).GT." ") EXIT
!       END DO
! Except that this relies on the index variable retaining its value  on exiting the loop, either as fingering the first non-blank or, being L + 1. This expectation is frowned upon in some quarters.
! Both variants would have to be followed by a test such as IF (L1 .LE. L) THEN to identify whether the start of a word has been found that would require further processing. So, all in all, suck up the GO TOs...      PROGRAM REVERSER    !Just fooling around.

      CHARACTER*(66) TEXT    !Holds the text. Easily long enough.
      CHARACTER*1 ATXT(66)    !But this is what I play with.
      EQUIVALENCE (TEXT,ATXT)    !Same storage, different access abilities..
      DATA TEXT/"Rosetta Code Phrase Reversal"/    !Easier to specify this for TEXT.
      INTEGER IST(6),LST(6)    !Start and stop positions.
      INTEGER N,L,I        !Counters.
      INTEGER L1,L2        !Fingers for the scan.
      CHARACTER*(*) AS,RW,FW,RO,FO            !Now for some cramming.
      PARAMETER (AS = "Words ordered as supplied")    !So that some statements can fit on a line.
      PARAMETER (RW = "Reversed words, ", FW = "Forward words, ")
      PARAMETER (RO = "reverse order",    FO = "forward order")

! Chop the text into words.
      N = 0        !No words found.
      L = LEN(TEXT)    !Multiple trailing spaces - no worries.
      L2 = 0        !Syncopation: where the previous chomp ended.
   10 L1 = L2        !Thus, where a fresh scan should follow.
   11 L1 = L1 + 1        !Advance one.
      IF (L1.GT.L) GO TO 20        !Finished yet?
      IF (ATXT(L1).LE." ") GO TO 11    !No. Skip leading spaces.
      L2 = L1            !Righto, L1 is the first non-blank.
   12 L2 = L2 + 1        !Scan through the non-blanks.
      IF (L2.GT.L) GO TO 13    !Is it safe to look?
      IF (ATXT(L2).GT." ") GO TO 12    !Yes. Speed through non-blanks.
   13 N = N + 1            !Righto, a word is found in TEXT(L1:L2 - 1)
      IST(N) = L1        !So, recall its first character.
      LST(N) = L2 - 1        !And its last.
      IF (L2.LT.L) GO TO 10    !Perhaps more text follows.

!  Chuck the words around.
   20 WRITE (6,21) N,TEXT    !First, say what has been discovered.
   21 FORMAT (I4," words have been isolated from the text ",A,/)

      WRITE (6,22) AS,    (" ",ATXT(IST(I):LST(I):+1), I = 1,N,+1)
      WRITE (6,22) RW//RO,(" ",ATXT(LST(I):IST(I):-1), I = N,1,-1)
      WRITE (6,22) FW//RO,(" ",ATXT(IST(I):LST(I):+1), I = N,1,-1)
      WRITE (6,22) RW//FO,(" ",ATXT(LST(I):IST(I):-1), I = 1,N,+1)

   22 FORMAT (A36,":",66A1)
      END

! With F77 such array spans can't be used, but all that is necessary is to supply a second implied DO-loop in the WRITE statement, for example       WRITE (6,22) RW//RO,(" ",(ATXT(J), J = LST(I),IST(I),-1), I = 1,N,+1)
