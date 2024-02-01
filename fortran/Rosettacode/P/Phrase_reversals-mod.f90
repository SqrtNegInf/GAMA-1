!   Fortran
!u# https://rosettacode.org/wiki/Phrase_reversals
!c# 2024-01-29 

! modernize a bit, exact same output as RC version

program phrase_reversal

      character(28) :: text = ("Rosetta Code Phrase Reversal")
      character*1      atxt(28)
      equivalence   (text,atxt)

      integer        ist(6), list(6), N, L, I, L1, L2
      character*(*)  AS,RW,FW,RO,FO, FMT1, FMT2
      parameter (    AS = "Words ordered as supplied",  &
                     RW = "Reversed words, ",           &
                     FW = "Forward words, ",            &
                     RO = "reverse order",              &
                     FO = "forward order",              &
                     FMT1 = '(I4," words have been isolated from the text ",A,/)', &
                     FMT2 = '(A36,":",66A1)' &
                )

        N = 0
        L = len(text)
        L2 = 0
loop10: do while (L2 < L)
          L1 = L2
loop11:   do 
            L1 = L1 + 1
            if (L1 > L) exit loop10  ! <== required
            if (atxt(L1) > " ") exit ! loop11
          end do loop11
          L2 = L1
loop12:   do
            L2 = L2 + 1
            if (L2 > L) exit ! loop12
            if (atxt(L2) <= " ") exit ! loop12
          end do loop12
          N = N + 1
          ist(N)  = L1
          list(N) = L2 - 1
        end do loop10

        write (*,FMT1) N,text
        write (*,FMT2) AS,    (" ",atxt(ist(I):list(I):+1), I = 1,N,+1)
        write (*,FMT2) RW//RO,(" ",atxt(list(I):ist(I):-1), I = N,1,-1)
        write (*,FMT2) FW//RO,(" ",atxt(ist(I):list(I):+1), I = N,1,-1)
        write (*,FMT2) RW//FO,(" ",atxt(list(I):ist(I):-1), I = 1,N,+1)

end program phrase_reversal
