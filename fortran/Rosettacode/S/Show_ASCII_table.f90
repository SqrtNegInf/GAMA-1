!   Fortran
!u# https://rosettacode.org/wiki/Show_ASCII_table
!c# 2024-01-28 

! The dollar sign $ in the format string isn't part of the standard but is a common extension.
! ACHAR may not be part of the standard, either.

       PROGRAM ASCTBL  ! show the ASCII characters from 32-127
       IMPLICIT NONE
       INTEGER I, J
       CHARACTER*3 H

  10   FORMAT (I3, ':', A3, '   ', $)
  20   FORMAT ()
       DO J = 0, 15, +1
         DO I = 32+J, 127, +16
           IF (I > 32 .AND. I < 127) THEN
             H = ' ' // ACHAR(I) // ' '
           ELSE IF (I .EQ. 32) THEN
             H = 'Spc'
           ELSE IF (I .EQ. 127) THEN
             H = 'Del'
           ELSE
             STOP 'bad value of i'
           END IF
           PRINT 10, I, H
         END DO
         PRINT 20
       END DO

       END
