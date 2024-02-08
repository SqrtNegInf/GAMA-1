!   Fortran
!u# https://rosettacode.org/wiki/Abstract_type
!c# 2024-02-07 

! does nothing of course...

program foo

! Simple abstract derived type (i.e. abstract class)  in Fortran 2008 

   ! abstract derived type
   type, abstract :: TFigure
      real(8) :: area
   contains
      ! deferred method i.e. abstract method =  must be overridden in extended type
      procedure(calculate_area), deferred, pass :: calculate_area
   end type TFigure
   ! only declaration of the abstract method/procedure for TFigure type
   abstract interface
      function  calculate_area(this)
         import TFigure !imports TFigure type from host scoping unit and makes it accessible here
         implicit none
         class(TFigure) :: this
         real(8) :: calculate_area
      end function calculate_area
   end interface

end program foo
