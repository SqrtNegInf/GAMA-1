!   Fortran
!u# https://rosettacode.org/wiki/Call_an_object_method
!c# 2024-02-02 

! In modern Fortran a "derived type" concept corresponds to "class" in OOP. Such types have "type bound procedures", i.e. static methods. Procedure pointer components depend on the value of the object (so they are object-bound), can be redefined runtime and correspond approx to instances.

! type declaration
type my_type
 contains
procedure, pass :: method1
procedure, pass, pointer :: method2
end type my_type

! declare object of type my_type
type(my_type) :: mytype_object

!static call
 call mytype_object%method1() ! call method1 defined as subroutine
!instance?
 mytype_object%method2() ! call method2 defined as function



