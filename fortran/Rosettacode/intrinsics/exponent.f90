program demo_exponent
implicit none
integer, parameter  :: qp = selected_real_kind(16)
real :: x = 1.0
integer :: i
   i = exponent(x)
   print *, i
   print *, exponent(0.0)
   print *, exponent([10.0,100.0,1000.0,-10000.0])
   print *, exponent([11.0,110.0,1010.0,-10010.0])

   print *, '--'
!  print *, 2**[10.0,100.0,1000.0,-10000.0]
   print *, 2**[10.0,100.0]
   print *, 2**[10.d0,100.d0,1000.d0]
   print *, 2**[10.0_qp,100.0_qp,1000.0_qp]
   print *, '--'

   print *, exponent(huge(0.0))
   print *, exponent(tiny(0.0))
end program demo_exponent
