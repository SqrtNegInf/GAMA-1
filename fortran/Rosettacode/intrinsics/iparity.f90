program demo_iparity
implicit none
integer, dimension(2) :: a
  a(1) = int(b'00100100')
  a(2) = int(b'01101010')
  print '(b8)',   iparity(a)
  print '(b8.8)', iparity(a)
  print '(b8.8)', ieor(a(1),a(2))
end program demo_iparity
