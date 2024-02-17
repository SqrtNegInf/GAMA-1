program demo_nearest
implicit none

   real(4) :: x, y
   real(8) :: xx, yy

   x = nearest(42.0, 1.0)
   y = nearest(42.0, -1.0)
   write (*,"(3(g20.15,1x))") x, y, x - y

   xx = nearest(42.0d0, 1.0)
   yy = nearest(42.0d0, -1.0)
   write (*,"(3(g25.20,1x))") xx, yy, xx - yy

! below can get over/under-flow

   write (*,"(3(g20.15,1x))") &
    nearest(tiny(0.0),1.0), &
    nearest(tiny(0.0),-1.0), &
    nearest(tiny(0.0),1.0) -nearest(tiny(0.0),-1.0)

!  write (*,"(3(g20.15))") &
!   nearest(huge(0.0),1.0), &
!   nearest(huge(0.0),-1.0), &
!   nearest(huge(0.0),1.0)- nearest(huge(0.0),-1.0)

end program demo_nearest
