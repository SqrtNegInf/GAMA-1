program L
real  g , S
real :: xa=4.0
real, dimension(5)::x=(/2.0,3.0,5.0,8.0,12.0/)
real, dimension (5)::y
!x=(/2.0,3.0,5.0,8.0,12.0/)
y=(/10.0,15.0,25.0,40.0,60.0/)
S=0.0
do i=1,5
g=1.0
do j=1,5
if (i.eq.j) cycle
g=g*((xa-x(j))/(x(i)-x(j)))
end do
S=S+g*y(i)
end do
print*,"Value of the y=",s
end 
