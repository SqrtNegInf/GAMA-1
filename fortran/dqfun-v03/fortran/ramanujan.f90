program ramanujan

use dqmodule

!implicit none
!type (dq_real) rama
!rama = exp ( dqpi() * sqrt( dqreal(163.q0) ) )
!call dqwrite (6, 80, 65, rama)

call dqwrite (6, 80, 49, exp ( dqpi() * sqrt( dqreal(163.q0) ) ) )

end program

! 262537412640768743.99999999999925007259719818568888
! 262537412640768743.99999999999925007259719818568887935385633733699e17
