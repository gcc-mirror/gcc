program main
  implicit none

  print *, "CheCKpOInT"
  !$acc parallel
  STOP 1
  !$acc end parallel

end program main

! { dg-output "CheCKpOInT" }
! { dg-shouldfail "" }
