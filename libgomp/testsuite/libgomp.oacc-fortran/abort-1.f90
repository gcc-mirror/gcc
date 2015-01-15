! { dg-shouldfail "" { *-*-* } { "*" } { "" } }

program main
  implicit none

  !$acc parallel
  call abort
  !$acc end parallel

end program main
