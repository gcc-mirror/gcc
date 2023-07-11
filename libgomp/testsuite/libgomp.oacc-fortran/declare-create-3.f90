! { dg-do run }

module m
integer, allocatable :: mint(:)
!$acc declare create (mint)
end module m

program p
use m

allocate(mint(1:20))

mint = 0

!$acc serial
! { dg-warning {using .vector_length \(32\)., ignoring 1} "" { target openacc_nvidia_accel_selected } .-1 }
mint = 5
!$acc end serial

!$acc update host(mint)

if (any(mint.ne.5)) stop 1

deallocate(mint)

end program p
