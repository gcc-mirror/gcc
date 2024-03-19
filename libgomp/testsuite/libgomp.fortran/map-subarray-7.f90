type t
integer, pointer :: p2(:)
end type t

integer, target :: A(5)
integer, pointer :: p(:), p2(:)
type(t) :: var

allocate(p2(1:20))
p => A
var%p2 => p2

A = 0
p2 = 0

! These arrays "share original storage", so are unsupported.  This will
! (correctly) fail with a non-shared address space.

!$omp target map(A(3:4), p2(4:8), p, var%p2)
A(3) = A(3) + 1
p2(4) = p2(4) + 2
!$omp end target

if (A(3).ne.1) stop 1
if (p2(4).ne.2) stop 2

end program

! { dg-shouldfail "" { offload_device_nonshared_as } }
