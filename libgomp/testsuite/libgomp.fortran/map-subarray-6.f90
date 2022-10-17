! { dg-do run }

type t
  integer, pointer :: p(:)
  integer, pointer :: p2(:)
end type t

type(t) :: var
integer, target :: tgt(5), tgt2(1000)
var%p => tgt
var%p2 => tgt2

p = 0
p2 = 0

!$omp target map(tgt, tgt2(4:6), var)
  var%p(1) = 5
  var%p2(5) = 7
!$omp end target

if (var%p(1).ne.5) stop 1
if (var%p2(5).ne.7) stop 2

end

! { dg-shouldfail "" { offload_device_nonshared_as } }
