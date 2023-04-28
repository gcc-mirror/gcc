! { dg-do run }

implicit none

type t
integer :: foo
integer, pointer :: bar
end type t

type(t) :: var
integer, target :: tgt

var%bar => tgt

var%foo = 99
tgt = 199

!$acc enter data copyin(var, tgt)

!$acc enter data attach(var%bar)

!$acc serial
! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } .-1 }
var%foo = 5
var%bar = 7
!$acc end serial

!$acc exit data detach(var%bar)

!$acc exit data copyout(var, tgt)

if (var%foo.ne.5) stop 1
if (tgt.ne.7) stop 2

end
