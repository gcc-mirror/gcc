! { dg-do run }

implicit none

type t
integer :: value
type(t), pointer :: chain
end type t

type(t), target :: var, var2

var%value = 99
var2%value = 199

var%chain => var2
nullify(var2%chain)

!$acc enter data copyin(var, var2)

!$acc enter data attach(var%chain)

!$acc serial
! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } .-1 }
var%value = 5
var%chain%value = 7
!$acc end serial

!$acc exit data detach(var%chain)

!$acc exit data copyout(var, var2)

if (var%value.ne.5) stop 1
if (var2%value.ne.7) stop 2

end
