! { dg-do run }

use openacc
implicit none

type t
integer :: foo
character(len=8), pointer :: bar
character(len=4), allocatable :: qux
end type t

type(t) :: var
character(len=8), target :: tgt

allocate(var%qux)

var%bar => tgt

var%foo = 99
tgt = "Octopus!"
var%qux = "Fish"

!$acc enter data copyin(var, tgt)

! Avoid automatic attach (i.e. with "enter data")
call acc_copyin (var%qux)

!$acc enter data attach(var%bar, var%qux)

!$acc serial
! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } .-1 }
var%foo = 5
var%bar = "Plankton"
var%qux = "Pond"
!$acc end serial

!$acc exit data detach(var%bar, var%qux)

call acc_copyout (var%qux)

!$acc exit data copyout(var, tgt)

if (var%foo.ne.5) stop 1
if (tgt.ne."Plankton") stop 2
if (var%qux.ne."Pond") stop 3

end
