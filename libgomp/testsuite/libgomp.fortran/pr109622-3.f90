! { dg-do run }

type t
integer :: foo
integer, pointer :: bar(:)
end type t

type(t) :: var
integer, target :: tgt(20)

var%bar => tgt

var%foo = 99
tgt = 199

!$acc enter data copyin(var, tgt)

!$acc enter data attach(var%bar)

!$acc serial
var%foo = 5
var%bar = 7
!$acc end serial

!$acc exit data detach(var%bar)

!$acc exit data copyout(var, tgt)

if (var%foo.ne.5) stop 1
if (any(tgt.ne.7)) stop 2

end
