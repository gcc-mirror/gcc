! { dg-do run }

use openacc
implicit none (type, external)
integer, target :: tgt_a, tgt_b(5)

integer, pointer :: p1, p2(:)

type t
  integer,pointer :: a => null ()
  integer,pointer :: b(:) => null ()
  integer,allocatable :: c, d(:)
end type t

type(t), target :: var

tgt_a = 51
tgt_b = [11,22,33,44,55]

var%b => tgt_b
!$acc enter data copyin(var, tgt_a, tgt_b)
var%a => tgt_a

call acc_attach(var%a)
call acc_attach(var%b)

!$acc serial
! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } .-1 }
  if (var%a /= 51) stop 1
  if (any (var%b /= [11,22,33,44,55])) stop 2
!$acc end serial

call acc_detach(var%a)
call acc_detach(var%b)

!$acc exit data delete(var, tgt_a, tgt_b)

var%c = 9
var%d = [1,2,3]

p1 => var%c
p2 => var%d

!$acc enter data copyin(p1, p2)
!$acc enter data copyin(var)
call acc_attach(var%c)
call acc_attach(var%d)

!$acc serial
! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } .-1 }
  if (var%c /= 9) stop 3
  if (any (var%d /= [1,2,3])) stop 4
!$acc end serial

call acc_detach(var%c)
call acc_detach(var%d)

!$acc exit data delete(var, p1, p2)

deallocate(var%d)

end
