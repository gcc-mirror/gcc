! { dg-do run }

program p

type t
integer :: x, y
end type t

type(t) :: var

var%x = 0
var%y = 0

var = sub(7)

contains

type(t) function sub(arg)
integer :: arg

!$omp declare mapper (t :: tvar) map(tvar%x, tvar%y)

!$omp target enter data map(alloc: sub)

sub%x = 5
sub%y = arg

!$omp target update to(sub)

!$omp target
if (sub%x.ne.5) stop 1
if (sub%y.ne.7) stop 2
!$omp end target

!$omp target exit data map(release: sub)

end function sub
end program p
