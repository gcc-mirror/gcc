implicit none
type t
  integer :: dummy
  integer, pointer :: p1(:), p2(:)
  integer :: dummy2
end type t
type(t) :: var
integer :: i
allocate(var%p1(5),var%p2(2:4))
var%p1 = [22,53,28,6,4]
var%p2 = [46,679,54]

!$omp target enter data map(to:var%p1, var%p2)
!$omp target
  if (.not.associated(var%p1).or.lbound(var%p1,1)/=1.or.ubound(var%p1,1)/=5) stop 1
  if (.not.associated(var%p2).or.lbound(var%p2,1)/=2.or.ubound(var%p2,1)/=4) stop 2
  if (any (var%p1 /= [22,53,28,6,4])) stop 3
  if (any (var%p2 /= [46,679,54])) stop 4
!$omp end target
!!$omp target exit data map(from:var%p1, var%p2)
end

