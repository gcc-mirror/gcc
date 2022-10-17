! { dg-do run }

type t
  integer, pointer :: p(:)
end type t

type(t) :: var(2)

allocate (var(1)%p, source=[1,2,3,5])
allocate (var(2)%p, source=[2,3,5])

!$omp target map(var(1)%p, var(2)%p)
var(1)%p(1) = 5
var(2)%p(2) = 7
!$omp end target

!$omp target map(var(1)%p(1:3), var(1)%p, var(2)%p)
var(1)%p(1) = var(1)%p(1) + 1
var(2)%p(2) = var(2)%p(2) + 1
!$omp end target

!$omp target map(var(1)%p, var(2)%p, var(2)%p(1:3))
var(1)%p(1) = var(1)%p(1) + 1
var(2)%p(2) = var(2)%p(2) + 1
!$omp end target

!$omp target map(var(1)%p, var(1)%p(1:3), var(2)%p, var(2)%p(2))
var(1)%p(1) = var(1)%p(1) + 1
var(2)%p(2) = var(2)%p(2) + 1
!$omp end target

if (var(1)%p(1).ne.8) stop 1
if (var(2)%p(2).ne.10) stop 2

end
