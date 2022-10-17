! { dg-do run }

type F
integer, pointer :: mem(:)
end type F

type(F) :: fv
integer, allocatable, target :: arr(:)

allocate(arr(1:20))

fv%mem => arr
fv%mem = 0

!$omp target enter data map(to: fv%mem(1:10))
!$omp target map(alloc: fv%mem)
fv%mem(1) = fv%mem(1) + 1
!$omp end target
!$omp target exit data map(from: fv%mem(1:10))

if (fv%mem(1).ne.1) stop 1

!$omp target enter data map(to: fv, fv%mem(1:10))
!$omp target
fv%mem(1) = fv%mem(1) + 1
!$omp end target
!$omp target exit data map(from: fv, fv%mem(1:10))

if (fv%mem(1).ne.2) stop 2

!$omp target enter data map(to: fv%mem, fv%mem(1:10))
!$omp target
fv%mem(1) = fv%mem(1) + 1
!$omp end target
!$omp target exit data map(from: fv%mem, fv%mem(1:10))

if (fv%mem(1).ne.3) stop 3

!$omp target enter data map(to: fv%mem)
!$omp target
fv%mem(1) = fv%mem(1) + 1
!$omp end target
!$omp target exit data map(from: fv%mem)

if (fv%mem(1).ne.4) stop 4

end
