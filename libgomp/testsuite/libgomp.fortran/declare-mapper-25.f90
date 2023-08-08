! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

type t
integer, allocatable :: arr(:)
end type t

!$omp declare mapper(odd: T :: tv) map(tv%arr(1::2))
!$omp declare mapper(even: T :: tv) map(tv%arr(2::2))

type(t) :: var
integer :: i

allocate(var%arr(100))

var%arr = 0

!$omp target enter data map(to: var)

var%arr = 1

!$omp target update to(mapper(odd): var)

!$omp target
do i=1,100
  if (mod(i,2).eq.0.and.var%arr(i).ne.0) stop 1
  if (mod(i,2).eq.1.and.var%arr(i).ne.1) stop 2
end do
!$omp end target

var%arr = 2

!$omp target update to(mapper(even): var)

!$omp target
do i=1,100
  if (mod(i,2).eq.0.and.var%arr(i).ne.2) stop 3
  if (mod(i,2).eq.1.and.var%arr(i).ne.1) stop 4
end do
!$omp end target

!$omp target exit data map(delete: var)

end
