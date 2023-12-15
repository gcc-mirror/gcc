! { dg-do run }

type t
  integer, pointer :: p(:)
end type t

type(t) :: var(3)
integer :: i, j

allocate (var(1)%p, source=[1,2,3,5])
allocate (var(2)%p, source=[2,3,5])
allocate (var(3)%p(1:3))

var(3)%p = 0

do i = 1, 3
  do j = 1, 3
!$omp target map(var(i)%p, var(j)%p)
    var(i)%p(1) = 5
    var(j)%p(2) = 7
!$omp end target

    if (i.ne.j) then
!$omp target map(var(i)%p(1:3), var(i)%p, var(j)%p)
      var(i)%p(1) = var(i)%p(1) + 1
      var(j)%p(2) = var(j)%p(2) + 1
!$omp end target

!$omp target map(var(i)%p, var(j)%p, var(j)%p(1:3))
      var(i)%p(1) = var(i)%p(1) + 1
      var(j)%p(2) = var(j)%p(2) + 1
!$omp end target

!$omp target map(var(i)%p, var(i)%p(1:3), var(j)%p, var(j)%p(2))
      var(i)%p(1) = var(i)%p(1) + 1
      var(j)%p(2) = var(j)%p(2) + 1
!$omp end target
    end if

    if (i.eq.j) then
      if (var(i)%p(1).ne.5) stop 1
      if (var(j)%p(2).ne.7) stop 2
    else
      if (var(i)%p(1).ne.8) stop 3
      if (var(j)%p(2).ne.10) stop 4
    end if
  end do
end do

end

! { dg-output "(\n|\r|\r\n)" { target offload_device_nonshared_as } }
! { dg-output "libgomp: Mapped array elements must be the same .*(\n|\r|\r\n)+" { target offload_device_nonshared_as } }
! { dg-shouldfail "" { offload_device_nonshared_as } }
