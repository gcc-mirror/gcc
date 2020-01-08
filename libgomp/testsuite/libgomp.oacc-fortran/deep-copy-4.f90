! { dg-do run }

! Test of attach/detach with "acc enter/exit data".

program dtype
  implicit none
  integer, parameter :: n = 512
  type mytype
    integer, allocatable :: a(:)
    integer, allocatable :: b(:)
  end type mytype
  integer, allocatable :: r(:)
  integer i

  type(mytype) :: var

  allocate(var%a(1:n))
  allocate(var%b(1:n))
  allocate(r(1:n))

!$acc enter data copyin(var)

!$acc enter data copyin(var%a, var%b, r)

!$acc parallel loop
  do i = 1,n
    var%a(i) = i
    var%b(i) = i * 2
    r(i) = i * 3
  end do
!$acc end parallel loop

!$acc exit data copyout(var%a)
!$acc exit data copyout(var%b)
!$acc exit data copyout(r)

  do i = 1,n
    if (i .ne. var%a(i)) stop 1
    if (i * 2 .ne. var%b(i)) stop 2
    if (i * 3 .ne. r(i)) stop 3
  end do

!$acc exit data delete(var)

  deallocate(var%a)
  deallocate(var%b)
  deallocate(r)

end program dtype
