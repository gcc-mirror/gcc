! { dg-do run }

! Test of attach/detach with scalar elements and nested derived types.

program dtype
  implicit none
  integer, parameter :: n = 512
  type subtype
    integer :: g, h
    integer, allocatable :: q(:)
  end type subtype
  type mytype
    integer, allocatable :: a(:)
    integer, allocatable :: c, d
    integer, allocatable :: b(:)
    integer :: f
    type(subtype) :: s
  end type mytype
  integer i

  type(mytype) :: var

  allocate(var%a(1:n))
  allocate(var%b(1:n))
  allocate(var%c)
  allocate(var%d)
  allocate(var%s%q(1:n))

  var%c = 16
  var%d = 20
  var%f = 7
  var%s%g = 21
  var%s%h = 38

!$acc enter data copyin(var)

  do i = 1, n
    var%a(i) = 0
    var%b(i) = 0
    var%s%q(i) = 0
  end do

!$acc data copy(var%a(5:n - 5), var%b(5:n - 5), var%c, var%d) &
!$acc & copy(var%s%q)

!$acc parallel loop default(none) present(var)
  do i = 5,n - 5
    var%a(i) = i
    var%b(i) = i * 2
    var%s%q(i) = i * 3
    var%s%g = 100
    var%s%h = 101
  end do
!$acc end parallel loop

!$acc end data

!$acc exit data copyout(var)

  do i = 1,4
    if (var%a(i) .ne. 0) stop 1
    if (var%b(i) .ne. 0) stop 2
    if (var%s%q(i) .ne. 0) stop 3
  end do

  do i = 5,n - 5
    if (i .ne. var%a(i)) stop 4
    if (i * 2 .ne. var%b(i)) stop 5
    if (i * 3 .ne. var%s%q(i)) stop 6
  end do

  do i = n - 4,n
    if (var%a(i) .ne. 0) stop 7
    if (var%b(i) .ne. 0) stop 8
    if (var%s%q(i) .ne. 0) stop 9
  end do

  if (var%c .ne. 16) stop 10
  if (var%d .ne. 20) stop 11
  if (var%s%g .ne. 100 .or. var%s%h .ne. 101) stop 12
  if (var%f .ne. 7) stop 13

  deallocate(var%a)
  deallocate(var%b)
  deallocate(var%c)
  deallocate(var%d)
  deallocate(var%s%q)

end program dtype
