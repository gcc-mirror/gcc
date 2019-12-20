! { dg-do run }

! Test of attach/detach, "enter data" inside "data", and subarray.

program dtype
  implicit none
  integer, parameter :: n = 512
  type mytype
    integer, allocatable :: a(:)
    integer, allocatable :: b(:)
  end type mytype
  integer i

  type(mytype) :: var

  allocate(var%a(1:n))
  allocate(var%b(1:n))

!$acc data copy(var)

  do i = 1, n
    var%a(i) = 0
    var%b(i) = 0
  end do

!$acc enter data copyin(var%a(5:n - 5), var%b(5:n - 5))

!$acc parallel loop
  do i = 5,n - 5
    var%a(i) = i
    var%b(i) = i * 2
  end do
!$acc end parallel loop

!$acc exit data copyout(var%a(5:n - 5), var%b(5:n - 5))

!$acc end data

  do i = 1,4
    if (var%a(i) .ne. 0) stop 1
    if (var%b(i) .ne. 0) stop 2
  end do

  do i = 5,n - 5
    if (i .ne. var%a(i)) stop 3
    if (i * 2 .ne. var%b(i)) stop 4
  end do

  do i = n - 4,n
    if (var%a(i) .ne. 0) stop 5
    if (var%b(i) .ne. 0) stop 6
  end do

  deallocate(var%a)
  deallocate(var%b)

end program dtype
