! { dg-do run }

! Test of attachment counters and finalize.

program dtype
  use openacc
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

  do i = 1,20
    !$acc enter data attach(var%a)
  end do

!$acc parallel loop
  do i = 5,n - 5
    var%a(i) = i
    var%b(i) = i * 2
  end do
!$acc end parallel loop

  if (.not. acc_is_present(var%a(5:n - 5))) stop 11
  if (.not. acc_is_present(var%b(5:n - 5))) stop 12
  if (.not. acc_is_present(var)) stop 13
!$acc exit data copyout(var%a(5:n - 5), var%b(5:n - 5)) finalize
  if (acc_get_device_type() .ne. acc_device_host) then
     if (acc_is_present(var%a(5:n - 5))) stop 21
     if (acc_is_present(var%b(5:n - 5))) stop 22
  end if
  if (.not. acc_is_present(var)) stop 23

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
