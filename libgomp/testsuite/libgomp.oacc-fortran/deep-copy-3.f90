! { dg-do run }

! Test of attach/detach with "acc parallel".

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
!$acc parallel loop copy(var%a(1:n)) copy(var%b(1:n))
  do i = 1,n
    var%a(i) = i
    var%b(i) = i
  end do
!$acc end parallel loop
!$acc end data

  do i = 1,n
    if (i .ne. var%a(i)) stop 1
    if (i .ne. var%b(i)) stop 2
  end do

  deallocate(var%a)
  deallocate(var%b)

end program dtype
