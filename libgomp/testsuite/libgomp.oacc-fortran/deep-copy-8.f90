! { dg-do run }

! Test of explicit attach/detach clauses and attachment counters. There are no
! acc_attach/acc_detach API routines in Fortran.

program dtype
  use openacc
  implicit none
  integer, parameter :: n = 512
  type mytype
    integer, allocatable :: a(:)
  end type mytype
  integer i

  type(mytype) :: var

  allocate(var%a(1:n))

  call acc_copyin(var)
  call acc_copyin(var%a)

  !$acc enter data attach(var%a)

!$acc parallel loop attach(var%a)
  do i = 1,n
    var%a(i) = i
  end do
!$acc end parallel loop

  !$acc exit data detach(var%a)

  call acc_copyout(var%a)
  call acc_copyout(var)

  do i = 1,n
    if (i .ne. var%a(i)) stop 1
  end do

  deallocate(var%a)

end program dtype
