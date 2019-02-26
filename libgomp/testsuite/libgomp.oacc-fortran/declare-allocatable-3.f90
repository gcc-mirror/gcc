! Test declare create with allocatable arrays.

! { dg-do run }

module vars
  implicit none
  integer, parameter :: n = 100
  real*8, allocatable :: a, b(:)
 !$acc declare create (a, b)
end module vars

program test
  use vars
  use openacc
  implicit none
  integer :: i

  interface
     subroutine sub1
       !$acc routine gang
     end subroutine sub1

     subroutine sub2
     end subroutine sub2

     real*8 function fun1 (ix)
       integer ix
       !$acc routine seq
     end function fun1

     real*8 function fun2 (ix)
       integer ix
       !$acc routine seq
     end function fun2
  end interface

  if (allocated (a)) stop 1
  if (allocated (b)) stop 2

  ! Test local usage of an allocated declared array.

  allocate (a)

  if (.not.allocated (a)) stop 3
  if (acc_is_present (a) .neqv. .true.) stop 4

  allocate (b(n))

  if (.not.allocated (b)) stop 5
  if (acc_is_present (b) .neqv. .true.) stop 6

  a = 2.0
  !$acc update device(a)

  !$acc parallel loop
  do i = 1, n
     b(i) = i * a
  end do

  if (.not.acc_is_present (b)) stop 7

  !$acc update host(b)

  do i = 1, n
     if (b(i) /= i*a) stop 8
  end do

  deallocate (b)

  ! Test the usage of an allocated declared array inside an acc
  ! routine subroutine.

  allocate (b(n))

  if (.not.allocated (b)) stop 9
  if (acc_is_present (b) .neqv. .true.) stop 10

  !$acc parallel
  call sub1
  !$acc end parallel

  if (.not.acc_is_present (b)) stop 11

  !$acc update host(b)

  do i = 1, n
     if (b(i) /= a+i*2) stop 12
  end do

  deallocate (b)

  ! Test the usage of an allocated declared array inside a host
  ! subroutine.

  call sub2

  if (.not.acc_is_present (b)) stop 13

  !$acc update host(b)

  do i = 1, n
     if (b(i) /= 1.0) stop 14
  end do

  deallocate (b)

  if (allocated (b)) stop 15

  ! Test the usage of an allocated declared array inside an acc
  ! routine function.

  allocate (b(n))

  if (.not.allocated (b)) stop 16
  if (acc_is_present (b) .neqv. .true.) stop 17

  !$acc parallel loop
  do i = 1, n
     b(i) = 1.0
  end do

  !$acc parallel loop
  do i = 1, n
     b(i) = fun1 (i)
  end do

  if (.not.acc_is_present (b)) stop 18

  !$acc update host(b)

  do i = 1, n
     if (b(i) /= i) stop 19
  end do

  deallocate (b)

  ! Test the usage of an allocated declared array inside a host
  ! function.

  allocate (b(n))

  if (.not.allocated (b)) stop 20
  if (acc_is_present (b) .neqv. .true.) stop 21

  !$acc parallel loop
  do i = 1, n
     b(i) = 1.0
  end do

  !$acc update host(b)

  do i = 1, n
     b(i) = fun2 (i)
  end do

  if (.not.acc_is_present (b)) stop 22

  do i = 1, n
     if (b(i) /= i*a) stop 23
  end do

  deallocate (a)
  deallocate (b)
end program test

! Set each element in array 'b' at index i to a+i*2.

subroutine sub1 ! { dg-warning "region is worker partitioned" }
  use vars
  implicit none
  integer i
  !$acc routine gang

  !$acc loop
  do i = 1, n
     b(i) = a+i*2
  end do
end subroutine sub1

! Allocate array 'b', and set it to all 1.0.

subroutine sub2
  use vars
  use openacc
  implicit none
  integer i

  allocate (b(n))

  if (.not.allocated (b)) stop 24
  if (acc_is_present (b) .neqv. .true.) stop 25

  !$acc parallel loop
  do i = 1, n
     b(i) = 1.0
  end do
end subroutine sub2

! Return b(i) * i;

real*8 function fun1 (i)
  use vars
  implicit none
  integer i
  !$acc routine seq

  fun1 = b(i) * i
end function fun1

! Return b(i) * i * a;

real*8 function fun2 (i)
  use vars
  implicit none
  integer i

  fun2 = b(i) * i * a
end function fun2
