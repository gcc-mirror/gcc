! { dg-do run }

module wrapper_mod

type compute
  integer, allocatable :: block(:,:)
contains
  procedure :: initialize
end type compute

type, extends(compute) :: cpu_compute
  integer :: blocksize
contains
  procedure :: setblocksize
end type cpu_compute

type, extends(compute) :: gpu_compute
  integer :: numgangs
  integer :: numworkers
  integer :: vectorsize
  integer, allocatable :: gpu_block(:,:)
contains
  procedure :: setdims
end type gpu_compute

contains

subroutine initialize(c, length, width)
  implicit none
  class(compute) :: c
  integer :: length
  integer :: width
  integer :: i
  integer :: j

  allocate (c%block(length, width))

  do i=1,length
    do j=1, width
      c%block(i,j) = i + j
    end do
  end do
end subroutine initialize

subroutine setdims(c, g, w, v)
  implicit none
  class(gpu_compute) :: c
  integer :: g
  integer :: w
  integer :: v
  c%numgangs = g
  c%numworkers = w
  c%vectorsize = v
end subroutine setdims

subroutine setblocksize(c, bs)
  implicit none
  class(cpu_compute) :: c
  integer :: bs
  c%blocksize = bs
end subroutine setblocksize

end module wrapper_mod

program main
  use wrapper_mod
  implicit none
  class(compute), allocatable, target :: mycomp
  integer :: i, j

  allocate(gpu_compute::mycomp)

  call mycomp%initialize(1024,1024)

  !$acc enter data copyin(mycomp)

  select type (mycomp)
  type is (cpu_compute)
    call mycomp%setblocksize(32)
  type is (gpu_compute)
    call mycomp%setdims(32,32,32)
    allocate(mycomp%gpu_block(1024,1024))
    !$acc update device(mycomp)
    !$acc parallel copyin(mycomp%block) copyout(mycomp%gpu_block)
    !$acc loop gang worker vector collapse(2)
    do i=1,1024
      do j=1,1024
        mycomp%gpu_block(i,j) = mycomp%block(i,j) + 1
      end do
    end do
    !$acc end parallel
  end select

  !$acc exit data copyout(mycomp)

  select type (g => mycomp)
  type is (gpu_compute)
  do i = 1, 1024
    do j = 1, 1024
      if (g%gpu_block(i,j) .ne. i + j + 1) stop 1
    end do
  end do
  end select

  deallocate(mycomp)
end program main
