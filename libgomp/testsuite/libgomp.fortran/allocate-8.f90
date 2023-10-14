module m
use omp_lib
implicit none
!!$omp requires dynamic_allocators

integer :: final_count

type t
  integer :: i = 0
  integer, allocatable :: A(:,:)
contains
  final :: count_finalization
end type t

contains

elemental impure subroutine count_finalization(self)
  type(t), intent(in) :: self
  final_count = final_count + 1
end

subroutine test(allocator)
integer(omp_allocator_handle_kind), optional, value :: allocator
call zero_size(allocator)
call finalization_test(allocator)
end subroutine test

subroutine finalization_test(allocator)
integer(omp_allocator_handle_kind), optional, value :: allocator
integer :: n = 5

final_count = 0;
block
  type(t) :: A
!  !$omp allocate(A) allocator(allocator)
  A%i = 1
end block
if (final_count /= 1) &
  stop 10

final_count = 0;
block
  type(t) :: B(7)
  !$omp allocate(B) allocator(allocator)
  B(1)%i = 1
end block
if (final_count /= 7) stop 10

final_count = 0;
block
  type(t) :: C(n)
!  !$omp allocate(C) allocator(allocator)
  C(1)%i = 1
end block
if (final_count /= 5) stop 10

final_count = 0;
block
  type(t) :: D(0)
!  !$omp allocate(D) allocator(allocator)
  D(1:0)%i = 1
end block
if (final_count /= 0) stop 10
end subroutine

subroutine zero_size(allocator)
integer(omp_allocator_handle_kind), optional, value :: allocator
integer :: n
n = -3

block
  integer :: A(n)
  character(len=n) :: B
!  !$omp allocate(A,b) allocator(allocator)
  if (size(A) /= 0 .or. len(b) /= 0) &
    stop 1
  B(1:len(b)) ='A'
end block

!!$omp target
block
  integer :: A(n)
  character(len=n) :: B
!  !$omp allocate(A,b) allocator(allocator)
  if (size(A) /= 0 .or. len(b) /= 0) &
    stop 2
  B(1:len(b)) ='A'
end block
end
end module

use m
call test()
call test(omp_default_mem_alloc)
call test(omp_large_cap_mem_alloc)
call test(omp_high_bw_mem_alloc)
call test(omp_low_lat_mem_alloc)
call test(omp_cgroup_mem_alloc)
end
