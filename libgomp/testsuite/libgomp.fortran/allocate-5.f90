! { dg-additional-options "-fdump-tree-gimple" }

! { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(" 5 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(" 5 "gimple" } }


module m
  use omp_lib
  use iso_c_binding
  implicit none (type, external)
  integer(c_intptr_t) :: intptr
contains

integer function one ()
  integer :: sum, i
  !$omp allocate(sum)
  ! { dg-final { scan-tree-dump-times "sum\\.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 4, 0B\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(sum\\.\[0-9\]+, 0B\\);" 1 "gimple" } }

  ! NOTE: Initializer cannot be omp_init_allocator - as 'A' is
  ! in the same scope and the auto-omp_free comes later than
  ! any omp_destroy_allocator.
  integer(omp_allocator_handle_kind) :: my_allocator = omp_low_lat_mem_alloc
  integer :: n = 25
  sum = 0
 block
  type(omp_alloctrait) :: traits(1) = [ omp_alloctrait(omp_atk_alignment, 64) ]
  integer :: A(n)
  !$omp allocate(A) align(128) allocator(my_allocator)
  ! { dg-final { scan-tree-dump-times "a = __builtin_GOMP_alloc \\(128, D\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(a, 0B\\);" 1 "gimple" } }

  if (mod (transfer(loc(A), intptr), 128_c_intptr_t) /= 0) &
    stop 2
  do i = 1, n
    A(i) = i
  end do

  my_allocator = omp_init_allocator(omp_low_lat_mem_space,1,traits)
  block
    integer B(n)
    integer C(5)
    !$omp allocate(B,C) allocator(my_allocator)
    ! { dg-final { scan-tree-dump-times "b = __builtin_GOMP_alloc \\(\[0-9\]+, D\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }
    ! { dg-final { scan-tree-dump-times "c\\.\[0-9\]+ = __builtin_GOMP_alloc \\(\[0-9\]+, 20, D\\.\[0-9\]+\\);" 1 "gimple" } }
    ! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(b, 0B\\);" 1 "gimple" } }
    ! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(c\\.\[0-9\]+, 0B\\);" 1 "gimple" } }

    integer :: D(5)
    !$omp allocate(D) align(256)
    ! { dg-final { scan-tree-dump-times "d\\.\[0-9\]+ = __builtin_GOMP_alloc \\(256, 20, 0B\\);" 1 "gimple" } }
    ! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(d\\.\[0-9\]+, 0B\\);" 1 "gimple" } }

    B = 0
    C = [1,2,3,4,5]
    D = [11,22,33,44,55]

    if (mod (transfer(loc(B), intptr), 64_c_intptr_t) /= 0) &
      stop 3
    if (mod (transfer(loc(C), intptr), 64_c_intptr_t) /= 0) &
      stop 4
    if (mod (transfer(loc(D), intptr), 256_c_intptr_t) /= 0) &
      stop 5

    do i = 1, 5
      if (C(i) /= i) &
        stop 6
      if (D(i) /= i + 10*i) &
        stop 7
    end do

    do i = 1, n
      if (B(i) /= 0) &
        stop 9
      sum = sum + A(i)+B(i)+C(mod(i,5)+1)+D(mod(i,5)+1)
    end do
  end block
  call omp_destroy_allocator (my_allocator)
 end block
 one = sum
end
end module

use m
if (one () /= 1225) &
  stop 1
end
