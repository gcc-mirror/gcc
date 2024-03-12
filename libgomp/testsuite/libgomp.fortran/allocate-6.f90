! { dg-additional-options "-fdump-tree-gimple" }
module m
  use iso_c_binding
  use omp_lib
  implicit none (type, external)
  integer(c_intptr_t) :: intptr

! { dg-final { scan-tree-dump-not "__builtin_stack_save" "gimple" } }
! { dg-final { scan-tree-dump-not "__builtin_alloca" "gimple" } }
! { dg-final { scan-tree-dump-not "__builtin_stack_restore" "gimple" } }

! { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(" 5 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(" 5 "gimple" } }

contains

subroutine one ()
  integer :: result, n, i
  result = 0
  n = 3
  !$omp target map(tofrom: result) firstprivate(n)
    block
      integer :: var, var2(n)
      !$omp allocate(var,var2) align(128) allocator(omp_low_lat_mem_alloc)
      var = 5
! { dg-final { scan-tree-dump-times "var\\.\[0-9\]+ = __builtin_GOMP_alloc \\(128, 4, 5\\);" 1 "gimple" } } */
! { dg-final { scan-tree-dump-times "var2 = __builtin_GOMP_alloc \\(128, D\\.\[0-9\]+, 5\\);" 1 "gimple" } } */

! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(var\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(var2, 0B\\);" 1 "gimple" } } */

      if (mod(transfer(loc(var), intptr), 128_c_intptr_t) /= 0) &
        stop 1
      if (mod(transfer(loc(var2), intptr), 128_c_intptr_t) /= 0) &
        stop 2
      if (var /= 5) &
        stop 3

      !$omp parallel do
      do i = 1, n
        var2(i) = (i+32);
      end do

      !$omp parallel loop reduction(+:result)
      do i = 1, n
        result = result + var + var2(i)
      end do
    end block
  if (result /= (3*5 + 33 + 34 + 35)) &
    stop 4
end

subroutine two ()
  type st
    integer :: a, b
  end type
  integer :: scalar, array(5), i
  type(st) s
  !$omp allocate(scalar, array, s)
! { dg-final { scan-tree-dump-times "scalar\\.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 4, 0B\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "array\\.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 20, 0B\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "s\\.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 8, 0B\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(scalar\\.\[0-9\]+, 0B\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(array\\.\[0-9\]+, 0B\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(s\\.\[0-9\]+, 0B\\);" 1 "gimple" } }

  scalar = 44
  array = [1,2,3,4,5]
  s = st(a=11, b=56)

  !$omp parallel firstprivate(scalar) firstprivate(array) firstprivate(s)
    if (scalar /= 44) &
      stop 5
    scalar = 33;
    if (any (array /= [1,2,3,4,5])) &
      stop 6
    array = [10,20,30,40,50]
    if (s%a /= 11 .or. s%b /= 56) &
      stop 7
    s%a = 74
    s%b = 674
  !$omp end parallel

  if (scalar /= 44) &
    stop 8
  if (any (array /= [1,2,3,4,5])) &
    stop 9
  if (s%a /= 11 .or. s%b /= 56) &
    stop 10

  !$omp target defaultmap(firstprivate : scalar) defaultmap(none : aggregate) defaultmap(none : pointer)
    if (scalar /= 44) &
      stop 11
    scalar = 33;
  !$omp end target

  if (scalar /= 44) &
    stop 12

  !$omp target defaultmap(none : scalar) defaultmap(firstprivate : aggregate) defaultmap(none : pointer) private(i)
    if (any (array /= [1,2,3,4,5])) &
      stop 13
    do i = 1, 5
      array(i) = 10*i
    end do
  !$omp end target

  if (any(array /= [1,2,3,4,5])) &
    stop 13
  !$omp target defaultmap(none : scalar) defaultmap(firstprivate : aggregate) defaultmap(none : pointer)
    if (s%a /= 11 .or. s%b /= 56) &
      stop 14
    s%a = 74
    s%b = 674
  !$omp end target
  if (s%a /= 11 .or. s%b /= 56) &
    stop 15
end
end module

use m
  call one ()
  call two ()
end
