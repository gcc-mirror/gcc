! { dg-additional-options "-fopenmp-allocators -fdump-tree-original" }
module m
  use omp_lib
  use iso_c_binding, only: c_intptr_t
  implicit none (type,external)
  integer(omp_allocator_handle_kind) :: handle  
  integer(c_intptr_t) :: iptr
end module m

subroutine scalar
  use m
  implicit none (type,external)
  integer :: i
  integer, allocatable :: SSS
  i = 5  ! required executive statement before 'omp allocators'
  !$omp allocate allocator(handle)
  allocate(SSS)
  if (mod (loc (sss), 64) /= 0) stop 1
  deallocate(SSS)
  allocate(SSS)
end
! { dg-final { scan-tree-dump-times "sss = \\(integer\\(kind=4\\) \\*\\) __builtin_GOMP_alloc \\(4, 4, D\\.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "GOMP_add_alloc \\(sss\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "if \\(GOMP_is_alloc \\(sss\\)\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(sss, 0B\\);" 2 "original" } }

subroutine array
  use m
  implicit none (type,external)
  integer :: i
  integer, allocatable :: A(:)
  i = 5  ! required executive statement before 'omp allocators'
  !$omp allocate allocator(handle) align(512)
  allocate(A(5))
  if (mod (loc (A), 512) /= 0) stop 2
  A=[1]
  if (mod (loc (A), 64) /= 0) stop 3
  deallocate(A)
  A=[1]
  deallocate(A)
  call omp_set_default_allocator (handle)
  !$omp allocate
  allocate(A(7))
  if (mod (loc (A), 64) /= 0) stop 4
end
! { dg-final { scan-tree-dump-times "a.dtype = {.elem_len=4, .version=0, .rank=1, .type=1};" 5 "original" } }
! { dg-final { scan-tree-dump-times "\\.elem_len=4" 5 "original" } }
! { dg-final { scan-tree-dump-times "a.data = \\(void \\* restrict\\) __builtin_GOMP_alloc \\(512, 20, D\\.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "a.data = \\(void \\* restrict\\) __builtin_GOMP_alloc \\(4, 28, 0B\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "a.dtype.version = 1;" 2 "original" } }
! { dg-final { scan-tree-dump-times "a.data = \\(void \\* restrict\\) \\(a.dtype.version == 1 \\? __builtin_omp_realloc \\(\\(void \\*\\) a.data, 4, 0B, 0B\\) : __builtin_realloc \\(\\(void \\*\\) a.data, 4\\)\\);" 2 "original" } }
! { dg-final { scan-tree-dump-times "if \\(a.dtype.version == 1\\)" 3 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) a.data, 0B\\);" 3 "original" } }
! { dg-final { scan-tree-dump-times "a.dtype.version = 0;" 3 "original" } }

program main
  use m
  implicit none (type,external)
  external :: scalar, array
  type (omp_alloctrait), parameter :: traits(*) &
      = [omp_alloctrait(omp_atk_sync_hint, omp_atv_contended), &
         omp_alloctrait(omp_atk_alignment, 64)]
  handle = omp_init_allocator (omp_high_bw_mem_alloc, size(traits), traits)
  call scalar
  call array
  call omp_destroy_allocator (handle)
end

