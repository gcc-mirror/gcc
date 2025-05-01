! { dg-additional-options "-fopenmp-allocators" }
! { dg-additional-options "-fdump-tree-omplower" }
program main
  use iso_c_binding
  use omp_lib
  implicit none (type, external)
  integer(omp_allocator_handle_kind):: alloc_h
  integer :: i, N
  integer(c_intptr_t) :: intptr
  integer, allocatable :: A(:)
  type(omp_alloctrait):: traits(1) = [omp_alloctrait(omp_atk_alignment, 128)]

  N = 10
  alloc_h = omp_init_allocator(omp_default_mem_space, 1, traits)

  !$omp allocate(A) allocator(alloc_h)
  allocate(A(N))
  a(:) = [(i, i=1,N)]
  if (mod (transfer (loc(a), intptr),128) /= 0) &
    stop 1
  if (any (a /= [(i, i=1,N)])) &
    stop 2
  deallocate(A)
  !$omp allocate(A) allocator(alloc_h) align(512)
  allocate(A(N))
  block
    integer, allocatable :: B(:)
    !$omp allocators allocate(allocator(alloc_h), align(256) : B)
    allocate(B(N))
    B(:) = [(2*i, i=1,N)]
    A(:) = B
    if (mod (transfer (loc(B), intptr), 256) /= 0) &
      stop 1
    ! end of scope deallocation
  end block
  if (mod (transfer (loc(a), intptr),512) /= 0) &
    stop 1
  if (any (a /= [(2*i, i=1,N)])) &
    stop 2
  deallocate(A) ! Must deallocate here - before deallocator is destroyed
  call omp_destroy_allocator(alloc_h)
  ! No auto dealloc of A because it is SAVE
end
! { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(" 3 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(" 3 "omplower" } }
