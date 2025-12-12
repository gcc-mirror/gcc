! { dg-additional-options "-fdump-tree-gimple" }

program main
  use iso_c_binding
  use omp_lib
  implicit none (type, external)
  integer :: x, xbuf(10)
  integer(c_intptr_t) :: iptr
  integer(omp_allocator_handle_kind) :: my_alloc
  type(omp_alloctrait), parameter :: trait(*) = [omp_alloctrait(omp_atk_alignment, 128)]

  !$omp target uses_allocators(omp_low_lat_mem_alloc) map(tofrom: x, xbuf) defaultmap(none)
    !$omp parallel allocate(allocator(omp_low_lat_mem_alloc), align(128): x, xbuf) if(.false.) firstprivate(x, xbuf)
      if (mod (TRANSFER (loc(x), iptr), 128) /= 0) &
        stop 1
      if (mod (TRANSFER (loc(xbuf), iptr), 128) /= 0) &
        stop 2
    !$omp end parallel
  !$omp end target

  my_alloc = transfer(int(z'ABCD', omp_allocator_handle_kind), my_alloc)

  !$omp target uses_allocators(traits(trait): my_alloc) defaultmap(none) map(tofrom: x, xbuf) 
    !$omp parallel allocate(allocator(my_alloc): x, xbuf) if(.false.) firstprivate(x, xbuf)
      if (mod (TRANSFER (loc(x), iptr), 128) /= 0) &
        stop 3
      if (mod (TRANSFER (loc(xbuf), iptr), 128) /= 0) &
        stop 4
    !$omp end parallel
  !$omp end target

  if (transfer(my_alloc, 0_omp_allocator_handle_kind) /= int(z'ABCD', omp_allocator_handle_kind)) &
    stop 5

  ! The following creates an allocator with empty traits + default mem space.
  !$omp target uses_allocators(my_alloc) map(tofrom: x, xbuf) defaultmap(none)
    !$omp parallel allocate(allocator(my_alloc), align(128): x, xbuf) if(.false.) firstprivate(x, xbuf)
      if (mod (TRANSFER (loc(x), iptr), 128) /= 0) &
        stop 6
      if (mod (TRANSFER (loc(xbuf), iptr), 128) /= 0) &
        stop 7
    !$omp end parallel
  !$omp end target

  if (transfer(my_alloc, 0_omp_allocator_handle_kind) /= int(z'ABCD', omp_allocator_handle_kind)) &
    stop 8
end


! FIXME ENABLE: 'dg FIXME final' -> 'dg-final'
! { dg  FIXME  final { scan-tree-dump-times "#pragma omp target .*private\\(my_alloc\\).*uses_allocators\\(my_alloc: memspace\\(\\), traits\\(trait\\)\\)" 1 "gimple" } }
! { dg  FIXME  final { scan-tree-dump-times "#pragma omp target .*private\\(my_alloc\\).*uses_allocators\\(my_alloc: memspace\\(\\), traits\\(\\)\\)" 1 "gimple" } }

! FIXME ENABLE code above for "gimple" once it has been implemented:
! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" "" { target *-*-* } 23 }
! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" "" { target *-*-* } 36 }
! { dg-bogus "'my_alloc' not specified in enclosing 'target'" "bogus issue because clause is ignored" { xfail *-*-* } 24 }
! { dg-bogus "'my_alloc' not specified in enclosing 'target'" "bogus issue because clause is ignored" { xfail *-*-* } 37 }
