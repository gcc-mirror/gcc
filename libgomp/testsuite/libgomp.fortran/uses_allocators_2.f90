! { dg-do compile }

! Minimal test for valid code:
! - predefined allocators do not need any special treatment in uses_allocators
!   (as 'requires dynamic_allocators' is the default).
!
! - Non-predefined allocators are currently rejected ('sorry)'

subroutine test
  use omp_lib
  implicit none

  !$omp target uses_allocators ( omp_default_mem_alloc , omp_large_cap_mem_alloc, &
  !$omp&                         omp_const_mem_alloc,omp_high_bw_mem_alloc, &
  !$omp&                         omp_low_lat_mem_alloc ,omp_cgroup_mem_alloc , &
  !$omp&                         omp_pteam_mem_alloc, omp_thread_mem_alloc )
  block; end block

  !$omp target parallel uses_allocators ( omp_default_mem_alloc , omp_large_cap_mem_alloc, &
  !$omp&                                  omp_const_mem_alloc,omp_high_bw_mem_alloc, &
  !$omp&                                  omp_low_lat_mem_alloc ,omp_cgroup_mem_alloc , &
  !$omp&                                  omp_pteam_mem_alloc, omp_thread_mem_alloc )
  block; end block
end

subroutine non_predef
  use omp_lib
  implicit none

  type(omp_alloctrait), parameter :: trait(0) = [omp_alloctrait :: ]
  type(omp_alloctrait), parameter :: trait2(*) &
    = [omp_alloctrait (omp_atk_alignment, 16),                    &
       omp_alloctrait (omp_atk_sync_hint, omp_atv_default),       &
       omp_alloctrait (omp_atk_access, omp_atv_default)]

  integer(kind=omp_allocator_handle_kind) :: a1, a2, a3

  !$omp target uses_allocators(omp_default_mem_alloc, a1(trait), a2(trait2))  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block

  !$omp target parallel uses_allocators(omp_default_mem_alloc, a1(trait), a2(trait2))  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block


  !$omp target uses_allocators(traits(trait):a1) &
  !$omp&        uses_allocators ( memspace ( omp_low_lat_mem_space ) , traits ( trait2 ) : a2 , a3)  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block

  !$omp target parallel uses_allocators(traits(trait):a1) &
  !$omp&        uses_allocators ( memspace ( omp_low_lat_mem_space ) , traits ( trait2 ) : a2 , a3)  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block

  !$omp target uses_allocators ( traits(trait2) , memspace ( omp_low_lat_mem_space ) : a2 , a3)  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block
end subroutine

subroutine trait_present
  use omp_lib
  implicit none

  type(omp_alloctrait), parameter :: trait1(0) = [omp_alloctrait :: ]
  integer(kind=omp_allocator_handle_kind) :: a1

  ! Invalid in OpenMP 5.0 / 5.1, but valid since 5.2 the same as omp_default_mem_space + emptry traits array
  !$omp target uses_allocators ( a1 )  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block
end

subroutine odd_names
  use omp_lib
  implicit none

  type(omp_alloctrait), parameter :: trait1(0) = [omp_alloctrait :: ]

  ! oddly named allocators:
  integer(kind=omp_allocator_handle_kind) :: traits
  integer(kind=omp_allocator_handle_kind) :: memspace

  !$omp target uses_allocators ( traits(trait1), memspace(trait1) )  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block

  !$omp target uses_allocators ( traits(trait1), memspace(omp_low_lat_mem_space)  : traits)  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block

  !$omp target uses_allocators ( memspace(omp_low_lat_mem_space), traits(trait1) : memspace)  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block
end

subroutine more_checks
  use omp_lib
  implicit none

  integer(kind=kind(omp_low_lat_mem_space)) :: my_memspace
  integer(kind=omp_allocator_handle_kind) :: a1, a2(4)
  integer(kind=1) :: a3

  !$omp target uses_allocators(memspace (omp_low_lat_mem_space) : a1 )  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" }
  block; end block
end
