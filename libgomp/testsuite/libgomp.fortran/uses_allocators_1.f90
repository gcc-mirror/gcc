! { dg-do compile }

subroutine test
  use omp_lib
  implicit none

  !$omp target uses_allocators ( omp_default_mem_alloc , omp_large_cap_mem_alloc, &
  !$omp&                         omp_const_mem_alloc,omp_high_bw_mem_alloc, &
  !$omp&                         omp_low_lat_mem_alloc ,omp_cgroup_mem_alloc , &
  !$omp&                         omp_pteam_mem_alloc, omp_thread_mem_alloc )
  block; end block

  !$omp target uses_allocators(omp_default_mem_alloc, omp_high_bw_mem_alloc) &
  !$omp&       uses_allocators(omp_high_bw_mem_alloc, omp_low_lat_mem_alloc)  ! { dg-error "Symbol 'omp_high_bw_mem_alloc' present on multiple clauses" }
  block; end block

  !$omp target firstprivate ( omp_default_mem_alloc ) , uses_allocators &
  !$omp&                                                 (omp_default_mem_alloc , omp_high_bw_mem_alloc ) &
  !$omp&       map(to: omp_high_bw_mem_alloc)
  block; end block
! { dg-error "Object 'omp_default_mem_alloc' is not a variable" "" { target *-*-* } .-4 }
! { dg-error "Symbol 'omp_default_mem_alloc' present on both data and map clauses" "" { target *-*-* } .-5 }
! { dg-error "Symbol 'omp_high_bw_mem_alloc' present on multiple clauses" "" { target *-*-* } .-5 }
! { dg-error "Object 'omp_high_bw_mem_alloc' is not a variable at .1.; parameters cannot be and need not be mapped" "" { target *-*-* } .-5 }
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

  !$omp target uses_allocators(omp_default_mem_alloc, a1(trait), a2(trait2))
  block; end block

  !$omp target uses_allocators(omp_default_mem_alloc, a1(trait), omp_cgroup_mem_alloc, a1(trait2)) ! { dg-error "Symbol 'a1' present on multiple clauses" }
  block; end block

  !$omp target uses_allocators(traits(trait):a1) &
  !$omp&        uses_allocators ( memspace ( omp_low_lat_mem_space ) , traits ( trait2 ) : a2 , a3)
  block; end block

  !$omp target uses_allocators ( traits(trait2) , memspace ( omp_low_lat_mem_space ) : a2 , a3)
  block; end block

  !$omp target firstprivate ( a2 ) , &  ! { dg-error "Symbol 'a2' present on both data and map clauses" }
  !$omp&       uses_allocators (a2, a3) &  ! { dg-error "Symbol 'a3' present on multiple clauses" }
  !$omp&       map(to: a3)
  block; end block
end subroutine

subroutine duplicate
  use omp_lib
  implicit none
  type(omp_alloctrait), parameter :: trait1(0) = [omp_alloctrait :: ]
  type(omp_alloctrait), parameter :: trait2(0) = [omp_alloctrait :: ]

  !$omp target uses_allocators(traits(trait1), memspace ( omp_low_lat_mem_space ) , traits ( trait2 ) : bar)  ! { dg-error "Duplicate TRAITS modifier" }
  block; end block

  !$omp target uses_allocators(traits(trait1), memspace ( omp_low_lat_mem_space ) , memspace (omp_large_cap_mem_space) : bar)  ! { dg-error "Duplicate MEMSPACE modifier" }
  block; end block
end

subroutine trait_present
  use omp_lib
  implicit none

  type(omp_alloctrait), parameter :: trait1(0) = [omp_alloctrait :: ]
  integer(kind=omp_allocator_handle_kind) :: a1

  !$omp target uses_allocators(omp_cgroup_mem_alloc(trait1))  ! { dg-error "A memory space or traits array may not be specified for predefined allocator 'omp_cgroup_mem_alloc'" }
  block; end block

  !$omp target uses_allocators(traits(trait1) : omp_pteam_mem_alloc)  ! { dg-error "A memory space or traits array may not be specified for predefined allocator 'omp_pteam_mem_alloc'" }
  block; end block

  !$omp target uses_allocators(memspace(omp_low_lat_mem_space) : omp_thread_mem_alloc)  ! { dg-error "A memory space or traits array may not be specified for predefined allocator 'omp_thread_mem_alloc'" }
  block; end block

  ! Invalid in OpenMP 5.0 / 5.1, but valid since 5.2 the same as omp_default_mem_space + emptry traits array
  !$omp target uses_allocators ( a1 )
  block; end block
end

subroutine odd_names
  use omp_lib
  implicit none

  type(omp_alloctrait), parameter :: trait1(0) = [omp_alloctrait :: ]

  ! oddly named allocators:
  integer(kind=omp_allocator_handle_kind) :: traits
  integer(kind=omp_allocator_handle_kind) :: memspace

  !$omp target uses_allocators ( traits(trait1), memspace(trait1) )
  block; end block

  !$omp target uses_allocators ( traits(trait1), memspace(omp_low_lat_mem_space)  : traits)
  block; end block

  !$omp target uses_allocators ( memspace(omp_low_lat_mem_space), traits(trait1) : memspace)
  block; end block
end

subroutine more_checks
  use omp_lib
  implicit none

  integer(kind=kind(omp_low_lat_mem_space)) :: my_memspace
  integer(kind=omp_allocator_handle_kind) :: a1, a2(4)
  integer(kind=1) :: a3

  !$omp target uses_allocators ( memspace(my_memspace) : a1)  ! { dg-error "Memspace 'my_memspace' at .1. in USES_ALLOCATORS must be a predefined memory space" }
  block; end block

  !$omp target uses_allocators ( omp_low_lat_mem_space)  ! { dg-error "Allocator 'omp_low_lat_mem_space' at .1. in USES_ALLOCATORS must either a variable or a predefined allocator" }
  block; end block

  !$omp target uses_allocators ( memspace (omp_low_lat_mem_alloc) : a1)  ! { dg-error "Memspace 'omp_low_lat_mem_alloc' at .1. in USES_ALLOCATORS must be a predefined memory space" }
  block; end block

  !$omp target uses_allocators(memspace (omp_low_lat_mem_space) : a1 )
  block; end block

  !$omp target uses_allocators(memspace (omp_low_lat_mem_space) : a2 )  ! { dg-error "Allocator 'a2' at .1. in USES_ALLOCATORS must be a scalar integer of kind 'omp_allocator_handle_kind'" }
  block; end block

  !$omp target uses_allocators(memspace (omp_low_lat_mem_space) : a3 )  ! { dg-error "Allocator 'a3' at .1. in USES_ALLOCATORS must be a scalar integer of kind 'omp_allocator_handle_kind'" }
  block; end block
end

subroutine traits_checks
  use omp_lib
  implicit none

  type(omp_alloctrait), parameter :: trait1 = omp_alloctrait (omp_atk_alignment, 16)
  type(omp_alloctrait) :: trait2
  integer(kind=omp_atk_alignment), parameter :: trait3(1) = omp_atk_alignment
  integer(kind=omp_allocator_handle_kind) :: a1

  ! Sensible - but not (yet?) valid - an array constructor:
  !$omp target uses_allocators(traits ([omp_alloctrait :: ]) : a1 )  ! { dg-error "Invalid character in name" }
  block; end block
  !$omp target uses_allocators(a1 ([omp_alloctrait :: ]))  ! { dg-error "Invalid character in name" }
  block; end block

  !$omp target uses_allocators(traits (trait1) : a1 )  ! { dg-error "Traits array 'trait1' in USES_ALLOCATORS .1. must be a one-dimensional named constant array of type 'omp_alloctrait'" }
  block; end block
  !$omp target uses_allocators(a1 (trait1))  ! { dg-error "Traits array 'trait1' in USES_ALLOCATORS .1. must be a one-dimensional named constant array of type 'omp_alloctrait'" }
  block; end block

  !$omp target uses_allocators(traits (trait2) : a1 )  ! { dg-error "Traits array 'trait2' in USES_ALLOCATORS .1. must be a one-dimensional named constant array of type 'omp_alloctrait'" }
  block; end block
  !$omp target uses_allocators(a1 (trait2))  ! { dg-error "Traits array 'trait2' in USES_ALLOCATORS .1. must be a one-dimensional named constant array of type 'omp_alloctrait'" }
  block; end block

  !$omp target uses_allocators(traits (trait3) : a1 )  ! { dg-error "Traits array 'trait3' in USES_ALLOCATORS .1. must be a one-dimensional named constant array of type 'omp_alloctrait'" }
  block; end block
  !$omp target uses_allocators(a1 (trait3))  ! { dg-error "Traits array 'trait3' in USES_ALLOCATORS .1. must be a one-dimensional named constant array of type 'omp_alloctrait'" }
  block; end block
end
