! { dg-do compile }

program main
  use omp_lib
  implicit none
  integer (omp_allocator_handle_kind) :: bar, foo

  type (omp_alloctrait), parameter :: traits_array(*) = &
       [omp_alloctrait(omp_atk_pinned,omp_atv_true),&
       omp_alloctrait(omp_atk_partition,omp_atv_nearest)]

  !$omp target uses_allocators(omp_non_existant_alloc) ! { dg-error "Allocator 'omp_non_existant_alloc' at .1. in USES_ALLOCATORS must be a scalar integer of kind 'omp_allocator_handle_kind'" }
  block  ! { dg-error "Symbol 'omp_non_existant_alloc' at .1. has no IMPLICIT type; did you mean 'omp_const_mem_alloc'\?" "" { target *-*-* } .-1 }
  end block

  !$omp target uses_allocators(bar(traits_array), foo (traits_array), ) ! { dg-error "Invalid character in name" }
  block
  end block

  !$omp target uses_allocators(traits(xyz) : bar) ! { dg-error "Symbol 'xyz' at .1. has no IMPLICIT type" }
  block  ! { dg-error "Traits array 'xyz' in USES_ALLOCATORS .1. must be a one-dimensional named constant array of type 'omp_alloctrait'" "" { target *-*-* } .-1 }
  end block

  !$omp target uses_allocators(memspace(omp_non_existant_mem_space) : foo) ! { dg-error "Symbol 'omp_non_existant_mem_space' at .1. has no IMPLICIT type; did you mean 'omp_const_mem_space'\?" }
  ! { dg-error "Memspace 'omp_non_existant_mem_space' at .1. in USES_ALLOCATORS must be a predefined memory space" "" { target *-*-* } .-1 }

  block
  end block

  !$omp target uses_allocators(traits(traits_array), traits(traits_array) : bar) ! { dg-error "Duplicate TRAITS modifier at .1. in USES_ALLOCATORS clause" }
  block
  end block

  !$omp target uses_allocators(memspace(omp_default_mem_space), memspace(omp_default_mem_space) : foo) ! { dg-error "Duplicate MEMSPACE modifier at .1. in USES_ALLOCATORS clause" }
  block
  end block

  !$omp target uses_allocators(memspace(omp_default_mem_space), traits(traits_array), traits(traits_array) : foo) ! { dg-error "Duplicate TRAITS modifier at .1. in USES_ALLOCATORS clause" }
  block
  end block

  !$omp target uses_allocators (omp_null_allocator) ! { dg-error "Allocator 'omp_null_allocator' at .1. in USES_ALLOCATORS must either a variable or a predefined allocator" }
  block
  end block

  !$omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo, bar)
  block
  end block

  !$omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo(foo_traits)) ! { dg-error "70:Unexpected '\\(' at .1." }
  block
  end block

end program main
