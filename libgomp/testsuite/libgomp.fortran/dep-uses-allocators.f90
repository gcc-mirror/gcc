! { dg-do compile }
! { dg-warning "The specification of arguments to 'uses_allocators' at \\(1\\) where each item is of the form 'allocator\\(traits\\)' is deprecated since OpenMP 5.2; instead use 'uses_allocators\\(traits\\(trait\\): a1\\)' \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* } 11 }

program test
  use omp_lib
  implicit none
  integer(kind=omp_allocator_handle_kind) :: a1

  type(omp_alloctrait), parameter :: trait(0) = [omp_alloctrait :: ]

  !$omp target uses_allocators(omp_default_mem_alloc, a1(trait))
  ! { dg-message "sorry, unimplemented: 'uses_allocators' clause with traits and memory spaces" "" { target *-*-* } .-1 }
  block; end block
end program
