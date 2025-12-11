! { dg-do compile }

program hint_contended_dep
  use omp_lib ! { dg-warning "Using parameter 'omp_lock_hint_contended' declared at \\(1\\) is deprecated \\\[-Wdeprecated-declarations\\\]" }
  integer (kind=omp_lock_kind) :: lock
  call omp_init_lock_with_hint(lock, omp_lock_hint_contended)
  call omp_destroy_lock(lock)
end program
