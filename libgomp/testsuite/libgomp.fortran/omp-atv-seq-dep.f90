! { dg-do run }

program omp_atv_seq_dep
  use omp_lib ! { dg-warning "Using parameter 'omp_atv_sequential' declared at \\(1\\) is deprecated \\\[-Wdeprecated-declarations\\\]" }
  integer :: x
  x = omp_atv_sequential
end program
