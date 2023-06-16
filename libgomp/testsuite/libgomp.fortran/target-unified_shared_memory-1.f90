! { dg-do run }

program requires_unified_shared_memory
  character(32) :: str
  !$omp requires unified_shared_memory

  str = trim (str)

  !$omp target
  block
  end block

end program requires_unified_shared_memory
