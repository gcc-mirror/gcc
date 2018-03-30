! PR fortran/42866
! { dg-do run }

program pr42866
  integer, allocatable :: a(:)
  allocate (a(16))
  a = 0
  !$omp parallel
    !$omp sections reduction(+:a)
      a = a + 1
    !$omp section
      a = a + 2
    !$omp end sections
  !$omp end parallel
  if (any (a.ne.3)) STOP 1
  deallocate (a)
end
