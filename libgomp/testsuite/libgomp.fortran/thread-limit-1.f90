! { dg-additional-options "-fdump-tree-original" }

! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(9\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target thread_limit\\(9\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp target nowait thread_limit\\(4\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(1\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp target thread_limit\\(6\\)" 1 "original" } }


module m
  use omp_lib
  implicit none
contains

subroutine uncalled()
    !$omp target teams thread_limit (9)
    !$omp end target teams
end

subroutine foo ()
  block
    !$omp target parallel nowait thread_limit (4) num_threads (1)
    if (omp_get_thread_limit () > 4) &
      stop 1
    !$omp end target parallel
  end block
  !$omp taskwait
end
end module

program main
  use m
  implicit none
  !$omp target thread_limit (6)
    if (omp_get_thread_limit () > 6) &
      stop 2
  !$omp end target
  call foo ()
end
