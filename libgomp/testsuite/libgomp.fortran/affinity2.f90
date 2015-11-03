! { dg-do run }
! { dg-additional-options "-fdefault-integer-8" }
! { dg-skip-if "" { ! run_expensive_tests } { "*" } { "-O2" } }
! { dg-set-target-env-var OMP_PROC_BIND "spread,close" }
! { dg-set-target-env-var OMP_PLACES "{6,7}:4:-2,!{2,3}" }
! { dg-set-target-env-var OMP_NUM_THREADS "2" }

include 'affinity1.f90'
