! { dg-do run }
!TODO { dg-prune-output {using 'vector_length \(32\)', ignoring 1} }
! { dg-additional-options -foffload-options=-lm }

include '../../../gcc/testsuite/gfortran.fortran-torture/execute/math.f90'
