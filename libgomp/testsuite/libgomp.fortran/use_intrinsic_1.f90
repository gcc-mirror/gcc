! { dg-do compile }
!
! PR fortran/55197
!
! Contributed by Erik Toussaint
!

use, intrinsic :: omp_lib, only: omp_get_num_threads
use, intrinsic :: omp_lib_kinds , foo => omp_lock_kind
print *, foo
end
