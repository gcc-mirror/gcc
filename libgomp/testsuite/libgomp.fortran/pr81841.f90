! PR fortran/81841
! { dg-do run }

block data
  integer :: a
  real :: b(2)
  common /c/ a, b
  !$omp threadprivate (/c/)
  data a / 32 /
  data b /2*1./
end

program pr81841
  use omp_lib
  integer :: e
  real :: f(2)
  common /c/ e, f
  !$omp threadprivate (/c/)
  !$omp parallel num_threads(8)
  if ((e /= 32) .or. any(f /= 1.)) STOP 1
  e = omp_get_thread_num ()
  f = e + 19.
  !$omp barrier
  if ((e /= omp_get_thread_num ()) .or. any(f /= e + 19.)) STOP 2
  !$omp end parallel
end
