! PR fortran/92899

program pr92899
  real :: x = 1.0
  double precision :: y
  integer(kind=4) :: z = 4
  integer(kind=8) :: w
  !$omp atomic capture
  y = x
  x = 2.0
  !$omp end atomic
  if (y /= 1.0 .or. x /= 2.0) stop 1
  !$omp atomic capture
  x = y
  y = 3.0
  !$omp end atomic
  if (x /= 1.0 .or. y /= 3.0) stop 2
  !$omp atomic capture
  w = z
  z = 5
  !$omp end atomic
  if (w /= 4 .or. z /= 5) stop 3
  !$omp atomic capture
  z = w
  w = 6
  !$omp end atomic
  if (z /= 4 .or. w /= 6) stop 4
  !$omp atomic write
  x = y
  !$omp end atomic
  if (x /= 3.0 .or. y /= 3.0) stop 5
  x = 7.0
  !$omp atomic write
  y = x
  !$omp end atomic
  if (x /= 7.0 .or. y /= 7.0) stop 6
  !$omp atomic write
  z = w
  !$omp end atomic
  if (z /= 6 .or. w /= 6) stop 7
  z = 8
  !$omp atomic write
  w = z
  !$omp end atomic
  if (z /= 8 .or. w /= 8) stop 8
end
