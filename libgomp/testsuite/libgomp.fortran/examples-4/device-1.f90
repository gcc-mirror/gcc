! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

program e_57_1
  use omp_lib, only: omp_is_initial_device
  integer :: a, b
  logical :: c, d

  a = 100
  b = 0

  !$omp target map(from: c) if(a > 200 .and. a < 400)
    c = omp_is_initial_device ()
  !$omp end target

  !$omp target data map(to: b) if(a > 200 .and. a < 400)
    !$omp target map(from: b, d)
      b = 100
      d = omp_is_initial_device ()
    !$omp end target
  !$omp end target data

  if (b /= 100 .or. .not. c .or. d) STOP 1

  a = a + 200
  b = 0

  !$omp target map(from: c) if(a > 200 .and. a < 400)
    c = omp_is_initial_device ()
  !$omp end target

  !$omp target data map(to: b) if(a > 200 .and. a < 400)
    !$omp target map(from: b, d)
      b = 100
      d = omp_is_initial_device ()
    !$omp end target
  !$omp end target data

  if (b /= 0 .or. c .or. d) STOP 2

  a = a + 200
  b = 0

  !$omp target map(from: c) if(a > 200 .and. a < 400)
    c = omp_is_initial_device ()
  !$omp end target

  !$omp target data map(to: b) if(a > 200 .and. a < 400)
    !$omp target map(from: b, d)
      b = 100
      d = omp_is_initial_device ()
    !$omp end target
  !$omp end target data

  if (b /= 100 .or. .not. c .or. d) STOP 3
end program
