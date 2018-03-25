! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

subroutine init (v1, v2, N)
  !$omp declare target
  integer :: i, N
  real :: v1(N), v2(N)
  do i = 1, N
    v1(i) = i + 2.0
    v2(i) = i - 3.0
  end do
end subroutine

subroutine check (p, N)
  integer :: i, N
  real, parameter :: EPS = 0.00001
  real :: diff, p(N)
  do i = 1, N
    diff = p(i) - (i + 2.0) * (i - 3.0)
    if (diff > EPS .or. -diff > EPS) STOP 1
  end do
end subroutine

subroutine vec_mult (p, N)
  use omp_lib, only: omp_is_initial_device
  real :: p(N)
  real, allocatable :: v1(:), v2(:)
  integer :: i
  !$omp declare target (init)
  !$omp target data map(to: v1, v2, N) map(from: p)
    !$omp task shared(v1, v2, p) depend(out: v1, v2)
      !$omp target map(to: v1, v2, N)
        if (omp_is_initial_device ()) STOP 2
        allocate (v1(N), v2(N))
        call init (v1, v2, N)
      !$omp end target
    !$omp end task
    !$omp task shared(v1, v2, p) depend(in: v1, v2)
      !$omp target map(to: v1, v2, N) map(from: p)
        if (omp_is_initial_device ()) STOP 3
        !$omp parallel do
        do i = 1, N
          p(i) = v1(i) * v2(i)
        end do
        deallocate (v1, v2)
      !$omp end target
    !$omp end task
  !$omp end target data

  !$omp taskwait
  call check (p, N)
end subroutine

program e_55_2
  integer, parameter :: N = 1000
  real :: p(N)
  call vec_mult (p, N)
end program
