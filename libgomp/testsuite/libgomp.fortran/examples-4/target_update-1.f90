! { dg-do run }

module e_52_1_mod
contains
  subroutine init (v1, v2, N)
    integer :: i, N
    real :: v1(N), v2(N)
    do i = 1, N
      v1(i) = i + 2.0
      v2(i) = i - 3.0
    end do
  end subroutine

  subroutine init_again (v1, v2, N)
    integer :: i, N
    real :: v1(N), v2(N)
    do i = 1, N
      v1(i) = i - 3.0
      v2(i) = i + 2.0
    end do
  end subroutine

  subroutine check (p, N)
    integer :: i, N
    real, parameter :: EPS = 0.00001
    real :: diff, p(N)
    do i = 1, N
      diff = p(i) - 2 * (i + 2.0) * (i - 3.0)
      if (diff > EPS .or. -diff > EPS) call abort
    end do
  end subroutine

  subroutine vec_mult (p, v1, v2, N)
    real :: p(N), v1(N), v2(N)
    integer :: i, N
    call init (v1, v2, N)
    !$omp target data map(to: v1, v2) map(from: p)
      !$omp target
        !$omp parallel do
        do i = 1, N
          p(i) = v1(i) * v2(i)
        end do
      !$omp end target
      call init_again (v1, v2, N)
      !$omp target update to(v1, v2)
      !$omp target
        !$omp parallel do
        do i = 1, N
          p(i) = p(i) + v1(i) * v2(i)
        end do
      !$omp end target
    !$omp end target data
    call check (p, N)
  end subroutine
end module

program e_52_1
  use e_52_1_mod, only : vec_mult
  integer :: n
  real, pointer :: p(:), v1(:), v2(:)
  n = 1000
  allocate (p(n), v1(n), v2(n))
  call vec_mult (p, v1, v2, n)
  deallocate (p, v1, v2)
end program
