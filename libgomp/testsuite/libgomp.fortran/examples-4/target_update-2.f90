! { dg-do run }

module e_52_2_mod
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
      diff = p(i) - (i * i + (i + 2.0) * (i - 3.0))
      if (diff > EPS .or. -diff > EPS) stop 1
    end do
  end subroutine

  logical function maybe_init_again (v, N)
    real :: v(N)
    integer :: i, N
    do i = 1, N
      v(i) = i
    end do
    maybe_init_again = .true.
  end function

  subroutine vec_mult (p, v1, v2, N)
    real :: p(N), v1(N), v2(N)
    integer :: i, N
    logical :: changed
    call init (v1, v2, N)
    !$omp target data map(to: v1, v2) map(from: p)
      !$omp target
        !$omp parallel do
        do i = 1, N
          p(i) = v1(i) * v2(i)
        end do
      !$omp end target
      changed = maybe_init_again (v1, N)
      !$omp target update if(changed) to(v1(:N))
      changed = maybe_init_again (v2, N)
      !$omp target update if(changed) to(v2(:N))
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

program e_52_2
  use e_52_2_mod, only : vec_mult
  integer :: n
  real, pointer :: p(:), v1(:), v2(:)
  n = 1000
  allocate (p(n), v1(n), v2(n))
  call vec_mult (p, v1, v2, n)
  deallocate (p, v1, v2)
end program
