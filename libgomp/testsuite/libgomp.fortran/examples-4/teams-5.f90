! { dg-do run }

module e_54_5_mod
contains
  subroutine init (v1, v2, N)
    integer :: i, N
    real, pointer, dimension(:) :: v1, v2
    do i = 1, N
      v1(i) = i + 2.0
      v2(i) = i - 3.0
    end do
  end subroutine

  subroutine check (p, N)
    integer :: i, N
    real, parameter :: EPS = 0.00001
    real, pointer, dimension(:) :: p
    real :: diff
    do i = 1, N
      diff = p(i) - (i + 2.0) * (i - 3.0)
      if (diff > EPS .or. -diff > EPS) stop 1
    end do
  end subroutine

  subroutine vec_mult (p, v1, v2, N)
    real :: p(N), v1(N), v2(N)
    integer :: i, N
    !$omp target teams map(to: v1, v2) map(from: p)
      !$omp distribute simd
      do i = 1, N
        p(i) = v1(i) * v2(i)
      end do
    !$omp end target teams
  end subroutine
end module

program e_54_5
  use e_54_5_mod, only : init, check, vec_mult
  real, pointer, dimension(:) :: p, v1, v2
  integer :: n
  n = 1000
  allocate (p(n), v1(n), v2(n))
  call init (v1, v2, n)
  call vec_mult (p, v1, v2, n)
  call check (p, N)
  deallocate (p, v1, v2)
end program
