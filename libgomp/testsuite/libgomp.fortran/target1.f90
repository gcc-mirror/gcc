! { dg-do run }

module target1
contains
  subroutine foo (p, v, w, n)
    double precision, pointer :: p(:), v(:), w(:)
    double precision :: q(n)
    integer :: i, n
    !$omp target if (n > 256) map (to: v(1:n), w(:n)) map (from: p(1:n), q)
    !$omp parallel do simd
      do i = 1, n
        p(i) = v(i) * w(i)
        q(i) = p(i)
      end do
    !$omp end target
    if (any (p /= q)) STOP 1
    do i = 1, n
      if (p(i) /= i * iand (i, 63)) STOP 2
    end do
    !$omp target data if (n > 256) map (to: v(1:n), w) map (from: p, q)
    !$omp target if (n > 256)
      do i = 1, n
        p(i) = 1.0
        q(i) = 2.0
      end do
    !$omp end target
    !$omp target if (n > 256)
      do i = 1, n
        p(i) = p(i) + v(i) * w(i)
        q(i) = q(i) + v(i) * w(i)
      end do
    !$omp end target
    !$omp target if (n > 256)
      !$omp teams distribute parallel do simd linear(i:1)
      do i = 1, n
        p(i) = p(i) + 2.0
        q(i) = q(i) + 3.0
      end do
    !$omp end target
    !$omp end target data
    if (any (p + 2.0 /= q)) STOP 3
  end subroutine
end module target1
  use target1, only : foo
  integer :: n, i
  double precision, pointer :: p(:), v(:), w(:)
  n = 10000
  allocate (p(n), v(n), w(n))
  do i = 1, n
    v(i) = i
    w(i) = iand (i, 63)
  end do
  call foo (p, v, w, n)
  do i = 1, n
    if (p(i) /= i * iand (i, 63) + 3) STOP 4
  end do
  deallocate (p, v, w)
end
