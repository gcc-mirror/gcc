! { dg-do run }

module target6
contains
  subroutine foo (p, v, w, n)
    double precision, pointer :: p(:), v(:), w(:)
    double precision :: q(n)
    integer :: i, n
    !$omp target data if (n > 256) map (to: v(1:n), w(:n)) map (from: p(1:n), q)
    !$omp target if (n > 256)
    !$omp parallel do simd
      do i = 1, n
        p(i) = v(i) * w(i)
        q(i) = p(i)
      end do
    !$omp end target
    !$omp target update if (n > 256) from (p)
    do i = 1, n
      if (p(i) /= i * iand (i, 63)) call abort
      v(i) = v(i) + 1
    end do
    !$omp target update if (n > 256) to (v(1:n))
    !$omp target if (n > 256)
    !$omp parallel do simd
      do i = 1, n
        p(i) = v(i) * w(i)
      end do
    !$omp end target
    !$omp end target data
    do i = 1, n
      if (q(i) /= (v(i) - 1) * w(i)) call abort
      if (p(i) /= q(i) + w(i)) call abort
    end do
  end subroutine
end module target6
  use target6, only : foo
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
    if (p(i) /= (i + 1) * iand (i, 63)) call abort
  end do
  deallocate (p, v, w)
end
