! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

  integer :: i, j, k, l, r, a(30)
  integer, target :: q(30)
  integer, pointer :: p(:)
  a(:) = 1
  q(:) = 1
  p => q
  r = 0
  j = 10
  k = 20
  !$omp simd safelen (8) reduction(+:r) linear(j, k : 2) &
  !$omp& private (l) aligned(p : 4)
  do i = 1, 30
    l = j + k + a(i) + p(i)
    r = r + l
    j = j + 2
    k = k + 2
  end do
  if (r.ne.2700.or.j.ne.70.or.k.ne.80) call abort
end
