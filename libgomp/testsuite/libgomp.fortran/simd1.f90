! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

  type dt
    integer :: x = 0
  end type
  type (dt) :: t
  integer :: i, j, k, l, r, s, a(30)
  integer, target :: q(30)
  integer, pointer :: p(:)
  !$omp declare reduction (foo : integer : &
  !$omp & omp_out = omp_out + omp_in) initializer (omp_priv = 0)
  !$omp declare reduction (+ : dt : omp_out%x = omp_out%x &
  !$omp & + omp_in%x)
  a(:) = 1
  q(:) = 1
  p => q
  r = 0
  j = 10
  k = 20
  s = 0
  !$omp simd safelen (8) reduction(+:r, t) linear(j, k : 2) &
  !$omp& private (l) aligned(p : 4) reduction(foo:s)
  do i = 1, 30
    l = j + k + a(i) + p(i)
    r = r + l
    j = j + 2
    k = k + 2
    s = s + l
    t%x = t%x + l
  end do
  if (r.ne.2700.or.j.ne.70.or.k.ne.80.or.s.ne.2700) STOP 1
  if (t%x.ne.2700) STOP 2
end
