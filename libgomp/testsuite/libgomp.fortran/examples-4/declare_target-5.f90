! { dg-do run { target vect_simd_clones } }
! { dg-options "-O2" }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module e_53_5_mod
  !$omp declare target (N, Q)
  integer, parameter :: N = 10000, M = 1024
  real :: Q(N,N)
contains
  real function Pfun (k, i)
    !$omp declare simd(Pfun) uniform(i) linear(k) notinbranch
    !$omp declare target
    integer, value, intent(in) :: i, k
    Pfun = (Q(k,i) * Q(i,k))
  end function
end module

real function accum () result (tmp)
  use e_53_5_mod
  real :: tmp1
  integer :: i
  tmp = 0.0e0
  !$omp target map(tofrom: tmp)
    !$omp parallel do private(tmp1) reduction(+:tmp)
    do i = 1, N
      tmp1 = 0.0e0
      !$omp simd reduction(+:tmp1)
      do k = 1, M
        tmp1 = tmp1 + Pfun (k, i)
      end do
      tmp = tmp + tmp1
    end do
  !$omp end target
end function

real function accum_ref () result (tmp)
  use e_53_5_mod
  real :: tmp1
  integer :: i
  tmp = 0.0e0
  do i = 1, N
    tmp1 = 0.0e0
    do k = 1, M
      tmp1 = tmp1 + Pfun (k, i)
    end do
    tmp = tmp + tmp1
  end do
end function

subroutine init ()
  use e_53_5_mod
  integer :: i, j
  do i = 1, N
    do j = 1, N
      Q(i,j) = 0.001 * i * j
    end do
  end do
end subroutine

subroutine check (a, b)
  real :: a, b, err
  real, parameter :: EPS = 0.00001
  if (b == 0.0) then
    err = a
  else if (a == 0.0) then
    err = b
  else
    err = (a - b) / b
  end if
  if (err > EPS .or. err < -EPS) STOP 1
end subroutine

program e_53_5
  use e_53_5_mod
  real :: accum, accum_ref, d
  call init ()
  !$omp target update to(Q)
  call check (accum (), accum_ref ())
end program
