! { dg-do run }
! { dg-additional-options "-O3 -ftree-parallelize-loops=2 -fno-signed-zeros -fno-trapping-math" }

complex function cdcdot(n, cx)
  implicit none

  integer :: n, i, kx
  complex :: cx(*)
  double precision :: dsdotr, dsdoti, dt1, dt3

  kx = 1
  dsdotr = 0
  dsdoti = 0
  do i = 1, n
     dt1 = real(cx(kx))
     dt3 = aimag(cx(kx))
     dsdotr = dsdotr + dt1 * 2 - dt3 * 2
     dsdoti = dsdoti + dt1 * 2 + dt3 * 2
     kx = kx + 1
  end do
  cdcdot = cmplx(real(dsdotr), real(dsdoti))
  return
end function cdcdot
program test
  implicit none
  complex :: cx(100), ct, cdcdot
  integer :: i
  do i = 1, 100
    cx(i) = cmplx(2*i, i)
  end do
  ct = cdcdot (100, cx)
  if (ct.ne.cmplx(10100.0000,30300.0000)) call abort
end
