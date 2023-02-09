! { dg-do compile }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

! PR fortran/107424

! Nonrectangular loop nests checks

!$omp simd collapse(2)
do i = 1, 10
  do j = i, 10, 2  ! { dg-message "sorry, unimplemented: non-rectangular loop nest with step other than constant 1 or -1 for 'j'" }
  end do
end do

!$omp do collapse(2) lastprivate(j)  ! { dg-error "lastprivate variable 'j' is private in outer context" }
do i = 1, 10
  do j = i, 10, 2  ! { dg-message "sorry, unimplemented: non-rectangular loop nest with step other than constant 1 or -1 for 'j'" }
  end do
end do
if (i /= 11) stop 1

!$omp simd collapse(2) lastprivate(j)
do i = 1, 10
  do j = i, 10, 2  ! { dg-message "sorry, unimplemented: non-rectangular loop nest with step other than constant 1 or -1 for 'j'" }
  end do
end do
if (i /= 11) stop 1
end
