! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

! PR fortran/107424

! Nonrectangular loop nests checks
! This testcase uses negative step sizes

module m
  implicit none (type, external)
contains

! The 'k' loop uses i or j as start value
! but a constant end value such that 'lastprivate'
! should be well-defined
subroutine lastprivate_check_simd_1
  integer :: n,m,p, i,j,k, one

  n = 11
  m = 23
  p = 27
  one = 1

  ! Use 'i' or 'j', unit step on 'i' or on 'j' -> 4 loops
  ! Then same, except use non-unit step for 'k'

  !$omp simd collapse(3) lastprivate(k)
  do i = n, one, -1
    do j = m, one, -2
      do k = p + j, p - 41, -1
        if (k < p - 41 .or. k > p+m) error stop
      end do
    end do
  end do
  if (k /= p - 41 - 1) error stop

  !$omp simd collapse(3) lastprivate(k)
  do i = n, 1, -2
    do j = m, 1, -1
      do k = p, i - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop

  !$omp simd collapse(3) lastprivate(k)
  do i = n, one, -2
    do j = m, one, -1
      do k = p, j - 41, -1
        if (k < 1 - 41 .or. k > p) then
          ! print *, i, j, k,p, " -> i, j, k, p   (k < 1 - 41 .or. k > p)"
          error stop
        end if
      end do
    end do
  end do
  if (k /= -41) error stop

  k = -43
  m = 0
  !$omp simd collapse(3) lastprivate(k)
  do i = m, one, -2
    do j = m, one, -1
      do k = p, j - 41, -1
        if (k < 1 - 41 .or. k > p) then
          ! print *, i, j, k,p, " -> i, j, k, p   (k < 1 - 41 .or. k > p)"
          error stop
        end if
      end do
    end do
  end do
  if (k /= -43) error stop

  m = 23

  !$omp simd collapse(3) lastprivate(k)
  do i = n, one, -1
    do j = m, one, -2
      do k = p, i - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop

  n = -5
  k = - 70
  !$omp simd collapse(3) lastprivate(k)
  do i = n, one, -1
    do j = m, one, -2
      do k = p, i - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -70) error stop

  n = 11

  ! Same but 'private' for all (i,j) vars

  !$omp simd collapse(3) lastprivate(k) private(i,j)
  do i = n, one, -1
    do j = m, one, -2
      do k = p, j - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop

  !$omp simd collapse(3) lastprivate(k) private(i,j)
  do i = n, one, -2
    do j = m, one, -1
      do k = p, i - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop

  !$omp simd collapse(3) lastprivate(k) private(i,j)
  do i = n, one, -2
    do j = m, one, -1
      do k = p, j - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop

  !$omp simd collapse(3) lastprivate(k) private(i,j)
  do i = n, one, -1
    do j = m, one, -2
      do k = p, i - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop

  ! Same - but with lastprivate(i,j)

  !$omp simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = n, one, -1
    do j = m, one, -2
      do k = p, j - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop
  if (i /= 0 .or. j /= -1) error stop

  !$omp simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = n, 1, -2
    do j = m, one, -1
      do k = p, i - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop
  if (i /= -1 .or. j /= 0) error stop

  !$omp simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = n, 1, -2
    do j = m, 1, -1
      do k = p, j - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop
  if (i /= -1 .or. j /= 0) error stop

  !$omp simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = n, one, -1
    do j = m, one, -2
      do k = p, i - 41, -1
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -41) error stop
  if (i /= 0 .or. j /= -1) error stop
end subroutine lastprivate_check_simd_1
end module m

program main
  use m
  implicit none (type, external)
  call lastprivate_check_simd_1
end
