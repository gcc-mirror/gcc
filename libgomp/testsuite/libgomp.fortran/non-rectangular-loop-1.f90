! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

! PR fortran/107424

! Nonrectangular loop nests checks

module m
  implicit none (type, external)
contains

! The 'k' loop uses i or j as start value
! but a constant end value such that 'lastprivate'
! should be well-defined
subroutine lastprivate_check_simd_1
  integer :: n,m,p, i,j,k

  n = 11
  m = 23
  p = 27

  ! Use 'i' or 'j', unit step on 'i' or on 'j' -> 4 loops
  ! Then same, except use non-unit step for 'k'

  !$omp simd collapse(3) lastprivate(k)
  do i = 1, n
    do j = 1, m, 2
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp simd collapse(3) lastprivate(k)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp simd collapse(3) lastprivate(k)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) then
          ! print *, i, j, k,p, " -> i, j, k, p   (k < 1 - 41 .or. k > p)"
          error stop
        end if
      end do
    end do
  end do
  if (k /= p + 1) error stop

  k = -43
  m = 0
  !$omp simd collapse(3) lastprivate(k)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
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
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  n = -5
  k = - 70
  !$omp simd collapse(3) lastprivate(k)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= -70) error stop

  n = 11

  ! Same but 'private' for all (i,j) vars

  !$omp simd collapse(3) lastprivate(k) private(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp simd collapse(3) lastprivate(k) private(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp simd collapse(3) lastprivate(k) private(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp simd collapse(3) lastprivate(k) private(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  ! Same - but with lastprivate(i,j)

  !$omp simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 1 .or. j /= m + 2) error stop

  !$omp simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 2 .or. j /= m + 1) error stop

  !$omp simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 2 .or. j /= m + 1) error stop

  !$omp simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 1 .or. j /= m + 2) error stop
end subroutine lastprivate_check_simd_1


! Same but with do simd
subroutine lastprivate_check_do_simd_1
  integer :: n,m,p, i,j,k

  n = 11
  m = 23
  p = 27

  ! Use 'i' or 'j', unit step on 'i' or on 'j' -> 4 loops
  ! Then same, except use non-unit step for 'k'

  !$omp parallel do simd collapse(3) lastprivate(k)
  do i = 1, n
    do j = 1, m, 2
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do simd collapse(3) lastprivate(k)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do simd collapse(3) lastprivate(k)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do simd collapse(3) lastprivate(k)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  ! Same but 'private' for all (i,j) vars

  !$omp parallel do simd collapse(3) lastprivate(k) private(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do simd collapse(3) lastprivate(k) private(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do simd collapse(3) lastprivate(k) private(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do simd collapse(3) lastprivate(k) private(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  ! Same - but with lastprivate(i,j)

  !$omp parallel do simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 1 .or. j /= m + 2) error stop

  !$omp parallel do simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 2 .or. j /= m + 1) error stop

  !$omp parallel do simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 2 .or. j /= m + 1) error stop

  !$omp parallel do simd collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 1 .or. j /= m + 2) error stop
end subroutine lastprivate_check_do_simd_1



! Same but with do
subroutine lastprivate_check_do_1
  integer :: n,m,p, i,j,k

  n = 11
  m = 23
  p = 27

  ! Use 'i' or 'j', unit step on 'i' or on 'j' -> 4 loops
  ! Then same, except use non-unit step for 'k'

  !$omp parallel do collapse(3) lastprivate(k)
  do i = 1, n
    do j = 1, m, 2
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do collapse(3) lastprivate(k)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do collapse(3) lastprivate(k)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do collapse(3) lastprivate(k)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  ! Same but 'private' for all (i,j) vars

  !$omp parallel do collapse(3) lastprivate(k) private(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do collapse(3) lastprivate(k) private(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do collapse(3) lastprivate(k) private(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  !$omp parallel do collapse(3) lastprivate(k) private(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop

  ! Same - but with lastprivate(i,j)

  !$omp parallel do collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n
    do j = 1, m, 2
     do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 1 .or. j /= m + 2) error stop

  !$omp parallel do collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 2 .or. j /= m + 1) error stop

  !$omp parallel do collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n, 2
    do j = 1, m
      do k = j - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 2 .or. j /= m + 1) error stop

  !$omp parallel do collapse(3) lastprivate(k) lastprivate(i,j)
  do i = 1, n
    do j = 1, m, 2
      do k = i - 41, p
        if (k < 1 - 41 .or. k > p) error stop
      end do
    end do
  end do
  if (k /= p + 1) error stop
  if (i /= n + 1 .or. j /= m + 2) error stop
end subroutine lastprivate_check_do_1



subroutine lastprivate_check_2
  integer :: n,m,p, i,j,k,ll

  n = 11
  m = 23
  p = 27

  !$omp parallel do simd collapse(3) lastprivate(ll)
  do i = 1, n
    do j = 1, m,2
      do k = 1, j + 41
        do ll = 1, p, 2
          if (k > 23 + 41 .or. k < 1) error stop
        end do
      end do
    end do
  end do
  if (ll /= 29) error stop

  !$omp simd collapse(3) lastprivate(ll)
  do i = 1, n
    do j = 1, m,2
      do k = 1, j + 41
        do ll = 1, p, 2
          if (k > 23 + 41 .or. k < 1) error stop
        end do
      end do
    end do
  end do
  if (ll /= 29) error stop

  !$omp simd collapse(3) lastprivate(k)
  do i = 1, n,2
    do j = 1, m
      do k = 1, i + 41
        if (k > 11 + 41 .or. k < 1) error stop
      end do
    end do
  end do
if (k /= 53) then
  print *, k, 53
  error stop
endif

!$omp simd collapse(3) lastprivate(k)
do i = 1, n,2
  do j = 1, m
    do k = 1, j + 41
      if (k > 23 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 65) then
  print *, k, 65
  error stop
endif


!$omp simd collapse(3) lastprivate(k)
do i = 1, n
  do j = 1, m,2
    do k = 1, i + 41
      if (k > 11 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 53) then
  print *, k, 53
  error stop
endif

! - Same but without 'private':
!$omp simd collapse(3) lastprivate(k)
do i = 1, n
  do j = 1, m,2
    do k = 1, j + 41
      if (k > 23 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 65) then
  print *, k, 65
  error stop
endif


!$omp simd collapse(3) lastprivate(k)
do i = 1, n,2
  do j = 1, m
    do k = 1, i + 41
      if (k > 11 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 53) then
  print *, k, 53
  error stop
endif

!$omp simd collapse(3) lastprivate(k)
do i = 1, n,2
  do j = 1, m
    do k = 1, j + 41
      if (k > 23 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 65) then
  print *, k, 65
  error stop
endif


!$omp simd collapse(3) lastprivate(k)
do i = 1, n
  do j = 1, m,2
    do k = 1, i + 41
      if (k > 11 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 53) then
  print *, k, 53
  error stop
endif

! - all with lastprivate
!$omp simd collapse(3) lastprivate(k) lastprivate(i, j)
do i = 1, n
  do j = 1, m,2
    do k = 1, j + 41
      if (k > 23 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 65) then
  print *, k, 65
  error stop
endif


!$omp simd collapse(3) lastprivate(k) lastprivate(i, j)
do i = 1, n,2
  do j = 1, m
    do k = 1, i + 41
      if (k > 11 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 53) then
  print *, k, 53
  error stop
endif

!$omp simd collapse(3) lastprivate(k) lastprivate(i, j)
do i = 1, n,2
  do j = 1, m
    do k = 1, j + 41
      if (k > 23 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 65) then
  print *, k, 65
  error stop
endif


!$omp simd collapse(3) lastprivate(k) lastprivate(i, j)
do i = 1, n
  do j = 1, m,2
    do k = 1, i + 41
      if (k > 11 + 41 .or. k < 1) error stop
    end do
  end do
end do
if (k /= 53) then
  print *, k, 53
  error stop
endif

end
end module m

program main
  use m
  implicit none (type, external)
  call lastprivate_check_simd_1
  call lastprivate_check_do_simd_1
  call lastprivate_check_do_1
  call lastprivate_check_2
end
