! { dg-require-effective-target size32plus }

module m
  implicit none
  integer r, a(1024), b(1024)
contains
subroutine foo (a, b)
  integer, contiguous :: a(:), b(:)
  integer :: i
  !$omp do reduction (inscan, +:r)
  do i = 1, 1024
    r = r + a(i)
    !$omp scan inclusive(r)
    b(i) = r
  end do
end

integer function bar ()
  integer s, i
  s = 0
  !$omp parallel
  !$omp do reduction (inscan, +:s)
  do i = 1, 1024
    s = s + 2 * a(i)
    !$omp scan inclusive(s)
    b(i) = s
  end do
  !$omp end parallel
  bar = s
end

subroutine baz (a, b)
  integer, contiguous :: a(:), b(:)
  integer :: i
  !$omp parallel do reduction (inscan, +:r)
  do i = 1, 1024
    r = r + a(i)
    !$omp scan inclusive(r)
    b(i) = r
  end do
end

integer function qux ()
  integer s, i
  s = 0
  !$omp parallel do reduction (inscan, +:s)
  do i = 1, 1024
    s = s + 2 * a(i)
    !$omp scan inclusive(s)
    b(i) = s
  end do
  qux = s
end
end module m

program main
  use m
  implicit none

  integer s, i
  s = 0
  do i = 1, 1024
    a(i) = i-1
    b(i) = -1
  end do

  !$omp parallel
  call foo (a, b)
  !$omp end parallel
  if (r /= 1024 * 1023 / 2) &
    stop 1
  do i = 1, 1024
    s = s + i-1
    if (b(i) /= s) then
      stop 2
    else
      b(i) = 25
    endif
  end do

  if (bar () /= 1024 * 1023) &
    stop 3
  s = 0
  do i = 1, 1024
    s = s + 2 * (i-1)
    if (b(i) /= s) then
      stop 4
    else
      b(i) = -1
    end if
  end do

  r = 0
  call baz (a, b)
  if (r /= 1024 * 1023 / 2) &
    stop 5
  s = 0
  do i = 1, 1024
    s = s + i-1
    if (b(i) /= s) then
      stop 6
    else
      b(i) = -25
    endif
  end do

  if (qux () /= 1024 * 1023) &
    stop 6
  s = 0
  do i = 1, 1024
    s = s + 2 * (i-1)
    if (b(i) /= s) &
      stop 7
  end do
end program
