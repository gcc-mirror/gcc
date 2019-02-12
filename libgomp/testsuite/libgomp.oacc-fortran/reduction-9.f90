! Test gang reductions on dummy variables.

! { dg-do run }

program main
  implicit none

  integer g, w, v, c

  g = 0
  w = 0
  v = 0
  c = 0

  call reduction (g, w, v, c)

  if (g /= 10) call abort
  if (w /= 10) call abort
  if (v /= 10) call abort
  if (c /= 100) call abort
end program main

subroutine reduction (g, w, v, c)
  implicit none

  integer g, w, v, c, i

  !$acc parallel
  !$acc loop reduction(+:g) gang
  do i = 1, 10
     g = g + 1
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop reduction(+:w) worker
  do i = 1, 10
     w = w + 1
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop reduction(+:v) vector
  do i = 1, 10
     v = v + 1
  end do
  !$acc end parallel

  !$acc parallel loop reduction(+:c) gang worker vector
  do i = 1, 100
     c = c + 1
  end do
  !$acc end parallel loop
end subroutine reduction
