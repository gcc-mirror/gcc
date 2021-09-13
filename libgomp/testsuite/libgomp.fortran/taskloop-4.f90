! { dg-do run }
! { dg-options "-O2" }
! { dg-additional-sources taskloop-4-a.f90 }

module m_taskloop4
  implicit none (type, external)
  integer :: v, u(0:63)

contains
integer function test (a, b, c, d, fn, num_tasks, min_iters, max_iters)
  integer, value :: a, b, c, d
  interface
    subroutine fn (n1, n2, n3, n4)
      integer, value :: n1, n2, n3, n4
    end
  end interface
  integer :: num_tasks, min_iters, max_iters
  integer :: i, t

  t = 0
  u = 0
  v = 0
  call fn (a, b, c, d)
  min_iters = 0
  max_iters = 0
  num_tasks = v
  if (v /= 0) then
    min_iters = u(0)
    max_iters = u(0)
    t = u(0)
    do i = 1, v - 1
      if (min_iters > u(i)) &
        min_iters = u(i)
      if (max_iters < u(i)) &
        max_iters = u(i)
      t = t + u(i)
    end do
  end if
  test = t
end
end module
