! { dg-do run }
! { dg-options "-O2" }
! { dg-additional-sources taskloop-5-a.f90 }

module m_taskloop5
  implicit none (type, external)
  integer :: u(0:63), v, w(0:63)

contains
integer function test (a, b, c, d, fn, num_tasks, min_iters, max_iters, sep)
  integer, value :: a, b, c, d
  interface
    subroutine fn (n1, n2, n3, n4)
      integer, value :: n1, n2, n3, n4
    end
  end interface
  integer :: num_tasks, min_iters, max_iters, sep
  integer :: i, j, t

  t = 0
  u = 0
  v = 0
  call fn (a, b, c, d)
  min_iters = 0
  max_iters = 0
  num_tasks = v
  sep = v
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

    if (min_iters /= max_iters) then
      do i = 0, v - 2
        block
          integer :: min_idx
          min_idx = i
          do j = i + 1, v - 1
            if (w(min_idx) > w(j)) &
              min_idx = j
          end do
          if (min_idx /= i) then
            block
              integer tem
              tem = u(i)
              u(i) = u(min_idx)
              u(min_idx) = tem
              tem = w(i)
              w(i) = w(min_idx)
              w(min_idx) = tem
            end block
          end if
        end block
      end do
      if (u(0) /= max_iters) &
        stop 1
      do i = 1, v - 1
        if (u(i) /= u(i - 1)) then
          if (sep /= v .or. u(i) /= min_iters) &
            stop 2
          sep = i;
        end if
      end do
    end if
  end if
  test = t
end
end module
