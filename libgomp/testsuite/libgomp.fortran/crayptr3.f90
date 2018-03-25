! { dg-do run }
! { dg-options "-fcray-pointer" }

  use omp_lib
  integer :: a, b, c, i, p
  logical :: l
  pointer (ip, p)
  a = 1
  b = 2
  c = 3
  l = .false.
  ip = loc (a)

!$omp parallel num_threads (2) reduction (.or.:l) firstprivate (ip)
  l = p .ne. 1
  ip = loc (b)
  if (omp_get_thread_num () .eq. 1) ip = loc (c)
  l = l .or. (p .ne. (2 + omp_get_thread_num ()))
!$omp end parallel

  if (l) STOP 1

  l = .false.
  ip = loc (a)
!$omp parallel do num_threads (2) reduction (.or.:l) &
!$omp & firstprivate (ip) lastprivate (ip)
  do i = 0, 1
    l = l .or. (p .ne. 1)
    ip = loc (b)
    if (i .eq. 1) ip = loc (c)
    l = l .or. (p .ne. (2 + i))
  end do

  if (l) STOP 2
  if (p .ne. 3) STOP 3
end
