! { dg-do run }
! { dg-options "-fcray-pointer" }
! { dg-require-effective-target tls_runtime }

  use omp_lib
  integer :: a, b, c, d, p
  logical :: l
  pointer (ip, p)
  save ip
!$omp threadprivate (ip)
  a = 1
  b = 2
  c = 3
  l = .false.
!$omp parallel num_threads (3) reduction (.or.:l) private (d)
  if (omp_get_thread_num () .eq. 0) then
    ip = loc (a)
  elseif (omp_get_thread_num () .eq. 1) then
    ip = loc (b)
  else
    ip = loc (c)
  end if
  l = p .ne. omp_get_thread_num () + 1
!$omp single
  d = omp_get_thread_num ()
!$omp end single copyprivate (d, ip)
  l = l .or. (p .ne. d + 1)
!$omp end parallel

  if (l) stop 1
end
