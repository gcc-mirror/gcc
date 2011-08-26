! { dg-do run }
! { dg-require-effective-target tls_runtime }

module threadprivate4
  integer :: vi
  procedure(), pointer :: foo
!$omp threadprivate (foo, vi)

contains
  subroutine fn0
    vi = 0
  end subroutine fn0
  subroutine fn1
    vi = 1
  end subroutine fn1
  subroutine fn2
    vi = 2
  end subroutine fn2
  subroutine fn3
    vi = 3
  end subroutine fn3
end module threadprivate4

  use omp_lib
  use threadprivate4

  integer :: i
  logical :: l

  procedure(), pointer :: bar1
  common /thrc/ bar1
!$omp threadprivate (/thrc/)

  procedure(), pointer, save :: bar2
!$omp threadprivate (bar2)

  l = .false.
  call omp_set_dynamic (.false.)
  call omp_set_num_threads (4)

!$omp parallel num_threads (4) reduction (.or.:l) private (i)
  i = omp_get_thread_num ()
  if (i.eq.0) then
    foo => fn0
    bar1 => fn0
    bar2 => fn0
  elseif (i.eq.1) then
    foo => fn1
    bar1 => fn1
    bar2 => fn1
  elseif (i.eq.2) then
    foo => fn2
    bar1 => fn2
    bar2 => fn2
  else
    foo => fn3
    bar1 => fn3
    bar2 => fn3
  end if
  vi = -1
!$omp barrier
  vi = -1
  call foo ()
  l=l.or.(vi.ne.i)
  vi = -2
  call bar1 ()
  l=l.or.(vi.ne.i)
  vi = -3
  call bar2 ()
  l=l.or.(vi.ne.i)
  vi = -1
!$omp end parallel

  if (l) call abort

end

! { dg-final { cleanup-modules "threadprivate4" } }
