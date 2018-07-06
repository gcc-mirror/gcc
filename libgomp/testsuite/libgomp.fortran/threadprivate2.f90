! { dg-do run }
! { dg-require-effective-target tls_runtime }

module threadprivate2
  integer, dimension(:,:), allocatable :: foo
!$omp threadprivate (foo)
end module threadprivate2

  use omp_lib
  use threadprivate2

  integer, dimension(:), pointer :: bar1
  integer, dimension(2), target :: bar2
  common /thrc/ bar1, bar2
!$omp threadprivate (/thrc/)

  integer, dimension(:), pointer, save :: bar3 => NULL()
!$omp threadprivate (bar3)

  logical :: l
  type tt
    integer :: a
    integer :: b = 32
  end type tt
  type (tt), save :: baz
!$omp threadprivate (baz)

  l = .false.
  call omp_set_dynamic (.false.)
  call omp_set_num_threads (4)

!$omp parallel num_threads (4) reduction (.or.:l)
  l = allocated (foo)
  allocate (foo (6 + omp_get_thread_num (), 3))
  l = l.or..not.allocated (foo)
  l = l.or.size (foo).ne.(18 + 3 * omp_get_thread_num ())
  foo = omp_get_thread_num () + 1

  bar2 = omp_get_thread_num ()
  l = l.or.associated (bar3)
  bar1 => bar2
  l = l.or..not.associated (bar1)
  l = l.or..not.associated (bar1, bar2)
  l = l.or.any (bar1.ne.omp_get_thread_num ())
  nullify (bar1)
  l = l.or.associated (bar1)
  allocate (bar3 (4))
  l = l.or..not.associated (bar3)
  bar3 = omp_get_thread_num () - 2

  l = l.or.(baz%b.ne.32)
  baz%a = omp_get_thread_num () * 2
  baz%b = omp_get_thread_num () * 2 + 1
!$omp end parallel

  if (l) STOP 1
  if (.not.allocated (foo)) STOP 2
  if (size (foo).ne.18) STOP 3
  if (any (foo.ne.1)) STOP 4

  if (associated (bar1)) STOP 5
  if (.not.associated (bar3)) STOP 6
  if (any (bar3 .ne. -2)) STOP 7
  deallocate (bar3)
  if (associated (bar3)) STOP 8

!$omp parallel num_threads (4) reduction (.or.:l)
  l = l.or..not.allocated (foo)
  l = l.or.size (foo).ne.(18 + 3 * omp_get_thread_num ())
  l = l.or.any (foo.ne.(omp_get_thread_num () + 1))
  if (omp_get_thread_num () .ne. 0) then
    deallocate (foo)
    l = l.or.allocated (foo)
  end if

  l = l.or.associated (bar1)
  if (omp_get_thread_num () .ne. 0) then
    l = l.or..not.associated (bar3)
    l = l.or.any (bar3 .ne. omp_get_thread_num () - 2)
    deallocate (bar3)
  end if
  l = l.or.associated (bar3)

  l = l.or.(baz%a.ne.(omp_get_thread_num () * 2))
  l = l.or.(baz%b.ne.(omp_get_thread_num () * 2 + 1))
!$omp end parallel

  if (l) STOP 9
  if (.not.allocated (foo)) STOP 10
  if (size (foo).ne.18) STOP 11
  if (any (foo.ne.1)) STOP 12
  deallocate (foo)
  if (allocated (foo)) STOP 13
end

! { dg-final { cleanup-modules "threadprivate2" } }
