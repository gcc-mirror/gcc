! { dg-do run }

  integer, allocatable :: a(:)
  logical :: l
  l = .false.
!$omp parallel firstprivate (a) reduction (.or.:l)
  l = allocated (a)
  allocate (a(10))
  l = l .or. .not. allocated (a)
  a = 10
  if (any (a .ne. 10)) l = .true.
  deallocate (a)
  l = l .or. allocated (a)
!$omp end parallel
  if (l) STOP 1
end
