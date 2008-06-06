! { dg-do run }
!$ use omp_lib

  integer, allocatable :: a(:, :)
  integer :: b(6, 3)
  integer :: i, j
  logical :: k, l
  b(:, :) = 16
  l = .false.
  if (allocated (a)) call abort
!$omp parallel private (a, b) reduction (.or.:l)
  l = l.or.allocated (a)
  allocate (a(3, 6))
  l = l.or..not.allocated (a)
  l = l.or.size(a).ne.18.or.size(a,1).ne.3.or.size(a,2).ne.6
  a(3, 2) = 1
  b(3, 2) = 1
  deallocate (a)
  l = l.or.allocated (a)
!$omp end parallel
  if (allocated (a).or.l) call abort
  allocate (a(6, 3))
  a(:, :) = 3
  if (.not.allocated (a)) call abort
  l = l.or.size(a).ne.18.or.size(a,1).ne.6.or.size(a,2).ne.3
  if (l) call abort
!$omp parallel private (a, b) reduction (.or.:l)
  l = l.or..not.allocated (a)
  a(3, 2) = 1
  b(3, 2) = 1
!$omp end parallel
  if (l.or..not.allocated (a)) call abort
!$omp parallel firstprivate (a, b) reduction (.or.:l)
  l = l.or..not.allocated (a)
  l = l.or.size(a).ne.18.or.size(a,1).ne.6.or.size(a,2).ne.3
  do i = 1, 6
    l = l.or.(a(i, 1).ne.3).or.(a(i, 2).ne.3)
    l = l.or.(a(i, 3).ne.3).or.(b(i, 1).ne.16)
    l = l.or.(b(i, 2).ne.16).or.(b(i, 3).ne.16)
  end do
  a(:, :) = omp_get_thread_num ()
  b(:, :) = omp_get_thread_num ()
!$omp end parallel
  if (any (a.ne.3).or.any (b.ne.16).or.l) call abort
  k = .true.
!$omp parallel do firstprivate (a, b, k) lastprivate (a, b) &
!$omp & reduction (.or.:l)
  do i = 1, 36
    l = l.or..not.allocated (a)
    l = l.or.size(a).ne.18.or.size(a,1).ne.6.or.size(a,2).ne.3
    if (k) then
      do j = 1, 6
        l = l.or.(a(j, 1).ne.3).or.(a(j, 2).ne.3)
        l = l.or.(a(j, 3).ne.3).or.(b(j, 1).ne.16)
	l = l.or.(b(j, 2).ne.16).or.(b(j, 3).ne.16)
      end do
      k = .false.
    end if
    a(:, :) = i + 2
    b(:, :) = i
  end do
  if (any (a.ne.38).or.any (b.ne.36).or.l) call abort
  deallocate (a)
  if (allocated (a)) call abort
  allocate (a (0:1, 0:3))
  a(:, :) = 0
!$omp parallel do reduction (+:a) reduction (.or.:l) &
!$omp & num_threads(3) schedule(static)
  do i = 0, 7
    l = l.or..not.allocated (a)
    l = l.or.size(a).ne.8.or.size(a,1).ne.2.or.size(a,2).ne.4
    a(modulo (i, 2), i / 2) = a(modulo (i, 2), i / 2) + i
    a(i / 4, modulo (i, 4)) = a(i / 4, modulo (i, 4)) + i
  end do
  if (l) call abort
  do i = 0, 1
    do j = 0, 3
      if (a(i, j) .ne. (5*i + 3*j)) call abort
    end do
  end do
end
