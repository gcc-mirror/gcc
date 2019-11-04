! { dg-do run }
!$ use omp_lib

  integer, allocatable :: a(:, :)
  integer :: b(6, 3)
  integer :: i, j
  logical :: k, l
  b(:, :) = 16
  l = .false.
  if (allocated (a)) stop 1
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
  if (allocated (a).or.l) stop 2
  allocate (a(6, 3))
  a(:, :) = 3
  if (.not.allocated (a)) stop 3
  l = l.or.size(a).ne.18.or.size(a,1).ne.6.or.size(a,2).ne.3
  if (l) stop 4
!$omp parallel private (a, b) reduction (.or.:l)
  l = l.or..not.allocated (a)
  a(3, 2) = 1
  b(3, 2) = 1
!$omp end parallel
  if (l.or..not.allocated (a)) stop 5
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
  if (any (a.ne.3).or.any (b.ne.16).or.l) stop 6
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
  if (any (a.ne.38).or.any (b.ne.36).or.l) stop 7
  deallocate (a)
  if (allocated (a)) stop 8
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
  if (l) stop 9
  do i = 0, 1
    do j = 0, 3
      if (a(i, j) .ne. (5*i + 3*j)) stop 10
    end do
  end do
end
