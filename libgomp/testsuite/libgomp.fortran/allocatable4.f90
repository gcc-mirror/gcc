! { dg-do run }

  integer, allocatable :: a(:, :)
  integer :: b(6, 3)
  integer :: i, j
  logical :: k, l
  b(:, :) = 16
  l = .false.
  if (allocated (a)) STOP 1
!$omp task private (a, b) shared (l)
  l = l.or.allocated (a)
  allocate (a(3, 6))
  l = l.or..not.allocated (a)
  l = l.or.size(a).ne.18.or.size(a,1).ne.3.or.size(a,2).ne.6
  a(3, 2) = 1
  b(3, 2) = 1
  deallocate (a)
  l = l.or.allocated (a)
!$omp end task
!$omp taskwait
  if (allocated (a).or.l) STOP 2
  allocate (a(6, 3))
  a(:, :) = 3
  if (.not.allocated (a)) STOP 3
  l = l.or.size(a).ne.18.or.size(a,1).ne.6.or.size(a,2).ne.3
  if (l) STOP 4
!$omp task private (a, b) shared (l)
  l = l.or..not.allocated (a)
  a(3, 2) = 1
  b(3, 2) = 1
!$omp end task
!$omp taskwait
  if (l.or..not.allocated (a)) STOP 5
!$omp task firstprivate (a, b) shared (l)
  l = l.or..not.allocated (a)
  l = l.or.size(a).ne.18.or.size(a,1).ne.6.or.size(a,2).ne.3
  do i = 1, 6
    l = l.or.(a(i, 1).ne.3).or.(a(i, 2).ne.3)
    l = l.or.(a(i, 3).ne.3).or.(b(i, 1).ne.16)
    l = l.or.(b(i, 2).ne.16).or.(b(i, 3).ne.16)
  end do
  a(:, :) = 7
  b(:, :) = 8
!$omp end task
!$omp taskwait
  if (any (a.ne.3).or.any (b.ne.16).or.l) STOP 6
end
