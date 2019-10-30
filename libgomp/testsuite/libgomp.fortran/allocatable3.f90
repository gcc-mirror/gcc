! { dg-do run }

  integer, allocatable :: a(:)
  integer :: i
  logical :: l
  l = .false.
  if (allocated (a)) stop 1
!$omp parallel private (a) reduction (.or.:l)
  allocate (a (-7:-5))
  l = l.or..not.allocated (a)
  l = l.or.size(a).ne.3.or.size(a,1).ne.3
  a(:) = 0
  !$omp do private (a)
  do i = 1, 7
    a(:) = i
    l = l.or.any (a.ne.i)
  end do
  l = l.or.any (a.ne.0)
  deallocate (a)
!$omp end parallel
  if (l.or.allocated (a)) stop 2
end
