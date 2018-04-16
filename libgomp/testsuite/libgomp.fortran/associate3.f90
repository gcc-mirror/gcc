! PR fortran/71717
! { dg-do run }

  type t
    real, allocatable :: f(:)
  end type
  type (t) :: v
  integer :: i, j
  allocate (v%f(4))
  v%f = 19.
  i = 5
  associate (u => v, k => i)
  !$omp parallel do
  do j = 1, 4
    u%f(j) = 21.
    if (j.eq.1) k = 7
  end do
  end associate
  if (any (v%f(:).ne.21.) .or. i.ne.7) STOP 1
end
