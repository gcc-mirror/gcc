! { dg-do run }

program associate1
  integer :: v, i, j
  real :: a(3, 3)
  v = 15
  a = 4.5
  a(2,1) = 3.5
  i = 2
  j = 1
  associate(u => v, b => a(i, j))
!$omp parallel private(v, a) default(none)
  v = -1
  a = 2.5
  if (v /= -1 .or. u /= 15) call abort
  if (a(2,1) /= 2.5 .or. b /= 3.5) call abort
  associate(u => v, b => a(2, 1))
  if (u /= -1 .or. b /= 2.5) call abort
  end associate
  if (u /= 15 .or. b /= 3.5) call abort
!$omp end parallel
  end associate
end program
