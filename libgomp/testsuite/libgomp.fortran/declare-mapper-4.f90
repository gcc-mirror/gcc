! { dg-do run }

program myprog
type s
  integer :: c
  integer :: d(99)
end type s

type t
  type(s) :: mys
end type t

type u
  type(t) :: myt
end type u

type(u) :: myu

! Here, the mappers are declared out of order, so later ones are not 'seen' by
! earlier ones.  Is that right?
!$omp declare mapper (u :: x) map(tofrom: x%myt)
!$omp declare mapper (t :: x) map(tofrom: x%mys)
!$omp declare mapper (s :: x) map(tofrom: x%c, x%d(1:x%c))

myu%myt%mys%c = 1
myu%myt%mys%d = 0

!$omp target map(tofrom: myu)
myu%myt%mys%d(5) = myu%myt%mys%d(5) + 1
!$omp end target

! Note: we used the default mapper, not the 's' mapper, so we mapped the
! whole array 'd'.
if (myu%myt%mys%d(5).ne.1) stop 1

end program myprog
