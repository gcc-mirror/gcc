! { dg-do run }

program myprog
type u
  integer, dimension (:), pointer :: tarr1
  integer, dimension (:), pointer :: tarr2
  integer, dimension (:), pointer :: tarr3
end type u

type(u) :: myu1, myu2, myu3

integer, dimension (12), target :: myarray1
integer, dimension (12), target :: myarray2
integer, dimension (12), target :: myarray3
integer, dimension (12), target :: myarray4
integer, dimension (12), target :: myarray5
integer, dimension (12), target :: myarray6
integer, dimension (12), target :: myarray7
integer, dimension (12), target :: myarray8
integer, dimension (12), target :: myarray9

myu1%tarr1 => myarray1
myu1%tarr2 => myarray2
myu1%tarr3 => myarray3
myu2%tarr1 => myarray4
myu2%tarr2 => myarray5
myu2%tarr3 => myarray6
myu3%tarr1 => myarray7
myu3%tarr2 => myarray8
myu3%tarr3 => myarray9

myu1%tarr1 = 0
myu1%tarr2 = 0
myu1%tarr3 = 0
myu2%tarr1 = 0
myu2%tarr2 = 0
myu2%tarr3 = 0
myu3%tarr1 = 0
myu3%tarr2 = 0
myu3%tarr3 = 0

!$omp target map(to:myu1%tarr1) map(tofrom:myu1%tarr1(:)) &
!$omp&       map(to:myu1%tarr2) map(tofrom:myu1%tarr2(:)) &
!$omp&       map(to:myu1%tarr3) map(tofrom:myu1%tarr3(:)) &
!$omp&       map(to:myu2%tarr1) map(tofrom:myu2%tarr1(:)) &
!$omp&       map(to:myu2%tarr2) map(tofrom:myu2%tarr2(:)) &
!$omp&       map(to:myu2%tarr3) map(tofrom:myu2%tarr3(:)) &
!$omp&       map(to:myu3%tarr1) map(tofrom:myu3%tarr1(:)) &
!$omp&       map(to:myu3%tarr2) map(tofrom:myu3%tarr2(:)) &
!$omp&       map(to:myu3%tarr3) map(tofrom:myu3%tarr3(:))
myu1%tarr1(1) = myu1%tarr1(1) + 1
myu2%tarr1(1) = myu2%tarr1(1) + 1
myu3%tarr1(1) = myu3%tarr1(1) + 1
!$omp end target

!$omp target map(to:myu1%tarr1) map(tofrom:myu1%tarr1(1:2)) &
!$omp&       map(to:myu1%tarr2) map(tofrom:myu1%tarr2(1:2)) &
!$omp&       map(to:myu1%tarr3) map(tofrom:myu1%tarr3(1:2)) &
!$omp&       map(to:myu2%tarr1) map(tofrom:myu2%tarr1(1:2)) &
!$omp&       map(to:myu2%tarr2) map(tofrom:myu2%tarr2(1:2)) &
!$omp&       map(to:myu2%tarr3) map(tofrom:myu2%tarr3(1:2)) &
!$omp&       map(to:myu3%tarr1) map(tofrom:myu3%tarr1(1:2)) &
!$omp&       map(to:myu3%tarr2) map(tofrom:myu3%tarr2(1:2)) &
!$omp&       map(to:myu3%tarr3) map(tofrom:myu3%tarr3(1:2))
myu1%tarr2(1) = myu1%tarr2(1) + 1
myu2%tarr2(1) = myu2%tarr2(1) + 1
myu3%tarr2(1) = myu3%tarr2(1) + 1
!$omp end target

!$omp target map(to:myu1%tarr1) map(tofrom:myu1%tarr1(1)) &
!$omp&       map(to:myu1%tarr2) map(tofrom:myu1%tarr2(1)) &
!$omp&       map(to:myu1%tarr3) map(tofrom:myu1%tarr3(1)) &
!$omp&       map(to:myu2%tarr1) map(tofrom:myu2%tarr1(1)) &
!$omp&       map(to:myu2%tarr2) map(tofrom:myu2%tarr2(1)) &
!$omp&       map(to:myu2%tarr3) map(tofrom:myu2%tarr3(1)) &
!$omp&       map(to:myu3%tarr1) map(tofrom:myu3%tarr1(1)) &
!$omp&       map(to:myu3%tarr2) map(tofrom:myu3%tarr2(1)) &
!$omp&       map(to:myu3%tarr3) map(tofrom:myu3%tarr3(1))
myu1%tarr3(1) = myu1%tarr3(1) + 1
myu2%tarr3(1) = myu2%tarr3(1) + 1
myu3%tarr3(1) = myu3%tarr3(1) + 1
!$omp end target

!$omp target map(tofrom:myu1%tarr1) &
!$omp&       map(tofrom:myu1%tarr2) &
!$omp&       map(tofrom:myu1%tarr3) &
!$omp&       map(tofrom:myu2%tarr1) &
!$omp&       map(tofrom:myu2%tarr2) &
!$omp&       map(tofrom:myu2%tarr3) &
!$omp&       map(tofrom:myu3%tarr1) &
!$omp&       map(tofrom:myu3%tarr2) &
!$omp&       map(tofrom:myu3%tarr3)
myu1%tarr2(1) = myu1%tarr2(1) + 1
myu2%tarr2(1) = myu2%tarr2(1) + 1
myu3%tarr2(1) = myu3%tarr2(1) + 1
!$omp end target

if (myu1%tarr1(1).ne.1) stop 1
if (myu2%tarr1(1).ne.1) stop 2
if (myu3%tarr1(1).ne.1) stop 3
if (myu1%tarr2(1).ne.2) stop 4
if (myu2%tarr2(1).ne.2) stop 5
if (myu3%tarr2(1).ne.2) stop 6
if (myu1%tarr3(1).ne.1) stop 7
if (myu2%tarr3(1).ne.1) stop 8
if (myu3%tarr3(1).ne.1) stop 9

end program myprog
