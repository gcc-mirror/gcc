! { dg-additional-options "-fdump-tree-original" }
! PR fortran/107424

! Same as non-rectangular-loop-4.f90 but expr in upper bound 

module m
contains
subroutine foo (av, avo, a0, a0o, a1, a2, a3, a4)
implicit none

integer, value :: av
integer, value, optional :: avo
integer :: a0
integer, optional :: a0o
integer, pointer :: a1
integer, pointer, optional :: a2
integer, allocatable :: a3
integer, allocatable, optional :: a4
integer :: a5
integer, pointer :: a6
integer, allocatable :: a7
integer :: arr(20,10), ref(20,10)

integer :: j, i, lp_i, lp_j

allocate(a6, a7)

ref = 44
do i = 1, 10
  do j = 1, i*2-1
    ref(j, i) = j + 100 * i
  end do
end do
lp_i = i; lp_j = j

! { dg-final { scan-tree-dump-times "for \\(av = 1; av <= 10; av = av \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= av \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! -> no temp var
arr = 44
av = 99; j = 99
!$omp simd collapse(2) lastprivate(av,j)
do av = 1, 10
  do j = 1, av*2-1
    arr(j, av) = j + 100 * av
  end do
end do
if (any (ref /= arr)) error stop
if (av /= lp_i .or. j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(avo = 1; avo <= 10; avo = avo \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= avo \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! -> no temp var
arr = 44
avo = 99; j = 99
!$omp simd collapse(2) lastprivate(avo, j)
do avo = 1, 10
  do j = 1, avo*2-1
    arr(j, avo) = j + 100 * avo
  end do
end do
if (any (ref /= arr)) error stop
if (avo /= lp_i .or. j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a0\\.\[0-9\]+ = 1; a0\\.\[0-9\]+ <= 10; a0\\.\[0-9\]+ = a0\\.\[0-9\]+ \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a0\\.\[0-9\]+ \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*a0 = a0\\.\[0-9\]+;" 1 "original" } }
arr = 44
a0 = 99; j = 99
!$omp simd collapse(2) lastprivate(a0,j)
do a0 = 1, 10
  do j = 1, a0*2-1
    arr(j, a0) = j + 100 * a0
  end do
end do
if (any (ref /= arr)) error stop
if (a0 /= lp_i .or. j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a0o\\.\[0-9\]+ = 1; a0o\\.\[0-9\]+ <= 10; a0o\\.\[0-9\]+ = a0o\\.\[0-9\]+ \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a0o\\.\[0-9\]+ \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*a0o = a0o\\.\[0-9\]+;" 1 "original" } }
arr = 44
a0o = 99; j = 99
!$omp simd collapse(2) lastprivate(a0o,j)
do a0o = 1, 10
  do j = 1, a0o*2-1
    arr(j, a0o) = j + 100 * a0o
  end do
end do
if (any (ref /= arr)) error stop
if (a0o /= lp_i .or. j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a1\\.\[0-9\]+ = 1; a1\\.\[0-9\]+ <= 10; a1\\.\[0-9\]+ = a1\\.\[0-9\]+ \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a1\\.\[0-9\]+ \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*\\*a1 = a1\\.\[0-9\]+;" 1 "original" } }
arr = 44
a1 = 99; j = 99
! no last private for 'a1' as "The initial status of a private pointer is undefined."
!$omp simd collapse(2) lastprivate(j)
do a1 = 1, 10
  do j = 1, a1*2-1
    arr(j, a1) = j + 100 * a1
  end do
end do
if (any (ref /= arr)) error stop
if (j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a2\\.\[0-9\]+ = 1; a2\\.\[0-9\]+ <= 10; a2\\.\[0-9\]+ = a2\\.\[0-9\]+ \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a2\\.\[0-9\]+ \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*\\*a2 = a2\\.\[0-9\]+;" 1 "original" } }
arr = 44
a2 = 99; j = 99
! no last private for 'a2' as "The initial status of a private pointer is undefined."
!$omp simd collapse(2) lastprivate(j)
do a2 = 1, 10
  do j = 1, a2*2-1
    arr(j, a2) = j + 100 * a2
  end do
end do
if (any (ref /= arr)) error stop
if (j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a3\\.\[0-9\]+ = 1; a3\\.\[0-9\]+ <= 10; a3\\.\[0-9\]+ = a3\\.\[0-9\]+ \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a3\\.\[0-9\]+ \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*\\*a3 = a3\\.\[0-9\]+;" 1 "original" } }
arr = 44
a3 = 99; j = 99
!$omp simd collapse(2) lastprivate(a3,j)
do a3 = 1, 10
  do j = 1, a3*2-1
    arr(j, a3) = j + 100 * a3
  end do
end do
if (any (ref /= arr)) error stop
if (a3 /= lp_i .or. j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a4\\.\[0-9\]+ = 1; a4\\.\[0-9\]+ <= 10; a4\\.\[0-9\]+ = a4\\.\[0-9\]+ \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a4\\.\[0-9\]+ \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*\\*a4 = a4\\.\[0-9\]+;" 1 "original" } }
arr = 44
a4 = 99; j = 99
!$omp simd collapse(2) lastprivate(a4,j)
do a4 = 1, 10
  do j = 1, a4*2-1
    arr(j, a4) = j + 100 * a4
  end do
end do
if (any (ref /= arr)) error stop
if (a4 /= lp_i .or. j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a5 = 1; a5 <= 10; a5 = a5 \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a5 \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! -> no temp var
arr = 44
a5 = 99; j = 99
!$omp simd collapse(2) lastprivate(a5,j)
do a5 = 1, 10
  do j = 1, a5*2-1
    arr(j, a5) = j + 100 * a5
  end do
end do
if (any (ref /= arr)) error stop
if (a5 /= lp_i .or. j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a6\\.\[0-9\]+ = 1; a6\\.\[0-9\]+ <= 10; a6\\.\[0-9\]+ = a6\\.\[0-9\]+ \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a6\\.\[0-9\]+ \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*a6 = a6\\.\[0-9\]+;" 1 "original" } }
arr = 44
a6 = 99; j = 99
! no last private for 'a6' as "The initial status of a private pointer is undefined."
!$omp simd collapse(2) lastprivate(j)
do a6 = 1, 10
  do j = 1, a6*2-1
    arr(j, a6) = j + 100 * a6
  end do
end do
if (any (ref /= arr)) error stop
if (j /= lp_j) error stop

! { dg-final { scan-tree-dump-times "for \\(a7\\.\[0-9\]+ = 1; a7\\.\[0-9\]+ <= 10; a7\\.\[0-9\]+ = a7\\.\[0-9\]+ \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "for \\(j = 1; j <= a7\\.\[0-9\]+ \\* 2 \\+ -1; j = j \\+ 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*a7 = a7\\.\[0-9\]+;" 1 "original" } }
arr = 44
a7 = 99; j = 99
!$omp simd collapse(2) lastprivate(a7,j)
do a7 = 1, 10
  do j = 1, a7*2-1
    arr(j, a7) = j + 100 * a7
  end do
end do
if (any (ref /= arr)) error stop
if (a7 /= lp_i .or. j /= lp_j) error stop

deallocate(a6, a7)
end

end module m


use m
implicit none

integer :: av
integer :: avo
integer :: a0
integer :: a0o
integer, pointer :: a1
integer, pointer :: a2
integer, allocatable :: a3
integer, allocatable :: a4

av = -99; avo = -99
allocate(a1,a2,a3,a4)
call foo (av, avo, a0, a0o, a1, a2, a3, a4)
deallocate(a1,a2,a3,a4)
end
