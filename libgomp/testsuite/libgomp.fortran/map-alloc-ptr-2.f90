! { dg-do run }
! 
! PR fortran/96668

module m
  implicit none
  integer, pointer :: p1(:) => null(), p3(:) => null()
  integer, allocatable :: a1(:), a2(:)
  !$omp declare target to(a1, a2, p1, p3)
end module m

use m
implicit none
  integer, pointer :: p2(:)

  !$omp target
    if (allocated (a1)) stop 1
    if (allocated (a2)) stop 1
    if (associated (p1)) stop 1
    if (associated (p3)) stop 1
  !$omp end target

  allocate (a1, source=[10,11,12,13,14])
  allocate (a2, source=[10,11,12,13,14])
  allocate (p1, source=[9,8,7,6,5,4])
  allocate (p3, source=[4,5,6])
  p2 => p1

  !$omp target enter data map(to:p3)

  ! allocatable, TR9 requires 'always' modifier:
  !$omp target map(always, tofrom: a1)
    if (.not. allocated(a1)) stop 2
    if (size(a1) /= 5) stop 3
    if (any (a1 /= [10,11,12,13,14])) stop 5
    a1(:) = [101, 102, 103, 104, 105]
  !$omp end target

  ! allocatable, extension (OpenMP 6.0?): without 'always'
  !$omp target
    if (.not. allocated(a2)) stop 2
    if (size(a2) /= 5) stop 3
    if (any (a2 /= [10,11,12,13,14])) stop 5
    a2(:) = [101, 102, 103, 104, 105]
  !$omp end target

  ! pointer: target is automatically mapped
  ! without requiring an explicit mapping or even the always modifier
  !$omp target  !! map(always, tofrom: p1)
    if (.not. associated(p1)) stop 7
    if (size(p1) /= 6) stop 8
    if (any (p1 /= [9,8,7,6,5,4])) stop 10
    p1(:) = [-1, -2, -3, -4, -5, -6]
  !$omp end target

  !$omp target  !! map(always, tofrom: p3)
    if (.not. associated(p3)) stop 7
    if (size(p3) /= 3) stop 8
    if (any (p3 /= [4,5,6])) stop 10
    p3(:) = [23,24,25]
  !$omp end target

  !$omp target update from(p1)
  if (any (p1 /= [-1, -2, -3, -4, -5, -6])) stop 141

  !$omp target exit data map(always, from:p3)
  if (any (p3 /= [23,24,25])) stop 141

  allocate (p1, source=[99,88,77,66,55,44,33])

  !$omp target  ! And this also should work
    if (.not. associated(p1)) stop 7
    if (size(p1) /= 7) stop 8
    if (any (p1 /= [99,88,77,66,55,44,33])) stop 10
    p1(:) = [-11, -22, -33, -44, -55, -66, -77]
  !$omp end target
  !$omp target update from(p1)

  if (any (a1 /= [101, 102, 103, 104, 105])) stop 12
  if (any (a2 /= [101, 102, 103, 104, 105])) stop 12

  if (any (p1 /= [-11, -22, -33, -44, -55, -66, -77])) stop 142
  if (any (p2 /= [-1, -2, -3, -4, -5, -6])) stop 143

  deallocate(a1, a2, p1, p2, p3)
end
