! Based on libgomp.c/target-23.c

! { dg-additional-options "-fdump-tree-original" }
! { dg-final { scan-tree-dump "omp target update to\\(xxs\\\[3\\\] \\\[len: 2\\\]\\)" "original" } }
! { dg-final { scan-tree-dump "omp target update to\\(s\\.s \\\[len: 4\\\]\\)" "original" } }
! { dg-final { scan-tree-dump "omp target update from\\(s\\.s \\\[len: 4\\\]\\)" "original" } }

module m
  implicit none
  type S_type
    integer s
    integer, pointer :: u(:) => null()
    integer :: v(0:4)
  end type S_type
  integer, volatile :: z
end module m

program main
  use m
  implicit none
  integer, target :: u(0:9) = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  logical :: err
  type (S_type) :: s
  integer, pointer :: v(:)
  integer(kind=2) :: xxs(5)
  err = .false.
  s = S_type(9, v=[10, 11, 12, 13, 14])
  s%u(0:) => u(3:)
  v(-4+3:) => u(3:)
  xxs = [-1,-2,-3,-4,-5]
  !$omp target enter data map (to: s%s, s%u, s%u(0:5)) map (alloc: s%v(1:4), xxs(3:5))
  s%s = s%s + 1
  u(3) = u(3) + 1
  s%v(1) = s%v(1) + 1
  xxs(3) = -33
  xxs(4) = -44
  xxs(5) = -55
  !$omp target update to (xxs(4))
  !$omp target update to (s%s) to (s%u(0:2), s%v(1:4))

  !$omp target map (alloc: s%s, s%v(1:4)) map (from: err)
    err = .false.
    if (s%s /= 10 .or. s%v(1) /= 12 .or. s%v(2) /= 12 .or. s%v(3) /= 13) &
      err = .true.
    if (v(-1) /= 4 .or. v(0) /= 4 .or. v(1) /= 5 .or. v(2) /= 6 .or. v(3) /= 7) &
      err = .true.
    if (xxs(4) /= -44) &
      err = .true.
    s%s = s%s + 1
    s%v(2) = s%v(2) + 2
    v(-1) = 5
    v(3) = 9
  !$omp end target

  if (err) &
    error stop

  !$omp target map (alloc: s%u(0:5))
    err = .false.
    if (s%u(0) /= 5 .or. s%u(1) /= 4 .or. s%u(2) /= 5 .or. s%u(3) /= 6 .or. s%u(4) /= 9) &
      err = .true.
    s%u(1) = 12
  !$omp end target

  !$omp target update from (s%s, s%u(0:5)) from (s%v(1:4))
  if (err .or. s%s /= 11 .or. u(0) /= 0 .or. u(1) /= 1 .or. u(2) /= 2 .or. u(3) /= 5 &
      .or. u(4) /= 12 .or. u(5) /= 5 .or. u(6) /= 6 .or. u(7) /= 9 .or. u(8) /= 8    &
      .or. u(9) /= 9 .or. s%v(0) /= 10 .or. s%v(1) /= 12 .or. s%v(2) /= 14           &
      .or. s%v(3) /= 13 .or. s%v(4) /= 14)                                           &
    error stop
  ! !$omp target exit data map (release: s%s)
  ! !$omp target exit data map (release: s%u(0:5))
  ! !$omp target exit data map (delete: s%v(1:4))
  ! !$omp target exit data map (release: s%s)
end
