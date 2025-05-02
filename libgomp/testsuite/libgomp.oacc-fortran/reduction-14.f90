! { dg-do run }

! record type reductions

program main
  implicit none

  type t1
     integer :: i
     real :: r
  end type t1

  type t2
     real :: r
     integer :: i
     double precision :: d
  end type t2

  double precision, parameter :: e = 0.001
  integer, parameter :: n = 10, ng = 8, nw = 4, vl = 32
  integer :: i
  type(t1) :: v1, a1
  type (t2) :: v2, a2

  v1%i = 0
  v1%r = 0
  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(v1)
  !$acc loop reduction (+:v1)
  do i = 1, n
     v1%i = v1%i + 1
     v1%r = v1%r + 2
  end do
  !$acc end parallel
  a1%i = 0
  a1%r = 0
  do i = 1, n
     a1%i = a1%i + 1
     a1%r = a1%r + 2
  end do
  if (v1%i .ne. a1%i) STOP 1
  if (v1%r .ne. a1%r) STOP 2

  v2%i = 1
  v2%r = 1
  v2%d = 1
  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(v2)
  !$acc loop reduction (*:v2)
  do i = 1, n
     v2%i = v2%i * 2
     v2%r = v2%r * 1.1
     v2%d = v2%d * 1.3
  end do
  !$acc end parallel
  a2%i = 1
  a2%r = 1
  a2%d = 1
  do i = 1, n
     a2%i = a2%i * 2
     a2%r = a2%r * 1.1
     a2%d = a2%d * 1.3
  end do

  if (v2%i .ne. a2%i) STOP 3
  if (v2%r .ne. a2%r) STOP 4
  if (abs (v2%d - a2%d) .ge. e) STOP 5

end program main

