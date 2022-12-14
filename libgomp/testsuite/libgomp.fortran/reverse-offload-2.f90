! { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } }

implicit none
!$omp requires reverse_offload
integer :: A(50), A2(50)
integer :: i, error
logical :: shared_mem

error = 0
shared_mem = .false.
A  = [(i, i=1,50)]
A2 = [(i, i=1,50)]

!$omp target map(to: shared_mem)
  shared_mem = .true.
!$omp end target

!$omp target map(to: A(20:40), A2(20:40)) map(from: error)
block
  integer :: B(10), C(10)
  B = 99
  C = 88
  A(20:40)  = -A(20:40)
  A2(20:40) = -A2(20:40)

  !$omp target device (ancestor:1)  &
  !$omp&       map(to: A(25:35)) map(always, to: A2(25:35))  &
  !$omp&       map(from:B(4:8)) map(tofrom:C(4:8))
     if (shared_mem) then
       if (any (A(25:35)  /= [(-i,i=25,35)])) stop 20
     else
       if (any (A(25:35)  /= [( i,i=25,35)])) stop 21
     end if
    if (any (A2(25:35) /= [(-i,i=25,35)])) stop 22
    if (any (C(4:8) /= 88)) stop 23

    A(25:35) = -A(25:35)*10
    A2(25:35) = -A2(25:35)*10
    B(4:8) = [4,5,6,7,8]
    C(4:8) = [-4,-5,-6,-7,-8]
  !$omp end target

  if (any (B(1:3) /= 99) .or. any (B(9:10) /= 99)) then
    error = 30
  elseif (any (B(4:8) /= [4,5,6,7,8])) then
    error = 31
  elseif (any (C(1:3) /= 88) .or. any (C(9:10) /= 88)) then
    error = 32
  elseif (any (C(4:8) /= [-4,-5,-6,-7,-8])) then
    error = 33
  else
    error = 0
  endif
end block

if (error /= 0) stop error

if (shared_mem) then
  if (any (A(1:19)  /= [( i, i=1,19)])) stop 1
  if (any (A(20:24) /= [(-i, i=20,24)])) stop 2
  if (any (A(36:40) /= [(-i, i=36,40)])) stop 3
  if (any (A(41:50) /= [( i, i=41,50)])) stop 4

  if (any (A(25:35) /= [( 10*i, i=25,35)])) stop 5
else
  if (any (A(1:24)  /= [( i, i=1,24)])) stop 6
  if (any (A(36:50) /= [( i, i=36,50)])) stop 7

  if (any (A(25:35) /= [(-10*i, i=25,35)])) stop 8
end if
if (any (A2(25:35) /= [( 10*i, i=25,35)])) stop 9
end
