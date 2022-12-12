! { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } }

implicit none
!$omp requires reverse_offload
integer :: A(50), A2(50), A3(50)
integer :: i
logical :: shared_mem

shared_mem = .false.
A  = [(3*i, i=1,50)]
A2 = [(7*i, i=1,50)]
A3 = [(11*i, i=1,50)]

!$omp target map(to: shared_mem)
  shared_mem = .true.
!$omp end target

!$omp target map(to: A(20:40), A2(20:40), A3(20:40))
block
  integer :: C(10)
  C = 88
  A(20:40)  = -2*A(20:40)
  A2(20:40) = -9*A2(20:40)
  A3(20:40) = -13*A3(20:40)

  !$omp target device (ancestor:1)  &
  !$omp&       map(from: A(25:35)) map(always, from: A2(25:35))  &
  !$omp&       map(alloc: A3(25:35)) map(alloc:C(4:8))
    if (shared_mem) then
      if (any (A(25:35)  /= [(-2*3*i, i=25,35)])) stop 1
      if (any (A2(25:35) /= [(-9*7*i, i=25,35)])) stop 2
      if (any (A3(25:35) /= [(-13*11*i, i=25,35)])) stop 3
    else
      if (any (A(25:35)  /= [(3*i, i=25,35)])) stop 4
      if (any (A2(25:35) /= [(7*i, i=25,35)])) stop 5
      if (any (A3(25:35) /= [(11*i, i=25,35)])) stop 6
    end if

    A(25:35) = A(25:35)*5
    A2(25:35) = A2(25:35)*8
    A3(25:35) = A3(25:35)*18
    C(4:8) = [4,5,6,7,8]
  !$omp end target

  if (shared_mem) then
    if (any (A(25:35)  /= [(-2*3*5*i, i=25,35)])) stop 7
    if (any (A2(25:35) /= [(-9*7*8*i, i=25,35)])) stop 8
    if (any (A3(25:35) /= [(-13*11*18*i, i=25,35)])) stop 9
    if (any (C(4:8) /= [4,5,6,7,8])) stop 10
  else
    if (any (A(25:35)  /= [(-2*3*i, i=25,35)])) stop 11
    if (any (A2(25:35) /= [(7*8*i, i=25,35)])) stop 12
    if (any (A3(25:35) /= [(-13*11*i, i=25,35)])) stop 13
    if (any (C(4:8) /= 88)) stop 14
  end if
end block

if (shared_mem) then
  if (any (A(25:35)  /= [(-2*3*5*i, i=25,35)])) stop
  if (any (A2(25:35) /= [(-9*7**8*i, i=25,35)])) stop
  if (any (A3(25:35) /= [(-13*11*18*i, i=25,35)])) stop
else
  if (any (A(25:35)  /= [(3*5*i, i=25,35)])) stop
  if (any (A2(25:35) /= [(7*8*i, i=25,35)])) stop
  if (any (A3(25:35) /= [(11*18*i, i=25,35)])) stop
end if

end
