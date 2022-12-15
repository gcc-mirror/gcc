! { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } }

implicit none
!$omp requires reverse_offload

type t2
  integer :: a, b, c
end type t2

type t
 integer :: A(5), B(5), C(5)
 integer, pointer :: P(:), P2 !Just some padding
 type(t2) :: tt !Just some padding
end type t

type(t) :: S1, S2
logical :: shared_mem

shared_mem = .false.

!$omp target map(to: shared_mem)
  shared_mem = .true.
!$omp end target

s1%A = [1,2,3,4,5]
s1%B = [10,20,30,40,50]
s1%C = [11,22,33,44,55]
s2%A = 2*s1%A
s2%B = 2*s1%B
s2%C = 2*s1%C

!$omp target &
!$omp&       map(to: s1%b, s1%c) &
!$omp&       map(to: s2%b, s2%c)
block
  type(t) :: si1, si2, si3, si4

  s1%B = -10 * s1%B
  s1%C = -10 * s1%C
  s2%B = -15 * s2%B
  s2%C = -15 * s2%C

  si1%A = -1 * [1,2,3,4,5]
  si1%B = -1 * [10,20,30,40,50]
  si1%C = -1 * [11,22,33,44,55]
  si2%A = -23 * [1,2,3,4,5]
  si2%B = -23 * [10,20,30,40,50]
  si2%C = -23 * [11,22,33,44,55]

  !$omp target device (ancestor:1)  &
  !$omp&       map(to: si1%C, si1%B)  &
  !$omp&       map(tofrom: si2%C, si2%B)  &
  !$omp&       map(always, to: s1%B)  &
  !$omp&       map(        to: s2%B)
    if (any (s1%A /=       [1,2,3,4,5])) stop 1
    if (any (s1%B /= -10 * [10,20,30,40,50])) stop 2
    if (shared_mem) then
      if (any (s1%C /= -10 * [11,22,33,44,55])) stop 4
    else
      if (any (s1%C /=       [11,22,33,44,55])) stop 3
    endif
    if (any (s2%A /=   2 * [1,2,3,4,5])) stop 4
    if (shared_mem) then
      if (any (s2%B /= -15 * 2 * [10,20,30,40,50])) stop 5
      if (any (s2%C /= -15 * 2 * [11,22,33,44,55])) stop 6
    else
      if (any (s2%B /=   2 * [10,20,30,40,50])) stop 7
      if (any (s2%C /=   2 * [11,22,33,44,55])) stop 8
    endif
    if (any (si1%B /= -1 * [10,20,30,40,50])) stop 9
    if (any (si1%C /= -1 * [11,22,33,44,55])) stop 10
    if (any (si2%B /= -23 * [10,20,30,40,50])) stop 10
    if (any (si2%C /= -23 * [11,22,33,44,55])) stop 11

    s1%A = 5 * s1%A
    s1%B = 7 * s1%B
    s1%C = 13 * s1%C
    s2%A = 9 * s2%A
    s2%B = 21 * s2%B
    s2%C = 31 * s2%C
    si1%B = -11 * si1%B
    si1%C = -13 * si1%C
    si2%B = -27 * si2%B
    si2%C = -29 * si2%C
  !$omp end target

  if (shared_mem) then
    if (any (s1%B /= -10 * 7 * [10,20,30,40,50])) stop 20
    if (any (s1%C /= -10 * 13 * [11,22,33,44,55])) stop 21
  else
    if (any (s1%B /= -10 * [10,20,30,40,50])) stop 22
    if (any (s1%C /= -10 * [11,22,33,44,55])) stop 23
  endif
  if (shared_mem) then
    if (any (s2%B /= -15 * 2 * 21 * [10,20,30,40,50])) stop 24
    if (any (s2%C /= -15 * 2 * 31 * [11,22,33,44,55])) stop 25
  else
    if (any (s2%B /= -15 * 2 * [10,20,30,40,50])) stop 26
    if (any (s2%C /= -15 * 2 * [11,22,33,44,55])) stop 27
  endif
  if (any (si1%A /= -1 * [1,2,3,4,5])) stop 28
  if (shared_mem) then
    if (any (si1%B /= -1 * (-11) * [10,20,30,40,50])) stop 29
    if (any (si1%C /= -1 * (-13) * [11,22,33,44,55])) stop 30
  else
    if (any (si1%B /= -1 * [10,20,30,40,50])) stop 31
    if (any (si1%C /= -1 * [11,22,33,44,55])) stop 32
  endif
  if (any (si2%A /= -23 * [1,2,3,4,5])) stop 33
  if (any (si2%B /= -23 * (-27) * [10,20,30,40,50])) stop 34
  if (any (si2%C /= -23 * (-29) * [11,22,33,44,55])) stop 35
end block

if (any (s1%A /=       5 * [1,2,3,4,5])) stop 40
if (any (s1%B /= -10 * 7 * [10,20,30,40,50])) stop 41
if (shared_mem) then
  if (any (s1%C /= -10 * 13 * [11,22,33,44,55])) stop 42
else
  if (any (s1%C /= 13 *    [11,22,33,44,55])) stop 43
endif
if (any (s2%A /=   2 * 9 * [1,2,3,4,5])) stop 44
if (shared_mem) then
  if (any (s2%B /= -15 * 2 * 21 * [10,20,30,40,50])) stop 45
  if (any (s2%C /= -15 * 2 * 31 * [11,22,33,44,55])) stop 46
else
  if (any (s2%B /=   2 * 21 * [10,20,30,40,50])) stop 47
  if (any (s2%C /=   2 * 31 * [11,22,33,44,55])) stop 48
endif
end
