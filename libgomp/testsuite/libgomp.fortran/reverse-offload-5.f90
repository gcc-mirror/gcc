! { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } }
! { dg-xfail-run-if "Copying on-device allocated memory fails with cuMemcpyDtoHAsync error: invalid argument" { offload_device_nvptx } }

! Because of the nvptx fail, a non-device alloc version has been
! created: reverse-offload-5a.f90

implicit none
!$omp requires reverse_offload

integer, allocatable :: A(:), A2(:), s1, s2
integer :: i
logical :: shared_mem

shared_mem = .false.

a = [1,2,3,4]
a2 = [8,7,6,5]
s1 = 532
s2 = 55

!$omp target map(to: shared_mem)
  shared_mem = .true.
!$omp end target

!$omp target map(to: A, A2, s1, s2)
block
  integer, allocatable :: ai(:), ai2(:), ai3(:), si1, si2, si3

  a = a * 2
  a2 = a2 * 3
  s1 = s1 * 4
  s2 = s2 * 5

  ai = [23,35,86,43]
  ai2 = [8,4,7,1]
  si1 = 64
  si2 = 765

  !$omp target device (ancestor:1)  &
  !$omp&       map(to: A, s1, ai, si1) map(always, to: a2, s2)  &
  !$omp&       map(tofrom: ai2, si2, ai3, si3)
    if (shared_mem) then
      if (any (a  /= 2 * [1,2,3,4])) stop 1
      if (s1 /= 4 * 532) stop 2
    else
      if (any (a  /=     [1,2,3,4])) stop 3
      if (s1 /=     532) stop 4
    endif
    if (any (a2 /= 3 * [8,7,6,5])) stop 5
    if (s2 /= 5 * 55) stop 6
    if (any (ai /= [23,35,86,43])) stop 7
    if (any (ai2 /= [8,4,7,1])) stop 8
    if (si1 /= 64) stop 9
    if (si2 /= 765) stop 10
    if (allocated (ai3) .or. allocated(si3)) stop 26

    a = a*3
    a2 = a2*7
    s1 = s1*11
    s2 = s2*5
    ai = ai*13
    ai2 = ai2*21
    si1 = si1*27
    si2 = si2*31
  !$omp end target

  if (shared_mem) then
    if (any (a  /= 3 * 2 * [1,2,3,4])) stop 11
    if (any (a2 /= 7 * 3 * [8,7,6,5])) stop 12
    if (s1 /= 11 * 4 * 532) stop 13
    if (s2 /= 5 * 5 * 55) stop 14
    if (any (ai /= 13 * [23,35,86,43])) stop 15
    if (si1 /= 27 * 64) stop 16
  else
    if (any (a  /= 2 * [1,2,3,4])) stop 17
    if (any (a2 /= 3 * [8,7,6,5])) stop 18
    if (s1 /= 4 * 532) stop 19
    if (s2 /= 5 * 55) stop 20
    if (any (ai /= [23,35,86,43])) stop 22
    if (si1 /= 64) stop 23
  endif
  if (any (ai2 /= 21 * [8,4,7,1])) stop 24
  if (si2 /= 31 * 765) stop 25
  if (allocated (ai3) .or. allocated(si3)) stop 27

  deallocate (ai, ai2, si1, si2)
end block

if (shared_mem) then
  if (any (a  /= 3 * 2 * [1,2,3,4])) stop 30
  if (any (a2 /= 7 * 3 * [8,7,6,5])) stop 31
  if (s1 /= 11 * 4 * 532) stop 32
  if (s2 /= 5 * 5 * 55) stop 33
else
  if (any (a  /= 3 * [1,2,3,4])) stop 34
  if (any (a2 /= 3 * 7 * [8,7,6,5])) stop 35
  if (s1 /= 11 * 532) stop 36
  if (s2 /= 5 * 5 * 55) stop 37
endif

deallocate (a, a2, s1, s2)
end
