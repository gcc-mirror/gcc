! This testcase checks that a mapped allocatable array is considered present
! on a target construct even when it is unallocated.

implicit none

real(kind=8), allocatable :: alloc0(:,:), alloc1(:,:), alloc2(:,:)

! Case 1: allocated and mapped -> present

alloc0 = reshape([1,2,3,4],[2,2])

!$omp target enter data &
!$omp   map(to: alloc0) &
!$omp   map(to: alloc1)

!$omp target map(present, alloc: alloc0)
  if (.not. allocated(alloc0)) stop 1
  if (any (alloc0 /= reshape([1,2,3,4],[2,2]))) stop 2
  alloc0 = alloc0 * 2
!$omp end target

! Case 2: unallocated but mapped -> present

alloc1 = reshape([11,22,33,44],[2,2])

!$omp target map(always, present, to: alloc1)
  if (.not. allocated(alloc1)) stop 3
  if (any (alloc1 /= reshape([11,22,33,44],[2,2]))) stop 4
  alloc1 = alloc1 * 3
!$omp end target

! Case 3: unallocated and not mapped -> not present

alloc2 = reshape([111,222,333,444],[2,2])

print *, "CheCKpOInT"
! { dg-output "CheCKpOInT(\n|\r\n|\r).*" }

! { dg-output "libgomp: present clause: not present on the device \\(addr: 0x\[0-9a-f\]+, size: \[0-9\]+ \\(0x\[0-9a-f\]+\\), dev: \[0-9\]+\\\)" { target offload_device_nonshared_as } }
! { dg-shouldfail "present error triggered" { offload_device_nonshared_as } }
!$omp target map(always, present, to: alloc2)
  if (.not. allocated(alloc2)) stop 5
  if (any (alloc2 /= reshape([111,222,333,444],[2,2]))) stop 6
  alloc2 = alloc2 * 4
!$omp end target

!$omp target exit data &
!$omp   map(from: alloc0) &
!$omp   map(from: alloc1)

end
