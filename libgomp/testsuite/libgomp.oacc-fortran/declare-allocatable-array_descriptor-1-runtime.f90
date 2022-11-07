! Test OpenACC 'declare create' with allocatable arrays.

! { dg-do run }

! Note that we're not testing OpenACC semantics here, but rather documenting
! current GCC behavior, specifically, behavior concerning updating of
! host/device array descriptors.
! { dg-skip-if n/a { *-*-* } { -DACC_MEM_SHARED=1 } }

!TODO-OpenACC-declare-allocate
! Missing support for OpenACC "Changes from Version 2.0 to 2.5":
! "The 'declare create' directive with a Fortran 'allocatable' has new behavior".
! Thus, after 'allocate'/before 'deallocate', call 'acc_create'/'acc_delete'
! manually.


!TODO { dg-additional-options -fno-inline } for stable results regarding OpenACC 'routine'.


!TODO OpenACC 'serial' vs. GCC/nvptx:
!TODO { dg-prune-output {using 'vector_length \(32\)', ignoring 1} }


! { dg-additional-options -fdump-tree-original }
! { dg-additional-options -fdump-tree-gimple }


module vars
  implicit none
  integer, parameter :: n1_lb = -3
  integer, parameter :: n1_ub = 6
  integer, parameter :: n2_lb = -9999
  integer, parameter :: n2_ub = 22222

  integer, allocatable :: b(:)
  !$acc declare create (b)

end module vars

program test
  use vars
  use openacc
  implicit none
  integer :: i

  ! Identifiers for purposes of reliable '-fdump-tree-[...]' scanning.
  integer :: id1_1, id1_2

  interface

     subroutine verify_initial
       implicit none
       !$acc routine seq
     end subroutine verify_initial

     subroutine verify_n1_allocated
       implicit none
       !$acc routine seq
     end subroutine verify_n1_allocated

     subroutine verify_n1_values (addend)
       implicit none
       !$acc routine gang
       integer, value :: addend
     end subroutine verify_n1_values

     subroutine verify_n1_deallocated (expect_allocated)
       implicit none
       !$acc routine seq
       logical, value :: expect_allocated
     end subroutine verify_n1_deallocated

     subroutine verify_n2_allocated
       implicit none
       !$acc routine seq
     end subroutine verify_n2_allocated

     subroutine verify_n2_values (addend)
       implicit none
       !$acc routine gang
       integer, value :: addend
     end subroutine verify_n2_values

     subroutine verify_n2_deallocated (expect_allocated)
       implicit none
       !$acc routine seq
       logical, value :: expect_allocated
     end subroutine verify_n2_deallocated

  end interface

  call acc_create (id1_1)
  call acc_create (id1_2)

  call verify_initial
  ! It is important here (and similarly, following) that there is no data
  ! clause for 'b' (explicit or implicit): no 'GOMP_MAP_TO_PSET'.
  !$acc serial
  call verify_initial
  !$acc end serial

  allocate (b(n1_lb:n1_ub))
  call verify_n1_allocated
  if (acc_is_present (b)) error stop
  call acc_create (b)
  ! This is now OpenACC "present":
  if (.not.acc_is_present (b)) error stop
  ! This still has the initial array descriptor:
  !$acc serial
  call verify_initial
  !$acc end serial

  do i = n1_lb, n1_ub
     b(i) = i - 1
  end do

  ! Verify that host-to-device copy doesn't touch the device-side (still
  ! initial) array descriptor (but it does copy the array data).
  call acc_update_device (b)
  !$acc serial
  call verify_initial
  !$acc end serial

  b = 40

  ! Verify that device-to-host copy doesn't touch the host-side array
  ! descriptor, doesn't copy out the device-side (still initial) array
  ! descriptor (but it does copy the array data).
  call acc_update_self (b)
  call verify_n1_allocated

  do i = n1_lb, n1_ub
     if (b(i) /= i - 1) error stop
     b(i) = b(i) + 2
  end do

  ! The same using the OpenACC 'update' directive.

  !$acc update device (b) self (id1_1)
  ! We do have 'GOMP_MAP_TO_PSET' here:
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma acc update map\(force_to:\*\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[pointer assign, bias: 0\]\) map\(force_from:id1_1\);$} 1 original } }
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma omp target oacc_update map\(force_to:MEM <integer\(kind=[0-9]+\)\[0:\]> \[\(integer\(kind=[0-9]+\)\[0:\] \*\)[^\]]+\] \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:b\.data \[pointer assign, bias: 0\]\) map\(force_from:id1_1 \[len: [0-9]+\]\)$} 1 gimple } }
  ! ..., but it's silently skipped in 'GOACC_update'.
  !$acc serial
  call verify_initial
  !$acc end serial

  b = 41

  !$acc update self (b) self (id1_2)
  ! We do have 'GOMP_MAP_TO_PSET' here:
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma acc update map\(force_from:\*\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[pointer assign, bias: 0\]\) map\(force_from:id1_2\);$} 1 original } }
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma omp target oacc_update map\(force_from:MEM <integer\(kind=[0-9]+\)\[0:\]> \[\(integer\(kind=[0-9]+\)\[0:\] \*\)[^\]]+\] \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:b\.data \[pointer assign, bias: 0\]\) map\(force_from:id1_2 \[len: [0-9]+\]\)$} 1 gimple } }
  ! ..., but it's silently skipped in 'GOACC_update'.
  call verify_n1_allocated

  do i = n1_lb, n1_ub
     if (b(i) /= i + 1) error stop
     b(i) = b(i) + 2
  end do

  ! Now install the actual array descriptor, via a data clause for 'b'
  ! (explicit or implicit): must get a 'GOMP_MAP_TO_PSET', which then in
  ! 'gomp_map_vars_internal' is handled as 'declare target', and because of
  ! '*(void **) hostaddrs[i] != NULL', we've got 'has_always_ptrset == true',
  ! 'always_to_cnt == 1', and therefore 'gomp_map_vars_existing' does update
  ! the 'GOMP_MAP_TO_PSET'.
  !$acc serial present (b) copyin (id1_1)
  call verify_initial
  id1_1 = 0
  !$acc end serial
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma acc serial map\(force_present:\*\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[pointer assign, bias: 0\]\) map\(to:id1_1\)$} 1 original } }
  !TODO ..., but without an actual use of 'b', the gimplifier removes the
  !TODO 'GOMP_MAP_TO_PSET':
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma omp target oacc_serial map\(force_present:MEM <integer\(kind=[0-9]+\)\[0:\]> \[\(integer\(kind=[0-9]+\)\[0:\] \*\)[^\]]+\] \[len: [^\]]+\]\) map\(alloc:b\.data \[pointer assign, bias: 0\]\) map\(to:id1_1 \[len: [0-9]+\]\)$} 1 gimple } }
  !$acc serial present (b) copyin (id1_2)
  call verify_n1_allocated
  !TODO Use of 'b':
  id1_2 = ubound (b, 1)
  !$acc end serial
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma acc serial map\(force_present:\*\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[pointer assign, bias: 0\]\) map\(to:id1_2\)$} 1 original } }
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma omp target oacc_serial map\(force_present:MEM <integer\(kind=[0-9]+\)\[0:\]> \[\(integer\(kind=[0-9]+\)\[0:\] \*\)[^\]]+\] \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:b\.data \[pointer assign, bias: 0\]\) map\(to:id1_2 \[len: [0-9]+\]\)$} 1 gimple } }

  !$acc parallel copyin (id1_1) ! No data clause for 'b' (explicit or implicit): no 'GOMP_MAP_TO_PSET'.
  call verify_n1_values (1)
  id1_1 = 0
  !$acc end parallel
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma acc parallel map\(to:id1_1\)$} 1 original } }
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma omp target oacc_parallel map\(to:id1_1 \[len: [0-9]+\]\)$} 1 gimple } }

  !$acc parallel copy (b) copyin (id1_2)
  ! As already present, 'copy (b)' doesn't copy; addend is still '1'.
  call verify_n1_values (1)
  id1_2 = 0
  !$acc end parallel
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma acc parallel map\(tofrom:\*\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[pointer assign, bias: 0\]\) map\(to:id1_2\)$} 1 original } }
  !TODO ..., but without an actual use of 'b', the gimplifier removes the
  !TODO 'GOMP_MAP_TO_PSET':
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma omp target oacc_parallel map\(tofrom:MEM <integer\(kind=[0-9]+\)\[0:\]> \[\(integer\(kind=[0-9]+\)\[0:\] \*\)[^\]]+\] \[len: [^\]]+\]\) map\(alloc:b\.data \[pointer assign, bias: 0\]\) map\(to:id1_2 \[len: [0-9]+\]\)$} 1 gimple } }

  call verify_n1_allocated
  if (.not.acc_is_present (b)) error stop

  call acc_delete (b)
  if (.not.allocated (b)) error stop
  if (acc_is_present (b)) error stop
  ! The device-side array descriptor doesn't get updated, so 'b' still appears
  ! as "allocated":
  !$acc serial
  call verify_n1_allocated
  !$acc end serial

  deallocate (b)
  call verify_n1_deallocated (.false.)
  ! The device-side array descriptor doesn't get updated, so 'b' still appears
  ! as "allocated":
  !$acc serial
  call verify_n1_allocated
  !$acc end serial

  ! Now try to install the actual array descriptor, via a data clause for 'b'
  ! (explicit or implicit): must get a 'GOMP_MAP_TO_PSET', which then in
  ! 'gomp_map_vars_internal' is handled as 'declare target', but because of
  ! '*(void **) hostaddrs[i] == NULL', we've got 'has_always_ptrset == false',
  ! 'always_to_cnt == 0', and therefore 'gomp_map_vars_existing' doesn't update
  ! the 'GOMP_MAP_TO_PSET'.
  ! The device-side array descriptor doesn't get updated, so 'b' still appears
  ! as "allocated":
  !TODO Why does 'present (b)' still work here?
  !$acc serial present (b) copyout (id1_2)
  call verify_n1_deallocated (.true.)
  !TODO Use of 'b'.
  id1_2 = ubound (b, 1)
  !$acc end serial
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma acc serial map\(force_present:\*\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:\(integer\(kind=[0-9]+\)\[0:\] \* restrict\) b\.data \[pointer assign, bias: 0\]\) map\(from:id1_2\)$} 1 original } }
  ! { dg-final { scan-tree-dump-times {(?n)^ *#pragma omp target oacc_serial map\(force_present:MEM <integer\(kind=[0-9]+\)\[0:\]> \[\(integer\(kind=[0-9]+\)\[0:\] \*\)[^\]]+\] \[len: [^\]]+\]\) map\(to:b \[pointer set, len: [0-9]+\]\) map\(alloc:b\.data \[pointer assign, bias: 0\]\) map\(from:id1_2 \[len: [0-9]+\]\)$} 1 gimple } }


  ! Restart the procedure, with different array dimensions.

  allocate (b(n2_lb:n2_ub))
  call verify_n2_allocated
  if (acc_is_present (b)) error stop
  call acc_create (b)
  if (.not.acc_is_present (b)) error stop
  ! This still has the previous (n1) array descriptor:
  !$acc serial
  call verify_n1_deallocated (.true.)
  !$acc end serial

  do i = n2_lb, n2_ub
     b(i) = i + 20
  end do

  call acc_update_device (b)
  !$acc serial
  call verify_n1_deallocated (.true.)
  !$acc end serial

  b = -40

  call acc_update_self (b)
  call verify_n2_allocated

  do i = n2_lb, n2_ub
     if (b(i) /= i + 20) error stop
     b(i) = b(i) - 40
  end do

  !$acc update device (b)
  !$acc serial
  call verify_n1_deallocated (.true.)
  !$acc end serial

  b = -41

  !$acc update self (b)
  call verify_n2_allocated

  do i = n2_lb, n2_ub
     if (b(i) /= i - 20) error stop
     b(i) = b(i) + 10
  end do

  !$acc serial present (b) copy (id1_2)
  call verify_n2_allocated
  !TODO Use of 'b':
  id1_2 = ubound (b, 1)
  !$acc end serial

  !$acc parallel
  call verify_n2_values (-20)
  !$acc end parallel

  !$acc parallel copy (b)
  call verify_n2_values (-20)
  !$acc end parallel

  call verify_n2_allocated
  if (.not.acc_is_present (b)) error stop

  call acc_delete (b)
  if (.not.allocated (b)) error stop
  if (acc_is_present (b)) error stop
  !$acc serial
  call verify_n2_allocated
  !$acc end serial

  deallocate (b)
  call verify_n2_deallocated (.false.)
  !$acc serial
  call verify_n2_allocated
  !$acc end serial

  !$acc serial present (b) copy (id1_2)
  call verify_n2_deallocated (.true.)
  !TODO Use of 'b':
  id1_2 = ubound (b, 1)
  !$acc end serial

end program test


subroutine verify_initial
  use vars
  implicit none
  !$acc routine seq

  if (allocated (b)) error stop "verify_initial allocated"
  if (any (lbound (b) /= [0])) error stop "verify_initial lbound"
  if (any (ubound (b) /= [0])) error stop "verify_initial ubound"
end subroutine verify_initial

subroutine verify_n1_allocated
  use vars
  implicit none
  !$acc routine seq

  if (.not.allocated (b)) error stop "verify_n1_allocated allocated"
  if (any (lbound (b) /= [n1_lb])) error stop "verify_n1_allocated lbound"
  if (any (ubound (b) /= [n1_ub])) error stop "verify_n1_allocated ubound"
end subroutine verify_n1_allocated

subroutine verify_n1_values (addend)
  use vars
  implicit none
  !$acc routine gang
  integer, value :: addend
  integer :: i

  !$acc loop
  do i = n1_lb, n1_ub
     if (b(i) /= i + addend) error stop
  end do
end subroutine verify_n1_values

subroutine verify_n1_deallocated (expect_allocated)
  use vars
  implicit none
  !$acc routine seq
  logical, value :: expect_allocated

  if (allocated(b) .neqv. expect_allocated) error stop "verify_n1_deallocated allocated"
  ! Apparently 'deallocate'ing doesn't unset the bounds.
  if (any (lbound (b) /= [n1_lb])) error stop "verify_n1_deallocated lbound"
  if (any (ubound (b) /= [n1_ub])) error stop "verify_n1_deallocated ubound"
end subroutine verify_n1_deallocated

subroutine verify_n2_allocated
  use vars
  implicit none
  !$acc routine seq

  if (.not.allocated(b)) error stop "verify_n2_allocated allocated"
  if (any (lbound (b) /= [n2_lb])) error stop "verify_n2_allocated lbound"
  if (any (ubound (b) /= [n2_ub])) error stop "verify_n2_allocated ubound"
end subroutine verify_n2_allocated

subroutine verify_n2_values (addend)
  use vars
  implicit none
  !$acc routine gang
  integer, value :: addend
  integer :: i

  !$acc loop
  do i = n2_lb, n2_ub
     if (b(i) /= i + addend) error stop
  end do
end subroutine verify_n2_values

subroutine verify_n2_deallocated (expect_allocated)
  use vars
  implicit none
  !$acc routine seq
  logical, value :: expect_allocated

  if (allocated(b) .neqv. expect_allocated) error stop "verify_n2_deallocated allocated"
  ! Apparently 'deallocate'ing doesn't unset the bounds.
  if (any (lbound (b) /= [n2_lb])) error stop "verify_n2_deallocated lbound"
  if (any (ubound (b) /= [n2_ub])) error stop "verify_n2_deallocated ubound"
end subroutine verify_n2_deallocated
