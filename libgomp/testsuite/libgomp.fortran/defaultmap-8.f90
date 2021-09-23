! { dg-do run }
! { dg-additional-options "-fdump-tree-gimple" }
!
! PR fortran/92568
!
program main
  implicit none
  integer :: xa1, xa2, xp1, xp2, xat1, xat2, xt1, xt2, xi1, xi2
  allocatable :: xa1, xa2, xat1, xat2
  pointer :: xp1, xp2

  allocate (xa1, xa2, xat1, xat2, xp1, xp2)

  call foo (xa1, xa2, xp1, xp2, xat1, xat2, xt1, xt2, xi1, xi2)
  call foo2 (xa1, xa2, xp1, xp2, xat1, xat2, xt1, xt2, xi1, xi2)
  call foo3 (xa1, xa2, xp1, xp2, xat1, xat2, xt1, xt2, xi1, xi2)
  call bar (xa1, xa2, xp1, xp2, xat1, xat2, xt1, xt2, xi1, xi2)

  deallocate (xa1, xa2, xat1, xat2, xp1, xp2)
contains
! Implicit mapping
subroutine foo (ia1, ia2, ip1, ip2, iat1, iat2, it1, it2, ii1, ii2)
  implicit none
  integer :: ia1, ia2, ia3, ip1, ip2, ip3, iat1, iat2, iat3, it1, it2, it3, ii1, ii2, ii3
  allocatable :: ia1, ia2, ia3, iat1, iat2, iat3
  pointer :: ip1, ip2, ip3
  target :: iat1, iat2, iat3, it1, it2, it3
  optional :: ia1, ip1, iat1, it1, ii1

  allocate(ia3, iat3, ip3)

  ia1 = 2; ia2 = 2; ia3 = 2; ip1 = 2; ip2 = 2; ip3 = 2;
  iat1 = 2; iat2 = 2; iat3 = 2; it1 = 2; it2 = 2; it3 = 2
  ii1 = 2; ii2 = 2; ii3 = 2

  ! Implicitly, scalars are 'firstprivate' except
  ! if target, allocatable, pointer they are always tofrom.
  !$omp target
    if (ia1 /= 2) stop 1
    if (ia2 /= 2) stop 2
    if (ia3 /= 2) stop 3
    if (ip1 /= 2) stop 4
    if (ip2 /= 2) stop 5
    if (ip3 /= 2) stop 6
    if (iat1 /= 2) stop 7
    if (iat2 /= 2) stop 8
    if (iat3 /= 2) stop 9
    if (it1 /= 2) stop 10
    if (it2 /= 2) stop 11
    if (it3 /= 2) stop 12
    if (ii1 /= 2) stop 13
    if (ii2 /= 2) stop 14
    if (ii3 /= 2) stop 15

    ia1 = 1; ia2 = 1; ia3 = 1; ip1 = 1; ip2 = 1; ip3 = 1;
    iat1 = 1; iat2 = 1; iat3 = 1; it1 = 1; it2 = 1; it3 = 1
    ii1 = 1; ii2 = 1; ii3 = 1
  !$omp end target

  ! (target,allocatable,pointer) -> tofrom
  if (ia1 /= 1) stop 16
  if (ia2 /= 1) stop 17
  if (ia3 /= 1) stop 18
  if (ip1 /= 1) stop 19
  if (ip2 /= 1) stop 20
  if (ip3 /= 1) stop 21
  if (iat1 /= 1) stop 22
  if (iat2 /= 1) stop 23
  if (iat3 /= 1) stop 24
  if (it1 /= 1) stop 25
  if (it2 /= 1) stop 26
  if (it3 /= 1) stop 27
  ! non-(target,allocatable,pointer) -> firstprivate
  !if (ii1 /= 2) stop 28  !  FIXME: optional scalar wrongly mapped as tofrom, PR fortran/100991
  if (ii2 /= 2) stop 29
  if (ii3 /= 2) stop 30

  deallocate(ia3, iat3, ip3)
end

! Implicit mapping likewise even though there is defaultmap
subroutine foo2 (ia1, ia2, ip1, ip2, iat1, iat2, it1, it2, ii1, ii2)
  implicit none
  integer :: ia1, ia2, ia3, ip1, ip2, ip3, iat1, iat2, iat3, it1, it2, it3, ii1, ii2, ii3
  allocatable :: ia1, ia2, ia3, iat1, iat2, iat3
  pointer :: ip1, ip2, ip3
  target :: iat1, iat2, iat3, it1, it2, it3
  optional :: ia1, ip1, iat1, it1, ii1

  allocate(ia3, iat3, ip3)

  ia1 = 2; ia2 = 2; ia3 = 2; ip1 = 2; ip2 = 2; ip3 = 2;
  iat1 = 2; iat2 = 2; iat3 = 2; it1 = 2; it2 = 2; it3 = 2
  ii1 = 2; ii2 = 2; ii3 = 2

  ! Implicitly, scalars are 'firstprivate' except
  ! if target, allocatable, pointer they are always tofrom.
  !$omp target defaultmap(default)
    if (ia1 /= 2) stop 31
    if (ia2 /= 2) stop 32
    if (ia3 /= 2) stop 33
    if (ip1 /= 2) stop 34
    if (ip2 /= 2) stop 35
    if (ip3 /= 2) stop 36
    if (iat1 /= 2) stop 37
    if (iat2 /= 2) stop 38
    if (iat3 /= 2) stop 39
    if (it1 /= 2) stop 40
    if (it2 /= 2) stop 41
    if (it3 /= 2) stop 42
    if (ii1 /= 2) stop 43
    if (ii2 /= 2) stop 44
    if (ii3 /= 2) stop 45

    ia1 = 1; ia2 = 1; ia3 = 1; ip1 = 1; ip2 = 1; ip3 = 1;
    iat1 = 1; iat2 = 1; iat3 = 1; it1 = 1; it2 = 1; it3 = 1
    ii1 = 1; ii2 = 1; ii3 = 1
  !$omp end target

  ! (target,allocatable,pointer) -> tofrom
  if (ia1 /= 1) stop 46
  if (ia2 /= 1) stop 47
  if (ia3 /= 1) stop 48
  if (ip1 /= 1) stop 49
  if (ip2 /= 1) stop 50
  if (ip3 /= 1) stop 51
  if (iat1 /= 1) stop 52
  if (iat2 /= 1) stop 53
  if (iat3 /= 1) stop 54
  if (it1 /= 1) stop 55
  if (it2 /= 1) stop 56
  if (it3 /= 1) stop 57
  ! non-(target,allocatable,pointer) -> firstprivate
  !if (ii1 /= 2) stop 58  !  FIXME: optional scalar wrongly mapped as tofrom, PR fortran/100991
  if (ii2 /= 2) stop 59
  if (ii3 /= 2) stop 60

  deallocate(ia3, iat3, ip3)
end

! Implicit mapping likewise even though there is defaultmap
subroutine foo3 (ia1, ia2, ip1, ip2, iat1, iat2, it1, it2, ii1, ii2)
  implicit none
  integer :: ia1, ia2, ia3, ip1, ip2, ip3, iat1, iat2, iat3, it1, it2, it3, ii1, ii2, ii3
  allocatable :: ia1, ia2, ia3, iat1, iat2, iat3
  pointer :: ip1, ip2, ip3
  target :: iat1, iat2, iat3, it1, it2, it3
  optional :: ia1, ip1, iat1, it1, ii1

  allocate(ia3, iat3, ip3)

  ia1 = 2; ia2 = 2; ia3 = 2; ip1 = 2; ip2 = 2; ip3 = 2;
  iat1 = 2; iat2 = 2; iat3 = 2; it1 = 2; it2 = 2; it3 = 2
  ii1 = 2; ii2 = 2; ii3 = 2

  ! Implicitly, scalars are 'firstprivate' except
  ! if target, allocatable, pointer they are always tofrom.
  !$omp target defaultmap(none:aggregate)
    if (ia1 /= 2) stop 61
    if (ia2 /= 2) stop 62
    if (ia3 /= 2) stop 63
    if (ip1 /= 2) stop 64
    if (ip2 /= 2) stop 65
    if (ip3 /= 2) stop 66
    if (iat1 /= 2) stop 67
    if (iat2 /= 2) stop 68
    if (iat3 /= 2) stop 69
    if (it1 /= 2) stop 70
    if (it2 /= 2) stop 71
    if (it3 /= 2) stop 72
    if (ii1 /= 2) stop 73
    if (ii2 /= 2) stop 74
    if (ii3 /= 2) stop 75

    ia1 = 1; ia2 = 1; ia3 = 1; ip1 = 1; ip2 = 1; ip3 = 1;
    iat1 = 1; iat2 = 1; iat3 = 1; it1 = 1; it2 = 1; it3 = 1
    ii1 = 1; ii2 = 1; ii3 = 1
  !$omp end target

  ! (target,allocatable,pointer) -> tofrom
  if (ia1 /= 1) stop 76
  if (ia2 /= 1) stop 77
  if (ia3 /= 1) stop 78
  if (ip1 /= 1) stop 79
  if (ip2 /= 1) stop 80
  if (ip3 /= 1) stop 81
  if (iat1 /= 1) stop 82
  if (iat2 /= 1) stop 83
  if (iat3 /= 1) stop 84
  if (it1 /= 1) stop 85
  if (it2 /= 1) stop 86
  if (it3 /= 1) stop 87
  ! non-(target,allocatable,pointer) -> firstprivate
  !if (ii1 /= 2) stop 88  !  FIXME: optional scalar wrongly mapped as tofrom, PR fortran/100991
  if (ii2 /= 2) stop 89
  if (ii3 /= 2) stop 90

  deallocate(ia3, iat3, ip3)
end

subroutine bar (ea1, ea2, ep1, ep2, eat1, eat2, et1, et2, ei1, ei2)
  implicit none
  integer :: ea1, ea2, ea3, ep1, ep2, ep3, eat1, eat2, eat3, et1, et2, et3, ei1, ei2, ei3
  allocatable :: ea1, ea2, ea3, eat1, eat2, eat3
  pointer :: ep1, ep2, ep3
  target :: eat1, eat2, eat3, et1, et2, et3
  optional :: ea1, ep1, eat1, et1, ei1
  logical :: shared_memory

  allocate(ea3, eat3, ep3)

  ea1 = 2; ea2 = 2; ea3 = 2; ep1 = 2; ep2 = 2; ep3 = 2;
  eat1 = 2; eat2 = 2; eat3 = 2; et1 = 2; et2 = 2; et3 = 2
  ei1 = 2; ei2 = 2; ei3 = 2

  shared_memory = .false.
  !$omp target map(to: shared_memory)
    shared_memory = .true.
  !$omp end target

  ! While here 'scalar' implies nonallocatable/nonpointer and
  ! the target attribute plays no role.
  !$omp target defaultmap(tofrom:scalar) defaultmap(firstprivate:allocatable) &
  !$omp&       defaultmap(none:aggregate) defaultmap(firstprivate:pointer) &
  !$omp&       map(always, to: shared_memory)
    if (shared_memory) then
      ! Due to fortran/90742 this fails when doing non-shared memory offloading
      if (ea1 /= 2) stop 91
      if (ea2 /= 2) stop 92
      if (ea3 /= 2) stop 93
      if (ep1 /= 2) stop 94
      if (ep2 /= 2) stop 95
      if (ep3 /= 2) stop 96
      if (eat1 /= 2) stop 97
      if (eat2 /= 2) stop 98
      if (eat3 /= 2) stop 99
    end if
    if (et1 /= 2) stop 100
    if (et2 /= 2) stop 101
    if (et3 /= 2) stop 102
    if (ei1 /= 2) stop 103
    if (ei2 /= 2) stop 104
    if (ei3 /= 2) stop 105
    ep1 => null(); ep2 => null(); ep3 => null()
    if (shared_memory) then
      ! Due to fortran/90742 this fails when doing non-shared memory offloading
      ea1 = 1; ea2 = 1; ea3 = 1
      eat1 = 1; eat2 = 1; eat3 = 1
    end if
    et1 = 1; et2 = 1; et3 = 1
    ei1 = 1; ei2 = 1; ei3 = 1
  !$omp end target
  ! (allocatable,pointer) -> firstprivate

! FIXME: allocatables not properly privatized, cf. PR fortran/90742

!  if (ea1 /= 2) stop 106
!  if (ea2 /= 2) stop 107
!  if (ea3 /= 2) stop 108
!  if (eat1 /= 2) stop 112
!  if (eat2 /= 2) stop 113
!  if (eat3 /= 2) stop 114
  if (ep1 /= 2) stop 109
  if (ep2 /= 2) stop 110
  if (ep3 /= 2) stop 111
  ! (scalar) -> tofrom
  !if (et1 /= 1) stop 115  !  FIXME: optional scalar wrongly mapped as 'firstprivate', PR fortran/100991
  if (et2 /= 1) stop 116
  if (et3 /= 1) stop 117
  !if (ei1 /= 1) stop 118  !  FIXME: optional scalar wrongly mapped as 'firstprivate', PR fortran/100991
  if (ei2 /= 1) stop 119
  if (ei3 /= 1) stop 120

  deallocate(ea3, eat3, ep3)
end

end

! FIXME/xfail: Optional scalars wrongly classified, PR fortran/100991
! { dg-final { scan-tree-dump-times "firstprivate\\(ii1\\)" 3 "gimple" { xfail *-*-* } } }
! { dg-final { scan-tree-dump-not "firstprivate\\(et1\\)" "gimple" { xfail *-*-* } } }
! { dg-final { scan-tree-dump-not "firstprivate\\(ei1\\)" "gimple" { xfail *-*-* } } }

! { dg-final { scan-tree-dump-times "firstprivate\\(ea1\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(ea2\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(ea3\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(eat1\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(eat2\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(eat3\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(ep1\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(ep2\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "firstprivate\\(ep3\\)" 1 "gimple" } }
