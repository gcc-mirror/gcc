! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
subroutine example_criticial ()
  use omp_lib
  implicit none
  integer, parameter :: my_omp_hint = omp_sync_hint_contended
  integer i, a, b

  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH0) hint(my_omp_hint)
      a = a + i;
      !$omp end critical (HASH0)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH1), hint(omp_sync_hint_none)
      a = a + i;
      !$omp end critical (HASH1)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH2) hint(omp_sync_hint_uncontended)
      a = a + i;
      !$omp end critical (HASH2)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH3) hint(omp_sync_hint_contended)
      a = a + i;
      !$omp end critical (HASH3)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH4) hint(omp_sync_hint_speculative)
      a = a + i;
      !$omp end critical (HASH4)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH5) hint(omp_sync_hint_nonspeculative)
      a = a + i;
      !$omp end critical (HASH5)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH6), hint(omp_sync_hint_contended + omp_sync_hint_speculative)
      a = a + i;
      !$omp end critical (HASH6)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical hint(omp_sync_hint_none + omp_sync_hint_none)
      a = a + i;
      !$omp end critical
  end do
end

! { dg-final { scan-tree-dump-times "omp critical \\(hash0\\) hint\\(2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp critical \\(hash1\\) hint\\(0\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp critical \\(hash2\\) hint\\(1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp critical \\(hash3\\) hint\\(2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp critical \\(hash4\\) hint\\(8\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp critical \\(hash5\\) hint\\(4\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp critical \\(hash6\\) hint\\(10\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp critical hint\\(0\\)" 1 "original" } }
