! { dg-do compile }

subroutine example_criticial ()
  use omp_lib
  implicit none
  integer, parameter :: my_omp_hint = omp_sync_hint_contended
  integer i, a, b

  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH0) hint(my_omp_hint)  ! OK
      a = a + i;
      !$omp end critical (HASH0)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH1) hint(omp_sync_hint_none)  ! OK
      a = a + i;
      !$omp end critical (HASH1)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH2) hint(omp_sync_hint_uncontended)  ! OK
      a = a + i;
      !$omp end critical (HASH2)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH3) hint(omp_sync_hint_contended)  ! OK
      a = a + i;
      !$omp end critical (HASH3)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH4) hint(omp_sync_hint_speculative)  ! OK
      a = a + i;
      !$omp end critical (HASH4)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH5) hint(omp_sync_hint_nonspeculative)  ! OK
      a = a + i;
      !$omp end critical (HASH5)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH6) hint(omp_sync_hint_contended + omp_sync_hint_speculative)  ! OK
      a = a + i;
      !$omp end critical (HASH6)
  end do

  !$omp parallel do
  do i = 1, 10
      ! Accepted but invalid: different hint for same name.
      !$omp critical (HASH6) hint(omp_sync_hint_contended + omp_sync_hint_speculative)  ! OK
      a = a + i;
      !$omp end critical (HASH6)
  end do
  !$omp parallel do
  do i = 1, 10
      ! Accepted but invalid: Some random integer expr.
      !$omp critical (HASH) hint(1 + 2)
      a = a + i;
      !$omp end critical (HASH)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH) hint(-3)  ! { dg-error "shall be a valid constant hint expression" }
      a = a + i;
      !$omp end critical (HASH)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical (HASH2) hint(b)  ! { dg-error "shall be a valid constant hint expression" }
      a = a + i;
      !$omp end critical (HASH2)
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical () hint(omp_hint_speculative)  ! { dg-error "Invalid character in name" }
      a = a + i;
!      !$omp end critical
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical hint(omp_sync_hint_none)  ! OK
      a = a + i;
      !$omp end critical
  end do
  !$omp parallel do
  do i = 1, 10
      !$omp critical hint(omp_sync_hint_contended)  ! { dg-error "CRITICAL at .1. with HINT clause requires a NAME, except when omp_sync_hint_none is used" }
      a = a + i;
      !$omp end critical
  end do
end
