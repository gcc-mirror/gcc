! { dg-additional-options "-fopenmp-allocators" }
module m
  implicit none (type, external)
  type t
    integer, allocatable :: Acomp, Bcomp(:)
  end type t

contains

subroutine intent_out(aa, bb, cc, dd, ee, ff)
  integer, allocatable,intent(out) :: aa, bb(:)
  type(t), intent(out) :: cc, dd(4)
  type(t), allocatable, intent(out) :: ee, ff(:)
end

subroutine q(qa, qb, qc, qd, qe, qf)
  integer, allocatable :: qa, qb(:)
  type(t) :: qc, qd(4)
  type(t), allocatable :: qe, qf(:)
  call intent_out (qa, qb, qc, qd, qe, qf)
end subroutine q

subroutine r
  integer, allocatable :: r1, r2(:)
  type(t) :: r3, r4(4)
  type(t), allocatable :: r5, r6(:)

  call q(r1,r2,r3,r4,r5,r6)

  allocate(r1,r2(3))
  allocate(r5,r6(4))
  allocate(r3%Acomp, r3%Bcomp(2))
  allocate(r4(2)%Acomp, r4(2)%Bcomp(2))
  allocate(r5%Acomp, r5%Bcomp(2))
  allocate(r6(3)%Acomp, r6(3)%Bcomp(2))
  !$omp allocate align(128)
  allocate(r4(3)%Acomp, r4(3)%Bcomp(2), &
           r6(1)%Acomp, r6(1)%Bcomp(2))
  if (mod (loc (r4(3)%Acomp), 128) /= 0) stop 1
  if (mod (loc (r4(3)%Bcomp), 128) /= 0) stop 2
  if (mod (loc (r6(1)%Acomp), 128) /= 0) stop 3
  if (mod (loc (r6(1)%Bcomp), 128) /= 0) stop 3
  call q(r1,r2,r3,r4,r5,r6)

  !$omp allocate align(64)
  allocate(r1,r2(3))
  if (mod (loc (r1), 64) /= 0) stop 1
  if (mod (loc (r2), 64) /= 0) stop 1
  !$omp allocate align(64)
  allocate(r5,r6(4))
  if (mod (loc (r5), 64) /= 0) stop 1
  if (mod (loc (r6), 64) /= 0) stop 1
  !$omp allocate align(64)
  allocate(r3%Acomp, r3%Bcomp(2))
  if (mod (loc (r3%Acomp), 64) /= 0) stop 1
  if (mod (loc (r3%Bcomp), 64) /= 0) stop 1
  !$omp allocate align(64)
  allocate(r4(2)%Acomp, r4(2)%Bcomp(2))
  if (mod (loc (r4(2)%Acomp), 64) /= 0) stop 1
  if (mod (loc (r4(2)%Bcomp), 64) /= 0) stop 1
  !$omp allocate align(64)
  allocate(r5%Acomp, r5%Bcomp(2))
  if (mod (loc (r5%Acomp), 64) /= 0) stop 1
  if (mod (loc (r5%Bcomp), 64) /= 0) stop 1
  !$omp allocate align(64)
  allocate(r6(3)%Acomp, r6(3)%Bcomp(2))
  if (mod (loc (r6(3)%Acomp), 64) /= 0) stop 1
  if (mod (loc (r6(3)%Bcomp), 64) /= 0) stop 1
  !$omp allocate align(128)
  allocate(r4(3)%Acomp, r4(3)%Bcomp(2), &
           r6(1)%Acomp, r6(1)%Bcomp(2))
  if (mod (loc (r4(3)%Acomp), 128) /= 0) stop 1
  if (mod (loc (r4(3)%Bcomp), 128) /= 0) stop 2
  if (mod (loc (r6(1)%Acomp), 128) /= 0) stop 3
  if (mod (loc (r6(1)%Bcomp), 128) /= 0) stop 3
  call q(r1,r2,r3,r4,r5,r6)
end subroutine r
end

subroutine s
  use m, only : t
  implicit none (type, external)
  type(t) :: xx
  integer :: i, iiiiii
  i = 4
  !$omp allocate
  allocate(xx%Acomp, xx%Bcomp(4))
  deallocate(xx%Acomp, xx%Bcomp)

  !$omp allocate
  allocate(xx%Acomp, xx%Bcomp(4))
  xx = t(1, [1,2])
end

program main
  use m, only: r
  implicit none (type, external)
  external s
  call s
  call r
end
