! { dg-additional-options "-fdump-tree-original" }
!
! Check that EXPR_EQ works with __builtin_omp_is_initial_device,
! which returns an 'int' while Fortran uses 'logical(4)'.
!
! Check that 'call ff (omp_is_initial_device)' accesses the library
! function and not the builtin.
!
! { dg-final { scan-tree-dump-times "__builtin_omp_is_initial_device \\(\\)" 14 "original" } } */
! { dg-final { scan-tree-dump "ff \\(omp_is_initial_device\\);" "original" } } */
!
program main
  use omp_lib, only: omp_is_initial_device
  implicit none (type, external)

  logical(1) :: t1
  logical(2) :: f2
  t1 = .true.
  f2 = .false.

  if (omp_is_initial_device () .eqv. .true.) then
  else
    stop 1
  end if
  if (omp_is_initial_device () .neqv. .true.) stop 2
  if (omp_is_initial_device () .eqv. .false.) stop 3
  if (omp_is_initial_device () .neqv. .false.) then
  else
    stop 4
  end if

  if (omp_is_initial_device () .neqv. .true._1) stop 5
  if (omp_is_initial_device () .eqv. .false._1) stop 6
  if (omp_is_initial_device () .neqv. .true._2) stop 7
  if (omp_is_initial_device () .eqv. .false._2) stop 8
  if (omp_is_initial_device () .neqv. .true._4) stop 9
  if (omp_is_initial_device () .eqv. .false._4) stop 10
  if (omp_is_initial_device () .neqv. .true._8) stop 11
  if (omp_is_initial_device () .eqv. .false._8) stop 12

  if (omp_is_initial_device () .neqv. t1) stop 13
  if (omp_is_initial_device () .eqv. f2) stop 14

  call ff (omp_is_initial_device)
contains
  subroutine ff(xx)
    procedure (omp_is_initial_device) :: xx
    if (.not. xx ()) stop 15
  end
end
