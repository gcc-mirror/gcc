! { dg-additional-options "-fdump-tree-gimple" }
!
! { dg-additional-sources my-usleep.c }
! { dg-additional-options -Wno-complain-wrong-lang }
!
! Ensure that 'depend(...: var)' and 'depobj(...) depend(...: var)'
! depend on the same variable when 'var' is a pointer
!
program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)

  interface
    subroutine usleep(t) bind(C, name="my_usleep")
      use iso_c_binding
      integer(c_int), value :: t
    end subroutine
  end interface

  integer :: bbb
  integer, target :: c
  integer(omp_depend_kind) :: obj(2)
  integer, pointer :: ppp

  integer :: x1, x2, x3 

  c = 42
  ppp => c
 
  if (.not. associated (ppp)) &
    stop 0;
 
  x1 = 43
  x2 = 44
  x3 = 45
  !$omp depobj(obj(1)) depend(inout: ppp)
  !$omp depobj(obj(2)) depend(in: bbb)
 
  !$omp parallel num_threads(5)
  !$omp single
 
    !$omp task depend (out: ppp)
    write (*,*) "task 1 (start)"
    call usleep(40)
    if (x1 /= 43) stop 11
    if (x2 /= 44) stop 12
    x1 = 11
    write (*,*) "task 1 (end)"
    !$omp end task
 
    !$omp task depend(inout: ppp)
    write (*,*) "task 2 (start)"
    call usleep(30)
    if (x1 /= 11) stop 21
    if (x2 /= 44) stop 22
    x1 = 111
    x2 = 222
    write (*,*) "task 2 (end)"
    !$omp end task
 
    !$omp task depend(out: bbb)
    write (*,*) "task 3 (start)"
    call usleep(40)
    if (x3 /= 45) stop 3
    x3 = 33
    write (*,*) "task 3 (end)"
    !$omp end task
 
   !$omp task depend(depobj: obj(1), obj(2))
    write (*,*) "task 4 (start)"
    if (x1 /= 111) stop 41
    if (x2 /= 222) stop 42
    if (x3 /= 33) stop 43
    call usleep(10)
    x1 = 411
    x2 = 422
    x3 = 433
    write (*,*) "task 4 (end)"
    !$omp end task
 
    !$omp task depend(in: ppp)
    if (x1 /= 411) stop 51
    if (x2 /= 422) stop 52
    if (x3 /= 433) stop 53
    write (*,*) "task 5"
    !$omp end task
 
  !$omp end single
  !$omp end parallel
 
  ! expectation (task dependencies):
  ! 1 - 2 \
  !        4 - 5
  ! 3 ----/
 
end program main

! Ensure that the pointer target address for ppp is taken
! but the address of bbb itself:

! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:ppp\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(inout:ppp\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:&bbb\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:&obj\\\[0\\\]\\) depend\\(depobj:&obj\\\[1\\\]\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(in:ppp\\)" 1 "gimple" } }

! { dg-final { scan-tree-dump-times "MEM\\\[\[^\r\n]+\\\] = ppp;" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "MEM\\\[\[^\r\n]+\\\] = &bbb;" 1 "gimple" } }
