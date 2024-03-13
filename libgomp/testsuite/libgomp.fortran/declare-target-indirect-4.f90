! { dg-additional-options "-fdump-tree-gimple" }

! PR fortran/114283

! { dg-final { scan-tree-dump "#pragma omp parallel shared\\(i\\) if\\(0\\) default\\(none\\) firstprivate\\(g\\)" "gimple" } }
! { dg-final { scan-tree-dump "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) firstprivate\\(h\\) map\\(from:j \\\[len: 4\\\]\\) defaultmap\\(none\\)" "gimple" } }


module m
  implicit none (type, external)
  !$omp declare target indirect enter(f1, f2)
contains
  integer function f1 ()
    f1 = 99
  end
  integer function f2 ()
    f2 = 89
  end
end module m

use m
implicit none (type, external)
call sub1(f1)
call sub2(f2)
contains
  subroutine sub1(g)
    procedure(integer) :: g
    integer :: i
    !$omp parallel default(none) if(.false.) shared(i)
      i = g ()
    !$omp end parallel
    if (i /= 99) stop 1
  end

  subroutine sub2(h)
    procedure(integer) :: h
    integer :: j
    !$omp target defaultmap(none) map(from:j)
      j = h ()
    !$omp end target
    if (j /= 89) stop 1
  end
end
