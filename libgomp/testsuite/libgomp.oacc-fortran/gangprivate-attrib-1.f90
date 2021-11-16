! Test for "oacc gangprivate" attribute on gang-private variables

! { dg-do run }
! { dg-additional-options "-fdump-tree-oaccdevlow-details -Wopenacc-parallelism" }
! { dg-final { scan-tree-dump-times "Decl UID \[0-9\]+ has gang partitioning:  integer\\(kind=4\\) w;" 1 "oaccdevlow1" { target { any-opts "-O[1,2,3,s]"} } } } */
! { dg-final { scan-tree-dump-times "Decl UID \[0-9\]+ has gang partitioning:  integer\\(kind=4\\) w;" 1 "oaccdevlow3" { target { no-opts "-O[1,2,3,s]"} } } } */

program main
  integer :: w, arr(0:31)

  !$acc parallel num_gangs(32) num_workers(32) copyout(arr) ! { dg-warning "region is worker partitioned but does not contain worker partitioned code" }
    !$acc loop gang private(w)
    do j = 0, 31
      w = 0
      !$acc loop seq
      do i = 0, 31
        !$acc atomic update
        w = w + 1
        !$acc end atomic
      end do
      arr(j) = w
    end do
  !$acc end parallel

  if (any (arr .ne. 32)) stop 1
end program main
