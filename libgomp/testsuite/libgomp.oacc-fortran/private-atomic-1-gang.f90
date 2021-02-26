! Test for "oacc gang-private" attribute on gang-private variables

! { dg-do run }
! { dg-additional-options "-fdump-tree-oaccdevlow-details -w" }

program main
  integer :: w, arr(0:31)

  !$acc parallel num_gangs(32) num_workers(32) copyout(arr)
    !$acc loop gang private(w)
! { dg-final { scan-tree-dump-times "Decl UID \[0-9\]+ has gang partitioning:  integer\\(kind=4\\) w;" 1 "oaccdevlow" } } */
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
