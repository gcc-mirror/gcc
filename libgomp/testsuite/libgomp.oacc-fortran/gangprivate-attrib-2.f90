! Test for lack of "oacc gangprivate" attribute on worker-private variables

! { dg-do run }
! { dg-additional-options "-fdump-tree-omplower-details" }
! { dg-final { scan-tree-dump-times "Setting 'oacc gangprivate' attribute for decl" 0 "omplower" } } */

program main
  integer :: w, arr(0:31)

  !$acc parallel num_gangs(32) num_workers(32) copyout(arr)
    !$acc loop gang worker private(w)
    do j = 0, 31
      w = 0
      !$acc loop seq
      do i = 0, 31
        w = w + 1
      end do
      arr(j) = w
    end do
  !$acc end parallel

  if (any (arr .ne. 32)) stop 1
end program main
