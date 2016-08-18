! { dg-do run }
! { dg-additional-options "-w" }

! subroutine reduction with private and firstprivate variables

program reduction
  integer, parameter    :: n = 100
  integer               :: i, j, vsum, cs, arr(n)

  call redsub_private (cs, n, arr)
  call redsub_bogus (cs, n)
  call redsub_combined (cs, n, arr)

  vsum = 0

  ! Verify the results
  do i = 1, n
     vsum = i
     do j = 1, n
        vsum = vsum + 1;
     end do
     if (vsum .ne. arr(i)) call abort ()
  end do
end program reduction

! This subroutine tests a reduction with an explicit private variable.

subroutine redsub_private(sum, n, arr)
  integer :: sum, n, arr(n)
  integer :: i, j, v

  !$acc parallel copyout (arr)
  !$acc loop gang private (v)
  do j = 1, n
     v = j

     !$acc loop vector reduction (+:v)
     do i = 1, 100
        v = v + 1
     end do

     arr(j) = v
  end do
  !$acc end parallel

  ! verify the results
  do i = 1, 10
     if (arr(i) .ne. 100+i) call abort ()
  end do
end subroutine redsub_private


! Bogus reduction on a firstprivate variable.  The results do
! survive the parallel region.  The goal here is to ensure that gfortran
! doesn't ICE.

subroutine redsub_bogus(sum, n)
  integer :: sum, n, arr(n)
  integer :: i

  !$acc parallel firstprivate(sum)
  !$acc loop gang worker vector reduction (+:sum)
  do i = 1, n
     sum = sum + 1
  end do
  !$acc end parallel
end subroutine redsub_bogus

! This reduction involving a firstprivate variable yields legitimate results.

subroutine redsub_combined(sum, n, arr)
  integer :: sum, n, arr(n)
  integer :: i, j

  !$acc parallel copy (arr) firstprivate(sum)
  !$acc loop gang
  do i = 1, n
     sum = i;

     !$acc loop reduction(+:sum)
     do j = 1, n
        sum = sum + 1
     end do

     arr(i) = sum
  end do
  !$acc end parallel
end subroutine redsub_combined
