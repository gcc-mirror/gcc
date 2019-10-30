! { dg-do run }
program lastprivate
  integer :: i
  common /c/ i
  !$omp parallel num_threads (4)
  call test1
  !$omp end parallel
  if (i .ne. 21) stop 1
  !$omp parallel num_threads (4)
  call test2
  !$omp end parallel
  if (i .ne. 64) stop 2
  !$omp parallel num_threads (4)
  call test3
  !$omp end parallel
  if (i .ne. 14) stop 3
  call test4
  call test5
  call test6
  call test7
  call test8
  call test9
  call test10
  call test11
  call test12
contains
  subroutine test1
    integer :: i
    common /c/ i
    !$omp do lastprivate (i)
    do i = 1, 20
    end do
  end subroutine test1
  subroutine test2
    integer :: i
    common /c/ i
    !$omp do lastprivate (i)
    do i = 7, 61, 3
    end do
  end subroutine test2
  function ret3 ()
    integer :: ret3
    ret3 = 3
  end function ret3
  subroutine test3
    integer :: i
    common /c/ i
    !$omp do lastprivate (i)
    do i = -10, 11, ret3 ()
    end do
  end subroutine test3
  subroutine test4
    integer :: j
    !$omp parallel do lastprivate (j) num_threads (4) default (none)
    do j = 1, 20
    end do
    if (j .ne. 21) stop 4
  end subroutine test4
  subroutine test5
    integer :: j
    !$omp parallel do lastprivate (j) num_threads (4) default (none)
    do j = 7, 61, 3
    end do
    if (j .ne. 64) stop 5
  end subroutine test5
  subroutine test6
    integer :: j
    !$omp parallel do lastprivate (j) num_threads (4) default (none)
    do j = -10, 11, ret3 ()
    end do
    if (j .ne. 14) stop 6
  end subroutine test6
  subroutine test7
    integer :: i
    common /c/ i
    !$omp parallel do lastprivate (i) num_threads (4) default (none)
    do i = 1, 20
    end do
    if (i .ne. 21) stop 7
  end subroutine test7
  subroutine test8
    integer :: i
    common /c/ i
    !$omp parallel do lastprivate (i) num_threads (4) default (none)
    do i = 7, 61, 3
    end do
    if (i .ne. 64) stop 8
  end subroutine test8
  subroutine test9
    integer :: i
    common /c/ i
    !$omp parallel do lastprivate (i) num_threads (4) default (none)
    do i = -10, 11, ret3 ()
    end do
    if (i .ne. 14) stop 9
  end subroutine test9
  subroutine test10
    integer :: i
    common /c/ i
    !$omp parallel num_threads (4) default (none) shared (i)
    !$omp do lastprivate (i)
    do i = 1, 20
    end do
    !$omp end parallel
    if (i .ne. 21) stop 10
  end subroutine test10
  subroutine test11
    integer :: i
    common /c/ i
    !$omp parallel num_threads (4) default (none) shared (i)
    !$omp do lastprivate (i)
    do i = 7, 61, 3
    end do
    !$omp end parallel
    if (i .ne. 64) stop 11
  end subroutine test11
  subroutine test12
    integer :: i
    common /c/ i
    !$omp parallel num_threads (4) default (none) shared (i)
    !$omp do lastprivate (i)
    do i = -10, 11, ret3 ()
    end do
    !$omp end parallel
    if (i .ne. 14) stop 12
  end subroutine test12
end program lastprivate
