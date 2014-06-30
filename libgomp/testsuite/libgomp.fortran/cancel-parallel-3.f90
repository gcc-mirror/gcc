! { dg-do run }
! { dg-options "-fno-inline -fno-ipa-sra -fno-ipa-cp -fno-ipa-cp-clone" }
! { dg-set-target-env-var OMP_CANCELLATION "true" }

  use omp_lib
  integer :: x, i, j
  common /x/ x

  call omp_set_dynamic (.false.)
  call omp_set_schedule (omp_sched_static, 1)
  !$omp parallel num_threads(16) private (i, j)
    call do_some_work
    !$omp barrier
    if (omp_get_thread_num ().eq.1) then
      call sleep (2)
      !$omp cancellation point parallel
    end if
    do j = 3, 16
      !$omp do schedule(runtime)
	do i = 0, j - 1
	  call do_some_work
	end do
      !$omp enddo nowait
    end do
    if (omp_get_thread_num ().eq.0) then
      call sleep (1)
      !$omp cancel parallel
    end if
  !$omp end parallel
contains
  subroutine do_some_work
    integer :: x
    common /x/ x
    !$omp atomic
      x = x + 1
    !$omp end atomic
  endsubroutine do_some_work
end
