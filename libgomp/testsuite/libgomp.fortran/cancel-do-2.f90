! { dg-do run }
! { dg-options "-fno-inline -fno-ipa-sra -fno-ipa-cp -fno-ipa-cp-clone" }
! { dg-set-target-env-var OMP_CANCELLATION "true" }

  use omp_lib
  integer :: i
  logical :: x(5)

  x(:) = .false.
  x(1) = .true.
  x(3) = .true.
  if (omp_get_cancellation ()) call foo (x)
contains
  subroutine foo (x)
    use omp_lib
    logical :: x(5)
    integer :: v, w, i

    v = 0
    w = 0
    !$omp parallel num_threads (32) shared (v, w)
      !$omp do
	do i = 0, 999
	  !$omp cancel do if (x(1))
	  STOP 1
	end do
      !$omp do
	do i = 0, 999
	  !$omp cancel do if (x(2))
	  !$omp atomic
	    v = v + 1
	  !$omp endatomic
	enddo
      !$omp do
	do i = 0, 999
	  !$omp cancel do if (x(3))
	  !$omp atomic
	    w = w + 8
	  !$omp end atomic
	end do
      !$omp do
	do i = 0, 999
	  !$omp cancel do if (x(4))
	  !$omp atomic
	    v = v + 2
	  !$omp end atomic
	end do
      !$omp end do
    !$omp end parallel
    if (v.ne.3000.or.w.ne.0) STOP 2
    !$omp parallel num_threads (32) shared (v, w)
      ! None of these cancel directives should actually cancel anything,
      ! but the compiler shouldn't know that and thus should use cancellable
      ! barriers at the end of all the workshares.
      !$omp cancel parallel if (omp_get_thread_num ().eq.1.and.x(5))
      !$omp do
	do i = 0, 999
	  !$omp cancel do if (x(1))
	  STOP 3
	end do
      !$omp cancel parallel if (omp_get_thread_num ().eq.2.and.x(5))
      !$omp do
	do i = 0, 999
	  !$omp cancel do if (x(2))
	  !$omp atomic
	    v = v + 1
	  !$omp endatomic
	enddo
      !$omp cancel parallel if (omp_get_thread_num ().eq.3.and.x(5))
      !$omp do
	do i = 0, 999
	  !$omp cancel do if (x(3))
	  !$omp atomic
	    w = w + 8
	  !$omp end atomic
	end do
      !$omp cancel parallel if (omp_get_thread_num ().eq.4.and.x(5))
      !$omp do
	do i = 0, 999
	  !$omp cancel do if (x(4))
	  !$omp atomic
	    v = v + 2
	  !$omp end atomic
	end do
      !$omp end do
      !$omp cancel parallel if (omp_get_thread_num ().eq.5.and.x(5))
    !$omp end parallel
    if (v.ne.6000.or.w.ne.0) STOP 4
  end subroutine
end
