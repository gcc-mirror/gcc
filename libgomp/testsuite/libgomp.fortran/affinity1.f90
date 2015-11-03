! { dg-do run }
! { dg-skip-if "" { ! run_expensive_tests } { "*" } { "-O2" } }
! { dg-set-target-env-var OMP_PROC_BIND "spread,close" }
! { dg-set-target-env-var OMP_PLACES "{6,7}:4:-2,!{2,3}" }
! { dg-set-target-env-var OMP_NUM_THREADS "2" }

  use omp_lib
  integer :: num, i, nump
  num = omp_get_num_places ()
  print *, 'omp_get_num_places () == ', num
  do i = 0, num - 1
    nump = omp_get_place_num_procs (place_num = i)
    if (nump .eq. 0) then
      print *, 'place ', i, ' {}'
    else
      call print_place (i, nump)
    end if
  end do
  call print_place_var
  call omp_set_nested (nested = .true.)
  !$omp parallel
    if (omp_get_thread_num () == omp_get_num_threads () - 1) then
      !$omp parallel
        if (omp_get_thread_num () == omp_get_num_threads () - 1) &
          call print_place_var
      !$omp end parallel
    end if
  !$omp end parallel
contains
  subroutine print_place (i, nump)
    integer, intent (in) :: i, nump
    integer :: ids(nump)
    call omp_get_place_proc_ids (place_num = i, ids = ids)
    print *, 'place ', i, ' {', ids, '}'
  end subroutine
  subroutine print_place_var
    integer :: place, num_places
    place = omp_get_place_num ()
    num_places = omp_get_partition_num_places ()
    print *, 'place ', place
    if (num_places .gt. 0) call print_partition (num_places)
  end subroutine
  subroutine print_partition (num_places)
    integer, intent (in) :: num_places
    integer :: place_nums(num_places)
    call omp_get_partition_place_nums (place_nums = place_nums)
    print *, 'partition ', place_nums(1), '-', place_nums(num_places)
  end subroutine
end
