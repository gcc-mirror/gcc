! Run the target region asynchronously and check it
!
! Note that  map(alloc: work(:, i)) + nowait  should be safe
! given that a nondescriptor array is used. However, it still
! violates a map clause restriction, added in OpenMP 5.1 [354:10-13].

PROGRAM test_target_teams_distribute_nowait
  USE ISO_Fortran_env, only: INT64
  implicit none
    INTEGER, parameter :: N = 1024, N_TASKS = 16
    INTEGER :: i, j, k, my_ticket
    INTEGER :: order(n_tasks)
    INTEGER(INT64) :: work(n, n_tasks)
    INTEGER :: ticket
    logical :: async

    ticket = 0

    !$omp target enter data map(to: ticket, order)

    !$omp parallel do num_threads(n_tasks)
    DO i = 1, n_tasks
       !$omp target map(alloc: work(:, i), ticket) private(my_ticket) nowait
       !!$omp target teams distribute map(alloc: work(:, i), ticket) private(my_ticket) nowait
       DO j = 1, n
          ! Waste cyles
!          work(j, i) = 0
!          DO k = 1, n*(n_tasks - i)
!             work(j, i) = work(j, i) + i*j*k
!          END DO
          my_ticket = 0
          !$omp atomic capture
          ticket = ticket + 1
          my_ticket = ticket
          !$omp end atomic
          !$omp atomic write
          order(i) = my_ticket
       END DO
       !$omp end target !teams distribute
    END DO
    !$omp end parallel do

    !$omp target exit data map(from:ticket, order)

    IF (ticket .ne. n_tasks*n) stop 1
    if (maxval(order) /= n_tasks*n) stop 2
    ! order(i) == n*i if synchronous and between n and n*n_tasks if run concurrently
    do i = 1, n_tasks
      if (order(i) < n .or. order(i) > n*n_tasks) stop 3
    end do
    async = .false.
    do i = 1, n_tasks
      if (order(i) /= n*i) async = .true.
    end do
    if (.not. async) stop 4 ! Did not run asynchronously
end
