program test
    use omp_lib
    implicit none
    integer(omp_event_handle_kind) :: oevent, ievent
    integer :: i
    integer, allocatable :: temp(:)
    ALLOCATE(temp(5))

    !$omp parallel num_threads(3)
    !$omp single
    DO i=1,5
    !$omp task firstprivate(i) firstprivate(temp)  detach(oevent)
          temp(:) = 0;
          temp(1) = -1;
          !print *,temp
          call omp_fulfill_event(oevent)
    !$omp end task
    ENDDO
    !$omp taskwait
    !$omp end single
    !$omp end parallel
end program
