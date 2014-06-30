! { dg-do run }

  call dep ()
  call dep2 ()
  call dep3 ()
  call firstpriv ()
  call antidep ()
  call antidep2 ()
  call antidep3 ()
  call outdep ()
  call concurrent ()
  call concurrent2 ()
  call concurrent3 ()
contains
  subroutine dep
    integer :: x
    x = 1
    !$omp parallel
      !$omp single
        !$omp task shared (x) depend(out: x)
          x = 2
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
      !$omp end single
    !$omp end parallel
  end subroutine dep

  subroutine dep2
    integer :: x
    !$omp parallel
      !$omp single private (x)
        x = 1
        !$omp task shared (x) depend(out: x)
          x = 2
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
        !$omp taskwait
      !$omp end single
    !$omp end parallel
  end subroutine dep2

  subroutine dep3
    integer :: x
    !$omp parallel private (x)
      x = 1
      !$omp single
        !$omp task shared (x) depend(out: x)
          x = 2
        !$omp endtask
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp endtask
      !$omp endsingle
    !$omp endparallel
  end subroutine dep3

  subroutine firstpriv
    integer :: x
    !$omp parallel private (x)
      !$omp single
        x = 1
        !$omp task depend(out: x)
          x = 2
        !$omp end task
        !$omp task depend(in: x)
          if (x.ne.1) call abort
        !$omp end task
      !$omp end single
    !$omp end parallel
  end subroutine firstpriv

  subroutine antidep
    integer :: x
    x = 1
    !$omp parallel
      !$omp single
        !$omp task shared(x) depend(in: x)
          if (x.ne.1) call abort
        !$omp end task
        !$omp task shared(x) depend(out: x)
          x = 2
        !$omp end task
      !$omp end single
    !$omp end parallel
  end subroutine antidep

  subroutine antidep2
    integer :: x
    !$omp parallel private (x)
      !$omp single
        x = 1
        !$omp taskgroup
          !$omp task shared(x) depend(in: x)
            if (x.ne.1) call abort
          !$omp end task
          !$omp task shared(x) depend(out: x)
            x = 2
          !$omp end task
        !$omp end taskgroup
      !$omp end single
    !$omp end parallel
  end subroutine antidep2

  subroutine antidep3
    integer :: x
    !$omp parallel
      x = 1
      !$omp single
        !$omp task shared(x) depend(in: x)
          if (x.ne.1) call abort
        !$omp end task
        !$omp task shared(x) depend(out: x)
          x = 2
        !$omp end task
      !$omp end single
    !$omp end parallel
  end subroutine antidep3

  subroutine outdep
    integer :: x
    !$omp parallel private (x)
      !$omp single
        x = 0
        !$omp task shared(x) depend(out: x)
          x = 1
        !$omp end task
        !$omp task shared(x) depend(out: x)
          x = 2
        !$omp end task
        !$omp taskwait
        if (x.ne.2) call abort
      !$omp end single
    !$omp end parallel
  end subroutine outdep

  subroutine concurrent
    integer :: x
    x = 1
    !$omp parallel
      !$omp single
        !$omp task shared (x) depend(out: x)
          x = 2
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
      !$omp end single
    !$omp end parallel
  end subroutine concurrent

  subroutine concurrent2
    integer :: x
    !$omp parallel private (x)
      !$omp single
        x = 1
        !$omp task shared (x) depend(out: x)
          x = 2;
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
        !$omp taskwait
      !$omp end single
    !$omp end parallel
  end subroutine concurrent2

  subroutine concurrent3
    integer :: x
    !$omp parallel private (x)
      x = 1
      !$omp single
        !$omp task shared (x) depend(out: x)
          x = 2
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
        !$omp task shared (x) depend(in: x)
          if (x.ne.2) call abort
        !$omp end task
      !$omp end single
    !$omp end parallel
  end subroutine concurrent3
end
