module m
  use omp_lib, only: omp_depend_kind
  implicit none (type, external)
  integer :: xx
  integer(omp_depend_kind) :: dd1, dd2
contains
  subroutine dep
    integer :: x
    integer(omp_depend_kind) :: d1, d2
    x = 1

    !$omp depobj (d1) depend(in: x)
    !$omp depobj (d2) depend(in: x)
    !$omp depobj (d2) update(out)
    !$omp parallel
      !$omp single
        !$omp task shared (x) depend(depobj: d2)
          x = 2
        !$omp end task
        !$omp task shared (x) depend(depobj: d1)
          if (x /= 2) &
            stop 1
        !$omp end task
      !$omp end single
    !$omp end parallel
    !$omp depobj (d2) destroy
    !$omp depobj (d1) destroy
  end

  subroutine dep2
    integer(omp_depend_kind) :: d1, d2
    pointer :: d1
    allocate(d1)
    call dep2i(d1, d2)
    deallocate(d1)
  contains
   subroutine dep2i(d1, d2)
    integer(omp_depend_kind) :: d1
    integer(omp_depend_kind), optional :: d2
    pointer :: d1
    !$omp parallel
      !$omp single
        block
        integer :: x
        x = 1
        !$omp depobj (d1) depend(out: x)
        !$omp depobj (d2) depend (in:x)
        !$omp depobj(d2)update(in)
        !$omp task shared (x) depend(depobj:d1)
          x = 2
        !$omp end task
        !$omp task shared (x) depend(depobj : d2)
          if (x /= 2) &
            stop 2
        !$omp end task
        !$omp taskwait
        !$omp depobj(d1)destroy
        !$omp depobj(d2) destroy
        end block
     !$omp end single
   !$omp end parallel
  end
  end

  subroutine dep3
    integer(omp_depend_kind) :: d(2)
    !$omp parallel
      block
      integer :: x
      x = 1
      !$omp single
        !$omp depobj(d(1)) depend(out:x)
        !$omp depobj(d(2)) depend(in: x)
        !$omp task shared (x) depend(depobj: d(1))
          x = 2
        !$omp end task
        !$omp task shared (x) depend(depobj: d(2))
          if (x /= 2) &
            stop 3
        !$omp end task
      !$omp end single
      end block
    !$omp end parallel
    !$omp depobj(d(1)) destroy
    !$omp depobj(d(2)) destroy
  end

  subroutine antidep
    xx = 1
    !$omp parallel
      !$omp single
        !$omp task shared(xx) depend(depobj:dd2)
          if (xx /= 1) &
            stop 4
        !$omp end task
        !$omp task shared(xx) depend(depobj:dd1)
          xx = 2
        !$omp end task
      !$omp end single
    !$omp end parallel
  end
end module m

program main
  use m
  implicit none (type, external)
  call dep ()
  call dep2 ()
  call dep3 ()
  !$omp depobj (dd1) depend (inout: xx)
  !$omp depobj (dd2) depend (in : xx)
  call antidep ()
  !$omp depobj (dd2) destroy
  !$omp depobj (dd1) destroy
end program main
