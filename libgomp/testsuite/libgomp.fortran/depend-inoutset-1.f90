! { dg-additional-sources my-usleep.c }
! { dg-additional-options -Wno-complain-wrong-lang }

program main
  use omp_lib
  implicit none (type, external)

  interface
    subroutine usleep(t) bind(C, name="my_usleep")
      use iso_c_binding
      integer(c_int), value :: t
    end subroutine
  end interface

  integer :: a(0:7) = 0
  integer(omp_depend_kind) :: d1, d2

  !$omp depobj (d1) depend(inoutset: a)
  !$omp depobj (d2) depend(inout: a)
  !$omp depobj (d2) update(inoutset)

  !$omp parallel
   !$omp barrier
   !$omp master
    !$omp task shared(a) depend(out: a)
    block
      call usleep (5000)
      a(0) = 1; a(1) = 2; a(2) = 3; a(3) = 4
    end block
    ! The above task needs to finish first.
    !$omp task shared(a) depend(in: a)
    block
      if (a(0) /= 1 .or. a(1) /= 2 .or. a(2) /= 3 .or. a(3) /= 4) &
        error stop
      call usleep (5000)
      a(4) = 42
    end block
    !$omp task shared(a) depend(in: a)
    block
      if (a(0) /= 1 .or. a(1) /= 2 .or. a(2) /= 3 .or. a(3) /= 4) &
        error stop
      call usleep (5000);
      a(5) = 43
    end block
    !$omp task shared(a) depend(in: a)
    block
      if (a(0) /= 1 .or. a(1) /= 2 .or. a(2) /= 3 .or. a(3) /= 4) &
        error stop
      call usleep (5000)
      a(6) = 44
    end block
    !$omp task shared(a) depend(in: a)
    block
      if (a(0) /= 1 .or. a(1) /= 2 .or. a(2) /= 3 .or. a(3) /= 4) &
        error stop
      call usleep (5000)
      a(7) = 45
    end block
    ! The above 4 tasks can be scheduled in any order but need to wait
    ! for the depend(out: a) task.
    !$omp task shared(a) depend(inoutset: a)
    block
      if (a(4) /= 42 .or. a(5) /= 43 .or. a(6) /= 44 .or. a(7) /= 45) &
        error stop
      call usleep (5000)
      a(0) = 42
    end block
    !$omp task shared(a) depend(iterator(i=1:3:2), inoutset: a)
    block
      if (a(4) /= 42 .or. a(5) /= 43 .or. a(6) /= 44 .or. a(7) /= 45) &
        error stop
      call usleep (5000)
      a(1) = 43
    end block
    !$omp task shared(a) depend(depobj: d1)
    block
      if (a(4) /= 42 .or. a(5) /= 43 .or. a(6) /= 44 .or. a(7) /= 45) &
        error stop
      call usleep (5000)
      a(2) = 44
    end block
    !$omp task shared(a) depend(depobj: d2)
    block
      if (a(4) /= 42 .or. a(5) /= 43 .or. a(6) /= 44 .or. a(7) /= 45) &
        error stop
      call usleep (5000)
      a(3) = 45
    end block
    ! The above 4 tasks can be scheduled in any order but need to wait
    ! for all the above depend(in: a) tasks.
    !$omp task shared(a) depend(in: a)
    block
      if (a(0) /= 42 .or. a(1) /= 43 .or. a(2) /= 44 .or. a(3) /= 45) &
        error stop
      call usleep (5000)
      a(4) = 46
    end block
    !$omp task shared(a) depend(in: a)
    block
      if (a(0) /= 42 .or. a(1) /= 43 .or. a(2) /= 44 .or. a(3) /= 45) &
        error stop
      call usleep (5000)
      a(5) = 47
    end block
    !$omp task shared(a) depend(in: a)
    block
      if (a(0) /= 42 .or. a(1) /= 43 .or. a(2) /= 44 .or. a(3) /= 45) &
        error stop
      call usleep (5000)
      a(6) = 48
    end block
    !$omp task shared(a) depend(in: a)
    block
      if (a(0) /= 42 .or. a(1) /= 43 .or. a(2) /= 44 .or. a(3) /= 45) &
        error stop
      call usleep (5000)
      a(7) = 49
    end block
    ! The above 4 tasks can be scheduled in any order but need to wait
    ! for all the above depend(inoutset: a),
    !  depend(iterator(i=1:3:2), inoutset: a), depend(depobj: d1) and
    !  depend(depobj: d2) tasks.
    !$omp task shared(a) depend(inoutset: a)
    block
      if (a(4) /= 46.or. a(5) /= 47 .or. a(6) /= 48 .or. a(7) /= 49) &
        error stop
      call usleep (5000)
      a(0) = 50
    end block
    ! The above task needs to wait for all the above 4 depend(in: a)
    ! tasks.
    !$omp task shared(a) depend(out: a)
    block
      if (a(0) /= 50 .or. a(4) /= 46.or. a(5) /= 47 .or. a(6) /= 48 .or. a(7) /= 49) &
        error stop
      call usleep (5000)
      a(0) = 51
    end block
    ! The above task needs to wait for the above depend(inoutset: a) task.
    !$omp task shared(a) depend(inoutset: a)
    block
      if (a(0) /= 51 .or. a(4) /= 46.or. a(5) /= 47 .or. a(6) /= 48 .or. a(7) /= 49) &
        error stop
      call usleep (5000)
      a(0) = 52
    end block
    ! The above task needs to wait for the above depend(out: a) task.
    !$omp task shared(a) depend(mutexinoutset: a)
    block
      if (a(0) /= 52 .or. a(4) /= 46.or. a(5) /= 47 .or. a(6) /= 48 .or. a(7) /= 49) &
        error stop
      call usleep (5000)
      a(0) = 53
    end block
    ! The above task needs to wait for the above depend(inoutset: a) task.
    !$omp task shared(a) depend(inoutset: a)
    block
      if (a(0) /= 53 .or. a(4) /= 46.or. a(5) /= 47 .or. a(6) /= 48 .or. a(7) /= 49) &
        error stop
      call usleep (5000)
      a(0) = 54
    end block
    ! The above task needs to wait for the above
    ! depend(mutexinoutset: a) task.
   !$omp end master
  !$omp end parallel
  if (a(0) /= 54 .or. a(1) /= 43 .or. a(2) /= 44 .or. a(3) /= 45 &
      .or. a(4) /= 46.or. a(5) /= 47 .or. a(6) /= 48 .or. a(7) /= 49) &
    error stop
end
