! { dg-additional-sources my-usleep.c }
! { dg-additional-options -Wno-complain-wrong-lang }

module m
  use omp_lib
  implicit none

  interface
    subroutine usleep(t) bind(C, name="my_usleep")
      use iso_c_binding
      integer(c_int), value :: t
    end subroutine
  end interface

contains
subroutine test (ifval)
  logical, value :: ifval
  integer :: a(0:7), b(0:7), i
  integer(omp_depend_kind) d1, d2
  !$omp depobj (d1) depend(inout: omp_all_memory) 
  !$omp depobj (d2) depend(out: omp_all_memory)
  do i = 0, 7
    a(i) = i
    b(i) = 2 * i
  end do
  !$omp parallel
  block
   !$omp single
   block
    !$omp task shared(a) depend(in: a(0))
    block
      call usleep (5000)
      a(0) = 42
    end block
    !$omp task shared(a) depend(out: a(1))
    block
      call usleep (5000)
      a(1) = 43
    end block
    !$omp task shared(a) depend(inout: a(2))
    block
      call usleep (5000)
      a(2) = 44
    end block
    !$omp task shared(a) depend(mutexinoutset: a(3))
    block
      call usleep (5000)
      a(3) = 45
    end block
    !$omp task shared(a)
    block
      call usleep (15000)
      a(4) = 46
    end block
    !$omp task shared(b) depend(in: b(0))
    block
      call usleep (5000)
      b(0) = 47
    end block
    !$omp task shared(b) depend(in: b(4))
    block
      call usleep (5000)
      b(4) = 48
    end block
    !$omp task shared(b) depend(inoutset: b(5))
    block
      call usleep (5000)
      b(5) = 49
    end block

    ! None of the above tasks depend on each other.
    ! The following task depends on all but the a(4) = 46; one.
    !$omp task shared(a, b) depend(depobj: d1) private(i) if(ifval)
    block
      if (a(0) /= 42 .or. a(1) /= 43 .or. a(2) /= 44 .or. a(3) /= 45       &
          .or. a(5) /= 5 .or. a(6) /= 6 .or. a(7) /= 7                     &
          .or. b(0) /= 47 .or. b(1) /= 2 .or. b(2) /= 4 .or. b(3) /= 6     &
          .or. b(4) /= 48 .or. b(5) /= 49 .or. b(6) /= 12 .or. b(7) /= 14) &
        error stop
      do i = 0, 7
        if (i /= 4) &
          a(i) = 3 * i + 7
      end do
      do i = 0, 7
        b(i) = 4 * i - 7
      end do
    end block
    ! The following task depends on both b(0) = 47; and
    ! above omp_all_memory tasks, but as the latter depends on
    ! the former, effectively it is dependent just on the omp_all_memory
    ! task.
    !$omp task shared(b) depend(inout: b(0))
    block
      call usleep (5000)
      b(0) = 49
    end block
    ! The following task depends on all the above except a(4) = 46; one,
    ! but it can be reduced to dependency on the above omp_all_memory
    ! one and b(0) = 49; one.
    !$omp task shared(a, b) depend(inout: b(6)) depend(depobj: d2) &
    !$omp&     depend(out: b(7)) private(i) if(ifval)
    block
      do i = 0, 7
        if (i /= 4) then
          if (a(i) /= 3 * i + 7) &
            error stop
          a(i) = 5 * i + 50
        end if
      end do
      if (b(0) /= 49) &
        error stop
      b(0) = 6 * i + 57
      do i = 1, 7
        if (b(i) /= 4 * i - 7) &
          error stop
        b(i) = 6 * i + 57
      end do
    end block
    !$omp taskwait
    if (a(4) /= 46) &
      error stop
   end block
  end block
  !$omp depobj (d2) destroy
  !$omp depobj (d1) destroy
end
end module m

use m
call test (.true.)
call test (.false.)
end
