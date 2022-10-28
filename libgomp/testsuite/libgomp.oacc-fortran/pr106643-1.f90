! { dg-do run }
! { dg-additional-options -cpp }


!TODO OpenACC 'serial' vs. GCC/nvptx:
!TODO { dg-prune-output {using 'vector_length \(32\)', ignoring 1} }


module m_macron

    implicit none

    real(kind(0d0)), allocatable, dimension(:) :: valls
    !$acc declare create(valls)

contains

    subroutine s_macron_compute(size)

        integer :: size

        !$acc routine seq

#if ACC_MEM_SHARED
        if (valls(size) /= 1) error stop
#else
        if (valls(size) /= size - 2) error stop
#endif

        valls(size) = size + 2

    end subroutine s_macron_compute

    subroutine s_macron_init(size)

        integer :: size

        print*, "size=", size

        print*, "allocate(valls(1:size))"
        allocate(valls(1:size))

        print*, "acc enter data create(valls(1:size))"
        !$acc enter data create(valls(1:size))

        print*, "!$acc update device(valls(1:size))"
        valls(size) = size - 2
        !$acc update device(valls(1:size))

        valls(size) = 1

        !$acc serial
        call s_macron_compute(size)
        !$acc end serial

        valls(size) = -1

        !$acc update host(valls(1:size))
#if ACC_MEM_SHARED
        if (valls(size) /= -1) error stop
#else
        if (valls(size) /= size + 2) error stop
#endif

        print*, valls(1:size)

        print*, "acc exit data delete(valls)"
        !$acc exit data delete(valls)

    end subroutine s_macron_init

end module m_macron


program p_main

    use m_macron

    implicit none

    call s_macron_init(10)

end program p_main
