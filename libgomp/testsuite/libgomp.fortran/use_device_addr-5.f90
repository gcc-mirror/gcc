program main
  use omp_lib
  implicit none
  integer, allocatable :: aaa(:,:,:)
  integer :: i

  allocate (aaa(-4:10,-3:8,2))
  aaa(:,:,:) = reshape ([(i, i = 1, size(aaa))], shape(aaa))

  do i = 0, omp_get_num_devices()
    !$omp target data map(to: aaa) device(i)
      call test_addr (aaa, i)
      call test_ptr (aaa, i)
    !$omp end target data
  end do
  deallocate (aaa)

contains

  subroutine test_addr (aaaa, dev)
    use iso_c_binding
    integer, target, allocatable :: aaaa(:,:,:), bbbb(:,:,:)
    integer, value :: dev
    integer :: i
    type(c_ptr) :: ptr
    logical :: is_shared

    is_shared = .false.
    !$omp target device(dev) map(to: is_shared)
      is_shared = .true.
    !$omp end target

    allocate (bbbb(-4:10,-3:8,2))
    bbbb(:,:,:) = reshape ([(-i, i = 1, size(bbbb))], shape(bbbb))
    !$omp target enter data map(to: bbbb) device(dev)
    if (any (lbound (aaaa) /= [-4, -3, 1])) error stop 1
    if (any (shape (aaaa) /= [15, 12, 2])) error stop 2
    if (any (lbound (bbbb) /= [-4, -3, 1])) error stop 3
    if (any (shape (bbbb) /= [15, 12, 2])) error stop 4
    if (any (aaaa /= -bbbb)) error stop 5
    if (any (aaaa /= reshape ([(i, i = 1, size(aaaa))], shape(aaaa)))) &
      error stop 6

    !$omp parallel do shared(bbbb, aaaa)
    do i = 1,1
      if (any (lbound (aaaa) /= [-4, -3, 1])) error stop 5
      if (any (shape (aaaa) /= [15, 12, 2])) error stop 6
      if (any (lbound (bbbb) /= [-4, -3, 1])) error stop 7
      if (any (shape (bbbb) /= [15, 12, 2])) error stop 8
      if (any (aaaa /= -bbbb)) error stop 5
      if (any (aaaa /= reshape ([(i, i = 1, size(aaaa))], shape(aaaa)))) &
        error stop 6
      ptr = c_loc (aaaa)
      !$omp target data use_device_addr(bbbb, aaaa) device(dev)
        if (any (lbound (aaaa) /= [-4, -3, 1])) error stop 9
        if (any (shape (aaaa) /= [15, 12, 2])) error stop 10
        if (any (lbound (bbbb) /= [-4, -3, 1])) error stop 11
        if (any (shape (bbbb) /= [15, 12, 2])) error stop 12
        if (is_shared) then
          if (any (aaaa /= -bbbb)) error stop 5
          if (any (aaaa /= reshape ([(i, i = 1, size(aaaa))], shape(aaaa)))) &
            error stop 6
        end if
        if (is_shared .neqv. c_associated (ptr, c_loc (aaaa))) error stop

        !$omp target has_device_addr(bbbb, aaaa) device(dev)
           if (any (lbound (aaaa) /= [-4, -3, 1])) error stop 9
           if (any (shape (aaaa) /= [15, 12, 2])) error stop 10
           if (any (lbound (bbbb) /= [-4, -3, 1])) error stop 11
           if (any (shape (bbbb) /= [15, 12, 2])) error stop 12
           if (any (aaaa /= -bbbb)) error stop 5
           if (any (aaaa /= reshape ([(i, i = 1, size(aaaa))], shape(aaaa)))) &
             error stop 6
        !$omp end target
      !$omp end target data
    end do
    !$omp target exit data map(delete: bbbb) device(dev)
    deallocate (bbbb)
  end subroutine test_addr

  subroutine test_ptr (aaaa, dev)
    use iso_c_binding
    integer, target, allocatable :: aaaa(:,:,:), bbbb(:,:,:)
    integer, value :: dev
    integer :: i
    type(c_ptr) :: ptr
    logical :: is_shared

    is_shared = .false.
    !$omp target device(dev) map(to: is_shared)
      is_shared = .true.
    !$omp end target

    allocate (bbbb(-4:10,-3:8,2))
    bbbb(:,:,:) = reshape ([(-i, i = 1, size(bbbb))], shape(bbbb))
    !$omp target enter data map(to: bbbb) device(dev)
    if (any (lbound (aaaa) /= [-4, -3, 1])) error stop 1
    if (any (shape (aaaa) /= [15, 12, 2])) error stop 2
    if (any (lbound (bbbb) /= [-4, -3, 1])) error stop 3
    if (any (shape (bbbb) /= [15, 12, 2])) error stop 4
    if (any (aaaa /= -bbbb)) error stop 5
    if (any (aaaa /= reshape ([(i, i = 1, size(aaaa))], shape(aaaa)))) &
      error stop 6

    !$omp parallel do shared(bbbb, aaaa)
    do i = 1,1
      if (any (lbound (aaaa) /= [-4, -3, 1])) error stop 5
      if (any (shape (aaaa) /= [15, 12, 2])) error stop 6
      if (any (lbound (bbbb) /= [-4, -3, 1])) error stop 7
      if (any (shape (bbbb) /= [15, 12, 2])) error stop 8
      if (any (aaaa /= -bbbb)) error stop 5
      if (any (aaaa /= reshape ([(i, i = 1, size(aaaa))], shape(aaaa)))) &
        error stop 6
      ptr = c_loc (aaaa)
      !$omp target data use_device_ptr(bbbb, aaaa) device(dev)
        if (any (lbound (aaaa) /= [-4, -3, 1])) error stop 9
        if (any (shape (aaaa) /= [15, 12, 2])) error stop 10
        if (any (lbound (bbbb) /= [-4, -3, 1])) error stop 11
        if (any (shape (bbbb) /= [15, 12, 2])) error stop 12
        if (is_shared) then
          if (any (aaaa /= -bbbb)) error stop 5
          if (any (aaaa /= reshape ([(i, i = 1, size(aaaa))], shape(aaaa)))) &
            error stop 6
        end if
        if (is_shared .neqv. c_associated (ptr, c_loc (aaaa))) error stop

        ! Uses has_device_addr due to PR fortran/105318
        !!$omp target is_device_ptr(bbbb, aaaa) device(dev)
        !$omp target has_device_addr(bbbb, aaaa) device(dev)
           if (any (lbound (aaaa) /= [-4, -3, 1])) error stop 9
           if (any (shape (aaaa) /= [15, 12, 2])) error stop 10
           if (any (lbound (bbbb) /= [-4, -3, 1])) error stop 11
           if (any (shape (bbbb) /= [15, 12, 2])) error stop 12
           if (any (aaaa /= -bbbb)) error stop 5
           if (any (aaaa /= reshape ([(i, i = 1, size(aaaa))], shape(aaaa)))) &
             error stop 6
        !$omp end target
      !$omp end target data
    end do
    !$omp target exit data map(delete: bbbb) device(dev)
    deallocate (bbbb)
  end subroutine test_ptr
end program main
