! Test array descriptor handling with the need_device_addr modifier to adjust_args

module m
  use iso_c_binding
  implicit none (type, external)

  integer :: case = 0
contains
  subroutine var_array_alloc(x)
    integer, allocatable :: x(:)
    !$omp target has_device_addr(x)
    block
      if (size(x) /= 3) stop 1
      if (any (x /= [1,2,3])) stop 2
      x = x * (-1)
    end block
  end

  subroutine base_array_alloc(x)
    !$omp declare variant(var_array_alloc) match(construct={dispatch}) adjust_args(need_device_addr : x)
    integer, allocatable :: x(:)
    error stop
  end

  subroutine var_array_nonalloc(x)
    integer :: x(:)
    !$omp target has_device_addr(x)
    block
      if (size(x) /= 4) stop 3
      if (any (x /= [11,22,33,44])) stop 4
      x = x * (-1)
    end block
  end

  subroutine base_array_nonalloc(x)
    !$omp declare variant(var_array_nonalloc) match(construct={dispatch}) adjust_args(need_device_addr : x)
    integer :: x(:)
    error stop
  end

  subroutine test_array_alloc(y)
    integer, allocatable :: y(:)
    !$omp target enter data map(y)


  ! Direct call (for testing; value check fails if both are enabled
  !  !$omp target data use_device_addr(y)
  !    call var_array_alloc (y)
  !  !$omp end target data

    !$omp dispatch
      call base_array_alloc (y)

    !$omp target exit data map(y)

    if (size(y) /= 3) stop 3
    if (any (y /= [-1,-2,-3])) stop 1
  end

  subroutine test_array_nonalloc()
    integer :: y(4)
    y = [11,22,33,44]

    !$omp target enter data map(y)

    ! Direct call (for testing; value check fails if both are enabled
    !!$omp target data use_device_addr(y)
    !  call var_array_nonalloc (y)
    !!$omp end target data

    !$omp dispatch
      call base_array_nonalloc (y)

    !$omp target exit data map(y)

    if (size(y) /= 4) stop 3
    if (any (y /= [-11,-22,-33,-44])) stop 1
  end
end module

use m
implicit none
integer, allocatable :: z(:)

z = [1,2,3]
call test_array_alloc(z)
call test_array_nonalloc()

end
