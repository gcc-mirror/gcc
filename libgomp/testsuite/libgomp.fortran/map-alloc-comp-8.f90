module m
  implicit none (type, external)
  type t
    integer, allocatable :: A(:)
  end type t
  type t2
    type(t), allocatable :: vT
    integer, allocatable :: x
  end type t2

contains

  subroutine test_alloc()
    type(t) :: var
    type(t), allocatable :: var2

    allocate(var2)
    allocate(var%A(4), var2%A(5))

    !$omp target enter data map(alloc: var, var2)
    !$omp target
      if (.not. allocated(Var2)) stop 1
      if (.not. allocated(Var%A)) stop 2
      if (.not. allocated(Var2%A)) stop 3
      if (lbound(var%A, 1) /= 1 .or. ubound(var%A, 1) /= 4) stop 4
      if (lbound(var2%A, 1) /= 1 .or. ubound(var2%A, 1) /= 5) stop 5
      var%A = [1,2,3,4]
      var2%A = [11,22,33,44,55]
    !$omp end target
    !$omp target exit data map(from: var, var2)

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%A)) error stop
    if (.not. allocated(Var2%A)) error stop
    if (lbound(var%A, 1) /= 1 .or. ubound(var%A, 1) /= 4) error stop
    if (lbound(var2%A, 1) /= 1 .or. ubound(var2%A, 1) /= 5) error stop
    if (any(var%A /= [1,2,3,4])) error stop
    if (any(var2%A /= [11,22,33,44,55])) error stop
  end subroutine test_alloc

  subroutine test2_alloc()
    type(t2) :: var
    type(t2), allocatable :: var2

    allocate(var2)
    allocate(var%x, var2%x)

    !$omp target enter data map(alloc: var, var2)
    !$omp target
      if (.not. allocated(Var2)) stop 6
      if (.not. allocated(Var%x)) stop 7
      if (.not. allocated(Var2%x)) stop 8
      var%x = 42
      var2%x = 43
    !$omp end target
    !$omp target exit data map(from: var, var2)

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%x)) error stop
    if (.not. allocated(Var2%x)) error stop
    if (var%x /= 42) error stop
    if (var2%x /= 43) error stop

    allocate(var%vt, var2%vt)
    allocate(var%vt%A(-1:3), var2%vt%A(0:4))

    !$omp target enter data map(alloc: var, var2)
    !$omp target
      if (.not. allocated(Var2)) stop 11
      if (.not. allocated(Var%x)) stop 12
      if (.not. allocated(Var2%x)) stop 13
      if (.not. allocated(Var%vt)) stop 14
      if (.not. allocated(Var2%vt)) stop 15
      if (.not. allocated(Var%vt%a)) stop 16
      if (.not. allocated(Var2%vt%a)) stop 17
      var%x = 42
      var2%x = 43
      if (lbound(var%vt%A, 1) /= -1 .or. ubound(var%vt%A, 1) /= 3) stop 4
      if (lbound(var2%vt%A, 1) /= 0 .or. ubound(var2%vt%A, 1) /= 4) stop 5
      var%vt%A = [1,2,3,4,5]
      var2%vt%A = [11,22,33,44,55]
    !$omp end target
    !$omp target exit data map(from: var, var2)

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%x)) error stop
    if (.not. allocated(Var2%x)) error stop
    if (.not. allocated(Var%vt)) error stop
    if (.not. allocated(Var2%vt)) error stop
    if (.not. allocated(Var%vt%a)) error stop
    if (.not. allocated(Var2%vt%a)) error stop
    if (var%x /= 42) error stop
    if (var2%x /= 43) error stop
    if (lbound(var%vt%A, 1) /= -1 .or. ubound(var%vt%A, 1) /= 3) error stop
    if (lbound(var2%vt%A, 1) /= 0 .or. ubound(var2%vt%A, 1) /= 4) error stop
    if (any(var%vt%A /= [1,2,3,4,5])) error stop
    if (any(var2%vt%A /= [11,22,33,44,55])) error stop
  end subroutine test2_alloc


  subroutine test_alloc_target()
    type(t) :: var
    type(t), allocatable :: var2

    allocate(var2)
    allocate(var%A(4), var2%A(5))

    !$omp target map(alloc: var, var2)
      if (.not. allocated(Var2)) stop 1
      if (.not. allocated(Var%A)) stop 2
      if (.not. allocated(Var2%A)) stop 3
      if (lbound(var%A, 1) /= 1 .or. ubound(var%A, 1) /= 4) stop 4
      if (lbound(var2%A, 1) /= 1 .or. ubound(var2%A, 1) /= 5) stop 5
      var%A = [1,2,3,4]
      var2%A = [11,22,33,44,55]
    !$omp end target

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%A)) error stop
    if (.not. allocated(Var2%A)) error stop
    if (lbound(var%A, 1) /= 1 .or. ubound(var%A, 1) /= 4) error stop
    if (lbound(var2%A, 1) /= 1 .or. ubound(var2%A, 1) /= 5) error stop
  end subroutine test_alloc_target

  subroutine test2_alloc_target()
    type(t2) :: var
    type(t2), allocatable :: var2

    allocate(var2)
    allocate(var%x, var2%x)

    !$omp target map(alloc: var, var2)
      if (.not. allocated(Var2)) stop 6
      if (.not. allocated(Var%x)) stop 7
      if (.not. allocated(Var2%x)) stop 8
      var%x = 42
      var2%x = 43
    !$omp end target

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%x)) error stop
    if (.not. allocated(Var2%x)) error stop

    allocate(var%vt, var2%vt)
    allocate(var%vt%A(-1:3), var2%vt%A(0:4))

    !$omp target map(alloc: var, var2)
      if (.not. allocated(Var2)) stop 11
      if (.not. allocated(Var%x)) stop 12
      if (.not. allocated(Var2%x)) stop 13
      if (.not. allocated(Var%vt)) stop 14
      if (.not. allocated(Var2%vt)) stop 15
      if (.not. allocated(Var%vt%a)) stop 16
      if (.not. allocated(Var2%vt%a)) stop 17
      var%x = 42
      var2%x = 43
      if (lbound(var%vt%A, 1) /= -1 .or. ubound(var%vt%A, 1) /= 3) stop 4
      if (lbound(var2%vt%A, 1) /= 0 .or. ubound(var2%vt%A, 1) /= 4) stop 5
      var%vt%A = [1,2,3,4,5]
      var2%vt%A = [11,22,33,44,55]
    !$omp end target

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%x)) error stop
    if (.not. allocated(Var2%x)) error stop
    if (.not. allocated(Var%vt)) error stop
    if (.not. allocated(Var2%vt)) error stop
    if (.not. allocated(Var%vt%a)) error stop
    if (.not. allocated(Var2%vt%a)) error stop
    if (lbound(var%vt%A, 1) /= -1 .or. ubound(var%vt%A, 1) /= 3) error stop
    if (lbound(var2%vt%A, 1) /= 0 .or. ubound(var2%vt%A, 1) /= 4) error stop
  end subroutine test2_alloc_target



  subroutine test_from()
    type(t) :: var
    type(t), allocatable :: var2

    allocate(var2)
    allocate(var%A(4), var2%A(5))

    !$omp target map(from: var, var2)
      if (.not. allocated(Var2)) stop 1
      if (.not. allocated(Var%A)) stop 2
      if (.not. allocated(Var2%A)) stop 3
      if (lbound(var%A, 1) /= 1 .or. ubound(var%A, 1) /= 4) stop 4
      if (lbound(var2%A, 1) /= 1 .or. ubound(var2%A, 1) /= 5) stop 5
      var%A = [1,2,3,4]
      var2%A = [11,22,33,44,55]
    !$omp end target

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%A)) error stop
    if (.not. allocated(Var2%A)) error stop
    if (lbound(var%A, 1) /= 1 .or. ubound(var%A, 1) /= 4) error stop
    if (lbound(var2%A, 1) /= 1 .or. ubound(var2%A, 1) /= 5) error stop
    if (any(var%A /= [1,2,3,4])) error stop
    if (any(var2%A /= [11,22,33,44,55])) error stop
  end subroutine test_from

  subroutine test2_from()
    type(t2) :: var
    type(t2), allocatable :: var2

    allocate(var2)
    allocate(var%x, var2%x)

    !$omp target map(from: var, var2)
      if (.not. allocated(Var2)) stop 6
      if (.not. allocated(Var%x)) stop 7
      if (.not. allocated(Var2%x)) stop 8
      var%x = 42
      var2%x = 43
    !$omp end target

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%x)) error stop
    if (.not. allocated(Var2%x)) error stop
    if (var%x /= 42) error stop
    if (var2%x /= 43) error stop

    allocate(var%vt, var2%vt)
    allocate(var%vt%A(-1:3), var2%vt%A(0:4))

    !$omp target map(from: var, var2)
      if (.not. allocated(Var2)) stop 11
      if (.not. allocated(Var%x)) stop 12
      if (.not. allocated(Var2%x)) stop 13
      if (.not. allocated(Var%vt)) stop 14
      if (.not. allocated(Var2%vt)) stop 15
      if (.not. allocated(Var%vt%a)) stop 16
      if (.not. allocated(Var2%vt%a)) stop 17
      var%x = 42
      var2%x = 43
      if (lbound(var%vt%A, 1) /= -1 .or. ubound(var%vt%A, 1) /= 3) stop 4
      if (lbound(var2%vt%A, 1) /= 0 .or. ubound(var2%vt%A, 1) /= 4) stop 5
      var%vt%A = [1,2,3,4,5]
      var2%vt%A = [11,22,33,44,55]
    !$omp end target

    if (.not. allocated(Var2)) error stop
    if (.not. allocated(Var%x)) error stop
    if (.not. allocated(Var2%x)) error stop
    if (.not. allocated(Var%vt)) error stop
    if (.not. allocated(Var2%vt)) error stop
    if (.not. allocated(Var%vt%a)) error stop
    if (.not. allocated(Var2%vt%a)) error stop
    if (var%x /= 42) error stop
    if (var2%x /= 43) error stop
    if (lbound(var%vt%A, 1) /= -1 .or. ubound(var%vt%A, 1) /= 3) error stop
    if (lbound(var2%vt%A, 1) /= 0 .or. ubound(var2%vt%A, 1) /= 4) error stop
    if (any(var%vt%A /= [1,2,3,4,5])) error stop
    if (any(var2%vt%A /= [11,22,33,44,55])) error stop
  end subroutine test2_from

end module m

use m
  implicit none (type, external)
  call test_alloc
  call test2_alloc
  call test_alloc_target
  call test2_alloc_target

  call test_from
  call test2_from
end
