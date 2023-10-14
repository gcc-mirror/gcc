! { dg-additional-options "-fdump-tree-omplower" }

! For the 4 vars in omp_parallel, 4 in omp_target and 2 in no_alloc2_func.
! { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(" 10 "omplower" } } 
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(" 10 "omplower" } }

module m
  use iso_c_binding
  use omp_lib
  implicit none (type, external)
  integer(c_intptr_t) :: intptr

contains

subroutine check_int (x, y)
  integer :: x, y
  value :: y
  if (x /= y) &
    stop 1
end

subroutine check_ptr (x, y)
  type(c_ptr) :: x
  integer(c_intptr_t), value :: y
  if (transfer(x,intptr) /= y) &
    stop 2
end

integer function no_alloc_func () result(res)
  ! There is no __builtin_GOMP_alloc / __builtin_GOMP_free as
  ! allocator == omp_default_mem_alloc (known at compile time.
  integer :: no_alloc
  !$omp allocate(no_alloc) allocator(omp_default_mem_alloc)
  no_alloc = 7
  res = no_alloc
end

integer function no_alloc2_func() result(res)
  ! If no_alloc2 were TREE_UNUSED, there would be no
  ! __builtin_GOMP_alloc / __builtin_GOMP_free
  ! However, as the parser already marks no_alloc2
  ! and is_alloc2 as used, the tree is generated for both vars.
  integer :: no_alloc2, is_alloc2
  !$omp allocate(no_alloc2, is_alloc2)
  is_alloc2 = 7
  res = is_alloc2
end


subroutine omp_parallel ()
  integer :: i, n, iii, jjj(5)
  type(c_ptr) :: ptr
  !$omp allocate(iii, jjj, ptr)
  n = 6
  iii = 5
  ptr = transfer (int(z'1234', c_intptr_t), ptr)
 block
  integer :: kkk(n)
  !$omp allocate(kkk)

  do i = 1, 5
    jjj(i) = 3*i
  end do
  do i = 1, 6
    kkk(i) = 7*i
  end do

  !$omp parallel default(none) firstprivate(iii, jjj, kkk, ptr) if(.false.)
    if (iii /= 5) &
      stop 3
    iii = 7
    call check_int (iii, 7)
    do i = 1, 5
      if (jjj(i) /= 3*i) &
        stop 4
    end do
    do i = 1, 6
      if (kkk(i) /= 7*i) &
        stop 5
    end do
    do i = 1, 5
      jjj(i) = 4*i
    end do
    do i = 1, 6
      kkk(i) = 8*i
    end do
    do i = 1, 5
      call check_int (jjj(i), 4*i)
    end do
    do i = 1, 6
      call check_int (kkk(i), 8*i)
    end do
    if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
      stop 6
    ptr = transfer (int(z'abcd', c_intptr_t), ptr)
    if (transfer (ptr, intptr) /= int(z'abcd', c_intptr_t)) &
      stop 7
    call check_ptr (ptr,  int(z'abcd', c_intptr_t))
  !$omp end parallel

  if (iii /= 5) &
    stop 8
  call check_int (iii, 5)
  do i = 1, 5
    if (jjj(i) /= 3*i) &
      stop 9
    call check_int (jjj(i), 3*i)
  end do
  do i = 1, 6
    if (kkk(i) /= 7*i) &
      stop 10
    call check_int (kkk(i), 7*i)
  end do
  if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
    stop 11
  call check_ptr (ptr, int(z'1234', c_intptr_t))

  !$omp parallel default(firstprivate) if(.false.)
    if (iii /= 5) &
      stop 12
    iii = 7
    call check_int (iii, 7)
    do i = 1, 5
      if (jjj(i) /= 3*i) &
        stop 13
    end do
    do i = 1, 6
      if (kkk(i) /= 7*i) &
        stop 14
    end do
    do i = 1, 5
      jjj(i) = 4*i
    end do
    do i = 1, 6
      kkk(i) = 8*i
    end do
    do i = 1, 5
      call check_int (jjj(i), 4*i)
    end do
    do i = 1, 6
      call check_int (kkk(i), 8*i)
    end do
    if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
      stop 15
    ptr = transfer (int (z'abcd', c_intptr_t), ptr)
    if (transfer (ptr, intptr) /= int(z'abcd', c_intptr_t)) &
      stop 16
    call check_ptr (ptr, int (z'abcd', c_intptr_t))
  !$omp end parallel
  if (iii /= 5) &
    stop 17
  call check_int (iii, 5)
  do i = 1, 5
    if (jjj(i) /= 3*i) &
      stop 18
    call check_int (jjj(i), 3*i)
  end do
  do i = 1, 6
    if (kkk(i) /= 7*i) &
      stop 19
    call check_int (kkk(i), 7*i)
  end do
  if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
    stop 20
  call check_ptr (ptr, int (z'1234', c_intptr_t))
 end block
end

subroutine omp_target ()
  integer :: i, n, iii, jjj(5)
  type(c_ptr) :: ptr
  !$omp allocate(iii, jjj, ptr)
  n = 6
  iii = 5
  ptr = transfer (int (z'1234', c_intptr_t), ptr)
 block
  integer :: kkk(n)
  !$omp allocate(kkk)
  do i = 1, 5
    jjj(i) = 3*i
  end do
  do i = 1, 6
    kkk(i) = 7*i
  end do

  !$omp target defaultmap(none) firstprivate(iii, jjj, kkk, ptr) private(i)
    if (iii /= 5) &
      stop 21
    iii = 7
    call check_int (iii, 7)
    do i = 1, 5
      if (jjj(i) /= 3*i) &
        stop 22
    end do
    do i = 1, 6
      if (kkk(i) /= 7*i) &
        stop 23
    end do
    do i = 1, 5
      jjj(i) = 4*i
    end do
    do i = 1, 6
      kkk(i) = 8*i
    end do
    do i = 1, 5
      call check_int (jjj(i), 4*i)
    end do
    do i = 1, 6
      call check_int (kkk(i), 8*i)
    end do
    if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
      stop 24
    ptr = transfer (int (z'abcd', c_intptr_t), ptr)
    if (transfer (ptr, intptr) /= int(z'abcd', c_intptr_t)) &
      stop 25
    call check_ptr (ptr, int (z'abcd', c_intptr_t))
  !$omp end target

  if (iii /= 5) &
    stop 26
  call check_int (iii, 5)
  do i = 1, 5
    if (jjj(i) /= 3*i) &
      stop 27
    call check_int (jjj(i), 3*i)
  end do
  do i = 1, 6
    if (kkk(i) /= 7*i) &
      stop 28
    call check_int (kkk(i), 7*i)
  end do
  if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
    stop 29
  call check_ptr (ptr, int (z'1234', c_intptr_t))

  !$omp target defaultmap(firstprivate)
    if (iii /= 5) &
      stop 30
    iii = 7
    call check_int (iii, 7)
    do i = 1, 5
      if (jjj(i) /= 3*i) &
        stop 31
    end do
    do i = 1, 6
      if (kkk(i) /= 7*i) &
        stop 32
    end do
    do i = 1, 5
      jjj(i) = 4*i
    end do
    do i = 1, 6
      kkk(i) = 8*i
    end do
    do i = 1, 5
      call check_int (jjj(i), 4*i)
    end do
    do i = 1, 6
      call check_int (kkk(i), 8*i)
    end do
    if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
      stop 33
    ptr = transfer (int (z'abcd', c_intptr_t), ptr)
    if (transfer (ptr, intptr) /= int(z'abcd', c_intptr_t)) &
      stop 34
    call check_ptr (ptr, int (z'abcd', c_intptr_t))
  !$omp end target
  if (iii /= 5) &
    stop 35
  call check_int (iii, 5)
  do i = 1, 5
    if (jjj(i) /= 3*i) &
      stop 36
    call check_int (jjj(i), 3*i)
  end do
  do i = 1, 6
    if (kkk(i) /= 7*i) &
      stop 37
    call check_int (kkk(i), 7*i)
  end do
  if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
    stop 38
  call check_ptr (ptr, int (z'1234', c_intptr_t))

  !$omp target defaultmap(tofrom)
    if (iii /= 5) &
      stop 39
    iii = 7
    call check_int (iii, 7)
    do i = 1, 5
      if (jjj(i) /= 3*i) &
        stop 40
    end do
    do i = 1, 6
      if (kkk(i) /= 7*i) &
        stop 41
    end do
    do i = 1, 5
      jjj(i) = 4*i
    end do
    do i = 1, 6
      kkk(i) = 8*i
    end do
    do i = 1, 5
      call check_int (jjj(i), 4*i)
    end do
    do i = 1, 6
      call check_int (kkk(i), 8*i)
    end do
    if (transfer (ptr, intptr) /= int(z'1234', c_intptr_t)) &
      stop 42
    ptr = transfer (int(z'abcd',c_intptr_t), ptr)
    if (transfer (ptr, intptr) /= int(z'abcd', c_intptr_t)) &
      stop 43
    call check_ptr (ptr, int (z'abcd', c_intptr_t))
  !$omp end target

  if (iii /= 7) &
    stop 44
  call check_int (iii, 7)
  do i = 1, 5
    if (jjj(i) /= 4*i) &
      stop 45
    call check_int (jjj(i), 4*i)
  end do
  do i = 1, 6
    if (kkk(i) /= 8*i) &
      stop 46
    call check_int (kkk(i), 8*i)
  end do
  if (transfer (ptr, intptr) /= int(z'abcd', c_intptr_t)) &
    stop 47
  call check_ptr (ptr, int (z'abcd', c_intptr_t))
 end block
end
end module


use m
  call omp_parallel ()
  call omp_target ()
end
