program main
  use omp_lib
  use ISO_C_Binding
  implicit none (external, type)

  interface
    ! omp_alloc + omp_free part of OpenMP for C/C++
    ! but not (yet) in the OpenMP spec for Fortran
    type(c_ptr) function omp_alloc (size, handle) bind(C)
      import
      integer (c_size_t), value :: size
      integer (omp_allocator_handle_kind), value :: handle
    end function

    subroutine omp_free (ptr, handle) bind(C)
      import
      type (c_ptr), value :: ptr
      integer (omp_allocator_handle_kind), value :: handle
    end subroutine
  end interface

  type (omp_alloctrait) :: traits(3)
  integer (omp_allocator_handle_kind) :: a

  traits = [omp_alloctrait (omp_atk_alignment, 64), &
            omp_alloctrait (omp_atk_fallback, omp_atv_null_fb), &
            omp_alloctrait (omp_atk_pool_size, 4096)]
  a = omp_init_allocator (omp_default_mem_space, 3, traits)
  if (a == omp_null_allocator) stop 1

  !$omp parallel num_threads(4)
  block
    integer :: n
    real(8) :: r
    type(c_ptr) :: cp, cq
    real(8), pointer, volatile :: p(:), q(:)
 
    n = omp_get_thread_num ()
    if (mod (n, 2) /= 0) then
      call omp_set_default_allocator (a)
    else
      call omp_set_default_allocator (omp_default_mem_alloc)
    endif
    cp = omp_alloc (1696_c_size_t, omp_null_allocator)
    if (.not. c_associated (cp)) stop 2
    call c_f_pointer (cp, p, [1696 / c_sizeof (r)])
    p(1) = 1.0
    p(1696 / c_sizeof (r)) = 2.0
    !$omp barrier
    if (mod (n, 2) /= 0) then
      call omp_set_default_allocator (omp_default_mem_alloc)
    else
      call omp_set_default_allocator (a)
    endif
    cq = omp_alloc (1696_c_size_t, omp_null_allocator)
    if (mod (n, 2) /= 0) then
      if (.not. c_associated (cq)) stop 3
      call c_f_pointer (cq, q, [1696 / c_sizeof (r)])
      q(1) = 3.0
      q(1696 / c_sizeof (r)) = 4.0
    else if (c_associated (cq)) then
      stop 4
    end if
    !$omp barrier
    call omp_free (cp, omp_null_allocator)
    call omp_free (cq, omp_null_allocator)
    call omp_set_default_allocator (omp_default_mem_alloc)
  end block
  !$omp end parallel
  call omp_destroy_allocator (a)
end program main
