module procedures
  use iso_c_binding, only: c_ptr, c_f_pointer
  use omp_lib
  implicit none

  contains

  function foo(bv, av, n) result(res)
    implicit none
    integer :: res, n, i
    type(c_ptr) :: bv
    type(c_ptr) :: av
    real(8), pointer :: fp_bv(:), fp_av(:)  ! Fortran pointers for array access
    !$omp declare variant(bar) match(construct={dispatch}) adjust_args(need_device_ptr: bv, av)
    !$omp declare variant(baz) match(implementation={vendor(gnu)})

    ! Associate C pointers with Fortran pointers
    call c_f_pointer(bv, fp_bv, [n])
    call c_f_pointer(av, fp_av, [n])

    ! Perform operations using Fortran pointers
    do i = 1, n
      fp_bv(i) = fp_av(i) * i
    end do
    res = -1
  end function foo

  function baz(d_bv, d_av, n) result(res)
    implicit none
    integer :: res, n, i
    type(c_ptr) :: d_bv
    type(c_ptr) :: d_av
    real(8), pointer :: fp_bv(:), fp_av(:)  ! Fortran pointers for array access
  
    ! Associate C pointers with Fortran pointers
    call c_f_pointer(d_bv, fp_bv, [n])
    call c_f_pointer(d_av, fp_av, [n])

    !$omp distribute parallel do
    do i = 1, n
      fp_bv(i) = fp_av(i) * i
    end do
    res = -3
  end function baz

  function bar(d_bv, d_av, n) result(res)
    implicit none
    integer :: res, n, i
    type(c_ptr) :: d_bv
    type(c_ptr) :: d_av

    !$omp target is_device_ptr(d_bv, d_av)
    block
      real(8), pointer :: fp_bv(:), fp_av(:)  ! Fortran pointers for array access

      ! Associate C pointers with Fortran pointers
      call c_f_pointer(d_bv, fp_bv, [n])
      call c_f_pointer(d_av, fp_av, [n])

      ! Perform operations on target
      do i = 1, n
        fp_bv(i) = fp_av(i) * i
      end do
    end block

    res = -2
  end function bar

  function test(n) result(res)
    use iso_c_binding, only: c_ptr, c_loc
    implicit none
    integer :: n, res, i, f, ff, last_dev
    real(8), allocatable, target :: av(:), bv(:), d_bv(:)
    real(8), parameter :: e = 2.71828d0
    type(c_ptr) :: c_av, c_bv, c_d_bv
    
    allocate(av(n), bv(n), d_bv(n))
    
    ! Initialize arrays
    do i = 1, n
      av(i) = e * i
      bv(i) = 0.0d0
      d_bv(i) = 0.0d0
    end do

    last_dev = omp_get_num_devices() - 1
    
    c_av = c_loc(av)
    c_d_bv = c_loc(d_bv)
    !$omp target data map(to: av(:n)) map(from: d_bv(:n)) device(last_dev) if(n == 1024)
      !$omp dispatch nocontext(n > 1024) novariants(n < 1024) device(last_dev)
      f = foo(c_d_bv, c_av, n)
    !$omp end target data
    
    c_bv = c_loc(bv)
    ff = foo(c_bv, c_loc(av), n)
    
    ! Verify results
    do i = 1, n
      if (d_bv(i) /= bv(i)) then
        write(0,*) 'ERROR at ', i, ': ', d_bv(i), ' (act) != ', bv(i), ' (exp)'
        res = 1
        return
      end if
    end do
    
    res = f
    deallocate(av, bv, d_bv)
  end function test
end module procedures

program main
  use procedures
  implicit none
  integer :: ret
  
  ret = test(1023)
  if (ret /= -1) stop 1
  
  ret = test(1024)
  if (ret /= -2) stop 1
  
  ret = test(1025)
  if (ret /= -3) stop 1
end program main
