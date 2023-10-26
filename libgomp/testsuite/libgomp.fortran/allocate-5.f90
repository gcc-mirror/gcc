! { dg-do compile }

module test
  integer, allocatable :: mvar1
  integer, allocatable :: mvar2
  integer, allocatable :: mvar3
end module

subroutine foo(x, y)
  use omp_lib
  implicit none
  integer  :: x
  integer  :: y
  
  integer, allocatable :: var1(:)
  integer, allocatable :: var2(:)
  integer, allocatable :: var3(:)
  integer, allocatable :: var4(:)
  integer, allocatable :: var5(:)
  integer, allocatable :: var6(:)
  integer, allocatable :: var7(:)
  integer, allocatable :: var8(:)
  integer, allocatable :: var9(:)

  x = 1 ! executable statement before '!$omp allocate'

  ! Don't use a hard-coded value (..., but it does pass the checks).
  !$omp allocate (var1) allocator(10_omp_allocator_handle_kind) ! { dg-bogus "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
  allocate (var1(x))

  ! Assumption is that 'omp_allocator_handle_kind' ('c_intptr_t') isn't 1.
  !$omp allocate (var1) allocator(10_1) ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind at .1." }
  allocate (var1(x))

  !$omp allocate (var2)  ! { dg-error "'var2' specified in 'allocate' at .1. but not in the associated ALLOCATE statement" }
  allocate (var3(x))  ! { dg-error "'var3' listed in 'allocate' statement at .1. but it is neither explicitly in listed in the '!.OMP ALLOCATE' directive nor exists a directive without argument list" }

  !$omp allocate (x)
  x = 2  ! { dg-error "Unexpected assignment at .1.; expected ALLOCATE or !.OMP ALLOCATE statement" }

  !$omp allocate (var4)
  y = 2 ! { dg-error "Unexpected assignment at .1.; expected ALLOCATE or !.OMP ALLOCATE statement" }

  !$omp allocate (var5)
  !$omp allocate
  allocate (var5(x))

  !$omp allocate (var6)
  !$omp allocate (var7)  ! { dg-error "'var7' specified in 'allocate' at .1. but not in the associated ALLOCATE statement" }
  !$omp allocate (var8)  ! { dg-error "'var8' specified in 'allocate' at .1. but not in the associated ALLOCATE statement" }
  allocate (var6(x))

  !$omp allocate (var9)
  !$omp allocate (var9)  ! { dg-warning "var9' appears more than once in 'allocate'" }
  allocate (var9(x))

end subroutine

function outer(a)
  IMPLICIT NONE

  integer :: outer, a
  integer, allocatable :: var1

  outer = inner(a) + 5
  return

  contains

    integer function inner(x)
    integer :: x
    integer, allocatable :: var2

    x = 1 ! executable statement before '!$omp allocate'

    !$omp allocate (var1, var2)  ! { dg-error "Sorry, allocation of allocatable 'var1' with '!.omp allocators' or '!.omp allocate' at .1. is only suppored in the scope where it has been declared, unless it has the SAVE attribute" }
    allocate (var1, var2)

    inner = x + 10
    return
    end function inner

end function outer

subroutine bar(s)
  use omp_lib
  use test
  integer  :: s
  integer, save, allocatable :: svar1
  integer, save, allocatable :: svar2
  integer, save, allocatable :: svar3

  type (omp_alloctrait) :: traits(3)
  integer (omp_allocator_handle_kind) :: a

  traits = [omp_alloctrait (omp_atk_alignment, 64), &
            omp_alloctrait (omp_atk_fallback, omp_atv_null_fb), &
            omp_alloctrait (omp_atk_pool_size, 8192)]
  a = omp_init_allocator (omp_default_mem_space, 3, traits)
  if (a == omp_null_allocator) stop 1

  !$omp allocate (mvar1) allocator(a)
  allocate (mvar1)

  !$omp allocate (mvar2)
  allocate (mvar2)

  !$omp allocate (mvar3) allocator(omp_low_lat_mem_alloc)
  allocate (mvar3)

  !$omp allocate (svar1)  allocator(a)
  allocate (svar1)

  !$omp allocate (svar2)
  allocate (svar2)

  !$omp allocate (svar3) allocator(omp_low_lat_mem_alloc)
  allocate (svar3)
end subroutine
