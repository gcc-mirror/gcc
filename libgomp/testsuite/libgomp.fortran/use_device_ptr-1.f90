! { dg-do run }

module target_procs
  use iso_c_binding
  implicit none (type, external)
  private
  public :: copy3_array, copy3_scalar, copy3_array1, copy3_array3
contains
  subroutine copy3_array_int(from_ptr, to_ptr, N)
    !$omp declare target
    real(c_double) :: from_ptr(:)
    real(c_double) :: to_ptr(:)
    integer, value :: N
    integer :: i

    !$omp parallel do
    do i = 1, N
      to_ptr(i) = 3 * from_ptr(i)
    end do
    !$omp end parallel do
  end subroutine copy3_array_int

  subroutine copy3_scalar_int(from, to)
    !$omp declare target
    real(c_double) :: from, to

    to = 3 * from
  end subroutine copy3_scalar_int


  subroutine copy3_array(from, to, N)
    type(c_ptr), value :: from, to
    integer, value :: N
    real(c_double), pointer :: from_ptr(:), to_ptr(:)

    call c_f_pointer(from, from_ptr, shape=[N])
    call c_f_pointer(to, to_ptr, shape=[N])

    call do_offload_scalar(from_ptr,to_ptr)
  contains
    subroutine do_offload_scalar(from_r, to_r)
      real(c_double), target :: from_r(:), to_r(:)
      ! The extra function is needed as is_device_ptr
      ! requires non-value, non-pointer dummy arguments

      !$omp target is_device_ptr(from_r, to_r)
      call copy3_array_int(from_r, to_r, N)
      !$omp end target
    end subroutine do_offload_scalar
  end subroutine copy3_array

  subroutine copy3_scalar(from, to)
    type(c_ptr), value, target :: from, to
    real(c_double), pointer :: from_ptr(:), to_ptr(:)

    ! Standard-conform detour of using an array as at time of writing
    ! is_device_ptr below does not handle scalars
    call c_f_pointer(from, from_ptr, shape=[1])
    call c_f_pointer(to, to_ptr, shape=[1])

    call do_offload_scalar(from_ptr,to_ptr)
  contains
    subroutine do_offload_scalar(from_r, to_r)
      real(c_double), target :: from_r(:), to_r(:)
      ! The extra function is needed as is_device_ptr
      ! requires non-value, non-pointer dummy arguments

      !$omp target is_device_ptr(from_r, to_r)
      call copy3_scalar_int(from_r(1), to_r(1))
      !$omp end target
    end subroutine do_offload_scalar
  end subroutine copy3_scalar

  subroutine copy3_array1(from, to)
    real(c_double), target :: from(:), to(:)
    integer :: N
    N = size(from)

    !!$omp target is_device_ptr(from, to)
    call copy3_array(c_loc(from), c_loc(to), N)
    !!$omp end target
  end subroutine copy3_array1

  subroutine copy3_array3(from, to)
    real(c_double), optional, target :: from(:), to(:)
    integer :: N
    N = size(from)

!    !$omp target is_device_ptr(from, to)
    call copy3_array(c_loc(from), c_loc(to), N)
!    !$omp end target
  end subroutine copy3_array3
end module target_procs



module offloading2
  use iso_c_binding
  use target_procs
  implicit none (type, external)
contains
  ! Same as main program but uses dummy *nonoptional* arguments
  subroutine use_device_ptr_sub(AA, BB, CC, DD, EE, FF, AptrA, BptrB, N)
    real(c_double), pointer :: AA(:), BB(:)
    real(c_double), allocatable, target :: CC(:), DD(:)
    real(c_double), target :: EE(N), FF(N), dummy(1)
    real(c_double), pointer :: AptrA(:), BptrB(:)
    intent(inout) :: AA, BB, CC, DD, EE, FF
    integer, value :: N

    type(c_ptr) :: tgt_aptr, tgt_bptr, tgt_cptr, tgt_dptr, tgt_eptr, tgt_fptr

    AA = 11.0_c_double
    BB = 22.0_c_double
    CC = 33.0_c_double
    DD = 44.0_c_double
    EE = 55.0_c_double
    FF = 66.0_c_double

    ! pointer-type array to use_device_ptr
    !$omp target data map(to:AA) map(from:BB) use_device_ptr(AA,BB)
    call copy3_array(c_loc(AA), c_loc(BB), N)
    !$omp end target data

    if (any(abs(AA - 11.0_c_double) > 10.0_c_double * epsilon(AA))) stop 1
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 2

    ! allocatable array to use_device_ptr
    !$omp target data map(to:CC) map(from:DD) use_device_ptr(CC,DD)
    call copy3_array(c_loc(CC), c_loc(DD), N)
    !$omp end target data

    if (any(abs(CC - 33.0_c_double) > 10.0_c_double * epsilon(CC))) stop 3
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 4

    ! fixed-size decriptorless array to use_device_ptr
    !$omp target data map(to:EE) map(from:FF) use_device_ptr(EE,FF)
    call copy3_array(c_loc(EE), c_loc(FF), N)
    !$omp end target data

    if (any(abs(EE - 55.0_c_double) > 10.0_c_double * epsilon(EE))) stop 5
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 6



    AA = 111.0_c_double
    BB = 222.0_c_double
    CC = 333.0_c_double
    DD = 444.0_c_double
    EE = 555.0_c_double
    FF = 666.0_c_double

    ! pointer-type array to use_device_ptr
    !$omp target data map(to:AA) map(from:BB)
    !$omp target data map(alloc:dummy) use_device_ptr(AA,BB)
    tgt_aptr = c_loc(AA)
    tgt_bptr = c_loc(BB)
    AptrA => AA
    BptrB => BB
    !$omp end target data

    call copy3_array(tgt_aptr, tgt_bptr, N)
    !$omp target update from(BB)
    if (any(abs(AA - 111.0_c_double) > 10.0_c_double * epsilon(AA))) stop 7
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 8

    AA = 1111.0_c_double
    !$omp target update to(AA)
    call copy3_array(tgt_aptr, tgt_bptr, N)
    !$omp target update from(BB)
    if (any(abs(AA - 1111.0_c_double) > 10.0_c_double * epsilon(AA))) stop 9
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 10

    ! AprtA tests
    AA = 7.0_c_double
    !$omp target update to(AA)
    call copy3_array(c_loc(AptrA), c_loc(BptrB), N)
    !$omp target update from(BB)
    if (any(abs(AA - 7.0_c_double) > 10.0_c_double * epsilon(AA))) stop 11
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 12

    AA = 77.0_c_double
    !$omp target update to(AA)
    call copy3_array1(AptrA, BptrB)
    !$omp target update from(BB)
    if (any(abs(AA - 77.0_c_double) > 10.0_c_double * epsilon(AA))) stop 13
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 14

!    AA = 777.0_c_double
!    !$omp target update to(AA)
!    call copy3_array2(AptrA, BptrB)
!    !$omp target update from(BB)
!    if (any(abs(AA - 777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 15
!    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 16

    AA = 7777.0_c_double
    !$omp target update to(AA)
    call copy3_array3(AptrA, BptrB)
    !$omp target update from(BB)
    if (any(abs(AA - 7777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 17
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 18

!    AA = 77777.0_c_double
!    !$omp target update to(AA)
!    call copy3_array4(AptrA, BptrB)
!    !$omp target update from(BB)
    !$omp end target data
!
!    if (any(abs(AA - 77777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 19
!    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 20



    ! allocatable array to use_device_ptr
    !$omp target data map(to:CC) map(from:DD)
    !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
    tgt_cptr = c_loc(CC)
    tgt_dptr = c_loc(DD)
    !$omp end target data

    call copy3_array(tgt_cptr, tgt_dptr, N)
    !$omp target update from(DD)
    if (any(abs(CC - 333.0_c_double) > 10.0_c_double * epsilon(CC))) stop 21
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 22

    CC = 3333.0_c_double
    !$omp target update to(CC)
    call copy3_array(tgt_cptr, tgt_dptr, N)
    !$omp target update from(DD)
    !$omp end target data

    if (any(abs(CC - 3333.0_c_double) > 10.0_c_double * epsilon(CC))) stop 23
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 24



    ! fixed-size decriptorless array to use_device_ptr
    !$omp target data map(to:EE) map(from:FF)
    !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
    tgt_eptr = c_loc(EE)
    tgt_fptr = c_loc(FF)
    !$omp end target data

    call copy3_array(tgt_eptr, tgt_fptr, N)
    !$omp target update from(FF)
    if (any(abs(EE - 555.0_c_double) > 10.0_c_double * epsilon(EE))) stop 25
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 26

    EE = 5555.0_c_double
    !$omp target update to(EE)
    call copy3_array(tgt_eptr, tgt_fptr, N)
    !$omp target update from(FF)
    !$omp end target data

    if (any(abs(EE - 5555.0_c_double) > 10.0_c_double * epsilon(EE))) stop 27
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 28
  end subroutine use_device_ptr_sub



  ! Same as main program but uses dummy *optional* arguments
  subroutine use_device_ptr_sub2(AA, BB, CC, DD, EE, FF, AptrA, BptrB, N)
    real(c_double), optional, pointer :: AA(:), BB(:)
    real(c_double), optional, allocatable, target :: CC(:), DD(:)
    real(c_double), optional, target :: EE(N), FF(N)
    real(c_double), pointer :: AptrA(:), BptrB(:)
    intent(inout) :: AA, BB, CC, DD, EE, FF
    real(c_double), target :: dummy(1)
    integer, value :: N

    type(c_ptr) :: tgt_aptr, tgt_bptr, tgt_cptr, tgt_dptr, tgt_eptr, tgt_fptr

    AA = 11.0_c_double
    BB = 22.0_c_double
    CC = 33.0_c_double
    DD = 44.0_c_double
    EE = 55.0_c_double
    FF = 66.0_c_double

    ! pointer-type array to use_device_ptr
    !$omp target data map(to:AA) map(from:BB) use_device_ptr(AA,BB)
    call copy3_array(c_loc(AA), c_loc(BB), N)
    !$omp end target data

    if (any(abs(AA - 11.0_c_double) > 10.0_c_double * epsilon(AA))) stop 29
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 30

    ! allocatable array to use_device_ptr
    !$omp target data map(to:CC) map(from:DD) use_device_ptr(CC,DD)
    call copy3_array(c_loc(CC), c_loc(DD), N)
    !$omp end target data

    if (any(abs(CC - 33.0_c_double) > 10.0_c_double * epsilon(CC))) stop 31
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 32

    ! fixed-size decriptorless array to use_device_ptr
    !$omp target data map(to:EE) map(from:FF) use_device_ptr(EE,FF)
    call copy3_array(c_loc(EE), c_loc(FF), N)
    !$omp end target data

    if (any(abs(EE - 55.0_c_double) > 10.0_c_double * epsilon(EE))) stop 33
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 34



    AA = 111.0_c_double
    BB = 222.0_c_double
    CC = 333.0_c_double
    DD = 444.0_c_double
    EE = 555.0_c_double
    FF = 666.0_c_double

    ! pointer-type array to use_device_ptr
    !$omp target data map(to:AA) map(from:BB)
    !$omp target data map(alloc:dummy) use_device_ptr(AA,BB)
    tgt_aptr = c_loc(AA)
    tgt_bptr = c_loc(BB)
    AptrA => AA
    BptrB => BB
    !$omp end target data

    call copy3_array(tgt_aptr, tgt_bptr, N)
    !$omp target update from(BB)
    if (any(abs(AA - 111.0_c_double) > 10.0_c_double * epsilon(AA))) stop 35
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 36

    AA = 1111.0_c_double
    !$omp target update to(AA)
    call copy3_array(tgt_aptr, tgt_bptr, N)
    !$omp target update from(BB)
    if (any(abs(AA - 1111.0_c_double) > 10.0_c_double * epsilon(AA))) stop 37
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 38

    ! AprtA tests
    AA = 7.0_c_double
    !$omp target update to(AA)
    call copy3_array(c_loc(AptrA), c_loc(BptrB), N)
    !$omp target update from(BB)
    if (any(abs(AA - 7.0_c_double) > 10.0_c_double * epsilon(AA))) stop 39
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 40

    AA = 77.0_c_double
    !$omp target update to(AA)
    call copy3_array1(AptrA, BptrB)
    !$omp target update from(BB)
    if (any(abs(AA - 77.0_c_double) > 10.0_c_double * epsilon(AA))) stop 41
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 42

!    AA = 777.0_c_double
!    !$omp target update to(AA)
!    call copy3_array2(AptrA, BptrB)
!    !$omp target update from(BB)
!    if (any(abs(AA - 777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 43
!    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 44

    AA = 7777.0_c_double
    !$omp target update to(AA)
    call copy3_array3(AptrA, BptrB)
    !$omp target update from(BB)
    if (any(abs(AA - 7777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 45
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 46

!    AA = 77777.0_c_double
!    !$omp target update to(AA)
!    call copy3_array4(AptrA, BptrB)
!    !$omp target update from(BB)
    !$omp end target data
!
!    if (any(abs(AA - 77777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 47
!    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 48



    ! allocatable array to use_device_ptr
    !$omp target data map(to:CC) map(from:DD)
    !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
    tgt_cptr = c_loc(CC)
    tgt_dptr = c_loc(DD)
    !$omp end target data

    call copy3_array(tgt_cptr, tgt_dptr, N)
    !$omp target update from(DD)
    if (any(abs(CC - 333.0_c_double) > 10.0_c_double * epsilon(CC))) stop 49
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 50

    CC = 3333.0_c_double
    !$omp target update to(CC)
    call copy3_array(tgt_cptr, tgt_dptr, N)
    !$omp target update from(DD)
    !$omp end target data

    if (any(abs(CC - 3333.0_c_double) > 10.0_c_double * epsilon(CC))) stop 51
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 52



    ! fixed-size decriptorless array to use_device_ptr
    !$omp target data map(to:EE) map(from:FF)
    !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
    tgt_eptr = c_loc(EE)
    tgt_fptr = c_loc(FF)
    !$omp end target data

    call copy3_array(tgt_eptr, tgt_fptr, N)
    !$omp target update from(FF)
    if (any(abs(EE - 555.0_c_double) > 10.0_c_double * epsilon(EE))) stop 53
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 54

    EE = 5555.0_c_double
    !$omp target update to(EE)
    call copy3_array(tgt_eptr, tgt_fptr, N)
    !$omp end target data

    if (any(abs(EE - 5555.0_c_double) > 10.0_c_double * epsilon(EE))) stop 55
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 56
  end subroutine use_device_ptr_sub2
end module offloading2



program omp_device_ptr
  use iso_c_binding
  use target_procs
  use offloading2
  implicit none (type, external)

  integer, parameter :: N = 1000
  real(c_double), pointer :: AA(:), BB(:), arg_AA(:), arg_BB(:), arg2_AA(:), arg2_BB(:)
  real(c_double), allocatable, target :: CC(:), DD(:), arg_CC(:), arg_DD(:), arg2_CC(:), arg2_DD(:)
  real(c_double), target :: EE(N), FF(N), dummy(1), arg_EE(N), arg_FF(N), arg2_EE(N), arg2_FF(N)

  real(c_double), pointer :: AptrA(:), BptrB(:)
  type(c_ptr) :: tgt_aptr, tgt_bptr, tgt_cptr, tgt_dptr, tgt_eptr, tgt_fptr

  allocate(AA(N), BB(N), CC(N), DD(N))

  AA = 11.0_c_double
  BB = 22.0_c_double
  CC = 33.0_c_double
  DD = 44.0_c_double
  EE = 55.0_c_double
  FF = 66.0_c_double

  ! pointer-type array to use_device_ptr
  !$omp target data map(to:AA) map(from:BB) use_device_ptr(AA,BB)
  call copy3_array(c_loc(AA), c_loc(BB), N)
  !$omp end target data

  if (any(abs(AA - 11.0_c_double) > 10.0_c_double * epsilon(AA))) stop 57
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 58

  ! allocatable array to use_device_ptr
  !$omp target data map(to:CC) map(from:DD) use_device_ptr(CC,DD)
  call copy3_array(c_loc(CC), c_loc(DD), N)
  !$omp end target data

  if (any(abs(CC - 33.0_c_double) > 10.0_c_double * epsilon(CC))) stop 59
  if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 60

  ! fixed-size decriptorless array to use_device_ptr
  !$omp target data map(to:EE) map(from:FF) use_device_ptr(EE,FF)
  call copy3_array(c_loc(EE), c_loc(FF), N)
  !$omp end target data

  if (any(abs(EE - 55.0_c_double) > 10.0_c_double * epsilon(EE))) stop 61
  if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 62



  AA = 111.0_c_double
  BB = 222.0_c_double
  CC = 333.0_c_double
  DD = 444.0_c_double
  EE = 555.0_c_double
  FF = 666.0_c_double

  ! pointer-type array to use_device_ptr
  !$omp target data map(to:AA) map(from:BB)
  !$omp target data map(alloc:dummy) use_device_ptr(AA,BB)
  tgt_aptr = c_loc(AA)
  tgt_bptr = c_loc(BB)
  AptrA => AA
  BptrB => BB
  !$omp end target data

  call copy3_array(tgt_aptr, tgt_bptr, N)
  !$omp target update from(BB)
  if (any(abs(AA - 111.0_c_double) > 10.0_c_double * epsilon(AA))) stop 63
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 64

  AA = 1111.0_c_double
  !$omp target update to(AA)
  call copy3_array(tgt_aptr, tgt_bptr, N)
  !$omp target update from(BB)
  if (any(abs(AA - 1111.0_c_double) > 10.0_c_double * epsilon(AA))) stop 65
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 66

  ! AprtA tests
  AA = 7.0_c_double
  !$omp target update to(AA)
  call copy3_array(c_loc(AptrA), c_loc(BptrB), N)
  !$omp target update from(BB)
  if (any(abs(AA - 7.0_c_double) > 10.0_c_double * epsilon(AA))) stop 67
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 68

  AA = 77.0_c_double
  !$omp target update to(AA)
  call copy3_array1(AptrA, BptrB)
  !$omp target update from(BB)
  if (any(abs(AA - 77.0_c_double) > 10.0_c_double * epsilon(AA))) stop 69
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 70

!  AA = 777.0_c_double
!  !$omp target update to(AA)
!  call copy3_array2(AptrA, BptrB)
!  !$omp target update from(BB)
!  if (any(abs(AA - 777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 71
!  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 72

  AA = 7777.0_c_double
  !$omp target update to(AA)
  call copy3_array3(AptrA, BptrB)
  !$omp target update from(BB)
  if (any(abs(AA - 7777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 73
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 74

!  AA = 77777.0_c_double
!  !$omp target update to(AA)
!  call copy3_array4(AptrA, BptrB)
!  !$omp target update from(BB)
  !$omp end target data
!
!  if (any(abs(AA - 77777.0_c_double) > 10.0_c_double * epsilon(AA))) stop 75
!  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) stop 76



  ! allocatable array to use_device_ptr
  !$omp target data map(to:CC) map(from:DD)
  !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
  tgt_cptr = c_loc(CC)
  tgt_dptr = c_loc(DD)
  !$omp end target data

  call copy3_array(tgt_cptr, tgt_dptr, N)
  !$omp target update from(DD)
  if (any(abs(CC - 333.0_c_double) > 10.0_c_double * epsilon(CC))) stop 77
  if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 78

  CC = 3333.0_c_double
  !$omp target update to(CC)
  call copy3_array(tgt_cptr, tgt_dptr, N)
  !$omp target update from(DD)
  !$omp end target data

  if (any(abs(CC - 3333.0_c_double) > 10.0_c_double * epsilon(CC))) stop 79
  if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) stop 80



  ! fixed-size decriptorless array to use_device_ptr
  !$omp target data map(to:EE) map(from:FF)
  !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
  tgt_eptr = c_loc(EE)
  tgt_fptr = c_loc(FF)
  !$omp end target data

  call copy3_array(tgt_eptr, tgt_fptr, N)
  !$omp target update from(FF)
  if (any(abs(EE - 555.0_c_double) > 10.0_c_double * epsilon(EE))) stop 81
  if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 82

  EE = 5555.0_c_double
  !$omp target update to(EE)
  call copy3_array(tgt_eptr, tgt_fptr, N)
  !$omp target update from(FF)
  !$omp end target data

  if (any(abs(EE - 5555.0_c_double) > 10.0_c_double * epsilon(EE))) stop 83
  if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) stop 84



  deallocate(AA, BB)  ! Free pointers only

  AptrA => null()
  BptrB => null()
  allocate(arg_AA(N), arg_BB(N), arg_CC(N), arg_DD(N))
  call use_device_ptr_sub(arg_AA, arg_BB, arg_CC, arg_DD, arg_EE, arg_FF, AptrA, BptrB, N)
  deallocate(arg_AA, arg_BB)

  AptrA => null()
  BptrB => null()
  allocate(arg2_AA(N), arg2_BB(N), arg2_CC(N), arg2_DD(N))
  call use_device_ptr_sub2(arg2_AA, arg2_BB, arg2_CC, arg2_DD, arg2_EE, arg2_FF, AptrA, BptrB, N)
  deallocate(arg2_AA, arg2_BB)
end program omp_device_ptr
