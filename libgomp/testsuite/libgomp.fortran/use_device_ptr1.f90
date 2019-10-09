module offloading
  use iso_c_binding
  implicit none
contains
  subroutine copy3_array_data_int(from, to, N)
    !$omp declare target
    type(c_ptr), value :: from, to
    integer, value :: N

    real(c_double), pointer :: from_ptr(:)
    real(c_double), pointer :: to_ptr(:)
    integer :: i

    call c_f_pointer(from, from_ptr, shape=[N])
    call c_f_pointer(to, to_ptr, shape=[N])
    !$omp parallel do
    do i = 1, N
      to_ptr(i) = 3 * from_ptr(i)
    end do
    !$omp end parallel do
  end subroutine copy3_array_data_int

  subroutine copy3_array_data(from, to, N)
    type(c_ptr), value :: from, to
    integer, value :: N
    !$omp target is_device_ptr(from, to)
    call copy3_array_data_int(from, to, N)
    !$omp end target
  end subroutine copy3_array_data

  subroutine copy3_array1(from, to)
    real(c_double), target :: from(:), to(:)
    integer :: N
    N = size(from)

    !$omp target is_device_ptr(from, to)
    call copy3_array_data_int(c_loc(from), c_loc(to), N)
    !$omp end target
  end subroutine copy3_array1

! ICE - the following code gives (currently) an ICE
! It is accepted by the frontend but it is invalid
! OpenMP 5 as only "a dummy argument that does not have the
! ALLOCATABLE, POINTER or VALUE attribute."
!
!  subroutine copy3_array2(from, to)
!    real(c_double), pointer :: from(:), to(:)
!    integer :: N
!    N = size(from)
!
!    !$omp target is_device_ptr(from, to)
!    call copy3_array_data_int(c_loc(from), c_loc(to), N)
!    !$omp end target
!  end subroutine copy3_array2

  subroutine copy3_array3(from, to)
    real(c_double), optional, target :: from(:), to(:)
    integer :: N
    N = size(from)

    !$omp target is_device_ptr(from, to)
    call copy3_array_data_int(c_loc(from), c_loc(to), N)
    !$omp end target
  end subroutine copy3_array3

! ICE - the following code gives (currently) an ICE
! It is accepted by the frontend but it is invalid
! OpenMP 5 as only "a dummy argument that does not have the
! ALLOCATABLE, POINTER or VALUE attribute."
!
!  subroutine copy3_array4(from, to)
!    real(c_double), optional, pointer :: from(:), to(:)
!    integer :: N
!    N = size(from)
!
!    !$omp target is_device_ptr(from, to)
!    call copy3_array_data_int(c_loc(from), c_loc(to), N)
!    !$omp end target
!  end subroutine copy3_array4
end module offloading



module offloading2
  use iso_c_binding
  use offloading
  implicit none
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

    ! NOTE: OpenMP 5's use_device_addr is (at time of writing) not yet supported

    ! pointer-type array to use_device_ptr
    !$omp target data map(to:AA) map(from:BB)
    !$omp target data map(alloc:dummy) use_device_ptr(AA,BB)
    call copy3_array_data(c_loc(AA), c_loc(BB), N)
    !$omp end target data
    !$omp end target data

    if (any(abs(AA - 11.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    ! allocatable array to use_device_ptr
    !$omp target data map(to:CC) map(from:DD)
    !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
    call copy3_array_data(c_loc(CC), c_loc(DD), N)
    !$omp end target data
    !$omp end target data

    if (any(abs(CC - 33.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()

    ! fixed-size decriptorless array to use_device_ptr
    !$omp target data map(to:EE) map(from:FF)
    !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
    call copy3_array_data(c_loc(EE), c_loc(FF), N)
    !$omp end target data
    !$omp end target data

    if (any(abs(EE - 55.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()



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

    call copy3_array_data(tgt_aptr, tgt_bptr, N)
    !$omp target update from(BB)
    if (any(abs(AA - 111.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    AA = 1111.0_c_double
    !$omp target update to(AA)
    call copy3_array_data(tgt_aptr, tgt_bptr, N)
    !$omp target update from(BB)
    if (any(abs(AA - 1111.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    ! AprtA tests
    AA = 7.0_c_double
    !$omp target update to(AA)
    call copy3_array_data(c_loc(AptrA), c_loc(BptrB), N)
    !$omp target update from(BB)
    if (any(abs(AA - 7.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    AA = 77.0_c_double
    !$omp target update to(AA)
    call copy3_array1(AptrA, BptrB)
    !$omp target update from(BB)
    if (any(abs(AA - 77.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

!    AA = 777.0_c_double
!    !$omp target update to(AA)
!    call copy3_array2(AptrA, BptrB)
!    !$omp target update from(BB)
!    if (any(abs(AA - 777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
!    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    AA = 7777.0_c_double
    !$omp target update to(AA)
    call copy3_array3(AptrA, BptrB)
    !$omp target update from(BB)
    if (any(abs(AA - 7777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

!    AA = 77777.0_c_double
!    !$omp target update to(AA)
!    call copy3_array4(AptrA, BptrB)
!    !$omp target update from(BB)
    !$omp end target data
!
!    if (any(abs(AA - 77777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
!    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()



    ! allocatable array to use_device_ptr
    !$omp target data map(to:CC) map(from:DD)
    !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
    tgt_cptr = c_loc(CC)
    tgt_dptr = c_loc(DD)
    !$omp end target data

    call copy3_array_data(tgt_cptr, tgt_dptr, N)
    !$omp target update from(DD)
    if (any(abs(CC - 333.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()

    CC = 3333.0_c_double
    !$omp target update to(CC)
    call copy3_array_data(tgt_cptr, tgt_dptr, N)
    !$omp target update from(DD)
    !$omp end target data

    if (any(abs(CC - 3333.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()



    ! fixed-size decriptorless array to use_device_ptr
    !$omp target data map(to:EE) map(from:FF)
    !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
    tgt_eptr = c_loc(EE)
    tgt_fptr = c_loc(FF)
    !$omp end target data

    call copy3_array_data(tgt_eptr, tgt_fptr, N)
    !$omp target update from(FF)
    if (any(abs(EE - 555.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()

    EE = 5555.0_c_double
    !$omp target update to(EE)
    call copy3_array_data(tgt_eptr, tgt_fptr, N)
    !$omp target update from(FF)
    !$omp end target data

    if (any(abs(EE - 5555.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()
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

    ! NOTE: OpenMP 5's use_device_addr is (at time of writing) not yet supported

    ! pointer-type array to use_device_ptr
    !$omp target data map(to:AA) map(from:BB)
    !$omp target data map(alloc:dummy) use_device_ptr(AA,BB)
    call copy3_array_data(c_loc(AA), c_loc(BB), N)
    !$omp end target data
    !$omp end target data

    if (any(abs(AA - 11.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    ! allocatable array to use_device_ptr
    !$omp target data map(to:CC) map(from:DD)
    !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
    call copy3_array_data(c_loc(CC), c_loc(DD), N)
    !$omp end target data
    !$omp end target data

    if (any(abs(CC - 33.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()

    ! fixed-size decriptorless array to use_device_ptr
    !$omp target data map(to:EE) map(from:FF)
    !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
    call copy3_array_data(c_loc(EE), c_loc(FF), N)
    !$omp end target data
    !$omp end target data

    if (any(abs(EE - 55.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()



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

    call copy3_array_data(tgt_aptr, tgt_bptr, N)
    !$omp target update from(BB)
    if (any(abs(AA - 111.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    AA = 1111.0_c_double
    !$omp target update to(AA)
    call copy3_array_data(tgt_aptr, tgt_bptr, N)
    !$omp target update from(BB)
    if (any(abs(AA - 1111.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    ! AprtA tests
    AA = 7.0_c_double
    !$omp target update to(AA)
    call copy3_array_data(c_loc(AptrA), c_loc(BptrB), N)
    !$omp target update from(BB)
    if (any(abs(AA - 7.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    AA = 77.0_c_double
    !$omp target update to(AA)
    call copy3_array1(AptrA, BptrB)
    !$omp target update from(BB)
    if (any(abs(AA - 77.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

!    AA = 777.0_c_double
!    !$omp target update to(AA)
!    call copy3_array2(AptrA, BptrB)
!    !$omp target update from(BB)
!    if (any(abs(AA - 777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
!    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

    AA = 7777.0_c_double
    !$omp target update to(AA)
    call copy3_array3(AptrA, BptrB)
    !$omp target update from(BB)
    if (any(abs(AA - 7777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

!    AA = 77777.0_c_double
!    !$omp target update to(AA)
!    call copy3_array4(AptrA, BptrB)
!    !$omp target update from(BB)
    !$omp end target data
!
!    if (any(abs(AA - 77777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
!    if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()



    ! allocatable array to use_device_ptr
    !$omp target data map(to:CC) map(from:DD)
    !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
    tgt_cptr = c_loc(CC)
    tgt_dptr = c_loc(DD)
    !$omp end target data

    call copy3_array_data(tgt_cptr, tgt_dptr, N)
    !$omp target update from(DD)
    if (any(abs(CC - 333.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()

    CC = 3333.0_c_double
    !$omp target update to(CC)
    call copy3_array_data(tgt_cptr, tgt_dptr, N)
    !$omp target update from(DD)
    !$omp end target data

    if (any(abs(CC - 3333.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
    if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()



    ! fixed-size decriptorless array to use_device_ptr
    !$omp target data map(to:EE) map(from:FF)
    !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
    tgt_eptr = c_loc(EE)
    tgt_fptr = c_loc(FF)
    !$omp end target data

    call copy3_array_data(tgt_eptr, tgt_fptr, N)
    !$omp target update from(FF)
    if (any(abs(EE - 555.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()

    EE = 5555.0_c_double
    !$omp target update to(EE)
    call copy3_array_data(tgt_eptr, tgt_fptr, N)
    !$omp end target data

    if (any(abs(EE - 5555.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
    if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()
  end subroutine use_device_ptr_sub2
end module offloading2



program omp_device_ptr
  use iso_c_binding
  use offloading
  use offloading2
  implicit none

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

  ! NOTE: OpenMP 5's use_device_addr is (at time of writing) not yet supported

  ! pointer-type array to use_device_ptr
  !$omp target data map(to:AA) map(from:BB)
  !$omp target data map(alloc:dummy) use_device_ptr(AA,BB)
  call copy3_array_data(c_loc(AA), c_loc(BB), N)
  !$omp end target data
  !$omp end target data

  if (any(abs(AA - 11.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

  ! allocatable array to use_device_ptr
  !$omp target data map(to:CC) map(from:DD)
  !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
  call copy3_array_data(c_loc(CC), c_loc(DD), N)
  !$omp end target data
  !$omp end target data

  if (any(abs(CC - 33.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
  if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()

  ! fixed-size decriptorless array to use_device_ptr
  !$omp target data map(to:EE) map(from:FF)
  !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
  call copy3_array_data(c_loc(EE), c_loc(FF), N)
  !$omp end target data
  !$omp end target data

  if (any(abs(EE - 55.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
  if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()



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

  call copy3_array_data(tgt_aptr, tgt_bptr, N)
  !$omp target update from(BB)
  if (any(abs(AA - 111.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

  AA = 1111.0_c_double
  !$omp target update to(AA)
  call copy3_array_data(tgt_aptr, tgt_bptr, N)
  !$omp target update from(BB)
  if (any(abs(AA - 1111.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

  ! AprtA tests
  AA = 7.0_c_double
  !$omp target update to(AA)
  call copy3_array_data(c_loc(AptrA), c_loc(BptrB), N)
  !$omp target update from(BB)
  if (any(abs(AA - 7.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

  AA = 77.0_c_double
  !$omp target update to(AA)
  call copy3_array1(AptrA, BptrB)
  !$omp target update from(BB)
  if (any(abs(AA - 77.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

!  AA = 777.0_c_double
!  !$omp target update to(AA)
!  call copy3_array2(AptrA, BptrB)
!  !$omp target update from(BB)
!  if (any(abs(AA - 777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
!  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

  AA = 7777.0_c_double
  !$omp target update to(AA)
  call copy3_array3(AptrA, BptrB)
  !$omp target update from(BB)
  if (any(abs(AA - 7777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()

!  AA = 77777.0_c_double
!  !$omp target update to(AA)
!  call copy3_array4(AptrA, BptrB)
!  !$omp target update from(BB)
  !$omp end target data
!
!  if (any(abs(AA - 77777.0_c_double) > 10.0_c_double * epsilon(AA))) call abort()
!  if (any(abs(3.0_c_double * AA - BB) > 10.0_c_double * epsilon(AA))) call abort()



  ! allocatable array to use_device_ptr
  !$omp target data map(to:CC) map(from:DD)
  !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
  tgt_cptr = c_loc(CC)
  tgt_dptr = c_loc(DD)
  !$omp end target data

  call copy3_array_data(tgt_cptr, tgt_dptr, N)
  !$omp target update from(DD)
  if (any(abs(CC - 333.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
  if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()

  CC = 3333.0_c_double
  !$omp target update to(CC)
  call copy3_array_data(tgt_cptr, tgt_dptr, N)
  !$omp target update from(DD)
  !$omp end target data

  if (any(abs(CC - 3333.0_c_double) > 10.0_c_double * epsilon(CC))) call abort()
  if (any(abs(3.0_c_double * CC - DD) > 10.0_c_double * epsilon(CC))) call abort()



  ! fixed-size decriptorless array to use_device_ptr
  !$omp target data map(to:EE) map(from:FF)
  !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
  tgt_eptr = c_loc(EE)
  tgt_fptr = c_loc(FF)
  !$omp end target data

  call copy3_array_data(tgt_eptr, tgt_fptr, N)
  !$omp target update from(FF)
  if (any(abs(EE - 555.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
  if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()

  EE = 5555.0_c_double
  !$omp target update to(EE)
  call copy3_array_data(tgt_eptr, tgt_fptr, N)
  !$omp target update from(FF)
  !$omp end target data

  if (any(abs(EE - 5555.0_c_double) > 10.0_c_double * epsilon(EE))) call abort()
  if (any(abs(3.0_c_double * EE - FF) > 10.0_c_double * epsilon(EE))) call abort()



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
