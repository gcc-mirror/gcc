! Minimal check whether HIP works - by checking whether the API routines
! seem to work.  This includes a  fallback if hipfort is not available

#ifndef HAVE_HIPFORT
#ifndef USE_HIP_FALLBACK_MODULE
#if USE_CUDA_NAMES
#warning "Using fallback implementation for module hipfort as HAVE_HIPFORT is undefined (for NVIDA/CUDA)"
#else
#warning "Using fallback implementation for module hipfort as HAVE_HIPFORT is undefined - assume AMD as USE_CUDA_NAMES is unset"
#endif
#endif
module hipfort  ! Minimal implementation for the testsuite
  implicit none

  enum, bind(c)
    enumerator :: hipSuccess = 0
    enumerator :: hipErrorNotSupported = 801
  end enum

  enum, bind(c)
    enumerator :: hipDeviceAttributeClockRate = 5
    enumerator :: hipDeviceAttributeMaxGridDimX = 29
  end enum

  interface
    integer(kind(hipSuccess)) function hipDeviceGetAttribute (ip, attr, dev) &
#if USE_CUDA_NAMES
        bind(c, name="cudaDeviceGetAttribute")
#else
        bind(c, name="hipDeviceGetAttribute")
#endif
      use iso_c_binding, only: c_ptr, c_int
      import
      implicit none
      type(c_ptr), value :: ip
      integer(kind(hipDeviceAttributeClockRate)), value :: attr
      integer(c_int), value :: dev
    end

    integer(kind(hipSuccess)) function hipCtxGetApiVersion (ctx, ip) &
#if USE_CUDA_NAMES
        bind(c, name="cudaCtxGetApiVersion")
#else
        bind(c, name="hipCtxGetApiVersion")
#endif
      use iso_c_binding, only: c_ptr
      import
      implicit none
      type(c_ptr), value :: ctx, ip
    end

    integer(kind(hipSuccess)) function hipStreamQuery (stream) &
#if USE_CUDA_NAMES
        bind(c, name="cudaStreamQuery")
#else
        bind(c, name="hipStreamQuery")
#endif
      use iso_c_binding, only: c_ptr
      import
      implicit none
      type(c_ptr), value :: stream
    end

   integer(kind(hipSuccess)) function hipStreamGetFlags (stream, flags) &
#if USE_CUDA_NAMES
        bind(c, name="cudaStreamGetFlags")
#else
        bind(c, name="hipStreamGetFlags")
#endif
      use iso_c_binding, only: c_ptr
      import
      implicit none
      type(c_ptr), value :: stream
      type(c_ptr), value :: flags
    end
  end interface
end module
#endif

program main
  use iso_c_binding, only: c_ptr, c_int, c_loc
  use omp_lib
  use hipfort
  implicit none (type, external)

! Only supported since CUDA 12.8 - skip for better compatibility
!  ! Manally implement hipStreamGetDevice as hipfort misses it
!  ! -> https://github.com/ROCm/hipfort/issues/238
!  interface
!    integer(kind(hipSuccess)) function my_hipStreamGetDevice(stream, dev) &
!#if USE_CUDA_NAMES
!        bind(c, name="cudaStreamGetDevice")
!#else
!        bind(c, name="hipStreamGetDevice")
!#endif
!      use iso_c_binding, only: c_ptr, c_int
!      import
!      implicit none
!      type(c_ptr), value :: stream
!      integer(c_int) :: dev
!    end
!  end interface

  integer(c_int), target :: ivar
  integer(omp_interop_rc_kind) :: res
  integer(omp_interop_kind) :: obj
  integer(omp_interop_fr_kind) :: fr
  integer(kind(hipSuccess)) :: hip_err
  integer(c_int) :: hip_dev, dev_stream
  type(c_ptr) :: hip_ctx, hip_sm

  logical :: vendor_is_amd

  obj = omp_interop_none

  !$omp interop init(target, targetsync, prefer_type("hip") : obj)

  fr = omp_get_interop_int (obj, omp_ipr_fr_id, res)
  if (res /= omp_irc_success) error stop 1
  if (fr /= omp_ifr_hip) error stop 1

  ivar = omp_get_interop_int (obj, omp_ipr_vendor, res)
  if (ivar == 1) then  ! AMD
    vendor_is_amd = .true.
  else if (ivar == 11) then  ! Nvidia
    vendor_is_amd = .false.
  else
    error stop 1  ! Unknown
  endif
#if USE_CUDA_NAMES
  if (vendor_is_amd) error stop 1
#else
  if (.not. vendor_is_amd) error stop 1
#endif

  ! Check whether the omp_ipr_device -> hipDevice_t yields a valid device.

  hip_dev = omp_get_interop_int (obj, omp_ipr_device, res)
  if (res /= omp_irc_success) error stop 1

! AMD messed up in Fortran with the attribute handling, missing the
! translation table it has for C.
block
  enum, bind(c)
    enumerator :: cudaDevAttrClockRate = 13
    enumerator :: cudaDevAttrMaxGridDimX = 5
  end enum

  ! Assume a clock size is available and > 1 GHz; value is in kHz.
  ! c_loc is completely bogus, but as AMD messed up the interface ...
  ! Cf. https://github.com/ROCm/hipfort/issues/239
if (vendor_is_amd) then
  hip_err = hipDeviceGetAttribute (c_loc(ivar), hipDeviceAttributeClockRate, hip_dev)
else
  hip_err = hipDeviceGetAttribute (c_loc(ivar), cudaDevAttrClockRate, hip_dev)
endif
  if (hip_err /= hipSuccess) error stop 1
  if (ivar <= 1000000) error stop 1  ! in kHz

  ! Assume that the MaxGridDimX is available and > 1024
  ! c_loc is completely bogus, but as AMD messed up the interface ...
  ! Cf. https://github.com/ROCm/hipfort/issues/239
if (vendor_is_amd) then
  hip_err = hipDeviceGetAttribute (c_loc(ivar), hipDeviceAttributeMaxGridDimX, hip_dev)
else
  hip_err = hipDeviceGetAttribute (c_loc(ivar), cudaDevAttrMaxGridDimX, hip_dev)
endif
  if (hip_err /= hipSuccess) error stop 1
  if (ivar <= 1024) error stop 1
end block


  ! Check whether the omp_ipr_device_context -> hipCtx_t yields a context.

  hip_ctx = omp_get_interop_ptr (obj, omp_ipr_device_context, res)
  if (res /= omp_irc_success) error stop 1

!  ! Assume API Version > 0 for Nvidia, hipErrorNotSupported for AMD.  */
!  ivar = -99
!  ! AMD deprectated hipCtxGetApiVersion (in C/C++)
!  hip_err = hipCtxGetApiVersion (hip_ctx, c_loc(ivar))
!
!  if (vendor_is_amd) then
!    if (hip_err /= hipErrorNotSupported .or. ivar /= -99) error stop 1
!  else
!    if (hip_err /= hipSuccess) error stop 1
!    if (ivar <= 0) error stop 1
!  end if


  ! Check whether the omp_ipr_targetsync -> hipStream_t yields a stream.

  hip_sm = omp_get_interop_ptr (obj, omp_ipr_targetsync, res)
  if (res /= omp_irc_success) error stop 1

! Skip as this is only in CUDA 12.8
!  dev_stream = 99
!    ! Not (yet) implemented: https://github.com/ROCm/hipfort/issues/238
!    !  hip_err = hipStreamGetDevice (hip_sm, dev_stream)
!  hip_err = my_hipStreamGetDevice (hip_sm, dev_stream)
!  if (hip_err /= hipSuccess) error stop 1
!  if (dev_stream /= hip_dev) error stop 1

  ! Get flags of the stream
  hip_err = hipStreamGetFlags (hip_sm, c_loc (ivar))
  if (hip_err /= hipSuccess) error stop 1
  ! Accept any value

  ! All jobs should have been completed (as there were none none)
  hip_err = hipStreamQuery (hip_sm)
  if (hip_err /= hipSuccess) error stop 1

  !$omp interop destroy(obj)
end
