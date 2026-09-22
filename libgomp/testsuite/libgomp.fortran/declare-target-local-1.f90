! { dg-do run { target offload_device } }
! { dg-additional-options "-Wno-deprecated-openmp" }
module m
  implicit none (type, external)

  integer :: local_var = 900
  !$omp declare target local(local_var)

  integer :: local_cblk_var = 800
  common /cblk/ local_cblk_var
  !$omp declare target local(/cblk/)

contains
  integer function incr_local_var (x)
    !$omp declare target
    integer :: x
    local_var = local_var + x
    incr_local_var = local_var
  end function

  integer function incr_local_cblk_var (x)
    !$omp declare target
    integer :: x
    local_cblk_var = local_cblk_var + x
    incr_local_cblk_var = local_cblk_var
  end function

end module m

program main
  use m
  implicit none (type, external)
  integer :: e

  ! Variables declared in the 'local' clause should not be corresponding
  ! storage between host/device. Test if 'target update' has no effect
  ! and host-side 'local_var' retains original value.  */

  !$omp target map(from : e)
  e = incr_local_var (23)
  !$omp end target

  !$omp target update from(local_var)
  if (local_var == e) stop 1
  if (local_var /= 900) stop 2

  !$omp target map(from : e)
  e = incr_local_cblk_var (68)
  !$omp end target

  !$omp target update from(local_cblk_var)
  if (local_cblk_var == e) stop 3
  if (local_cblk_var /= 800) stop 4

end program main
