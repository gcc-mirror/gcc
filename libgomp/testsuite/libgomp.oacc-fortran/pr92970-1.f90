! Verify that 'acc_delete' etc. on non-present data is a no-op.
!
! Fortran version to libgomp.oacc-c-c++-common/pr92970-1.c

program main
use openacc
implicit none (type, external)

integer :: a, b, async

! Side remark: 'sizeof' is a GNU extension;
! for standard conforming code, use c_sizeof or (in bits) storage_size.

async = 0
!$acc exit data copyout (a)
  call acc_copyout (a, sizeof (a))
!$acc exit data copyout (a) async (async)
  async = async + 1
  call acc_copyout_async (a, sizeof (a), async)
  async = async + 1
!$acc exit data copyout (a) finalize
  call acc_copyout_finalize (a, sizeof (a))
!$acc exit data copyout (a) finalize async (async)
  async = async + 1
  call acc_copyout_finalize_async (a, sizeof (a), async)
  async = async + 1

!$acc exit data delete (a)
  call acc_delete (a, sizeof (a))
!$acc exit data delete (a) async (async)
  async = async + 1
  call acc_delete_async (a, sizeof (a), async)
  async = async + 1
!$acc exit data delete (a) finalize
  call acc_delete_finalize (a, sizeof (a))
!$acc exit data delete (a) finalize async (async)
  async = async + 1
  call acc_delete_finalize_async (a, sizeof (a), async)
  async = async + 1


! Same but taking the byte size from the argument

!$acc exit data copyout (b)
  call acc_copyout (b)
!$acc exit data copyout (b) async (async)
  async = async + 1
  call acc_copyout_async (b, async)
  async = async + 1
!$acc exit data copyout (b) finalize
  call acc_copyout_finalize (b)
!$acc exit data copyout (b) finalize async (async)
  async = async + 1
  call acc_copyout_finalize_async (b, async)
  async = async + 1

!$acc exit data delete (b)
  call acc_delete (b)
!$acc exit data delete (b) async (async)
  async = async + 1
  call acc_delete_async (b, async)
  async = async + 1
!$acc exit data delete (b) finalize
  call acc_delete_finalize (b)
!$acc exit data delete (b) finalize async (async)
  async = async + 1
  call acc_delete_finalize_async (b, async)
  async = async + 1

  call acc_wait_all ()
end
