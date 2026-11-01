! { dg-do run }
! PR 127893
!
! Concurrent OPEN/CLOSE of units whose numbers share a unit-cache slot,
! mixed with I/O on long-lived and shared units.

program unit_cache_1
  use omp_lib
  implicit none
  integer, parameter :: niter = 500, nslot = 4, nthr = 8
  integer :: tid, k, u, nu, lu, v, w, ios, shared_u, nlines, errs
  character(len=16) :: str

  errs = 0
  call omp_set_num_threads (nthr)
  open (newunit=shared_u, status='scratch')

!$omp parallel private(tid,k,u,nu,lu,v,w,ios,str) reduction(+:errs)
  tid = omp_get_thread_num ()

  lu = 500 + tid
  open (lu, status='scratch', form='unformatted')
  write (lu) tid

  do k = 1, niter
    ! Unit numbers 64 apart.
    u = 1024 + 64 * (tid * nslot + mod (k, nslot))
    open (u, status='scratch', form='unformatted')
    write (u) k, tid
    rewind (u)
    read (u) v, w
    if (v /= k .or. w /= tid) errs = errs + 1
    close (u)

    open (newunit=nu, status='scratch')
    write (nu, '(2i8)') k, tid
    rewind (nu)
    read (nu, '(2i8)') v, w
    if (v /= k .or. w /= tid) errs = errs + 1
    close (nu)

    write (str, '(i0)') 3 * k + tid
    read (str, *) v
    if (v /= 3 * k + tid) errs = errs + 1

    rewind (lu)
    read (lu) v
    if (v /= tid) errs = errs + 1

    write (shared_u, '(i0,1x,i0)') tid, k
    inquire (unit=1024 + 64 * mod (k, nthr * nslot), iostat=ios)
  end do
  close (lu)
!$omp end parallel

  rewind (shared_u)
  nlines = 0
  do
    read (shared_u, *, iostat=ios) v, w
    if (ios /= 0) exit
    nlines = nlines + 1
  end do
  close (shared_u)

  if (errs /= 0) stop 1
  if (nlines /= nthr * niter) stop 2
end program unit_cache_1
