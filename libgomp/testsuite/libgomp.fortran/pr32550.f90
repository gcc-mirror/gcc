! PR fortran/32550
! { dg-do run }
! { dg-require-effective-target tls_runtime }

      integer, pointer, save :: ptr
      integer, target :: targ
      integer :: e
!$omp threadprivate(ptr)
      e = 0
      targ = 42
!$omp parallel shared(targ)
!$omp single
      ptr => targ
!$omp end single copyprivate(ptr)
      if (ptr.ne.42) then
!$omp atomic
	e = e + 1
      end if
!$omp end parallel
      if (e.ne.0) call abort
      end
