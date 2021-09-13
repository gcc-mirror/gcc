! { dg-additional-options "-fdump-tree-original" }

! PR fortran/98476

program abc
  implicit none
  integer a, b

  a = 83
  b = 73
  call test(a, b)

contains
  subroutine test(aa, bb)
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
    integer :: aa, bb
    integer, target :: cc, dd
    type(c_ptr) :: pcc, pdd
    cc = 131
    dd = 484

    !$omp target enter data map(to: aa, bb, cc, dd)

    !$omp target data use_device_ptr(aa, cc) use_device_addr(bb, dd)
      pcc = c_loc(cc)
      pdd = c_loc(dd)

      ! TODO: has_device_addr(cc, dd)
      !$omp target is_device_ptr(aa, bb)
        if (aa /= 83 .or. bb /= 73) stop 1
        aa = 42
        bb = 43
        block
          integer, pointer :: c2, d2
          call c_f_pointer(pcc, c2)
          call c_f_pointer(pdd, d2)
          if (c2 /= 131 .or. d2 /= 484) stop 2
          c2 = 44
          d2 = 45
        end block
      !$omp end target
    !$omp end target data

    !$omp target exit data map(from:aa, bb, cc, dd)

    if (aa /= 42 .or. bb /= 43) stop 3
    if (cc /= 44 .or. dd /= 45) stop 5
  endsubroutine
end program

! { dg-final { scan-tree-dump-times "omp target data .*use_device_addr\\(aa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp target data .*use_device_addr\\(bb\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp target data .*use_device_addr\\(cc\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp target data .*use_device_addr\\(dd\\)" 1 "original" } }
