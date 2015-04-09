! PR fortran/65597
! { dg-do run }

  integer :: i, a(151)
  a(:) = 0
  !$omp do simd
    do i = 1, 151, 31
      a(i) = a(i) + 1
    end do
  !$omp do simd linear (i: 31)
    do i = 1, 151, 31
      a(i) = a(i) + 1
    end do
  do i = 1, 151
    if (mod (i, 31) .eq. 1) then
      if (a(i) .ne. 2) call abort
    else
      if (a(i) .ne. 0) call abort
    end if
  end do
end
