program teams2
  use omp_lib
  integer :: i, j, err
  err = 0
!$omp teams reduction(+:err)
  err = err + bar (0, 0, 0)
!$omp end teams
  if (err .ne. 0) stop 1
!$omp teams reduction(+:err)
  err = err + bar (1, 0, 0)
!$omp end teams
  if (err .ne. 0) stop 2
!$omp teams reduction(+:err)
!$omp distribute
  do i = 0, 63
    err = err + bar (2, i, 0)
  end do
!$omp end teams
  if (err .ne. 0) stop 3
!$omp teams reduction(+:err)
!$omp distribute
  do i = 0, 63
!$omp parallel do reduction(+:err)
    do j = 0, 31
      err = err + bar (3, i, j)
    end do
  end do
!$omp end teams
  if (err .ne. 0) stop 4
contains
  subroutine foo (x, y, z, a, b)
    integer :: x, y, z, a, b(64), i, j
    if (x .eq. 0) then
      do i = 0, 63
!$omp parallel do shared (a, b)
        do j = 0, 31
	  call foo (3, i, j, a, b)
	end do
      end do
    else if (x .eq. 1) then
!$omp distribute dist_schedule (static, 1)
      do i = 0, 63
!$omp parallel do shared (a, b)
	do j = 0, 31
	  call foo (3, i, j, a, b)
	end do
      end do
    else if (x .eq. 2) then
!$omp parallel do shared (a, b)
      do j = 0, 31
	call foo (3, y, j, a, b)
      end do
    else
!$omp atomic
      b(y + 1) = b(y + 1) + z
!$omp end atomic
!$omp atomic
      a = a + 1
!$omp end atomic
    end if
  end subroutine

  integer function bar (x, y, z)
    use omp_lib
    integer :: x, y, z, a, b(64), i, c, d, e, f
    a = 8
    do i = 0, 63
      b(i + 1) = i
    end do
    call foo (x, y, z, a, b)
    if (x .eq. 0) then
      if (a .ne. 8 + 64 * 32) then
        bar = 1
        return
      end if
      do i = 0, 63
	if (b(i + 1) .ne. i + 31 * 32 / 2) then
	  bar = 1
	  return
	end if
      end do
    else if (x .eq. 1) then
      c = omp_get_num_teams ()
      d = omp_get_team_num ()
      e = d
      f = 0
      do i = 0, 63
	if (i .eq. e) then
          if (b(i + 1) .ne. i + 31 * 32 / 2) then
            bar = 1
            return
          end if
          f = f + 1
          e = e + c
	else if (b(i + 1) .ne. i) then
	  bar = 1
	  return
	end if
      end do
      if (a .lt. 8 .or. a > 8 + f * 32) then
        bar = 1
        return
      end if
    else if (x .eq. 2) then
      if (a .ne. 8 + 32) then
        bar = 1
        return
      end if
      do i = 0, 63
        if (i .eq. y) then
          c = 31 * 32 / 2
        else
          c = 0
        end if
	if (b(i + 1) .ne. i + c) then
	  bar = 1
	  return
	end if
      end do
    else if (x .eq. 3) then
      if (a .ne. 8 + 1) then
        bar = 1
        return
      end if
      do i = 0, 63
        if (i .eq. y) then
          c = z
        else
          c = 0
        end if
        if (b (i + 1) .ne. i + c) then
          bar = 1
          return
        end if
      end do
    end if
    bar = 0
    return
  end function
end program
