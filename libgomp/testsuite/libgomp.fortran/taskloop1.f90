! { dg-do run }
  common /blk/ q, e
  integer :: q, r
  logical :: e
!$omp parallel
!$omp single
  call foo (2, 7)
  r = bar (12, 18)
!$omp end single
!$omp end parallel
  if (q .ne. 6 .or. r .ne. 17 .or. e) stop 1
contains
  subroutine foo (a, b)
    integer, intent (in) :: a, b
    common /blk/ q, e
    integer :: q, r, d
    logical :: e
!$omp taskloop lastprivate (q) nogroup
    do d = a, b, 2
      q = d
      if (d < 2 .or. d > 6 .or. iand (d, 1) .ne. 0) then
!$omp atomic write
        e = .true.
      end if
    end do
  end subroutine foo
  function bar (a, b)
    integer, intent (in) :: a, b
    integer :: bar
    common /blk/ q, e
    integer :: q, r, d, s
    logical :: e
    s = 7
!$omp taskloop lastprivate (s)
    do d = a, b - 1
      if (d < 12 .or. d > 17) then
!$omp atomic write
        e = .true.
      end if
      s = d
    end do
!$omp end taskloop
    bar = s
  end function bar
end
