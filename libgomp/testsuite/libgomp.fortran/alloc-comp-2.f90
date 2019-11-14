! { dg-do run }
! Don't cycle by default through all options, just test -O0 and -O2,
! as this is quite large test.
! { dg-skip-if "" { ! run_expensive_tests } { "*" } { "-O0" "-O2" } }

module m
  type dl
    integer :: a, b
    integer, allocatable :: c(:,:)
    integer :: d, e
    integer, allocatable :: f
  end type
  type dt
    integer :: g
    type (dl), allocatable :: h(:)
    integer :: i
    type (dl) :: j(2, 2)
    type (dl), allocatable :: k
  end type
contains
  subroutine ver_dl (obj, val, c, cl1, cu1, cl2, cu2, f)
    type (dl), intent (in) :: obj
    integer, intent (in) :: val, cl1, cu1, cl2, cu2
    logical, intent (in) :: c, f
    if ((c .neqv. allocated (obj%c)) .or. (f .neqv. allocated (obj%f))) stop 1
    if (c) then
      if (lbound (obj%c, 1) /= cl1 .or. ubound (obj%c, 1) /= cu1) stop 2
      if (lbound (obj%c, 2) /= cl2 .or. ubound (obj%c, 2) /= cu2) stop 3
    end if
    if (val /= 0) then
      if (obj%a /= val .or. obj%b /= val) stop 4
      if (obj%d /= val .or. obj%e /= val) stop 5
      if (c) then
        if (any (obj%c /= val)) stop 6
      end if
      if (f) then
        if (obj%f /= val) stop 7
      end if
    end if
  end subroutine ver_dl
  subroutine ver_dt (obj, val, h, hl, hu, k, c, cl1, cu1, cl2, cu2, f)
    type (dt), intent (in) :: obj
    integer, intent (in) :: val, hl, hu, cl1, cu1, cl2, cu2
    logical, intent (in) :: h, k, c, f
    integer :: i, j
    if ((h .neqv. allocated (obj%h)) .or. (k .neqv. allocated (obj%k))) stop 8
    if (h) then
      if (lbound (obj%h, 1) /= hl .or. ubound (obj%h, 1) /= hu) stop 9
      do i = hl, hu
        call ver_dl (obj%h(i), val, c, cl1, cu1, cl2, cu2, f)
      end do
    end if
    do i = 1, 2
      do j = 1, 2
        call ver_dl (obj%j(i, j), val, c, cl1, cu1, cl2, cu2, f)
      end do
    end do
    if (k) call ver_dl (obj%k, val, c, cl1, cu1, cl2, cu2, f)
    if (val /= 0) then
      if (obj%g /= val .or. obj%i /= val) stop 10
    end if
  end subroutine ver_dt
  subroutine alloc_dl (obj, val, c, cl1, cu1, cl2, cu2, f)
    type (dl), intent (inout) :: obj
    integer, intent (in) :: val, cl1, cu1, cl2, cu2
    logical, intent (in) :: c, f
    if (val /= 0) then
      obj%a = val
      obj%b = val
      obj%d = val
      obj%e = val
    end if
    if (allocated (obj%c)) deallocate (obj%c)
    if (c) then
      allocate (obj%c(cl1:cu1, cl2:cu2))
      if (val /= 0) obj%c = val
    end if
    if (f) then
      if (.not.allocated (obj%f)) allocate (obj%f)
      if (val /= 0) obj%f = val
    else
      if (allocated (obj%f)) deallocate (obj%f)
    end if
  end subroutine alloc_dl
  subroutine alloc_dt (obj, val, h, hl, hu, k, c, cl1, cu1, cl2, cu2, f)
    type (dt), intent (inout) :: obj
    integer, intent (in) :: val, hl, hu, cl1, cu1, cl2, cu2
    logical, intent (in) :: h, k, c, f
    integer :: i, j
    if (val /= 0) then
      obj%g = val
      obj%i = val
    end if
    if (allocated (obj%h)) deallocate (obj%h)
    if (h) then
      allocate (obj%h(hl:hu))
      do i = hl, hu
        call alloc_dl (obj%h(i), val, c, cl1, cu1, cl2, cu2, f)
      end do
    end if
    do i = 1, 2
      do j = 1, 2
        call alloc_dl (obj%j(i, j), val, c, cl1, cu1, cl2, cu2, f)
      end do
    end do
    if (k) then
      if (.not.allocated (obj%k)) allocate (obj%k)
      call alloc_dl (obj%k, val, c, cl1, cu1, cl2, cu2, f)
    else
      if (allocated (obj%k)) deallocate (obj%k)
    end if
  end subroutine alloc_dt
end module m
  use m
  type (dt), allocatable :: y
  call foo (y)
contains
  subroutine foo (y)
    use m
    type (dt), allocatable :: x, y, z(:,:)
    logical, parameter :: F = .false.
    logical, parameter :: T = .true.
    logical :: l
!$omp parallel private (x, y, z)
    if (allocated (x) .or. allocated (y) .or. allocated (z)) stop 11
!$omp end parallel
!$omp parallel firstprivate (x, y, z)
    if (allocated (x) .or. allocated (y) .or. allocated (z)) stop 12
!$omp end parallel
    l = F
!$omp parallel sections lastprivate (x, y, z) firstprivate (l)
!$omp section
    if (.not. l) then
      if (allocated (x) .or. allocated (y) .or. allocated (z)) stop 13
    end if
!$omp section
    if (.not. l) then
      if (allocated (x) .or. allocated (y) .or. allocated (z)) stop 14
    end if
    allocate (x, y, z(-3:-3,2:3))
    call alloc_dt (x, 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (x, 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call alloc_dt (y, 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call ver_dt (y, 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call alloc_dt (z(-3,2), 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (z(-3,2), 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call alloc_dt (z(-3,3), 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call ver_dt (z(-3,3), 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
!$omp section
!$omp end parallel sections
    if (.not.allocated (x) .or. .not.allocated (y)) stop 15
    if (.not.allocated (z)) stop 16
    if (lbound (z, 1) /= -3 .or. ubound (z, 1) /= -3) stop 17
    if (lbound (z, 2) /= 2 .or. ubound (z, 2) /= 3) stop 18
    call ver_dt (x, 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (y, 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call ver_dt (z(-3,2), 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (z(-3,3), 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call alloc_dt (x, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (x, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (y, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (y, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,2), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (z(-3,2), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,3), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (z(-3,3), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
!$omp parallel private (x, y, z)
    call ver_dt (x, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (x, 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (x, 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (y, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (y, 14, T, 3, 4, F, T, 1, 1, 2, 4, T)
    call ver_dt (y, 14, T, 3, 4, F, T, 1, 1, 2, 4, T)
    call ver_dt (z(-3,2), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,2), 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (z(-3,2), 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (z(-3,3), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,3), 14, T, 3, 4, F, T, 1, 1, 2, 4, T)
    call ver_dt (z(-3,3), 14, T, 3, 4, F, T, 1, 1, 2, 4, T)
!$omp end parallel
    call ver_dt (x, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (x, 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (y, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (y, 14, T, 3, 4, F, T, 1, 1, 2, 4, T)
    call ver_dt (z(-3,2), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,2), 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (z(-3,3), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,3), 14, T, 3, 4, F, T, 1, 1, 2, 4, T)
!$omp parallel private (x, y, z)
    call ver_dt (x, 0, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (y, 0, T, 3, 4, F, T, 1, 1, 2, 4, T)
    deallocate (x%h, x%k)
    deallocate (y%h)
    allocate (y%k)
    call ver_dt (z(-3,2), 0, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (z(-3,3), 0, T, 3, 4, F, T, 1, 1, 2, 4, T)
    deallocate (z(-3,2)%h, z(-3,2)%k)
    deallocate (z(-3,3)%h)
    allocate (z(-3,3)%k)
!$omp end parallel
    call alloc_dt (x, 5, T, 1, 2, F, T, 2, 3, -2, -2, F)
    call alloc_dt (y, 15, F, 0, 0, T, T, 2, 2, 2, 2, T)
    call alloc_dt (z(-3,2), 5, T, 1, 2, F, T, 2, 3, -2, -2, F)
    call alloc_dt (z(-3,3), 15, F, 0, 0, T, T, 2, 2, 2, 2, T)
!$omp parallel firstprivate (x, y, z)
    call ver_dt (x, 5, T, 1, 2, F, T, 2, 3, -2, -2, F)
    call alloc_dt (x, 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (x, 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (y, 15, F, 0, 0, T, T, 2, 2, 2, 2, T)
    call alloc_dt (y, 4, T, 3, 4, T, T, 1, 1, 2, 4, T)
    call ver_dt (y, 4, T, 3, 4, T, T, 1, 1, 2, 4, T)
    call ver_dt (z(-3,2), 5, T, 1, 2, F, T, 2, 3, -2, -2, F)
    call alloc_dt (z(-3,2), 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (z(-3,2), 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (z(-3,3), 15, F, 0, 0, T, T, 2, 2, 2, 2, T)
    call alloc_dt (z(-3,3), 4, T, 3, 4, T, T, 1, 1, 2, 4, T)
    call ver_dt (z(-3,3), 4, T, 3, 4, T, T, 1, 1, 2, 4, T)
!$omp end parallel
    call ver_dt (x, 5, T, 1, 2, F, T, 2, 3, -2, -2, F)
    call alloc_dt (x, 4, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (y, 15, F, 0, 0, T, T, 2, 2, 2, 2, T)
    call alloc_dt (y, 16, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (z(-3,2), 5, T, 1, 2, F, T, 2, 3, -2, -2, F)
    call alloc_dt (z(-3,2), 4, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (z(-3,3), 15, F, 0, 0, T, T, 2, 2, 2, 2, T)
    call alloc_dt (z(-3,3), 16, F, 0, 0, F, F, 0, 0, 0, 0, F)
!$omp parallel firstprivate (x, y, z)
    call ver_dt (x, 4, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (x, 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (x, 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (y, 16, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (y, 17, T, 1, 2, F, T, 2, 2, 3, 3, F)
    call ver_dt (y, 17, T, 1, 2, F, T, 2, 2, 3, 3, F)
    call ver_dt (z(-3,2), 4, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,2), 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (z(-3,2), 4, T, -3, -1, T, T, -1, -1, 2, 3, T)
    call ver_dt (z(-3,3), 16, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,3), 17, T, 1, 2, F, T, 2, 2, 3, 3, F)
    call ver_dt (z(-3,3), 17, T, 1, 2, F, T, 2, 2, 3, 3, F)
!$omp end parallel
    call ver_dt (x, 4, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (y, 16, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (y, 18, T, 0, 1, T, T, 0, 1, 0, 1, T)
    call ver_dt (z(-3,2), 4, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call ver_dt (z(-3,3), 16, F, 0, 0, F, F, 0, 0, 0, 0, F)
    call alloc_dt (z(-3,3), 18, T, 0, 1, T, T, 0, 1, 0, 1, T)
    l = F
!$omp parallel sections lastprivate (x, y, z) firstprivate (l)
!$omp section
    if (l) then
      call ver_dt (x, 9, T, 1, 1, F, F, 0, 0, 0, 0, T)
      call ver_dt (y, 21, F, 0, 0, T, T, 1, 2, 3, 4, T)
      call ver_dt (z(-3,2), 9, T, 1, 1, F, F, 0, 0, 0, 0, T)
      call ver_dt (z(-3,3), 21, F, 0, 0, T, T, 1, 2, 3, 4, T)
    else
      call ver_dt (x, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
      call ver_dt (y, 0, T, 0, 1, T, T, 0, 1, 0, 1, T)
      call ver_dt (z(-3,2), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
      call ver_dt (z(-3,3), 0, T, 0, 1, T, T, 0, 1, 0, 1, T)
    end if
    l = T
    call alloc_dt (x, 7, T, 1, 1, T, T, 1, 2, 3, 3, T)
    call ver_dt (x, 7, T, 1, 1, T, T, 1, 2, 3, 3, T)
    call alloc_dt (y, 20, T, 0, 0, F, T, 2, 2, 3, 4, F)
    call ver_dt (y, 20, T, 0, 0, F, T, 2, 2, 3, 4, F)
    call alloc_dt (z(-3,2), 7, T, 1, 1, T, T, 1, 2, 3, 3, T)
    call ver_dt (z(-3,2), 7, T, 1, 1, T, T, 1, 2, 3, 3, T)
    call alloc_dt (z(-3,3), 20, T, 0, 0, F, T, 2, 2, 3, 4, F)
    call ver_dt (z(-3,3), 20, T, 0, 0, F, T, 2, 2, 3, 4, F)
!$omp section
    if (l) then
      call ver_dt (x, 7, T, 1, 1, T, T, 1, 2, 3, 3, T)
      call ver_dt (y, 20, T, 0, 0, F, T, 2, 2, 3, 4, F)
      call ver_dt (z(-3,2), 7, T, 1, 1, T, T, 1, 2, 3, 3, T)
      call ver_dt (z(-3,3), 20, T, 0, 0, F, T, 2, 2, 3, 4, F)
    else
      call ver_dt (x, 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
      call ver_dt (y, 0, T, 0, 1, T, T, 0, 1, 0, 1, T)
      call ver_dt (z(-3,2), 0, F, 0, 0, F, F, 0, 0, 0, 0, F)
      call ver_dt (z(-3,3), 0, T, 0, 1, T, T, 0, 1, 0, 1, T)
    end if
    l = T
    call alloc_dt (x, 9, T, 1, 1, F, F, 0, 0, 0, 0, T)
    call ver_dt (x, 9, T, 1, 1, F, F, 0, 0, 0, 0, T)
    call alloc_dt (y, 21, F, 0, 0, T, T, 1, 2, 3, 4, T)
    call ver_dt (y, 21, F, 0, 0, T, T, 1, 2, 3, 4, T)
    call alloc_dt (z(-3,2), 9, T, 1, 1, F, F, 0, 0, 0, 0, T)
    call ver_dt (z(-3,2), 9, T, 1, 1, F, F, 0, 0, 0, 0, T)
    call alloc_dt (z(-3,3), 21, F, 0, 0, T, T, 1, 2, 3, 4, T)
    call ver_dt (z(-3,3), 21, F, 0, 0, T, T, 1, 2, 3, 4, T)
!$omp section
!$omp end parallel sections
    call ver_dt (x, 9, T, 1, 1, F, F, 0, 0, 0, 0, T)
    call ver_dt (y, 21, F, 0, 0, T, T, 1, 2, 3, 4, T)
    call ver_dt (z(-3,2), 9, T, 1, 1, F, F, 0, 0, 0, 0, T)
    call ver_dt (z(-3,3), 21, F, 0, 0, T, T, 1, 2, 3, 4, T)
!$omp parallel sections lastprivate (x, y, z) firstprivate (l)
!$omp section
    if (l) then
      call ver_dt (x, 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
      call ver_dt (y, 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
      call ver_dt (z(-3,2), 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
      call ver_dt (z(-3,3), 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    else
      call ver_dt (x, 0, T, 1, 1, F, F, 0, 0, 0, 0, T)
      call ver_dt (y, 0, F, 0, 0, T, T, 1, 2, 3, 4, T)
      call ver_dt (z(-3,2), 0, T, 1, 1, F, F, 0, 0, 0, 0, T)
      call ver_dt (z(-3,3), 0, F, 0, 0, T, T, 1, 2, 3, 4, T)
    end if
    l = T
    call alloc_dt (x, 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
    call ver_dt (x, 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
    call alloc_dt (y, 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
    call ver_dt (y, 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
    call alloc_dt (z(-3,2), 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
    call ver_dt (z(-3,2), 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
    call alloc_dt (z(-3,3), 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
    call ver_dt (z(-3,3), 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
!$omp section
    if (l) then
      call ver_dt (x, 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
      call ver_dt (y, 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
      call ver_dt (z(-3,2), 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
      call ver_dt (z(-3,3), 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
    else
      call ver_dt (x, 0, T, 1, 1, F, F, 0, 0, 0, 0, T)
      call ver_dt (y, 0, F, 0, 0, T, T, 1, 2, 3, 4, T)
      call ver_dt (z(-3,2), 0, T, 1, 1, F, F, 0, 0, 0, 0, T)
      call ver_dt (z(-3,3), 0, F, 0, 0, T, T, 1, 2, 3, 4, T)
    end if
    l = T
    call alloc_dt (x, 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (x, 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call alloc_dt (y, 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call ver_dt (y, 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call alloc_dt (z(-3,2), 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (z(-3,2), 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call alloc_dt (z(-3,3), 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call ver_dt (z(-3,3), 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
!$omp section
!$omp end parallel sections
    call ver_dt (x, 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (y, 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call ver_dt (z(-3,2), 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (z(-3,3), 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
!$omp parallel private (x, y, z)
    call ver_dt (x, 0, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (y, 0, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call ver_dt (z(-3,2), 0, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (z(-3,3), 0, T, 0, 1, T, T, 2, 2, 2, 2, F)
!$omp single
    call alloc_dt (x, 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
    call alloc_dt (y, 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
    call alloc_dt (z(-3,2), 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
    call alloc_dt (z(-3,3), 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
!$omp end single copyprivate (x, y, z)
    call ver_dt (x, 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
    call ver_dt (y, 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
    call ver_dt (z(-3,2), 3, F, 0, 0, T, T, 0, 1, 0, 1, F)
    call ver_dt (z(-3,3), 22, T, 5, 5, F, T, 2, 3, 2, 2, T)
!$omp end parallel
    call ver_dt (x, 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (y, 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
    call ver_dt (z(-3,2), 5, F, 0, 0, T, T, -1, -1, -1, -1, T)
    call ver_dt (z(-3,3), 23, T, 0, 1, T, T, 2, 2, 2, 2, F)
  end subroutine foo
end
