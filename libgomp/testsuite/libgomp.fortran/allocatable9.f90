! { dg-do run }

  integer, allocatable :: a, b(:), c(:,:)
  logical :: l
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
!$omp parallel private (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
  allocate (a, b(-7:-1), c(2:3, 3:5))
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 7) call abort
  if (lbound (b, 1) /= -7 .or. ubound (b, 1) /= -1) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 2 .or. size (c, 2) /= 3) call abort
  if (lbound (c, 1) /= 2 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 3 .or. ubound (c, 2) /= 5) call abort
  a = 4
  b = 3
  c = 2
!$omp end parallel
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
!$omp parallel firstprivate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
  allocate (a, b(-7:-1), c(2:3, 3:5))
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 7) call abort
  if (lbound (b, 1) /= -7 .or. ubound (b, 1) /= -1) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 2 .or. size (c, 2) /= 3) call abort
  if (lbound (c, 1) /= 2 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 3 .or. ubound (c, 2) /= 5) call abort
  a = 4
  b = 3
  c = 2
!$omp end parallel
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
  allocate (a, b(6:9), c(3, 8:9))
  a = 2
  b = 4
  c = 5
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
!$omp parallel firstprivate (a, b, c)
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  if (a /= 2 .or. any (b .ne. 4) .or. any (c .ne. 5)) call abort
  deallocate (a)
  if (allocated (a)) call abort
  allocate (a)
  a = 8
  b = (/ 1, 2, 3 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 2, 4 /))
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 3) call abort
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) call abort
  if (.not.allocated (c) .or. size (c) /= 8) call abort
  if (size (c, 1) /= 2 .or. size (c, 2) /= 4) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) call abort
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) call abort
  if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) call abort
!$omp end parallel
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  if (a /= 2 .or. any (b .ne. 4) .or. any (c .ne. 5)) call abort
  l = .false.
!$omp parallel sections lastprivate (a, b, c) firstprivate (l)
!$omp section
  if (.not.allocated (a)) call abort
  if (l) then
    if (.not.allocated (b) .or. size (b) /= 6) call abort
    if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) call abort
    if (.not.allocated (c) .or. size (c) /= 8) call abort
    if (size (c, 1) /= 4 .or. size (c, 2) /= 2) call abort
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) call abort
    if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) call abort
    if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) call abort
  else
    if (.not.allocated (b) .or. size (b) /= 4) call abort
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
    if (.not.allocated (c) .or. size (c) /= 6) call abort
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  end if
  l = .true.
  deallocate (a)
  if (allocated (a)) call abort
  allocate (a)
  a = 8
  b = (/ 1, 2, 3 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 2, 4 /))
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 3) call abort
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) call abort
  if (.not.allocated (c) .or. size (c) /= 8) call abort
  if (size (c, 1) /= 2 .or. size (c, 2) /= 4) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) call abort
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) call abort
  if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) call abort
!$omp section
  if (.not.allocated (a)) call abort
  if (l) then
    if (.not.allocated (b) .or. size (b) /= 3) call abort
    if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) call abort
    if (.not.allocated (c) .or. size (c) /= 8) call abort
    if (size (c, 1) /= 2 .or. size (c, 2) /= 4) call abort
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) call abort
    if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) call abort
    if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) call abort
  else
    if (.not.allocated (b) .or. size (b) /= 4) call abort
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
    if (.not.allocated (c) .or. size (c) /= 6) call abort
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  end if
  l = .true.
  deallocate (a)
  if (allocated (a)) call abort
  allocate (a)
  a = 12
  b = (/ 9, 8, 7, 6, 5, 4 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 4, 2 /))
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 6) call abort
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) call abort
  if (.not.allocated (c) .or. size (c) /= 8) call abort
  if (size (c, 1) /= 4 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) call abort
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) call abort
  if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) call abort
!$omp end parallel sections
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 6) call abort
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) call abort
  if (.not.allocated (c) .or. size (c) /= 8) call abort
  if (size (c, 1) /= 4 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) call abort
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) call abort
  if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) call abort
end
