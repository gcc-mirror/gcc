! { dg-do run }
! { dg-additional-options "-fdump-tree-original -fcheck=all" }

! PR fortran/107424

! Nonrectangular loop nests checks

! Valid patterns are:
!  (1)  a2 - var-outer
!  (2)  a1 * var-outer
!  (3)  a1 * var-outer + a2
!  (4)  a2 + a1 * var-outer
!  (5)  a1 * var-outer - a2
!  (6)  a2 - a1 * var-outer
!  (7)  var-outer * a1
!  (8)  var-outer * a1 + a2
!  (9)  a2 + var-outer * a1
! (10)  var-outer * a1 - a2
! (11)  a2 - var-outer * a1

module m
contains


! { dg-final { scan-tree-dump-times "for \\(one_two_inner = one_two_outer \\* -1 \\+ one_a2; one_two_inner <= one_two_outer \\* two_a1 \\+ 0; one_two_inner = one_two_inner \\+ 1\\)" 1 original } }

!  (1)  a2 - var-outer
!  (2)  a1 * var-outer
subroutine one_two()
  implicit none
  integer :: one_a2
  integer :: two_a1
  integer :: one_two_outer, one_two_inner
  integer :: i, j
  integer, allocatable :: var(:,:)

  one_a2 = 13
  two_a1 = 5
  allocate(var(1:10, one_a2 - 10:two_a1 * 10), &
           source=0)
  if (size(var) <= 4) error stop

  !$omp simd collapse(2)
  do one_two_outer = 1, 10
    do one_two_inner = one_a2 - one_two_outer, two_a1 * one_two_outer
      !$omp atomic update
      var(one_two_outer,one_two_inner) = var(one_two_outer,one_two_inner) + 2
    end do
  end do

  do i = 1, 10
    do j = one_a2 - i, two_a1 * i
      if (var(i,j) /= 2) error stop
    end do
  end do
end


! { dg-final { scan-tree-dump-times "for \\(three_four_inner = three_four_outer \\* three_a1 \\+ three_a2; three_four_inner <= three_four_outer \\* four_a1 \\+ four_a2; three_four_inner = three_four_inner \\+ 1\\)" 1 original } }

!  (3)  a1 * var-outer + a2
!  (4)  a2 + a1 * var-outer
subroutine three_four()
  implicit none
  integer :: three_a1, three_a2
  integer :: four_a1, four_a2
  integer :: three_four_outer, three_four_inner
  integer :: i, j
  integer, allocatable :: var(:,:)

  three_a1 = 2
  three_a2 = 3
  four_a1 = 3
  four_a2 = 5
  allocate(var(1:10, three_a1 * 1 + three_a2:four_a2 + four_a1 * 10), &
           source=0)
  if (size(var) <= 4) error stop

  !$omp simd collapse(2)
  do three_four_outer = 1, 10
    do three_four_inner = three_a1 * three_four_outer + three_a2, four_a2 + four_a1 * three_four_outer
      !$omp atomic update
      var(three_four_outer, three_four_inner) = var(three_four_outer, three_four_inner) + 2
    end do
  end do
  do i = 1, 10
    do j = three_a1 * i + three_a2, four_a2 + four_a1 * i
      if (var(i,j) /= 2) error stop
    end do
  end do
end


! { dg-final { scan-tree-dump-times "for \\(five_six_inner = five_six_outer \\* five_a1 \\+ D\\.\[0-9\]+; five_six_inner <= five_six_outer \\* D\\.\[0-9\]+ \\+ six_a2; five_six_inner = five_six_inner \\+ 1\\)" 1 original } }

!  (5)  a1 * var-outer - a2
!  (6)  a2 - a1 * var-outer
subroutine five_six()
  implicit none
  integer :: five_a1, five_a2
  integer :: six_a1, six_a2
  integer :: five_six_outer, five_six_inner
  integer :: i, j
  integer, allocatable :: var(:,:)

  five_a1 = 2
  five_a2 = -3
  six_a1 = 3
  six_a2 = 20
  allocate(var(1:10, five_a1 * 1 - five_a2:six_a2 - six_a1 * 1), &
           source=0)
  if (size(var) <= 4) error stop

  !$omp simd collapse(2)
  do five_six_outer = 1, 10
    do five_six_inner = five_a1 * five_six_outer - five_a2, six_a2 - six_a1 * five_six_outer
      !$omp atomic update
      var(five_six_outer, five_six_inner) = var(five_six_outer, five_six_inner) + 2
    end do
  end do

  do i = 1, 10
    do j = five_a1 * i - five_a2, six_a2 - six_a1 * i
      if (var(i,j) /= 2) error stop
    end do
  end do
end


! { dg-final { scan-tree-dump-times "for \\(seven_eight_inner = seven_eight_outer \\* seven_a1 \\+ 0; seven_eight_inner <= seven_eight_outer \\* eight_a1 \\+ eight_a2; seven_eight_inner = seven_eight_inner \\+ 1\\)" 1 original } }

!  (7)  var-outer * a1
!  (8)  var-outer * a1 + a2
subroutine seven_eight()
  implicit none
  integer :: seven_a1
  integer :: eight_a1, eight_a2
  integer :: seven_eight_outer, seven_eight_inner
  integer :: i, j
  integer, allocatable :: var(:,:)

  seven_a1 = 3
  eight_a1 = 2
  eight_a2 = -4
  allocate(var(1:10, 1 * seven_a1 : 10 * eight_a1 + eight_a2), &
           source=0)
  if (size(var) <= 4) error stop

  !$omp simd collapse(2)
  do seven_eight_outer = 1, 10
    do seven_eight_inner = seven_eight_outer * seven_a1, seven_eight_outer * eight_a1 + eight_a2
      !$omp atomic update
      var(seven_eight_outer, seven_eight_inner) = var(seven_eight_outer, seven_eight_inner) + 2
    end do
  end do

  do i = 1, 10
    do j = i * seven_a1, i * eight_a1 + eight_a2
      if (var(i,j) /= 2) error stop
    end do
  end do
end


! { dg-final { scan-tree-dump-times "for \\(nine_ten_inner = nine_ten_outer \\* nine_a1 \\+ nine_a2; nine_ten_inner <= nine_ten_outer \\* ten_a1 \\+ D\\.\[0-9\]+; nine_ten_inner = nine_ten_inner \\+ 1\\)" 1 original } }

!  (9)  a2 + var-outer * a1
! (10)  var-outer * a1 - a2
subroutine nine_ten()
  implicit none
  integer :: nine_a1, nine_a2
  integer :: ten_a1, ten_a2
  integer :: nine_ten_outer, nine_ten_inner
  integer :: i, j
  integer, allocatable :: var(:,:)

  nine_a1 = 3
  nine_a2 = 5
  ten_a1 = 2
  ten_a2 = 3
  allocate(var(1:10, nine_a2 + 1 * nine_a1:10 * ten_a1 - ten_a2), &
           source=0)
  if (size(var) <= 4) error stop

  !$omp simd collapse(2)
  do nine_ten_outer = 1, 10
    do nine_ten_inner = nine_a2 + nine_ten_outer * nine_a1, nine_ten_outer * ten_a1 - ten_a2
      !$omp atomic update
      var(nine_ten_outer, nine_ten_inner) = var(nine_ten_outer, nine_ten_inner) + 2
    end do
  end do

  do i = 1, 10
    do j = nine_a2 + i * nine_a1, i * ten_a1 - ten_a2
      if (var(i,j) /= 2) error stop
    end do
  end do
end


! { dg-final { scan-tree-dump-times "for \\(eleven_inner = eleven_outer \\* D\\.\[0-9\]+ \\+ eleven_a2; eleven_inner <= 10; eleven_inner = eleven_inner \\+ 1\\)" 1 original } }

! (11)  a2 - var-outer * a1

subroutine eleven()
  implicit none
  integer :: eleven_a1, eleven_a2
  integer :: eleven_outer, eleven_inner
  integer :: i, j
  integer, allocatable :: var(:,:)

  eleven_a1 = 2
  eleven_a2 = 3
  allocate(var(1:10, eleven_a2 - 10 * eleven_a1 : 10), &
           source=0)
  if (size(var) <= 4) error stop

  !$omp simd collapse(2)
  do eleven_outer = 1, 10
    do eleven_inner = eleven_a2 - eleven_outer * eleven_a1, 10
      !$omp atomic update
      var(eleven_outer, eleven_inner) = var(eleven_outer, eleven_inner) + 2
    end do
  end do

  do i = 1, 10
    do j = eleven_a2 - i * eleven_a1, 10
      if (var(i,j) /= 2) error stop
    end do
  end do
end
end module m

program main
use m
implicit none
call one_two()
call three_four()
call five_six()
call seven_eight()
call nine_ten()
call eleven()
end
