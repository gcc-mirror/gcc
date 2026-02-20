! { dg-do run }

! PR fortran/120505

! Check that struct components are mapped in increasing address order.

module m
  type t
    integer, allocatable :: den1(:,:), den2(:,:), den3(:,:)
    real, allocatable :: data1(:), data2(:)
  end type t

  type t2
    type(t), allocatable :: tiles(:)
  end type t2

  type(t2) :: var
contains
  ! Helper subroutine to validate array contents
  subroutine validate_arrays(test_id, expect_den1, expect_den2, expect_den3, &
                             expect_data1, expect_data2)
    integer :: test_id, i, j
    integer, intent(in) :: expect_den1(:,:), expect_den2(:,:), expect_den3(:,:)
    real, intent(in) :: expect_data1(:), expect_data2(:)

    if (any (var%tiles(1)%den1 /= expect_den1)) then
      print *, "Test", test_id, ": den1 mismatch"
      stop 1
    end if
    if (any (var%tiles(1)%den2 /= expect_den2)) then
      print *, "Test", test_id, ": den2 mismatch"
      stop 1
    end if
    if (any (var%tiles(1)%den3 /= expect_den3)) then
      print *, "Test", test_id, ": den3 mismatch"
      stop 1
    end if
    if (any (abs(var%tiles(1)%data1 - expect_data1) > 1.0e-6)) then
      print *, "Test", test_id, ": data1 mismatch"
      stop 1
    end if
    if (any (abs(var%tiles(1)%data2 - expect_data2) > 1.0e-2)) then
      print *, "Test", test_id, ": data2 mismatch"
      stop 1
    end if
  end subroutine validate_arrays
end module m

use m

! Initialize test data
allocate(var%tiles(1))
var%tiles(1)%den1 = reshape([1,2,3,4],[2,2])
var%tiles(1)%den2 = reshape([11,22,33,44],[2,2])
var%tiles(1)%den3 = reshape([111,222,333,444],[2,2])
var%tiles(1)%data1 = [1.5, 2.5, 3.5]
var%tiles(1)%data2 = [10.1, 20.2, 30.3]

! ========== TEST 1: Reverse mapping order (den2, den3, den1, data2, data1) ==========
!$omp target enter data map(var%tiles(1)%den2, var%tiles(1)%den3, &
!$omp&                       var%tiles(1)%den1, var%tiles(1)%data2, &
!$omp&                       var%tiles(1)%data1)

!$omp target
 if (any (var%tiles(1)%den1 /= reshape([1,2,3,4],[2,2]))) stop 1
 if (any (var%tiles(1)%den2 /= reshape([11,22,33,44],[2,2]))) stop 1
 if (any (var%tiles(1)%den3 /= reshape([111,222,333,444],[2,2]))) stop 1
 if (any (abs(var%tiles(1)%data1 - [1.5, 2.5, 3.5]) > 1.0e-6)) stop 1
 if (any (abs(var%tiles(1)%data2 - [10.1, 20.2, 30.3]) > 1.0e-6)) stop 1

 var%tiles(1)%den1 = var%tiles(1)%den1 + 5
 var%tiles(1)%den2 = var%tiles(1)%den2 + 7
 var%tiles(1)%den3 = var%tiles(1)%den3 + 9
 var%tiles(1)%data1 = var%tiles(1)%data1 * 2.0
 var%tiles(1)%data2 = var%tiles(1)%data2 * 3.0
!$omp end target

!$omp target exit data map(var%tiles(1)%den2, var%tiles(1)%den3, &
!$omp&                      var%tiles(1)%den1, var%tiles(1)%data2, &
!$omp&                      var%tiles(1)%data1)

call validate_arrays(1, &
  reshape([6,7,8,9],[2,2]), reshape([18,29,40,51],[2,2]), reshape([120,231,342,453],[2,2]), &
  [3.0, 5.0, 7.0], [30.3, 60.6, 90.9])

! Reset data
var%tiles(1)%den1 = reshape([1,2,3,4],[2,2])
var%tiles(1)%den2 = reshape([11,22,33,44],[2,2])
var%tiles(1)%den3 = reshape([111,222,333,444],[2,2])
var%tiles(1)%data1 = [1.5, 2.5, 3.5]
var%tiles(1)%data2 = [10.1, 20.2, 30.3]

! ========== TEST 2: Different permutation (den3, data1, den1, den2, data2) ==========
!$omp target enter data map(var%tiles(1)%den3, var%tiles(1)%data1, &
!$omp&                       var%tiles(1)%den1, var%tiles(1)%den2, &
!$omp&                       var%tiles(1)%data2)

!$omp target
 var%tiles(1)%den1 = var%tiles(1)%den1 * 2
 var%tiles(1)%den2 = var%tiles(1)%den2 * 2
 var%tiles(1)%den3 = var%tiles(1)%den3 * 2
 var%tiles(1)%data1 = var%tiles(1)%data1 + 100.0
 var%tiles(1)%data2 = var%tiles(1)%data2 + 100.0
!$omp end target

!$omp target exit data map(var%tiles(1)%den3, var%tiles(1)%data1, &
!$omp&                      var%tiles(1)%den1, var%tiles(1)%den2, &
!$omp&                      var%tiles(1)%data2)

call validate_arrays(2, &
  reshape([2,4,6,8],[2,2]), reshape([22,44,66,88],[2,2]), reshape([222,444,666,888],[2,2]), &
  [101.5, 102.5, 103.5], [110.1, 120.2, 130.3])

! Reset data
var%tiles(1)%den1 = reshape([1,2,3,4],[2,2])
var%tiles(1)%den2 = reshape([11,22,33,44],[2,2])
var%tiles(1)%den3 = reshape([111,222,333,444],[2,2])
var%tiles(1)%data1 = [1.5, 2.5, 3.5]
var%tiles(1)%data2 = [10.1, 20.2, 30.3]

! ========== TEST 3: Subset of components mapped (den2, data1 only) ==========
!$omp target enter data map(var%tiles(1)%data1, var%tiles(1)%den2)

!$omp target
 if (any (var%tiles(1)%den2 /= reshape([11,22,33,44],[2,2]))) stop 1
 if (any (abs(var%tiles(1)%data1 - [1.5, 2.5, 3.5]) > 1.0e-6)) stop 1

 var%tiles(1)%den2 = var%tiles(1)%den2 - 3
 var%tiles(1)%data1 = var%tiles(1)%data1 * 10.0
!$omp end target

!$omp target exit data map(var%tiles(1)%data1, var%tiles(1)%den2)

call validate_arrays(3, &
  reshape([1,2,3,4],[2,2]), reshape([8,19,30,41],[2,2]), reshape([111,222,333,444],[2,2]), &
  [15.0, 25.0, 35.0], [10.1, 20.2, 30.3])

! Reset data
var%tiles(1)%den1 = reshape([1,2,3,4],[2,2])
var%tiles(1)%den2 = reshape([11,22,33,44],[2,2])
var%tiles(1)%den3 = reshape([111,222,333,444],[2,2])
var%tiles(1)%data1 = [1.5, 2.5, 3.5]
var%tiles(1)%data2 = [10.1, 20.2, 30.3]

! ========== TEST 4: Enter and exit maps in different orders ==========
!$omp target enter data map(var%tiles(1)%den1, var%tiles(1)%den3, &
!$omp&                       var%tiles(1)%data2)

!$omp target
 if (any (var%tiles(1)%den1 /= reshape([1,2,3,4],[2,2]))) stop 1
 if (any (var%tiles(1)%den3 /= reshape([111,222,333,444],[2,2]))) stop 1
 if (any (abs(var%tiles(1)%data2 - [10.1, 20.2, 30.3]) > 1.0e-2)) stop 1

 var%tiles(1)%den1 = var%tiles(1)%den1 * 3
 var%tiles(1)%den3 = var%tiles(1)%den3 + 50
 var%tiles(1)%data2 = var%tiles(1)%data2 * 2.0
!$omp end target

!$omp target exit data map(var%tiles(1)%data2, var%tiles(1)%den3, &
!$omp&                      var%tiles(1)%den1)

call validate_arrays(4, &
  reshape([3,6,9,12],[2,2]), reshape([11,22,33,44],[2,2]), reshape([161,272,383,494],[2,2]), &
  [1.5, 2.5, 3.5], [20.2, 40.4, 60.6])

print *, "All tests passed!"
end
