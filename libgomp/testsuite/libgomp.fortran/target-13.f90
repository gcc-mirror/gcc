module m
  implicit none
  type t
    integer :: s, a(5)
  end type t

  type t2
    integer :: s, a(5)
    type(t) :: st, at(2:3)
  end type t2

  interface operator(/=)
    procedure ne_compare_t
    procedure ne_compare_t2
  end interface

contains

  logical pure elemental function ne_compare_t (a, b) result(res)
    type(t), intent(in) :: a, b
    res = (a%s /= b%s) .or. any(a%a /= b%a)
  end function

  logical pure elemental function ne_compare_t2 (a, b) result(res)
    type(t2), intent(in) :: a, b
    res = (a%s /= b%s) .or. any(a%a /= b%a)     &
          .or. (a%st /= b%st) .or. any(a%at /= b%at)
  end function
end module m

program p
use m
implicit none

type(t2) :: var1, var2(5), var3(:)
type(t2) :: var1a, var2a(5), var3a(:)
allocatable :: var3, var3a
logical :: shared_memory = .false.

!$omp target map(to: shared_memory)
  shared_memory = .true.
!$omp end target

var1 = T2(1, [1,2,3,4,5], T(11, [11,22,33,44,55]), &
          [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])])

var2 = [T2(101, [201,202,203,204,205], T(2011, [2011,2022,2033,2044,2055]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])]),       &
        T2(111, [211,212,213,214,215], T(2111, [2111,2122,2133,2144,2155]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])]),       &
        T2(121, [221,222,223,224,225], T(2211, [2211,2222,2233,2244,2255]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])]),       &
        T2(131, [231,232,233,234,235], T(2311, [2311,2322,2333,2344,2355]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])]),       &
        T2(141, [241,242,243,244,245], T(2411, [2411,2422,2433,2444,2455]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])])]

var3 = [T2(301, [401,402,403,404,405], T(4011, [4011,4022,4033,4044,4055]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])]),       &
        T2(311, [411,412,413,414,415], T(4111, [4111,4122,4133,4144,4155]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])]),       &
        T2(321, [421,422,423,424,425], T(4211, [4211,4222,4233,4244,4255]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])]),       &
        T2(331, [431,432,433,434,435], T(4311, [4311,4322,4333,4344,4355]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])]),       &
        T2(341, [441,442,443,444,445], T(4411, [4411,4422,4433,4444,4455]), &
           [T(-11, [-11,-22,-33,-44,-55]), T(11, [11,22,33,44,55])])]

var1a = var1
var2a = var2
var3a = var3

!$omp target enter data map(to:var1)
!$omp target enter data map(to:var2)
!$omp target enter data map(to:var3)

! ---------------

!$omp target update from(var1%at(2:3))

if (var1a /= var1) error stop
if (any (var2a /= var2)) error stop
if (any (var3a /= var3)) error stop

! ---------------

!$omp target
  var1%st%s = 1243
  var2(3)%at(2) = T(123, [345,64,356,39,13])
  var2(3)%at(3) = T(48, [74,162,572,357,3])
!$omp end target

if (.not. shared_memory) then
  if (var1 /= var1) error stop
  if (any (var2a /= var2)) error stop
  if (any (var3a /= var3)) error stop
endif

!$omp target update from(var1%st) from(var2(3)%at(2:3))

var1a%st%s = 1243
var2a(3)%at(2) = T(123, [345,64,356,39,13])
var2a(3)%at(3) = T(48, [74,162,572,357,3])
if (var1 /= var1) error stop
if (any (var2a /= var2)) error stop
if (any (var3a /= var3)) error stop

! ---------------

var3(1) = var2(1)
var1%at(2)%a = var2(1)%a
var1%at(3)%a = var2(2)%a

var1a = var1
var2a = var2
var3a = var3

!$omp target update to(var3) to(var1%at(2:3))

!$omp target
  var3(1)%s = var3(1)%s + 123
  var1%at(2)%a = var1%at(2)%a * 7
  var1%at(3)%s = var1%at(3)%s * (-3)
!$omp end target

if (.not. shared_memory) then
  if (var1 /= var1) error stop
  if (any (var2a /= var2)) error stop
  if (any (var3a /= var3)) error stop
endif

var3a(1)%s = var3a(1)%s + 123
var1a%at(2)%a = var1a%at(2)%a * 7
var1a%at(3)%s = var1a%at(3)%s * (-3)

block
  integer, volatile :: i1,i2,i3,i4
  i1 = 1
  i2 = 2
  i3 = 1
  i4 = 2
  !$omp target update from(var3(i1:i2)) from(var1%at(i3:i4))
  i1 = 3
  i2 = 3
  i3 = 1
  i4 = 5
  !$omp target update from(var1%at(i1)%s) from(var1%at(i2)%a(i3:i4))
end block

if (var1 /= var1) error stop
if (any (var2a /= var2)) error stop
if (any (var3a /= var3)) error stop

! ---------------

!$omp target exit data map(from:var1)
!$omp target exit data map(from:var2)
!$omp target exit data map(from:var3)
end
