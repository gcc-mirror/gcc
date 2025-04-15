! Ensure that polymorphic mapping is diagnosed as undefined behavior
! Ensure that static access to polymorphic variables works

subroutine test(case)
implicit none(type, external)
type t
  integer :: x(4)
end type t

type ta
  integer, allocatable :: x(:)
end type ta

type t2
  class(t), allocatable :: x
  class(t), allocatable :: x2(:)
end type t2

type t3
   type(t2) :: y
   type(t2) :: y2(2)
end type t3

type t4
   type(t3), allocatable :: y
   type(t3), allocatable :: y2(:)
end type t4

integer, value :: case

logical :: is_shared_mem

! Mangle stack addresses
integer, volatile :: case_var(100*case)

type(t), allocatable :: var1
type(ta), allocatable :: var1a
class(t), allocatable :: var2
type(t2), allocatable :: var3
type(t4), allocatable :: var4

case_var(100) = 0
!print *, 'case', case

var1 = t([1,2,3,4])
var1a = ta([-1,-2,-3,-4,-5])

var2 = t([11,22,33,44])

allocate(t2 :: var3)
allocate(t  :: var3%x)
allocate(t  :: var3%x2(2))
var3%x%x = [111,222,333,444]
var3%x2(1)%x = 2*[111,222,333,444]
var3%x2(2)%x = 3*[111,222,333,444]

allocate(t4 :: var4)
allocate(t3 :: var4%y)
allocate(t3 :: var4%y2(2))
allocate(t :: var4%y%y%x)
allocate(t :: var4%y%y%x2(2))
allocate(t :: var4%y2(1)%y%x)
allocate(t :: var4%y2(1)%y%x2(2))
allocate(t :: var4%y2(2)%y%x)
allocate(t :: var4%y2(2)%y%x2(2))
var4%y%y%x%x = -1 * [1111,2222,3333,4444]
var4%y%y%x2(1)%x = -2 * [1111,2222,3333,4444]
var4%y%y%x2(2)%x = -3 * [1111,2222,3333,4444]
var4%y2(1)%y%x%x = -4 * [1111,2222,3333,4444]
var4%y2(1)%y%x2(1)%x = -5 * [1111,2222,3333,4444]
var4%y2(1)%y%x2(2)%x = -6 * [1111,2222,3333,4444]
var4%y2(2)%y%x%x = -7 * [1111,2222,3333,4444]
var4%y2(2)%y%x2(1)%x = -8 * [1111,2222,3333,4444]
var4%y2(2)%y%x2(2)%x = -9 * [1111,2222,3333,4444]

is_shared_mem = .false.
!$omp target map(to: is_shared_mem)
  is_shared_mem = .true.
!$omp end target

if (case == 1) then
  ! implicit mapping
  !$omp target
    if (any (var1%x /= [1,2,3,4])) stop 1
    var1%x = 2 * var1%x
  !$omp end target

  !$omp target
    if (any (var1a%x /= [-1,-2,-3,-4])) stop 2
    var1a%x = 3 * var1a%x
  !$omp end target

  !$omp target  ! { dg-warning "Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var2%x /= [11,22,33,44])) stop 3
    var2%x = 4 * var2%x
  !$omp end target

  !$omp target  ! { dg-warning "Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var3%x%x /= [111,222,333,444])) stop 4
    var3%x%x = 5 * var3%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var3%x2(1)%x /= 2*[111,222,333,444])) stop 4
      if (any (var3%x2(2)%x /= 3*[111,222,333,444])) stop 4
      var3%x2(1)%x = 5 * var3%x2(1)%x
      var3%x2(2)%x = 5 * var3%x2(2)%x
    end if
  !$omp end target

  !$omp target  ! { dg-warning "Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var4%y%y%x%x /= -1 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y%y%x2(1)%x /= -2 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y%y%x2(2)%x /= -3 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(1)%y%x%x /= -4 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(1)%y%x2(1)%x /= -5 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(1)%y%x2(2)%x /= -6 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(2)%y%x%x /= -7 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(2)%y%x2(1)%x /= -8 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(2)%y%x2(2)%x /= -9 * [1111,2222,3333,4444])) stop 5
    end if
    var4%y%y%x%x = 6 * var4%y%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y%y%x2(1)%x = 6 * var4%y%y%x2(1)%x
      var4%y%y%x2(2)%x = 6 * var4%y%y%x2(2)%x
    endif
    var4%y2(1)%y%x%x = 6 * var4%y2(1)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(1)%y%x2(1)%x = 6 * var4%y2(1)%y%x2(1)%x
      var4%y2(1)%y%x2(2)%x = 6 * var4%y2(1)%y%x2(2)%x
    endif
    var4%y2(2)%y%x%x = 6 * var4%y2(2)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(2)%y%x2(1)%x = 6 * var4%y2(2)%y%x2(1)%x
      var4%y2(2)%y%x2(2)%x = 6 * var4%y2(2)%y%x2(2)%x
    endif
  !$omp end target

else if (case == 2) then
  ! Use target with defaultmap(TO)

  !$omp target defaultmap(to : all)
    if (any (var1%x /= [1,2,3,4])) stop 1
    var1%x = 2 * var1%x
  !$omp end target

  !$omp target defaultmap(to : all)
    if (any (var1a%x /= [-1,-2,-3,-4])) stop 2
    var1a%x = 3 * var1a%x
  !$omp end target

  !$omp target defaultmap(to : all) ! { dg-warning "Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var2%x /= [11,22,33,44])) stop 3
    var2%x = 4 * var2%x
  !$omp end target

  !$omp target defaultmap(to : all)  ! { dg-warning "Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var3%x%x /= [111,222,333,444])) stop 4
    var3%x%x = 5 * var3%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var3%x2(1)%x /= 2*[111,222,333,444])) stop 4
      if (any (var3%x2(2)%x /= 3*[111,222,333,444])) stop 4
      var3%x2(1)%x = 5 * var3%x2(1)%x
      var3%x2(2)%x = 5 * var3%x2(2)%x
    endif
  !$omp end target

  !$omp target defaultmap(to : all) firstprivate(is_shared_mem)  ! { dg-warning "Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var4%y%y%x%x /= -1 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y%y%x2(1)%x /= -2 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y%y%x2(2)%x /= -3 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(1)%y%x%x /= -4 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(1)%y%x2(1)%x /= -5 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(1)%y%x2(2)%x /= -6 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(2)%y%x%x /= -7 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(2)%y%x2(1)%x /= -8 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(2)%y%x2(2)%x /= -9 * [1111,2222,3333,4444])) stop 5
    endif
    var4%y%y%x%x = 6 * var4%y%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y%y%x2(1)%x = 6 * var4%y%y%x2(1)%x
      var4%y%y%x2(2)%x = 6 * var4%y%y%x2(2)%x
    endif
    var4%y2(1)%y%x%x = 6 * var4%y2(1)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(1)%y%x2(1)%x = 6 * var4%y2(1)%y%x2(1)%x
      var4%y2(1)%y%x2(2)%x = 6 * var4%y2(1)%y%x2(2)%x
    endif
    var4%y2(2)%y%x%x = 6 * var4%y2(2)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(2)%y%x2(1)%x = 6 * var4%y2(2)%y%x2(1)%x
      var4%y2(2)%y%x2(2)%x = 6 * var4%y2(2)%y%x2(2)%x
    endif
  !$omp end target

else if (case == 3) then
  ! Use target with map clause

  !$omp target map(tofrom: var1)
    if (any (var1%x /= [1,2,3,4])) stop 1
    var1%x = 2 * var1%x
  !$omp end target

  !$omp target map(tofrom: var1a)
    if (any (var1a%x /= [-1,-2,-3,-4])) stop 2
    var1a%x = 3 * var1a%x
  !$omp end target

  !$omp target map(tofrom: var2)  ! { dg-warning "28: Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var2%x /= [11,22,33,44])) stop 3
    var2%x = 4 * var2%x
  !$omp end target

  !$omp target map(tofrom: var3)  ! { dg-warning "28: Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var3%x%x /= [111,222,333,444])) stop 4
    var3%x%x = 5 * var3%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var3%x2(1)%x /= 2*[111,222,333,444])) stop 4
      if (any (var3%x2(2)%x /= 3*[111,222,333,444])) stop 4
      var3%x2(1)%x = 5 * var3%x2(1)%x
      var3%x2(2)%x = 5 * var3%x2(2)%x
    endif
  !$omp end target

  !$omp target map(tofrom: var4)  ! { dg-warning "28: Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var4%y%y%x%x /= -1 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y%y%x2(1)%x /= -2 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y%y%x2(2)%x /= -3 * [1111,2222,3333,4444])) stop 5
    end if
    if (any (var4%y2(1)%y%x%x /= -4 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(1)%y%x2(1)%x /= -5 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(1)%y%x2(2)%x /= -6 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(2)%y%x%x /= -7 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(2)%y%x2(1)%x /= -8 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(2)%y%x2(2)%x /= -9 * [1111,2222,3333,4444])) stop 5
    endif
    var4%y%y%x%x = 6 * var4%y%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y%y%x2(1)%x = 6 * var4%y%y%x2(1)%x
      var4%y%y%x2(2)%x = 6 * var4%y%y%x2(2)%x
    endif
    var4%y2(1)%y%x%x = 6 * var4%y2(1)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(1)%y%x2(1)%x = 6 * var4%y2(1)%y%x2(1)%x
      var4%y2(1)%y%x2(2)%x = 6 * var4%y2(1)%y%x2(2)%x
    endif
    var4%y2(2)%y%x%x = 6 * var4%y2(2)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(2)%y%x2(1)%x = 6 * var4%y2(2)%y%x2(1)%x
      var4%y2(2)%y%x2(2)%x = 6 * var4%y2(2)%y%x2(2)%x
    endif
  !$omp end target

else if (case == 4) then
  ! Use target with map clause -- NOTE: This uses TO not TOFROM

  !$omp target map(to: var1)
    if (any (var1%x /= [1,2,3,4])) stop 1
    var1%x = 2 * var1%x
  !$omp end target

  !$omp target map(to: var1a)
    if (any (var1a%x /= [-1,-2,-3,-4])) stop 2
    var1a%x = 3 * var1a%x
  !$omp end target

  !$omp target map(to: var2)  ! { dg-warning "24: Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var2%x /= [11,22,33,44])) stop 3
    var2%x = 4 * var2%x
  !$omp end target

  !$omp target map(to: var3)  ! { dg-warning "24: Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var3%x%x /= [111,222,333,444])) stop 4
    var3%x%x = 5 * var3%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var3%x2(1)%x /= 2*[111,222,333,444])) stop 4
      if (any (var3%x2(2)%x /= 3*[111,222,333,444])) stop 4
      var3%x2(1)%x = 5 * var3%x2(1)%x
      var3%x2(2)%x = 5 * var3%x2(2)%x
    endif
  !$omp end target

  !$omp target map(to: var4)  ! { dg-warning "24: Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var4%y%y%x%x /= -1 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y%y%x2(1)%x /= -2 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y%y%x2(2)%x /= -3 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(1)%y%x%x /= -4 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(1)%y%x2(1)%x /= -5 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(1)%y%x2(2)%x /= -6 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(2)%y%x%x /= -7 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(2)%y%x2(1)%x /= -8 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(2)%y%x2(2)%x /= -9 * [1111,2222,3333,4444])) stop 5
    endif
    var4%y%y%x%x = 6 * var4%y%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y%y%x2(1)%x = 6 * var4%y%y%x2(1)%x
      var4%y%y%x2(2)%x = 6 * var4%y%y%x2(2)%x
    endif
    var4%y2(1)%y%x%x = 6 * var4%y2(1)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(1)%y%x2(1)%x = 6 * var4%y2(1)%y%x2(1)%x
      var4%y2(1)%y%x2(2)%x = 6 * var4%y2(1)%y%x2(2)%x
    endif
    var4%y2(2)%y%x%x = 6 * var4%y2(2)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(2)%y%x2(1)%x = 6 * var4%y2(2)%y%x2(1)%x
      var4%y2(2)%y%x2(2)%x = 6 * var4%y2(2)%y%x2(2)%x
    endif
  !$omp end target

else if (case == 5) then
  ! Use target enter/exit data + target with explicit map
  !$omp target enter data map(to: var1)
  !$omp target enter data map(to: var1a)
  !$omp target enter data map(to: var2)  ! { dg-warning "35: Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
  !$omp target enter data map(to: var3)  ! { dg-warning "35: Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
  !$omp target enter data map(to: var4)  ! { dg-warning "35: Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }

  !$omp target map(to: var1)
    if (any (var1%x /= [1,2,3,4])) stop 1
    var1%x = 2 * var1%x
  !$omp end target

  !$omp target map(to: var1a)
    if (any (var1a%x /= [-1,-2,-3,-4])) stop 2
    var1a%x = 3 * var1a%x
  !$omp end target

  !$omp target map(to: var2)  ! { dg-warning "24: Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var2%x /= [11,22,33,44])) stop 3
    var2%x = 4 * var2%x
  !$omp end target

  !$omp target map(to: var3)  ! { dg-warning "24: Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var3%x%x /= [111,222,333,444])) stop 4
    var3%x%x = 5 * var3%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var3%x2(1)%x /= 2*[111,222,333,444])) stop 4
      if (any (var3%x2(2)%x /= 3*[111,222,333,444])) stop 4
      var3%x2(1)%x = 5 * var3%x2(1)%x
      var3%x2(2)%x = 5 * var3%x2(2)%x
    endif
  !$omp end target

  !$omp target map(to: var4)  ! { dg-warning "24: Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var4%y%y%x%x /= -1 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y%y%x2(1)%x /= -2 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y%y%x2(2)%x /= -3 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(1)%y%x%x /= -4 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(1)%y%x2(1)%x /= -5 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(1)%y%x2(2)%x /= -6 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(2)%y%x%x /= -7 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(2)%y%x2(1)%x /= -8 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(2)%y%x2(2)%x /= -9 * [1111,2222,3333,4444])) stop 5
    endif
    var4%y%y%x%x = 6 * var4%y%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y%y%x2(1)%x = 6 * var4%y%y%x2(1)%x
      var4%y%y%x2(2)%x = 6 * var4%y%y%x2(2)%x
    endif
    var4%y2(1)%y%x%x = 6 * var4%y2(1)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(1)%y%x2(1)%x = 6 * var4%y2(1)%y%x2(1)%x
      var4%y2(1)%y%x2(2)%x = 6 * var4%y2(1)%y%x2(2)%x
    endif
    var4%y2(2)%y%x%x = 6 * var4%y2(2)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(2)%y%x2(1)%x = 6 * var4%y2(2)%y%x2(1)%x
      var4%y2(2)%y%x2(2)%x = 6 * var4%y2(2)%y%x2(2)%x
    endif
  !$omp end target

  !$omp target exit data map(from: var1)
  !$omp target exit data map(from: var1a)
  !$omp target exit data map(from: var2)  ! { dg-warning "36: Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
  !$omp target exit data map(from: var3)  ! { dg-warning "36: Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
  !$omp target exit data map(from: var4)  ! { dg-warning "36: Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }

else if (case == 6) then
  ! Use target enter/exit data + target with implicit map

  !$omp target enter data map(to: var1)
  !$omp target enter data map(to: var1a)
  !$omp target enter data map(to: var2)  ! { dg-warning "35: Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
  !$omp target enter data map(to: var3)  ! { dg-warning "35: Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
  !$omp target enter data map(to: var4)  ! { dg-warning "35: Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }

  !$omp target
    if (any (var1%x /= [1,2,3,4])) stop 1
    var1%x = 2 * var1%x
  !$omp end target

  !$omp target
    if (any (var1a%x /= [-1,-2,-3,-4])) stop 2
    var1a%x = 3 * var1a%x
  !$omp end target

  !$omp target  ! { dg-warning "Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var2%x /= [11,22,33,44])) stop 3
    var2%x = 4 * var2%x
  !$omp end target

  !$omp target  ! { dg-warning "Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var3%x%x /= [111,222,333,444])) stop 4
    var3%x%x = 5 * var3%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var3%x2(1)%x /= 2*[111,222,333,444])) stop 4
      if (any (var3%x2(2)%x /= 3*[111,222,333,444])) stop 4
      var3%x2(1)%x = 5 * var3%x2(1)%x
      var3%x2(2)%x = 5 * var3%x2(2)%x
    endif
  !$omp end target

  !$omp target  ! { dg-warning "Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }
    if (any (var4%y%y%x%x /= -1 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y%y%x2(1)%x /= -2 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y%y%x2(2)%x /= -3 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(1)%y%x%x /= -4 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(1)%y%x2(1)%x /= -5 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(1)%y%x2(2)%x /= -6 * [1111,2222,3333,4444])) stop 5
    endif
    if (any (var4%y2(2)%y%x%x /= -7 * [1111,2222,3333,4444])) stop 5
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      if (any (var4%y2(2)%y%x2(1)%x /= -8 * [1111,2222,3333,4444])) stop 5
      if (any (var4%y2(2)%y%x2(2)%x /= -9 * [1111,2222,3333,4444])) stop 5
    endif
    var4%y%y%x%x = 6 * var4%y%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y%y%x2(1)%x = 6 * var4%y%y%x2(1)%x
      var4%y%y%x2(2)%x = 6 * var4%y%y%x2(2)%x
    endif
    var4%y2(1)%y%x%x = 6 * var4%y2(1)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(1)%y%x2(1)%x = 6 * var4%y2(1)%y%x2(1)%x
      var4%y2(1)%y%x2(2)%x = 6 * var4%y2(1)%y%x2(2)%x
    endif
    var4%y2(2)%y%x%x = 6 * var4%y2(2)%y%x%x
    if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
      var4%y2(2)%y%x2(1)%x = 6 * var4%y2(2)%y%x2(1)%x
      var4%y2(2)%y%x2(2)%x = 6 * var4%y2(2)%y%x2(2)%x
    endif
  !$omp end target

  !$omp target exit data map(from: var1)
  !$omp target exit data map(from: var1a)
  !$omp target exit data map(from: var2)  ! { dg-warning "36: Mapping of polymorphic list item 'var2' is unspecified behavior \\\[-Wopenmp\\\]" }
  !$omp target exit data map(from: var3)  ! { dg-warning "36: Mapping of polymorphic list item 'var3->x' is unspecified behavior \\\[-Wopenmp\\\]" }
  !$omp target exit data map(from: var4)  ! { dg-warning "36: Mapping of polymorphic list item 'var4\.\[0-9\]+->y->y\.x' is unspecified behavior \\\[-Wopenmp\\\]" }

else
  error stop
end if

if ((case /= 2 .and. case /= 4)  .or. is_shared_mem) then
  ! The target update should have been active, check for the updated values
  if (any (var1%x /= 2 * [1,2,3,4])) stop 11
  if (any (var1a%x /= 3 * [-1,-2,-3,-4])) stop 22
  if (any (var2%x /= 4 * [11,22,33,44])) stop 33

  if (any (var3%x%x /= 5 * [111,222,333,444])) stop 44
  if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
    if (any (var3%x2(1)%x /= 2 * 5 * [111,222,333,444])) stop 44
    if (any (var3%x2(2)%x /= 3 * 5 * [111,222,333,444])) stop 44
  endif

  if (any (var4%y%y%x%x /= -1 * 6 * [1111,2222,3333,4444])) stop 55
  if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
    if (any (var4%y%y%x2(1)%x /= -2 * 6 * [1111,2222,3333,4444])) stop 55
    if (any (var4%y%y%x2(2)%x /= -3 * 6 * [1111,2222,3333,4444])) stop 55
  endif
  if (any (var4%y2(1)%y%x%x /= -4 * 6 * [1111,2222,3333,4444])) stop 55
  if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
    if (any (var4%y2(1)%y%x2(1)%x /= -5 * 6 * [1111,2222,3333,4444])) stop 55
    if (any (var4%y2(1)%y%x2(2)%x /= -6 * 6 * [1111,2222,3333,4444])) stop 55
  endif
  if (any (var4%y2(2)%y%x%x /= -7 * 6 * [1111,2222,3333,4444])) stop 55
  if (is_shared_mem) then  ! For stride data, this accesses the host's _vtab
    if (any (var4%y2(2)%y%x2(1)%x /= -8 * 6 * [1111,2222,3333,4444])) stop 55
    if (any (var4%y2(2)%y%x2(2)%x /= -9 * 6 * [1111,2222,3333,4444])) stop 55
  endif
else
  ! The old host values should still be there as 'to:' created a device copy
  if (any (var1%x /= [1,2,3,4])) stop 12
  if (any (var1a%x /= [-1,-2,-3,-4])) stop 22
  if (any (var2%x /= [11,22,33,44])) stop 33

  if (any (var3%x%x /= [111,222,333,444])) stop 44
  ! .not. is_shared_mem:
  ! if (any (var3%x2(1)%x /= 2*[111,222,333,444])) stop 44
  ! if (any (var3%x2(2)%x /= 3*[111,222,333,444])) stop 44

  if (any (var4%y%y%x%x /= -1 * [1111,2222,3333,4444])) stop 55
  if (any (var4%y%y%x2(1)%x /= -2 * [1111,2222,3333,4444])) stop 55
  if (any (var4%y%y%x2(2)%x /= -3 * [1111,2222,3333,4444])) stop 55
  if (any (var4%y2(1)%y%x%x /= -4 * [1111,2222,3333,4444])) stop 55
  ! .not. is_shared_mem:
  !if (any (var4%y2(1)%y%x2(1)%x /= -5 * [1111,2222,3333,4444])) stop 55
  !if (any (var4%y2(1)%y%x2(2)%x /= -6 * [1111,2222,3333,4444])) stop 55
  if (any (var4%y2(2)%y%x%x /= -7 * [1111,2222,3333,4444])) stop 55
  ! .not. is_shared_mem:
  !if (any (var4%y2(2)%y%x2(1)%x /= -8 * [1111,2222,3333,4444])) stop 55
  !if (any (var4%y2(2)%y%x2(2)%x /= -9 * [1111,2222,3333,4444])) stop 55
end if
if (case_var(100) /= 0) stop 123
end subroutine test

program main
  use omp_lib
  implicit none(type, external)
  interface
    subroutine test(case)
      integer, value :: case
    end
  end interface
  integer :: dev
  call run_it(omp_get_default_device())
  do dev = 0, omp_get_num_devices()
    call run_it(dev)
  end do
  call run_it(omp_initial_device)
!  print *, 'all done'
contains
subroutine run_it(dev)
  integer, value :: dev
!  print *, 'DEVICE', dev
  call omp_set_default_device(dev)
  call test(1)
  call test(2)
  call test(3)
  call test(4)
  call test(5)
  call test(6)
end
end
