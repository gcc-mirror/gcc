! Check that 'map(alloc:' properly works with
! - deferred-length character strings
! - arrays with array descriptors
! For those, the array descriptor / string length must be mapped with 'to:'

program main
implicit none

type t
  integer :: ic(2:5)
  character(len=11) :: ccstr(3:4)
  character(len=11,kind=4) :: cc4str(3:7)
  integer, pointer :: pc(:)
  character(len=:), pointer :: pcstr(:)
  character(len=:,kind=4), pointer :: pc4str(:)
end type t

type(t) :: dt

integer :: ii(5)
character(len=11) :: clstr(-1:1)
character(len=11,kind=4) :: cl4str(0:3)
integer, pointer :: ip(:)
integer, allocatable :: ia(:)
character(len=:), pointer :: pstr(:)
character(len=:), allocatable :: astr(:)
character(len=:,kind=4), pointer :: p4str(:)
character(len=:,kind=4), allocatable :: a4str(:)

allocate(dt%pc(5))
allocate(character(len=2) :: dt%pcstr(2))

allocate(character(len=3,kind=4) :: dt%pc4str(2:3))

allocate(ip(5), ia(8))
allocate(character(len=2) :: pstr(-2:0))
allocate(character(len=6) :: astr(3:5))

allocate(character(len=3,kind=4) :: p4str(2:4))
allocate(character(len=7,kind=4) :: a4str(-2:3))


! integer :: ic(2:5)

!$omp target enter data map(alloc: dt%ic(3:5))
dt%ic(2) = 22
!$omp target map(alloc: dt%ic(3:5))
  if (size(dt%ic) /= 4) error stop
  if (lbound(dt%ic, 1) /= 2) error stop
  if (ubound(dt%ic, 1) /= 5) error stop
  dt%ic(3:5) = [33, 44, 55]
!$omp end target
!$omp target exit data map(from: dt%ic(3:5))
if (size(dt%ic) /= 4) error stop
if (lbound(dt%ic, 1) /= 2) error stop
if (ubound(dt%ic, 1) /= 5) error stop
if (any (dt%ic /= [22, 33, 44, 55])) error stop

! character(len=11) :: ccstr(3:4)

!$omp target enter data map(alloc: dt%ccstr(4:4))
dt%ccstr(3) = "12345678901"
!$omp target map(alloc: dt%ccstr(4:4))
  if (len(dt%ccstr) /= 11) error stop
  if (size(dt%ccstr) /= 2) error stop
  if (lbound(dt%ccstr, 1) /= 3) error stop
  if (ubound(dt%ccstr, 1) /= 4) error stop
  dt%ccstr(4:4) = ["abcdefghijk"]
!$omp end target
!$omp target exit data map(from: dt%ccstr(4:4))
if (len(dt%ccstr) /= 11) error stop
if (size(dt%ccstr) /= 2) error stop
if (lbound(dt%ccstr, 1) /= 3) error stop
if (ubound(dt%ccstr, 1) /= 4) error stop
if (any (dt%ccstr /= ["12345678901", "abcdefghijk"])) error stop


! character(len=11,kind=4) :: cc4str(3:7)

! Value check fails
!$omp target enter data map(alloc: dt%cc4str(4:7))
dt%cc4str(3) = 4_"12345678901"
!$omp target map(alloc: dt%cc4str(4:7))
  if (len(dt%cc4str) /= 11) error stop
  if (size(dt%cc4str) /= 5) error stop
  if (lbound(dt%cc4str, 1) /= 3) error stop
  if (ubound(dt%cc4str, 1) /= 7) error stop
  dt%cc4str(4:7) = [4_"abcdefghijk", &
               4_"qerftcea6ds", 4_"a1f9g37ga4.", &
               4_"45ngwj56sj2"]
!$omp end target
!$omp target exit data map(from: dt%cc4str(4:7))
if (len(dt%cc4str) /= 11) error stop
if (size(dt%cc4str) /= 5) error stop
if (lbound(dt%cc4str, 1) /= 3) error stop
if (ubound(dt%cc4str, 1) /= 7) error stop
if (dt%cc4str(3) /= 4_"12345678901") error stop
if (dt%cc4str(4) /= 4_"abcdefghijk") error stop
if (dt%cc4str(5) /= 4_"qerftcea6ds") error stop
if (dt%cc4str(6) /= 4_"a1f9g37ga4.") error stop
if (dt%cc4str(7) /= 4_"45ngwj56sj2") error stop

! integer, pointer :: pc(:)
! allocate(dt%pc(5))

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x00

!$omp target enter data map(alloc: dt%pc(2:5))
dt%pc(1) = 11
!$omp target map(alloc: dt%pc(2:5))
  if (.not. associated(dt%pc)) error stop
  if (size(dt%pc) /= 5) error stop
  if (lbound(dt%pc, 1) /= 1) error stop
  if (ubound(dt%pc, 1) /= 5) error stop
  dt%pc(2:5) = [22, 33, 44, 55]
!$omp end target
!$omp target exit data map(from: dt%pc(2:5))
if (.not. associated(dt%pc)) error stop
if (size(dt%pc) /= 5) error stop
if (lbound(dt%pc, 1) /= 1) error stop
if (ubound(dt%pc, 1) /= 5) error stop
if (any (dt%pc /= [11, 22, 33, 44, 55])) error stop

! character(len=:), pointer :: pcstr(:)
! allocate(character(len=2) :: dt%pcstr(2))

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x00

! FIXME: Disabled befause of PR108837
!
!!$omp target enter data map(alloc: dt%pcstr(2:2))
!dt%pcstr(1) = "01"
!!$omp target map(alloc: dt%pcstr(2:2))
!  if (.not. associated(dt%pcstr)) error stop
!  if (len(dt%pcstr) /= 2) error stop
!  if (size(dt%pcstr) /= 2) error stop
!  if (lbound(dt%pcstr, 1) /= 1) error stop
!  if (ubound(dt%pcstr, 1) /= 2) error stop
!  dt%pcstr(2:2) = ["jk"]
!!$omp end target
!!$omp target exit data map(from: dt%pcstr(2:2))
!if (.not. associated(dt%pcstr)) error stop
!if (len(dt%pcstr) /= 2) error stop
!if (size(dt%pcstr) /= 2) error stop
!if (lbound(dt%pcstr, 1) /= 1) error stop
!if (ubound(dt%pcstr, 1) /= 2) error stop
!if (any (dt%pcstr /= ["01", "jk"])) error stop


! character(len=:,kind=4), pointer :: pc4str(:)
! allocate(character(len=3,kind=4) :: dt%pc4str(2:3))

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x00
! structure element when other mapped elements from the same structure weren't mapped together with it

! FIXME: Disabled befause of PR108837
!
!!$omp target enter data map(alloc: dt%pc4str(3:3))
!dt%pc4str(2) = 4_"456"
!!$omp target map(alloc: dt%pc4str(3:3))
!  if (.not. associated(dt%pc4str)) error stop
!  if (len(dt%pc4str) /= 3) error stop
!  if (size(dt%pc4str) /= 2) error stop
!  if (lbound(dt%pc4str, 1) /= 2) error stop
!  if (ubound(dt%pc4str, 1) /= 3) error stop
!  dt%pc4str(3:3) = [4_"tzu"]
!!$omp end target
!!$omp target exit data map(from: dt%pc4str(3:3))
!if (.not. associated(dt%pc4str)) error stop
!if (len(dt%pc4str) /= 3) error stop
!if (size(dt%pc4str) /= 2) error stop
!if (lbound(dt%pc4str, 1) /= 2) error stop
!if (ubound(dt%pc4str, 1) /= 3) error stop
!if (dt%pc4str(2) /= 4_"456") error stop
!if (dt%pc4str(3) /= 4_"tzu") error stop

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x01

! integer :: ii(5)

!$omp target enter data map(alloc: ii(2:5))
ii(1) = -1
!$omp target map(alloc: ii(2:5))
  if (size(ii) /= 5) error stop
  if (lbound(ii, 1) /= 1) error stop
  if (ubound(ii, 1) /= 5) error stop
  ii(2:5) = [-2, -3, -4, -5]
!$omp end target
!$omp target exit data map(from: ii(2:5))
if (size(ii) /= 5) error stop
if (lbound(ii, 1) /= 1) error stop
if (ubound(ii, 1) /= 5) error stop
if (any (ii /= [-1, -2, -3, -4, -5])) error stop


! character(len=11) :: clstr(-1:1)

!$omp target enter data map(alloc: clstr(0:1))
clstr(-1) = "12345678901"
!$omp target map(alloc: clstr(0:1))
  if (len(clstr) /= 11) error stop
  if (size(clstr) /= 3) error stop
  if (lbound(clstr, 1) /= -1) error stop
  if (ubound(clstr, 1) /= 1) error stop
  clstr(0:1) = ["abcdefghijk", "ABCDEFGHIJK"]
!$omp end target
!$omp target exit data map(from: clstr(0:1))
if (len(clstr) /= 11) error stop
if (size(clstr) /= 3) error stop
if (lbound(clstr, 1) /= -1) error stop
if (ubound(clstr, 1) /= 1) error stop
if (any (clstr /= ["12345678901", "abcdefghijk", "ABCDEFGHIJK"])) error stop

! character(len=11,kind=4) :: cl4str(0:3)

!$omp target enter data map(alloc: cl4str(1:3))
cl4str(0) = 4_"12345678901"
!$omp target map(alloc: cl4str(1:3))
  if (len(cl4str) /= 11) error stop
  if (size(cl4str) /= 4) error stop
  if (lbound(cl4str, 1) /= 0) error stop
  if (ubound(cl4str, 1) /= 3) error stop
  cl4str(1:3) = [4_"abcdefghijk", &
            4_"qerftcea6ds", 4_"a1f9g37ga4."]
!$omp end target
!$omp target exit data map(from: cl4str(1:3))
if (len(cl4str) /= 11) error stop
if (size(cl4str) /= 4) error stop
if (lbound(cl4str, 1) /= 0) error stop
if (ubound(cl4str, 1) /= 3) error stop
if (cl4str(0) /= 4_"12345678901") error stop
if (cl4str(1) /= 4_"abcdefghijk") error stop
if (cl4str(2) /= 4_"qerftcea6ds") error stop
if (cl4str(3) /= 4_"a1f9g37ga4.") error stop


! allocate(ip(5), ia(8))

!$omp target enter data map(alloc: ip(2:5))
ip(1) = 11
!$omp target map(alloc: ip(2:5))
  if (.not. associated(ip)) error stop
  if (size(ip) /= 5) error stop
  if (lbound(ip, 1) /= 1) error stop
  if (ubound(ip, 1) /= 5) error stop
  ip(2:5) = [22, 33, 44, 55]
!$omp end target
!$omp target exit data map(from: ip(2:5))
if (.not. associated(ip)) error stop
if (size(ip) /= 5) error stop
if (lbound(ip, 1) /= 1) error stop
if (ubound(ip, 1) /= 5) error stop
if (any (ip /= [11, 22, 33, 44, 55])) error stop

! allocate(ip(5), ia(8))

!$omp target enter data map(alloc: ia(2:8))
ia(1) = 1
!$omp target map(alloc: ia(2:8))
  if (.not. allocated(ia)) error stop
  if (size(ia) /= 8) error stop
  if (lbound(ia, 1) /= 1) error stop
  if (ubound(ia, 1) /= 8) error stop
  ia(2:8) = [2,3,4,5,6,7,8]
!$omp end target
!$omp target exit data map(from: ia(2:8))
if (.not. allocated(ia)) error stop
if (size(ia) /= 8) error stop
if (lbound(ia, 1) /= 1) error stop
if (ubound(ia, 1) /= 8) error stop
if (any (ia /= [1,2,3,4,5,6,7,8])) error stop


! character(len=:), pointer :: pstr(:)
! allocate(character(len=2) :: pstr(-2:0))

! libgomp: nvptx_alloc error: out of memory

! FIXME: array offset wrongly calculated as it uses TYPE_SIZE_UNIT, which is a SAVE_EXPR
!
!!$omp target enter data map(alloc: pstr(-1:0))
!pstr(-2) = "01"
!!$omp target map(alloc: pstr(-1:0))
!  if (.not. associated(pstr)) error stop
!  if (len(pstr) /= 2) error stop
!  if (size(pstr) /= 3) error stop
!  if (lbound(pstr, 1) /= -2) error stop
!  if (ubound(pstr, 1) /= 0) error stop
!  pstr(-1:0) = ["jk", "aq"]
!!$omp end target
!!$omp target exit data map(from: pstr(-1:0))
!if (.not. associated(pstr)) error stop
!if (len(pstr) /= 2) error stop
!if (size(pstr) /= 3) error stop
!if (lbound(pstr, 1) /= -2) error stop
!if (ubound(pstr, 1) /= 0) error stop
!if (any (pstr /= ["01", "jk", "aq"])) error stop


! character(len=:), allocatable :: astr(:)
! allocate(character(len=6) :: astr(3:5))

! libgomp: nvptx_alloc error: out of memory

! FIXME
!!$omp target enter data map(alloc: astr(4:5))
!astr(3) = "01db45"
!!$omp target map(alloc: astr(4:5))
!  if (.not. allocated(astr)) error stop
!  if (len(astr) /= 6) error stop
!  if (size(astr) /= 3) error stop
!  if (lbound(astr, 1) /= 3) error stop
!  if (ubound(astr, 1) /= 5) error stop
!!!  astr(4:5) = ["jk$D%S", "zutg47"]
!!$omp end target
!!!$omp target exit data map(from: astr(4:5))
!!if (.not. allocated(astr)) error stop
!!!if (len(astr) /= 6) error stop
!if (size(astr) /= 3) error stop
!if (lbound(astr, 1) /= 3) error stop
!if (ubound(astr, 1) /= 5) error stop
!if (any (astr /= ["01db45", "jk$D%S", "zutg47"])) error stop
!

! character(len=:,kind=4), pointer :: p4str(:)
! allocate(character(len=3,kind=4) :: p4str(2:4))

! FAILS with value check

! FIXME: array offset wrongly calculated as it uses TYPE_SIZE_UNIT, which is a SAVE_EXPR
!
!!$omp target enter data map(alloc: p4str(3:4))
!p4str(2) = 4_"f85"
!!$omp target map(alloc: p4str(3:4))
!  if (.not. associated(p4str)) error stop
!  if (len(p4str) /= 3) error stop
!  if (size(p4str) /= 3) error stop
!  if (lbound(p4str, 1) /= 2) error stop
!  if (ubound(p4str, 1) /= 4) error stop
!  p4str(3:4) = [4_"8af", 4_"A%F"]
!!$omp end target
!!$omp target exit data map(from: p4str(3:4))
!if (.not. associated(p4str)) error stop
!if (len(p4str) /= 3) error stop
!if (size(p4str) /= 3) error stop
!if (lbound(p4str, 1) /= 2) error stop
!if (ubound(p4str, 1) /= 4) error stop
!if (p4str(2)  /= 4_"f85") error stop
!if (p4str(3)  /= 4_"8af") error stop
!if (p4str(4)  /= 4_"A%F") error stop

! character(len=:,kind=4), allocatable :: a4str(:)
! allocate(character(len=7,kind=4) :: a4str(-2:3))

! libgomp: Trying to map into device [0x1027ba0..0x251050bb9c9ebba0) object when [0x7ffd026e6708..0x7ffd026e6710) is already mapped

! FIXME: Disabled befause of PR108838
!!$omp target enter data map(alloc: a4str(-1:3))
!!a4str(-2) = 4_"sf456aq"
!!$omp target map(alloc: a4str(-1:3))
!  if (.not. allocated(a4str)) error stop
!  if (len(a4str) /= 7) error stop
!  if (size(a4str) /= 6) error stop
!  if (lbound(a4str, 1) /= -2) error stop
!  if (ubound(a4str, 1) /= 3) error stop
!  a4str(-1:3) = [4_"3dtzu24", 4_"_4fh7sm", 4_"=ff85s7", 4_"j=8af4d", 4_".,A%Fsz"]
!!$omp end target
!!$omp target exit data map(from: a4str(-1:3))
!if (.not. allocated(a4str)) error stop
!if (len(a4str) /= 7) error stop
!if (size(a4str) /= 6) error stop
!if (lbound(a4str, 1) /= -2) error stop
!if (ubound(a4str, 1) /= 3) error stop
!if (a4str(-2) /= 4_"sf456aq") error stop
!if (a4str(-1) /= 4_"3dtzu24") error stop
!if (a4str(0)  /= 4_"_4fh7sm") error stop
!if (a4str(1)  /= 4_"=ff85s7") error stop
!if (a4str(2)  /= 4_"j=8af4d") error stop
!if (a4str(3)  /= 4_".,A%Fsz") error stop

deallocate(dt%pc)
deallocate(dt%pcstr)

deallocate(dt%pc4str)

deallocate(ip, ia)
deallocate(pstr)
deallocate(astr)

deallocate(p4str)
deallocate(a4str)
end
