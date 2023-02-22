! Check that 'map(alloc:' properly works with
! - deferred-length character strings
! - arrays with array descriptors
! For those, the array descriptor / string length must be mapped with 'to:'

program main
implicit none

type t
  integer :: ic(2:5), ic2
  character(len=11) :: ccstr(3:4), ccstr2
  character(len=11,kind=4) :: cc4str(3:7), cc4str2
  integer, pointer :: pc(:), pc2
  character(len=:), pointer :: pcstr(:), pcstr2
  character(len=:,kind=4), pointer :: pc4str(:), pc4str2
end type t

type(t) :: dt

integer :: ii(5), ii2
character(len=11) :: clstr(-1:1), clstr2
character(len=11,kind=4) :: cl4str(0:3), cl4str2
integer, pointer :: ip(:), ip2
integer, allocatable :: ia(:), ia2
character(len=:), pointer :: pstr(:), pstr2
character(len=:), allocatable :: astr(:), astr2
character(len=:,kind=4), pointer :: p4str(:), p4str2
character(len=:,kind=4), allocatable :: a4str(:), a4str2

allocate(dt%pc(5), dt%pc2)
allocate(character(len=2) :: dt%pcstr(2))
allocate(character(len=4) :: dt%pcstr2)

allocate(character(len=3,kind=4) :: dt%pc4str(2:3))
allocate(character(len=5,kind=4) :: dt%pc4str2)

allocate(ip(5), ip2, ia(8), ia2)
allocate(character(len=2) :: pstr(-2:0))
allocate(character(len=4) :: pstr2)
allocate(character(len=6) :: astr(3:5))
allocate(character(len=8) :: astr2)

allocate(character(len=3,kind=4) :: p4str(2:4))
allocate(character(len=5,kind=4) :: p4str2)
allocate(character(len=7,kind=4) :: a4str(-2:3))
allocate(character(len=9,kind=4) :: a4str2)


! integer :: ic(2:5), ic2

!$omp target enter data map(alloc: dt%ic)
!$omp target map(alloc: dt%ic)
  if (size(dt%ic) /= 4) error stop
  if (lbound(dt%ic, 1) /= 2) error stop
  if (ubound(dt%ic, 1) /= 5) error stop
  dt%ic = [22, 33, 44, 55]
!$omp end target
!$omp target exit data map(from: dt%ic)
if (size(dt%ic) /= 4) error stop
if (lbound(dt%ic, 1) /= 2) error stop
if (ubound(dt%ic, 1) /= 5) error stop
if (any (dt%ic /= [22, 33, 44, 55])) error stop

!$omp target enter data map(alloc: dt%ic2)
!$omp target map(alloc: dt%ic2)
  dt%ic2 = 42
!$omp end target
!$omp target exit data map(from: dt%ic2)
if (dt%ic2 /= 42) error stop


! character(len=11) :: ccstr(3:4), ccstr2

!$omp target enter data map(alloc: dt%ccstr)
!$omp target map(alloc: dt%ccstr)
  if (len(dt%ccstr) /= 11) error stop
  if (size(dt%ccstr) /= 2) error stop
  if (lbound(dt%ccstr, 1) /= 3) error stop
  if (ubound(dt%ccstr, 1) /= 4) error stop
  dt%ccstr = ["12345678901", "abcdefghijk"]
!$omp end target
!$omp target exit data map(from: dt%ccstr)
if (len(dt%ccstr) /= 11) error stop
if (size(dt%ccstr) /= 2) error stop
if (lbound(dt%ccstr, 1) /= 3) error stop
if (ubound(dt%ccstr, 1) /= 4) error stop
if (any (dt%ccstr /= ["12345678901", "abcdefghijk"])) error stop

!$omp target enter data map(alloc: dt%ccstr2)
!$omp target map(alloc: dt%ccstr2)
  if (len(dt%ccstr2) /= 11) error stop
  dt%ccstr2 = "ABCDEFGHIJK"
!$omp end target
!$omp target exit data map(from: dt%ccstr2)
if (len(dt%ccstr2) /= 11) error stop
if (dt%ccstr2 /= "ABCDEFGHIJK") error stop


! character(len=11,kind=4) :: cc4str(3:7), cc4str2

! Value check fails
!$omp target enter data map(alloc: dt%cc4str)
!$omp target map(alloc: dt%cc4str)
  if (len(dt%cc4str) /= 11) error stop
  if (size(dt%cc4str) /= 5) error stop
  if (lbound(dt%cc4str, 1) /= 3) error stop
  if (ubound(dt%cc4str, 1) /= 7) error stop
  dt%cc4str = [4_"12345678901", 4_"abcdefghijk", &
               4_"qerftcea6ds", 4_"a1f9g37ga4.", &
               4_"45ngwj56sj2"]
!$omp end target
!$omp target exit data map(from: dt%cc4str)
if (len(dt%cc4str) /= 11) error stop
if (size(dt%cc4str) /= 5) error stop
if (lbound(dt%cc4str, 1) /= 3) error stop
if (ubound(dt%cc4str, 1) /= 7) error stop
if (dt%cc4str(3) /= 4_"12345678901") error stop
if (dt%cc4str(4) /= 4_"abcdefghijk") error stop
if (dt%cc4str(5) /= 4_"qerftcea6ds") error stop
if (dt%cc4str(6) /= 4_"a1f9g37ga4.") error stop
if (dt%cc4str(7) /= 4_"45ngwj56sj2") error stop

!$omp target enter data map(alloc: dt%cc4str2)
!$omp target map(alloc: dt%cc4str2)
  if (len(dt%cc4str2) /= 11) error stop
  dt%cc4str2 = 4_"ABCDEFGHIJK"
!$omp end target
!$omp target exit data map(from: dt%cc4str2)
if (len(dt%cc4str2) /= 11) error stop
if (dt%cc4str2 /= 4_"ABCDEFGHIJK") error stop


! integer, pointer :: pc(:), pc2
! allocate(dt%pc(5), dt%pc2)

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x00

!$omp target enter data map(alloc: dt%pc)
!$omp target map(alloc: dt%pc)
  if (.not. associated(dt%pc)) error stop
  if (size(dt%pc) /= 5) error stop
  if (lbound(dt%pc, 1) /= 1) error stop
  if (ubound(dt%pc, 1) /= 5) error stop
  dt%pc = [11, 22, 33, 44, 55]
!$omp end target
!$omp target exit data map(from: dt%pc)
if (.not. associated(dt%pc)) error stop
if (size(dt%pc) /= 5) error stop
if (lbound(dt%pc, 1) /= 1) error stop
if (ubound(dt%pc, 1) /= 5) error stop
if (any (dt%pc /= [11, 22, 33, 44, 55])) error stop

!$omp target enter data map(alloc: dt%pc2)
!$omp target map(alloc: dt%pc2)
  if (.not. associated(dt%pc2)) error stop
  dt%pc2 = 99
!$omp end target
!$omp target exit data map(from: dt%pc2)
if (dt%pc2 /= 99) error stop
if (.not. associated(dt%pc2)) error stop


! character(len=:), pointer :: pcstr(:), pcstr2
! allocate(character(len=2) :: dt%pcstr(2))
! allocate(character(len=4) :: dt%pcstr2)

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x00

!$omp target enter data map(alloc: dt%pcstr)
!$omp target map(alloc: dt%pcstr)
  if (.not. associated(dt%pcstr)) error stop
  if (len(dt%pcstr) /= 2) error stop
  if (size(dt%pcstr) /= 2) error stop
  if (lbound(dt%pcstr, 1) /= 1) error stop
  if (ubound(dt%pcstr, 1) /= 2) error stop
  dt%pcstr = ["01", "jk"]
!$omp end target
!$omp target exit data map(from: dt%pcstr)
if (.not. associated(dt%pcstr)) error stop
if (len(dt%pcstr) /= 2) error stop
if (size(dt%pcstr) /= 2) error stop
if (lbound(dt%pcstr, 1) /= 1) error stop
if (ubound(dt%pcstr, 1) /= 2) error stop
if (any (dt%pcstr /= ["01", "jk"])) error stop

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x01

!$omp target enter data map(alloc: dt%pcstr2)
!$omp target map(alloc: dt%pcstr2)
  if (.not. associated(dt%pcstr2)) error stop
  if (len(dt%pcstr2) /= 4) error stop
  dt%pcstr2 = "HIJK"
!$omp end target
!$omp target exit data map(from: dt%pcstr2)
if (.not. associated(dt%pcstr2)) error stop
if (len(dt%pcstr2) /= 4) error stop
if (dt%pcstr2 /= "HIJK") error stop


! character(len=:,kind=4), pointer :: pc4str(:), pc4str2
! allocate(character(len=3,kind=4) :: dt%pc4str(2:3))
! allocate(character(len=5,kind=4) :: dt%pc4str2)

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x00
! structure element when other mapped elements from the same structure weren't mapped together with it
!$omp target enter data map(alloc: dt%pc4str)
!$omp target map(alloc: dt%pc4str)
  if (.not. associated(dt%pc4str)) error stop
  if (len(dt%pc4str) /= 3) error stop
  if (size(dt%pc4str) /= 2) error stop
  if (lbound(dt%pc4str, 1) /= 2) error stop
  if (ubound(dt%pc4str, 1) /= 3) error stop
  dt%pc4str = [4_"456", 4_"tzu"]
!$omp end target
!$omp target exit data map(from: dt%pc4str)
if (.not. associated(dt%pc4str)) error stop
if (len(dt%pc4str) /= 3) error stop
if (size(dt%pc4str) /= 2) error stop
if (lbound(dt%pc4str, 1) /= 2) error stop
if (ubound(dt%pc4str, 1) /= 3) error stop
if (dt%pc4str(2) /= 4_"456") error stop
if (dt%pc4str(3) /= 4_"tzu") error stop

! libgomp: GOMP_target_enter_exit_data unhandled kind 0x01

!$omp target enter data map(alloc: dt%pc4str2)
!$omp target map(alloc: dt%pc4str2)
  if (.not. associated(dt%pc4str2)) error stop
  if (len(dt%pc4str2) /= 5) error stop
  dt%pc4str2 = 4_"98765"
!$omp end target
!$omp target exit data map(from: dt%pc4str2)
if (.not. associated(dt%pc4str2)) error stop
if (len(dt%pc4str2) /= 5) error stop
if (dt%pc4str2 /= 4_"98765") error stop


! integer :: ii(5), ii2

!$omp target enter data map(alloc: ii)
!$omp target map(alloc: ii)
  if (size(ii) /= 5) error stop
  if (lbound(ii, 1) /= 1) error stop
  if (ubound(ii, 1) /= 5) error stop
  ii = [-1, -2, -3, -4, -5]
!$omp end target
!$omp target exit data map(from: ii)
if (size(ii) /= 5) error stop
if (lbound(ii, 1) /= 1) error stop
if (ubound(ii, 1) /= 5) error stop
if (any (ii /= [-1, -2, -3, -4, -5])) error stop

!$omp target enter data map(alloc: ii2)
!$omp target map(alloc: ii2)
  ii2 = -410
!$omp end target
!$omp target exit data map(from: ii2)
if (ii2 /= -410) error stop


! character(len=11) :: clstr(-1:1), clstr2

!$omp target enter data map(alloc: clstr)
!$omp target map(alloc: clstr)
  if (len(clstr) /= 11) error stop
  if (size(clstr) /= 3) error stop
  if (lbound(clstr, 1) /= -1) error stop
  if (ubound(clstr, 1) /= 1) error stop
  clstr = ["12345678901", "abcdefghijk", "ABCDEFGHIJK"]
!$omp end target
!$omp target exit data map(from: clstr)
if (len(clstr) /= 11) error stop
if (size(clstr) /= 3) error stop
if (lbound(clstr, 1) /= -1) error stop
if (ubound(clstr, 1) /= 1) error stop
if (any (clstr /= ["12345678901", "abcdefghijk", "ABCDEFGHIJK"])) error stop

!$omp target enter data map(alloc: clstr2)
!$omp target map(alloc: clstr2)
  if (len(clstr2) /= 11) error stop
  clstr2 = "ABCDEFghijk"
!$omp end target
!$omp target exit data map(from: clstr2)
if (len(clstr2) /= 11) error stop
if (clstr2 /= "ABCDEFghijk") error stop


! character(len=11,kind=4) :: cl4str(0:3), cl4str2

!$omp target enter data map(alloc: cl4str)
!$omp target map(alloc: cl4str)
  if (len(cl4str) /= 11) error stop
  if (size(cl4str) /= 4) error stop
  if (lbound(cl4str, 1) /= 0) error stop
  if (ubound(cl4str, 1) /= 3) error stop
  cl4str = [4_"12345678901", 4_"abcdefghijk", &
            4_"qerftcea6ds", 4_"a1f9g37ga4."]
!$omp end target
!$omp target exit data map(from: cl4str)
if (len(cl4str) /= 11) error stop
if (size(cl4str) /= 4) error stop
if (lbound(cl4str, 1) /= 0) error stop
if (ubound(cl4str, 1) /= 3) error stop
if (cl4str(0) /= 4_"12345678901") error stop
if (cl4str(1) /= 4_"abcdefghijk") error stop
if (cl4str(2) /= 4_"qerftcea6ds") error stop
if (cl4str(3) /= 4_"a1f9g37ga4.") error stop

!$omp target enter data map(alloc: cl4str2)
!$omp target map(alloc: cl4str2)
  if (len(cl4str2) /= 11) error stop
  cl4str2 = 4_"ABCDEFGHIJK"
!$omp end target
!$omp target exit data map(from: cl4str2)
if (len(cl4str2) /= 11) error stop
if (cl4str2 /= 4_"ABCDEFGHIJK") error stop


! allocate(ip(5), ip2, ia(8), ia2)

!$omp target enter data map(alloc: ip)
!$omp target map(alloc: ip)
  if (.not. associated(ip)) error stop
  if (size(ip) /= 5) error stop
  if (lbound(ip, 1) /= 1) error stop
  if (ubound(ip, 1) /= 5) error stop
  ip = [11, 22, 33, 44, 55]
!$omp end target
!$omp target exit data map(from: ip)
if (.not. associated(ip)) error stop
if (size(ip) /= 5) error stop
if (lbound(ip, 1) /= 1) error stop
if (ubound(ip, 1) /= 5) error stop
if (any (ip /= [11, 22, 33, 44, 55])) error stop

!$omp target enter data map(alloc: ip2)
!$omp target map(alloc: ip2)
  if (.not. associated(ip2)) error stop
  ip2 = 99
!$omp end target
!$omp target exit data map(from: ip2)
if (ip2 /= 99) error stop
if (.not. associated(ip2)) error stop


! allocate(ip(5), ip2, ia(8), ia2)

!$omp target enter data map(alloc: ia)
!$omp target map(alloc: ia)
  if (.not. allocated(ia)) error stop
  if (size(ia) /= 8) error stop
  if (lbound(ia, 1) /= 1) error stop
  if (ubound(ia, 1) /= 8) error stop
  ia = [1,2,3,4,5,6,7,8]
!$omp end target
!$omp target exit data map(from: ia)
if (.not. allocated(ia)) error stop
if (size(ia) /= 8) error stop
if (lbound(ia, 1) /= 1) error stop
if (ubound(ia, 1) /= 8) error stop
if (any (ia /= [1,2,3,4,5,6,7,8])) error stop

!$omp target enter data map(alloc: ia2)
!$omp target map(alloc: ia2)
  if (.not. allocated(ia2)) error stop
  ia2 = 102
!$omp end target
!$omp target exit data map(from: ia2)
if (ia2 /= 102) error stop
if (.not. allocated(ia2)) error stop


! character(len=:), pointer :: pstr(:), pstr2
! allocate(character(len=2) :: pstr(-2:0))
! allocate(character(len=4) :: pstr2)

! libgomp: nvptx_alloc error: out of memory

!$omp target enter data map(alloc: pstr)
!$omp target map(alloc: pstr)
  if (.not. associated(pstr)) error stop
  if (len(pstr) /= 2) error stop
  if (size(pstr) /= 3) error stop
  if (lbound(pstr, 1) /= -2) error stop
  if (ubound(pstr, 1) /= 0) error stop
  pstr = ["01", "jk", "aq"]
!$omp end target
!$omp target exit data map(from: pstr)
if (.not. associated(pstr)) error stop
if (len(pstr) /= 2) error stop
if (size(pstr) /= 3) error stop
if (lbound(pstr, 1) /= -2) error stop
if (ubound(pstr, 1) /= 0) error stop
if (any (pstr /= ["01", "jk", "aq"])) error stop

!$omp target enter data map(alloc: pstr2)
!$omp target map(alloc: pstr2)
  if (.not. associated(pstr2)) error stop
  if (len(pstr2) /= 4) error stop
  pstr2 = "HIJK"
!$omp end target
!$omp target exit data map(from: pstr2)
if (.not. associated(pstr2)) error stop
if (len(pstr2) /= 4) error stop
if (pstr2 /= "HIJK") error stop


! character(len=:), allocatable :: astr(:), astr2
! allocate(character(len=6) :: astr(3:5))
! allocate(character(len=8) :: astr2)

! libgomp: nvptx_alloc error: out of memory

!$omp target enter data map(alloc: astr)
!$omp target map(alloc: astr)
  if (.not. allocated(astr)) error stop
  if (len(astr) /= 6) error stop
  if (size(astr) /= 3) error stop
  if (lbound(astr, 1) /= 3) error stop
  if (ubound(astr, 1) /= 5) error stop
  astr = ["01db45", "jk$D%S", "zutg47"]
!$omp end target
!$omp target exit data map(from: astr)
if (.not. allocated(astr)) error stop
if (len(astr) /= 6) error stop
if (size(astr) /= 3) error stop
if (lbound(astr, 1) /= 3) error stop
if (ubound(astr, 1) /= 5) error stop
if (any (astr /= ["01db45", "jk$D%S", "zutg47"])) error stop

! libgomp: nvptx_alloc error: out of memory

!$omp target enter data map(alloc: astr2)
!$omp target map(alloc: astr2)
  if (.not. allocated(astr2)) error stop
  if (len(astr2) /= 8) error stop
  astr2 = "HIJKhijk"
!$omp end target
!$omp target exit data map(from: astr2)
if (.not. allocated(astr2)) error stop
if (len(astr2) /= 8) error stop
if (astr2 /= "HIJKhijk") error stop


! character(len=:,kind=4), pointer :: p4str(:), p4str2
! allocate(character(len=3,kind=4) :: p4str(2:4))
! allocate(character(len=5,kind=4) :: p4str2)

! FAILS with value check

!$omp target enter data map(alloc: p4str)
!$omp target map(alloc: p4str)
  if (.not. associated(p4str)) error stop
  if (len(p4str) /= 3) error stop
  if (size(p4str) /= 3) error stop
  if (lbound(p4str, 1) /= 2) error stop
  if (ubound(p4str, 1) /= 4) error stop
  p4str(:) = [4_"f85", 4_"8af", 4_"A%F"]
!$omp end target
!$omp target exit data map(from: p4str)
if (.not. associated(p4str)) error stop
if (len(p4str) /= 3) error stop
if (size(p4str) /= 3) error stop
if (lbound(p4str, 1) /= 2) error stop
if (ubound(p4str, 1) /= 4) error stop
if (p4str(2)  /= 4_"f85") error stop
if (p4str(3)  /= 4_"8af") error stop
if (p4str(4)  /= 4_"A%F") error stop

!$omp target enter data map(alloc: p4str2)
!$omp target map(alloc: p4str2)
  if (.not. associated(p4str2)) error stop
  if (len(p4str2) /= 5) error stop
  p4str2 = 4_"9875a"
!$omp end target
!$omp target exit data map(from: p4str2)
if (.not. associated(p4str2)) error stop
if (len(p4str2) /= 5) error stop
if (p4str2 /= 4_"9875a") error stop


! character(len=:,kind=4), allocatable :: a4str(:), a4str2
! allocate(character(len=7,kind=4) :: a4str(-2:3))
! allocate(character(len=9,kind=4) :: a4str2)

! libgomp: Trying to map into device [0x1027ba0..0x251050bb9c9ebba0) object when [0x7ffd026e6708..0x7ffd026e6710) is already mapped

!$omp target enter data map(alloc: a4str)
!$omp target map(alloc: a4str)
  if (.not. allocated(a4str)) error stop
  if (len(a4str) /= 7) error stop
  if (size(a4str) /= 6) error stop
  if (lbound(a4str, 1) /= -2) error stop
  if (ubound(a4str, 1) /= 3) error stop
  ! See PR fortran/107508 why '(:)' is required
  a4str(:) = [4_"sf456aq", 4_"3dtzu24", 4_"_4fh7sm", 4_"=ff85s7", 4_"j=8af4d", 4_".,A%Fsz"]
!$omp end target
!$omp target exit data map(from: a4str)
if (.not. allocated(a4str)) error stop
if (len(a4str) /= 7) error stop
if (size(a4str) /= 6) error stop
if (lbound(a4str, 1) /= -2) error stop
if (ubound(a4str, 1) /= 3) error stop
if (a4str(-2) /= 4_"sf456aq") error stop
if (a4str(-1) /= 4_"3dtzu24") error stop
if (a4str(0)  /= 4_"_4fh7sm") error stop
if (a4str(1)  /= 4_"=ff85s7") error stop
if (a4str(2)  /= 4_"j=8af4d") error stop
if (a4str(3)  /= 4_".,A%Fsz") error stop

!$omp target enter data map(alloc: a4str2)
!$omp target map(alloc: a4str2)
  if (.not. allocated(a4str2)) error stop
  if (len(a4str2) /= 9) error stop
  a4str2 = 4_"98765a23d"
!$omp end target
!$omp target exit data map(from: a4str2)
if (.not. allocated(a4str2)) error stop
if (len(a4str2) /= 9) error stop
if (a4str2 /= 4_"98765a23d") error stop


deallocate(dt%pc, dt%pc2)
deallocate(dt%pcstr)
deallocate(dt%pcstr2)

deallocate(dt%pc4str)
deallocate(dt%pc4str2)

deallocate(ip, ip2, ia, ia2)
deallocate(pstr)
deallocate(pstr2)
deallocate(astr)
deallocate(astr2)

deallocate(p4str)
deallocate(p4str2)
deallocate(a4str)
deallocate(a4str2)
end
