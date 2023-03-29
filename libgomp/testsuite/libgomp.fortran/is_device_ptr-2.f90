! { dg-additional-options "-fdump-tree-original" }
!
! Since OpenMP 5.1, non-TYPE(c_ptr) arguments to is_device_ptr
! map to has_device_ptr - check this!
!
! PR fortran/105318
!
module m
  use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_associated
  implicit none (type, external)
contains
  subroutine one (as, ar, asp, arp, asa, ara, cptr_a)
    integer, target :: AS, AR(5)
    integer, pointer :: ASP, ARP(:)
    integer, allocatable :: ASA, ARA(:)

    type(c_ptr) :: cptr_a

    !$omp target is_device_ptr(as, ar, asp, arp, asa, ara, cptr_a)
      if (.not. c_associated (cptr_a, c_loc(as))) stop 18
      if (as /= 5) stop 19
      if (any (ar /= [1,2,3,4,5])) stop 20
      if (asp /= 9) stop 21
      if (any (arp /= [2,4,6])) stop 22
    !$omp end target
  end

  subroutine two (cptr_v)
    type(c_ptr), value :: cptr_v
    integer, pointer :: xx

    xx => null()
    !$omp target is_device_ptr(cptr_v)
      if (.not. c_associated (cptr_v)) stop 23
      call c_f_pointer (cptr_v, xx)
      if (xx /= 5) stop 24
      xx => null()
    !$omp end target
  end

  subroutine three (os, or, osp, orp, osa, ora, cptr_o)
    integer, optional, target :: OS, OR(5)
    integer, optional, pointer :: OSP, ORP(:)
    integer, optional, allocatable :: OSA, ORA(:)

    type(c_ptr) :: cptr_o

    !$omp target is_device_ptr(os, or, osp, orp, osa, ora, cptr_o)
      if (.not. c_associated (cptr_o, c_loc(os))) stop 25
      if (os /= 5) stop 26
      if (any (or /= [1,2,3,4,5])) stop 27
      if (osp /= 9) stop 28
      if (any (orp /= [2,4,6])) stop 29
    !$omp end target
  end

  subroutine four(NVS, NVSO)
    use omp_lib, only: omp_initial_device, omp_invalid_device
    integer, value :: NVS
    integer, optional, value :: NVSO
    integer :: NS, NR(5)
    logical, volatile :: false_

    false_ = .false.

    !$omp target is_device_ptr (NS, NR, NVS, NVSO) device(omp_initial_device)
      NVS = 5
      NVSO = 5
      NS = 5
      NR(1) = 7
    !$omp end target

    if (false_) then
      !$omp target device(omp_invalid_device)
      !$omp end target
    end if 
  end subroutine

end module m

program main
  use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_associated
  use m
  implicit none (type, external)

  integer, target :: IS, IR(5)
  integer, pointer :: ISP, IRP(:)
  integer, allocatable :: ISA, IRA(:)
  integer :: xxx, xxxx

  type(c_ptr) :: cptr_i

  is = 5
  ir = [1,2,3,4,5]
  allocate(ISP, source=9)
  allocate(IRP, source=[2,4,6])

  !$omp target data    map(is, ir, isp, irp, isa, ira) &
  !$omp&   use_device_ptr(is, ir, isp, irp, isa, ira)

  cptr_i = c_loc(is)
  !$omp target is_device_ptr(is, ir, isp, irp, isa, ira, cptr_i)
    if (.not. c_associated (cptr_i, c_loc(is))) stop 30
    if (is /= 5) stop 31
    if (any (ir /= [1,2,3,4,5])) stop 32
    if (isp /= 9) stop 33
    if (any (irp /= [2,4,6])) stop 34
  !$omp end target

  call one (is, ir, isp, irp, isa, ira, cptr_i)
  call two (cptr_i)
  call three (is, ir, isp, irp, isa, ira, cptr_i)

  !$omp end target data

  call four(xxx, xxxx)
end

! { dg-final { scan-tree-dump-not "use_device_ptr" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(ira\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(isa\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(irp\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(isp\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(ir\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(is\\)" "original" } }

! { dg-final { scan-tree-dump-not "use_device_addr\\(cptr" "original" } }
! { dg-final { scan-tree-dump-not "use_device_ptr\\(o" "original" } }
! { dg-final { scan-tree-dump-not "use_device_ptr\\(a" "original" } }
! { dg-final { scan-tree-dump-not "use_device_ptr\\(i" "original" } }

! { dg-final { scan-tree-dump "is_device_ptr\\(cptr_o\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(ora\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(osa\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(orp\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(osp\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(or\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(os\\)" "original" } }
! { dg-final { scan-tree-dump "is_device_ptr\\(cptr_v\\)" "original" } }
! { dg-final { scan-tree-dump "is_device_ptr\\(cptr_a\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(ara\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(asa\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(arp\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(asp\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(ar\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(as\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(is\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(ir\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(isp\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(irp\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(isa\\)" "original" } }
! { dg-final { scan-tree-dump "use_device_addr\\(ira\\)" "original" } }
! { dg-final { scan-tree-dump "is_device_ptr\\(cptr_i\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(ira\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(isa\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(irp\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(isp\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(ir\\)" "original" } }
! { dg-final { scan-tree-dump "has_device_addr\\(is\\)" "original" } }
