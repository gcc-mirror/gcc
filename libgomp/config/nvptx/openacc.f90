!  OpenACC Runtime Library Definitions.

!  Copyright (C) 2014-2019 Free Software Foundation, Inc.

!  Contributed by Tobias Burnus <burnus@net-b.de>
!              and Mentor Embedded.

!  This file is part of the GNU Offloading and Multi Processing Library
!  (libgomp).

!  Libgomp is free software; you can redistribute it and/or modify it
!  under the terms of the GNU General Public License as published by
!  the Free Software Foundation; either version 3, or (at your option)
!  any later version.

!  Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
!  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
!  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
!  more details.

!  Under Section 7 of GPL version 3, you are granted additional
!  permissions described in the GCC Runtime Library Exception, version
!  3.1, as published by the Free Software Foundation.

!  You should have received a copy of the GNU General Public License and
!  a copy of the GCC Runtime Library Exception along with this program;
!  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
!  <http://www.gnu.org/licenses/>.

! Wrapper functions will be built from openacc.f90.  We use a separate file
! here, because for using ../../openacc.f90, implementations are required for
! all the functions that it wraps, which we currently don't provide, so linking
! would fail.

module openacc_kinds
  use iso_fortran_env, only: int32
  implicit none

  private :: int32
  public :: acc_device_kind

  integer, parameter :: acc_device_kind = int32

  public :: acc_device_none, acc_device_default, acc_device_host
  public :: acc_device_not_host, acc_device_nvidia

  ! Keep in sync with include/gomp-constants.h.
  integer (acc_device_kind), parameter :: acc_device_none = 0
  integer (acc_device_kind), parameter :: acc_device_default = 1
  integer (acc_device_kind), parameter :: acc_device_host = 2
  ! integer (acc_device_kind), parameter :: acc_device_host_nonshm = 3 removed.
  integer (acc_device_kind), parameter :: acc_device_not_host = 4
  integer (acc_device_kind), parameter :: acc_device_nvidia = 5

end module

module openacc_internal
  use openacc_kinds
  implicit none

  interface
    function acc_on_device_h (d)
      import
      integer (acc_device_kind) d
      logical acc_on_device_h
    end function
  end interface

  interface
    function acc_on_device_l (d) &
        bind (C, name = "acc_on_device")
      use iso_c_binding, only: c_int
      integer (c_int) :: acc_on_device_l
      integer (c_int), value :: d
    end function
  end interface
end module

module openacc
  use openacc_kinds
  use openacc_internal
  implicit none

  public :: acc_on_device

  interface acc_on_device
    procedure :: acc_on_device_h
  end interface

end module openacc

function acc_on_device_h (d)
  use openacc_internal, only: acc_on_device_l
  use openacc_kinds
  integer (acc_device_kind) d
  logical acc_on_device_h
  if (acc_on_device_l (d) .eq. 1) then
    acc_on_device_h = .TRUE.
  else
    acc_on_device_h = .FALSE.
  end if
end function
