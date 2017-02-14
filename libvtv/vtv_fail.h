// Copyright (C) 2012-2017 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.

// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#ifndef _VTV_FAIL_H
#define _VTV_FAIL_H 1

/* __vtv_really_fail prints a backtrace and a memory dump, then calls
  abort.  It is here for programmers to call, presumably from
  __vtv_verify_fail, if they choose to overwrite the standard
  __vtv_verify_fail with one of their own.  Programmers should NOT
  attempt to rewrite __vtv_really_fail. */

extern void
__vtv_really_fail (const char *fail_msg)
  __attribute__ ((visibility ("default"), noreturn, nothrow));

/* __vtv_verify_fail is the function that gets called if the vtable
  verification code discovers a vtable pointer that it cannot verify
  as valid.  Normally __vtv_verify_fail calls __vtv_really_fail.
  However programmers can write and link in their own version of
  __vtv_verify_fail, if they wish to do some kind of secondary
  verification, for example.  The main verification code assumes that
  IF __vtv_verify_fail returns, then some kind of secondary
  verification was done AND that the secondary verification succeeded,
  i.e. that the vtable pointer is actually valid and ok to use.  If
  the secondary verification fails, then __vtv_verify_fail should not
  return.  */

extern void
__vtv_verify_fail (void **data_set_ptr, const void *vtbl_pointer)
  __attribute__((visibility ("default"), nothrow));

extern void
__vtv_verify_fail_debug (void **data_set_ptr, const void *vtbl_pointer,
			 const char *debug_msg)
  __attribute__((visibility ("default"), nothrow));

#endif /* _VTV_FAIL_H */
