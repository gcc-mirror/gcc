// Copyright (C) 2012-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
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

/* This is part of the vtable verification runtime library.  For more
   information about this feature, see the comments in libvtv/vtv_rts.cc.  */

/* The functions in this file are used to create the libvtv_stubs
   library, as part of the vtable verification feature.  When building
   a binary without vtable verification, and linking it with a
   (possibly pre-built third-party) library that was built with
   verification, it is possible that vtable verification will fail due
   to incomplete data (rather than due to corrupt vtable pointers).  In
   this case we need to give programmers a way of turning off the
   vtable verification in their libraries.  They can do so by linking
   with the libvtv_stubs library, which (as you can see) will replace
   the real verification functions with a set of functions that do
   nothing (so no more verification failures/aborts).  */

#include <cstddef>

/* weak symbols on Windows work differently than on Linux. To be able
   to switch vtv on and off on Windows two dlls are built. One with
   the sources from libvtv, the other from these stubs. Depending on
   which dll is placed in the folder of the executable the functions
   from libvtv or the stubs functions are used. */
#if defined (__CYGWIN__) || defined (__MINGW32__)
extern "C"
void
__VLTChangePermission(int);

void
__VLTRegisterSet(void**, const void*, std::size_t, std::size_t,
		 void**);

void
__VLTRegisterPair(void**, const void*, std::size_t,
		  const void*);

const void*
__VLTVerifyVtablePointer(void**, const void*);

void
__VLTRegisterSetDebug(void**, const void*, std::size_t, std::size_t,
		      void**);

void
__VLTRegisterPairDebug(void**, const void*, std::size_t, const void*,
		       const char*, const char*);

const void*
__VLTVerifyVtablePointerDebug(void**, const void*, const char*,
			      const char*);
#else
// Declare as weak for libsupc++, strong definitions are in libvtv.
#if __GXX_WEAK__
extern "C"
void
__VLTChangePermission(int) __attribute__((weak));

void
__VLTRegisterSet(void**, const void*, std::size_t, std::size_t,
		 void**) __attribute__((weak));

void
__VLTRegisterPair(void**, const void*, std::size_t,
		  const void*) __attribute__((weak));

const void*
__VLTVerifyVtablePointer(void**, const void*) __attribute__((weak));

void
__VLTRegisterSetDebug(void**, const void*, std::size_t, std::size_t,
		      void**) __attribute__((weak));

void
__VLTRegisterPairDebug(void**, const void*, std::size_t, const void*,
		       const char*, const char*) __attribute__((weak));

const void*
__VLTVerifyVtablePointerDebug(void**, const void*, const char*,
			      const char*) __attribute__((weak));
#endif
#endif

// Stub definitions.
extern "C"
void
__VLTChangePermission(int)
{ }

void
__VLTRegisterSet(void**, const void*, std::size_t, std::size_t, void**)
{ }

void
__VLTRegisterPair(void**, const void*, std::size_t, const void*)
{ }

const void*
__VLTVerifyVtablePointer(void**, const void* vtable_ptr)
{ return vtable_ptr; }

void
__VLTRegisterSetDebug(void**, const void*, std::size_t, std::size_t, void**)
{ }

void
__VLTRegisterPairDebug(void**, const void*, std::size_t, const void*,
		       const char*, const  char*)
{ }

const void*
__VLTVerifyVtablePointerDebug(void**, const void* vtable_ptr, const char*,
			      const char*)
{ return vtable_ptr; }
