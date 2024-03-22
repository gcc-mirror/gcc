// Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

#ifndef _VTV_RTS_H
#define _VTV_RTS_H 1

#include <cstdlib>

// These prototypes needs to be kept in sync with the compiler-generated declarations in vtable-class-hierarchy.c
extern void 
__VLTRegisterSet(void**, const void*, std::size_t, std::size_t, void**);

extern void 
__VLTRegisterSetDebug(void**, const void*, std::size_t, std::size_t, void**);

extern void 
__VLTRegisterPair(void**, const void*, size_t, const void*);

extern void 
__VLTRegisterPairDebug(void**, const void*, size_t, const void*, 
		       const char*, const char*);

extern const void*
__VLTVerifyVtablePointer(void**, const void*);

extern const void*
__VLTVerifyVtablePointerDebug(void**, const void*, const char*, const char*);

#endif /* _VTV_RTS_H */
