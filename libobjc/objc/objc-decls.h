/* GNU Objective-C Extern helpers for Win32.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with files compiled
   with GCC to produce an executable, this does not cause the resulting
   executable to be covered by the GNU General Public License.  This
   exception does not however invalidate any other reasons why the
   executable file might be covered by the GNU General Public License. */

#ifndef __objc_decls_INCLUDE_GNU
#define __objc_decls_INCLUDE_GNU

#if defined (_WIN32) || defined (__WIN32__) || defined (WIN32)

#    ifdef DLL_EXPORT /* defined by libtool (if required) */
#  define objc_EXPORT  __declspec(dllexport)
#  define objc_DECLARE __declspec(dllexport)
#else
#  define objc_EXPORT  extern __declspec(dllimport)
#  define objc_DECLARE extern __declspec(dllimport)
#endif

#else

#  define objc_EXPORT  extern
#  define objc_DECLARE 

#endif

#endif /* __objc_decls_INCLUDE_GNU */
