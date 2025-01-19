/* GNU Objective-C Extern helpers for Win32.
   Copyright (C) 2004-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#ifndef __objc_decls_INCLUDE_GNU
#define __objc_decls_INCLUDE_GNU

#if defined (_WIN32) || defined (__WIN32__) || defined (WIN32)

#  ifdef DLL_EXPORT /* defined by libtool (if required) */
#    define objc_EXPORT extern
#    define objc_DECLARE
#  else
#    define objc_EXPORT  extern __declspec(dllimport)
#    define objc_DECLARE extern __declspec(dllimport)
#  endif

#else

#  define objc_EXPORT  extern
#  define objc_DECLARE 

#endif

#endif /* __objc_decls_INCLUDE_GNU */
