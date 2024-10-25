/* Copyright (C) 2013-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   modify it under the terms of the GNU Library General Public License
   as published by the Free Software Foundation; either version 2, or
   (at your option) any later version.

   In addition to the permissions in the GNU Library General Public
   License, the Free Software Foundation gives you unlimited
   permission to link the compiled version of this file into
   combinations with other programs, and to distribute those
   combinations without any restriction coming from the use of this
   file.  (The Library Public License restrictions do apply in other
   respects; for example, they cover modification of the file, and
   distribution when not linked into a combined executable.)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */


#ifndef __VTV_H__
#define __VTV_H__

/* We could have used an enumeration here but it just makes it more
   difficult for the compiler to generate a call to this.  These are
   used as arguments to the function __VLTChangePermission, declared
   below.  */
#define __VLTP_READ_ONLY  0
#define __VLTP_READ_WRITE 1

#ifdef __cplusplus
extern "C" void __VLTChangePermission (int);
#else
extern void __VLTChangePermission (int);
#endif

#ifdef BIG_PAGE_SIZE
/* TODO - Replace '4096' below with correct big page size.  */
#define VTV_PAGE_SIZE 4096
#else
#if defined(__sun__) && defined(__svr4__) && defined(__sparc__)
#define VTV_PAGE_SIZE 8192
#elif defined(__loongarch_lp64)
/* The page size is configurable by the kernel to be 4, 16 or 64 KiB.
   For now, only the default page size of 16KiB is supported.  */
#define VTV_PAGE_SIZE 16384
#else
#define VTV_PAGE_SIZE 4096
#endif
#endif



#endif /* __VTV_H__ */
