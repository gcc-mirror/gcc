/* Checking macros for unistd functions.
   Copyright (C) 2005, 2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#ifndef _SSP_UNISTD_H
#define _SSP_UNISTD_H 1

#include <ssp.h>
#include_next <unistd.h>

#if __SSP_FORTIFY_LEVEL > 0

#undef read
#undef readlink
#undef getcwd

extern ssize_t __SSP_REDIRECT (__read_alias, (int __fd, void *__buf,
					      size_t __nbytes), read);

extern inline __attribute__((__always_inline__)) ssize_t
read (int __fd, void *__buf, size_t __nbytes)
{
  if (__ssp_bos0 (__buf) != (size_t) -1 && __nbytes > __ssp_bos0 (__buf))
    __chk_fail ();
  return __read_alias (__fd, __buf, __nbytes);
}

extern int __SSP_REDIRECT (__readlink_alias,
			   (const char *__restrict__ __path,
			    char *__restrict__ __buf, size_t __len),
			   readlink);

extern inline __attribute__((__always_inline__)) int
readlink (const char *__restrict__ __path, char *__restrict__ __buf,
	  size_t __len)
{
  if (__ssp_bos (__buf) != (size_t) -1 && __len > __ssp_bos (__buf))
    __chk_fail ();
  return __readlink_alias (__path, __buf, __len);
}

extern char *__SSP_REDIRECT (__getcwd_alias,
			     (char *__buf, size_t __size), getcwd);

extern inline __attribute__((__always_inline__)) char *
getcwd (char *__buf, size_t __size)
{
  if (__ssp_bos (__buf) != (size_t) -1 && __size > __ssp_bos (__buf))
    __chk_fail ();
  return __getcwd_alias (__buf, __size);
}

#endif /* __SSP_FORTIFY_LEVEL > 0 */
#endif /* _SSP_UNISTD_H */
