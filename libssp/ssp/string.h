/* Checking macros for string functions.
   Copyright (C) 2004, 2005, 2009 Free Software Foundation, Inc.

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


#ifndef _SSP_STRING_H
#define _SSP_STRING_H 1

#include <ssp.h>
#include_next <string.h>

#if __SSP_FORTIFY_LEVEL > 0

#undef memcpy
#undef memmove
#undef memset
#undef strcat
#undef strcpy
#undef strncat
#undef strncpy
#undef mempcpy
#undef stpcpy
#undef bcopy
#undef bzero

#define memcpy(dest, src, len) \
  ((__ssp_bos0 (dest) != (size_t) -1)					\
   ? __builtin___memcpy_chk (dest, src, len, __ssp_bos0 (dest))		\
   : __memcpy_ichk (dest, src, len))
static inline __attribute__((__always_inline__)) void *
__memcpy_ichk (void *__restrict__ __dest, const void *__restrict__ __src,
	       size_t __len)
{
  return __builtin___memcpy_chk (__dest, __src, __len, __ssp_bos0 (__dest));
}


#define memmove(dest, src, len) \
  ((__ssp_bos0 (dest) != (size_t) -1)					\
   ? __builtin___memmove_chk (dest, src, len, __ssp_bos0 (dest))		\
   : __memmove_ichk (dest, src, len))
static inline __attribute__((__always_inline__)) void *
__memmove_ichk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memmove_chk (__dest, __src, __len, __ssp_bos0 (__dest));
}


#define mempcpy(dest, src, len) \
  ((__ssp_bos0 (dest) != (size_t) -1)					\
   ? __builtin___mempcpy_chk (dest, src, len, __ssp_bos0 (dest))	\
   : __mempcpy_ichk (dest, src, len))
static inline __attribute__((__always_inline__)) void *
__mempcpy_ichk (void *__restrict__ __dest, const void *__restrict__ __src,
		size_t __len)
{
  return __builtin___mempcpy_chk (__dest, __src, __len, __ssp_bos0 (__dest));
}


#define memset(dest, ch, len) \
  ((__ssp_bos0 (dest) != (size_t) -1)					\
   ? __builtin___memset_chk (dest, ch, len, __ssp_bos0 (dest))		\
   : __memset_ichk (dest, ch, len))
static inline __attribute__((__always_inline__)) void *
__memset_ichk (void *__dest, int __ch, size_t __len)
{
  return __builtin___memset_chk (__dest, __ch, __len, __ssp_bos0 (__dest));
}

#define bcopy(src, dest, len) ((void) \
 ((__ssp_bos0 (dest) != (size_t) -1)					\
   ? __builtin___memmove_chk (dest, src, len, __ssp_bos0 (dest))	\
   : __memmove_ichk (dest, src, len)))
#define bzero(dest, len) ((void) \
  ((__ssp_bos0 (dest) != (size_t) -1)					\
   ? __builtin___memset_chk (dest, '\0', len, __ssp_bos0 (dest))	\
   : __memset_ichk (dest, '\0', len)))


#define strcpy(dest, src) \
  ((__ssp_bos (dest) != (size_t) -1)					\
   ? __builtin___strcpy_chk (dest, src, __ssp_bos (dest))		\
   : __strcpy_ichk (dest, src))
static inline __attribute__((__always_inline__)) char *
__strcpy_ichk (char *__restrict__ __dest, const char *__restrict__ __src)
{
  return __builtin___strcpy_chk (__dest, __src, __ssp_bos (__dest));
}


#define stpcpy(dest, src) \
  ((__ssp_bos (dest) != (size_t) -1)					\
   ? __builtin___stpcpy_chk (dest, src, __ssp_bos (dest))		\
   : __stpcpy_ichk (dest, src))
static inline __attribute__((__always_inline__)) char *
__stpcpy_ichk (char *__restrict__ __dest, const char *__restrict__ __src)
{
  return __builtin___stpcpy_chk (__dest, __src, __ssp_bos (__dest));
}


#define strncpy(dest, src, len) \
  ((__ssp_bos (dest) != (size_t) -1)					\
   ? __builtin___strncpy_chk (dest, src, len, __ssp_bos (dest))		\
   : __strncpy_ichk (dest, src, len))
static inline __attribute__((__always_inline__)) char *
__strncpy_ichk (char *__restrict__ __dest, const char *__restrict__ __src,
		size_t __len)
{
  return __builtin___strncpy_chk (__dest, __src, __len, __ssp_bos (__dest));
}


#define strcat(dest, src) \
  ((__ssp_bos (dest) != (size_t) -1)					\
   ? __builtin___strcat_chk (dest, src, __ssp_bos (dest))		\
   : __strcat_ichk (dest, src))
static inline __attribute__((__always_inline__)) char *
__strcat_ichk (char *__restrict__ __dest, const char *__restrict__ __src)
{
  return __builtin___strcat_chk (__dest, __src, __ssp_bos (__dest));
}


#define strncat(dest, src, len) \
  ((__ssp_bos (dest) != (size_t) -1)					\
   ? __builtin___strncat_chk (dest, src, len, __ssp_bos (dest))		\
   : __strncat_ichk (dest, src, len))
static inline __attribute__((__always_inline__)) char *
__strncat_ichk (char *__restrict__ __dest, const char *__restrict__ __src,
		size_t __len)
{
  return __builtin___strncat_chk (__dest, __src, __len, __ssp_bos (__dest));
}

#endif /* __SSP_FORTIFY_LEVEL > 0 */
#endif /* _SSP_STRING_H */
