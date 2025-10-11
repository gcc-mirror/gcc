/* Definitions for libga68.
   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   Under Section 7 of GPL version 3, you are granted additional permissions
   described in the GCC Runtime Library Exception, version 3.1, as published by
   the Free Software Foundation.

   You should have received a copy of the GNU General Public License and a copy
   of the GCC Runtime Library Exception along with this program; see the files
   COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef GA68_H
#define GA68_H

#include "config.h"

#include <stddef.h> /* For size_t.  */
#include <stdint.h>
#include <stdarg.h>
#ifdef __has_include
# if __has_include (<sys/types.h>)
#  include <sys/types.h> /* For ssize_t.  */
# endif
#endif

/* ga68-error.c  */

void _libga68_abort (const char *fmt, ...)
  __attribute__ ((__format__ (__printf__, 1, 2), __nonnull__ (1),
		  __noreturn__));

void _libga68_assert (const char *filename, unsigned int lineno);
void _libga68_derefnil (const char *filename, unsigned int lineno);
void _libga68_invalidcharerror (const char *filename, unsigned int lineno,
				int c);

void _libga68_bitsboundserror (const char *filename, unsigned int lineno,
			       ssize_t pos);
void _libga68_unreachable (const char *filename, unsigned int lineno);
void _libga68_lower_bound (const char *filename, unsigned int lineno,
			   ssize_t index, ssize_t lower_bound);
void _libga68_upper_bound (const char *filename, unsigned int lineno,
			   ssize_t index, ssize_t upper_bound);
void _libga68_bounds (const char *filename, unsigned int lineno,
		      ssize_t index, ssize_t lower_bound, ssize_t upper_bound);
void _libga68_dim (const char *filename, unsigned int lineno,
		   size_t dim, size_t index);
void _libga68_bounds_mismatch (const char *filename, unsigned int lineno,
			       size_t dim, ssize_t lb1, ssize_t ub1,
			       ssize_t lb2, ssize_t ub2);

/* ga68-alloc.c  */

void _libga68_init_heap (void);
void *_libga68_malloc (size_t size);
void *_libga68_malloc_internal (size_t size);
void *_libga68_realloc (void *ptr, size_t size);
void *_libga68_realloc_unchecked (void *ptr, size_t size);
void _libga68_free_internal (void *ptr);

/* ga68-standenv.c  */

float _libga68_random (void);
double _libga68_longrandom (void);
long double _libga68_longlongrandom (void);

/* ga68-posix.c  */

int _libga68_posixerrno (void);
void _libga68_posixperror (uint32_t *s, size_t len, size_t stride);
uint32_t *_libga68_posixstrerror (int errnum, size_t *len);
long long int _libga68_posixfsize (int fd);
int _libga68_posixfopen (const uint32_t *pathname, size_t len, size_t stride,
			 unsigned int flags);
int _libga68_posixcreat (uint32_t *pathname, size_t len, size_t stride, uint32_t mode);
int _libga68_posixclose (int fd);
int _libga68_posixargc (void);
uint32_t *_libga68_posixargv (int n, size_t *len);
void _libga68_posixgetenv (uint32_t *s, size_t len, size_t stride,
			   uint32_t **r, size_t *rlen);
void _libga68_posixputs (uint32_t *s, size_t len, size_t stride);
uint32_t _libga68_posixputchar (uint32_t c);
uint32_t _libga68_posixfputc (int fd, uint32_t c);
int _libga68_posixfputs (int fd, uint32_t *s, size_t len, size_t stride);

uint32_t _libga68_posixgetchar (void);
uint32_t _libga68_posixfgetc (int fd);
uint32_t *_libga68_posixfgets (int fd, int nchars, size_t *len);
uint32_t *_libga68_posixgets (int nchars, size_t *len);

int _libga68_posixfconnect (uint32_t *str, size_t len, size_t stride,
			    int port);
long long int _libga68_posixlseek (int fd, long long int offset, int whence);

/* ga68-unistr.c  */

int _libga68_u32_cmp (const uint32_t *s1, size_t stride1,
		      const uint32_t *s2, size_t stride2,
		      size_t n);
int _libga68_u32_cmp2 (const uint32_t *s1, size_t n1, size_t stride1,
		       const uint32_t *s2, size_t n2, size_t stride2);
int _libga68_u8_uctomb (uint8_t *s, uint32_t uc, ptrdiff_t n);
int _libga68_u8_mbtouc (uint32_t *puc, const uint8_t *s, size_t n);
uint8_t *_libga68_u32_to_u8 (const uint32_t *s, size_t n, size_t stride,
			     uint8_t *resultbuf, size_t *lengthp);
uint32_t *_libga68_u8_to_u32 (const uint8_t *s, size_t n,
			      uint32_t *resultbuf, size_t *lengthp);

/* libga68.c  */

extern int _libga68_argc;
extern char **_libga68_argv;

void _libga68_set_exit_status (int status);

#endif /* ! GA68_H */
