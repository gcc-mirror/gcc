/* MPX Wrappers Library
   Copyright (C) 2014 Free Software Foundation, Inc.
   Contributed by Ilya Enkovich (ilya.enkovich@intel.com)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

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

#include "stdlib.h"
#include "string.h"
#include <sys/mman.h>

void *
__mpx_wrapper_malloc (size_t size)
{
  void *p = (void *)malloc (size);
  if (!p) return __bnd_null_ptr_bounds (p);
  return __bnd_set_ptr_bounds (p, size);
}


void *
__mpx_wrapper_mmap (void *addr, size_t length, int prot, int flags,
		    int fd, off_t offset)
{
  void *p = mmap (addr, length, prot, flags, fd, offset);
  if (!p) return __bnd_null_ptr_bounds (p);
  return __bnd_set_ptr_bounds (p, length);
}

void *
__mpx_wrapper_realloc (void *ptr, size_t n)
{
  if (!ptr)
    return __mpx_wrapper_malloc (n);

  /* We don't kwnow how much data is copied by realloc
     and therefore may check only lower bounds.  */
  __bnd_chk_ptr_lbounds (ptr);
  ptr = realloc (ptr, n);

  if (!ptr)
    return __bnd_null_ptr_bounds (ptr);

  return __bnd_set_ptr_bounds (ptr, n);
}

void *
__mpx_wrapper_calloc (size_t n_elements, size_t element_size)
{
  void *p = calloc (n_elements, element_size);
  if (!p)
    return __bnd_null_ptr_bounds (p);
  return __bnd_set_ptr_bounds (p, n_elements * element_size);
}

void *
__mpx_wrapper_memset (void *dstpp, int c, size_t len)
{
  if (len > 0)
    {
      __bnd_chk_ptr_bounds (dstpp, len);
      memset (dstpp, c, len);
    }
  return dstpp;
}

void
__mpx_wrapper_bzero (void *dst, size_t len)
{
  __mpx_wrapper_memset (dst, 0, len);
}

void *
__mpx_wrapper_memmove (void *dst, const void *src, size_t n)
{
  const char *s = (const char*)src;
  char *d = (char*)dst;
  void *ret = dst;
  size_t offset_src = ((size_t) s) & (sizeof (void *) - 1);
  size_t offset_dst = ((size_t) d) & (sizeof (void *) - 1);

  if (n == 0)
    return ret;

  __bnd_chk_ptr_bounds (dst, n);
  __bnd_chk_ptr_bounds (src, n);

  /* Different alignment means that even if
     pointers exist in memory, we don't how
     pointers are aligned and therefore cann't
     copy bounds anyway.  */
  if (offset_src != offset_dst)
    memmove (dst, src, n);
  else
    {
      if (s < d)
	{
	  d += n;
	  s += n;
	  offset_src = (offset_src + n) & (sizeof (void *) -1);
	  while (n-- && offset_src--)
	    *--d = *--s;
	  n++;
	  if (!n)
	    return ret;
	  void **d1 = (void **)d;
	  void **s1 = (void **)s;
	  /* This loop will also copy bounds.  */
	  while (n >= sizeof (void *))
	    {
	      n -= sizeof (void *);
	      *--d1 = *--s1;
	    }
	  s = (char *)s1;
	  d = (char *)d1;
	  while (n--)
	    *--d = *--s;
	}
      else
	{
	  offset_src = sizeof (void *) - offset_src;
	  while (n-- && offset_src--)
	    *d++ = *s++;
	  n++;
	  if (!n)
	    return ret;
	  void **d1 = (void **)d;
	  void **s1 = (void **)s;
	  /* This loop will also copy bounds.  */
	  while (n >= sizeof (void *))
	    {
	      n -= sizeof (void *);
	      *d1++ = *s1++;
	    }
	  s = (char *)s1;
	  d = (char *)d1;
	  while (n--)
	    *d++ = *s++;
	}
    }
  return ret;
}


void *
__mpx_wrapper_memcpy (void *dst, const void *src, size_t n)
{
  return __mpx_wrapper_memmove (dst, src, n);
}

void *
__mpx_wrapper_mempcpy (void *dst, const void *src, size_t n)
{
  return (char *)__mpx_wrapper_memcpy (dst, src, n) + n;
}

char *
__mpx_wrapper_strncat (char *dst, const char *src, size_t n)
{
  size_t dst_size = strlen (dst);
  size_t src_size = strnlen (src, n);

  __bnd_chk_ptr_bounds (dst, dst_size + src_size + 1);
  if (src_size < n)
    __bnd_chk_ptr_bounds (src, src_size + 1);
  else
    __bnd_chk_ptr_bounds (src, src_size);

  strncat (dst, src, n);

  return dst;
}

char *
__mpx_wrapper_strcat (char *dst, const char *src)
{
  size_t dst_size = strlen (dst);
  size_t src_size = strlen (src);

  __bnd_chk_ptr_bounds (dst, dst_size + src_size + 1);
  __bnd_chk_ptr_bounds (src, src_size + 1);

  strcat (dst, src);

  return dst;
}

char *
__mpx_wrapper_stpcpy (char *dst, const char *src)
{
  size_t src_size = strlen (src);

  __bnd_chk_ptr_bounds (dst, src_size + 1);
  __bnd_chk_ptr_bounds (src, src_size + 1);

  memcpy (dst, src, src_size + 1);

  return dst + src_size;
}

char *
__mpx_wrapper_stpncpy (char *dst, const char *src, size_t n)
{
  size_t src_size = strnlen (src, n);
  char *res;

  __bnd_chk_ptr_bounds (dst, n);
  if (src_size < n)
    {
      __bnd_chk_ptr_bounds (src, src_size + 1);
      res = dst + src_size;
    }
  else
    {
      __bnd_chk_ptr_bounds (src, src_size);
      res = dst + n;
    }

  memcpy (dst, src, src_size);
  if (n > src_size)
    memset (dst + src_size, 0, n - src_size);

  return res;
}

char *
__mpx_wrapper_strcpy (char *dst, const char *src)
{
  size_t src_size = strlen (src);

  __bnd_chk_ptr_bounds (dst, src_size + 1);
  __bnd_chk_ptr_bounds (src, src_size + 1);

  memcpy (dst, src, src_size + 1);

  return dst;
}

char *
__mpx_wrapper_strncpy (char *dst, const char *src, size_t n)
{
  size_t src_size = strnlen (src, n);

  __bnd_chk_ptr_bounds (dst, n);
  if (src_size < n)
    __bnd_chk_ptr_bounds (src, src_size + 1);
  else
    __bnd_chk_ptr_bounds (src, src_size);

  memcpy (dst, src, src_size);
  if (n > src_size)
    memset (dst + src_size, 0, n - src_size);

  return dst;
}

size_t
__mpx_wrapper_strlen (const char *s)
{
  size_t length = strlen (s);
  __bnd_chk_ptr_bounds (s, length + 1);
  return length;
}
