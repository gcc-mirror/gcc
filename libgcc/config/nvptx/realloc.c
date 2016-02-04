/* Implement realloc with the help of the malloc and free wrappers.

   Copyright (C) 2014-2016 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <stddef.h>
#include "nvptx-malloc.h"

void *
__nvptx_realloc (void *ptr, size_t newsz)
{
  if (newsz == 0)
    {
      __nvptx_free (ptr);
      return NULL;
    }
  void *newptr = __nvptx_malloc (newsz);

  size_t oldsz;
  if (ptr == NULL)
    oldsz = 0;
  else
    {
      size_t *sp = __extension__ (size_t *)(ptr - 8);
      oldsz = *sp;
    }
  if (oldsz != 0)
    __builtin_memcpy (newptr, ptr, oldsz > newsz ? newsz : oldsz);

  __nvptx_free (ptr);
  return newptr;
}
