/*
 * Copyright (c) 2000-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include <regions.h>
#include <assert.h>
#include <limits.h>
#include "array.h"

struct array {
  region sameregion r;
  void *sameregion data;
  size_t elemsize;
  type_t elemtype;
  size_t nelems, nalloc;
};

struct array *new_array(region r, size_t initialsize,
			size_t typesize, type_t typeinfo)
{
  struct array *a = ralloc(r, struct array);

  a->r = r;
  a->data = typed_rarrayalloc(r, initialsize, typesize, typeinfo);
  a->elemsize = typesize;
  a->elemtype = typeinfo;
  a->nelems = 0;
  a->nalloc = initialsize;

  return a;
}

void *array_extend(struct array *a, int by)
{
  size_t oldelems = a->nelems;

  if (by < 0)
    assert(((unsigned int)-by) <= a->nelems && by != INT_MIN);
  else if (a->nelems + by > a->nalloc)
    {
      size_t newsize = a->nalloc * 2 + by;
      void *newdata = typed_rarrayalloc(a->r, newsize, a->elemsize, a->elemtype);

      /* XXX: could work harder to support really large array sizes
	 (this code will fail for a->nalloc >= (max(size_t)-by)/2) */
      assert(newsize > a->nalloc); /* die when we get really big */
      typed_rarraycopy(newdata, a->data, a->nelems, a->elemsize, a->elemtype);
      a->data = newdata;
      a->nalloc = newsize;
    }
  a->nelems += by;

  return (char *)a->data + a->elemsize * oldelems;
}

void array_reset(struct array *a)
{
  a->nelems = 0;
}

size_t array_length(struct array *a)
{
  return a->nelems;
}

void *array_data(struct array *a)
{
  return a->data;
}

