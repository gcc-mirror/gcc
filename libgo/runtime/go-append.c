/* go-append.c -- the go builtin append function.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-assert.h"
#include "go-type.h"
#include "array.h"
#include "runtime.h"
#include "malloc.h"

struct __go_open_array
__go_append (const struct __go_slice_type *type,
	     struct __go_open_array a, struct __go_open_array b)
{
  size_t element_size;
  unsigned int ucount;
  int count;

  if (b.__values == NULL || b.__count == 0)
    return a;

  __go_assert (type->__common.__code == GO_SLICE);
  element_size = type->__element_type->__size;

  ucount = (unsigned int) a.__count + (unsigned int) b.__count;
  count = (int) ucount;
  __go_assert (ucount == (unsigned int) count && count >= a.__count);
  if (count > a.__capacity)
    {
      int m;
      struct __go_open_array n;

      m = a.__capacity;
      if (m == 0)
	m = b.__count;
      else
	{
	  do
	    {
	      if (a.__count < 1024)
		m += m;
	      else
		m += m / 4;
	    }
	  while (m < count);
	}

      n.__values = __go_alloc (m * element_size);
      n.__count = a.__count;
      n.__capacity = m;
      __builtin_memcpy (n.__values, a.__values, n.__count * element_size);

      a = n;
    }

  __builtin_memmove ((char *) a.__values + a.__count * element_size,
		     b.__values, b.__count * element_size);
  a.__count = count;
  return a;
}
