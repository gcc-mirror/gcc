/* go-rec-nb-big.c -- nonblocking receive of something big on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "channel.h"

/* Return true if a value was received, false if not.  */

_Bool
__go_receive_nonblocking_big (struct __go_channel* channel, void *val,
			      _Bool *closed)
{
  uintptr_t element_size;
  size_t alloc_size;
  size_t offset;

  if (channel == NULL)
    {
      if (closed != NULL)
	*closed = 0;
      return 0;
    }

  element_size = channel->element_type->__size;
  alloc_size = (element_size + sizeof (uint64_t) - 1) / sizeof (uint64_t);

  int data = __go_receive_nonblocking_acquire (channel);
  if (data != RECEIVE_NONBLOCKING_ACQUIRE_DATA)
    {
      __builtin_memset (val, 0, element_size);
      if (closed != NULL)
	*closed = data == RECEIVE_NONBLOCKING_ACQUIRE_CLOSED;
      return 0;
    }

  offset = channel->next_fetch * alloc_size;
  __builtin_memcpy (val, &channel->data[offset], element_size);

  __go_receive_release (channel);

  return 1;
}
