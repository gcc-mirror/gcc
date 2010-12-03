/* go-rec-nb-big.c -- nonblocking receive of something big on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "channel.h"

_Bool
__go_receive_nonblocking_big (struct __go_channel* channel, void *val)
{
  size_t alloc_size;
  size_t offset;

  alloc_size = ((channel->element_size + sizeof (uint64_t) - 1)
		/ sizeof (uint64_t));

  int data = __go_receive_nonblocking_acquire (channel);
  if (data != RECEIVE_NONBLOCKING_ACQUIRE_DATA)
    {
      __builtin_memset (val, 0, channel->element_size);
      if (data == RECEIVE_NONBLOCKING_ACQUIRE_NODATA)
	return 0;
      else
	{
	  /* Channel is closed.  */
	  return 1;
	}
    }

  offset = channel->next_fetch * alloc_size;
  __builtin_memcpy (val, &channel->data[offset], channel->element_size);

  __go_receive_release (channel);

  return 1;
}
