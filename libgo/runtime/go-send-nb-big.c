/* go-send-nb-big.c -- nonblocking send of something big on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "channel.h"

_Bool
__go_send_nonblocking_big (struct __go_channel* channel, const void *val)
{
  size_t alloc_size;
  size_t offset;

  alloc_size = ((channel->element_size + sizeof (uint64_t) - 1)
		/ sizeof (uint64_t));

  int data = __go_send_nonblocking_acquire (channel);
  if (data != SEND_NONBLOCKING_ACQUIRE_SPACE)
    return data == SEND_NONBLOCKING_ACQUIRE_CLOSED;

  offset = channel->next_store * alloc_size;
  __builtin_memcpy (&channel->data[offset], val, channel->element_size);

  __go_send_release (channel);

  return 1;
}
