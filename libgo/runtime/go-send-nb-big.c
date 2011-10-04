/* go-send-nb-big.c -- nonblocking send of something big on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "channel.h"

_Bool
__go_send_nonblocking_big (struct __go_channel* channel, const void *val)
{
  uintptr_t element_size;
  size_t alloc_size;
  size_t offset;

  if (channel == NULL)
    return 0;

  element_size = channel->element_type->__size;
  alloc_size = (element_size + sizeof (uint64_t) - 1) / sizeof (uint64_t);

  if (!__go_send_nonblocking_acquire (channel))
    return 0;

  offset = channel->next_store * alloc_size;
  __builtin_memcpy (&channel->data[offset], val, element_size);

  __go_send_release (channel);

  return 1;
}
