/* go-send-big.c -- send something bigger than uint64_t on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "go-panic.h"
#include "channel.h"

void
__go_send_big (struct __go_channel* channel, const void *val, _Bool for_select)
{
  uintptr_t element_size;
  size_t alloc_size;
  size_t offset;

  if (channel == NULL)
    {
      // Block forever.
      __go_select (0, 0, NULL, NULL);
    }

  element_size = channel->element_type->__size;
  alloc_size = (element_size + sizeof (uint64_t) - 1) / sizeof (uint64_t);

  __go_send_acquire (channel, for_select);

  offset = channel->next_store * alloc_size;
  __builtin_memcpy (&channel->data[offset], val, element_size);

  __go_send_release (channel);
}
