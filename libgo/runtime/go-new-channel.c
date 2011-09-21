/* go-new-channel.c -- allocate a new channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>
#include <stdint.h>

#include "go-alloc.h"
#include "go-assert.h"
#include "go-panic.h"
#include "channel.h"

struct __go_channel*
__go_new_channel (const struct __go_type_descriptor *channel_type,
		  uintptr_t entries)
{
  const struct __go_channel_type *ctd;
  const struct __go_type_descriptor *element_type;
  uintptr_t element_size;
  int ientries;
  struct __go_channel* ret;
  size_t alloc_size;
  int i;

  __go_assert (channel_type->__code == GO_CHAN);
  ctd = (const struct __go_channel_type *) channel_type;
  element_type = ctd->__element_type;

  element_size = element_type->__size;

  ientries = (int) entries;
  if (ientries < 0
      || (uintptr_t) ientries != entries
      || (element_size > 0 && entries > (uintptr_t) -1 / element_size))
    __go_panic_msg ("chan size out of range");

  alloc_size = (element_size + sizeof (uint64_t) - 1) / sizeof (uint64_t);

  /* We use a circular buffer which means that when next_fetch ==
     next_store we don't know whether the buffer is empty or full.  So
     we allocate an extra space, and always leave a space open.
     FIXME.  */
  if (entries != 0)
    ++entries;

  ret = (struct __go_channel*) __go_alloc (sizeof (struct __go_channel)
					   + ((entries == 0 ? 1 : entries)
					      * alloc_size
					      * sizeof (uint64_t)));
  i = pthread_mutex_init (&ret->lock, NULL);
  __go_assert (i == 0);
  i = pthread_cond_init (&ret->cond, NULL);
  __go_assert (i == 0);
  ret->element_type = element_type;
  ret->waiting_to_send = 0;
  ret->waiting_to_receive = 0;
  ret->selected_for_send = 0;
  ret->selected_for_receive = 0;
  ret->is_closed = 0;
  ret->select_send_queue = NULL;
  ret->select_receive_queue = NULL;
  ret->select_mutex = NULL;
  ret->select_cond = NULL;
  ret->num_entries = entries;
  ret->next_store = 0;
  ret->next_fetch = 0;
  return ret;
}
