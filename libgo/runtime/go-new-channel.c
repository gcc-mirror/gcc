/* go-new-channel.c -- allocate a new channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "go-alloc.h"
#include "go-assert.h"
#include "go-panic.h"
#include "channel.h"

struct __go_channel*
__go_new_channel (size_t element_size, size_t entries)
{
  struct __go_channel* ret;
  size_t alloc_size;
  int i;

  if ((size_t) (int) entries != entries || entries > (size_t) -1 / element_size)
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
  ret->element_size = element_size;
  ret->closed_op_count = 0;
  ret->waiting_to_send = 0;
  ret->waiting_to_receive = 0;
  ret->selected_for_send = 0;
  ret->selected_for_receive = 0;
  ret->is_closed = 0;
  ret->saw_close = 0;
  ret->select_send_queue = NULL;
  ret->select_receive_queue = NULL;
  ret->select_mutex = NULL;
  ret->select_cond = NULL;
  ret->num_entries = entries;
  ret->next_store = 0;
  ret->next_fetch = 0;
  return ret;
}
