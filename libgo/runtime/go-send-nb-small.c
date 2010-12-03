/* go-send-nb-small.c -- nonblocking send of something small on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "go-assert.h"
#include "go-panic.h"
#include "channel.h"

/* Prepare to send something on a nonblocking channel.  */

int
__go_send_nonblocking_acquire (struct __go_channel *channel)
{
  int i;
  _Bool has_space;

  i = pthread_mutex_lock (&channel->lock);
  __go_assert (i == 0);

  while (channel->selected_for_send)
    {
      i = pthread_cond_wait (&channel->cond, &channel->lock);
      __go_assert (i == 0);
    }

  if (channel->is_closed)
    {
      ++channel->closed_op_count;
      if (channel->closed_op_count >= MAX_CLOSED_OPERATIONS)
	{
	  i = pthread_mutex_unlock (&channel->lock);
	  __go_assert (i == 0);
	  __go_panic_msg ("too many operations on closed channel");
	}
      i = pthread_mutex_unlock (&channel->lock);
      __go_assert (i == 0);
      return SEND_NONBLOCKING_ACQUIRE_CLOSED;
    }

  if (channel->num_entries > 0)
      has_space = ((channel->next_store + 1) % channel->num_entries
		   != channel->next_fetch);
  else
    {
      /* This is a synchronous channel.  If somebody is current
	 sending, then we can't send.  Otherwise, see if somebody is
	 waiting to receive, or see if we can synch with a select.  */
      if (channel->waiting_to_send)
	{
	  /* Some other goroutine is currently sending on this
	     channel, which means that we can't.  */
	  has_space = 0;
	}
      else if (channel->waiting_to_receive)
	{
	  /* Some other goroutine is waiting to receive a value, so we
	     can send directly to them.  */
	  has_space = 1;
	}
      else if (__go_synch_with_select (channel, 1))
	{
	  /* We found a select waiting to receive data, so we can send
	     to that.  */
	  __go_broadcast_to_select (channel);
	  has_space = 1;
	}
      else
	{
	  /* Otherwise, we can't send, because nobody is waiting to
	     receive.  */
	  has_space = 0;
	}

      if (has_space)
	{
	  channel->waiting_to_send = 1;
	  __go_assert (channel->next_store == 0);
	}
    }

  if (!has_space)
    {
      i = pthread_mutex_unlock (&channel->lock);
      __go_assert (i == 0);

      return SEND_NONBLOCKING_ACQUIRE_NOSPACE;
    }

  return SEND_NONBLOCKING_ACQUIRE_SPACE;
}

/* Send something 64 bits or smaller on a channel.  */

_Bool
__go_send_nonblocking_small (struct __go_channel *channel, uint64_t val)
{
  __go_assert (channel->element_size <= sizeof (uint64_t));

  int data = __go_send_nonblocking_acquire (channel);
  if (data != SEND_NONBLOCKING_ACQUIRE_SPACE)
    return data == SEND_NONBLOCKING_ACQUIRE_CLOSED;

  channel->data[channel->next_store] = val;

  __go_send_release (channel);

  return 1;
}
