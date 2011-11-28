/* go-rec-nb-small.c -- nonblocking receive of something smal on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "runtime.h"
#include "go-assert.h"
#include "go-panic.h"
#include "channel.h"

/* Prepare to receive something on a nonblocking channel.  */

int
__go_receive_nonblocking_acquire (struct __go_channel *channel)
{
  int i;
  _Bool has_data;

  i = pthread_mutex_lock (&channel->lock);
  __go_assert (i == 0);

  while (channel->selected_for_receive)
    runtime_cond_wait (&channel->cond, &channel->lock);

  if (channel->is_closed
      && (channel->num_entries == 0
	  ? channel->next_store == 0
	  : channel->next_fetch == channel->next_store))
    {
      __go_unlock_and_notify_selects (channel);
      return RECEIVE_NONBLOCKING_ACQUIRE_CLOSED;
    }

  if (channel->num_entries > 0)
    has_data = channel->next_fetch != channel->next_store;
  else
    {
      if (channel->waiting_to_receive)
	{
	  /* Some other goroutine is already waiting for data on this
	     channel, so we can't pick it up.  */
	  has_data = 0;
	}
      else if (channel->next_store > 0)
	{
	  /* There is data on the channel.  */
	  has_data = 1;
	}
      else if (__go_synch_with_select (channel, 0))
	{
	  /* We synched up with a select sending data, so there will
	     be data for us shortly.  Tell the select to go, and then
	     wait for the data.  */
	  __go_broadcast_to_select (channel);

	  while (channel->next_store == 0)
	    runtime_cond_wait (&channel->cond, &channel->lock);

	  has_data = 1;
	}
      else
	{
	  /* Otherwise there is no data.  */
	  has_data = 0;
	}

      if (has_data)
	{
	  channel->waiting_to_receive = 1;
	  __go_assert (channel->next_store == 1);
	}
    }

  if (!has_data)
    {
      i = pthread_mutex_unlock (&channel->lock);
      __go_assert (i == 0);
      return RECEIVE_NONBLOCKING_ACQUIRE_NODATA;
    }

  return RECEIVE_NONBLOCKING_ACQUIRE_DATA;
}

/* Receive something 64 bits or smaller on a nonblocking channel.  */

struct __go_receive_nonblocking_small
__go_receive_nonblocking_small (struct __go_channel *channel)
{
  uintptr_t element_size;
  struct __go_receive_nonblocking_small ret;

  if (channel == NULL)
    {
      ret.__val = 0;
      ret.__success = 0;
      ret.__closed = 0;
      return ret;
    }

  element_size = channel->element_type->__size;
  __go_assert (element_size <= sizeof (uint64_t));

  int data = __go_receive_nonblocking_acquire (channel);
  if (data != RECEIVE_NONBLOCKING_ACQUIRE_DATA)
    {
      ret.__val = 0;
      ret.__success = 0;
      ret.__closed = data == RECEIVE_NONBLOCKING_ACQUIRE_CLOSED;
      return ret;
    }

  ret.__val = channel->data[channel->next_fetch];

  __go_receive_release (channel);

  ret.__success = 1;
  ret.__closed = 0;

  return ret;
}
