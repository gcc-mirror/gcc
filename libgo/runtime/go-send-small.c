/* go-send-small.c -- send something 64 bits or smaller on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "go-assert.h"
#include "go-panic.h"
#include "channel.h"

/* Prepare to send something on a channel.  Return true if the channel
   is acquired, false, if it is closed.  FOR_SELECT is true if this
   call is being made after a select statement returned with this
   channel selected.  */

_Bool
__go_send_acquire (struct __go_channel *channel, _Bool for_select)
{
  int i;

  i = pthread_mutex_lock (&channel->lock);
  __go_assert (i == 0);

  while (1)
    {
      /* Check whether the channel is closed.  */
      if (channel->is_closed)
	{
	  ++channel->closed_op_count;
	  if (channel->closed_op_count >= MAX_CLOSED_OPERATIONS)
	    {
	      i = pthread_mutex_unlock (&channel->lock);
	      __go_assert (i == 0);
	      __go_panic_msg ("too many operations on closed channel");
	    }
	  channel->selected_for_send = 0;
	  __go_unlock_and_notify_selects (channel);
	  return 0;
	}

      /* If somebody else has the channel locked for sending, we have
	 to wait.  If FOR_SELECT is true, then we are the one with the
	 lock.  */
      if (!channel->selected_for_send || for_select)
	{
	  if (channel->num_entries == 0)
	    {
	      /* This is a synchronous channel.  If nobody else is
		 waiting to send, we grab the channel and tell the
		 caller to send the data.  We will then wait for a
		 receiver.  */
	      if (!channel->waiting_to_send)
		{
		  __go_assert (channel->next_store == 0);
		  return 1;
		}
	    }
	  else
	    {
	      /* If there is room on the channel, we are OK.  */
	      if ((channel->next_store + 1) % channel->num_entries
		  != channel->next_fetch)
		return 1;
	    }
	}

      /* Wait for something to change, then loop around and try
	 again.  */

      i = pthread_cond_wait (&channel->cond, &channel->lock);
      __go_assert (i == 0);
    }
}

/* Finished sending something on a channel.  */

void
__go_send_release (struct __go_channel *channel)
{
  int i;

  if (channel->num_entries != 0)
    {
      /* This is a buffered channel.  Bump the store count and signal
	 the condition variable.  */
      channel->next_store = (channel->next_store + 1) % channel->num_entries;

      i = pthread_cond_signal (&channel->cond);
      __go_assert (i == 0);
    }
  else
    {
      _Bool synched_with_select;

      /* This is a synchronous channel.  Indicate that we have a value
	 waiting.  */
      channel->next_store = 1;
      channel->waiting_to_send = 1;

      /* Tell everybody else to do something.  This has to be a
	 broadcast because we might have both senders and receivers
	 waiting on the condition, but senders won't send another
	 signal.  */
      i = pthread_cond_broadcast (&channel->cond);
      __go_assert (i == 0);

      /* Wait until the value is received.  */
      synched_with_select = 0;
      while (1)
	{
	  if (channel->next_store == 0)
	    break;

	  /* If nobody is currently waiting to receive, try to synch
	     up with a select.  */
	  if (!channel->waiting_to_receive && !synched_with_select)
	    {
	      if (__go_synch_with_select (channel, 1))
		{
		  synched_with_select = 1;
		  __go_broadcast_to_select (channel);
		  continue;
		}
	    }

	  i = pthread_cond_wait (&channel->cond, &channel->lock);
	  __go_assert (i == 0);
	}

      channel->waiting_to_send = 0;

      /* Using the mutexes should implement a memory barrier.  */

      /* We have to signal again since we cleared the waiting_to_send
	 field.  This has to be a broadcast because both senders and
	 receivers might be waiting, but only senders will be able to
	 act.  */
      i = pthread_cond_broadcast (&channel->cond);
      __go_assert (i == 0);
    }

  channel->selected_for_send = 0;

  __go_unlock_and_notify_selects (channel);
}

/* Send something 64 bits or smaller on a channel.  */

void
__go_send_small (struct __go_channel *channel, uint64_t val, _Bool for_select)
{
  if (channel == NULL)
    __go_panic_msg ("send to nil channel");

  __go_assert (channel->element_size <= sizeof (uint64_t));

  if (!__go_send_acquire (channel, for_select))
    return;

  channel->data[channel->next_store] = val;

  __go_send_release (channel);
}
