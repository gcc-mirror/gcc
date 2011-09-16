/* go-rec-small.c -- receive something smaller than 64 bits on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "go-assert.h"
#include "go-panic.h"
#include "channel.h"

/* This mutex controls access to the selected field of struct
   __go_channel_select.  While this mutex is held, no other mutexes
   may be acquired.  */

pthread_mutex_t __go_select_data_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Try to synchronize with a select waiting on a sychronized channel.
   This is used by a send or receive.  The channel is locked.  This
   returns true if it was able to synch.  */

_Bool
__go_synch_with_select (struct __go_channel *channel, _Bool is_send)
{
  struct __go_channel_select *p;
  int i;

  __go_assert (channel->num_entries == 0);

  i = pthread_mutex_lock (&__go_select_data_mutex);
  __go_assert (i == 0);

  for (p = (is_send
	    ? channel->select_receive_queue
	    : channel->select_send_queue);
       p != NULL;
       p = p->next)
    {
      if (*p->selected == NULL)
	{
	  *p->selected = channel;
	  *p->is_read = !is_send;
	  if (is_send)
	    channel->selected_for_receive = 1;
	  else
	    channel->selected_for_send = 1;
	  break;
	}
    }

  i = pthread_mutex_unlock (&__go_select_data_mutex);
  __go_assert (i == 0);

  /* The caller is responsible for signalling the select condition
     variable so that the other select knows that something has
     changed.  We can't signal it here because we can't acquire the
     select mutex while we hold a channel lock.  */

  return p != NULL;
}

/* If we synch with a select, then we need to signal the select that
   something has changed.  This requires grabbing the select mutex,
   which can only be done when the channel is unlocked.  This routine
   does the signalling.  It is called with the channel locked.  It
   unlocks the channel, broadcasts the signal and relocks the
   channel.  */

void
__go_broadcast_to_select (struct __go_channel *channel)
{
  pthread_mutex_t *select_mutex;
  pthread_cond_t *select_cond;
  int i;

  select_mutex = channel->select_mutex;
  select_cond = channel->select_cond;

  i = pthread_mutex_unlock (&channel->lock);
  __go_assert (i == 0);

  __go_assert (select_mutex != NULL && select_cond != NULL);

  i = pthread_mutex_lock (select_mutex);
  __go_assert (i == 0);

  i = pthread_cond_broadcast (select_cond);
  __go_assert (i == 0);

  i = pthread_mutex_unlock (select_mutex);
  __go_assert (i == 0);

  i = pthread_mutex_lock (&channel->lock);
  __go_assert (i == 0);
}

/* Prepare to receive something on a channel.  Return true if the
   channel is acquired (which implies that there is data available),
   false if it is closed.  */

_Bool
__go_receive_acquire (struct __go_channel *channel, _Bool for_select)
{
  int i;
  _Bool my_wait_lock;
  _Bool synched_with_select;

  my_wait_lock = 0;
  synched_with_select = 0;

  i = pthread_mutex_lock (&channel->lock);
  __go_assert (i == 0);

  while (1)
    {
      _Bool need_broadcast;

      need_broadcast = 0;

      /* Check whether the channel is closed.  */
      if (channel->is_closed
	  && (channel->num_entries == 0
	      ? channel->next_store == 0
	      : channel->next_fetch == channel->next_store))
	{
	  channel->selected_for_receive = 0;
	  __go_unlock_and_notify_selects (channel);
	  return 0;
	}

      /* If somebody else has the channel locked for receiving, we
	 have to wait.  If FOR_SELECT is true, then we are the one
	 with the lock.  */
      if (!channel->selected_for_receive || for_select)
	{
	  if (channel->num_entries == 0)
	    {
	      /* If somebody else is waiting to receive, we have to
		 wait.  */
	      if (!channel->waiting_to_receive || my_wait_lock)
		{
		  _Bool was_marked;

		  /* Lock the channel so that we get to receive
		     next.  */
		  was_marked = channel->waiting_to_receive;
		  channel->waiting_to_receive = 1;
		  my_wait_lock = 1;

		  /* See if there is a value to receive.  */
		  if (channel->next_store > 0)
		    return 1;

		  /* If we haven't already done so, try to synch with
		     a select waiting to send on this channel.  If we
		     have already synched with a select, we are just
		     looping until the select eventually causes
		     something to be sent.  */
		  if (!synched_with_select && !for_select)
		    {
		      if (__go_synch_with_select (channel, 0))
			{
			  synched_with_select = 1;
			  need_broadcast = 1;
			}
		    }

		  /* If we marked the channel as waiting, we need to
		     signal, because something changed.  It needs to
		     be a broadcast since there might be other
		     receivers waiting.  */
		  if (!was_marked)
		    {
		      i = pthread_cond_broadcast (&channel->cond);
		      __go_assert (i == 0);
		    }
		}
	    }
	  else
	    {
	      /* If there is a value on the channel, we are OK.  */
	      if (channel->next_fetch != channel->next_store)
		return 1;
	    }
	}

      /* If we just synched with a select, then we need to signal the
	 select condition variable.  We can only do that if we unlock
	 the channel.  So we need to unlock, signal, lock, and go
	 around the loop again without waiting.  */
      if (need_broadcast)
	{
	  __go_broadcast_to_select (channel);
	  continue;
	}

      /* Wait for something to change, then loop around and try
	 again.  */

      i = pthread_cond_wait (&channel->cond, &channel->lock);
      __go_assert (i == 0);
    }
}

/* Finished receiving something on a channel.  */

void
__go_receive_release (struct __go_channel *channel)
{
  int i;

  if (channel->num_entries != 0)
    channel->next_fetch = (channel->next_fetch + 1) % channel->num_entries;
  else
    {
      /* For a synchronous receiver, we tell the sender that we picked
	 up the value by setting the next_store field back to 0.
	 Using the mutexes should implement a memory barrier.  */
      __go_assert (channel->next_store == 1);
      channel->next_store = 0;

      channel->waiting_to_receive = 0;
    }

  channel->selected_for_receive = 0;

  /* This is a broadcast to make sure that a synchronous sender sees
     it.  */
  i = pthread_cond_broadcast (&channel->cond);
  __go_assert (i == 0);

  __go_unlock_and_notify_selects (channel);
}

/* Unlock a channel and notify any waiting selects that something
   happened.  */

void
__go_unlock_and_notify_selects (struct __go_channel *channel)
{
  pthread_mutex_t* select_mutex;
  pthread_cond_t* select_cond;
  int i;

  select_mutex = channel->select_mutex;
  select_cond = channel->select_cond;

  i = pthread_mutex_unlock (&channel->lock);
  __go_assert (i == 0);

  if (select_mutex != NULL)
    {
      i = pthread_mutex_lock (select_mutex);
      __go_assert (i == 0);
      i = pthread_cond_broadcast (select_cond);
      __go_assert (i == 0);
      i = pthread_mutex_unlock (select_mutex);
      __go_assert (i == 0);
    }
}

/* Receive something 64 bits or smaller on a channel.  */

uint64_t
__go_receive_small_closed (struct __go_channel *channel, _Bool for_select,
			   _Bool *received)
{
  uintptr_t element_size;
  uint64_t ret;

  if (channel == NULL)
    {
      /* Block forever.  */
      __go_select (0, 0, NULL, NULL);
    }

  element_size = channel->element_type->__size;
  __go_assert (element_size <= sizeof (uint64_t));

  if (!__go_receive_acquire (channel, for_select))
    {
      if (received != NULL)
	*received = 0;
      return 0;
    }

  ret = channel->data[channel->next_fetch];

  __go_receive_release (channel);

  if (received != NULL)
    *received = 1;

  return ret;
}

/* Called by the compiler.  */

uint64_t
__go_receive_small (struct __go_channel *channel, _Bool for_select)
{
  return __go_receive_small_closed (channel, for_select, NULL);
}
