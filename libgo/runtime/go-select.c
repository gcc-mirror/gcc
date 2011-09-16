/* go-select.c -- implement select.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <pthread.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "go-assert.h"
#include "channel.h"

/* __go_select builds an array of these structures.  */

struct select_channel
{
  /* The channel being selected.  */
  struct __go_channel* channel;
  /* If this channel is selected, the value to return.  */
  uintptr_t retval;
  /* If this channel is a duplicate of one which appears earlier in
     the array, this is the array index of the earlier channel.  This
     is -1UL if this is not a dup.  */
  uintptr_t dup_index;
  /* An entry to put on the send or receive queue.  */
  struct __go_channel_select queue_entry;
  /* True if selected for send.  */
  _Bool is_send;
  /* True if channel is ready--it has data to receive or space to
     send.  */
  _Bool is_ready;
};

/* This mutex controls access to __go_select_cond.  This mutex may not
   be acquired if any channel locks are held.  */

static pthread_mutex_t __go_select_mutex = PTHREAD_MUTEX_INITIALIZER;

/* When we have to wait for channels, we tell them to trigger this
   condition variable when they send or receive something.  */

static pthread_cond_t __go_select_cond = PTHREAD_COND_INITIALIZER;

/* Sort the channels by address.  This avoids deadlock when multiple
   selects are running on overlapping sets of channels.  */

static int
channel_sort (const void *p1, const void *p2)
{
  const struct select_channel *c1 = (const struct select_channel *) p1;
  const struct select_channel *c2 = (const struct select_channel *) p2;

  if ((uintptr_t) c1->channel < (uintptr_t) c2->channel)
    return -1;
  else if ((uintptr_t) c1->channel > (uintptr_t) c2->channel)
    return 1;
  else
    return 0;
}

/* Return whether there is an entry on QUEUE which can be used for a
   synchronous send or receive.  */

static _Bool
is_queue_ready (struct __go_channel_select *queue)
{
  int x;

  if (queue == NULL)
    return 0;

  x = pthread_mutex_lock (&__go_select_data_mutex);
  __go_assert (x == 0);

  while (queue != NULL)
    {
      if (*queue->selected == NULL)
	break;
      queue = queue->next;
    }

  x = pthread_mutex_unlock (&__go_select_data_mutex);
  __go_assert (x == 0);

  return queue != NULL;
}

/* Return whether CHAN is ready.  If IS_SEND is true check whether it
   has space to send, otherwise check whether it has a value to
   receive.  */

static _Bool
is_channel_ready (struct __go_channel* channel, _Bool is_send)
{
  if (is_send)
    {
      if (channel->selected_for_send)
	return 0;
      if (channel->is_closed)
	return 1;
      if (channel->num_entries > 0)
	{
	  /* An asynchronous channel is ready for sending if there is
	     room in the buffer.  */
	  return ((channel->next_store + 1) % channel->num_entries
		  != channel->next_fetch);
	}
      else
	{
	  if (channel->waiting_to_send)
	    {
	      /* Some other goroutine is waiting to send on this
		 channel, so we can't.  */
	      return 0;
	    }
	  if (channel->waiting_to_receive)
	    {
	      /* Some other goroutine is waiting to receive a value,
		 so we can send one.  */
	      return 1;
	    }
	  if (is_queue_ready (channel->select_receive_queue))
	    {
	      /* There is a select statement waiting to synchronize
		 with this one.  */
	      return 1;
	    }
	  return 0;
	}
    }
  else
    {
      if (channel->selected_for_receive)
	return 0;
      if (channel->is_closed)
	return 1;
      if (channel->num_entries > 0)
	{
	  /* An asynchronous channel is ready for receiving if there
	     is a value in the buffer.  */
	  return channel->next_fetch != channel->next_store;
	}
      else
	{
	  if (channel->waiting_to_receive)
	    {
	      /* Some other goroutine is waiting to receive from this
		 channel, so it is not ready for us to receive.  */
	      return 0;
	    }
	  if (channel->next_store > 0)
	    {
	      /* There is data on the channel.  */
	      return 1;
	    }
	  if (is_queue_ready (channel->select_send_queue))
	    {
	      /* There is a select statement waiting to synchronize
		 with this one.  */
	      return 1;
	    }
	  return 0;
	}
    }
}

/* Mark a channel as selected.  The channel is locked.  IS_SELECTED is
   true if the channel was selected for us by another goroutine.  We
   set *NEEDS_BROADCAST if we need to broadcast on the select
   condition variable.  Return true if we got it.  */

static _Bool
mark_channel_selected (struct __go_channel *channel, _Bool is_send,
		       _Bool is_selected, _Bool *needs_broadcast)
{
  if (channel->num_entries == 0)
    {
      /* This is a synchronous channel.  If there is no goroutine
	 currently waiting, but there is another select waiting, then
	 we need to tell that select to use this channel.  That may
	 fail--there may be no other goroutines currently waiting--as
	 a third goroutine may already have claimed the select.  */
      if (!is_selected
	  && !channel->is_closed
	  && (is_send
	      ? !channel->waiting_to_receive
	      : channel->next_store == 0))
	{
	  int x;
	  struct __go_channel_select *queue;

	  x = pthread_mutex_lock (&__go_select_data_mutex);
	  __go_assert (x == 0);

	  queue = (is_send
		   ? channel->select_receive_queue
		   : channel->select_send_queue);
	  __go_assert (queue != NULL);

	  while (queue != NULL)
	    {
	      if (*queue->selected == NULL)
		{
		  *queue->selected = channel;
		  *queue->is_read = !is_send;
		  break;
		}
	      queue = queue->next;
	    }

	  x = pthread_mutex_unlock (&__go_select_data_mutex);
	  __go_assert (x == 0);

	  if (queue == NULL)
	    return 0;

	  if (is_send)
	    channel->selected_for_receive = 1;
	  else
	    channel->selected_for_send = 1;

	  /* We are going to have to tell the other select that there
	     is something to do.  */
	  *needs_broadcast = 1;
	}
    }

  if (is_send)
    channel->selected_for_send = 1;
  else
    channel->selected_for_receive = 1;

  return 1;
}

/* Mark a channel to indicate that a select is waiting.  The channel
   is locked.  */

static void
mark_select_waiting (struct select_channel *sc,
		     struct __go_channel **selected_pointer,
		     _Bool *selected_for_read_pointer)
{
  struct __go_channel *channel = sc->channel;
  _Bool is_send = sc->is_send;

  if (channel->num_entries == 0)
    {
      struct __go_channel_select **pp;

      pp = (is_send
	    ? &channel->select_send_queue
	    : &channel->select_receive_queue);

      /* Add an entry to the queue of selects on this channel.  */
      sc->queue_entry.next = *pp;
      sc->queue_entry.selected = selected_pointer;
      sc->queue_entry.is_read = selected_for_read_pointer;

      *pp = &sc->queue_entry;
    }

  channel->select_mutex = &__go_select_mutex;
  channel->select_cond = &__go_select_cond;

  /* We never actually clear the select_mutex and select_cond fields.
     In order to clear them safely, we would need to have some way of
     knowing when no select is waiting for the channel.  Thus we
     introduce a bit of inefficiency for every channel that select
     needs to wait for.  This is harmless other than the performance
     cost.  */
}

/* Remove the entry for this select waiting on this channel.  The
   channel is locked.  We check both queues, because the channel may
   be selected for both reading and writing.  */

static void
clear_select_waiting (struct select_channel *sc,
		      struct __go_channel **selected_pointer)
{
  struct __go_channel *channel = sc->channel;

  if (channel->num_entries == 0)
    {
      _Bool found;
      struct __go_channel_select **pp;

      found = 0;

      for (pp = &channel->select_send_queue; *pp != NULL; pp = &(*pp)->next)
	{
	  if ((*pp)->selected == selected_pointer)
	    {
	      *pp = (*pp)->next;
	      found = 1;
	      break;
	    }
	}

      for (pp = &channel->select_receive_queue; *pp != NULL; pp = &(*pp)->next)
	{
	  if ((*pp)->selected == selected_pointer)
	    {
	      *pp = (*pp)->next;
	      found = 1;
	      break;
	    }
	}

      __go_assert (found);
    }
}

/* Look through the list of channels to see which ones are ready.
   Lock each channels, and set the is_ready flag.  Return the number
   of ready channels.  */

static uintptr_t
lock_channels_find_ready (struct select_channel *channels, uintptr_t count)
{
  uintptr_t ready_count;
  uintptr_t i;

  ready_count = 0;
  for (i = 0; i < count; ++i)
    {
      struct __go_channel *channel = channels[i].channel;
      _Bool is_send = channels[i].is_send;
      uintptr_t dup_index = channels[i].dup_index;
      int x;

      if (channel == NULL)
	continue;

      if (dup_index != (uintptr_t) -1UL)
	{
	  if (channels[dup_index].is_ready)
	    {
	      channels[i].is_ready = 1;
	      ++ready_count;
	    }
	  continue;
	}

      x = pthread_mutex_lock (&channel->lock);
      __go_assert (x == 0);

      if (is_channel_ready (channel, is_send))
	{
	  channels[i].is_ready = 1;
	  ++ready_count;
	}
    }

  return ready_count;
}

/* The channel we are going to select has been forced by some other
   goroutine.  SELECTED_CHANNEL is the channel we will use,
   SELECTED_FOR_READ is whether the other goroutine wants to read from
   the channel.  Note that the channel could be specified multiple
   times in this select, so we must mark each appropriate entry for
   this channel as ready.  Every other channel is marked as not ready.
   All the channels are locked before this routine is called.  This
   returns the number of ready channels.  */

uintptr_t
force_selected_channel_ready (struct select_channel *channels, uintptr_t count,
			      struct __go_channel *selected_channel,
			      _Bool selected_for_read)
{
  uintptr_t ready_count;
  uintptr_t i;

  ready_count = 0;
  for (i = 0; i < count; ++i)
    {
      struct __go_channel *channel = channels[i].channel;
      _Bool is_send = channels[i].is_send;

      if (channel == NULL)
	continue;

      if (channel != selected_channel
	  || (is_send ? !selected_for_read : selected_for_read))
	channels[i].is_ready = 0;
      else
	{
	  channels[i].is_ready = 1;
	  ++ready_count;
	}
    }
  __go_assert (ready_count > 0);
  return ready_count;
}

/* Unlock all the channels.  */

static void
unlock_channels (struct select_channel *channels, uintptr_t count)
{
  uintptr_t i;
  int x;

  for (i = 0; i < count; ++i)
    {
      struct __go_channel *channel = channels[i].channel;

      if (channel == NULL)
	continue;

      if (channels[i].dup_index != (uintptr_t) -1UL)
	continue;

      x = pthread_mutex_unlock (&channel->lock);
      __go_assert (x == 0);
    }
}

/* At least one channel is ready.  Randomly pick a channel to return.
   Unlock all the channels.  IS_SELECTED is true if the channel was
   picked for us by some other goroutine.  If SELECTED_POINTER is not
   NULL, remove it from the queue for all the channels.  Return the
   retval field of the selected channel.  This will return 0 if we
   can't use the selected channel, because it relied on synchronizing
   with some other select, and that select already synchronized with a
   different channel.  */

static uintptr_t
unlock_channels_and_select (struct select_channel *channels,
			    uintptr_t count, uintptr_t ready_count,
			    _Bool is_selected,
			    struct __go_channel **selected_pointer)
{
  uintptr_t selected;
  uintptr_t ret;
  _Bool needs_broadcast;
  uintptr_t i;
  int x;

  /* Pick which channel we are going to return.  */
#if defined(HAVE_RANDOM)
  selected = (uintptr_t) random () % ready_count;
#else
  selected = (uintptr_t) rand () % ready_count;
#endif
  ret = 0;
  needs_broadcast = 0;

  /* Look at the channels in reverse order so that we don't unlock a
     duplicated channel until we have seen all its dups.  */
  for (i = 0; i < count; ++i)
    {
      uintptr_t j = count - i - 1;
      struct __go_channel *channel = channels[j].channel;
      _Bool is_send = channels[j].is_send;

      if (channel == NULL)
	continue;

      if (channels[j].is_ready)
	{
	  if (selected == 0)
	    {
	      if (mark_channel_selected (channel, is_send, is_selected,
					 &needs_broadcast))
		ret = channels[j].retval;
	    }

	  --selected;
	}

      if (channels[j].dup_index == (uintptr_t) -1UL)
	{
	  if (selected_pointer != NULL)
	    clear_select_waiting (&channels[j], selected_pointer);

	  x = pthread_mutex_unlock (&channel->lock);
	  __go_assert (x == 0);
	}
    }

  /* The NEEDS_BROADCAST variable is set if we are synchronizing with
     some other select statement.  We can't do the actual broadcast
     until we have unlocked all the channels.  */

  if (needs_broadcast)
    {
      x = pthread_mutex_lock (&__go_select_mutex);
      __go_assert (x == 0);

      x = pthread_cond_broadcast (&__go_select_cond);
      __go_assert (x == 0);

      x = pthread_mutex_unlock (&__go_select_mutex);
      __go_assert (x == 0);
    }

  return ret;
}

/* Mark all channels to show that we are waiting for them.  This is
   called with the select mutex held, but none of the channels are
   locked.  This returns true if some channel was found to be
   ready.  */

static _Bool
mark_all_channels_waiting (struct select_channel* channels, uintptr_t count,
			   struct __go_channel **selected_pointer,
			   _Bool *selected_for_read_pointer)
{
  _Bool ret;
  int x;
  uintptr_t i;

  ret = 0;
  for (i = 0; i < count; ++i)
    {
      struct __go_channel *channel = channels[i].channel;
      _Bool is_send = channels[i].is_send;

      if (channel == NULL)
	continue;

      if (channels[i].dup_index != (uintptr_t) -1UL)
	{
	  uintptr_t j;

	  /* A channel may be selected for both read and write.  */
	  if (channels[channels[i].dup_index].is_send == is_send)
	    continue;
	  else
	    {
	      for (j = channels[i].dup_index + 1; j < i; ++j)
		{
		  if (channels[j].channel == channel
		      && channels[j].is_send == is_send)
		    break;
		}
	      if (j < i)
		continue;
	    }
	}

      x = pthread_mutex_lock (&channel->lock);
      __go_assert (x == 0);

      /* To avoid a race condition, we have to check again whether the
	 channel is ready.  It may have become ready since we did the
	 first set of checks but before we acquired the select mutex.
	 If we don't check here, we could sleep forever on the select
	 condition variable.  */
      if (is_channel_ready (channel, is_send))
	ret = 1;

      /* If SELECTED_POINTER is NULL, then we have already marked the
	 channel as waiting.  */
      if (selected_pointer != NULL)
	mark_select_waiting (&channels[i], selected_pointer,
			     selected_for_read_pointer);

      x = pthread_mutex_unlock (&channel->lock);
      __go_assert (x == 0);
    }

  return ret;
}

/* Implement select.  This is called by the compiler-generated code
   with pairs of arguments: a pointer to a channel, and an int which
   is non-zero for send, zero for receive.  */

uintptr_t
__go_select (uintptr_t count, _Bool has_default,
	     struct __go_channel **channel_args, _Bool *is_send_args)
{
  struct select_channel stack_buffer[16];
  struct select_channel *allocated_buffer;
  struct select_channel *channels;
  uintptr_t i;
  int x;
  struct __go_channel *selected_channel;
  _Bool selected_for_read;
  _Bool is_queued;

  if (count < sizeof stack_buffer / sizeof stack_buffer[0])
    {
      channels = &stack_buffer[0];
      allocated_buffer = NULL;
    }
  else
    {
      allocated_buffer = ((struct select_channel *)
			  malloc (count * sizeof (struct select_channel)));
      channels = allocated_buffer;
    }

  for (i = 0; i < count; ++i)
    {
      struct __go_channel *channel_arg = channel_args[i];
      _Bool is_send = is_send_args[i];

      channels[i].channel = (struct __go_channel*) channel_arg;
      channels[i].retval = i + 1;
      channels[i].dup_index = (uintptr_t) -1UL;
      channels[i].queue_entry.next = NULL;
      channels[i].queue_entry.selected = NULL;
      channels[i].is_send = is_send;
      channels[i].is_ready = 0;
    }

  qsort (channels, count, sizeof (struct select_channel), channel_sort);

  for (i = 0; i < count; ++i)
    {
      uintptr_t j;

      for (j = 0; j < i; ++j)
	{
	  if (channels[j].channel == channels[i].channel)
	    {
	      channels[i].dup_index = j;
	      break;
	    }
	}
    }

  /* SELECT_CHANNEL is used to select synchronized channels.  If no
     channels are ready, we store a pointer to this variable on the
     select queue for each synchronized channel.  Because the variable
     may be set by channel operations running in other goroutines,
     SELECT_CHANNEL may only be accessed when all the channels are
     locked and/or when the select_data_mutex is locked.  */
  selected_channel = NULL;

  /* SELECTED_FOR_READ is set to true if SELECTED_CHANNEL was set by a
     goroutine which wants to read from the channel.  The access
     restrictions for this are like those for SELECTED_CHANNEL.  */
  selected_for_read = 0;

  /* IS_QUEUED is true if we have queued up this select on the queues
     for any associated synchronous channels.  We only do this if no
     channels are ready the first time around the loop.  */
  is_queued = 0;

  while (1)
    {
      int ready_count;
      _Bool is_selected;

      /* Lock all channels, identify which ones are ready.  */
      ready_count = lock_channels_find_ready (channels, count);

      /* All the channels are locked, so we can look at
	 SELECTED_CHANNEL.  If it is not NULL, then our choice has
	 been forced by some other goroutine.  This can only happen
	 after the first time through the loop.  */
      is_selected = selected_channel != NULL;
      if (is_selected)
	ready_count = force_selected_channel_ready (channels, count,
						    selected_channel,
						    selected_for_read);

      if (ready_count > 0)
	{
	  uintptr_t ret;

	  ret = unlock_channels_and_select (channels, count, ready_count,
					    is_selected,
					    (is_queued
					     ? &selected_channel
					     : NULL));

	  /* If RET is zero, it means that the channel we picked
	     turned out not to be ready, because some other select
	     grabbed it during our traversal.  Loop around and try
	     again.  */
	  if (ret == 0)
	    {
	      is_queued = 0;
	      /* We are no longer on any channel queues, so it is safe
		 to touch SELECTED_CHANNEL here.  It must be NULL,
		 because otherwise that would somebody has promised to
		 synch up with us and then failed to do so.  */
	      __go_assert (selected_channel == NULL);
	      continue;
	    }

	  if (allocated_buffer != NULL)
	    free (allocated_buffer);

	  return ret;
	}

      /* No channels were ready.  */

      unlock_channels (channels, count);

      if (has_default)
	{
	  /* Use the default clause.  */
	  if (allocated_buffer != NULL)
	    free (allocated_buffer);
	  return 0;
	}

      /* This is a blocking select.  Grab the select lock, tell all
	 the channels to notify us when something happens, and wait
	 for something to happen.  */

      x = pthread_mutex_lock (&__go_select_mutex);
      __go_assert (x == 0);

      /* Check whether CHANNEL_SELECTED was set while the channels
	 were unlocked.  If it was set, then we can simply loop around
	 again.  We need to check this while the select mutex is held.
	 It is possible that something will set CHANNEL_SELECTED while
	 we mark the channels as waiting.  If this happens, that
	 goroutine is required to signal the select condition
	 variable, which means acquiring the select mutex.  Since we
	 have the select mutex locked ourselves, we can not miss that
	 signal.  */

      x = pthread_mutex_lock (&__go_select_data_mutex);
      __go_assert (x == 0);

      is_selected = selected_channel != NULL;

      x = pthread_mutex_unlock (&__go_select_data_mutex);
      __go_assert (x == 0);

      if (!is_selected)
	{
	  /* Mark the channels as waiting, and check whether they have
	     become ready.  */
	  if (!mark_all_channels_waiting (channels, count,
					  (is_queued
					   ? NULL
					   : &selected_channel),
					  (is_queued
					   ? NULL
					   : &selected_for_read)))
	    {
	      x = pthread_cond_wait (&__go_select_cond, &__go_select_mutex);
	      __go_assert (x == 0);
	    }

	  is_queued = 1;
	}

      x = pthread_mutex_unlock (&__go_select_mutex);
      __go_assert (x == 0);
    }
}
