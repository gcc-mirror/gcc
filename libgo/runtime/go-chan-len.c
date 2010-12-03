/* go-chan-len.c -- the len function applied to a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "go-assert.h"
#include "channel.h"

/* Return the len function applied to a channel--the number of
   elements in the buffer.  This could be done inline but I'm doing it
   as a function for now to make it easy to change the channel
   structure.  */

size_t
__go_chan_len (struct __go_channel *channel)
{
  int i;
  size_t ret;

  if (channel == NULL)
    return 0;

  i = pthread_mutex_lock (&channel->lock);
  __go_assert (i == 0);

  if (channel->num_entries == 0)
    ret = 0;
  else if (channel->next_fetch == channel->next_store)
    ret = 0;
  else
    ret = ((channel->next_store + channel->num_entries - channel->next_fetch)
	   % channel->num_entries);

  i = pthread_mutex_unlock (&channel->lock);
  __go_assert  (i == 0);

  return ret;
}
