/* go-close.c -- the builtin close function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-assert.h"
#include "channel.h"

/* Close a channel.  After a channel is closed, sends are no longer
   permitted.  Receives always return zero.  */

void
__go_builtin_close (struct __go_channel *channel)
{
  int i;

  i = pthread_mutex_lock (&channel->lock);
  __go_assert (i == 0);

  while (channel->selected_for_send)
    {
      i = pthread_cond_wait (&channel->cond, &channel->lock);
      __go_assert (i == 0);
    }

  channel->is_closed = 1;

  i = pthread_cond_broadcast (&channel->cond);
  __go_assert (i == 0);

  __go_unlock_and_notify_selects (channel);
}
