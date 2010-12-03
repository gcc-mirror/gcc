/* go-closed.c -- the builtin closed function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-assert.h"
#include "channel.h"

/* Return whether a channel is closed.  We only return true after at
   least one nil value has been read from the channel.  */

_Bool
__go_builtin_closed (struct __go_channel *channel)
{
  int i;
  _Bool ret;

  i = pthread_mutex_lock (&channel->lock);
  __go_assert (i == 0);

  while (channel->selected_for_receive)
    {
      i = pthread_cond_wait (&channel->cond, &channel->lock);
      __go_assert (i == 0);
    }

  ret = channel->saw_close;

  i = pthread_mutex_unlock (&channel->lock);
  __go_assert (i == 0);

  return ret;
}
