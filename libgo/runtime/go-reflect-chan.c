/* go-reflect-chan.c -- channel reflection support for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdlib.h>
#include <stdint.h>

#include "config.h"
#include "go-alloc.h"
#include "go-assert.h"
#include "go-panic.h"
#include "go-type.h"
#include "channel.h"

/* This file implements support for reflection on channels.  These
   functions are called from reflect/value.go.  */

extern uintptr_t makechan (const struct __go_type_descriptor *, uint32_t)
  asm ("libgo_reflect.reflect.makechan");

uintptr_t
makechan (const struct __go_type_descriptor *typ, uint32_t size)
{
  struct __go_channel *channel;
  void *ret;

  channel = __go_new_channel (typ, size);

  ret = __go_alloc (sizeof (void *));
  __builtin_memcpy (ret, &channel, sizeof (void *));
  return (uintptr_t) ret;
}

extern _Bool chansend (struct __go_channel_type *, uintptr_t, uintptr_t, _Bool)
  asm ("libgo_reflect.reflect.chansend");

_Bool
chansend (struct __go_channel_type *ct, uintptr_t ch, uintptr_t val_i,
	  _Bool nb)
{
  struct __go_channel *channel = (struct __go_channel *) ch;
  uintptr_t element_size;
  void *pv;

  __go_assert (ct->__common.__code == GO_CHAN);

  if (__go_is_pointer_type (ct->__element_type))
    pv = &val_i;
  else
    pv = (void *) val_i;

  element_size = ct->__element_type->__size;
  if (element_size <= sizeof (uint64_t))
    {
      union
      {
	char b[sizeof (uint64_t)];
	uint64_t v;
      } u;

      __builtin_memset (u.b, 0, sizeof (uint64_t));
#ifndef WORDS_BIGENDIAN
      __builtin_memcpy (u.b, pv, element_size);
#else
      __builtin_memcpy (u.b + sizeof (uint64_t) - element_size, pv,
			element_size);
#endif
      if (nb)
	return __go_send_nonblocking_small (channel, u.v);
      else
	{
	  __go_send_small (channel, u.v, 0);
	  return 1;
	}
    }
  else
    {
      if (nb)
	return __go_send_nonblocking_big (channel, pv);
      else
	{
	  __go_send_big (channel, pv, 0);
	  return 1;
	}
    }
}

struct chanrecv_ret
{
  uintptr_t val;
  _Bool selected;
  _Bool received;
};

extern struct chanrecv_ret chanrecv (struct __go_channel_type *, uintptr_t,
				     _Bool)
  asm ("libgo_reflect.reflect.chanrecv");

struct chanrecv_ret
chanrecv (struct __go_channel_type *ct, uintptr_t ch, _Bool nb)
{
  struct __go_channel *channel = (struct __go_channel *) ch;
  void *pv;
  uintptr_t element_size;
  struct chanrecv_ret ret;

  __go_assert (ct->__common.__code == GO_CHAN);

  element_size = ct->__element_type->__size;

  if (__go_is_pointer_type (ct->__element_type))
    pv = &ret.val;
  else
    {
      pv = __go_alloc (element_size);
      ret.val = (uintptr_t) pv;
    }

  if (element_size <= sizeof (uint64_t))
    {
      union
      {
	char b[sizeof (uint64_t)];
	uint64_t v;
      } u;

      if (!nb)
	{
	  u.v = __go_receive_small_closed (channel, 0, &ret.received);
	  ret.selected = 1;
	}
      else
	{
	  struct __go_receive_nonblocking_small s;

	  s = __go_receive_nonblocking_small (channel);
	  ret.selected = s.__success || s.__closed;
	  ret.received = s.__success;
	  u.v = s.__val;
	}

#ifndef WORDS_BIGENDIAN
      __builtin_memcpy (pv, u.b, element_size);
#else
      __builtin_memcpy (pv, u.b + sizeof (uint64_t) - element_size,
			element_size);
#endif
    }
  else
    {
      if (!nb)
	{
	  ret.received = __go_receive_big (channel, pv, 0);
	  ret.selected = 1;
	}
      else
	{
	  _Bool got;
	  _Bool closed;

	  got = __go_receive_nonblocking_big (channel, pv, &closed);
	  ret.selected = got || closed;
	  ret.received = got;
	}
    }

  return ret;
}

extern void chanclose (uintptr_t) asm ("libgo_reflect.reflect.chanclose");

void
chanclose (uintptr_t ch)
{
  struct __go_channel *channel = (struct __go_channel *) ch;

  __go_builtin_close (channel);
}

extern int32_t chanlen (uintptr_t) asm ("libgo_reflect.reflect.chanlen");

int32_t
chanlen (uintptr_t ch)
{
  struct __go_channel *channel = (struct __go_channel *) ch;

  return (int32_t) __go_chan_len (channel);
}

extern int32_t chancap (uintptr_t) asm ("libgo_reflect.reflect.chancap");

int32_t
chancap (uintptr_t ch)
{
  struct __go_channel *channel = (struct __go_channel *) ch;

  return (int32_t) __go_chan_cap (channel);
}
