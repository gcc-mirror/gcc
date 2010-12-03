/* channel.h -- the channel type for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>
#include <pthread.h>

/* This structure is used when a select is waiting for a synchronous
   channel.  */

struct __go_channel_select
{
  /* A pointer to the next select waiting for this channel.  */
  struct __go_channel_select *next;
  /* A pointer to the channel which this select will use.  This starts
     out as NULL and is set to the first channel which synchs up with
     this one.  This variable to which this points may only be
     accessed when __go_select_data_mutex is held.  */
  struct __go_channel **selected;
  /* A pointer to a variable which must be set to true if the
     goroutine which sets *SELECTED wants to read from the channel,
     false if it wants to write to it.  */
  _Bool *is_read;
};

/* A channel is a pointer to this structure.  */

struct __go_channel
{
  /* A mutex to control access to the channel.  */
  pthread_mutex_t lock;
  /* A condition variable.  This is signalled when data is added to
     the channel and when data is removed from the channel.  */
  pthread_cond_t cond;
  /* The size of elements on this channel.  */
  size_t element_size;
  /* Number of operations on closed channel.  */
  unsigned short closed_op_count;
  /* True if a goroutine is waiting to send on a synchronous
     channel.  */
  _Bool waiting_to_send;
  /* True if a goroutine is waiting to receive on a synchronous
     channel.  */
  _Bool waiting_to_receive;
  /* True if this channel was selected for send in a select statement.
     This looks out all other sends.  */
  _Bool selected_for_send;
  /* True if this channel was selected for receive in a select
     statement.  This locks out all other receives.  */
  _Bool selected_for_receive;
  /* True if this channel has been closed.  */
  _Bool is_closed;
  /* True if at least one null value has been read from a closed
     channel.  */
  _Bool saw_close;
  /* The list of select statements waiting to send on a synchronous
     channel.  */
  struct __go_channel_select *select_send_queue;
  /* The list of select statements waiting to receive on a synchronous
     channel.  */
  struct __go_channel_select *select_receive_queue;
  /* If a select statement is waiting for this channel, it sets these
     pointers.  When something happens on the channel, the channel
     locks the mutex, signals the condition, and unlocks the
     mutex.  */
  pthread_mutex_t *select_mutex;
  pthread_cond_t *select_cond;
  /* The number of entries in the circular buffer.  */
  unsigned int num_entries;
  /* Where to store the next value.  */
  unsigned int next_store;
  /* Where to fetch the next value.  If next_fetch == next_store, the
     buffer is empty.  If next_store + 1 == next_fetch, the buffer is
     full.  */
  unsigned int next_fetch;
  /* The circular buffer.  */
  uint64_t data[];
};

/* The mutex used to control access to the value pointed to by the
   __go_channel_select selected field.  No additional mutexes may be
   acquired while this mutex is held.  */
extern pthread_mutex_t __go_select_data_mutex;

/* Maximum permitted number of operations on a closed channel.  */
#define MAX_CLOSED_OPERATIONS (0x100)

extern struct __go_channel *__go_new_channel (size_t, size_t);

extern _Bool __go_synch_with_select (struct __go_channel *, _Bool);

extern void __go_broadcast_to_select (struct __go_channel *);

extern _Bool __go_send_acquire (struct __go_channel *, _Bool);

#define SEND_NONBLOCKING_ACQUIRE_SPACE 0
#define SEND_NONBLOCKING_ACQUIRE_NOSPACE 1
#define SEND_NONBLOCKING_ACQUIRE_CLOSED 2

extern int __go_send_nonblocking_acquire (struct __go_channel *);

extern void __go_send_release (struct __go_channel *);

extern void __go_send_small (struct __go_channel *, uint64_t, _Bool);

extern _Bool __go_send_nonblocking_small (struct __go_channel *, uint64_t);

extern void __go_send_big (struct __go_channel *, const void *, _Bool);

extern _Bool __go_send_nonblocking_big (struct __go_channel *, const void *);

extern _Bool __go_receive_acquire (struct __go_channel *, _Bool);

#define RECEIVE_NONBLOCKING_ACQUIRE_DATA 0
#define RECEIVE_NONBLOCKING_ACQUIRE_NODATA 1
#define RECEIVE_NONBLOCKING_ACQUIRE_CLOSED 2

extern int __go_receive_nonblocking_acquire (struct __go_channel *);

extern uint64_t __go_receive_small (struct __go_channel *, _Bool);

extern void __go_receive_release (struct __go_channel *);

struct __go_receive_nonblocking_small
{
  uint64_t __val;
  _Bool __success;
};

extern struct __go_receive_nonblocking_small
__go_receive_nonblocking_small (struct __go_channel *);

extern void __go_receive_big (struct __go_channel *, void *, _Bool);

extern _Bool __go_receive_nonblocking_big (struct __go_channel *, void *);

extern void __go_unlock_and_notify_selects (struct __go_channel *);

extern _Bool __go_builtin_closed (struct __go_channel *);

extern void __go_builtin_close (struct __go_channel *);

extern size_t __go_chan_len (struct __go_channel *);

extern size_t __go_chan_cap (struct __go_channel *);
