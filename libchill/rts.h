/* GNU CHILL compiler regression test file
 Copyright (C) 1992, 1993 Free Software Foundation, Inc.
 
This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef __rts_h_
#define __rts_h_

typedef enum
{
  UNUSED,
  Process,
  Signal,
  Buffer,
  Event,
  Synonym,
  Exception,
  LAST_AND_UNUSED,
} TaskingEnum;

typedef void (*EntryPoint) ();

typedef struct
{
  char       *name;
  short      *value;
  int         value_defined;
  EntryPoint  entry;
  unsigned char /*TaskingEnum*/ type;
} TaskingStruct;

/* how an INSTANCE is implemented */
typedef struct
{
  short ptype;
  short pcopy;
} INSTANCE;

/* interface to underlaying os */
typedef enum
{
  wait_wait,
  wait_buffer_send,
  wait_buffer_receive,
  wait_buffer_free,
  wait_event_delay,
  wait_event_free,
} Delay_Reason;

extern INSTANCE __whoami ();
extern void *__xmalloc_ ();

#define THIS  __whoami()
/* for easier changing to something different,
   i.e. allocate_memory */
#define MALLOC(ADDR,SIZE)  ADDR = __xmalloc_(SIZE)
#define FREE(ADDR)         free (ADDR)

/* definitions for EVENTS */
typedef struct EVENTQUEUE
{
  struct EVENTQUEUE    *forward;       /* next in the list */
  struct EVENTQUEUE   **listhead;      /* pointer to EVENT location */
  int                   priority;      /* prio for DELAY or DELAY CASE */
  INSTANCE              this;          /* specify the instance is delayed */
  struct EVENTQUEUE    *startlist;     /* start of the list */
  struct EVENTQUEUE    *chain;         /* list of all events in an DELAY CASE */
  int                   is_continued;  /* indicates a continue action on that event */
  INSTANCE              who_continued; /* indicates who continued */
} Event_Queue;

typedef struct
{
  Event_Queue     **ev;
  unsigned long     maxqueuelength;
} Event_Descr;

/* definitions for BUFFERS */
struct BUFFERQUEUE;

typedef struct BUFFER_WAIT_QUEUE
{
  struct BUFFER_WAIT_QUEUE   *forward;
  struct BUFFERQUEUE        **bufferaddr;
  INSTANCE                    this;
  struct BUFFER_WAIT_QUEUE   *startlist;
  struct BUFFER_WAIT_QUEUE   *chain;
  int                         is_sent;
  INSTANCE                    who_sent;     /* instance which have
					       send a buffer */
  unsigned long               datalen;
  void                       *dataptr;
} Buffer_Wait_Queue;

typedef struct BUFFER_SEND_QUEUE
{
  struct BUFFER_SEND_QUEUE   *forward;
  int                         priority;
  INSTANCE                    this;
  int                         is_delayed;
  unsigned long               datalen;
  void                       *dataptr;
} Buffer_Send_Queue;

typedef struct BUFFERQUEUE
{
  Buffer_Wait_Queue    *waitqueue;
  unsigned long         waitqueuelength;
  Buffer_Send_Queue    *sendqueue;
  unsigned long         sendqueuelength;
} Buffer_Queue;

typedef struct
{
  Buffer_Queue    **buf;
  unsigned long     maxqueuelength;
} Buffer_Descr;

/* descriptor for data */
typedef struct
{
  void         *ptr;
  int           length;
} Data_Descr;

/* time format runtime delivers */
typedef struct
{
    unsigned long	secs;
    unsigned long	nanosecs;
} RtsTime;

extern void __rtstime (RtsTime *t);
extern int __delay_this (Delay_Reason reason, RtsTime *t, char *file, int lineno);
extern void __continue_that (INSTANCE ins, int prio, char *file, int lineno);

#endif /* __rts_h_ */
