/* Copyright (C) 2018-2023 Free Software Foundation, Inc.
   Contributed by Nicolas Koenig

   This file is part of the GNU Fortran runtime library (libgfortran).

   Libgfortran is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgfortran is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef ASYNC_H
#define ASYNC_H

/* Async I/O will not work on targets which do not support
   __gthread_cond_t and __gthread_equal / __gthread_self.  Check
   this.  */

#if defined(__GTHREAD_HAS_COND) && defined(__GTHREADS_CXX0X)
#define ASYNC_IO 1
#else
#define ASYNC_IO 0
#endif

/* Defining DEBUG_ASYNC will enable somewhat verbose debugging
   output for async I/O.  */

#define DEBUG_ASYNC
#undef DEBUG_ASYNC

#ifdef DEBUG_ASYNC

/* Define this if you want to use ANSI color escape sequences in your
   debugging output.  */

#define DEBUG_COLOR

#ifdef DEBUG_COLOR
#define MPREFIX "\033[30;46mM:\033[0m "
#define TPREFIX "\033[37;44mT:\033[0m "
#define RPREFIX "\033[37;41mR:\033[0m "
#define DEBUG_RED "\033[31m"
#define DEBUG_ORANGE "\033[33m"
#define DEBUG_GREEN "\033[32m"
#define DEBUG_DARKRED "\033[31;2m"
#define DEBUG_PURPLE "\033[35m"
#define DEBUG_NORM "\033[0m"
#define DEBUG_REVERSE_RED "\033[41;37m"
#define DEBUG_BLUE "\033[34m"

#else

#define MPREFIX "M: "
#define TPREFIX "T: "
#define RPREFIX ""
#define DEBUG_RED ""
#define DEBUG_ORANGE ""
#define DEBUG_GREEN ""
#define DEBUG_DARKRED ""
#define DEBUG_PURPLE ""
#define DEBUG_NORM ""
#define DEBUG_REVERSE_RED ""
#define DEBUG_BLUE ""

#endif

#define DEBUG_PRINTF(...) fprintf (stderr,__VA_ARGS__)

#define IN_DEBUG_QUEUE(mutex) ({		\
      __label__ end;				\
      aio_lock_debug *curr = aio_debug_head;	\
      while (curr) {				\
	if (curr->m == mutex) {			\
	  goto end;				\
	}					\
	curr = curr->next;			\
      }						\
    end:;					\
      curr;					\
    })

#define TAIL_DEBUG_QUEUE ({			\
      aio_lock_debug *curr = aio_debug_head;	\
      while (curr && curr->next) {		\
	curr = curr->next;			\
      }						\
      curr;					\
    })

#define CHECK_LOCK(mutex, status) do {					\
    aio_lock_debug *curr;						\
    INTERN_LOCK (&debug_queue_lock);					\
    if (__gthread_mutex_trylock (mutex)) {				\
      if ((curr = IN_DEBUG_QUEUE (mutex))) {				\
	sprintf (status, DEBUG_RED "%s():%d" DEBUG_NORM, curr->func, curr->line); \
      } else								\
	sprintf (status, DEBUG_RED "unknown" DEBUG_NORM);			\
    }									\
    else {								\
      __gthread_mutex_unlock (mutex);					\
      sprintf (status, DEBUG_GREEN "unlocked" DEBUG_NORM);			\
    }									\
    INTERN_UNLOCK (&debug_queue_lock);					\
  }while (0)

#define T_ERROR(func, ...) do {				\
    int t_error_temp;					\
    t_error_temp = func(__VA_ARGS__);			\
    if (t_error_temp)					\
      ERROR (t_error_temp, "args: " #__VA_ARGS__ "\n");	\
  } while (0)

#define NOTE(str, ...) do{						\
    char note_str[200];							\
    sprintf (note_str, "%s" DEBUG_PURPLE "NOTE: " DEBUG_NORM str, aio_prefix, ##__VA_ARGS__); \
    DEBUG_PRINTF ("%-90s %20s():%-5d\n", note_str, __FUNCTION__, __LINE__); \
  }while (0);

#define ERROR(errnum, str, ...) do{					\
    char note_str[200];							\
    sprintf (note_str, "%s" DEBUG_REVERSE_RED "ERROR:" DEBUG_NORM " [%d] " str, aio_prefix, \
	    errnum, ##__VA_ARGS__);					\
    DEBUG_PRINTF ("%-68s %s():%-5d\n", note_str, __FUNCTION__, __LINE__);	\
  }while (0)

#define MUTEX_DEBUG_ADD(mutex) do {		\
    aio_lock_debug *n;				\
    n = malloc (sizeof(aio_lock_debug));	\
    n->prev = TAIL_DEBUG_QUEUE;			\
    if (n->prev)				\
      n->prev->next = n;			\
    n->next = NULL;				\
    n->line = __LINE__;				\
    n->func = __FUNCTION__;			\
    n->m = mutex;				\
    if (!aio_debug_head) {			\
      aio_debug_head = n;			\
    }						\
  } while (0)

#define UNLOCK(mutex) do {						\
    aio_lock_debug *curr;						\
    DEBUG_PRINTF ("%s%-75s %20s():%-5d %18p\n", aio_prefix, DEBUG_GREEN "UNLOCK: " DEBUG_NORM #mutex, \
		 __FUNCTION__, __LINE__, (void *) mutex);		\
    INTERN_LOCK (&debug_queue_lock);					\
    curr = IN_DEBUG_QUEUE (mutex);					\
    if (curr)								\
      {									\
	if (curr->prev)							\
	  curr->prev->next = curr->next;				\
	if (curr->next) {						\
	  curr->next->prev = curr->prev;				\
	  if (curr == aio_debug_head)					\
	    aio_debug_head = curr->next;				\
	} else {							\
	  if (curr == aio_debug_head)					\
	    aio_debug_head = NULL;					\
	}								\
	free (curr);							\
      }									\
    INTERN_UNLOCK (&debug_queue_lock);					\
    INTERN_UNLOCK (mutex);						\
  }while (0)

#define TRYLOCK(mutex) ({						\
			 char status[200];				\
			 int res;					\
			 aio_lock_debug *curr;				\
			 res = __gthread_mutex_trylock (mutex);		\
			 INTERN_LOCK (&debug_queue_lock);		\
			 if (res) {					\
			   if ((curr = IN_DEBUG_QUEUE (mutex))) {	\
			     sprintf (status, DEBUG_RED "%s():%d" DEBUG_NORM, curr->func, curr->line);	\
			   } else					\
			     sprintf (status, DEBUG_RED "unknown" DEBUG_NORM);	\
			 }						\
			 else {						\
			   sprintf (status, DEBUG_GREEN "unlocked" DEBUG_NORM);	\
			   MUTEX_DEBUG_ADD (mutex);			\
			 }						\
			 DEBUG_PRINTF ("%s%-44s prev: %-35s %20s():%-5d %18p\n", aio_prefix, \
				      DEBUG_DARKRED "TRYLOCK: " DEBUG_NORM #mutex, status, __FUNCTION__, __LINE__, \
				      (void *) mutex);			\
			 INTERN_UNLOCK (&debug_queue_lock);		\
			 res;						\
    })

#define LOCK(mutex) do {						\
    char status[200];							\
    CHECK_LOCK (mutex, status);						\
    DEBUG_PRINTF ("%s%-42s prev: %-35s %20s():%-5d %18p\n", aio_prefix,	\
		 DEBUG_RED "LOCK: " DEBUG_NORM #mutex, status, __FUNCTION__, __LINE__, (void *) mutex); \
    INTERN_LOCK (mutex);							\
    INTERN_LOCK (&debug_queue_lock);					\
    MUTEX_DEBUG_ADD (mutex);						\
    INTERN_UNLOCK (&debug_queue_lock);					\
    DEBUG_PRINTF ("%s" DEBUG_RED "ACQ:" DEBUG_NORM " %-30s %78p\n", aio_prefix, #mutex, mutex); \
  } while (0)

#define DEBUG_LINE(...) __VA_ARGS__

#else
#define DEBUG_PRINTF(...) {}
#define CHECK_LOCK(au, mutex, status) {}
#define NOTE(str, ...) {}
#define DEBUG_LINE(...)
#define T_ERROR(func, ...) func(__VA_ARGS__)
#define LOCK(mutex) INTERN_LOCK (mutex)
#define UNLOCK(mutex) INTERN_UNLOCK (mutex)
#define TRYLOCK(mutex) (__gthread_mutex_trylock (mutex))
#endif

#define INTERN_LOCK(mutex) T_ERROR (__gthread_mutex_lock, mutex);

#define INTERN_UNLOCK(mutex) T_ERROR (__gthread_mutex_unlock, mutex);

#if ASYNC_IO

/* au->lock has to be held when calling this macro.  */

#define SIGNAL(advcond) do{						\
    (advcond)->pending = 1;						\
    DEBUG_PRINTF ("%s%-75s %20s():%-5d %18p\n", aio_prefix, DEBUG_ORANGE "SIGNAL: " DEBUG_NORM \
		 #advcond, __FUNCTION__, __LINE__, (void *) advcond);	\
    T_ERROR (__gthread_cond_broadcast, &(advcond)->signal);			\
  } while (0)

/* Has to be entered with mutex locked.  */

#define WAIT_SIGNAL_MUTEX(advcond, condition, mutex) do{		\
    __label__ finish;		       					\
    DEBUG_PRINTF ("%s%-75s %20s():%-5d %18p\n", aio_prefix, DEBUG_BLUE "WAITING: " DEBUG_NORM \
		 #advcond, __FUNCTION__, __LINE__, (void *) advcond);	\
    if ((advcond)->pending || (condition))				\
      goto finish;							\
    while (1)								\
      {									\
	int err_ret = __gthread_cond_wait(&(advcond)->signal, mutex);	\
	if (err_ret) internal_error (NULL, "WAIT_SIGNAL_MUTEX failed");	\
	if (condition)							\
	  {								\
	    DEBUG_PRINTF ("%s%-75s %20s():%-5d %18p\n", aio_prefix, DEBUG_ORANGE \
			  "REC: " DEBUG_NORM				\
			  #advcond,  __FUNCTION__, __LINE__, (void *)advcond); \
	    break;				      			\
	  }								\
      }									\
  finish:								\
    (advcond)->pending = 0;						\
    UNLOCK (mutex);							\
  } while (0)

/* au->lock has to be held when calling this macro.  */

#define REVOKE_SIGNAL(advcond) do{		\
    (advcond)->pending = 0;			\
  } while (0)

#else

#define SIGNAL(advcond) do{} while(0)
#define WAIT_SIGNAL_MUTEX(advcond, condition, mutex) do{} while(0)
#define REVOKE_SIGNAL(advcond) do{} while(0)

#endif

#if ASYNC_IO
DEBUG_LINE (extern __thread const char *aio_prefix);

DEBUG_LINE (typedef struct aio_lock_debug{
  __gthread_mutex_t *m;
  int line;
  const char *func;
  struct aio_lock_debug *next;
  struct aio_lock_debug *prev;
} aio_lock_debug;)

DEBUG_LINE (extern aio_lock_debug *aio_debug_head;)
DEBUG_LINE (extern __gthread_mutex_t debug_queue_lock;)

/* Thread - local storage of the current unit we are looking at. Needed for
   error reporting.  */

extern __thread gfc_unit *thread_unit;
#endif

enum aio_do {
  AIO_INVALID = 0,
  AIO_DATA_TRANSFER_INIT,
  AIO_TRANSFER_SCALAR,
  AIO_TRANSFER_ARRAY,
  AIO_WRITE_DONE,
  AIO_READ_DONE,
  AIO_CLOSE
};

typedef union transfer_args
{
  struct
  {
    void (*transfer) (struct st_parameter_dt *, bt, void *, int, size_t, size_t);
    bt arg_bt;
    void *data;
    int i;
    size_t s1;
    size_t s2;
  } scalar;
  struct
  {
    gfc_array_char *desc;
    int kind;
    gfc_charlen_type charlen;
  } array;
} transfer_args;

struct adv_cond
{
#if ASYNC_IO
  int pending;
  __gthread_cond_t signal;
#endif
};

typedef struct async_unit
{
  __gthread_mutex_t io_lock;   /* Lock for doing actual I/O. */
  __gthread_mutex_t lock;      /* Lock for manipulating the queue structure.  */
  bool empty;
  struct
  {
    int waiting;
    int low;
    int high;
    struct adv_cond done;
  } id;

#if ASYNC_IO
  struct adv_cond work;
  struct adv_cond emptysignal;
  struct st_parameter_dt *pdt;
  __gthread_t thread;
  struct transfer_queue *head;
  struct transfer_queue *tail;

  struct {
    const char *message;
    st_parameter_common *cmp;
    bool has_error;
    int last_good_id;
    int family;
    bool fatal_error;
  } error;
#endif
} async_unit;

void init_async_unit (gfc_unit *);
internal_proto (init_async_unit);

bool async_wait (st_parameter_common *, async_unit *);
internal_proto (async_wait);

bool async_wait_id (st_parameter_common *, async_unit *, int);
internal_proto (async_wait_id);

bool collect_async_errors (st_parameter_common *, async_unit *);
internal_proto (collect_async_errors); 

void async_close (async_unit *);
internal_proto (async_close);

void enqueue_transfer (async_unit * au, transfer_args * arg, enum aio_do);
internal_proto (enqueue_transfer);

void enqueue_done (async_unit *, enum aio_do type);
internal_proto (enqueue_done);

int enqueue_done_id (async_unit *, enum aio_do type);
internal_proto (enqueue_done_id);

void enqueue_init (async_unit *);
internal_proto (enqueue_init);

void enqueue_data_transfer_init (async_unit *, st_parameter_dt *, int);
internal_proto (enqueue_data_transfer_init);

void enqueue_close (async_unit *);
internal_proto (enqueue_close);

#endif
