/* runtime.h -- runtime support for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#define _GNU_SOURCE
#include "go-assert.h"
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <semaphore.h>

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include "go-alloc.h"
#include "go-panic.h"
#include "go-string.h"

typedef struct __go_string String;

/* This file supports C files copied from the 6g runtime library.
   This is a version of the 6g runtime.h rewritten for gccgo's version
   of the code.  */

typedef signed int   int8    __attribute__ ((mode (QI)));
typedef unsigned int uint8   __attribute__ ((mode (QI)));
typedef signed int   int16   __attribute__ ((mode (HI)));
typedef unsigned int uint16  __attribute__ ((mode (HI)));
typedef signed int   int32   __attribute__ ((mode (SI)));
typedef unsigned int uint32  __attribute__ ((mode (SI)));
typedef signed int   int64   __attribute__ ((mode (DI)));
typedef unsigned int uint64  __attribute__ ((mode (DI)));
typedef float        float32 __attribute__ ((mode (SF)));
typedef double       float64 __attribute__ ((mode (DF)));
typedef unsigned int uintptr __attribute__ ((mode (pointer)));

/* Defined types.  */

typedef	uint8			bool;
typedef	uint8			byte;
typedef	struct	M		M;
typedef	struct	MCache		MCache;
typedef	struct	Lock		Lock;

/* We use mutexes for locks.  6g uses futexes directly, and perhaps
   someday we will do that too.  */

struct	Lock
{
	uint32 key;
	sem_t sem;
};

/* A Note.  */

typedef	struct	Note		Note;

struct Note {
	int32 woken;
};

/* Per CPU declarations.  */

#ifdef __rtems__
#define __thread
#endif

extern __thread		M* 	m;

extern M	m0;

#ifdef __rtems__
#undef __thread
#endif

/* Constants.  */

enum
{
	true	= 1,
	false	= 0,
};

/* Structures.  */

struct	M
{
	int32	mallocing;
	int32	gcing;
	int32	locks;
	int32	nomemprof;
	int32	gcing_for_prof;
	int32	holds_finlock;
	int32	gcing_for_finlock;
	MCache	*mcache;

	/* For the list of all threads.  */
	struct __go_thread_id *list_entry;

	/* For the garbage collector.  */
	void	*gc_sp;
	size_t	gc_len;
	void	*gc_next_segment;
	void	*gc_next_sp;
	void	*gc_initial_sp;
	struct __go_panic_defer_struct *gc_panic_defer;
};

/* Macros.  */
#define	nelem(x)	(sizeof(x)/sizeof((x)[0]))
#define	nil		((void*)0)
#define USED(v)		((void) v)

/* We map throw to assert.  */
#define runtime_throw(s) __go_assert(s == 0)

void*	runtime_mal(uintptr);
void	runtime_mallocinit(void);
void	runtime_initfintab(void);
void	siginit(void);
bool	__go_sigsend(int32 sig);
int64	runtime_nanotime(void);

void	runtime_stoptheworld(void);
void	runtime_starttheworld(void);
void	__go_go(void (*pfn)(void*), void*);
void	__go_gc_goroutine_init(void*);
void	__go_enable_gc(void);
int	__go_run_goroutine_gc(int);
void	__go_scanstacks(void (*scan)(byte *, int64));
void	__go_stealcache(void);
void	__go_cachestats(void);

/*
 * mutual exclusion locks.  in the uncontended case,
 * as fast as spin locks (just a few user-level instructions),
 * but on the contention path they sleep in the kernel.
 */
void	runtime_initlock(Lock*);
void	runtime_lock(Lock*);
void	runtime_unlock(Lock*);
void	runtime_destroylock(Lock*);

void semacquire (uint32 *) asm ("libgo_runtime.runtime.Semacquire");
void semrelease (uint32 *) asm ("libgo_runtime.runtime.Semrelease");

/*
 * sleep and wakeup on one-time events.
 * before any calls to notesleep or notewakeup,
 * must call noteclear to initialize the Note.
 * then, any number of threads can call notesleep
 * and exactly one thread can call notewakeup (once).
 * once notewakeup has been called, all the notesleeps
 * will return.  future notesleeps will return immediately.
 */
void	noteclear(Note*);
void	notesleep(Note*);
void	notewakeup(Note*);

/* Functions.  */
#define runtime_printf printf
#define runtime_malloc(s) __go_alloc(s)
#define runtime_free(p) __go_free(p)
#define runtime_memclr(buf, size) __builtin_memset((buf), 0, (size))
#define runtime_strcmp(s1, s2) __builtin_strcmp((s1), (s2))
#define runtime_getenv(s) getenv(s)
#define runtime_atoi(s) atoi(s)
#define runtime_mcmp(a, b, s) __builtin_memcmp((a), (b), (s))
#define runtime_memmove(a, b, s) __builtin_memmove((a), (b), (s))
MCache*	runtime_allocmcache(void);
void	free(void *v);
struct __go_func_type;
void	runtime_addfinalizer(void*, void(*fn)(void*), const struct __go_func_type *);
void	runtime_walkfintab(void (*fn)(void*), void (*scan)(byte *, int64));
#define runtime_mmap mmap
#define runtime_munmap(p, s) munmap((p), (s))
#define runtime_cas(pval, old, new) __sync_bool_compare_and_swap (pval, old, new)

struct __go_func_type;
void reflect_call(const struct __go_func_type *, const void *, _Bool, void **,
		  void **)
  asm ("libgo_reflect.reflect.call");

#ifdef __rtems__
void __wrap_rtems_task_variable_add(void **);
#endif
