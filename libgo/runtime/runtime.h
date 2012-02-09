/* runtime.h -- runtime support for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include "go-assert.h"
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <semaphore.h>
#include <ucontext.h>

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include "array.h"
#include "go-alloc.h"
#include "go-panic.h"
#include "go-string.h"

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
typedef	struct	G		G;
typedef	union	Lock		Lock;
typedef	struct	M		M;
typedef	union	Note		Note;
typedef	struct	SigTab		SigTab;
typedef	struct	MCache		MCache;
typedef struct	FixAlloc	FixAlloc;
typedef	struct	Hchan		Hchan;
typedef	struct	Timers		Timers;
typedef	struct	Timer		Timer;

typedef	struct	__go_open_array		Slice;
typedef	struct	__go_string		String;
typedef struct	__go_interface		Iface;
typedef	struct	__go_empty_interface	Eface;
typedef	struct	__go_type_descriptor	Type;
typedef	struct	__go_defer_stack	Defer;
typedef	struct	__go_panic_stack	Panic;

typedef struct	__go_func_type		FuncType;
typedef struct	__go_map_type		MapType;

/*
 * per-cpu declaration.
 */
extern M*	runtime_m(void);
extern G*	runtime_g(void);

extern M	runtime_m0;
extern G	runtime_g0;

/*
 * defined constants
 */
enum
{
	// G status
	//
	// If you add to this list, add to the list
	// of "okay during garbage collection" status
	// in mgc0.c too.
	Gidle,
	Grunnable,
	Grunning,
	Gsyscall,
	Gwaiting,
	Gmoribund,
	Gdead,
};
enum
{
	true	= 1,
	false	= 0,
};

/*
 * structures
 */
union	Lock
{
	uint32	key;	// futex-based impl
	M*	waitm;	// linked list of waiting M's (sema-based impl)
};
union	Note
{
	uint32	key;	// futex-based impl
	M*	waitm;	// waiting M (sema-based impl)
};
struct	G
{
	Defer*	defer;
	Panic*	panic;
	void*	exception;	// current exception being thrown
	bool	is_foreign;	// whether current exception from other language
	void	*gcstack;	// if status==Gsyscall, gcstack = stackbase to use during gc
	uintptr	gcstack_size;
	void*	gcnext_segment;
	void*	gcnext_sp;
	void*	gcinitial_sp;
	jmp_buf	gcregs;
	byte*	entry;		// initial function
	G*	alllink;	// on allg
	void*	param;		// passed parameter on wakeup
	bool	fromgogo;	// reached from gogo
	int16	status;
	int32	goid;
	uint32	selgen;		// valid sudog pointer
	const char*	waitreason;	// if status==Gwaiting
	G*	schedlink;
	bool	readyonstop;
	bool	ispanic;
	M*	m;		// for debuggers, but offset not hard-coded
	M*	lockedm;
	M*	idlem;
	// int32	sig;
	// uintptr	sigcode0;
	// uintptr	sigcode1;
	// uintptr	sigpc;
	uintptr	gopc;	// pc of go statement that created this goroutine

	ucontext_t	context;
	void*		stack_context[10];
};

struct	M
{
	G*	g0;		// goroutine with scheduling stack
	G*	gsignal;	// signal-handling G
	G*	curg;		// current running goroutine
	int32	id;
	int32	mallocing;
	int32	gcing;
	int32	locks;
	int32	nomemprof;
	int32	waitnextg;
	int32	dying;
	int32	profilehz;
	int32	helpgc;
	uint32	fastrand;
	Note	havenextg;
	G*	nextg;
	M*	alllink;	// on allm
	M*	schedlink;
	MCache	*mcache;
	G*	lockedg;
	G*	idleg;
	M*	nextwaitm;	// next M waiting for lock
	uintptr	waitsema;	// semaphore for parking on locks
	uint32	waitsemacount;
	uint32	waitsemalock;
};

struct	SigTab
{
	int32	sig;
	int32	flags;
};
enum
{
	SigCatch = 1<<0,
	SigIgnore = 1<<1,
	SigRestart = 1<<2,
	SigQueue = 1<<3,
	SigPanic = 1<<4,
};

/* Macros.  */

#ifdef GOOS_windows
enum {
   Windows = 1
};
#else
enum {
   Windows = 0
};
#endif

struct	Timers
{
	Lock;
	G	*timerproc;
	bool		sleeping;
	bool		rescheduling;
	Note	waitnote;
	Timer	**t;
	int32	len;
	int32	cap;
};

// Package time knows the layout of this structure.
// If this struct changes, adjust ../time/sleep.go:/runtimeTimer.
struct	Timer
{
	int32	i;		// heap index

	// Timer wakes up at when, and then at when+period, ... (period > 0 only)
	// each time calling f(now, arg) in the timer goroutine, so f must be
	// a well-behaved function and not block.
	int64	when;
	int64	period;
	void	(*f)(int64, Eface);
	Eface	arg;
};

/*
 * defined macros
 *    you need super-gopher-guru privilege
 *    to add this list.
 */
#define	nelem(x)	(sizeof(x)/sizeof((x)[0]))
#define	nil		((void*)0)
#define USED(v)		((void) v)

/*
 * external data
 */
G*	runtime_allg;
G*	runtime_lastg;
M*	runtime_allm;
extern	int32	runtime_gomaxprocs;
extern	bool	runtime_singleproc;
extern	uint32	runtime_panicking;
extern	int32	runtime_gcwaiting;		// gc is waiting to run
int32	runtime_ncpu;

/*
 * common functions and data
 */
int32	runtime_findnull(const byte*);

/*
 * very low level c-called
 */
void	runtime_args(int32, byte**);
void	runtime_osinit();
void	runtime_goargs(void);
void	runtime_goenvs(void);
void	runtime_goenvs_unix(void);
void	runtime_throw(const char*) __attribute__ ((noreturn));
void	runtime_panicstring(const char*) __attribute__ ((noreturn));
void*	runtime_mal(uintptr);
void	runtime_schedinit(void);
void	runtime_initsig(int32);
String	runtime_gostringnocopy(const byte*);
void*	runtime_mstart(void*);
G*	runtime_malg(int32, byte**, size_t*);
void	runtime_minit(void);
void	runtime_mallocinit(void);
void	runtime_gosched(void);
void	runtime_tsleep(int64);
M*	runtime_newm(void);
void	runtime_goexit(void);
void	runtime_entersyscall(void) __asm__("libgo_syscall.syscall.entersyscall");
void	runtime_exitsyscall(void) __asm__("libgo_syscall.syscall.exitsyscall");
void	siginit(void);
bool	__go_sigsend(int32 sig);
int64	runtime_nanotime(void);
int64	runtime_cputicks(void);

void	runtime_stoptheworld(void);
void	runtime_starttheworld(bool);
G*	__go_go(void (*pfn)(void*), void*);

/*
 * mutual exclusion locks.  in the uncontended case,
 * as fast as spin locks (just a few user-level instructions),
 * but on the contention path they sleep in the kernel.
 * a zeroed Lock is unlocked (no need to initialize each lock).
 */
void	runtime_lock(Lock*);
void	runtime_unlock(Lock*);

/*
 * sleep and wakeup on one-time events.
 * before any calls to notesleep or notewakeup,
 * must call noteclear to initialize the Note.
 * then, exactly one thread can call notesleep
 * and exactly one thread can call notewakeup (once).
 * once notewakeup has been called, the notesleep
 * will return.  future notesleep will return immediately.
 * subsequent noteclear must be called only after
 * previous notesleep has returned, e.g. it's disallowed
 * to call noteclear straight after notewakeup.
 *
 * notetsleep is like notesleep but wakes up after
 * a given number of nanoseconds even if the event
 * has not yet happened.  if a goroutine uses notetsleep to
 * wake up early, it must wait to call noteclear until it
 * can be sure that no other goroutine is calling
 * notewakeup.
 */
void	runtime_noteclear(Note*);
void	runtime_notesleep(Note*);
void	runtime_notewakeup(Note*);
void	runtime_notetsleep(Note*, int64);

/*
 * low-level synchronization for implementing the above
 */
uintptr	runtime_semacreate(void);
int32	runtime_semasleep(int64);
void	runtime_semawakeup(M*);
// or
void	runtime_futexsleep(uint32*, uint32, int64);
void	runtime_futexwakeup(uint32*, uint32);

/*
 * runtime go-called
 */
void	runtime_panic(Eface);

/* Functions.  */
#define runtime_panic __go_panic
#define runtime_printf printf
#define runtime_malloc(s) __go_alloc(s)
#define runtime_free(p) __go_free(p)
#define runtime_strcmp(s1, s2) __builtin_strcmp((s1), (s2))
#define runtime_mcmp(a, b, s) __builtin_memcmp((a), (b), (s))
#define runtime_memmove(a, b, s) __builtin_memmove((a), (b), (s))
#define runtime_exit(s) _exit(s)
MCache*	runtime_allocmcache(void);
void	free(void *v);
struct __go_func_type;
bool	runtime_addfinalizer(void*, void(*fn)(void*), const struct __go_func_type *);
#define runtime_cas(pval, old, new) __sync_bool_compare_and_swap (pval, old, new)
#define runtime_casp(pval, old, new) __sync_bool_compare_and_swap (pval, old, new)
#define runtime_xadd(p, v) __sync_add_and_fetch (p, v)
#define runtime_xchg(p, v) __atomic_exchange_n (p, v, __ATOMIC_SEQ_CST)
#define runtime_atomicload(p) __atomic_load_n (p, __ATOMIC_SEQ_CST)
#define runtime_atomicstore(p, v) __atomic_store_n (p, v, __ATOMIC_SEQ_CST)
#define runtime_atomicloadp(p) __atomic_load_n (p, __ATOMIC_SEQ_CST)
#define runtime_atomicstorep(p, v) __atomic_store_n (p, v, __ATOMIC_SEQ_CST)

void	runtime_dopanic(int32) __attribute__ ((noreturn));
void	runtime_startpanic(void);
void	runtime_ready(G*);
const byte*	runtime_getenv(const char*);
int32	runtime_atoi(const byte*);
uint32	runtime_fastrand1(void);

void	runtime_sigprof(uint8 *pc, uint8 *sp, uint8 *lr, G *gp);
void	runtime_resetcpuprofiler(int32);
void	runtime_setcpuprofilerate(void(*)(uintptr*, int32), int32);
void	runtime_usleep(uint32);

void	runtime_semacquire(uint32 volatile *);
void	runtime_semrelease(uint32 volatile *);
int32	runtime_gomaxprocsfunc(int32 n);
void	runtime_procyield(uint32);
void	runtime_osyield(void);
void	runtime_LockOSThread(void) __asm__("libgo_runtime.runtime.LockOSThread");
void	runtime_UnlockOSThread(void) __asm__("libgo_runtime.runtime.UnlockOSThread");

/*
 * low level C-called
 */
#define runtime_mmap mmap
#define runtime_munmap munmap
#define runtime_madvise madvise
#define runtime_memclr(buf, size) __builtin_memset((buf), 0, (size))

struct __go_func_type;
void reflect_call(const struct __go_func_type *, const void *, _Bool, _Bool,
		  void **, void **)
  asm ("libgo_reflect.reflect.call");

#ifdef __rtems__
void __wrap_rtems_task_variable_add(void **);
#endif

void	runtime_time_scan(void (*)(byte*, int64));
