// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "config.h"

#include "go-assert.h"
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include <ucontext.h>

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include "interface.h"
#include "go-alloc.h"

#define _STRINGIFY2_(x) #x
#define _STRINGIFY_(x) _STRINGIFY2_(x)
#define GOSYM_PREFIX _STRINGIFY_(__USER_LABEL_PREFIX__)

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
typedef signed int   intptr __attribute__ ((mode (pointer)));
typedef unsigned int uintptr __attribute__ ((mode (pointer)));

typedef intptr		intgo; // Go's int
typedef uintptr		uintgo; // Go's uint

/* Defined types.  */

typedef	uint8			bool;
typedef	uint8			byte;
typedef	struct	Func		Func;
typedef	struct	G		G;
typedef	struct	Lock		Lock;
typedef	struct	M		M;
typedef	struct	P		P;
typedef	struct	Note		Note;
typedef	struct	String		String;
typedef	struct	FuncVal		FuncVal;
typedef	struct	SigTab		SigTab;
typedef	struct	MCache		MCache;
typedef struct	FixAlloc	FixAlloc;
typedef	struct	Hchan		Hchan;
typedef	struct	Timers		Timers;
typedef	struct	Timer		Timer;
typedef	struct	GCStats		GCStats;
typedef	struct	LFNode		LFNode;
typedef	struct	ParFor		ParFor;
typedef	struct	ParForThread	ParForThread;
typedef	struct	CgoMal		CgoMal;
typedef	struct	PollDesc	PollDesc;

typedef	struct	__go_open_array		Slice;
typedef struct	__go_interface		Iface;
typedef	struct	__go_empty_interface	Eface;
typedef	struct	__go_type_descriptor	Type;
typedef	struct	__go_defer_stack	Defer;
typedef	struct	__go_panic_stack	Panic;

typedef struct	__go_ptr_type		PtrType;
typedef struct	__go_func_type		FuncType;
typedef struct	__go_map_type		MapType;
typedef struct	__go_channel_type	ChanType;

typedef struct  Traceback	Traceback;

typedef struct	Location	Location;

/*
 * Per-CPU declaration.
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
	Gmoribund_unused,  // currently unused, but hardcoded in gdb scripts
	Gdead,
};
enum
{
	// P status
	Pidle,
	Prunning,
	Psyscall,
	Pgcstop,
	Pdead,
};
enum
{
	true	= 1,
	false	= 0,
};
enum
{
	PtrSize = sizeof(void*),
};
enum
{
	// Per-M stack segment cache size.
	StackCacheSize = 32,
	// Global <-> per-M stack segment cache transfer batch size.
	StackCacheBatch = 16,
};
/*
 * structures
 */
struct	Lock
{
	// Futex-based impl treats it as uint32 key,
	// while sema-based impl as M* waitm.
	// Used to be a union, but unions break precise GC.
	uintptr	key;
};
struct	Note
{
	// Futex-based impl treats it as uint32 key,
	// while sema-based impl as M* waitm.
	// Used to be a union, but unions break precise GC.
	uintptr	key;
};
struct String
{
	const byte*	str;
	intgo		len;
};
struct FuncVal
{
	void	(*fn)(void);
	// variable-size, fn-specific data here
};
struct	GCStats
{
	// the struct must consist of only uint64's,
	// because it is casted to uint64[].
	uint64	nhandoff;
	uint64	nhandoffcnt;
	uint64	nprocyield;
	uint64	nosyield;
	uint64	nsleep;
};

// A location in the program, used for backtraces.
struct	Location
{
	uintptr	pc;
	String	filename;
	String	function;
	intgo	lineno;
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
	ucontext_t gcregs;
	byte*	entry;		// initial function
	G*	alllink;	// on allg
	void*	param;		// passed parameter on wakeup
	bool	fromgogo;	// reached from gogo
	int16	status;
	int64	goid;
	uint32	selgen;		// valid sudog pointer
	const char*	waitreason;	// if status==Gwaiting
	G*	schedlink;
	bool	ispanic;
	bool	issystem;	// do not output in stack dump
	bool	isbackground;	// ignore in deadlock detector
	bool	blockingsyscall;	// hint that the next syscall will block
	M*	m;		// for debuggers, but offset not hard-coded
	M*	lockedm;
	int32	sig;
	int32	writenbuf;
	byte*	writebuf;
	// DeferChunk	*dchunk;
	// DeferChunk	*dchunknext;
	uintptr	sigcode0;
	uintptr	sigcode1;
	// uintptr	sigpc;
	uintptr	gopc;	// pc of go statement that created this goroutine

	int32	ncgo;
	CgoMal*	cgomal;

	Traceback* traceback;

	ucontext_t	context;
	void*		stack_context[10];
};

struct	M
{
	G*	g0;		// goroutine with scheduling stack
	G*	gsignal;	// signal-handling G
	byte*	gsignalstack;
	size_t	gsignalstacksize;
	void	(*mstartfn)(void);
	G*	curg;		// current running goroutine
	P*	p;		// attached P for executing Go code (nil if not executing Go code)
	P*	nextp;
	int32	id;
	int32	mallocing;
	int32	throwing;
	int32	gcing;
	int32	locks;
	int32	nomemprof;
	int32	dying;
	int32	profilehz;
	int32	helpgc;
	bool	blockingsyscall;
	bool	spinning;
	uint32	fastrand;
	uint64	ncgocall;	// number of cgo calls in total
	int32	ncgo;		// number of cgo calls currently in progress
	CgoMal*	cgomal;
	Note	park;
	M*	alllink;	// on allm
	M*	schedlink;
	MCache	*mcache;
	G*	lockedg;
	Location createstack[32];	// Stack that created this thread.
	uint32	locked;	// tracking for LockOSThread
	M*	nextwaitm;	// next M waiting for lock
	uintptr	waitsema;	// semaphore for parking on locks
	uint32	waitsemacount;
	uint32	waitsemalock;
	GCStats	gcstats;
	bool	racecall;
	bool	needextram;
	bool	dropextram;	// for gccgo: drop after call is done.
	void*	racepc;
	void	(*waitunlockf)(Lock*);
	void*	waitlock;

	uintptr	settype_buf[1024];
	uintptr	settype_bufsize;

	uintptr	end[];
};

struct P
{
	Lock;

	uint32	status;  // one of Pidle/Prunning/...
	P*	link;
	uint32	tick;   // incremented on every scheduler or system call
	M*	m;	// back-link to associated M (nil if idle)
	MCache*	mcache;

	// Queue of runnable goroutines.
	G**	runq;
	int32	runqhead;
	int32	runqtail;
	int32	runqsize;

	// Available G's (status == Gdead)
	G*	gfree;
	int32	gfreecnt;

	byte	pad[64];
};

// The m->locked word holds a single bit saying whether
// external calls to LockOSThread are in effect, and then a counter
// of the internal nesting depth of lockOSThread / unlockOSThread.
enum
{
	LockExternal = 1,
	LockInternal = 2,
};

struct	SigTab
{
	int32	sig;
	int32	flags;
};
enum
{
	SigNotify = 1<<0,	// let signal.Notify have signal, even if from kernel
	SigKill = 1<<1,		// if signal.Notify doesn't take it, exit quietly
	SigThrow = 1<<2,	// if signal.Notify doesn't take it, exit loudly
	SigPanic = 1<<3,	// if the signal is from the kernel, panic
	SigDefault = 1<<4,	// if the signal isn't explicitly requested, don't monitor it
	SigHandling = 1<<5,	// our signal handler is registered
	SigIgnored = 1<<6,	// the signal was ignored before we registered for it
};

#ifndef NSIG
#define NSIG 32
#endif

// NOTE(rsc): keep in sync with extern.go:/type.Func.
// Eventually, the loaded symbol table should be closer to this form.
struct	Func
{
	String	name;
	uintptr	entry;	// entry pc
};


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
	FuncVal	*fv;
	Eface	arg;
};

// Lock-free stack node.
struct LFNode
{
	LFNode	*next;
	uintptr	pushcnt;
};

// Parallel for descriptor.
struct ParFor
{
	void (*body)(ParFor*, uint32);	// executed for each element
	uint32 done;			// number of idle threads
	uint32 nthr;			// total number of threads
	uint32 nthrmax;			// maximum number of threads
	uint32 thrseq;			// thread id sequencer
	uint32 cnt;			// iteration space [0, cnt)
	void *ctx;			// arbitrary user context
	bool wait;			// if true, wait while all threads finish processing,
					// otherwise parfor may return while other threads are still working
	ParForThread *thr;		// array of thread descriptors
	uint32 pad;			// to align ParForThread.pos for 64-bit atomic operations
	// stats
	uint64 nsteal;
	uint64 nstealcnt;
	uint64 nprocyield;
	uint64 nosyield;
	uint64 nsleep;
};

// Track memory allocated by code not written in Go during a cgo call,
// so that the garbage collector can see them.
struct CgoMal
{
	CgoMal	*next;
	void	*alloc;
};

/*
 * defined macros
 *    you need super-gopher-guru privilege
 *    to add this list.
 */
#define	nelem(x)	(sizeof(x)/sizeof((x)[0]))
#define	nil		((void*)0)
#define USED(v)		((void) v)
#define	ROUND(x, n)	(((x)+(n)-1)&~((n)-1)) /* all-caps to mark as macro: it evaluates n twice */

byte*	runtime_startup_random_data;
uint32	runtime_startup_random_data_len;
void	runtime_get_random_data(byte**, int32*);

enum {
	// hashinit wants this many random bytes
	HashRandomBytes = 32
};
void	runtime_hashinit(void);

void	runtime_traceback();
void	runtime_tracebackothers(G*);

/*
 * external data
 */
extern	uintptr runtime_zerobase;
extern	G*	runtime_allg;
extern	G*	runtime_lastg;
extern	M*	runtime_allm;
extern	P**	runtime_allp;
extern	int32	runtime_gomaxprocs;
extern	uint32	runtime_needextram;
extern	bool	runtime_singleproc;
extern	uint32	runtime_panicking;
extern	uint32	runtime_gcwaiting;		// gc is waiting to run
extern	int8*	runtime_goos;
extern	int32	runtime_ncpu;
extern 	void	(*runtime_sysargs)(int32, uint8**);

/*
 * common functions and data
 */
#define runtime_strcmp(s1, s2) __builtin_strcmp((s1), (s2))
#define runtime_strstr(s1, s2) __builtin_strstr((s1), (s2))
intgo	runtime_findnull(const byte*);
void	runtime_dump(byte*, int32);

/*
 * very low level c-called
 */
struct __go_func_type;
void	runtime_args(int32, byte**);
void	runtime_osinit();
void	runtime_goargs(void);
void	runtime_goenvs(void);
void	runtime_goenvs_unix(void);
void	runtime_throw(const char*) __attribute__ ((noreturn));
void	runtime_panicstring(const char*) __attribute__ ((noreturn));
void	runtime_prints(const char*);
void	runtime_printf(const char*, ...);
#define runtime_mcmp(a, b, s) __builtin_memcmp((a), (b), (s))
#define runtime_memmove(a, b, s) __builtin_memmove((a), (b), (s))
void*	runtime_mal(uintptr);
String	runtime_gostring(const byte*);
String	runtime_gostringnocopy(const byte*);
void	runtime_schedinit(void);
void	runtime_initsig(void);
void	runtime_sigenable(uint32 sig);
void	runtime_sigdisable(uint32 sig);
int32	runtime_gotraceback(bool *crash);
void	runtime_goroutineheader(G*);
void	runtime_goroutinetrailer(G*);
void	runtime_printtrace(Location*, int32, bool);
#define runtime_open(p, f, m) open((p), (f), (m))
#define runtime_read(d, v, n) read((d), (v), (n))
#define runtime_write(d, v, n) write((d), (v), (n))
#define runtime_close(d) close(d)
#define runtime_cas(pval, old, new) __sync_bool_compare_and_swap (pval, old, new)
#define runtime_cas64(pval, pold, new) __atomic_compare_exchange_n (pval, pold, new, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)
#define runtime_casp(pval, old, new) __sync_bool_compare_and_swap (pval, old, new)
// Don't confuse with XADD x86 instruction,
// this one is actually 'addx', that is, add-and-fetch.
#define runtime_xadd(p, v) __sync_add_and_fetch (p, v)
#define runtime_xadd64(p, v) __sync_add_and_fetch (p, v)
#define runtime_xchg(p, v) __atomic_exchange_n (p, v, __ATOMIC_SEQ_CST)
#define runtime_xchg64(p, v) __atomic_exchange_n (p, v, __ATOMIC_SEQ_CST)
#define runtime_atomicload(p) __atomic_load_n (p, __ATOMIC_SEQ_CST)
#define runtime_atomicstore(p, v) __atomic_store_n (p, v, __ATOMIC_SEQ_CST)
#define runtime_atomicstore64(p, v) __atomic_store_n (p, v, __ATOMIC_SEQ_CST)
#define runtime_atomicload64(p) __atomic_load_n (p, __ATOMIC_SEQ_CST)
#define runtime_atomicloadp(p) __atomic_load_n (p, __ATOMIC_SEQ_CST)
#define runtime_atomicstorep(p, v) __atomic_store_n (p, v, __ATOMIC_SEQ_CST)
void	runtime_ready(G*);
const byte*	runtime_getenv(const char*);
int32	runtime_atoi(const byte*);
void*	runtime_mstart(void*);
G*	runtime_malg(int32, byte**, size_t*);
void	runtime_mpreinit(M*);
void	runtime_minit(void);
void	runtime_unminit(void);
void	runtime_needm(void);
void	runtime_dropm(void);
void	runtime_signalstack(byte*, int32);
MCache*	runtime_allocmcache(void);
void	runtime_freemcache(MCache*);
void	runtime_mallocinit(void);
void	runtime_mprofinit(void);
#define runtime_malloc(s) __go_alloc(s)
#define runtime_free(p) __go_free(p)
bool	runtime_addfinalizer(void*, FuncVal *fn, const struct __go_func_type *);
#define runtime_getcallersp(p) __builtin_frame_address(1)
int32	runtime_mcount(void);
int32	runtime_gcount(void);
uint32	runtime_fastrand1(void);

void runtime_setmg(M*, G*);
void runtime_newextram(void);
#define runtime_exit(s) exit(s)
#define runtime_breakpoint() __builtin_trap()
void	runtime_gosched(void);
void	runtime_park(void(*)(Lock*), Lock*, const char*);
void	runtime_tsleep(int64, const char*);
M*	runtime_newm(void);
void	runtime_goexit(void);
void	runtime_entersyscall(void) __asm__ (GOSYM_PREFIX "syscall.Entersyscall");
void	runtime_entersyscallblock(void);
void	runtime_exitsyscall(void) __asm__ (GOSYM_PREFIX "syscall.Exitsyscall");
G*	__go_go(void (*pfn)(void*), void*);
void	siginit(void);
bool	__go_sigsend(int32 sig);
int32	runtime_callers(int32, Location*, int32);
int64	runtime_nanotime(void);
void	runtime_dopanic(int32) __attribute__ ((noreturn));
void	runtime_startpanic(void);
void	runtime_sigprof();
void	runtime_resetcpuprofiler(int32);
void	runtime_setcpuprofilerate(void(*)(uintptr*, int32), int32);
void	runtime_usleep(uint32);
int64	runtime_cputicks(void);
int64	runtime_tickspersecond(void);
void	runtime_blockevent(int64, int32);
extern int64 runtime_blockprofilerate;
void	runtime_addtimer(Timer*);
bool	runtime_deltimer(Timer*);
G*	runtime_netpoll(bool);
void	runtime_netpollinit(void);
int32	runtime_netpollopen(int32, PollDesc*);
int32   runtime_netpollclose(int32);
void	runtime_netpollready(G**, PollDesc*, int32);
void	runtime_crash(void);

void	runtime_stoptheworld(void);
void	runtime_starttheworld(void);
extern uint32 runtime_worldsema;

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
 * Lock-free stack.
 * Initialize uint64 head to 0, compare with 0 to test for emptiness.
 * The stack does not keep pointers to nodes,
 * so they can be garbage collected if there are no other pointers to nodes.
 */
void	runtime_lfstackpush(uint64 *head, LFNode *node)
  __asm__ (GOSYM_PREFIX "runtime.lfstackpush");
LFNode*	runtime_lfstackpop(uint64 *head);

/*
 * Parallel for over [0, n).
 * body() is executed for each iteration.
 * nthr - total number of worker threads.
 * ctx - arbitrary user context.
 * if wait=true, threads return from parfor() when all work is done;
 * otherwise, threads can return while other threads are still finishing processing.
 */
ParFor*	runtime_parforalloc(uint32 nthrmax);
void	runtime_parforsetup(ParFor *desc, uint32 nthr, uint32 n, void *ctx, bool wait, void (*body)(ParFor*, uint32));
void	runtime_parfordo(ParFor *desc) __asm__ (GOSYM_PREFIX "runtime.parfordo");

/*
 * low level C-called
 */
#define runtime_mmap mmap
#define runtime_munmap munmap
#define runtime_madvise madvise
#define runtime_memclr(buf, size) __builtin_memset((buf), 0, (size))
#define runtime_getcallerpc(p) __builtin_return_address(0)

#ifdef __rtems__
void __wrap_rtems_task_variable_add(void **);
#endif

/*
 * Names generated by gccgo.
 */
#define runtime_printbool	__go_print_bool
#define runtime_printfloat	__go_print_double
#define runtime_printint	__go_print_int64
#define runtime_printiface	__go_print_interface
#define runtime_printeface	__go_print_empty_interface
#define runtime_printstring	__go_print_string
#define runtime_printpointer	__go_print_pointer
#define runtime_printuint	__go_print_uint64
#define runtime_printslice	__go_print_slice
#define runtime_printcomplex	__go_print_complex

/*
 * runtime go-called
 */
void	runtime_printbool(_Bool);
void	runtime_printbyte(int8);
void	runtime_printfloat(double);
void	runtime_printint(int64);
void	runtime_printiface(Iface);
void	runtime_printeface(Eface);
void	runtime_printstring(String);
void	runtime_printpc(void*);
void	runtime_printpointer(void*);
void	runtime_printuint(uint64);
void	runtime_printhex(uint64);
void	runtime_printslice(Slice);
void	runtime_printcomplex(__complex double);
void reflect_call(const struct __go_func_type *, FuncVal *, _Bool, _Bool,
		  void **, void **)
  __asm__ (GOSYM_PREFIX "reflect.call");
#define runtime_panic __go_panic

/*
 * runtime c-called (but written in Go)
 */
void	runtime_printany(Eface)
     __asm__ (GOSYM_PREFIX "runtime.Printany");
void	runtime_newTypeAssertionError(const String*, const String*, const String*, const String*, Eface*)
     __asm__ (GOSYM_PREFIX "runtime.NewTypeAssertionError");
void	runtime_newErrorString(String, Eface*)
     __asm__ (GOSYM_PREFIX "runtime.NewErrorString");

/*
 * wrapped for go users
 */
void	runtime_semacquire(uint32 volatile *);
void	runtime_semrelease(uint32 volatile *);
int32	runtime_gomaxprocsfunc(int32 n);
void	runtime_procyield(uint32);
void	runtime_osyield(void);
void	runtime_lockOSThread(void);
void	runtime_unlockOSThread(void);

bool	runtime_showframe(String, bool);

uintptr	runtime_memlimit(void);

// If appropriate, ask the operating system to control whether this
// thread should receive profiling signals.  This is only necessary on OS X.
// An operating system should not deliver a profiling signal to a
// thread that is not actually executing (what good is that?), but that's
// what OS X prefers to do.  When profiling is turned on, we mask
// away the profiling signal when threads go to sleep, so that OS X
// is forced to deliver the signal to a thread that's actually running.
// This is a no-op on other systems.
void	runtime_setprof(bool);

#define ISNAN(f) __builtin_isnan(f)

enum
{
	UseSpanType = 0,
};

#define runtime_setitimer setitimer

void	runtime_check(void);

// A list of global variables that the garbage collector must scan.
struct root_list {
	struct root_list *next;
	struct root {
		void *decl;
		size_t size;
	} roots[];
};

void	__go_register_gc_roots(struct root_list*);

// Size of stack space allocated using Go's allocator.
// This will be 0 when using split stacks, as in that case
// the stacks are allocated by the splitstack library.
extern uintptr runtime_stacks_sys;

struct backtrace_state;
extern struct backtrace_state *__go_get_backtrace_state(void);
extern _Bool __go_file_line(uintptr, String*, String*, intgo *);
extern byte* runtime_progname();
extern void runtime_main(void*);

int32 getproccount(void);

#define PREFETCH(p) __builtin_prefetch(p)
