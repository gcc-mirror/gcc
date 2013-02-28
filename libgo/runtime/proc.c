// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <limits.h>
#include <signal.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

#include "config.h"

#ifdef HAVE_DL_ITERATE_PHDR
#include <link.h>
#endif

#include "runtime.h"
#include "arch.h"
#include "defs.h"
#include "malloc.h"
#include "race.h"
#include "go-type.h"
#include "go-defer.h"

#ifdef USING_SPLIT_STACK

/* FIXME: These are not declared anywhere.  */

extern void __splitstack_getcontext(void *context[10]);

extern void __splitstack_setcontext(void *context[10]);

extern void *__splitstack_makecontext(size_t, void *context[10], size_t *);

extern void * __splitstack_resetcontext(void *context[10], size_t *);

extern void *__splitstack_find(void *, void *, size_t *, void **, void **,
			       void **);

extern void __splitstack_block_signals (int *, int *);

extern void __splitstack_block_signals_context (void *context[10], int *,
						int *);

#endif

#ifndef PTHREAD_STACK_MIN
# define PTHREAD_STACK_MIN 8192
#endif

#if defined(USING_SPLIT_STACK) && defined(LINKER_SUPPORTS_SPLIT_STACK)
# define StackMin PTHREAD_STACK_MIN
#else
# define StackMin 2 * 1024 * 1024
#endif

uintptr runtime_stacks_sys;

static void schedule(G*);

static void gtraceback(G*);

typedef struct Sched Sched;

M	runtime_m0;
G	runtime_g0;	// idle goroutine for m0

#ifdef __rtems__
#define __thread
#endif

static __thread G *g;
static __thread M *m;

#ifndef SETCONTEXT_CLOBBERS_TLS

static inline void
initcontext(void)
{
}

static inline void
fixcontext(ucontext_t *c __attribute__ ((unused)))
{
}

#else

# if defined(__x86_64__) && defined(__sun__)

// x86_64 Solaris 10 and 11 have a bug: setcontext switches the %fs
// register to that of the thread which called getcontext.  The effect
// is that the address of all __thread variables changes.  This bug
// also affects pthread_self() and pthread_getspecific.  We work
// around it by clobbering the context field directly to keep %fs the
// same.

static __thread greg_t fs;

static inline void
initcontext(void)
{
	ucontext_t c;

	getcontext(&c);
	fs = c.uc_mcontext.gregs[REG_FSBASE];
}

static inline void
fixcontext(ucontext_t* c)
{
	c->uc_mcontext.gregs[REG_FSBASE] = fs;
}

# elif defined(__NetBSD__)

// NetBSD has a bug: setcontext clobbers tlsbase, we need to save
// and restore it ourselves.

static __thread __greg_t tlsbase;

static inline void
initcontext(void)
{
	ucontext_t c;

	getcontext(&c);
	tlsbase = c.uc_mcontext._mc_tlsbase;
}

static inline void
fixcontext(ucontext_t* c)
{
	c->uc_mcontext._mc_tlsbase = tlsbase;
}

# else

#  error unknown case for SETCONTEXT_CLOBBERS_TLS

# endif

#endif

// We can not always refer to the TLS variables directly.  The
// compiler will call tls_get_addr to get the address of the variable,
// and it may hold it in a register across a call to schedule.  When
// we get back from the call we may be running in a different thread,
// in which case the register now points to the TLS variable for a
// different thread.  We use non-inlinable functions to avoid this
// when necessary.

G* runtime_g(void) __attribute__ ((noinline, no_split_stack));

G*
runtime_g(void)
{
	return g;
}

M* runtime_m(void) __attribute__ ((noinline, no_split_stack));

M*
runtime_m(void)
{
	return m;
}

int32	runtime_gcwaiting;

G*	runtime_allg;
G*	runtime_lastg;
M*	runtime_allm;

int8*	runtime_goos;
int32	runtime_ncpu;

// The static TLS size.  See runtime_newm.
static int tlssize;

#ifdef HAVE_DL_ITERATE_PHDR

// Called via dl_iterate_phdr.

static int
addtls(struct dl_phdr_info* info, size_t size __attribute__ ((unused)), void *data)
{
	size_t *total = (size_t *)data;
	unsigned int i;

	for(i = 0; i < info->dlpi_phnum; ++i) {
		if(info->dlpi_phdr[i].p_type == PT_TLS)
			*total += info->dlpi_phdr[i].p_memsz;
	}
	return 0;
}

// Set the total TLS size.

static void
inittlssize()
{
	size_t total = 0;

	dl_iterate_phdr(addtls, (void *)&total);
	tlssize = total;
}

#else

static void
inittlssize()
{
}

#endif

// Go scheduler
//
// The go scheduler's job is to match ready-to-run goroutines (`g's)
// with waiting-for-work schedulers (`m's).  If there are ready g's
// and no waiting m's, ready() will start a new m running in a new
// OS thread, so that all ready g's can run simultaneously, up to a limit.
// For now, m's never go away.
//
// By default, Go keeps only one kernel thread (m) running user code
// at a single time; other threads may be blocked in the operating system.
// Setting the environment variable $GOMAXPROCS or calling
// runtime.GOMAXPROCS() will change the number of user threads
// allowed to execute simultaneously.  $GOMAXPROCS is thus an
// approximation of the maximum number of cores to use.
//
// Even a program that can run without deadlock in a single process
// might use more m's if given the chance.  For example, the prime
// sieve will use as many m's as there are primes (up to runtime_sched.mmax),
// allowing different stages of the pipeline to execute in parallel.
// We could revisit this choice, only kicking off new m's for blocking
// system calls, but that would limit the amount of parallel computation
// that go would try to do.
//
// In general, one could imagine all sorts of refinements to the
// scheduler, but the goal now is just to get something working on
// Linux and OS X.

struct Sched {
	Lock;

	G *gfree;	// available g's (status == Gdead)
	int64 goidgen;

	G *ghead;	// g's waiting to run
	G *gtail;
	int32 gwait;	// number of g's waiting to run
	int32 gcount;	// number of g's that are alive
	int32 grunning;	// number of g's running on cpu or in syscall

	M *mhead;	// m's waiting for work
	int32 mwait;	// number of m's waiting for work
	int32 mcount;	// number of m's that have been created

	volatile uint32 atomic;	// atomic scheduling word (see below)

	int32 profilehz;	// cpu profiling rate

	bool init;  // running initialization
	bool lockmain;  // init called runtime.LockOSThread

	Note	stopped;	// one g can set waitstop and wait here for m's to stop
};

// The atomic word in sched is an atomic uint32 that
// holds these fields.
//
//	[15 bits] mcpu		number of m's executing on cpu
//	[15 bits] mcpumax	max number of m's allowed on cpu
//	[1 bit] waitstop	some g is waiting on stopped
//	[1 bit] gwaiting	gwait != 0
//
// These fields are the information needed by entersyscall
// and exitsyscall to decide whether to coordinate with the
// scheduler.  Packing them into a single machine word lets
// them use a fast path with a single atomic read/write and
// no lock/unlock.  This greatly reduces contention in
// syscall- or cgo-heavy multithreaded programs.
//
// Except for entersyscall and exitsyscall, the manipulations
// to these fields only happen while holding the schedlock,
// so the routines holding schedlock only need to worry about
// what entersyscall and exitsyscall do, not the other routines
// (which also use the schedlock).
//
// In particular, entersyscall and exitsyscall only read mcpumax,
// waitstop, and gwaiting.  They never write them.  Thus, writes to those
// fields can be done (holding schedlock) without fear of write conflicts.
// There may still be logic conflicts: for example, the set of waitstop must
// be conditioned on mcpu >= mcpumax or else the wait may be a
// spurious sleep.  The Promela model in proc.p verifies these accesses.
enum {
	mcpuWidth = 15,
	mcpuMask = (1<<mcpuWidth) - 1,
	mcpuShift = 0,
	mcpumaxShift = mcpuShift + mcpuWidth,
	waitstopShift = mcpumaxShift + mcpuWidth,
	gwaitingShift = waitstopShift+1,

	// The max value of GOMAXPROCS is constrained
	// by the max value we can store in the bit fields
	// of the atomic word.  Reserve a few high values
	// so that we can detect accidental decrement
	// beyond zero.
	maxgomaxprocs = mcpuMask - 10,
};

#define atomic_mcpu(v)		(((v)>>mcpuShift)&mcpuMask)
#define atomic_mcpumax(v)	(((v)>>mcpumaxShift)&mcpuMask)
#define atomic_waitstop(v)	(((v)>>waitstopShift)&1)
#define atomic_gwaiting(v)	(((v)>>gwaitingShift)&1)

Sched runtime_sched;
int32 runtime_gomaxprocs;
bool runtime_singleproc;

static bool canaddmcpu(void);

// An m that is waiting for notewakeup(&m->havenextg).  This may
// only be accessed while the scheduler lock is held.  This is used to
// minimize the number of times we call notewakeup while the scheduler
// lock is held, since the m will normally move quickly to lock the
// scheduler itself, producing lock contention.
static M* mwakeup;

// Scheduling helpers.  Sched must be locked.
static void gput(G*);	// put/get on ghead/gtail
static G* gget(void);
static void mput(M*);	// put/get on mhead
static M* mget(G*);
static void gfput(G*);	// put/get on gfree
static G* gfget(void);
static void matchmg(void);	// match m's to g's
static void readylocked(G*);	// ready, but sched is locked
static void mnextg(M*, G*);
static void mcommoninit(M*);

void
setmcpumax(uint32 n)
{
	uint32 v, w;

	for(;;) {
		v = runtime_sched.atomic;
		w = v;
		w &= ~(mcpuMask<<mcpumaxShift);
		w |= n<<mcpumaxShift;
		if(runtime_cas(&runtime_sched.atomic, v, w))
			break;
	}
}

// First function run by a new goroutine.  This replaces gogocall.
static void
kickoff(void)
{
	void (*fn)(void*);

	if(g->traceback != nil)
		gtraceback(g);

	fn = (void (*)(void*))(g->entry);
	fn(g->param);
	runtime_goexit();
}

// Switch context to a different goroutine.  This is like longjmp.
static void runtime_gogo(G*) __attribute__ ((noinline));
static void
runtime_gogo(G* newg)
{
#ifdef USING_SPLIT_STACK
	__splitstack_setcontext(&newg->stack_context[0]);
#endif
	g = newg;
	newg->fromgogo = true;
	fixcontext(&newg->context);
	setcontext(&newg->context);
	runtime_throw("gogo setcontext returned");
}

// Save context and call fn passing g as a parameter.  This is like
// setjmp.  Because getcontext always returns 0, unlike setjmp, we use
// g->fromgogo as a code.  It will be true if we got here via
// setcontext.  g == nil the first time this is called in a new m.
static void runtime_mcall(void (*)(G*)) __attribute__ ((noinline));
static void
runtime_mcall(void (*pfn)(G*))
{
	M *mp;
	G *gp;
#ifndef USING_SPLIT_STACK
	int i;
#endif

	// Ensure that all registers are on the stack for the garbage
	// collector.
	__builtin_unwind_init();

	mp = m;
	gp = g;
	if(gp == mp->g0)
		runtime_throw("runtime: mcall called on m->g0 stack");

	if(gp != nil) {

#ifdef USING_SPLIT_STACK
		__splitstack_getcontext(&g->stack_context[0]);
#else
		gp->gcnext_sp = &i;
#endif
		gp->fromgogo = false;
		getcontext(&gp->context);

		// When we return from getcontext, we may be running
		// in a new thread.  That means that m and g may have
		// changed.  They are global variables so we will
		// reload them, but the addresses of m and g may be
		// cached in our local stack frame, and those
		// addresses may be wrong.  Call functions to reload
		// the values for this thread.
		mp = runtime_m();
		gp = runtime_g();

		if(gp->traceback != nil)
			gtraceback(gp);
	}
	if (gp == nil || !gp->fromgogo) {
#ifdef USING_SPLIT_STACK
		__splitstack_setcontext(&mp->g0->stack_context[0]);
#endif
		mp->g0->entry = (byte*)pfn;
		mp->g0->param = gp;

		// It's OK to set g directly here because this case
		// can not occur if we got here via a setcontext to
		// the getcontext call just above.
		g = mp->g0;

		fixcontext(&mp->g0->context);
		setcontext(&mp->g0->context);
		runtime_throw("runtime: mcall function returned");
	}
}

// Keep trace of scavenger's goroutine for deadlock detection.
static G *scvg;

// The bootstrap sequence is:
//
//	call osinit
//	call schedinit
//	make & queue new G
//	call runtime_mstart
//
// The new G calls runtime_main.
void
runtime_schedinit(void)
{
	int32 n;
	const byte *p;

	m = &runtime_m0;
	g = &runtime_g0;
	m->g0 = g;
	m->curg = g;
	g->m = m;

	initcontext();
	inittlssize();

	m->nomemprof++;
	runtime_mallocinit();
	mcommoninit(m);

	runtime_goargs();
	runtime_goenvs();

	// For debugging:
	// Allocate internal symbol table representation now,
	// so that we don't need to call malloc when we crash.
	// runtime_findfunc(0);

	runtime_gomaxprocs = 1;
	p = runtime_getenv("GOMAXPROCS");
	if(p != nil && (n = runtime_atoi(p)) != 0) {
		if(n > maxgomaxprocs)
			n = maxgomaxprocs;
		runtime_gomaxprocs = n;
	}
	// wait for the main goroutine to start before taking
	// GOMAXPROCS into account.
	setmcpumax(1);
	runtime_singleproc = runtime_gomaxprocs == 1;

	canaddmcpu();	// mcpu++ to account for bootstrap m
	m->helpgc = 1;	// flag to tell schedule() to mcpu--
	runtime_sched.grunning++;

	// Can not enable GC until all roots are registered.
	// mstats.enablegc = 1;
	m->nomemprof--;

	if(raceenabled)
		runtime_raceinit();
}

extern void main_init(void) __asm__ (GOSYM_PREFIX "__go_init_main");
extern void main_main(void) __asm__ (GOSYM_PREFIX "main.main");

// The main goroutine.
void
runtime_main(void)
{
	// Lock the main goroutine onto this, the main OS thread,
	// during initialization.  Most programs won't care, but a few
	// do require certain calls to be made by the main thread.
	// Those can arrange for main.main to run in the main thread
	// by calling runtime.LockOSThread during initialization
	// to preserve the lock.
	runtime_LockOSThread();
	// From now on, newgoroutines may use non-main threads.
	setmcpumax(runtime_gomaxprocs);
	runtime_sched.init = true;
	scvg = __go_go(runtime_MHeap_Scavenger, nil);
	scvg->issystem = true;
	main_init();
	runtime_sched.init = false;
	if(!runtime_sched.lockmain)
		runtime_UnlockOSThread();

	// For gccgo we have to wait until after main is initialized
	// to enable GC, because initializing main registers the GC
	// roots.
	mstats.enablegc = 1;

	// The deadlock detection has false negatives.
	// Let scvg start up, to eliminate the false negative
	// for the trivial program func main() { select{} }.
	runtime_gosched();

	main_main();
	if(raceenabled)
		runtime_racefini();
	runtime_exit(0);
	for(;;)
		*(int32*)0 = 0;
}

// Lock the scheduler.
static void
schedlock(void)
{
	runtime_lock(&runtime_sched);
}

// Unlock the scheduler.
static void
schedunlock(void)
{
	M *mp;

	mp = mwakeup;
	mwakeup = nil;
	runtime_unlock(&runtime_sched);
	if(mp != nil)
		runtime_notewakeup(&mp->havenextg);
}

void
runtime_goexit(void)
{
	g->status = Gmoribund;
	runtime_gosched();
}

void
runtime_goroutineheader(G *gp)
{
	const char *status;

	switch(gp->status) {
	case Gidle:
		status = "idle";
		break;
	case Grunnable:
		status = "runnable";
		break;
	case Grunning:
		status = "running";
		break;
	case Gsyscall:
		status = "syscall";
		break;
	case Gwaiting:
		if(gp->waitreason)
			status = gp->waitreason;
		else
			status = "waiting";
		break;
	case Gmoribund:
		status = "moribund";
		break;
	default:
		status = "???";
		break;
	}
	runtime_printf("goroutine %D [%s]:\n", gp->goid, status);
}

void
runtime_goroutinetrailer(G *g)
{
	if(g != nil && g->gopc != 0 && g->goid != 1) {
		String fn;
		String file;
		intgo line;

		if(__go_file_line(g->gopc - 1, &fn, &file, &line)) {
			runtime_printf("created by %S\n", fn);
			runtime_printf("\t%S:%D\n", file, (int64) line);
		}
	}
}

struct Traceback
{
	G* gp;
	Location locbuf[100];
	int32 c;
};

void
runtime_tracebackothers(G * volatile me)
{
	G * volatile gp;
	Traceback tb;
	int32 traceback;

	tb.gp = me;
	traceback = runtime_gotraceback();
	for(gp = runtime_allg; gp != nil; gp = gp->alllink) {
		if(gp == me || gp->status == Gdead)
			continue;
		if(gp->issystem && traceback < 2)
			continue;
		runtime_printf("\n");
		runtime_goroutineheader(gp);

		// Our only mechanism for doing a stack trace is
		// _Unwind_Backtrace.  And that only works for the
		// current thread, not for other random goroutines.
		// So we need to switch context to the goroutine, get
		// the backtrace, and then switch back.

		// This means that if g is running or in a syscall, we
		// can't reliably print a stack trace.  FIXME.
		if(gp->status == Gsyscall || gp->status == Grunning) {
			runtime_printf("no stack trace available\n");
			runtime_goroutinetrailer(gp);
			continue;
		}

		gp->traceback = &tb;

#ifdef USING_SPLIT_STACK
		__splitstack_getcontext(&me->stack_context[0]);
#endif
		getcontext(&me->context);

		if(gp->traceback != nil) {
			runtime_gogo(gp);
		}

		runtime_printtrace(tb.locbuf, tb.c, false);
		runtime_goroutinetrailer(gp);
	}
}

// Do a stack trace of gp, and then restore the context to
// gp->dotraceback.

static void
gtraceback(G* gp)
{
	Traceback* traceback;

	traceback = gp->traceback;
	gp->traceback = nil;
	traceback->c = runtime_callers(1, traceback->locbuf,
		sizeof traceback->locbuf / sizeof traceback->locbuf[0]);
	runtime_gogo(traceback->gp);
}

// Mark this g as m's idle goroutine.
// This functionality might be used in environments where programs
// are limited to a single thread, to simulate a select-driven
// network server.  It is not exposed via the standard runtime API.
void
runtime_idlegoroutine(void)
{
	if(g->idlem != nil)
		runtime_throw("g is already an idle goroutine");
	g->idlem = m;
}

static void
mcommoninit(M *mp)
{
	mp->id = runtime_sched.mcount++;
	mp->fastrand = 0x49f6428aUL + mp->id + runtime_cputicks();

	if(mp->mcache == nil)
		mp->mcache = runtime_allocmcache();

	runtime_callers(1, mp->createstack, nelem(mp->createstack));

	// Add to runtime_allm so garbage collector doesn't free m
	// when it is just in a register or thread-local storage.
	mp->alllink = runtime_allm;
	// runtime_NumCgoCall() iterates over allm w/o schedlock,
	// so we need to publish it safely.
	runtime_atomicstorep(&runtime_allm, mp);
}

// Try to increment mcpu.  Report whether succeeded.
static bool
canaddmcpu(void)
{
	uint32 v;

	for(;;) {
		v = runtime_sched.atomic;
		if(atomic_mcpu(v) >= atomic_mcpumax(v))
			return 0;
		if(runtime_cas(&runtime_sched.atomic, v, v+(1<<mcpuShift)))
			return 1;
	}
}

// Put on `g' queue.  Sched must be locked.
static void
gput(G *gp)
{
	M *mp;

	// If g is wired, hand it off directly.
	if((mp = gp->lockedm) != nil && canaddmcpu()) {
		mnextg(mp, gp);
		return;
	}

	// If g is the idle goroutine for an m, hand it off.
	if(gp->idlem != nil) {
		if(gp->idlem->idleg != nil) {
			runtime_printf("m%d idle out of sync: g%D g%D\n",
				gp->idlem->id,
				gp->idlem->idleg->goid, gp->goid);
			runtime_throw("runtime: double idle");
		}
		gp->idlem->idleg = gp;
		return;
	}

	gp->schedlink = nil;
	if(runtime_sched.ghead == nil)
		runtime_sched.ghead = gp;
	else
		runtime_sched.gtail->schedlink = gp;
	runtime_sched.gtail = gp;

	// increment gwait.
	// if it transitions to nonzero, set atomic gwaiting bit.
	if(runtime_sched.gwait++ == 0)
		runtime_xadd(&runtime_sched.atomic, 1<<gwaitingShift);
}

// Report whether gget would return something.
static bool
haveg(void)
{
	return runtime_sched.ghead != nil || m->idleg != nil;
}

// Get from `g' queue.  Sched must be locked.
static G*
gget(void)
{
	G *gp;

	gp = runtime_sched.ghead;
	if(gp) {
		runtime_sched.ghead = gp->schedlink;
		if(runtime_sched.ghead == nil)
			runtime_sched.gtail = nil;
		// decrement gwait.
		// if it transitions to zero, clear atomic gwaiting bit.
		if(--runtime_sched.gwait == 0)
			runtime_xadd(&runtime_sched.atomic, -1<<gwaitingShift);
	} else if(m->idleg != nil) {
		gp = m->idleg;
		m->idleg = nil;
	}
	return gp;
}

// Put on `m' list.  Sched must be locked.
static void
mput(M *mp)
{
	mp->schedlink = runtime_sched.mhead;
	runtime_sched.mhead = mp;
	runtime_sched.mwait++;
}

// Get an `m' to run `g'.  Sched must be locked.
static M*
mget(G *gp)
{
	M *mp;

	// if g has its own m, use it.
	if(gp && (mp = gp->lockedm) != nil)
		return mp;

	// otherwise use general m pool.
	if((mp = runtime_sched.mhead) != nil) {
		runtime_sched.mhead = mp->schedlink;
		runtime_sched.mwait--;
	}
	return mp;
}

// Mark g ready to run.
void
runtime_ready(G *gp)
{
	schedlock();
	readylocked(gp);
	schedunlock();
}

// Mark g ready to run.  Sched is already locked.
// G might be running already and about to stop.
// The sched lock protects g->status from changing underfoot.
static void
readylocked(G *gp)
{
	if(gp->m) {
		// Running on another machine.
		// Ready it when it stops.
		gp->readyonstop = 1;
		return;
	}

	// Mark runnable.
	if(gp->status == Grunnable || gp->status == Grunning) {
		runtime_printf("goroutine %D has status %d\n", gp->goid, gp->status);
		runtime_throw("bad g->status in ready");
	}
	gp->status = Grunnable;

	gput(gp);
	matchmg();
}

// Same as readylocked but a different symbol so that
// debuggers can set a breakpoint here and catch all
// new goroutines.
static void
newprocreadylocked(G *gp)
{
	readylocked(gp);
}

// Pass g to m for running.
// Caller has already incremented mcpu.
static void
mnextg(M *mp, G *gp)
{
	runtime_sched.grunning++;
	mp->nextg = gp;
	if(mp->waitnextg) {
		mp->waitnextg = 0;
		if(mwakeup != nil)
			runtime_notewakeup(&mwakeup->havenextg);
		mwakeup = mp;
	}
}

// Get the next goroutine that m should run.
// Sched must be locked on entry, is unlocked on exit.
// Makes sure that at most $GOMAXPROCS g's are
// running on cpus (not in system calls) at any given time.
static G*
nextgandunlock(void)
{
	G *gp;
	uint32 v;

top:
	if(atomic_mcpu(runtime_sched.atomic) >= maxgomaxprocs)
		runtime_throw("negative mcpu");

	// If there is a g waiting as m->nextg, the mcpu++
	// happened before it was passed to mnextg.
	if(m->nextg != nil) {
		gp = m->nextg;
		m->nextg = nil;
		schedunlock();
		return gp;
	}

	if(m->lockedg != nil) {
		// We can only run one g, and it's not available.
		// Make sure some other cpu is running to handle
		// the ordinary run queue.
		if(runtime_sched.gwait != 0) {
			matchmg();
			// m->lockedg might have been on the queue.
			if(m->nextg != nil) {
				gp = m->nextg;
				m->nextg = nil;
				schedunlock();
				return gp;
			}
		}
	} else {
		// Look for work on global queue.
		while(haveg() && canaddmcpu()) {
			gp = gget();
			if(gp == nil)
				runtime_throw("gget inconsistency");

			if(gp->lockedm) {
				mnextg(gp->lockedm, gp);
				continue;
			}
			runtime_sched.grunning++;
			schedunlock();
			return gp;
		}

		// The while loop ended either because the g queue is empty
		// or because we have maxed out our m procs running go
		// code (mcpu >= mcpumax).  We need to check that
		// concurrent actions by entersyscall/exitsyscall cannot
		// invalidate the decision to end the loop.
		//
		// We hold the sched lock, so no one else is manipulating the
		// g queue or changing mcpumax.  Entersyscall can decrement
		// mcpu, but if does so when there is something on the g queue,
		// the gwait bit will be set, so entersyscall will take the slow path
		// and use the sched lock.  So it cannot invalidate our decision.
		//
		// Wait on global m queue.
		mput(m);
	}

	// Look for deadlock situation.
	// There is a race with the scavenger that causes false negatives:
	// if the scavenger is just starting, then we have
	//	scvg != nil && grunning == 0 && gwait == 0
	// and we do not detect a deadlock.  It is possible that we should
	// add that case to the if statement here, but it is too close to Go 1
	// to make such a subtle change.  Instead, we work around the
	// false negative in trivial programs by calling runtime.gosched
	// from the main goroutine just before main.main.
	// See runtime_main above.
	//
	// On a related note, it is also possible that the scvg == nil case is
	// wrong and should include gwait, but that does not happen in
	// standard Go programs, which all start the scavenger.
	//
	if((scvg == nil && runtime_sched.grunning == 0) ||
	   (scvg != nil && runtime_sched.grunning == 1 && runtime_sched.gwait == 0 &&
	    (scvg->status == Grunning || scvg->status == Gsyscall))) {
		m->throwing = -1;  // do not dump full stacks
		runtime_throw("all goroutines are asleep - deadlock!");
	}

	m->nextg = nil;
	m->waitnextg = 1;
	runtime_noteclear(&m->havenextg);

	// Stoptheworld is waiting for all but its cpu to go to stop.
	// Entersyscall might have decremented mcpu too, but if so
	// it will see the waitstop and take the slow path.
	// Exitsyscall never increments mcpu beyond mcpumax.
	v = runtime_atomicload(&runtime_sched.atomic);
	if(atomic_waitstop(v) && atomic_mcpu(v) <= atomic_mcpumax(v)) {
		// set waitstop = 0 (known to be 1)
		runtime_xadd(&runtime_sched.atomic, -1<<waitstopShift);
		runtime_notewakeup(&runtime_sched.stopped);
	}
	schedunlock();

	runtime_notesleep(&m->havenextg);
	if(m->helpgc) {
		runtime_gchelper();
		m->helpgc = 0;
		runtime_lock(&runtime_sched);
		goto top;
	}
	if((gp = m->nextg) == nil)
		runtime_throw("bad m->nextg in nextgoroutine");
	m->nextg = nil;
	return gp;
}

int32
runtime_gcprocs(void)
{
	int32 n;
	
	// Figure out how many CPUs to use during GC.
	// Limited by gomaxprocs, number of actual CPUs, and MaxGcproc.
	n = runtime_gomaxprocs;
	if(n > runtime_ncpu)
		n = runtime_ncpu > 0 ? runtime_ncpu : 1;
	if(n > MaxGcproc)
		n = MaxGcproc;
	if(n > runtime_sched.mwait+1) // one M is currently running
		n = runtime_sched.mwait+1;
	return n;
}

void
runtime_helpgc(int32 nproc)
{
	M *mp;
	int32 n;

	runtime_lock(&runtime_sched);
	for(n = 1; n < nproc; n++) { // one M is currently running
		mp = mget(nil);
		if(mp == nil)
			runtime_throw("runtime_gcprocs inconsistency");
		mp->helpgc = 1;
		mp->waitnextg = 0;
		runtime_notewakeup(&mp->havenextg);
	}
	runtime_unlock(&runtime_sched);
}

void
runtime_stoptheworld(void)
{
	uint32 v;

	schedlock();
	runtime_gcwaiting = 1;

	setmcpumax(1);

	// while mcpu > 1
	for(;;) {
		v = runtime_sched.atomic;
		if(atomic_mcpu(v) <= 1)
			break;

		// It would be unsafe for multiple threads to be using
		// the stopped note at once, but there is only
		// ever one thread doing garbage collection.
		runtime_noteclear(&runtime_sched.stopped);
		if(atomic_waitstop(v))
			runtime_throw("invalid waitstop");

		// atomic { waitstop = 1 }, predicated on mcpu <= 1 check above
		// still being true.
		if(!runtime_cas(&runtime_sched.atomic, v, v+(1<<waitstopShift)))
			continue;

		schedunlock();
		runtime_notesleep(&runtime_sched.stopped);
		schedlock();
	}
	runtime_singleproc = runtime_gomaxprocs == 1;
	schedunlock();
}

void
runtime_starttheworld(void)
{
	M *mp;
	int32 max;
	
	// Figure out how many CPUs GC could possibly use.
	max = runtime_gomaxprocs;
	if(max > runtime_ncpu)
		max = runtime_ncpu > 0 ? runtime_ncpu : 1;
	if(max > MaxGcproc)
		max = MaxGcproc;

	schedlock();
	runtime_gcwaiting = 0;
	setmcpumax(runtime_gomaxprocs);
	matchmg();
	if(runtime_gcprocs() < max && canaddmcpu()) {
		// If GC could have used another helper proc, start one now,
		// in the hope that it will be available next time.
		// It would have been even better to start it before the collection,
		// but doing so requires allocating memory, so it's tricky to
		// coordinate.  This lazy approach works out in practice:
		// we don't mind if the first couple gc rounds don't have quite
		// the maximum number of procs.
		// canaddmcpu above did mcpu++
		// (necessary, because m will be doing various
		// initialization work so is definitely running),
		// but m is not running a specific goroutine,
		// so set the helpgc flag as a signal to m's
		// first schedule(nil) to mcpu-- and grunning--.
		mp = runtime_newm();
		mp->helpgc = 1;
		runtime_sched.grunning++;
	}
	schedunlock();
}

// Called to start an M.
void*
runtime_mstart(void* mp)
{
	m = (M*)mp;
	g = m->g0;

	initcontext();

	g->entry = nil;
	g->param = nil;

	// Record top of stack for use by mcall.
	// Once we call schedule we're never coming back,
	// so other calls can reuse this stack space.
#ifdef USING_SPLIT_STACK
	__splitstack_getcontext(&g->stack_context[0]);
#else
	g->gcinitial_sp = &mp;
	// Setting gcstack_size to 0 is a marker meaning that gcinitial_sp
	// is the top of the stack, not the bottom.
	g->gcstack_size = 0;
	g->gcnext_sp = &mp;
#endif
	getcontext(&g->context);

	if(g->entry != nil) {
		// Got here from mcall.
		void (*pfn)(G*) = (void (*)(G*))g->entry;
		G* gp = (G*)g->param;
		pfn(gp);
		*(int*)0x21 = 0x21;
	}
	runtime_minit();

#ifdef USING_SPLIT_STACK
	{
	  int dont_block_signals = 0;
	  __splitstack_block_signals(&dont_block_signals, nil);
	}
#endif

	// Install signal handlers; after minit so that minit can
	// prepare the thread to be able to handle the signals.
	if(m == &runtime_m0)
		runtime_initsig();

	schedule(nil);

	// TODO(brainman): This point is never reached, because scheduler
	// does not release os threads at the moment. But once this path
	// is enabled, we must remove our seh here.

	return nil;
}

typedef struct CgoThreadStart CgoThreadStart;
struct CgoThreadStart
{
	M *m;
	G *g;
	void (*fn)(void);
};

// Kick off new m's as needed (up to mcpumax).
// Sched is locked.
static void
matchmg(void)
{
	G *gp;
	M *mp;

	if(m->mallocing || m->gcing)
		return;

	while(haveg() && canaddmcpu()) {
		gp = gget();
		if(gp == nil)
			runtime_throw("gget inconsistency");

		// Find the m that will run gp.
		if((mp = mget(gp)) == nil)
			mp = runtime_newm();
		mnextg(mp, gp);
	}
}

// Create a new m.  It will start off with a call to runtime_mstart.
M*
runtime_newm(void)
{
	M *mp;
	pthread_attr_t attr;
	pthread_t tid;
	size_t stacksize;
	sigset_t clear;
	sigset_t old;
	int ret;

#if 0
	static const Type *mtype;  // The Go type M
	if(mtype == nil) {
		Eface e;
		runtime_gc_m_ptr(&e);
		mtype = ((const PtrType*)e.__type_descriptor)->__element_type;
	}
#endif

	mp = runtime_mal(sizeof *mp);
	mcommoninit(mp);
	mp->g0 = runtime_malg(-1, nil, nil);

	if(pthread_attr_init(&attr) != 0)
		runtime_throw("pthread_attr_init");
	if(pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0)
		runtime_throw("pthread_attr_setdetachstate");

	stacksize = PTHREAD_STACK_MIN;

	// With glibc before version 2.16 the static TLS size is taken
	// out of the stack size, and we get an error or a crash if
	// there is not enough stack space left.  Add it back in if we
	// can, in case the program uses a lot of TLS space.  FIXME:
	// This can be disabled in glibc 2.16 and later, if the bug is
	// indeed fixed then.
	stacksize += tlssize;

	if(pthread_attr_setstacksize(&attr, stacksize) != 0)
		runtime_throw("pthread_attr_setstacksize");

	// Block signals during pthread_create so that the new thread
	// starts with signals disabled.  It will enable them in minit.
	sigfillset(&clear);

#ifdef SIGTRAP
	// Blocking SIGTRAP reportedly breaks gdb on Alpha GNU/Linux.
	sigdelset(&clear, SIGTRAP);
#endif

	sigemptyset(&old);
	sigprocmask(SIG_BLOCK, &clear, &old);
	ret = pthread_create(&tid, &attr, runtime_mstart, mp);
	sigprocmask(SIG_SETMASK, &old, nil);

	if (ret != 0)
		runtime_throw("pthread_create");

	return mp;
}

// One round of scheduler: find a goroutine and run it.
// The argument is the goroutine that was running before
// schedule was called, or nil if this is the first call.
// Never returns.
static void
schedule(G *gp)
{
	int32 hz;
	uint32 v;

	schedlock();
	if(gp != nil) {
		// Just finished running gp.
		gp->m = nil;
		runtime_sched.grunning--;

		// atomic { mcpu-- }
		v = runtime_xadd(&runtime_sched.atomic, -1<<mcpuShift);
		if(atomic_mcpu(v) > maxgomaxprocs)
			runtime_throw("negative mcpu in scheduler");

		switch(gp->status) {
		case Grunnable:
		case Gdead:
			// Shouldn't have been running!
			runtime_throw("bad gp->status in sched");
		case Grunning:
			gp->status = Grunnable;
			gput(gp);
			break;
		case Gmoribund:
			if(raceenabled)
				runtime_racegoend(gp->goid);
			gp->status = Gdead;
			if(gp->lockedm) {
				gp->lockedm = nil;
				m->lockedg = nil;
			}
			gp->idlem = nil;
			runtime_memclr(&gp->context, sizeof gp->context);
			gfput(gp);
			if(--runtime_sched.gcount == 0)
				runtime_exit(0);
			break;
		}
		if(gp->readyonstop) {
			gp->readyonstop = 0;
			readylocked(gp);
		}
	} else if(m->helpgc) {
		// Bootstrap m or new m started by starttheworld.
		// atomic { mcpu-- }
		v = runtime_xadd(&runtime_sched.atomic, -1<<mcpuShift);
		if(atomic_mcpu(v) > maxgomaxprocs)
			runtime_throw("negative mcpu in scheduler");
		// Compensate for increment in starttheworld().
		runtime_sched.grunning--;
		m->helpgc = 0;
	} else if(m->nextg != nil) {
		// New m started by matchmg.
	} else {
		runtime_throw("invalid m state in scheduler");
	}

	// Find (or wait for) g to run.  Unlocks runtime_sched.
	gp = nextgandunlock();
	gp->readyonstop = 0;
	gp->status = Grunning;
	m->curg = gp;
	gp->m = m;

	// Check whether the profiler needs to be turned on or off.
	hz = runtime_sched.profilehz;
	if(m->profilehz != hz)
		runtime_resetcpuprofiler(hz);

	runtime_gogo(gp);
}

// Enter scheduler.  If g->status is Grunning,
// re-queues g and runs everyone else who is waiting
// before running g again.  If g->status is Gmoribund,
// kills off g.
void
runtime_gosched(void)
{
	if(m->locks != 0)
		runtime_throw("gosched holding locks");
	if(g == m->g0)
		runtime_throw("gosched of g0");
	runtime_mcall(schedule);
}

// Puts the current goroutine into a waiting state and unlocks the lock.
// The goroutine can be made runnable again by calling runtime_ready(gp).
void
runtime_park(void (*unlockf)(Lock*), Lock *lock, const char *reason)
{
	g->status = Gwaiting;
	g->waitreason = reason;
	if(unlockf)
		unlockf(lock);
	runtime_gosched();
}

// The goroutine g is about to enter a system call.
// Record that it's not using the cpu anymore.
// This is called only from the go syscall library and cgocall,
// not from the low-level system calls used by the runtime.
//
// Entersyscall cannot split the stack: the runtime_gosave must
// make g->sched refer to the caller's stack segment, because
// entersyscall is going to return immediately after.
// It's okay to call matchmg and notewakeup even after
// decrementing mcpu, because we haven't released the
// sched lock yet, so the garbage collector cannot be running.

void runtime_entersyscall(void) __attribute__ ((no_split_stack));

void
runtime_entersyscall(void)
{
	uint32 v;

	if(m->profilehz > 0)
		runtime_setprof(false);

	// Leave SP around for gc and traceback.
#ifdef USING_SPLIT_STACK
	g->gcstack = __splitstack_find(nil, nil, &g->gcstack_size,
				       &g->gcnext_segment, &g->gcnext_sp,
				       &g->gcinitial_sp);
#else
	g->gcnext_sp = (byte *) &v;
#endif

	// Save the registers in the g structure so that any pointers
	// held in registers will be seen by the garbage collector.
	getcontext(&g->gcregs);

	g->status = Gsyscall;

	// Fast path.
	// The slow path inside the schedlock/schedunlock will get
	// through without stopping if it does:
	//	mcpu--
	//	gwait not true
	//	waitstop && mcpu <= mcpumax not true
	// If we can do the same with a single atomic add,
	// then we can skip the locks.
	v = runtime_xadd(&runtime_sched.atomic, -1<<mcpuShift);
	if(!atomic_gwaiting(v) && (!atomic_waitstop(v) || atomic_mcpu(v) > atomic_mcpumax(v)))
		return;

	schedlock();
	v = runtime_atomicload(&runtime_sched.atomic);
	if(atomic_gwaiting(v)) {
		matchmg();
		v = runtime_atomicload(&runtime_sched.atomic);
	}
	if(atomic_waitstop(v) && atomic_mcpu(v) <= atomic_mcpumax(v)) {
		runtime_xadd(&runtime_sched.atomic, -1<<waitstopShift);
		runtime_notewakeup(&runtime_sched.stopped);
	}

	schedunlock();
}

// The goroutine g exited its system call.
// Arrange for it to run on a cpu again.
// This is called only from the go syscall library, not
// from the low-level system calls used by the runtime.
void
runtime_exitsyscall(void)
{
	G *gp;
	uint32 v;

	// Fast path.
	// If we can do the mcpu++ bookkeeping and
	// find that we still have mcpu <= mcpumax, then we can
	// start executing Go code immediately, without having to
	// schedlock/schedunlock.
	// Also do fast return if any locks are held, so that
	// panic code can use syscalls to open a file.
	gp = g;
	v = runtime_xadd(&runtime_sched.atomic, (1<<mcpuShift));
	if((m->profilehz == runtime_sched.profilehz && atomic_mcpu(v) <= atomic_mcpumax(v)) || m->locks > 0) {
		// There's a cpu for us, so we can run.
		gp->status = Grunning;
		// Garbage collector isn't running (since we are),
		// so okay to clear gcstack.
#ifdef USING_SPLIT_STACK
		gp->gcstack = nil;
#endif
		gp->gcnext_sp = nil;
		runtime_memclr(&gp->gcregs, sizeof gp->gcregs);

		if(m->profilehz > 0)
			runtime_setprof(true);
		return;
	}

	// Tell scheduler to put g back on the run queue:
	// mostly equivalent to g->status = Grunning,
	// but keeps the garbage collector from thinking
	// that g is running right now, which it's not.
	gp->readyonstop = 1;

	// All the cpus are taken.
	// The scheduler will ready g and put this m to sleep.
	// When the scheduler takes g away from m,
	// it will undo the runtime_sched.mcpu++ above.
	runtime_gosched();

	// Gosched returned, so we're allowed to run now.
	// Delete the gcstack information that we left for
	// the garbage collector during the system call.
	// Must wait until now because until gosched returns
	// we don't know for sure that the garbage collector
	// is not running.
#ifdef USING_SPLIT_STACK
	gp->gcstack = nil;
#endif
	gp->gcnext_sp = nil;
	runtime_memclr(&gp->gcregs, sizeof gp->gcregs);
}

// Allocate a new g, with a stack big enough for stacksize bytes.
G*
runtime_malg(int32 stacksize, byte** ret_stack, size_t* ret_stacksize)
{
	G *newg;

	newg = runtime_malloc(sizeof(G));
	if(stacksize >= 0) {
#if USING_SPLIT_STACK
		int dont_block_signals = 0;

		*ret_stack = __splitstack_makecontext(stacksize,
						      &newg->stack_context[0],
						      ret_stacksize);
		__splitstack_block_signals_context(&newg->stack_context[0],
						   &dont_block_signals, nil);
#else
		*ret_stack = runtime_mallocgc(stacksize, FlagNoProfiling|FlagNoGC, 0, 0);
		*ret_stacksize = stacksize;
		newg->gcinitial_sp = *ret_stack;
		newg->gcstack_size = stacksize;
		runtime_xadd(&runtime_stacks_sys, stacksize);
#endif
	}
	return newg;
}

/* For runtime package testing.  */

void runtime_testing_entersyscall(void)
  __asm__ (GOSYM_PREFIX "runtime.entersyscall");

void
runtime_testing_entersyscall()
{
	runtime_entersyscall();
}

void runtime_testing_exitsyscall(void)
  __asm__ (GOSYM_PREFIX "runtime.exitsyscall");

void
runtime_testing_exitsyscall()
{
	runtime_exitsyscall();
}

G*
__go_go(void (*fn)(void*), void* arg)
{
	byte *sp;
	size_t spsize;
	G *newg;
	int64 goid;

	goid = runtime_xadd64((uint64*)&runtime_sched.goidgen, 1);
	if(raceenabled)
		runtime_racegostart(goid, runtime_getcallerpc(&fn));

	schedlock();

	if((newg = gfget()) != nil) {
#ifdef USING_SPLIT_STACK
		int dont_block_signals = 0;

		sp = __splitstack_resetcontext(&newg->stack_context[0],
					       &spsize);
		__splitstack_block_signals_context(&newg->stack_context[0],
						   &dont_block_signals, nil);
#else
		sp = newg->gcinitial_sp;
		spsize = newg->gcstack_size;
		if(spsize == 0)
			runtime_throw("bad spsize in __go_go");
		newg->gcnext_sp = sp;
#endif
	} else {
		newg = runtime_malg(StackMin, &sp, &spsize);
		if(runtime_lastg == nil)
			runtime_allg = newg;
		else
			runtime_lastg->alllink = newg;
		runtime_lastg = newg;
	}
	newg->status = Gwaiting;
	newg->waitreason = "new goroutine";

	newg->entry = (byte*)fn;
	newg->param = arg;
	newg->gopc = (uintptr)__builtin_return_address(0);

	runtime_sched.gcount++;
	newg->goid = goid;

	if(sp == nil)
		runtime_throw("nil g->stack0");

	{
		// Avoid warnings about variables clobbered by
		// longjmp.
		byte * volatile vsp = sp;
		size_t volatile vspsize = spsize;
		G * volatile vnewg = newg;

		getcontext(&vnewg->context);
		vnewg->context.uc_stack.ss_sp = vsp;
#ifdef MAKECONTEXT_STACK_TOP
		vnewg->context.uc_stack.ss_sp += vspsize;
#endif
		vnewg->context.uc_stack.ss_size = vspsize;
		makecontext(&vnewg->context, kickoff, 0);

		newprocreadylocked(vnewg);
		schedunlock();

		return vnewg;
	}
}

// Put on gfree list.  Sched must be locked.
static void
gfput(G *gp)
{
	gp->schedlink = runtime_sched.gfree;
	runtime_sched.gfree = gp;
}

// Get from gfree list.  Sched must be locked.
static G*
gfget(void)
{
	G *gp;

	gp = runtime_sched.gfree;
	if(gp)
		runtime_sched.gfree = gp->schedlink;
	return gp;
}

void runtime_Gosched (void) __asm__ (GOSYM_PREFIX "runtime.Gosched");

void
runtime_Gosched(void)
{
	runtime_gosched();
}

// Implementation of runtime.GOMAXPROCS.
// delete when scheduler is stronger
int32
runtime_gomaxprocsfunc(int32 n)
{
	int32 ret;
	uint32 v;

	schedlock();
	ret = runtime_gomaxprocs;
	if(n <= 0)
		n = ret;
	if(n > maxgomaxprocs)
		n = maxgomaxprocs;
	runtime_gomaxprocs = n;
	if(runtime_gomaxprocs > 1)
		runtime_singleproc = false;
 	if(runtime_gcwaiting != 0) {
 		if(atomic_mcpumax(runtime_sched.atomic) != 1)
 			runtime_throw("invalid mcpumax during gc");
		schedunlock();
		return ret;
	}

	setmcpumax(n);

	// If there are now fewer allowed procs
	// than procs running, stop.
	v = runtime_atomicload(&runtime_sched.atomic);
	if((int32)atomic_mcpu(v) > n) {
		schedunlock();
		runtime_gosched();
		return ret;
	}
	// handle more procs
	matchmg();
	schedunlock();
	return ret;
}

void
runtime_LockOSThread(void)
{
	if(m == &runtime_m0 && runtime_sched.init) {
		runtime_sched.lockmain = true;
		return;
	}
	m->lockedg = g;
	g->lockedm = m;
}

void
runtime_UnlockOSThread(void)
{
	if(m == &runtime_m0 && runtime_sched.init) {
		runtime_sched.lockmain = false;
		return;
	}
	m->lockedg = nil;
	g->lockedm = nil;
}

bool
runtime_lockedOSThread(void)
{
	return g->lockedm != nil && m->lockedg != nil;
}

// for testing of callbacks

_Bool runtime_golockedOSThread(void)
  __asm__ (GOSYM_PREFIX "runtime.golockedOSThread");

_Bool
runtime_golockedOSThread(void)
{
	return runtime_lockedOSThread();
}

// for testing of wire, unwire
uint32
runtime_mid()
{
	return m->id;
}

intgo runtime_NumGoroutine (void)
  __asm__ (GOSYM_PREFIX "runtime.NumGoroutine");

intgo
runtime_NumGoroutine()
{
	return runtime_sched.gcount;
}

int32
runtime_gcount(void)
{
	return runtime_sched.gcount;
}

int32
runtime_mcount(void)
{
	return runtime_sched.mcount;
}

static struct {
	Lock;
	void (*fn)(uintptr*, int32);
	int32 hz;
	uintptr pcbuf[100];
	Location locbuf[100];
} prof;

// Called if we receive a SIGPROF signal.
void
runtime_sigprof()
{
	int32 n, i;

	if(prof.fn == nil || prof.hz == 0)
		return;

	runtime_lock(&prof);
	if(prof.fn == nil) {
		runtime_unlock(&prof);
		return;
	}
	n = runtime_callers(0, prof.locbuf, nelem(prof.locbuf));
	for(i = 0; i < n; i++)
		prof.pcbuf[i] = prof.locbuf[i].pc;
	if(n > 0)
		prof.fn(prof.pcbuf, n);
	runtime_unlock(&prof);
}

// Arrange to call fn with a traceback hz times a second.
void
runtime_setcpuprofilerate(void (*fn)(uintptr*, int32), int32 hz)
{
	// Force sane arguments.
	if(hz < 0)
		hz = 0;
	if(hz == 0)
		fn = nil;
	if(fn == nil)
		hz = 0;

	// Stop profiler on this cpu so that it is safe to lock prof.
	// if a profiling signal came in while we had prof locked,
	// it would deadlock.
	runtime_resetcpuprofiler(0);

	runtime_lock(&prof);
	prof.fn = fn;
	prof.hz = hz;
	runtime_unlock(&prof);
	runtime_lock(&runtime_sched);
	runtime_sched.profilehz = hz;
	runtime_unlock(&runtime_sched);

	if(hz != 0)
		runtime_resetcpuprofiler(hz);
}
