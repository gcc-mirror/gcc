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

static void gtraceback(G*);

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

// Set m and g.
void
runtime_setmg(M* mp, G* gp)
{
	m = mp;
	g = gp;
}

// The static TLS size.  See runtime_newm.
static int tlssize;

// Start a new thread.
static void
runtime_newosproc(M *mp)
{
	pthread_attr_t attr;
	size_t stacksize;
	sigset_t clear, old;
	pthread_t tid;
	int ret;

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

// Goroutine scheduler
// The scheduler's job is to distribute ready-to-run goroutines over worker threads.
//
// The main concepts are:
// G - goroutine.
// M - worker thread, or machine.
// P - processor, a resource that is required to execute Go code.
//     M must have an associated P to execute Go code, however it can be
//     blocked or in a syscall w/o an associated P.
//
// Design doc at http://golang.org/s/go11sched.

typedef struct Sched Sched;
struct Sched {
	Lock;

	uint64	goidgen;
	M*	midle;	 // idle m's waiting for work
	int32	nmidle;	 // number of idle m's waiting for work
	int32	mlocked; // number of locked m's waiting for work
	int32	mcount;	 // number of m's that have been created

	P*	pidle;  // idle P's
	uint32	npidle;
	uint32	nmspinning;

	// Global runnable queue.
	G*	runqhead;
	G*	runqtail;
	int32	runqsize;

	// Global cache of dead G's.
	Lock	gflock;
	G*	gfree;

	int32	stopwait;
	Note	stopnote;
	uint32	sysmonwait;
	Note	sysmonnote;
	uint64	lastpoll;

	int32	profilehz;	// cpu profiling rate
};

// The max value of GOMAXPROCS.
// There are no fundamental restrictions on the value.
enum { MaxGomaxprocs = 1<<8 };

Sched	runtime_sched;
int32	runtime_gomaxprocs;
bool	runtime_singleproc;
bool	runtime_iscgo;
uint32	runtime_gcwaiting;
M	runtime_m0;
G	runtime_g0;	 // idle goroutine for m0
G*	runtime_allg;
G*	runtime_lastg;
M*	runtime_allm;
P**	runtime_allp;
M*	runtime_extram;
int8*	runtime_goos;
int32	runtime_ncpu;
static int32	newprocs;

void* runtime_mstart(void*);
static void runqput(P*, G*);
static G* runqget(P*);
static void runqgrow(P*);
static G* runqsteal(P*, P*);
static void mput(M*);
static M* mget(void);
static void mcommoninit(M*);
static void schedule(void);
static void procresize(int32);
static void acquirep(P*);
static P* releasep(void);
static void newm(void(*)(void), P*);
static void stopm(void);
static void startm(P*, bool);
static void handoffp(P*);
static void wakep(void);
static void stoplockedm(void);
static void startlockedm(G*);
static void sysmon(void);
static uint32 retake(uint32*);
static void inclocked(int32);
static void checkdead(void);
static void exitsyscall0(G*);
static void park0(G*);
static void gosched0(G*);
static void goexit0(G*);
static void gfput(P*, G*);
static G* gfget(P*);
static void gfpurge(P*);
static void globrunqput(G*);
static G* globrunqget(P*);
static P* pidleget(void);
static void pidleput(P*);
static void injectglist(G*);

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
	int32 n, procs;
	const byte *p;

	m = &runtime_m0;
	g = &runtime_g0;
	m->g0 = g;
	m->curg = g;
	g->m = m;

	initcontext();
	inittlssize();

	m->nomemprof++;
	runtime_mprofinit();
	runtime_mallocinit();
	mcommoninit(m);

	runtime_goargs();
	runtime_goenvs();

	// For debugging:
	// Allocate internal symbol table representation now,
	// so that we don't need to call malloc when we crash.
	// runtime_findfunc(0);

	runtime_sched.lastpoll = runtime_nanotime();
	procs = 1;
	p = runtime_getenv("GOMAXPROCS");
	if(p != nil && (n = runtime_atoi(p)) > 0) {
		if(n > MaxGomaxprocs)
			n = MaxGomaxprocs;
		procs = n;
	}
	runtime_allp = runtime_malloc((MaxGomaxprocs+1)*sizeof(runtime_allp[0]));
	procresize(procs);

	// Can not enable GC until all roots are registered.
	// mstats.enablegc = 1;
	m->nomemprof--;
}

extern void main_init(void) __asm__ (GOSYM_PREFIX "__go_init_main");
extern void main_main(void) __asm__ (GOSYM_PREFIX "main.main");

// The main goroutine.
void
runtime_main(void* dummy __attribute__((unused)))
{
	newm(sysmon, nil);

	// Lock the main goroutine onto this, the main OS thread,
	// during initialization.  Most programs won't care, but a few
	// do require certain calls to be made by the main thread.
	// Those can arrange for main.main to run in the main thread
	// by calling runtime.LockOSThread during initialization
	// to preserve the lock.
	runtime_lockOSThread();
	if(m != &runtime_m0)
		runtime_throw("runtime_main not on m0");
	__go_go(runtime_MHeap_Scavenger, nil);
	main_init();
	runtime_unlockOSThread();

	// For gccgo we have to wait until after main is initialized
	// to enable GC, because initializing main registers the GC
	// roots.
	mstats.enablegc = 1;

	main_main();
	if(raceenabled)
		runtime_racefini();

	// Make racy client program work: if panicking on
	// another goroutine at the same time as main returns,
	// let the other goroutine finish printing the panic trace.
	// Once it does, it will exit. See issue 3934.
	if(runtime_panicking)
		runtime_park(nil, nil, "panicwait");

	runtime_exit(0);
	for(;;)
		*(int32*)0 = 0;
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
	traceback = runtime_gotraceback(nil);
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

static void
mcommoninit(M *mp)
{
	// If there is no mcache runtime_callers() will crash,
	// and we are most likely in sysmon thread so the stack is senseless anyway.
	if(m->mcache)
		runtime_callers(1, mp->createstack, nelem(mp->createstack));

	mp->fastrand = 0x49f6428aUL + mp->id + runtime_cputicks();

	runtime_lock(&runtime_sched);
	mp->id = runtime_sched.mcount++;

	runtime_mpreinit(mp);

	// Add to runtime_allm so garbage collector doesn't free m
	// when it is just in a register or thread-local storage.
	mp->alllink = runtime_allm;
	// runtime_NumCgoCall() iterates over allm w/o schedlock,
	// so we need to publish it safely.
	runtime_atomicstorep(&runtime_allm, mp);
	runtime_unlock(&runtime_sched);
}

// Mark gp ready to run.
void
runtime_ready(G *gp)
{
	// Mark runnable.
	if(gp->status != Gwaiting) {
		runtime_printf("goroutine %D has status %d\n", gp->goid, gp->status);
		runtime_throw("bad g->status in ready");
	}
	gp->status = Grunnable;
	runqput(m->p, gp);
	if(runtime_atomicload(&runtime_sched.npidle) != 0 && runtime_atomicload(&runtime_sched.nmspinning) == 0)  // TODO: fast atomic
		wakep();
}

int32
runtime_gcprocs(void)
{
	int32 n;

	// Figure out how many CPUs to use during GC.
	// Limited by gomaxprocs, number of actual CPUs, and MaxGcproc.
	runtime_lock(&runtime_sched);
	n = runtime_gomaxprocs;
	if(n > runtime_ncpu)
		n = runtime_ncpu > 0 ? runtime_ncpu : 1;
	if(n > MaxGcproc)
		n = MaxGcproc;
	if(n > runtime_sched.nmidle+1) // one M is currently running
		n = runtime_sched.nmidle+1;
	runtime_unlock(&runtime_sched);
	return n;
}

static bool
needaddgcproc(void)
{
	int32 n;

	runtime_lock(&runtime_sched);
	n = runtime_gomaxprocs;
	if(n > runtime_ncpu)
		n = runtime_ncpu;
	if(n > MaxGcproc)
		n = MaxGcproc;
	n -= runtime_sched.nmidle+1; // one M is currently running
	runtime_unlock(&runtime_sched);
	return n > 0;
}

void
runtime_helpgc(int32 nproc)
{
	M *mp;
	int32 n, pos;

	runtime_lock(&runtime_sched);
	pos = 0;
	for(n = 1; n < nproc; n++) {  // one M is currently running
		if(runtime_allp[pos]->mcache == m->mcache)
			pos++;
		mp = mget();
		if(mp == nil)
			runtime_throw("runtime_gcprocs inconsistency");
		mp->helpgc = n;
		mp->mcache = runtime_allp[pos]->mcache;
		pos++;
		runtime_notewakeup(&mp->park);
	}
	runtime_unlock(&runtime_sched);
}

void
runtime_stoptheworld(void)
{
	int32 i;
	uint32 s;
	P *p;
	bool wait;

	runtime_lock(&runtime_sched);
	runtime_sched.stopwait = runtime_gomaxprocs;
	runtime_atomicstore((uint32*)&runtime_gcwaiting, 1);
	// stop current P
	m->p->status = Pgcstop;
	runtime_sched.stopwait--;
	// try to retake all P's in Psyscall status
	for(i = 0; i < runtime_gomaxprocs; i++) {
		p = runtime_allp[i];
		s = p->status;
		if(s == Psyscall && runtime_cas(&p->status, s, Pgcstop))
			runtime_sched.stopwait--;
	}
	// stop idle P's
	while((p = pidleget()) != nil) {
		p->status = Pgcstop;
		runtime_sched.stopwait--;
	}
	wait = runtime_sched.stopwait > 0;
	runtime_unlock(&runtime_sched);

	// wait for remaining P's to stop voluntary
	if(wait) {
		runtime_notesleep(&runtime_sched.stopnote);
		runtime_noteclear(&runtime_sched.stopnote);
	}
	if(runtime_sched.stopwait)
		runtime_throw("stoptheworld: not stopped");
	for(i = 0; i < runtime_gomaxprocs; i++) {
		p = runtime_allp[i];
		if(p->status != Pgcstop)
			runtime_throw("stoptheworld: not stopped");
	}
}

static void
mhelpgc(void)
{
	m->helpgc = -1;
}

void
runtime_starttheworld(void)
{
	P *p, *p1;
	M *mp;
	G *gp;
	bool add;

	gp = runtime_netpoll(false);  // non-blocking
	injectglist(gp);
	add = needaddgcproc();
	runtime_lock(&runtime_sched);
	if(newprocs) {
		procresize(newprocs);
		newprocs = 0;
	} else
		procresize(runtime_gomaxprocs);
	runtime_gcwaiting = 0;

	p1 = nil;
	while((p = pidleget()) != nil) {
		// procresize() puts p's with work at the beginning of the list.
		// Once we reach a p without a run queue, the rest don't have one either.
		if(p->runqhead == p->runqtail) {
			pidleput(p);
			break;
		}
		mp = mget();
		if(mp == nil) {
			p->link = p1;
			p1 = p;
			continue;
		}
		if(mp->nextp)
			runtime_throw("starttheworld: inconsistent mp->nextp");
		mp->nextp = p;
		runtime_notewakeup(&mp->park);
	}
	if(runtime_sched.sysmonwait) {
		runtime_sched.sysmonwait = false;
		runtime_notewakeup(&runtime_sched.sysmonnote);
	}
	runtime_unlock(&runtime_sched);

	while(p1) {
		p = p1;
		p1 = p1->link;
		add = false;
		newm(nil, p);
	}

	if(add) {
		// If GC could have used another helper proc, start one now,
		// in the hope that it will be available next time.
		// It would have been even better to start it before the collection,
		// but doing so requires allocating memory, so it's tricky to
		// coordinate.  This lazy approach works out in practice:
		// we don't mind if the first couple gc rounds don't have quite
		// the maximum number of procs.
		newm(mhelpgc, nil);
	}
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
	if(m == &runtime_m0) {
		runtime_initsig();
		if(runtime_iscgo)
			runtime_newextram();
	}
	
	if(m->mstartfn)
		m->mstartfn();

	if(m->helpgc) {
		m->helpgc = 0;
		stopm();
	} else if(m != &runtime_m0) {
		acquirep(m->nextp);
		m->nextp = nil;
	}
	schedule();

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

// Allocate a new m unassociated with any thread.
// Can use p for allocation context if needed.
M*
runtime_allocm(P *p)
{
	M *mp;

	m->locks++;  // disable GC because it can be called from sysmon
	if(m->p == nil)
		acquirep(p);  // temporarily borrow p for mallocs in this function
#if 0
	if(mtype == nil) {
		Eface e;
		runtime_gc_m_ptr(&e);
		mtype = ((const PtrType*)e.__type_descriptor)->__element_type;
	}
#endif

	mp = runtime_mal(sizeof *mp);
	mcommoninit(mp);
	mp->g0 = runtime_malg(-1, nil, nil);

	if(p == m->p)
		releasep();
	m->locks--;

	return mp;
}

static M* lockextra(bool nilokay);
static void unlockextra(M*);

// needm is called when a cgo callback happens on a
// thread without an m (a thread not created by Go).
// In this case, needm is expected to find an m to use
// and return with m, g initialized correctly.
// Since m and g are not set now (likely nil, but see below)
// needm is limited in what routines it can call. In particular
// it can only call nosplit functions (textflag 7) and cannot
// do any scheduling that requires an m.
//
// In order to avoid needing heavy lifting here, we adopt
// the following strategy: there is a stack of available m's
// that can be stolen. Using compare-and-swap
// to pop from the stack has ABA races, so we simulate
// a lock by doing an exchange (via casp) to steal the stack
// head and replace the top pointer with MLOCKED (1).
// This serves as a simple spin lock that we can use even
// without an m. The thread that locks the stack in this way
// unlocks the stack by storing a valid stack head pointer.
//
// In order to make sure that there is always an m structure
// available to be stolen, we maintain the invariant that there
// is always one more than needed. At the beginning of the
// program (if cgo is in use) the list is seeded with a single m.
// If needm finds that it has taken the last m off the list, its job
// is - once it has installed its own m so that it can do things like
// allocate memory - to create a spare m and put it on the list.
//
// Each of these extra m's also has a g0 and a curg that are
// pressed into service as the scheduling stack and current
// goroutine for the duration of the cgo callback.
//
// When the callback is done with the m, it calls dropm to
// put the m back on the list.
void
runtime_needm(void)
{
	M *mp;

	// Lock extra list, take head, unlock popped list.
	// nilokay=false is safe here because of the invariant above,
	// that the extra list always contains or will soon contain
	// at least one m.
	mp = lockextra(false);

	// Set needextram when we've just emptied the list,
	// so that the eventual call into cgocallbackg will
	// allocate a new m for the extra list. We delay the
	// allocation until then so that it can be done
	// after exitsyscall makes sure it is okay to be
	// running at all (that is, there's no garbage collection
	// running right now).
	mp->needextram = mp->schedlink == nil;
	unlockextra(mp->schedlink);

	// Install m and g (= m->g0) and set the stack bounds
	// to match the current stack. We don't actually know
	// how big the stack is, like we don't know how big any
	// scheduling stack is, but we assume there's at least 32 kB,
	// which is more than enough for us.
	runtime_setmg(mp, mp->g0);

	// We assume that the split stack support has been initialized
	// for this new thread.

	// Initialize this thread to use the m.
	runtime_minit();
}

// newextram allocates an m and puts it on the extra list.
// It is called with a working local m, so that it can do things
// like call schedlock and allocate.
void
runtime_newextram(void)
{
	M *mp, *mnext;
	G *gp;

	// Create extra goroutine locked to extra m.
	// The goroutine is the context in which the cgo callback will run.
	// The sched.pc will never be returned to, but setting it to
	// runtime.goexit makes clear to the traceback routines where
	// the goroutine stack ends.
	mp = runtime_allocm(nil);
	gp = runtime_malg(StackMin, nil, nil);
	gp->status = Gsyscall;
	mp->curg = gp;
	mp->locked = LockInternal;
	mp->lockedg = gp;
	gp->lockedm = mp;
	// put on allg for garbage collector
	runtime_lock(&runtime_sched);
	if(runtime_lastg == nil)
		runtime_allg = gp;
	else
		runtime_lastg->alllink = gp;
	runtime_lastg = gp;
	runtime_unlock(&runtime_sched);
	gp->goid = runtime_xadd64(&runtime_sched.goidgen, 1);

	// Add m to the extra list.
	mnext = lockextra(true);
	mp->schedlink = mnext;
	unlockextra(mp);
}

// dropm is called when a cgo callback has called needm but is now
// done with the callback and returning back into the non-Go thread.
// It puts the current m back onto the extra list.
//
// The main expense here is the call to signalstack to release the
// m's signal stack, and then the call to needm on the next callback
// from this thread. It is tempting to try to save the m for next time,
// which would eliminate both these costs, but there might not be
// a next time: the current thread (which Go does not control) might exit.
// If we saved the m for that thread, there would be an m leak each time
// such a thread exited. Instead, we acquire and release an m on each
// call. These should typically not be scheduling operations, just a few
// atomics, so the cost should be small.
//
// TODO(rsc): An alternative would be to allocate a dummy pthread per-thread
// variable using pthread_key_create. Unlike the pthread keys we already use
// on OS X, this dummy key would never be read by Go code. It would exist
// only so that we could register at thread-exit-time destructor.
// That destructor would put the m back onto the extra list.
// This is purely a performance optimization. The current version,
// in which dropm happens on each cgo call, is still correct too.
// We may have to keep the current version on systems with cgo
// but without pthreads, like Windows.
void
runtime_dropm(void)
{
	M *mp, *mnext;

	// Undo whatever initialization minit did during needm.
	runtime_unminit();

	// Clear m and g, and return m to the extra list.
	// After the call to setmg we can only call nosplit functions.
	mp = m;
	runtime_setmg(nil, nil);

	mnext = lockextra(true);
	mp->schedlink = mnext;
	unlockextra(mp);
}

#define MLOCKED ((M*)1)

// lockextra locks the extra list and returns the list head.
// The caller must unlock the list by storing a new list head
// to runtime.extram. If nilokay is true, then lockextra will
// return a nil list head if that's what it finds. If nilokay is false,
// lockextra will keep waiting until the list head is no longer nil.
static M*
lockextra(bool nilokay)
{
	M *mp;
	void (*yield)(void);

	for(;;) {
		mp = runtime_atomicloadp(&runtime_extram);
		if(mp == MLOCKED) {
			yield = runtime_osyield;
			yield();
			continue;
		}
		if(mp == nil && !nilokay) {
			runtime_usleep(1);
			continue;
		}
		if(!runtime_casp(&runtime_extram, mp, MLOCKED)) {
			yield = runtime_osyield;
			yield();
			continue;
		}
		break;
	}
	return mp;
}

static void
unlockextra(M *mp)
{
	runtime_atomicstorep(&runtime_extram, mp);
}


// Create a new m.  It will start off with a call to fn, or else the scheduler.
static void
newm(void(*fn)(void), P *p)
{
	M *mp;

	mp = runtime_allocm(p);
	mp->nextp = p;
	mp->mstartfn = fn;

	runtime_newosproc(mp);
}

// Stops execution of the current m until new work is available.
// Returns with acquired P.
static void
stopm(void)
{
	if(m->locks)
		runtime_throw("stopm holding locks");
	if(m->p)
		runtime_throw("stopm holding p");
	if(m->spinning) {
		m->spinning = false;
		runtime_xadd(&runtime_sched.nmspinning, -1);
	}

retry:
	runtime_lock(&runtime_sched);
	mput(m);
	runtime_unlock(&runtime_sched);
	runtime_notesleep(&m->park);
	runtime_noteclear(&m->park);
	if(m->helpgc) {
		runtime_gchelper();
		m->helpgc = 0;
		m->mcache = nil;
		goto retry;
	}
	acquirep(m->nextp);
	m->nextp = nil;
}

static void
mspinning(void)
{
	m->spinning = true;
}

// Schedules some M to run the p (creates an M if necessary).
// If p==nil, tries to get an idle P, if no idle P's returns false.
static void
startm(P *p, bool spinning)
{
	M *mp;
	void (*fn)(void);

	runtime_lock(&runtime_sched);
	if(p == nil) {
		p = pidleget();
		if(p == nil) {
			runtime_unlock(&runtime_sched);
			if(spinning)
				runtime_xadd(&runtime_sched.nmspinning, -1);
			return;
		}
	}
	mp = mget();
	runtime_unlock(&runtime_sched);
	if(mp == nil) {
		fn = nil;
		if(spinning)
			fn = mspinning;
		newm(fn, p);
		return;
	}
	if(mp->spinning)
		runtime_throw("startm: m is spinning");
	if(mp->nextp)
		runtime_throw("startm: m has p");
	mp->spinning = spinning;
	mp->nextp = p;
	runtime_notewakeup(&mp->park);
}

// Hands off P from syscall or locked M.
static void
handoffp(P *p)
{
	// if it has local work, start it straight away
	if(p->runqhead != p->runqtail || runtime_sched.runqsize) {
		startm(p, false);
		return;
	}
	// no local work, check that there are no spinning/idle M's,
	// otherwise our help is not required
	if(runtime_atomicload(&runtime_sched.nmspinning) + runtime_atomicload(&runtime_sched.npidle) == 0 &&  // TODO: fast atomic
		runtime_cas(&runtime_sched.nmspinning, 0, 1)) {
		startm(p, true);
		return;
	}
	runtime_lock(&runtime_sched);
	if(runtime_gcwaiting) {
		p->status = Pgcstop;
		if(--runtime_sched.stopwait == 0)
			runtime_notewakeup(&runtime_sched.stopnote);
		runtime_unlock(&runtime_sched);
		return;
	}
	if(runtime_sched.runqsize) {
		runtime_unlock(&runtime_sched);
		startm(p, false);
		return;
	}
	// If this is the last running P and nobody is polling network,
	// need to wakeup another M to poll network.
	if(runtime_sched.npidle == (uint32)runtime_gomaxprocs-1 && runtime_atomicload64(&runtime_sched.lastpoll) != 0) {
		runtime_unlock(&runtime_sched);
		startm(p, false);
		return;
	}
	pidleput(p);
	runtime_unlock(&runtime_sched);
}

// Tries to add one more P to execute G's.
// Called when a G is made runnable (newproc, ready).
static void
wakep(void)
{
	// be conservative about spinning threads
	if(!runtime_cas(&runtime_sched.nmspinning, 0, 1))
		return;
	startm(nil, true);
}

// Stops execution of the current m that is locked to a g until the g is runnable again.
// Returns with acquired P.
static void
stoplockedm(void)
{
	P *p;

	if(m->lockedg == nil || m->lockedg->lockedm != m)
		runtime_throw("stoplockedm: inconsistent locking");
	if(m->p) {
		// Schedule another M to run this p.
		p = releasep();
		handoffp(p);
	}
	inclocked(1);
	// Wait until another thread schedules lockedg again.
	runtime_notesleep(&m->park);
	runtime_noteclear(&m->park);
	if(m->lockedg->status != Grunnable)
		runtime_throw("stoplockedm: not runnable");
	acquirep(m->nextp);
	m->nextp = nil;
}

// Schedules the locked m to run the locked gp.
static void
startlockedm(G *gp)
{
	M *mp;
	P *p;

	mp = gp->lockedm;
	if(mp == m)
		runtime_throw("startlockedm: locked to me");
	if(mp->nextp)
		runtime_throw("startlockedm: m has p");
	// directly handoff current P to the locked m
	inclocked(-1);
	p = releasep();
	mp->nextp = p;
	runtime_notewakeup(&mp->park);
	stopm();
}

// Stops the current m for stoptheworld.
// Returns when the world is restarted.
static void
gcstopm(void)
{
	P *p;

	if(!runtime_gcwaiting)
		runtime_throw("gcstopm: not waiting for gc");
	if(m->spinning) {
		m->spinning = false;
		runtime_xadd(&runtime_sched.nmspinning, -1);
	}
	p = releasep();
	runtime_lock(&runtime_sched);
	p->status = Pgcstop;
	if(--runtime_sched.stopwait == 0)
		runtime_notewakeup(&runtime_sched.stopnote);
	runtime_unlock(&runtime_sched);
	stopm();
}

// Schedules gp to run on the current M.
// Never returns.
static void
execute(G *gp)
{
	int32 hz;

	if(gp->status != Grunnable) {
		runtime_printf("execute: bad g status %d\n", gp->status);
		runtime_throw("execute: bad g status");
	}
	gp->status = Grunning;
	m->p->tick++;
	m->curg = gp;
	gp->m = m;

	// Check whether the profiler needs to be turned on or off.
	hz = runtime_sched.profilehz;
	if(m->profilehz != hz)
		runtime_resetcpuprofiler(hz);

	runtime_gogo(gp);
}

// Finds a runnable goroutine to execute.
// Tries to steal from other P's, get g from global queue, poll network.
static G*
findrunnable(void)
{
	G *gp;
	P *p;
	int32 i;

top:
	if(runtime_gcwaiting) {
		gcstopm();
		goto top;
	}
	// local runq
	gp = runqget(m->p);
	if(gp)
		return gp;
	// global runq
	if(runtime_sched.runqsize) {
		runtime_lock(&runtime_sched);
		gp = globrunqget(m->p);
		runtime_unlock(&runtime_sched);
		if(gp)
			return gp;
	}
	// poll network
	gp = runtime_netpoll(false);  // non-blocking
	if(gp) {
		injectglist(gp->schedlink);
		gp->status = Grunnable;
		return gp;
	}
	// If number of spinning M's >= number of busy P's, block.
	// This is necessary to prevent excessive CPU consumption
	// when GOMAXPROCS>>1 but the program parallelism is low.
	if(!m->spinning && 2 * runtime_atomicload(&runtime_sched.nmspinning) >= runtime_gomaxprocs - runtime_atomicload(&runtime_sched.npidle))  // TODO: fast atomic
		goto stop;
	if(!m->spinning) {
		m->spinning = true;
		runtime_xadd(&runtime_sched.nmspinning, 1);
	}
	// random steal from other P's
	for(i = 0; i < 2*runtime_gomaxprocs; i++) {
		if(runtime_gcwaiting)
			goto top;
		p = runtime_allp[runtime_fastrand1()%runtime_gomaxprocs];
		if(p == m->p)
			gp = runqget(p);
		else
			gp = runqsteal(m->p, p);
		if(gp)
			return gp;
	}
stop:
	// return P and block
	runtime_lock(&runtime_sched);
	if(runtime_gcwaiting) {
		runtime_unlock(&runtime_sched);
		goto top;
	}
	if(runtime_sched.runqsize) {
		gp = globrunqget(m->p);
		runtime_unlock(&runtime_sched);
		return gp;
	}
	p = releasep();
	pidleput(p);
	runtime_unlock(&runtime_sched);
	if(m->spinning) {
		m->spinning = false;
		runtime_xadd(&runtime_sched.nmspinning, -1);
	}
	// check all runqueues once again
	for(i = 0; i < runtime_gomaxprocs; i++) {
		p = runtime_allp[i];
		if(p && p->runqhead != p->runqtail) {
			runtime_lock(&runtime_sched);
			p = pidleget();
			runtime_unlock(&runtime_sched);
			if(p) {
				acquirep(p);
				goto top;
			}
			break;
		}
	}
	// poll network
	if(runtime_xchg64(&runtime_sched.lastpoll, 0) != 0) {
		if(m->p)
			runtime_throw("findrunnable: netpoll with p");
		if(m->spinning)
			runtime_throw("findrunnable: netpoll with spinning");
		gp = runtime_netpoll(true);  // block until new work is available
		runtime_atomicstore64(&runtime_sched.lastpoll, runtime_nanotime());
		if(gp) {
			runtime_lock(&runtime_sched);
			p = pidleget();
			runtime_unlock(&runtime_sched);
			if(p) {
				acquirep(p);
				injectglist(gp->schedlink);
				gp->status = Grunnable;
				return gp;
			}
			injectglist(gp);
		}
	}
	stopm();
	goto top;
}

// Injects the list of runnable G's into the scheduler.
// Can run concurrently with GC.
static void
injectglist(G *glist)
{
	int32 n;
	G *gp;

	if(glist == nil)
		return;
	runtime_lock(&runtime_sched);
	for(n = 0; glist; n++) {
		gp = glist;
		glist = gp->schedlink;
		gp->status = Grunnable;
		globrunqput(gp);
	}
	runtime_unlock(&runtime_sched);

	for(; n && runtime_sched.npidle; n--)
		startm(nil, false);
}

// One round of scheduler: find a runnable goroutine and execute it.
// Never returns.
static void
schedule(void)
{
	G *gp;

	if(m->locks)
		runtime_throw("schedule: holding locks");

top:
	if(runtime_gcwaiting) {
		gcstopm();
		goto top;
	}

	gp = runqget(m->p);
	if(gp == nil)
		gp = findrunnable();

	if(m->spinning) {
		m->spinning = false;
		runtime_xadd(&runtime_sched.nmspinning, -1);
	}

	// M wakeup policy is deliberately somewhat conservative (see nmspinning handling),
	// so see if we need to wakeup another M here.
	if (m->p->runqhead != m->p->runqtail &&
		runtime_atomicload(&runtime_sched.nmspinning) == 0 &&
		runtime_atomicload(&runtime_sched.npidle) > 0)  // TODO: fast atomic
		wakep();

	if(gp->lockedm) {
		startlockedm(gp);
		goto top;
	}

	execute(gp);
}

// Puts the current goroutine into a waiting state and unlocks the lock.
// The goroutine can be made runnable again by calling runtime_ready(gp).
void
runtime_park(void(*unlockf)(Lock*), Lock *lock, const char *reason)
{
	m->waitlock = lock;
	m->waitunlockf = unlockf;
	g->waitreason = reason;
	runtime_mcall(park0);
}

// runtime_park continuation on g0.
static void
park0(G *gp)
{
	gp->status = Gwaiting;
	gp->m = nil;
	m->curg = nil;
	if(m->waitunlockf) {
		m->waitunlockf(m->waitlock);
		m->waitunlockf = nil;
		m->waitlock = nil;
	}
	if(m->lockedg) {
		stoplockedm();
		execute(gp);  // Never returns.
	}
	schedule();
}

// Scheduler yield.
void
runtime_gosched(void)
{
	runtime_mcall(gosched0);
}

// runtime_gosched continuation on g0.
static void
gosched0(G *gp)
{
	gp->status = Grunnable;
	gp->m = nil;
	m->curg = nil;
	runtime_lock(&runtime_sched);
	globrunqput(gp);
	runtime_unlock(&runtime_sched);
	if(m->lockedg) {
		stoplockedm();
		execute(gp);  // Never returns.
	}
	schedule();
}

// Finishes execution of the current goroutine.
void
runtime_goexit(void)
{
	if(raceenabled)
		runtime_racegoend();
	runtime_mcall(goexit0);
}

// runtime_goexit continuation on g0.
static void
goexit0(G *gp)
{
	gp->status = Gdead;
	gp->entry = nil;
	gp->m = nil;
	gp->lockedm = nil;
	m->curg = nil;
	m->lockedg = nil;
	if(m->locked & ~LockExternal) {
		runtime_printf("invalid m->locked = %d", m->locked);
		runtime_throw("internal lockOSThread error");
	}	
	m->locked = 0;
	gfput(m->p, gp);
	schedule();
}

// The goroutine g is about to enter a system call.
// Record that it's not using the cpu anymore.
// This is called only from the go syscall library and cgocall,
// not from the low-level system calls used by the runtime.
//
// Entersyscall cannot split the stack: the runtime_gosave must
// make g->sched refer to the caller's stack segment, because
// entersyscall is going to return immediately after.

void runtime_entersyscall(void) __attribute__ ((no_split_stack));

void
runtime_entersyscall()
{
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

	if(runtime_atomicload(&runtime_sched.sysmonwait)) {  // TODO: fast atomic
		runtime_lock(&runtime_sched);
		if(runtime_atomicload(&runtime_sched.sysmonwait)) {
			runtime_atomicstore(&runtime_sched.sysmonwait, 0);
			runtime_notewakeup(&runtime_sched.sysmonnote);
		}
		runtime_unlock(&runtime_sched);
	}

	m->mcache = nil;
	m->p->tick++;
	m->p->m = nil;
	runtime_atomicstore(&m->p->status, Psyscall);
	if(runtime_gcwaiting) {
		runtime_lock(&runtime_sched);
		if (runtime_sched.stopwait > 0 && runtime_cas(&m->p->status, Psyscall, Pgcstop)) {
			if(--runtime_sched.stopwait == 0)
				runtime_notewakeup(&runtime_sched.stopnote);
		}
		runtime_unlock(&runtime_sched);
	}
}

// The same as runtime_entersyscall(), but with a hint that the syscall is blocking.
void
runtime_entersyscallblock(void)
{
	P *p;

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

	p = releasep();
	handoffp(p);
	if(g->isbackground)  // do not consider blocked scavenger for deadlock detection
		inclocked(1);
}

// The goroutine g exited its system call.
// Arrange for it to run on a cpu again.
// This is called only from the go syscall library, not
// from the low-level system calls used by the runtime.
void
runtime_exitsyscall(void)
{
	G *gp;
	P *p;

	// Check whether the profiler needs to be turned on.
	if(m->profilehz > 0)
		runtime_setprof(true);

	gp = g;
	// Try to re-acquire the last P.
	if(m->p && m->p->status == Psyscall && runtime_cas(&m->p->status, Psyscall, Prunning)) {
		// There's a cpu for us, so we can run.
		m->mcache = m->p->mcache;
		m->p->m = m;
		m->p->tick++;
		gp->status = Grunning;
		// Garbage collector isn't running (since we are),
		// so okay to clear gcstack and gcsp.
#ifdef USING_SPLIT_STACK
		gp->gcstack = nil;
#endif
		gp->gcnext_sp = nil;
		runtime_memclr(&gp->gcregs, sizeof gp->gcregs);
		return;
	}

	if(gp->isbackground)  // do not consider blocked scavenger for deadlock detection
		inclocked(-1);
	// Try to get any other idle P.
	m->p = nil;
	if(runtime_sched.pidle) {
		runtime_lock(&runtime_sched);
		p = pidleget();
		runtime_unlock(&runtime_sched);
		if(p) {
			acquirep(p);
#ifdef USING_SPLIT_STACK
			gp->gcstack = nil;
#endif
			gp->gcnext_sp = nil;
			runtime_memclr(&gp->gcregs, sizeof gp->gcregs);
			return;
		}
	}

	// Call the scheduler.
	runtime_mcall(exitsyscall0);

	// Scheduler returned, so we're allowed to run now.
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

// runtime_exitsyscall slow path on g0.
// Failed to acquire P, enqueue gp as runnable.
static void
exitsyscall0(G *gp)
{
	P *p;

	gp->status = Grunnable;
	gp->m = nil;
	m->curg = nil;
	runtime_lock(&runtime_sched);
	p = pidleget();
	if(p == nil)
		globrunqput(gp);
	runtime_unlock(&runtime_sched);
	if(p) {
		acquirep(p);
		execute(gp);  // Never returns.
	}
	if(m->lockedg) {
		// Wait until another thread schedules gp and so m again.
		stoplockedm();
		execute(gp);  // Never returns.
	}
	stopm();
	schedule();  // Never returns.
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

	m->locks++;  // disable preemption because it can be holding p in a local var

	if((newg = gfget(m->p)) != nil) {
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
		runtime_lock(&runtime_sched);
		if(runtime_lastg == nil)
			runtime_allg = newg;
		else
			runtime_lastg->alllink = newg;
		runtime_lastg = newg;
		runtime_unlock(&runtime_sched);
	}

	newg->entry = (byte*)fn;
	newg->param = arg;
	newg->gopc = (uintptr)__builtin_return_address(0);
	newg->status = Grunnable;
	newg->goid = runtime_xadd64(&runtime_sched.goidgen, 1);

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

		runqput(m->p, vnewg);

		if(runtime_atomicload(&runtime_sched.npidle) != 0 && runtime_atomicload(&runtime_sched.nmspinning) == 0 && fn != runtime_main)  // TODO: fast atomic
			wakep();
		m->locks--;
		return vnewg;
	}
}

// Put on gfree list.
// If local list is too long, transfer a batch to the global list.
static void
gfput(P *p, G *gp)
{
	gp->schedlink = p->gfree;
	p->gfree = gp;
	p->gfreecnt++;
	if(p->gfreecnt >= 64) {
		runtime_lock(&runtime_sched.gflock);
		while(p->gfreecnt >= 32) {
			p->gfreecnt--;
			gp = p->gfree;
			p->gfree = gp->schedlink;
			gp->schedlink = runtime_sched.gfree;
			runtime_sched.gfree = gp;
		}
		runtime_unlock(&runtime_sched.gflock);
	}
}

// Get from gfree list.
// If local list is empty, grab a batch from global list.
static G*
gfget(P *p)
{
	G *gp;

retry:
	gp = p->gfree;
	if(gp == nil && runtime_sched.gfree) {
		runtime_lock(&runtime_sched.gflock);
		while(p->gfreecnt < 32 && runtime_sched.gfree) {
			p->gfreecnt++;
			gp = runtime_sched.gfree;
			runtime_sched.gfree = gp->schedlink;
			gp->schedlink = p->gfree;
			p->gfree = gp;
		}
		runtime_unlock(&runtime_sched.gflock);
		goto retry;
	}
	if(gp) {
		p->gfree = gp->schedlink;
		p->gfreecnt--;
	}
	return gp;
}

// Purge all cached G's from gfree list to the global list.
static void
gfpurge(P *p)
{
	G *gp;

	runtime_lock(&runtime_sched.gflock);
	while(p->gfreecnt) {
		p->gfreecnt--;
		gp = p->gfree;
		p->gfree = gp->schedlink;
		gp->schedlink = runtime_sched.gfree;
		runtime_sched.gfree = gp;
	}
	runtime_unlock(&runtime_sched.gflock);
}

void
runtime_Breakpoint(void)
{
	runtime_breakpoint();
}

void runtime_Gosched (void) __asm__ (GOSYM_PREFIX "runtime.Gosched");

void
runtime_Gosched(void)
{
	runtime_gosched();
}

// Implementation of runtime.GOMAXPROCS.
// delete when scheduler is even stronger
int32
runtime_gomaxprocsfunc(int32 n)
{
	int32 ret;

	if(n > MaxGomaxprocs)
		n = MaxGomaxprocs;
	runtime_lock(&runtime_sched);
	ret = runtime_gomaxprocs;
	if(n <= 0 || n == ret) {
		runtime_unlock(&runtime_sched);
		return ret;
	}
	runtime_unlock(&runtime_sched);

	runtime_semacquire(&runtime_worldsema);
	m->gcing = 1;
	runtime_stoptheworld();
	newprocs = n;
	m->gcing = 0;
	runtime_semrelease(&runtime_worldsema);
	runtime_starttheworld();

	return ret;
}

static void
LockOSThread(void)
{
	m->lockedg = g;
	g->lockedm = m;
}

void	runtime_LockOSThread(void) __asm__ (GOSYM_PREFIX "runtime.LockOSThread");
void
runtime_LockOSThread(void)
{
	m->locked |= LockExternal;
	LockOSThread();
}

void
runtime_lockOSThread(void)
{
	m->locked += LockInternal;
	LockOSThread();
}

static void
UnlockOSThread(void)
{
	if(m->locked != 0)
		return;
	m->lockedg = nil;
	g->lockedm = nil;
}

void	runtime_UnlockOSThread(void) __asm__ (GOSYM_PREFIX "runtime.UnlockOSThread");

void
runtime_UnlockOSThread(void)
{
	m->locked &= ~LockExternal;
	UnlockOSThread();
}

void
runtime_unlockOSThread(void)
{
	if(m->locked < LockInternal)
		runtime_throw("runtime: internal error: misuse of lockOSThread/unlockOSThread");
	m->locked -= LockInternal;
	UnlockOSThread();
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
	return runtime_gcount();
}

int32
runtime_gcount(void)
{
	G *gp;
	int32 n, s;

	n = 0;
	runtime_lock(&runtime_sched);
	// TODO(dvyukov): runtime.NumGoroutine() is O(N).
	// We do not want to increment/decrement centralized counter in newproc/goexit,
	// just to make runtime.NumGoroutine() faster.
	// Compromise solution is to introduce per-P counters of active goroutines.
	for(gp = runtime_allg; gp; gp = gp->alllink) {
		s = gp->status;
		if(s == Grunnable || s == Grunning || s == Gsyscall || s == Gwaiting)
			n++;
	}
	runtime_unlock(&runtime_sched);
	return n;
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

	// Windows does profiling in a dedicated thread w/o m.
	if(!Windows && (m == nil || m->mcache == nil))
		return;
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

// Change number of processors.  The world is stopped, sched is locked.
static void
procresize(int32 new)
{
	int32 i, old;
	G *gp;
	P *p;

	old = runtime_gomaxprocs;
	if(old < 0 || old > MaxGomaxprocs || new <= 0 || new >MaxGomaxprocs)
		runtime_throw("procresize: invalid arg");
	// initialize new P's
	for(i = 0; i < new; i++) {
		p = runtime_allp[i];
		if(p == nil) {
			p = (P*)runtime_mallocgc(sizeof(*p), 0, 0, 1);
			p->status = Pgcstop;
			runtime_atomicstorep(&runtime_allp[i], p);
		}
		if(p->mcache == nil) {
			if(old==0 && i==0)
				p->mcache = m->mcache;  // bootstrap
			else
				p->mcache = runtime_allocmcache();
		}
		if(p->runq == nil) {
			p->runqsize = 128;
			p->runq = (G**)runtime_mallocgc(p->runqsize*sizeof(G*), 0, 0, 1);
		}
	}

	// redistribute runnable G's evenly
	for(i = 0; i < old; i++) {
		p = runtime_allp[i];
		while((gp = runqget(p)) != nil)
			globrunqput(gp);
	}
	// start at 1 because current M already executes some G and will acquire allp[0] below,
	// so if we have a spare G we want to put it into allp[1].
	for(i = 1; runtime_sched.runqhead; i++) {
		gp = runtime_sched.runqhead;
		runtime_sched.runqhead = gp->schedlink;
		runqput(runtime_allp[i%new], gp);
	}
	runtime_sched.runqtail = nil;
	runtime_sched.runqsize = 0;

	// free unused P's
	for(i = new; i < old; i++) {
		p = runtime_allp[i];
		runtime_freemcache(p->mcache);
		p->mcache = nil;
		gfpurge(p);
		p->status = Pdead;
		// can't free P itself because it can be referenced by an M in syscall
	}

	if(m->p)
		m->p->m = nil;
	m->p = nil;
	m->mcache = nil;
	p = runtime_allp[0];
	p->m = nil;
	p->status = Pidle;
	acquirep(p);
	for(i = new-1; i > 0; i--) {
		p = runtime_allp[i];
		p->status = Pidle;
		pidleput(p);
	}
	runtime_singleproc = new == 1;
	runtime_atomicstore((uint32*)&runtime_gomaxprocs, new);
}

// Associate p and the current m.
static void
acquirep(P *p)
{
	if(m->p || m->mcache)
		runtime_throw("acquirep: already in go");
	if(p->m || p->status != Pidle) {
		runtime_printf("acquirep: p->m=%p(%d) p->status=%d\n", p->m, p->m ? p->m->id : 0, p->status);
		runtime_throw("acquirep: invalid p state");
	}
	m->mcache = p->mcache;
	m->p = p;
	p->m = m;
	p->status = Prunning;
}

// Disassociate p and the current m.
static P*
releasep(void)
{
	P *p;

	if(m->p == nil || m->mcache == nil)
		runtime_throw("releasep: invalid arg");
	p = m->p;
	if(p->m != m || p->mcache != m->mcache || p->status != Prunning) {
		runtime_printf("releasep: m=%p m->p=%p p->m=%p m->mcache=%p p->mcache=%p p->status=%d\n",
			m, m->p, p->m, m->mcache, p->mcache, p->status);
		runtime_throw("releasep: invalid p state");
	}
	m->p = nil;
	m->mcache = nil;
	p->m = nil;
	p->status = Pidle;
	return p;
}

static void
inclocked(int32 v)
{
	runtime_lock(&runtime_sched);
	runtime_sched.mlocked += v;
	if(v > 0)
		checkdead();
	runtime_unlock(&runtime_sched);
}

// Check for deadlock situation.
// The check is based on number of running M's, if 0 -> deadlock.
static void
checkdead(void)
{
	G *gp;
	int32 run, grunning, s;

	// -1 for sysmon
	run = runtime_sched.mcount - runtime_sched.nmidle - runtime_sched.mlocked - 1;
	if(run > 0)
		return;
	if(run < 0) {
		runtime_printf("checkdead: nmidle=%d mlocked=%d mcount=%d\n",
			runtime_sched.nmidle, runtime_sched.mlocked, runtime_sched.mcount);
		runtime_throw("checkdead: inconsistent counts");
	}
	grunning = 0;
	for(gp = runtime_allg; gp; gp = gp->alllink) {
		if(gp->isbackground)
			continue;
		s = gp->status;
		if(s == Gwaiting)
			grunning++;
		else if(s == Grunnable || s == Grunning || s == Gsyscall) {
			runtime_printf("checkdead: find g %D in status %d\n", gp->goid, s);
			runtime_throw("checkdead: runnable g");
		}
	}
	if(grunning == 0)  // possible if main goroutine calls runtime_Goexit()
		runtime_exit(0);
	m->throwing = -1;  // do not dump full stacks
	runtime_throw("all goroutines are asleep - deadlock!");
}

static void
sysmon(void)
{
	uint32 idle, delay;
	int64 now, lastpoll;
	G *gp;
	uint32 ticks[MaxGomaxprocs];

	idle = 0;  // how many cycles in succession we had not wokeup somebody
	delay = 0;
	for(;;) {
		if(idle == 0)  // start with 20us sleep...
			delay = 20;
		else if(idle > 50)  // start doubling the sleep after 1ms...
			delay *= 2;
		if(delay > 10*1000)  // up to 10ms
			delay = 10*1000;
		runtime_usleep(delay);
		if(runtime_gcwaiting || runtime_atomicload(&runtime_sched.npidle) == (uint32)runtime_gomaxprocs) {  // TODO: fast atomic
			runtime_lock(&runtime_sched);
			if(runtime_atomicload(&runtime_gcwaiting) || runtime_atomicload(&runtime_sched.npidle) == (uint32)runtime_gomaxprocs) {
				runtime_atomicstore(&runtime_sched.sysmonwait, 1);
				runtime_unlock(&runtime_sched);
				runtime_notesleep(&runtime_sched.sysmonnote);
				runtime_noteclear(&runtime_sched.sysmonnote);
				idle = 0;
				delay = 20;
			} else
				runtime_unlock(&runtime_sched);
		}
		// poll network if not polled for more than 10ms
		lastpoll = runtime_atomicload64(&runtime_sched.lastpoll);
		now = runtime_nanotime();
		if(lastpoll != 0 && lastpoll + 10*1000*1000 > now) {
			gp = runtime_netpoll(false);  // non-blocking
			injectglist(gp);
		}
		// retake P's blocked in syscalls
		if(retake(ticks))
			idle = 0;
		else
			idle++;
	}
}

static uint32
retake(uint32 *ticks)
{
	uint32 i, s, n;
	int64 t;
	P *p;

	n = 0;
	for(i = 0; i < (uint32)runtime_gomaxprocs; i++) {
		p = runtime_allp[i];
		if(p==nil)
			continue;
		t = p->tick;
		if(ticks[i] != t) {
			ticks[i] = t;
			continue;
		}
		s = p->status;
		if(s != Psyscall)
			continue;
		if(p->runqhead == p->runqtail && runtime_atomicload(&runtime_sched.nmspinning) + runtime_atomicload(&runtime_sched.npidle) > 0)  // TODO: fast atomic
			continue;
		// Need to increment number of locked M's before the CAS.
		// Otherwise the M from which we retake can exit the syscall,
		// increment nmidle and report deadlock.
		inclocked(-1);
		if(runtime_cas(&p->status, s, Pidle)) {
			n++;
			handoffp(p);
		}
		inclocked(1);
	}
	return n;
}

// Put mp on midle list.
// Sched must be locked.
static void
mput(M *mp)
{
	mp->schedlink = runtime_sched.midle;
	runtime_sched.midle = mp;
	runtime_sched.nmidle++;
	checkdead();
}

// Try to get an m from midle list.
// Sched must be locked.
static M*
mget(void)
{
	M *mp;

	if((mp = runtime_sched.midle) != nil){
		runtime_sched.midle = mp->schedlink;
		runtime_sched.nmidle--;
	}
	return mp;
}

// Put gp on the global runnable queue.
// Sched must be locked.
static void
globrunqput(G *gp)
{
	gp->schedlink = nil;
	if(runtime_sched.runqtail)
		runtime_sched.runqtail->schedlink = gp;
	else
		runtime_sched.runqhead = gp;
	runtime_sched.runqtail = gp;
	runtime_sched.runqsize++;
}

// Try get a batch of G's from the global runnable queue.
// Sched must be locked.
static G*
globrunqget(P *p)
{
	G *gp, *gp1;
	int32 n;

	if(runtime_sched.runqsize == 0)
		return nil;
	n = runtime_sched.runqsize/runtime_gomaxprocs+1;
	if(n > runtime_sched.runqsize)
		n = runtime_sched.runqsize;
	runtime_sched.runqsize -= n;
	if(runtime_sched.runqsize == 0)
		runtime_sched.runqtail = nil;
	gp = runtime_sched.runqhead;
	runtime_sched.runqhead = gp->schedlink;
	n--;
	while(n--) {
		gp1 = runtime_sched.runqhead;
		runtime_sched.runqhead = gp1->schedlink;
		runqput(p, gp1);
	}
	return gp;
}

// Put p to on pidle list.
// Sched must be locked.
static void
pidleput(P *p)
{
	p->link = runtime_sched.pidle;
	runtime_sched.pidle = p;
	runtime_xadd(&runtime_sched.npidle, 1);  // TODO: fast atomic
}

// Try get a p from pidle list.
// Sched must be locked.
static P*
pidleget(void)
{
	P *p;

	p = runtime_sched.pidle;
	if(p) {
		runtime_sched.pidle = p->link;
		runtime_xadd(&runtime_sched.npidle, -1);  // TODO: fast atomic
	}
	return p;
}

// Put g on local runnable queue.
// TODO(dvyukov): consider using lock-free queue.
static void
runqput(P *p, G *gp)
{
	int32 h, t, s;

	runtime_lock(p);
retry:
	h = p->runqhead;
	t = p->runqtail;
	s = p->runqsize;
	if(t == h-1 || (h == 0 && t == s-1)) {
		runqgrow(p);
		goto retry;
	}
	p->runq[t++] = gp;
	if(t == s)
		t = 0;
	p->runqtail = t;
	runtime_unlock(p);
}

// Get g from local runnable queue.
static G*
runqget(P *p)
{
	G *gp;
	int32 t, h, s;

	if(p->runqhead == p->runqtail)
		return nil;
	runtime_lock(p);
	h = p->runqhead;
	t = p->runqtail;
	s = p->runqsize;
	if(t == h) {
		runtime_unlock(p);
		return nil;
	}
	gp = p->runq[h++];
	if(h == s)
		h = 0;
	p->runqhead = h;
	runtime_unlock(p);
	return gp;
}

// Grow local runnable queue.
// TODO(dvyukov): consider using fixed-size array
// and transfer excess to the global list (local queue can grow way too big).
static void
runqgrow(P *p)
{
	G **q;
	int32 s, t, h, t2;

	h = p->runqhead;
	t = p->runqtail;
	s = p->runqsize;
	t2 = 0;
	q = runtime_malloc(2*s*sizeof(*q));
	while(t != h) {
		q[t2++] = p->runq[h++];
		if(h == s)
			h = 0;
	}
	runtime_free(p->runq);
	p->runq = q;
	p->runqhead = 0;
	p->runqtail = t2;
	p->runqsize = 2*s;
}

// Steal half of elements from local runnable queue of p2
// and put onto local runnable queue of p.
// Returns one of the stolen elements (or nil if failed).
static G*
runqsteal(P *p, P *p2)
{
	G *gp, *gp1;
	int32 t, h, s, t2, h2, s2, c, i;

	if(p2->runqhead == p2->runqtail)
		return nil;
	// sort locks to prevent deadlocks
	if(p < p2)
		runtime_lock(p);
	runtime_lock(p2);
	if(p2->runqhead == p2->runqtail) {
		runtime_unlock(p2);
		if(p < p2)
			runtime_unlock(p);
		return nil;
	}
	if(p >= p2)
		runtime_lock(p);
	// now we've locked both queues and know the victim is not empty
	h = p->runqhead;
	t = p->runqtail;
	s = p->runqsize;
	h2 = p2->runqhead;
	t2 = p2->runqtail;
	s2 = p2->runqsize;
	gp = p2->runq[h2++];  // return value
	if(h2 == s2)
		h2 = 0;
	// steal roughly half
	if(t2 > h2)
		c = (t2 - h2) / 2;
	else
		c = (s2 - h2 + t2) / 2;
	// copy
	for(i = 0; i != c; i++) {
		// the target queue is full?
		if(t == h-1 || (h == 0 && t == s-1))
			break;
		// the victim queue is empty?
		if(t2 == h2)
			break;
		gp1 = p2->runq[h2++];
		if(h2 == s2)
			h2 = 0;
		p->runq[t++] = gp1;
		if(t == s)
			t = 0;
	}
	p->runqtail = t;
	p2->runqhead = h2;
	runtime_unlock(p2);
	runtime_unlock(p);
	return gp;
}

void runtime_testSchedLocalQueue(void)
  __asm__("runtime.testSchedLocalQueue");

void
runtime_testSchedLocalQueue(void)
{
	P p;
	G gs[1000];
	int32 i, j;

	runtime_memclr((byte*)&p, sizeof(p));
	p.runqsize = 1;
	p.runqhead = 0;
	p.runqtail = 0;
	p.runq = runtime_malloc(p.runqsize*sizeof(*p.runq));

	for(i = 0; i < (int32)nelem(gs); i++) {
		if(runqget(&p) != nil)
			runtime_throw("runq is not empty initially");
		for(j = 0; j < i; j++)
			runqput(&p, &gs[i]);
		for(j = 0; j < i; j++) {
			if(runqget(&p) != &gs[i]) {
				runtime_printf("bad element at iter %d/%d\n", i, j);
				runtime_throw("bad element");
			}
		}
		if(runqget(&p) != nil)
			runtime_throw("runq is not empty afterwards");
	}
}

void runtime_testSchedLocalQueueSteal(void)
  __asm__("runtime.testSchedLocalQueueSteal");

void
runtime_testSchedLocalQueueSteal(void)
{
	P p1, p2;
	G gs[1000], *gp;
	int32 i, j, s;

	runtime_memclr((byte*)&p1, sizeof(p1));
	p1.runqsize = 1;
	p1.runqhead = 0;
	p1.runqtail = 0;
	p1.runq = runtime_malloc(p1.runqsize*sizeof(*p1.runq));

	runtime_memclr((byte*)&p2, sizeof(p2));
	p2.runqsize = nelem(gs);
	p2.runqhead = 0;
	p2.runqtail = 0;
	p2.runq = runtime_malloc(p2.runqsize*sizeof(*p2.runq));

	for(i = 0; i < (int32)nelem(gs); i++) {
		for(j = 0; j < i; j++) {
			gs[j].sig = 0;
			runqput(&p1, &gs[j]);
		}
		gp = runqsteal(&p2, &p1);
		s = 0;
		if(gp) {
			s++;
			gp->sig++;
		}
		while((gp = runqget(&p2)) != nil) {
			s++;
			gp->sig++;
		}
		while((gp = runqget(&p1)) != nil)
			gp->sig++;
		for(j = 0; j < i; j++) {
			if(gs[j].sig != 1) {
				runtime_printf("bad element %d(%d) at iter %d\n", j, gs[j].sig, i);
				runtime_throw("bad element");
			}
		}
		if(s != i/2 && s != i/2+1) {
			runtime_printf("bad steal %d, want %d or %d, iter %d\n",
				s, i/2, i/2+1, i);
			runtime_throw("bad steal");
		}
	}
}

void
runtime_proc_scan(void (*addroot)(Obj))
{
	addroot((Obj){(byte*)&runtime_sched, sizeof runtime_sched, 0});
}
