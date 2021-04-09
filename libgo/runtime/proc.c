// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <errno.h>
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

#ifdef USING_SPLIT_STACK

/* FIXME: These are not declared anywhere.  */

extern void __splitstack_getcontext(void *context[10]);

extern void __splitstack_setcontext(void *context[10]);

extern void *__splitstack_makecontext(size_t, void *context[10], size_t *);

extern void * __splitstack_resetcontext(void *context[10], size_t *);

extern void __splitstack_releasecontext(void *context[10]);

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
# define StackMin ((sizeof(char *) < 8) ? 2 * 1024 * 1024 : 4 * 1024 * 1024)
#endif

uintptr runtime_stacks_sys;

void gtraceback(G*)
  __asm__(GOSYM_PREFIX "runtime.gtraceback");

static void gscanstack(G*);

#ifdef __rtems__
#define __thread
#endif

__thread G *g __asm__(GOSYM_PREFIX "runtime.g");

#ifndef SETCONTEXT_CLOBBERS_TLS

static inline void
initcontext(void)
{
}

static inline void
fixcontext(__go_context_t *c __attribute__ ((unused)))
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

# elif defined(__sparc__)

static inline void
initcontext(void)
{
}

static inline void
fixcontext(ucontext_t *c)
{
	/* ??? Using 
	     register unsigned long thread __asm__("%g7");
	     c->uc_mcontext.gregs[REG_G7] = thread;
	   results in
	     error: variable ‘thread’ might be clobbered by \
		‘longjmp’ or ‘vfork’ [-Werror=clobbered]
	   which ought to be false, as %g7 is a fixed register.  */

	if (sizeof (c->uc_mcontext.gregs[REG_G7]) == 8)
		asm ("stx %%g7, %0" : "=m"(c->uc_mcontext.gregs[REG_G7]));
	else
		asm ("st %%g7, %0" : "=m"(c->uc_mcontext.gregs[REG_G7]));
}

# elif defined(_AIX)

static inline void
initcontext(void)
{
}

static inline void
fixcontext(ucontext_t* c)
{
	// Thread pointer is in r13, per 64-bit ABI.
	if (sizeof (c->uc_mcontext.jmp_context.gpr[13]) == 8)
		asm ("std 13, %0" : "=m"(c->uc_mcontext.jmp_context.gpr[13]));
}

# else

#  error unknown case for SETCONTEXT_CLOBBERS_TLS

# endif

#endif

// ucontext_arg returns a properly aligned ucontext_t value.  On some
// systems a ucontext_t value must be aligned to a 16-byte boundary.
// The g structure that has fields of type ucontext_t is defined in
// Go, and Go has no simple way to align a field to such a boundary.
// So we make the field larger in runtime2.go and pick an appropriate
// offset within the field here.
static __go_context_t*
ucontext_arg(uintptr_t* go_ucontext)
{
	uintptr_t p = (uintptr_t)go_ucontext;
	size_t align = __alignof__(__go_context_t);
	if(align > 16) {
		// We only ensured space for up to a 16 byte alignment
		// in libgo/go/runtime/runtime2.go.
		runtime_throw("required alignment of __go_context_t too large");
	}
	p = (p + align - 1) &~ (uintptr_t)(align - 1);
	return (__go_context_t*)p;
}

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
	if(g == nil)
		return nil;
	return g->m;
}

// Set g.

void runtime_setg(G*) __attribute__ ((no_split_stack));

void
runtime_setg(G* gp)
{
	g = gp;
}

void runtime_newosproc(M *)
  __asm__(GOSYM_PREFIX "runtime.newosproc");

// Start a new thread.
void
runtime_newosproc(M *mp)
{
	pthread_attr_t attr;
	sigset_t clear, old;
	pthread_t tid;
	int tries;
	int ret;

	if(pthread_attr_init(&attr) != 0)
		runtime_throw("pthread_attr_init");
	if(pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0)
		runtime_throw("pthread_attr_setdetachstate");

	// Block signals during pthread_create so that the new thread
	// starts with signals disabled.  It will enable them in minit.
	sigfillset(&clear);

#ifdef SIGTRAP
	// Blocking SIGTRAP reportedly breaks gdb on Alpha GNU/Linux.
	sigdelset(&clear, SIGTRAP);
#endif

	sigemptyset(&old);
	pthread_sigmask(SIG_BLOCK, &clear, &old);

	for (tries = 0; tries < 20; tries++) {
		ret = pthread_create(&tid, &attr, runtime_mstart, mp);
		if (ret != EAGAIN) {
			break;
		}
		runtime_usleep((tries + 1) * 1000); // Milliseconds.
	}

	pthread_sigmask(SIG_SETMASK, &old, nil);

	if (ret != 0) {
		runtime_printf("pthread_create failed: %d\n", ret);
		runtime_throw("pthread_create");
	}

	if(pthread_attr_destroy(&attr) != 0)
		runtime_throw("pthread_attr_destroy");
}

// Switch context to a different goroutine.  This is like longjmp.
void runtime_gogo(G*) __attribute__ ((noinline));
void
runtime_gogo(G* newg)
{
#ifdef USING_SPLIT_STACK
	__splitstack_setcontext((void*)(&newg->stackcontext[0]));
#endif
	g = newg;
	newg->fromgogo = true;
	fixcontext(ucontext_arg(&newg->context[0]));
	__go_setcontext(ucontext_arg(&newg->context[0]));
	runtime_throw("gogo setcontext returned");
}

// Save context and call fn passing g as a parameter.  This is like
// setjmp.  Because getcontext always returns 0, unlike setjmp, we use
// g->fromgogo as a code.  It will be true if we got here via
// setcontext.  g == nil the first time this is called in a new m.
void runtime_mcall(FuncVal *) __attribute__ ((noinline));
void
runtime_mcall(FuncVal *fv)
{
	M *mp;
	G *gp;
#ifndef USING_SPLIT_STACK
	void *afterregs;
#endif

	// Ensure that all registers are on the stack for the garbage
	// collector.
	__builtin_unwind_init();
	flush_registers_to_secondary_stack();

	gp = g;
	mp = gp->m;
	if(gp == mp->g0)
		runtime_throw("runtime: mcall called on m->g0 stack");

	if(gp != nil) {

#ifdef USING_SPLIT_STACK
		__splitstack_getcontext((void*)(&gp->stackcontext[0]));
#else
		// We have to point to an address on the stack that is
		// below the saved registers.
		gp->gcnextsp = (uintptr)(&afterregs);
		gp->gcnextsp2 = (uintptr)(secondary_stack_pointer());
#endif
		gp->fromgogo = false;
		__go_getcontext(ucontext_arg(&gp->context[0]));

		// When we return from getcontext, we may be running
		// in a new thread.  That means that g may have
		// changed.  It is a global variables so we will
		// reload it, but the address of g may be cached in
		// our local stack frame, and that address may be
		// wrong.  Call the function to reload the value for
		// this thread.
		gp = runtime_g();
		mp = gp->m;

		if(gp->traceback != 0)
			gtraceback(gp);
		if(gp->scang != 0)
			gscanstack(gp);
	}
	if (gp == nil || !gp->fromgogo) {
#ifdef USING_SPLIT_STACK
		__splitstack_setcontext((void*)(&mp->g0->stackcontext[0]));
#endif
		mp->g0->entry = fv;
		mp->g0->param = gp;

		// It's OK to set g directly here because this case
		// can not occur if we got here via a setcontext to
		// the getcontext call just above.
		g = mp->g0;

		fixcontext(ucontext_arg(&mp->g0->context[0]));
		__go_setcontext(ucontext_arg(&mp->g0->context[0]));
		runtime_throw("runtime: mcall function returned");
	}
}

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

extern G* allocg(void)
  __asm__ (GOSYM_PREFIX "runtime.allocg");

bool	runtime_isarchive;

extern void kickoff(void)
  __asm__(GOSYM_PREFIX "runtime.kickoff");
extern void minit(void)
  __asm__(GOSYM_PREFIX "runtime.minit");
extern void mstart1()
  __asm__(GOSYM_PREFIX "runtime.mstart1");
extern void stopm(void)
  __asm__(GOSYM_PREFIX "runtime.stopm");
extern void mexit(bool)
  __asm__(GOSYM_PREFIX "runtime.mexit");
extern void handoffp(P*)
  __asm__(GOSYM_PREFIX "runtime.handoffp");
extern void wakep(void)
  __asm__(GOSYM_PREFIX "runtime.wakep");
extern void stoplockedm(void)
  __asm__(GOSYM_PREFIX "runtime.stoplockedm");
extern void schedule(void)
  __asm__(GOSYM_PREFIX "runtime.schedule");
extern void execute(G*, bool)
  __asm__(GOSYM_PREFIX "runtime.execute");
extern void reentersyscall(uintptr, uintptr)
  __asm__(GOSYM_PREFIX "runtime.reentersyscall");
extern void reentersyscallblock(uintptr, uintptr)
  __asm__(GOSYM_PREFIX "runtime.reentersyscallblock");
extern G* gfget(P*)
  __asm__(GOSYM_PREFIX "runtime.gfget");
extern void acquirep(P*)
  __asm__(GOSYM_PREFIX "runtime.acquirep");
extern P* releasep(void)
  __asm__(GOSYM_PREFIX "runtime.releasep");
extern void incidlelocked(int32)
  __asm__(GOSYM_PREFIX "runtime.incidlelocked");
extern void globrunqput(G*)
  __asm__(GOSYM_PREFIX "runtime.globrunqput");
extern P* pidleget(void)
  __asm__(GOSYM_PREFIX "runtime.pidleget");
extern struct mstats* getMemstats(void)
  __asm__(GOSYM_PREFIX "runtime.getMemstats");

bool runtime_isstarted;

// Used to determine the field alignment.

struct field_align
{
  char c;
  Hchan *p;
};

void getTraceback(G*, G*) __asm__(GOSYM_PREFIX "runtime.getTraceback");

// getTraceback stores a traceback of gp in the g's traceback field
// and then returns to me.  We expect that gp's traceback is not nil.
// It works by saving me's current context, and checking gp's traceback field.
// If gp's traceback field is not nil, it starts running gp.
// In places where we call getcontext, we check the traceback field.
// If it is not nil, we collect a traceback, and then return to the
// goroutine stored in the traceback field, which is me.
void getTraceback(G* me, G* gp)
{
	M* holdm;

	holdm = gp->m;
	gp->m = me->m;

#ifdef USING_SPLIT_STACK
	__splitstack_getcontext((void*)(&me->stackcontext[0]));
#endif
	__go_getcontext(ucontext_arg(&me->context[0]));

	if (gp->traceback != 0) {
		runtime_gogo(gp);
	}

	gp->m = holdm;
}

// Do a stack trace of gp, and then restore the context to
// gp->traceback->gp.

void
gtraceback(G* gp)
{
	Traceback* traceback;

	traceback = (Traceback*)gp->traceback;
	gp->traceback = 0;
	traceback->c = runtime_callers(1, traceback->locbuf,
		sizeof traceback->locbuf / sizeof traceback->locbuf[0], false);
	runtime_gogo(traceback->gp);
}

void doscanstackswitch(G*, G*) __asm__(GOSYM_PREFIX "runtime.doscanstackswitch");

// Switch to gp and let it scan its stack.
// The first time gp->scang is set (to me). The second time here
// gp is done scanning, and has unset gp->scang, so we just return.
void
doscanstackswitch(G* me, G* gp)
{
	M* holdm;

	__go_assert(me->entry == nil);
	me->fromgogo = false;

	holdm = gp->m;
	gp->m = me->m;

#ifdef USING_SPLIT_STACK
	__splitstack_getcontext((void*)(&me->stackcontext[0]));
#endif
	__go_getcontext(ucontext_arg(&me->context[0]));

	if(me->entry != nil) {
		// Got here from mcall.
		// The stack scanning code may call systemstack, which calls
		// mcall, which calls setcontext.
		// Run the function, which at the end will switch back to gp.
		FuncVal *fv = me->entry;
		void (*pfn)(G*) = (void (*)(G*))fv->fn;
		G* gp1 = (G*)me->param;
		__go_assert(gp1 == gp);
		me->entry = nil;
		me->param = nil;
		__builtin_call_with_static_chain(pfn(gp1), fv);
		abort();
	}

	if (gp->scang != 0)
		runtime_gogo(gp);

	gp->m = holdm;
}

// Do a stack scan, then switch back to the g that triggers this scan.
// We come here from doscanstackswitch.
static void
gscanstack(G *gp)
{
	G *oldg, *oldcurg;

	oldg = (G*)gp->scang;
	oldcurg = oldg->m->curg;
	oldg->m->curg = gp;
	gp->scang = 0;

	doscanstack(gp, (void*)gp->scangcw);

	gp->scangcw = 0;
	oldg->m->curg = oldcurg;
	runtime_gogo(oldg);
}

// Called by pthread_create to start an M.
void*
runtime_mstart(void *arg)
{
	M* mp;
	G* gp;

	mp = (M*)(arg);
	gp = mp->g0;
	gp->m = mp;

	g = gp;

	gp->entry = nil;
	gp->param = nil;

	// We have to call minit before we call getcontext,
	// because getcontext will copy the signal mask.
	minit();

	initcontext();

	// Record top of stack for use by mcall.
	// Once we call schedule we're never coming back,
	// so other calls can reuse this stack space.
#ifdef USING_SPLIT_STACK
	__splitstack_getcontext((void*)(&gp->stackcontext[0]));
#else
	gp->gcinitialsp = &arg;
	// Setting gcstacksize to 0 is a marker meaning that gcinitialsp
	// is the top of the stack, not the bottom.
	gp->gcstacksize = 0;
	gp->gcnextsp = (uintptr)(&arg);
	gp->gcinitialsp2 = secondary_stack_pointer();
	gp->gcnextsp2 = (uintptr)(gp->gcinitialsp2);
#endif

	// Save the currently active context.  This will return
	// multiple times via the setcontext call in mcall.
	__go_getcontext(ucontext_arg(&gp->context[0]));

	if(gp->traceback != 0) {
		// Got here from getTraceback.
		// I'm not sure this ever actually happens--getTraceback
		// may always go to the getcontext call in mcall.
		gtraceback(gp);
	}
	if(gp->scang != 0)
		// Got here from doscanswitch. Should not happen.
		runtime_throw("mstart with scang");

	if(gp->entry != nil) {
		// Got here from mcall.
		FuncVal *fv = gp->entry;
		void (*pfn)(G*) = (void (*)(G*))fv->fn;
		G* gp1 = (G*)gp->param;
		gp->entry = nil;
		gp->param = nil;
		__builtin_call_with_static_chain(pfn(gp1), fv);
		*(int*)0x21 = 0x21;
	}

	if(mp->exiting) {
		mexit(true);
		return nil;
	}

	// Initial call to getcontext--starting thread.

#ifdef USING_SPLIT_STACK
	{
		int dont_block_signals = 0;
		__splitstack_block_signals(&dont_block_signals, nil);
	}
#endif

	mstart1();

	// mstart1 does not return, but we need a return statement
	// here to avoid a compiler warning.
	return nil;
}

typedef struct CgoThreadStart CgoThreadStart;
struct CgoThreadStart
{
	M *m;
	G *g;
	uintptr *tls;
	void (*fn)(void);
};

void setGContext(void) __asm__ (GOSYM_PREFIX "runtime.setGContext");

// setGContext sets up a new goroutine context for the current g.
void
setGContext(void)
{
	int val;
	G *gp;

	initcontext();
	gp = g;
	gp->entry = nil;
	gp->param = nil;
#ifdef USING_SPLIT_STACK
	__splitstack_getcontext((void*)(&gp->stackcontext[0]));
	val = 0;
	__splitstack_block_signals(&val, nil);
#else
	gp->gcinitialsp = &val;
	gp->gcstack = 0;
	gp->gcstacksize = 0;
	gp->gcnextsp = (uintptr)(&val);
	gp->gcinitialsp2 = secondary_stack_pointer();
	gp->gcnextsp2 = (uintptr)(gp->gcinitialsp2);
#endif
	__go_getcontext(ucontext_arg(&gp->context[0]));

	if(gp->entry != nil) {
		// Got here from mcall.
		FuncVal *fv = gp->entry;
		void (*pfn)(G*) = (void (*)(G*))fv->fn;
		G* gp1 = (G*)gp->param;
		gp->entry = nil;
		gp->param = nil;
		__builtin_call_with_static_chain(pfn(gp1), fv);
		*(int*)0x22 = 0x22;
	}
}

void makeGContext(G*, byte*, uintptr)
	__asm__(GOSYM_PREFIX "runtime.makeGContext");

// makeGContext makes a new context for a g.
void
makeGContext(G* gp, byte* sp, uintptr spsize) {
	__go_context_t *uc;

	uc = ucontext_arg(&gp->context[0]);
	__go_getcontext(uc);
	__go_makecontext(uc, kickoff, sp, (size_t)spsize);
}

// The goroutine g is about to enter a system call.
// Record that it's not using the cpu anymore.
// This is called only from the go syscall library and cgocall,
// not from the low-level system calls used by the runtime.
//
// Entersyscall cannot split the stack: the runtime_gosave must
// make g->sched refer to the caller's stack segment, because
// entersyscall is going to return immediately after.

void runtime_entersyscall() __attribute__ ((no_split_stack));
static void doentersyscall(uintptr, uintptr)
  __attribute__ ((no_split_stack, noinline));

void
runtime_entersyscall()
{
	// Save the registers in the g structure so that any pointers
	// held in registers will be seen by the garbage collector.
	if (!runtime_usestackmaps)
		__go_getcontext(ucontext_arg(&g->gcregs[0]));

	// Note that if this function does save any registers itself,
	// we might store the wrong value in the call to getcontext.
	// FIXME: This assumes that we do not need to save any
	// callee-saved registers to access the TLS variable g.  We
	// don't want to put the ucontext_t on the stack because it is
	// large and we can not split the stack here.
	doentersyscall((uintptr)runtime_getcallerpc(),
		       (uintptr)runtime_getcallersp());
}

static void
doentersyscall(uintptr pc, uintptr sp)
{
	// Leave SP around for GC and traceback.
#ifdef USING_SPLIT_STACK
	{
	  size_t gcstacksize;
	  g->gcstack = (uintptr)(__splitstack_find(nil, nil, &gcstacksize,
						   (void**)(&g->gcnextsegment),
						   (void**)(&g->gcnextsp),
						   &g->gcinitialsp));
	  g->gcstacksize = (uintptr)gcstacksize;
	}
#else
	{
		void *v;

		g->gcnextsp = (uintptr)(&v);
		g->gcnextsp2 = (uintptr)(secondary_stack_pointer());
	}
#endif

	reentersyscall(pc, sp);
}

static void doentersyscallblock(uintptr, uintptr)
  __attribute__ ((no_split_stack, noinline));

// The same as runtime_entersyscall(), but with a hint that the syscall is blocking.
void
runtime_entersyscallblock()
{
	// Save the registers in the g structure so that any pointers
	// held in registers will be seen by the garbage collector.
	if (!runtime_usestackmaps)
		__go_getcontext(ucontext_arg(&g->gcregs[0]));

	// See comment in runtime_entersyscall.
	doentersyscallblock((uintptr)runtime_getcallerpc(),
			    (uintptr)runtime_getcallersp());
}

static void
doentersyscallblock(uintptr pc, uintptr sp)
{
	// Leave SP around for GC and traceback.
#ifdef USING_SPLIT_STACK
	{
	  size_t gcstacksize;
	  g->gcstack = (uintptr)(__splitstack_find(nil, nil, &gcstacksize,
						   (void**)(&g->gcnextsegment),
						   (void**)(&g->gcnextsp),
						   &g->gcinitialsp));
	  g->gcstacksize = (uintptr)gcstacksize;
	}
#else
	{
		void *v;

		g->gcnextsp = (uintptr)(&v);
		g->gcnextsp2 = (uintptr)(secondary_stack_pointer());
	}
#endif

	reentersyscallblock(pc, sp);
}

// Allocate a new g, with a stack big enough for stacksize bytes.
G*
runtime_malg(bool allocatestack, bool signalstack, byte** ret_stack, uintptr* ret_stacksize)
{
	uintptr stacksize;
	G *newg;
	byte* unused_stack;
	uintptr unused_stacksize;
#ifdef USING_SPLIT_STACK
	int dont_block_signals = 0;
	size_t ss_stacksize;
#endif

	if (ret_stack == nil) {
		ret_stack = &unused_stack;
	}
	if (ret_stacksize == nil) {
		ret_stacksize = &unused_stacksize;
	}
	newg = allocg();
	if(allocatestack) {
		stacksize = StackMin;
		if(signalstack) {
			stacksize = 32 * 1024; // OS X wants >= 8K, GNU/Linux >= 2K
#ifdef SIGSTKSZ
			if(stacksize < (uintptr)(SIGSTKSZ))
				stacksize = (uintptr)(SIGSTKSZ);
#endif
		}

#ifdef USING_SPLIT_STACK
		*ret_stack = __splitstack_makecontext(stacksize,
						      (void*)(&newg->stackcontext[0]),
						      &ss_stacksize);
		*ret_stacksize = (uintptr)ss_stacksize;
		__splitstack_block_signals_context((void*)(&newg->stackcontext[0]),
						   &dont_block_signals, nil);
#else
                // In 64-bit mode, the maximum Go allocation space is
                // 128G.  Our stack size is 4M, which only permits 32K
                // goroutines.  In order to not limit ourselves,
                // allocate the stacks out of separate memory.  In
                // 32-bit mode, the Go allocation space is all of
                // memory anyhow.
		if(sizeof(void*) == 8) {
			void *p = runtime_sysAlloc(stacksize, &getMemstats()->stacks_sys);
			if(p == nil)
				runtime_throw("runtime: cannot allocate memory for goroutine stack");
			*ret_stack = (byte*)p;
		} else {
			*ret_stack = runtime_mallocgc(stacksize, nil, false);
			runtime_xadd(&runtime_stacks_sys, stacksize);
		}
		*ret_stacksize = (uintptr)stacksize;
		newg->gcinitialsp = *ret_stack;
		newg->gcstacksize = (uintptr)stacksize;
		newg->gcinitialsp2 = initial_secondary_stack_pointer(*ret_stack);
#endif
	}
	return newg;
}

void stackfree(G*)
  __asm__(GOSYM_PREFIX "runtime.stackfree");

// stackfree frees the stack of a g.
void
stackfree(G* gp)
{
#ifdef USING_SPLIT_STACK
  __splitstack_releasecontext((void*)(&gp->stackcontext[0]));
#else
  // If gcstacksize is 0, the stack is allocated by libc and will be
  // released when the thread exits. Otherwise, in 64-bit mode it was
  // allocated using sysAlloc and in 32-bit mode it was allocated
  // using garbage collected memory.
  if (gp->gcstacksize != 0) {
    if (sizeof(void*) == 8) {
      runtime_sysFree(gp->gcinitialsp, gp->gcstacksize, &getMemstats()->stacks_sys);
    }
    gp->gcinitialsp = nil;
    gp->gcstacksize = 0;
  }
#endif
}

void resetNewG(G*, void **, uintptr*)
  __asm__(GOSYM_PREFIX "runtime.resetNewG");

// Reset stack information for g pulled out of the cache to start a
// new goroutine.
void
resetNewG(G *newg, void **sp, uintptr *spsize)
{
#ifdef USING_SPLIT_STACK
  int dont_block_signals = 0;
  size_t ss_spsize;

  *sp = __splitstack_resetcontext((void*)(&newg->stackcontext[0]), &ss_spsize);
  *spsize = ss_spsize;
  __splitstack_block_signals_context((void*)(&newg->stackcontext[0]),
				     &dont_block_signals, nil);
#else
  *sp = newg->gcinitialsp;
  *spsize = newg->gcstacksize;
  if(*spsize == 0)
    runtime_throw("bad spsize in resetNewG");
  newg->gcnextsp = (uintptr)(*sp);
  newg->gcnextsp2 = (uintptr)(newg->gcinitialsp2);
#endif
}
