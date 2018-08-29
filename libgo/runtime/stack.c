// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Stack scanning code for the garbage collector.

#include "runtime.h"

#ifdef USING_SPLIT_STACK

extern void * __splitstack_find (void *, void *, size_t *, void **, void **,
				 void **);

extern void * __splitstack_find_context (void *context[10], size_t *, void **,
					 void **, void **);

#endif

// Calling unwind_init in doscanstack only works if it does not do a
// tail call to doscanstack1.
#pragma GCC optimize ("-fno-optimize-sibling-calls")

extern void scanstackblock(void *addr, uintptr size, void *gcw)
  __asm__("runtime.scanstackblock");

void doscanstack(G*, void*)
  __asm__("runtime.doscanstack");

static void doscanstack1(G*, void*)
  __attribute__ ((noinline));

// Scan gp's stack, passing stack chunks to scanstackblock.
void doscanstack(G *gp, void* gcw) {
	// Save registers on the stack, so that if we are scanning our
	// own stack we will see them.
	__builtin_unwind_init();
	flush_registers_to_secondary_stack();

	doscanstack1(gp, gcw);
}

// Scan gp's stack after saving registers.
static void doscanstack1(G *gp, void *gcw) {
#ifdef USING_SPLIT_STACK
	void* sp;
	size_t spsize;
	void* next_segment;
	void* next_sp;
	void* initial_sp;

	if (gp == runtime_g()) {
		// Scanning our own stack.
		sp = __splitstack_find(nil, nil, &spsize, &next_segment,
				       &next_sp, &initial_sp);
	} else {
		// Scanning another goroutine's stack.
		// The goroutine is usually asleep (the world is stopped).

		// The exception is that if the goroutine is about to enter or might
		// have just exited a system call, it may be executing code such
		// as schedlock and may have needed to start a new stack segment.
		// Use the stack segment and stack pointer at the time of
		// the system call instead, since that won't change underfoot.
		if(gp->gcstack != 0) {
			sp = (void*)(gp->gcstack);
			spsize = gp->gcstacksize;
			next_segment = (void*)(gp->gcnextsegment);
			next_sp = (void*)(gp->gcnextsp);
			initial_sp = (void*)(gp->gcinitialsp);
		} else {
			sp = __splitstack_find_context((void**)(&gp->stackcontext[0]),
						       &spsize, &next_segment,
						       &next_sp, &initial_sp);
		}
	}
	if(sp != nil) {
		scanstackblock(sp, (uintptr)(spsize), gcw);
		while((sp = __splitstack_find(next_segment, next_sp,
					      &spsize, &next_segment,
					      &next_sp, &initial_sp)) != nil)
			scanstackblock(sp, (uintptr)(spsize), gcw);
	}
#else
	byte* bottom;
	byte* top;
	byte* nextsp2;
	byte* initialsp2;

	if(gp == runtime_g()) {
		// Scanning our own stack.
		bottom = (byte*)&gp;
		nextsp2 = secondary_stack_pointer();
	} else {
		// Scanning another goroutine's stack.
		// The goroutine is usually asleep (the world is stopped).
		bottom = (void*)gp->gcnextsp;
		if(bottom == nil)
			return;
		nextsp2 = (void*)gp->gcnextsp2;
	}
	top = (byte*)(void*)(gp->gcinitialsp) + gp->gcstacksize;
	if(top > bottom)
		scanstackblock(bottom, (uintptr)(top - bottom), gcw);
	else
		scanstackblock(top, (uintptr)(bottom - top), gcw);
	if (nextsp2 != nil) {
		initialsp2 = (byte*)(void*)(gp->gcinitialsp2);
		if(initialsp2 > nextsp2)
			scanstackblock(nextsp2, (uintptr)(initialsp2 - nextsp2), gcw);
		else
			scanstackblock(initialsp2, (uintptr)(nextsp2 - initialsp2), gcw);
	}
#endif
}
