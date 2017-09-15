/* go-signal.c -- signal handling for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <ucontext.h>

#include "runtime.h"

#ifndef SA_RESTART
  #define SA_RESTART 0
#endif

#ifdef USING_SPLIT_STACK

extern void __splitstack_getcontext(void *context[10]);

extern void __splitstack_setcontext(void *context[10]);

extern void *__splitstack_find_context(void *context[10], size_t *,
				       void **, void **, void **);

#endif

// The rest of the signal handler, written in Go.

extern void sigtrampgo(uint32, siginfo_t *, void *)
	__asm__(GOSYM_PREFIX "runtime.sigtrampgo");

// The Go signal handler, written in C.  This should be running on the
// alternate signal stack.  This is responsible for setting up the
// split stack context so that stack guard checks will work as
// expected.

void sigtramp(int, siginfo_t *, void *)
	__attribute__ ((no_split_stack));

void sigtramp(int, siginfo_t *, void *)
	__asm__ (GOSYM_PREFIX "runtime.sigtramp");

#ifndef USING_SPLIT_STACK

// When not using split stacks, there are no stack checks, and there
// is nothing special for this function to do.

void
sigtramp(int sig, siginfo_t *info, void *context)
{
	sigtrampgo(sig, info, context);
}

#else // USING_SPLIT_STACK

void
sigtramp(int sig, siginfo_t *info, void *context)
{
	G *gp;
	void *stack_context[10];
	void *stack;
	size_t stack_size;
	void *next_segment;
	void *next_sp;
	void *initial_sp;
	uintptr sp;
	stack_t st;
	uintptr stsp;

	gp = runtime_g();

	if (gp == nil) {
		// Let the Go code handle this case.
		// It should only call nosplit functions in this case.
		sigtrampgo(sig, info, context);
		return;
	}

	// If this signal is one for which we will panic, we are not
	// on the alternate signal stack.  It's OK to call split-stack
	// functions here.
	if (sig == SIGBUS || sig == SIGFPE || sig == SIGSEGV) {
		sigtrampgo(sig, info, context);
		return;
	}

	// We are running on the alternate signal stack.

	__splitstack_getcontext(&stack_context[0]);

	stack = __splitstack_find_context((void*)(&gp->m->gsignal->stackcontext[0]),
					  &stack_size, &next_segment,
					  &next_sp, &initial_sp);

	// If some non-Go code called sigaltstack, adjust.
	sp = (uintptr)(&stack_size);
	if (sp < (uintptr)(stack) || sp >= (uintptr)(stack) + stack_size) {
		sigaltstack(nil, &st);
		if ((st.ss_flags & SS_DISABLE) != 0) {
			runtime_printf("signal %d received on thread with no signal stack\n", (int32)(sig));
			runtime_throw("non-Go code disabled sigaltstack");
		}

		stsp = (uintptr)(st.ss_sp);
		if (sp < stsp || sp >= stsp + st.ss_size) {
			runtime_printf("signal %d received but handler not on signal stack\n", (int32)(sig));
			runtime_throw("non-Go code set up signal handler without SA_ONSTACK flag");
		}

		// Unfortunately __splitstack_find_context will return NULL
		// when it is called on a context that has never been used.
		// There isn't much we can do but assume all is well.
		if (stack != NULL) {
			// Here the gc runtime adjusts the gsignal
			// stack guard to match the values returned by
			// sigaltstack.  Unfortunately we have no way
			// to do that.
			runtime_printf("signal %d received on unknown signal stack\n", (int32)(sig));
			runtime_throw("non-Go code changed signal stack");
		}
	}

	// Set the split stack context so that the stack guards are
	// checked correctly.

	__splitstack_setcontext((void*)(&gp->m->gsignal->stackcontext[0]));

	sigtrampgo(sig, info, context);

	// We are going to return back to the signal trampoline and
	// then to whatever we were doing before we got the signal.
	// Restore the split stack context so that stack guards are
	// checked correctly.

	__splitstack_setcontext(&stack_context[0]);
}

#endif // USING_SPLIT_STACK

// C function to return the address of the sigtramp function.
uintptr getSigtramp(void) __asm__ (GOSYM_PREFIX "runtime.getSigtramp");

uintptr
getSigtramp()
{
  return (uintptr)(void*)sigtramp;
}

// C code to manage the sigaction sa_sigaction field, which is
// typically a union and so hard for mksysinfo.sh to handle.

uintptr getSigactionHandler(struct sigaction*)
	__attribute__ ((no_split_stack));

uintptr getSigactionHandler(struct sigaction*)
	__asm__ (GOSYM_PREFIX "runtime.getSigactionHandler");

uintptr
getSigactionHandler(struct sigaction* sa)
{
	return (uintptr)(sa->sa_sigaction);
}

void setSigactionHandler(struct sigaction*, uintptr)
	__attribute__ ((no_split_stack));

void setSigactionHandler(struct sigaction*, uintptr)
	__asm__ (GOSYM_PREFIX "runtime.setSigactionHandler");

void
setSigactionHandler(struct sigaction* sa, uintptr handler)
{
	sa->sa_sigaction = (void*)(handler);
}

// C code to fetch values from the siginfo_t and ucontext_t pointers
// passed to a signal handler.

struct getSiginfoRet {
	uintptr sigaddr;
	uintptr sigpc;
};

struct getSiginfoRet getSiginfo(siginfo_t *, void *)
	__asm__(GOSYM_PREFIX "runtime.getSiginfo");

struct getSiginfoRet
getSiginfo(siginfo_t *info, void *context __attribute__((unused)))
{
	struct getSiginfoRet ret;
	Location loc[1];
	int32 n;

	if (info == nil) {
		ret.sigaddr = 0;
	} else {
		ret.sigaddr = (uintptr)(info->si_addr);
	}
	ret.sigpc = 0;

	// There doesn't seem to be a portable way to get the PC.
	// Use unportable code to pull it from context, and if that fails
	// try a stack backtrace across the signal handler.

#ifdef __x86_64__
 #ifdef __linux__
	ret.sigpc = ((ucontext_t*)(context))->uc_mcontext.gregs[REG_RIP];
 #endif
#endif
#ifdef __i386__
  #ifdef __linux__
	ret.sigpc = ((ucontext_t*)(context))->uc_mcontext.gregs[REG_EIP];
  #endif
#endif
#ifdef __alpha__
  #ifdef __linux__
	ret.sigpc = ((ucontext_t*)(context))->uc_mcontext.sc_pc;
  #endif
#endif
#ifdef __PPC__
  #ifdef __linux__
	ret.sigpc = ((ucontext_t*)(context))->uc_mcontext.regs->nip;
  #endif
  #ifdef _AIX
	ret.sigpc = ((ucontext_t*)(context))->uc_mcontext.jmp_context.iar;
  #endif
#endif

	if (ret.sigpc == 0) {
		// Skip getSiginfo/sighandler/sigtrampgo/sigtramp/handler.
		n = runtime_callers(5, &loc[0], 1, false);
		if (n > 0) {
			ret.sigpc = loc[0].pc;
		}
	}

	return ret;
}

// Dump registers when crashing in a signal.
// There is no portable way to write this,
// so we just have some CPU/OS specific implementations.

void dumpregs(siginfo_t *, void *)
	__asm__(GOSYM_PREFIX "runtime.dumpregs");

void
dumpregs(siginfo_t *info __attribute__((unused)), void *context __attribute__((unused)))
{
#ifdef __x86_64__
 #ifdef __linux__
	{
		mcontext_t *m = &((ucontext_t*)(context))->uc_mcontext;

		runtime_printf("rax    %X\n", m->gregs[REG_RAX]);
		runtime_printf("rbx    %X\n", m->gregs[REG_RBX]);
		runtime_printf("rcx    %X\n", m->gregs[REG_RCX]);
		runtime_printf("rdx    %X\n", m->gregs[REG_RDX]);
		runtime_printf("rdi    %X\n", m->gregs[REG_RDI]);
		runtime_printf("rsi    %X\n", m->gregs[REG_RSI]);
		runtime_printf("rbp    %X\n", m->gregs[REG_RBP]);
		runtime_printf("rsp    %X\n", m->gregs[REG_RSP]);
		runtime_printf("r8     %X\n", m->gregs[REG_R8]);
		runtime_printf("r9     %X\n", m->gregs[REG_R9]);
		runtime_printf("r10    %X\n", m->gregs[REG_R10]);
		runtime_printf("r11    %X\n", m->gregs[REG_R11]);
		runtime_printf("r12    %X\n", m->gregs[REG_R12]);
		runtime_printf("r13    %X\n", m->gregs[REG_R13]);
		runtime_printf("r14    %X\n", m->gregs[REG_R14]);
		runtime_printf("r15    %X\n", m->gregs[REG_R15]);
		runtime_printf("rip    %X\n", m->gregs[REG_RIP]);
		runtime_printf("rflags %X\n", m->gregs[REG_EFL]);
		runtime_printf("cs     %X\n", m->gregs[REG_CSGSFS] & 0xffff);
		runtime_printf("fs     %X\n", (m->gregs[REG_CSGSFS] >> 16) & 0xffff);
		runtime_printf("gs     %X\n", (m->gregs[REG_CSGSFS] >> 32) & 0xffff);
	  }
 #endif
#endif

#ifdef __i386__
 #ifdef __linux__
	{
		mcontext_t *m = &((ucontext_t*)(context))->uc_mcontext;

		runtime_printf("eax    %x\n", m->gregs[REG_EAX]);
		runtime_printf("ebx    %x\n", m->gregs[REG_EBX]);
		runtime_printf("ecx    %x\n", m->gregs[REG_ECX]);
		runtime_printf("edx    %x\n", m->gregs[REG_EDX]);
		runtime_printf("edi    %x\n", m->gregs[REG_EDI]);
		runtime_printf("esi    %x\n", m->gregs[REG_ESI]);
		runtime_printf("ebp    %x\n", m->gregs[REG_EBP]);
		runtime_printf("esp    %x\n", m->gregs[REG_ESP]);
		runtime_printf("eip    %x\n", m->gregs[REG_EIP]);
		runtime_printf("eflags %x\n", m->gregs[REG_EFL]);
		runtime_printf("cs     %x\n", m->gregs[REG_CS]);
		runtime_printf("fs     %x\n", m->gregs[REG_FS]);
		runtime_printf("gs     %x\n", m->gregs[REG_GS]);
	  }
 #endif
#endif

#ifdef __alpha__
  #ifdef __linux__
	{
		mcontext_t *m = &((ucontext_t*)(context))->uc_mcontext;

		runtime_printf("v0  %X\n", m->sc_regs[0]);
		runtime_printf("t0  %X\n", m->sc_regs[1]);
		runtime_printf("t1  %X\n", m->sc_regs[2]);
		runtime_printf("t2  %X\n", m->sc_regs[3]);
		runtime_printf("t3  %X\n", m->sc_regs[4]);
		runtime_printf("t4  %X\n", m->sc_regs[5]);
		runtime_printf("t5  %X\n", m->sc_regs[6]);
		runtime_printf("t6  %X\n", m->sc_regs[7]);
		runtime_printf("t7  %X\n", m->sc_regs[8]);
		runtime_printf("s0  %X\n", m->sc_regs[9]);
		runtime_printf("s1  %X\n", m->sc_regs[10]);
		runtime_printf("s2  %X\n", m->sc_regs[11]);
		runtime_printf("s3  %X\n", m->sc_regs[12]);
		runtime_printf("s4  %X\n", m->sc_regs[13]);
		runtime_printf("s5  %X\n", m->sc_regs[14]);
		runtime_printf("fp  %X\n", m->sc_regs[15]);
		runtime_printf("a0  %X\n", m->sc_regs[16]);
		runtime_printf("a1  %X\n", m->sc_regs[17]);
		runtime_printf("a2  %X\n", m->sc_regs[18]);
		runtime_printf("a3  %X\n", m->sc_regs[19]);
		runtime_printf("a4  %X\n", m->sc_regs[20]);
		runtime_printf("a5  %X\n", m->sc_regs[21]);
		runtime_printf("t8  %X\n", m->sc_regs[22]);
		runtime_printf("t9  %X\n", m->sc_regs[23]);
		runtime_printf("t10 %X\n", m->sc_regs[24]);
		runtime_printf("t11 %X\n", m->sc_regs[25]);
		runtime_printf("ra  %X\n", m->sc_regs[26]);
		runtime_printf("t12 %X\n", m->sc_regs[27]);
		runtime_printf("at  %X\n", m->sc_regs[28]);
		runtime_printf("gp  %X\n", m->sc_regs[29]);
		runtime_printf("sp  %X\n", m->sc_regs[30]);
		runtime_printf("pc  %X\n", m->sc_pc);
	  }
  #endif
#endif

#if defined(__PPC__) && defined(__LITTLE_ENDIAN__)
  #ifdef __linux__
	  {
		mcontext_t *m = &((ucontext_t*)(context))->uc_mcontext;
		int i;

		for (i = 0; i < 32; i++)
			runtime_printf("r%d %X\n", i, m->regs->gpr[i]);
		runtime_printf("pc  %X\n", m->regs->nip);
		runtime_printf("msr %X\n", m->regs->msr);
		runtime_printf("cr  %X\n", m->regs->ccr);
		runtime_printf("lr  %X\n", m->regs->link);
		runtime_printf("ctr %X\n", m->regs->ctr);
		runtime_printf("xer %X\n", m->regs->xer);
	  }
  #endif
#endif

#ifdef __PPC__
  #ifdef _AIX
	  {
		mcontext_t *m = &((ucontext_t*)(context))->uc_mcontext;
		int i;

		for (i = 0; i < 32; i++)
			runtime_printf("r%d %p\n", i, m->jmp_context.gpr[i]);
		runtime_printf("pc  %p\n", m->jmp_context.iar);
		runtime_printf("msr %p\n", m->jmp_context.msr);
		runtime_printf("cr  %x\n", m->jmp_context.cr);
		runtime_printf("lr  %p\n", m->jmp_context.lr);
		runtime_printf("ctr %p\n", m->jmp_context.ctr);
		runtime_printf("xer %x\n", m->jmp_context.xer);
	  }
  #endif
#endif
}
