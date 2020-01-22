// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <errno.h>
#include <signal.h>
#include <unistd.h>

#if defined(__i386__) || defined(__x86_64__)
#include <cpuid.h>
#endif

#ifdef __linux__
#include <syscall.h>
#endif

#include "config.h"

#include "runtime.h"
#include "arch.h"
#include "array.h"

int32
runtime_atoi(const byte *p, intgo len)
{
	int32 n;

	n = 0;
	while(len > 0 && '0' <= *p && *p <= '9') {
		n = n*10 + *p++ - '0';
		len--;
	}
	return n;
}

// A random number from the GNU/Linux auxv array.
static uint32 randomNumber;

// Set the random number from Go code.

void
setRandomNumber(uint32 r)
{
	randomNumber = r;
}

#if defined(__i386__) || defined(__x86_64__) || defined (__s390__) || defined (__s390x__)

// When cputicks is just asm instructions, skip the split stack
// prologue for speed.

int64 runtime_cputicks(void) __attribute__((no_split_stack));

#endif

// Whether the processor supports SSE2.
#if defined (__i386__)
static _Bool hasSSE2;

// Force appropriate CPU level so that we can call the lfence/mfence
// builtins.

#pragma GCC push_options
#pragma GCC target("sse2")

#elif defined(__x86_64__)
#define hasSSE2 true
#endif

#if defined(__i386__) || defined(__x86_64__)
// Whether to use lfence, as opposed to mfence.
// Set based on cpuid.
static _Bool lfenceBeforeRdtsc;
#endif // defined(__i386__) || defined(__x86_64__)

int64
runtime_cputicks(void)
{
#if defined(__i386__) || defined(__x86_64__)
  if (hasSSE2) {
    if (lfenceBeforeRdtsc) {
      __builtin_ia32_lfence();
    } else {
      __builtin_ia32_mfence();
    }
  }
  return __builtin_ia32_rdtsc();
#elif defined (__s390__) || defined (__s390x__)
  uint64 clock = 0;
  /* stckf may not write the return variable in case of a clock error, so make
     it read-write to prevent that the initialisation is optimised out.
     Note: Targets below z9-109 will crash when executing store clock fast, i.e.
     we don't support Go for machines older than that.  */
  asm volatile(".insn s,0xb27c0000,%0" /* stckf */ : "+Q" (clock) : : "cc" );
  return (int64)clock;
#else
  // Currently cputicks() is used in blocking profiler and to seed runtime·fastrand().
  // runtime·nanotime() is a poor approximation of CPU ticks that is enough for the profiler.
  // randomNumber provides better seeding of fastrand.
  return runtime_nanotime1() + randomNumber;
#endif
}

#if defined(__i386__)
#pragma GCC pop_options
#endif

void
runtime_signalstack(byte *p, uintptr n)
{
	stack_t st;

	st.ss_sp = p;
	st.ss_size = n;
	st.ss_flags = 0;
	if(p == nil)
		st.ss_flags = SS_DISABLE;
	if(sigaltstack(&st, nil) < 0)
		*(int *)0xf1 = 0xf1;
}

int32 go_open(char *, int32, int32)
  __asm__ (GOSYM_PREFIX "runtime.open");

int32
go_open(char *name, int32 mode, int32 perm)
{
  return runtime_open(name, mode, perm);
}

int32 go_read(int32, void *, int32)
  __asm__ (GOSYM_PREFIX "runtime.read");

int32
go_read(int32 fd, void *p, int32 n)
{
  ssize_t r = runtime_read(fd, p, n);
  if (r < 0)
    r = - errno;
  return (int32)r;
}

int32 go_write1(uintptr, void *, int32)
  __asm__ (GOSYM_PREFIX "runtime.write1");

int32
go_write1(uintptr fd, void *p, int32 n)
{
  ssize_t r = runtime_write(fd, p, n);
  if (r < 0)
    r = - errno;
  return (int32)r;
}

int32 go_closefd(int32)
  __asm__ (GOSYM_PREFIX "runtime.closefd");

int32
go_closefd(int32 fd)
{
  return runtime_close(fd);
}

intgo go_errno(void)
  __asm__ (GOSYM_PREFIX "runtime.errno");

intgo
go_errno()
{
  return (intgo)errno;
}

uintptr getEnd(void)
  __asm__ (GOSYM_PREFIX "runtime.getEnd");

uintptr
getEnd()
{
#ifdef _AIX
  // mmap adresses range start at 0x30000000 on AIX for 32 bits processes
  uintptr end = 0x30000000U;
#else
  uintptr end = 0;
  uintptr *pend;

  pend = &__go_end;
  if (pend != nil) {
    end = *pend;
  }
#endif

  return end;
}

// Return an address that is before the read-only data section.
// Unfortunately there is no standard symbol for this so we use a text
// address.

uintptr getText(void)
  __asm__ (GOSYM_PREFIX "runtime.getText");

uintptr
getText(void)
{
  return (uintptr)(const void *)(getText);
}

// Return the end of the text segment, assumed to come after the
// read-only data section.

uintptr getEtext(void)
  __asm__ (GOSYM_PREFIX "runtime.getEtext");

uintptr
getEtext(void)
{
  const void *p;

  p = __data_start;
  if (p == nil)
    p = __etext;
  if (p == nil)
    p = _etext;
  return (uintptr)(p);
}

// CPU-specific initialization.
// Fetch CPUID info on x86.

void
runtime_cpuinit()
{
#if defined(__i386__) || defined(__x86_64__)
	unsigned int eax, ebx, ecx, edx;

	if (__get_cpuid(0, &eax, &ebx, &ecx, &edx)) {
		if (eax != 0
		    && ebx == 0x756E6547    // "Genu"
		    && edx == 0x49656E69    // "ineI"
		    && ecx == 0x6C65746E) { // "ntel"
			lfenceBeforeRdtsc = true;
		}
	}
	if (__get_cpuid(1, &eax, &ebx, &ecx, &edx)) {
#if defined(__i386__)
		if ((edx & bit_SSE2) != 0) {
			hasSSE2 = true;
		}
#endif
	}

#if defined(HAVE_AS_X86_AES)
	setSupportAES(true);
#endif
#endif
}

// A publication barrier: a store/store barrier.

void publicationBarrier(void)
  __asm__ (GOSYM_PREFIX "runtime.publicationBarrier");

void
publicationBarrier()
{
  __atomic_thread_fence(__ATOMIC_RELEASE);
}

#ifdef __linux__

/* Currently sbrk0 is only called on GNU/Linux.  */

uintptr sbrk0(void)
  __asm__ (GOSYM_PREFIX "runtime.sbrk0");

uintptr
sbrk0()
{
  return syscall(SYS_brk, (uintptr)(0));
}

#endif /* __linux__ */
