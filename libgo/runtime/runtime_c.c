// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <errno.h>
#include <signal.h>
#include <unistd.h>

#if defined(__i386__) || defined(__x86_64__)
#include <cpuid.h>
#endif

#include "config.h"

#include "runtime.h"
#include "arch.h"
#include "array.h"

enum {
	maxround = sizeof(uintptr),
};

extern volatile intgo runtime_MemProfileRate
  __asm__ (GOSYM_PREFIX "runtime.MemProfileRate");

struct gotraceback_ret {
	int32 level;
	bool  all;
	bool  crash;
};

extern struct gotraceback_ret gotraceback(void)
  __asm__ (GOSYM_PREFIX "runtime.gotraceback");

// runtime_gotraceback is the C interface to runtime.gotraceback.
int32
runtime_gotraceback(bool *crash)
{
	struct gotraceback_ret r;

	r = gotraceback();
	if(crash != nil)
		*crash = r.crash;
	return r.level;
}

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

uint32
runtime_fastrand(void)
{
	M *m;
	uint32 x;

	m = runtime_m();
	x = m->fastrand;
	x += x;
	if(x & 0x80000000L)
		x ^= 0x88888eefUL;
	m->fastrand = x;
	return x;
}

int64
runtime_cputicks(void)
{
#if defined(__386__) || defined(__x86_64__)
  uint32 low, high;
  asm("rdtsc" : "=a" (low), "=d" (high));
  return (int64)(((uint64)high << 32) | (uint64)low);
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
  // TODO: need more entropy to better seed fastrand.
  return runtime_nanotime();
#endif
}

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

struct debugVars	runtime_debug;

void
runtime_setdebug(struct debugVars* d) {
  runtime_debug = *d;
}

void memclrBytes(Slice)
     __asm__ (GOSYM_PREFIX "runtime.memclrBytes");

void
memclrBytes(Slice s)
{
	runtime_memclr(s.__values, s.__count);
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
  return runtime_read(fd, p, n);
}

int32 go_write(uintptr, void *, int32)
  __asm__ (GOSYM_PREFIX "runtime.write");

int32
go_write(uintptr fd, void *p, int32 n)
{
  return runtime_write(fd, p, n);
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

// CPU-specific initialization.
// Fetch CPUID info on x86.

void
runtime_cpuinit()
{
#if defined(__i386__) || defined(__x86_64__)
	unsigned int eax, ebx, ecx, edx;

	if (__get_cpuid(1, &eax, &ebx, &ecx, &edx)) {
		setCpuidECX(ecx);
	}

#if defined(HAVE_AS_X86_AES)
	setSupportAES(true);
#endif
#endif
}
