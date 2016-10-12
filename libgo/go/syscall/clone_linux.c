/* clone_linux.c -- consistent wrapper around Linux clone syscall

   Copyright 2016 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <errno.h>
#include <asm/ptrace.h>
#include <sys/syscall.h>

#include "runtime.h"

long rawClone (unsigned long flags, void *child_stack, void *ptid,
	       void *ctid, struct pt_regs *regs)
  __asm__ (GOSYM_PREFIX "syscall.rawClone")
  __attribute__ ((no_split_stack));

long
rawClone (unsigned long flags, void *child_stack, void *ptid, void *ctid, struct pt_regs *regs)
{
#if defined(__arc__) || defined(__aarch64__) || defined(__arm__) || defined(__mips__) || defined(__hppa__) || defined(__powerpc__) || defined(__score__) || defined(__i386__) || defined(__xtensa__)
  // CLONE_BACKWARDS
  return syscall(__NR_clone, flags, child_stack, ptid, regs, ctid);
#elif defined(__s390__) || defined(__cris__)
  // CLONE_BACKWARDS2
  return syscall(__NR_clone, child_stack, flags, ptid, ctid, regs);
#elif defined(__microblaze__)
  // CLONE_BACKWARDS3
  return syscall(__NR_clone, flags, child_stack, 0, ptid, ctid, regs);
#elif defined(__sparc__)

  /* SPARC has a unique return value convention:

     Parent -->  %o0 == child's  pid, %o1 == 0
     Child  -->  %o0 == parent's pid, %o1 == 1

     Translate this to look like a normal clone.  */

# if defined(__arch64__)

#  define SYSCALL_STRING						\
	"ta	0x6d;"							\
	"bcc,pt	%%xcc, 1f;"						\
	" mov	0, %%g1;"						\
	"sub	%%g0, %%o0, %%o0;"					\
	"mov	1, %%g1;"						\
	"1:"

#  define SYSCALL_CLOBBERS						\
	"f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",			\
	"f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",		\
	"f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",		\
	"f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",		\
	"f32", "f34", "f36", "f38", "f40", "f42", "f44", "f46",		\
	"f48", "f50", "f52", "f54", "f56", "f58", "f60", "f62",		\
	"cc", "memory"

# else /* __arch64__ */

#  define SYSCALL_STRING						\
	"ta	0x10;"							\
	"bcc	1f;"							\
	" mov	0, %%g1;"						\
	"sub	%%g0, %%o0, %%o0;"					\
	"mov	1, %%g1;"						\
	"1:"

#  define SYSCALL_CLOBBERS						\
	"f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",			\
	"f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",		\
	"f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",		\
	"f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",		\
	"cc", "memory"

# endif /* __arch64__ */

  register long o0 __asm__ ("o0") = (long)flags;
  register long o1 __asm__ ("o1") = (long)child_stack;
  register long o2 __asm__ ("o2") = (long)ptid;
  register long o3 __asm__ ("o3") = (long)ctid;
  register long o4 __asm__ ("o4") = (long)regs;
  register long g1 __asm__ ("g1") = __NR_clone;

  __asm __volatile (SYSCALL_STRING :
		    "=r" (g1), "=r" (o0), "=r" (o1) :
		    "0" (g1), "1" (o0), "2" (o1),
		    "r" (o2), "r" (o3), "r" (o4) :
		    SYSCALL_CLOBBERS);

  if (__builtin_expect(g1 != 0, 0))
    {
      errno = -o0;
      o0 = -1L;
    }
  else
    o0 &= (o1 - 1);

  return o0;

#else
  return syscall(__NR_clone, flags, child_stack, ptid, ctid, regs);
#endif
}
