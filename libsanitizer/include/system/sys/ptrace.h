#include_next <sys/ptrace.h>
#ifndef PTRACE_GETREGSET
/* glibc before
   https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=cbff0d9689c4d68578b6a4f0a17807232506ea27
   doesn't define PTRACE_GETREGSET.  */
#define PTRACE_GETREGSET 0x4204
#endif
