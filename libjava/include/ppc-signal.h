// ppc-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>
#include <ucontext.h>

#define HANDLE_SEGV 1
#undef HANDLE_FPE

#define SIGNAL_HANDLER(_name)			\
__asm ("\t.section\t\".text\"\n"		\
       "\t.align 2\n"				\
       "_Jv_" #_name ":\n"			\
       "\tmr 3, 1\n"				\
       "\tb _Jv_stub_" #_name "\n");		\
extern "C" void _Jv_##_name (int _dummy);	\
extern "C" void _Jv_stub_##_name (char *_sp)	\

class java::lang::Throwable;

// Unwind the stack to the point at which the signal was generated and
// then throw an exception.

#define MAKE_THROW_FRAME(_exception)					\
do									\
{									\
  struct sigcontext_struct *_context					\
    = (struct sigcontext_struct *)(_sp + __SIGNAL_FRAMESIZE);		\
  long int regs [34];							\
  memcpy (regs, &_context->regs->gpr[0], 32 * sizeof (long int));	\
  regs[32] = _context->regs->nip + sizeof (long int);			\
  regs[33] = _context->regs->link;					\
									\
  __asm volatile (							\
	"\tmr 31,%0\n"							\
        "\tmr 3,%1              # exception to throw\n"			\
        "\tlwz 0,128(31)        # ip\n"					\
        "\tmtlr 0\n"							\
        "\tlwz 1,4(31)          # previous r1\n"			\
        "\tlwz 0,132(31)        # previous lr\n"			\
        "\tlwz 2,0(1)           # previous previous r1\n"		\
        "\tstw 0,4(2)           # save previous lr\n"			\
	"\tlwz 0,0(31)\n"						\
	"\tlwz 2,8(31)\n"						\
	"\tlwz 4,16(31)\n"						\
	"\tlwz 5,20(31)\n"						\
	"\tlwz 6,24(31)\n"						\
	"\tlwz 7,28(31)\n"						\
	"\tlwz 8,32(31)\n"						\
	"\tlwz 9,36(31)\n"						\
	"\tlwz 10,40(31)\n"						\
	"\tlwz 11,44(31)\n"						\
	"\tlwz 12,48(31)\n"						\
	"\tlwz 13,52(31)\n"						\
	"\tlwz 14,56(31)\n"						\
	"\tlwz 15,60(31)\n"						\
	"\tlwz 16,64(31)\n"						\
	"\tlwz 17,68(31)\n"						\
	"\tlwz 18,72(31)\n"						\
	"\tlwz 19,76(31)\n"						\
	"\tlwz 20,80(31)\n"						\
	"\tlwz 21,84(31)\n"						\
	"\tlwz 22,88(31)\n"						\
	"\tlwz 23,92(31)\n"						\
	"\tlwz 24,96(31)\n"						\
	"\tlwz 25,100(31)\n"						\
	"\tlwz 26,104(31)\n"						\
	"\tlwz 27,108(31)\n"						\
	"\tlwz 28,112(31)\n"						\
	"\tlwz 29,116(31)\n"						\
	"\tlwz 30,120(31)\n"						\
	"\tlwz 31,124(31)\n"						\
	"\tb _Jv_ThrowSignal\n"						\
		  : : "r"(regs), "r"(_exception)	       		\
		  : "r31", "r3");					\
}									\
while (0)  


#define INIT_SEGV						\
do								\
  {								\
    nullp = new java::lang::NullPointerException ();    	\
    struct sigaction act;					\
    act.sa_handler = _Jv_catch_segv;				\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = 0;						\
    __sigaction (SIGSEGV, &act, NULL);				\
  }								\
while (0)  

#define INIT_FPE						\
do								\
  { 								\
    arithexception = new java::lang::ArithmeticException 	\
      (JvNewStringLatin1 ("/ by zero"));			\
    struct sigaction act;					\
    act.sa_handler = _Jv_catch_fpe;				\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = 0;						\
    __sigaction (SIGFPE, &act, NULL);				\
  }								\
while (0)  

#endif /* JAVA_SIGNAL_H */
