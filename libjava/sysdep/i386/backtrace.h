// backtrace.h - Fallback backtrace implementation. i386 implementation.

/* Copyright (C) 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_BACKTRACE_H__
#define __SYSDEP_BACKTRACE_H__

#include <java-stack.h>

#ifdef __CYGWIN__
/* To allow this to link as a DLL.  */
#define MAIN_FUNC dll_crt0__FP11per_process
extern "C" int MAIN_FUNC () __declspec(dllimport);
#elif defined (_WIN32)
#define MAIN_FUNC DllMain
extern "C" int __stdcall MAIN_FUNC (void *, unsigned long, void *);
#else /* !__CYGWIN__ && !_WIN32 */
#define MAIN_FUNC main
extern int MAIN_FUNC (int, char **);
#endif /* ?__CYGWIN__ */

/* The context used to keep track of our position while unwinding through
   the call stack.  */
struct _Unwind_Context
{
  /* The starting address of the method.  */
  _Jv_uintptr_t meth_addr;

  /* The return address in the method.  */
  _Jv_uintptr_t ret_addr;
};

#ifdef SJLJ_EXCEPTIONS

#undef _Unwind_GetIPInfo
#define _Unwind_GetIPInfo(ctx,ip_before_insn) \
  (*(ip_before_insn) = 1, (ctx)->ret_addr)

#undef _Unwind_GetRegionStart
#define _Unwind_GetRegionStart(ctx) \
  ((ctx)->meth_addr)

#undef _Unwind_Backtrace
#define _Unwind_Backtrace(trace_fn,state_ptr) \
  (fallback_backtrace (trace_fn, state_ptr))

#endif /* SJLJ_EXCEPTIONS */

/* Unwind through the call stack calling TRACE_FN with STATE for each stack
   frame.  Returns the reason why the unwinding was stopped.  */
_Unwind_Reason_Code
fallback_backtrace (_Unwind_Trace_Fn trace_fn, _Jv_UnwindState *state)
{
  register _Jv_uintptr_t *_ebp __asm__ ("ebp");
  register _Jv_uintptr_t _esp __asm__ ("esp");
  _Jv_uintptr_t rfp;
  _Unwind_Context ctx;

  for (rfp = *_ebp; rfp; rfp = *(_Jv_uintptr_t *)rfp)
    {
      /* Sanity checks to eliminate dubious-looking frame pointer chains.
         The frame pointer should be a 32-bit word-aligned stack address.
         Since the stack grows downwards on x86, the frame pointer must have
         a value greater than the current value of the stack pointer, it
         should not be below the supposed next frame pointer and it should
         not be too far off from the supposed next frame pointer.  */
      int diff = *(_Jv_uintptr_t *)rfp - rfp;
      if ((rfp & 0x00000003) != 0 || rfp < _esp
          || diff > 4 * 1024 || diff < 0)
        break;

      /* Get the return address in the calling function.  This is stored on
         the stack just before the value of the old frame pointer.  */
      ctx.ret_addr = *(_Jv_uintptr_t *)(rfp + sizeof (_Jv_uintptr_t));

      /* Try to locate a "pushl %ebp; movl %esp, %ebp" function prologue
         by scanning backwards at even addresses below the return address.
         This instruction sequence is encoded either as 0x55 0x89 0xE5 or as
         0x55 0x8B 0xEC.  We give up if we do not find this sequence even
         after scanning 1024K of memory.
         FIXME: This is not robust and will probably give us false positives,
         but this is about the best we can do if we do not have DWARF-2 unwind
         information based exception handling.  */
      ctx.meth_addr = (_Jv_uintptr_t)NULL;
      _Jv_uintptr_t scan_addr = (ctx.ret_addr & 0xFFFFFFFE) - 2;
      _Jv_uintptr_t limit_addr
        = (scan_addr > 1024 * 1024) ? (scan_addr - 1024 * 1024) : 2;
      for ( ; scan_addr >= limit_addr; scan_addr -= 2)
        {
          unsigned char *scan_bytes = (unsigned char *)scan_addr;
          if (scan_bytes[0] == 0x55
              && ((scan_bytes[1] == 0x89 && scan_bytes[2] == 0xE5)
                  || (scan_bytes[1] == 0x8B && scan_bytes[2] == 0xEC)))
            {
              ctx.meth_addr = scan_addr;
              break;
            }
        }

      /* Now call the unwinder callback function. */
      if (trace_fn != NULL)
        (*trace_fn) (&ctx, state);

      /* No need to unwind beyond _Jv_RunMain(), _Jv_ThreadStart or
         main().  */
      void *jv_runmain
        = (void *)(void (*)(JvVMInitArgs *, jclass, const char *, int,
                            const char **, bool))_Jv_RunMain;
      if (ctx.meth_addr == (_Jv_uintptr_t)jv_runmain
          || ctx.meth_addr == (_Jv_uintptr_t)_Jv_ThreadStart
          || (ctx.meth_addr - (_Jv_uintptr_t)MAIN_FUNC) < 16)
        break;
    }

  return _URC_NO_REASON;
}
#endif
