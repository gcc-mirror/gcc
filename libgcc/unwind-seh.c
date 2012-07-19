/* Structured Exception Handling (SEH) runtime interface routines.
   Copyright (C) 2010  Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "unwind.h"

#ifdef __SEH__

/* At the moment everything is written for x64, but in theory this could
   also be used for i386, arm, mips and other extant embedded Windows.  */
#ifndef __x86_64__
#error "Unsupported architecture."
#endif

/* Define GCC's exception codes.  See
     http://msdn.microsoft.com/en-us/library/het71c37(v=VS.80).aspx
   In particular, MS defines bits:
     [31:30] = 3 (error), 2 (warning), 1 (info), 0 (success)
     [29]    = 1 (user-defined)
     [28]    = 0 (reserved)
   We define bits:
     [24:27] = type
     [0:23]  = magic
   We set "magic" to "GCC", which is similar to MVC++ which uses "msc"
   as the low 3 bytes of its user-defined codes for C++ exceptions.

   We define the ExceptionInformation entries as follows:
     [0] = _Unwind_Exception pointer
     [1] = target frame
     [2] = target ip
     [3] = target rdx
*/

#define STATUS_USER_DEFINED		(1U << 29)

#define GCC_MAGIC			(('G' << 16) | ('C' << 8) | 'C')
#define GCC_EXCEPTION(TYPE)		\
       (STATUS_USER_DEFINED | ((TYPE) << 24) | GCC_MAGIC)

#define STATUS_GCC_THROW		GCC_EXCEPTION (0)
#define STATUS_GCC_UNWIND		GCC_EXCEPTION (1)
#define STATUS_GCC_FORCED		GCC_EXCEPTION (2)


struct _Unwind_Context
{
  _Unwind_Word cfa;
  _Unwind_Word ra;
  _Unwind_Word reg[2];
  PDISPATCHER_CONTEXT disp;
};

/* Get the value of register INDEX as saved in CONTEXT.  */

_Unwind_Word
_Unwind_GetGR (struct _Unwind_Context *c, int index)
{
  if (index < 0 || index > 2)
    abort ();
  return c->reg[index];
}

/* Overwrite the saved value for register INDEX in CONTEXT with VAL.  */

void
_Unwind_SetGR (struct _Unwind_Context *c, int index, _Unwind_Word val)
{
  if (index < 0 || index > 2)
    abort ();
  c->reg[index] = val;
}

/* Get the value of the CFA as saved in CONTEXT.  */

_Unwind_Word
_Unwind_GetCFA (struct _Unwind_Context *c)
{
  return c->cfa;
}

/* Retrieve the return address for CONTEXT.  */

_Unwind_Ptr
_Unwind_GetIP (struct _Unwind_Context *c)
{
  return c->ra;
}

/* Retrieve the return address and flag whether that IP is before
   or after first not yet fully executed instruction.  */

_Unwind_Ptr
_Unwind_GetIPInfo (struct _Unwind_Context *c, int *ip_before_insn)
{
  /* ??? Is there a concept of a signal context properly?  There's
     obviously an UNWP_PUSH_MACHFRAME opcode, but the runtime might
     have arranged for that not to matter, really.  */
  *ip_before_insn = 0;
  return c->ra;
}

/* Overwrite the return address for CONTEXT with VAL.  */

void
_Unwind_SetIP (struct _Unwind_Context *c, _Unwind_Ptr val)
{
  c->ra = val;
}

void *
_Unwind_GetLanguageSpecificData (struct _Unwind_Context *c)
{
  return c->disp->HandlerData;
}

_Unwind_Ptr
_Unwind_GetRegionStart (struct _Unwind_Context *c)
{
  return c->disp->FunctionEntry->BeginAddress + c->disp->ImageBase;
}

void *
_Unwind_FindEnclosingFunction (void *pc)
{
  PRUNTIME_FUNCTION entry;
  ULONG64 ImageBase;

  entry = RtlLookupFunctionEntry ((ULONG64)pc, &ImageBase, NULL);

  return (entry ? (void *)(entry->BeginAddress + ImageBase) : NULL);
}

_Unwind_Ptr
_Unwind_GetDataRelBase (struct _Unwind_Context *c ATTRIBUTE_UNUSED)
{
  return 0;
}

_Unwind_Ptr
_Unwind_GetTextRelBase (struct _Unwind_Context *c)
{
  return c->disp->ImageBase;
}


/* The two-phase unwind process that GCC uses is ordered differently
   from the two-phase unwind process that SEH uses.  The mechansism
   that GCC uses is to have the filter return _URC_HANDER_FOUND; the
   mechanism that SEH uses is for the filter function call back into
   the unwinder.

   An Ideal port to SEH would have GCC emit handler functions that
   can be called, given a pointer to the "EstablisherFrame" (i.e.
   the frame pointer base of the user-level function) can manipulate
   the user-level variables within the user-level function's stack
   frame.  Once done manipulating the variables, it would return
   a ExceptionContinueSearch, and the unwind process would continue.

   GCC has always done things a bit differently.  We continue to
   transfer control back into the user-level function which, once
   done manipulating the user-level variables, re-throws the exception.  */

/* The "real" language-specific personality handler forwards to here
   where we handle the MS SEH state and transforms it into the GCC
   unwind state as per GCC's <unwind.h>, at which point we defer to
   the regular language-specfic exception handler, which is passed in.  */

EXCEPTION_DISPOSITION
_GCC_specific_handler (PEXCEPTION_RECORD ms_exc, void *this_frame,
		       PCONTEXT ms_orig_context, PDISPATCHER_CONTEXT ms_disp,
		       _Unwind_Personality_Fn gcc_per)
{
  DWORD ms_flags = ms_exc->ExceptionFlags;
  DWORD ms_code = ms_exc->ExceptionCode;

  struct _Unwind_Exception *gcc_exc
    = (struct _Unwind_Exception *) ms_exc->ExceptionInformation[0];
  struct _Unwind_Context gcc_context;
  _Unwind_Action gcc_action;
  _Unwind_Reason_Code gcc_reason;

  if (ms_flags & EXCEPTION_TARGET_UNWIND)
    {
      /* This frame is known to be the target frame.  We've already
         "installed" the target_ip and RAX value via the arguments
         to RtlUnwindEx.  All that's left is to set the RDX value
         and "continue" to have the context installed.  */
      ms_disp->ContextRecord->Rdx = ms_exc->ExceptionInformation[3];
      return ExceptionContinueSearch;
    }

  if (ms_code == STATUS_GCC_UNWIND)
    {
      /* This is a colliding exception that we threw so that we could
         cancel the already in-flight exception and stop in a frame
	 that wanted to perform some unwind action.  The only relevant
	 test is that we're the target frame.  */
      if (ms_exc->ExceptionInformation[1] == (_Unwind_Ptr) this_frame)
	{
	  RtlUnwindEx (this_frame, ms_exc->ExceptionInformation[2],
		       ms_exc, gcc_exc, ms_orig_context,
		       ms_disp->HistoryTable);
	  abort ();
	}
      return ExceptionContinueSearch;
    }

  gcc_context.cfa = ms_disp->ContextRecord->Rsp;
  gcc_context.ra = ms_disp->ControlPc;
  gcc_context.reg[0] = 0xdeadbeef;	/* These are write-only.  */
  gcc_context.reg[1] = 0xdeadbeef;
  gcc_context.disp = ms_disp;

  if (ms_code == STATUS_GCC_FORCED)
    {
       _Unwind_Stop_Fn stop = (_Unwind_Stop_Fn) gcc_exc->private_[0];
       void *stop_argument = (void *) gcc_exc->private_[4];

       gcc_action = _UA_FORCE_UNWIND | _UA_CLEANUP_PHASE;

       stop (1, gcc_action, gcc_exc->exception_class, gcc_exc,
             &gcc_context, stop_argument);

       goto phase2;
    }

  /* ??? TODO: handling non-gcc user-defined exceptions as foreign.  */
  if (ms_code != STATUS_GCC_THROW)
    return ExceptionContinueSearch;

  if (ms_flags & (EXCEPTION_UNWINDING | EXCEPTION_EXIT_UNWIND))
    {
      /* This is Phase 2.  */
      /* We know this isn't the target frame because we've already tested
	 EXCEPTION_TARGET_UNWIND.  The remaining possibility is that the
	 gcc personality has unwind code to run.  */

      gcc_action = _UA_CLEANUP_PHASE;
    phase2:
      gcc_reason = gcc_per (1, gcc_action, gcc_exc->exception_class,
			    gcc_exc, &gcc_context);

      if (gcc_reason == _URC_CONTINUE_UNWIND)
	return ExceptionContinueSearch;

      if (gcc_reason == _URC_INSTALL_CONTEXT)
	{
	  /* Scratch space for the bits for the unwind catch.  */
	  ms_exc->ExceptionInformation[1] = (_Unwind_Ptr) this_frame;
	  ms_exc->ExceptionInformation[2] = gcc_context.ra;
	  ms_exc->ExceptionInformation[3] = gcc_context.reg[1];

	  /* Cancel the current exception by raising another.  */
	  RaiseException (STATUS_GCC_UNWIND, EXCEPTION_NONCONTINUABLE,
			  4, ms_exc->ExceptionInformation);

	  /* Is RaiseException declared noreturn?  */
	}

      /* In _Unwind_RaiseException_Phase2 we return _URC_FATAL_PHASE2_ERROR. */
    }
  else
    {
      /* This is Phase 1.  */
      gcc_reason = gcc_per (1, _UA_SEARCH_PHASE, gcc_exc->exception_class,
			    gcc_exc, &gcc_context);

      if (gcc_reason == _URC_CONTINUE_UNWIND)
	return ExceptionContinueSearch;

      if (gcc_reason == _URC_HANDLER_FOUND)
	{
	  /* We really need some of the information that GCC's personality
	     routines compute during phase 2 right now, like the target IP.
	     Go ahead and ask for it now, and cache it.  */
	  gcc_reason = gcc_per (1, _UA_CLEANUP_PHASE | _UA_HANDLER_FRAME,
				gcc_exc->exception_class, gcc_exc,
				&gcc_context);
	  if (gcc_reason != _URC_INSTALL_CONTEXT)
	    abort ();

	  gcc_exc->private_[1] = (_Unwind_Ptr) this_frame;
	  gcc_exc->private_[2] = gcc_context.ra;
	  gcc_exc->private_[3] = gcc_context.reg[1];

	  ms_exc->NumberParameters = 4;
	  ms_exc->ExceptionInformation[1] = (_Unwind_Ptr) this_frame;
	  ms_exc->ExceptionInformation[2] = gcc_context.ra;
	  ms_exc->ExceptionInformation[3] = gcc_context.reg[1];

	  /* Begin phase 2.  Perform the unwinding.  */
	  RtlUnwindEx (this_frame, gcc_context.ra, ms_exc, gcc_exc,
		       ms_orig_context, ms_disp->HistoryTable);
	}

      /* In _Unwind_RaiseException we return _URC_FATAL_PHASE1_ERROR.  */
    }
  abort ();
}

/* Raise an exception, passing along the given exception object.  */

_Unwind_Reason_Code
_Unwind_RaiseException (struct _Unwind_Exception *exc)
{
  memset (exc->private_, 0, sizeof (exc->private_));

  /* The ExceptionInformation array will have only 1 element, EXC.  */
  RaiseException (STATUS_GCC_THROW, 0, 1, (ULONG_PTR *)&exc);

  /* The exception handler installed in crt0 will continue any GCC
     exception that reaches there (and isn't marked non-continuable).
     Returning allows the C++ runtime to call std::terminate.  */
  return _URC_END_OF_STACK;
}

/* Resume propagation of an existing exception.  This is used after
   e.g. executing cleanup code, and not to implement rethrowing.  */

void
_Unwind_Resume (struct _Unwind_Exception *gcc_exc)
{
  UNWIND_HISTORY_TABLE ms_history;
  EXCEPTION_RECORD ms_exc;
  CONTEXT ms_context;

  memset (&ms_exc, 0, sizeof(ms_exc));
  memset (&ms_history, 0, sizeof(ms_history));

  /* ??? Not 100% perfect, since we aren't passing on the *original*
     exception context, but should be good enough.  */
  ms_exc.ExceptionCode = STATUS_GCC_THROW;
  ms_exc.ExceptionFlags = EXCEPTION_NONCONTINUABLE;
  ms_exc.NumberParameters = 4;
  ms_exc.ExceptionInformation[0] = (ULONG_PTR) gcc_exc;
  ms_exc.ExceptionInformation[1] = gcc_exc->private_[1];
  ms_exc.ExceptionInformation[2] = gcc_exc->private_[2];
  ms_exc.ExceptionInformation[3] = gcc_exc->private_[3];

  ms_context.ContextFlags = CONTEXT_ALL;
  RtlCaptureContext (&ms_context);

  RtlUnwindEx ((void *) gcc_exc->private_[1], gcc_exc->private_[2],
	       &ms_exc, gcc_exc, &ms_context, &ms_history);

  /* Is RtlUnwindEx declared noreturn?  */
  abort ();
}

static _Unwind_Reason_Code
_Unwind_ForcedUnwind_Phase2 (struct _Unwind_Exception *exc)
{
  _Unwind_Stop_Fn stop;
  void * stop_argument;

  RaiseException (STATUS_GCC_FORCED, 0, 1, (ULONG_PTR *)&exc);

  /* If we get here, we got to top-of-stack.  */
  /* ??? We no longer have a context pointer to pass in.  */

  stop = (_Unwind_Stop_Fn) exc->private_[0];
  stop_argument = (void *) exc->private_[4];
  stop (1, _UA_FORCE_UNWIND | _UA_CLEANUP_PHASE | _UA_END_OF_STACK,
	exc->exception_class, exc, NULL, stop_argument);

  return _UA_END_OF_STACK;
}

_Unwind_Reason_Code
_Unwind_Resume_or_Rethrow (struct _Unwind_Exception *exc)
{
  if (exc->private_[0] == 0)
    _Unwind_RaiseException (exc);
  else
    _Unwind_ForcedUnwind_Phase2 (exc);
  abort ();
}

/* Raise an exception for forced unwinding.  */

_Unwind_Reason_Code
_Unwind_ForcedUnwind (struct _Unwind_Exception *exc,
		      _Unwind_Stop_Fn stop, void * stop_argument)
{
  /* ??? This is a hack that only works with _GCC_specific_handler.
     There's no way to invoke STOP within frames that use a different
     exception handler.  This is essentially just good enough to run
     the code within the gcc testsuite.  */

  memset (exc->private_, 0, sizeof (exc->private_));
  exc->private_[0] = (_Unwind_Ptr) stop;
  exc->private_[4] = (_Unwind_Ptr) stop_argument;

  return _Unwind_ForcedUnwind_Phase2 (exc);
}

/* A convenience function that calls the exception_cleanup field.  */

void
_Unwind_DeleteException (struct _Unwind_Exception *exc)
{
  if (exc->exception_cleanup)
    (*exc->exception_cleanup) (_URC_FOREIGN_EXCEPTION_CAUGHT, exc);
}

/* Perform stack backtrace through unwind data.  */

_Unwind_Reason_Code
_Unwind_Backtrace(_Unwind_Trace_Fn trace ATTRIBUTE_UNUSED,
		  void *trace_argument ATTRIBUTE_UNUSED)
{
#if 0
  UNWIND_HISTORY_TABLE ms_history;
  CONTEXT ms_context;
  struct _Unwind_Context gcc_context;

  memset (&ms_history, 0, sizeof(ms_history));
  memset (&gcc_context, 0, sizeof(gcc_context));

  ms_context.ContextFlags = CONTEXT_ALL;
  RtlCaptureContext (&ms_context);

  gcc_context.disp.ContextRecord = &ms_context;
  gcc_context.disp.HistoryTable = &ms_history;

  while (1)
    {
      gcc_context.disp.ControlPc = ms_context.Rip;
      gcc_context.disp.FunctionEntry
	= RtlLookupFunctionEntry (ms_context.Rip, &gcc_context.disp.ImageBase,
				  &ms_history);

      if (gcc_context.disp.FunctionEntry)
	{
	  gcc_context.disp.LanguageHandler
	    = RtlVirtualUnwind (0, gcc_context.disp.ImageBase, ms_context.Rip,
				gcc_context.disp.FunctionEntry, &ms_context,
				&gcc_context.disp.HandlerData,
				&gcc_context.disp.EstablisherFrame, NULL);
	}
      else
	{
	  ms_context.Rip = *(ULONG_PTR *)ms_context.Rsp;
	  ms_context.Rsp += 8;
	}

      /* Call trace function.  */
      if (trace (&gcc_context, trace_argument) != _URC_NO_REASON)
	return _URC_FATAL_PHASE1_ERROR;

      /* ??? Check for invalid stack pointer.  */
      if (ms_context.Rip == 0)
	return _URC_END_OF_STACK;
    }
#else
  return _URC_END_OF_STACK;
#endif
}
#endif /* __SEH__ */
