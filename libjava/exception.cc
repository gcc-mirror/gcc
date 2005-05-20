// Functions for Exception Support for Java.

/* Copyright (C) 1998, 1999, 2001, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stddef.h>
#include <stdlib.h>

#include <java/lang/Class.h>
#include <java/lang/NullPointerException.h>
#include <gnu/gcj/RawData.h> 
#include <gcj/cni.h>
#include <jvm.h>

// unwind-pe.h uses std::abort(), but sometimes we compile libjava
// without libstdc++-v3. The following hack forces it to use
// stdlib.h's abort().
namespace std
{
  static __attribute__ ((__noreturn__)) void
  abort ()
  {
    ::abort ();
  }
}
#include "unwind.h"

struct alignment_test_struct
{
  char space;
  char end[0] __attribute__((aligned));
};

struct java_exception_header
{
  /* Cache handler details between Phase 1 and Phase 2.  */
  _Unwind_Ptr landingPad;
  int handlerSwitchValue;

  /* The object being thrown.  Compiled code expects this to be immediately
     before the generic exception header.  Which is complicated by the fact
     that _Unwind_Exception is ((aligned)).  */

  char pad[sizeof(jthrowable) < sizeof(alignment_test_struct)
	   ? sizeof(alignment_test_struct) - sizeof(jthrowable) : 0]
    __attribute__((aligned));

  jthrowable value;

  /* The generic exception header.  */
  _Unwind_Exception unwindHeader;
};

// This is the exception class we report -- "GNUCJAVA".
const _Unwind_Exception_Class __gcj_exception_class
= ((((((((_Unwind_Exception_Class) 'G' 
         << 8 | (_Unwind_Exception_Class) 'N')
        << 8 | (_Unwind_Exception_Class) 'U')
       << 8 | (_Unwind_Exception_Class) 'C')
      << 8 | (_Unwind_Exception_Class) 'J')
     << 8 | (_Unwind_Exception_Class) 'A')
    << 8 | (_Unwind_Exception_Class) 'V')
   << 8 | (_Unwind_Exception_Class) 'A');


static inline java_exception_header *
get_exception_header_from_ue (_Unwind_Exception *exc)
{
  return reinterpret_cast<java_exception_header *>(exc + 1) - 1;
}

/* Perform a throw, Java style. Throw will unwind through this call,
   so there better not be any handlers or exception thrown here. */

extern "C" void
_Jv_Throw (jthrowable value)
{
  java_exception_header *xh
    = static_cast<java_exception_header *>(_Jv_AllocRawObj (sizeof (*xh)));

  if (value == NULL)
    value = new java::lang::NullPointerException ();
  xh->value = value;

  xh->unwindHeader.exception_class = __gcj_exception_class;
  xh->unwindHeader.exception_cleanup = NULL;

  /* We're happy with setjmp/longjmp exceptions or region-based
     exception handlers: entry points are provided here for both.  */
  _Unwind_Reason_Code code;
#ifdef SJLJ_EXCEPTIONS
  code = _Unwind_SjLj_RaiseException (&xh->unwindHeader);
#else
  code = _Unwind_RaiseException (&xh->unwindHeader);
#endif

  /* If code == _URC_END_OF_STACK, then we reached top of stack without
     finding a handler for the exception.  Since each thread is run in
     a try/catch, this oughtn't happen.  If code is something else, we
     encountered some sort of heinous lossage from which we could not
     recover.  As is the way of such things, almost certainly we will have
     crashed before now, rather than actually being able to diagnose the
     problem.  */
  abort();
}


#include "unwind-pe.h"

struct lsda_header_info
{
  _Unwind_Ptr Start;
  _Unwind_Ptr LPStart;
  const unsigned char *TType;
  const unsigned char *action_table;
  unsigned char ttype_encoding;
  unsigned char call_site_encoding;
};

static const unsigned char *
parse_lsda_header (_Unwind_Context *context, const unsigned char *p,
		   lsda_header_info *info)
{
  _Unwind_Word tmp;
  unsigned char lpstart_encoding;

  info->Start = (context ? _Unwind_GetRegionStart (context) : 0);

  // Find @LPStart, the base to which landing pad offsets are relative.
  lpstart_encoding = *p++;
  if (lpstart_encoding != DW_EH_PE_omit)
    p = read_encoded_value (context, lpstart_encoding, p, &info->LPStart);
  else
    info->LPStart = info->Start;

  // Find @TType, the base of the handler and exception spec type data.
  info->ttype_encoding = *p++;
  if (info->ttype_encoding != DW_EH_PE_omit)
    {
      p = read_uleb128 (p, &tmp);
      info->TType = p + tmp;
    }
  else
    info->TType = 0;

  // The encoding and length of the call-site table; the action table
  // immediately follows.
  info->call_site_encoding = *p++;
  p = read_uleb128 (p, &tmp);
  info->action_table = p + tmp;

  return p;
}

static void **
get_ttype_entry (_Unwind_Context *context, lsda_header_info *info, long i)
{
  _Unwind_Ptr ptr;

  i *= size_of_encoded_value (info->ttype_encoding);
  read_encoded_value (context, info->ttype_encoding, info->TType - i, &ptr);

  return reinterpret_cast<void **>(ptr);
}


// Using a different personality function name causes link failures
// when trying to mix code using different exception handling models.
#ifdef SJLJ_EXCEPTIONS
#define PERSONALITY_FUNCTION	__gcj_personality_sj0
#define __builtin_eh_return_data_regno(x) x
#else
#define PERSONALITY_FUNCTION	__gcj_personality_v0
#endif

extern "C" _Unwind_Reason_Code
PERSONALITY_FUNCTION (int version,
		      _Unwind_Action actions,
		      _Unwind_Exception_Class exception_class,
		      struct _Unwind_Exception *ue_header,
		      struct _Unwind_Context *context)
{
  java_exception_header *xh = get_exception_header_from_ue (ue_header);

  lsda_header_info info;
  const unsigned char *language_specific_data;
  const unsigned char *action_record;
  const unsigned char *p;
  _Unwind_Ptr landing_pad, ip;
  int handler_switch_value;
  bool saw_cleanup;
  bool saw_handler;


  // Interface version check.
  if (version != 1)
    return _URC_FATAL_PHASE1_ERROR;

  // Shortcut for phase 2 found handler for domestic exception.
  if (actions == (_UA_CLEANUP_PHASE | _UA_HANDLER_FRAME)
      && exception_class == __gcj_exception_class)
    {
      handler_switch_value = xh->handlerSwitchValue;
      landing_pad = xh->landingPad;
      goto install_context;
    }

  // FIXME: In Phase 1, record _Unwind_GetIP in xh->obj as a part of
  // the stack trace for this exception.  This will only collect Java
  // frames, but perhaps that is acceptable.
  // FIXME2: _Unwind_GetIP is nonsensical for SJLJ, being a call-site
  // index instead of a PC value.  We could perhaps arrange for
  // _Unwind_GetRegionStart to return context->fc->jbuf[1], which
  // is the address of the handler label for __builtin_longjmp, but
  // there is no solution for DONT_USE_BUILTIN_SETJMP.

  language_specific_data = (const unsigned char *)
    _Unwind_GetLanguageSpecificData (context);

  // If no LSDA, then there are no handlers or cleanups.
  if (! language_specific_data)
    return _URC_CONTINUE_UNWIND;

  // Parse the LSDA header.
  p = parse_lsda_header (context, language_specific_data, &info);
  ip = _Unwind_GetIP (context) - 1;
  landing_pad = 0;
  action_record = 0;
  handler_switch_value = 0;

#ifdef SJLJ_EXCEPTIONS
  // The given "IP" is an index into the call-site table, with two
  // exceptions -- -1 means no-action, and 0 means terminate.  But
  // since we're using uleb128 values, we've not got random access
  // to the array.
  if ((int) ip <= 0)
    return _URC_CONTINUE_UNWIND;
  else
    {
      _Unwind_Word cs_lp, cs_action;
      do
	{
	  p = read_uleb128 (p, &cs_lp);
	  p = read_uleb128 (p, &cs_action);
	}
      while (--ip);

      // Can never have null landing pad for sjlj -- that would have
      // been indicated by a -1 call site index.
      landing_pad = cs_lp + 1;
      if (cs_action)
	action_record = info.action_table + cs_action - 1;
      goto found_something;
    }
#else
  // Search the call-site table for the action associated with this IP.
  while (p < info.action_table)
    {
      _Unwind_Ptr cs_start, cs_len, cs_lp;
      _Unwind_Word cs_action;

      // Note that all call-site encodings are "absolute" displacements.
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_start);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_len);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_lp);
      p = read_uleb128 (p, &cs_action);

      // The table is sorted, so if we've passed the ip, stop.
      if (ip < info.Start + cs_start)
	p = info.action_table;
      else if (ip < info.Start + cs_start + cs_len)
	{
	  if (cs_lp)
	    landing_pad = info.LPStart + cs_lp;
	  if (cs_action)
	    action_record = info.action_table + cs_action - 1;
	  goto found_something;
	}
    }
#endif // SJLJ_EXCEPTIONS

  // If ip is not present in the table, C++ would call terminate.
  // ??? It is perhaps better to tweek the LSDA so that no-action
  // is mapped to no-entry for Java.
  return _URC_CONTINUE_UNWIND;

 found_something:
  saw_cleanup = false;
  saw_handler = false;

  if (landing_pad == 0)
    {
      // If ip is present, and has a null landing pad, there are
      // no cleanups or handlers to be run.
    }
  else if (action_record == 0)
    {
      // If ip is present, has a non-null landing pad, and a null
      // action table offset, then there are only cleanups present.
      // Cleanups use a zero switch value, as set above.
      saw_cleanup = true;
    }
  else
    {
      // Otherwise we have a catch handler.
      _Unwind_Sword ar_filter, ar_disp;

      while (1)
	{
	  p = action_record;
	  p = read_sleb128 (p, &ar_filter);
	  read_sleb128 (p, &ar_disp);

	  if (ar_filter == 0)
	    {
	      // Zero filter values are cleanups.
	      saw_cleanup = true;
	    }

	  // During forced unwinding, we only run cleanups.  With a
	  // foreign exception class, we have no class info to match.
	  else if ((actions & _UA_FORCE_UNWIND)
	      || exception_class != __gcj_exception_class)
	    ;

	  else if (ar_filter > 0)
	    {
	      // Positive filter values are handlers.

	      void **catch_word = get_ttype_entry (context, &info, ar_filter);
	      jclass catch_type = (jclass)*catch_word;

	      // FIXME: This line is a kludge to work around exception
	      // handlers written in C++, which don't yet use indirect
	      // dispatch.
	      if (catch_type == *(void **)&java::lang::Class::class$)
		catch_type = (jclass)catch_word;

	      if (_Jv_IsInstanceOf (xh->value, catch_type))
		{
		  handler_switch_value = ar_filter;
		  saw_handler = true;
		  break;
		}
	    }
	  else
	    {
	      // Negative filter values are exception specifications,
	      // which Java does not use.
	      // ??? Perhaps better to make them an index into a table
	      // of null-terminated strings instead of playing games
	      // with Utf8Const+1 as above.
	      abort ();
	    }

	  if (ar_disp == 0)
	    break;
	  action_record = p + ar_disp;
	}
    }

  if (! saw_handler && ! saw_cleanup)
    return _URC_CONTINUE_UNWIND;

  if (actions & _UA_SEARCH_PHASE)
    {
      if (! saw_handler)
	return _URC_CONTINUE_UNWIND;

      // For domestic exceptions, we cache data from phase 1 for phase 2.
      if (exception_class == __gcj_exception_class)
        {
          xh->handlerSwitchValue = handler_switch_value;
          xh->landingPad = landing_pad;
	}
      return _URC_HANDLER_FOUND;
    }

 install_context:
  _Unwind_SetGR (context, __builtin_eh_return_data_regno (0),
		 (_Unwind_Ptr) &xh->unwindHeader);
  _Unwind_SetGR (context, __builtin_eh_return_data_regno (1),
		 handler_switch_value);
  _Unwind_SetIP (context, landing_pad);
  return _URC_INSTALL_CONTEXT;
}
