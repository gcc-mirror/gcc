/* The implementation of exception handling primitives for Objective-C.
   Copyright (C) 2004-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "objc-private/common.h"
#include <stdlib.h>
#include "config.h"
#include "objc/runtime.h"
#include "objc/objc-exception.h"
#include "unwind.h"
#include "unwind-pe.h"
#include <string.h> /* For memcpy */

/* 'is_kind_of_exception_matcher' is our default exception matcher -
   it determines if the object 'exception' is of class 'catch_class',
   or of a subclass.  */
static int
is_kind_of_exception_matcher (Class catch_class, id exception)
{
  /* NULL catch_class is catch-all (eg, @catch (id object)).  */
  if (catch_class == Nil)
    return 1;

  /* If exception is nil (eg, @throw nil;), then it can only be
     catched by a catch-all (eg, @catch (id object)).  */
  if (exception != nil)
    {
      Class c;

      for (c = exception->class_pointer; c != Nil; 
	   c = class_getSuperclass (c))
	if (c == catch_class)
	  return 1;
    }
  return 0;
}

/* The exception matcher currently in use.  */
static objc_exception_matcher
__objc_exception_matcher = is_kind_of_exception_matcher;

objc_exception_matcher
objc_setExceptionMatcher (objc_exception_matcher new_matcher)
{
  objc_exception_matcher old_matcher = __objc_exception_matcher;
  __objc_exception_matcher = new_matcher;
  return old_matcher;
}

/* The uncaught exception handler currently in use.  */
static objc_uncaught_exception_handler
__objc_uncaught_exception_handler = NULL;

objc_uncaught_exception_handler
objc_setUncaughtExceptionHandler (objc_uncaught_exception_handler 
				  new_handler)
{
  objc_uncaught_exception_handler old_handler 
    = __objc_uncaught_exception_handler;
  __objc_uncaught_exception_handler = new_handler;
  return old_handler;
}



#ifdef __ARM_EABI_UNWINDER__

const _Unwind_Exception_Class __objc_exception_class
  = {'G', 'N', 'U', 'C', 'O', 'B', 'J', 'C'};
  
#else

/* This is the exception class we report -- "GNUCOBJC".  */
static const _Unwind_Exception_Class __objc_exception_class
  = ((((((((_Unwind_Exception_Class) 'G'
            << 8 | (_Unwind_Exception_Class) 'N')
           << 8 | (_Unwind_Exception_Class) 'U')
          << 8 | (_Unwind_Exception_Class) 'C')
         << 8 | (_Unwind_Exception_Class) 'O')
        << 8 | (_Unwind_Exception_Class) 'B')
       << 8 | (_Unwind_Exception_Class) 'J')
      << 8 | (_Unwind_Exception_Class) 'C');

#endif

/* This is the object that is passed around by the Objective C runtime
   to represent the exception in flight.  */
struct ObjcException
{
  /* This bit is needed in order to interact with the unwind runtime.  */
  struct _Unwind_Exception base;

  /* The actual object we want to throw. Note: must come immediately
     after unwind header.  */
  id value;

#ifdef __ARM_EABI_UNWINDER__
  /* Note: we use the barrier cache defined in the unwind control
     block for ARM EABI.  */
#else
  /* Cache some internal unwind data between phase 1 and phase 2.  */
  _Unwind_Ptr landingPad;
  int handlerSwitchValue;
#endif
};



struct lsda_header_info
{
  _Unwind_Ptr Start;
  _Unwind_Ptr LPStart;
  _Unwind_Ptr ttype_base;
  const unsigned char *TType;
  const unsigned char *action_table;
  unsigned char ttype_encoding;
  unsigned char call_site_encoding;
};

static const unsigned char *
parse_lsda_header (struct _Unwind_Context *context, const unsigned char *p,
		   struct lsda_header_info *info)
{
  _uleb128_t tmp;
  unsigned char lpstart_encoding;

  info->Start = (context ? _Unwind_GetRegionStart (context) : 0);

  /* Find @LPStart, the base to which landing pad offsets are
     relative.  */
  lpstart_encoding = *p++;
  if (lpstart_encoding != DW_EH_PE_omit)
    p = read_encoded_value (context, lpstart_encoding, p, &info->LPStart);
  else
    info->LPStart = info->Start;

  /* Find @TType, the base of the handler and exception spec type
     data.  */
  info->ttype_encoding = *p++;
  if (info->ttype_encoding != DW_EH_PE_omit)
    {
#if _GLIBCXX_OVERRIDE_TTYPE_ENCODING
      /* Older ARM EABI toolchains set this value incorrectly, so use a
	 hardcoded OS-specific format.  */
      info->ttype_encoding = _GLIBCXX_OVERRIDE_TTYPE_ENCODING;
#endif
      p = read_uleb128 (p, &tmp);
      info->TType = p + tmp;
    }
  else
    info->TType = 0;

  /* The encoding and length of the call-site table; the action table
     immediately follows.  */
  info->call_site_encoding = *p++;
  p = read_uleb128 (p, &tmp);
  info->action_table = p + tmp;

  return p;
}

static Class
get_ttype_entry (struct lsda_header_info *info, _Unwind_Word i)
{
  _Unwind_Ptr ptr;

  i *= size_of_encoded_value (info->ttype_encoding);
  read_encoded_value_with_base (info->ttype_encoding, info->ttype_base,
				info->TType - i, &ptr);

  /* NULL ptr means catch-all.  Note that if the class is not found,
     this will abort the program.  */
  if (ptr)
    return objc_getRequiredClass ((const char *) ptr);
  else
    return 0;
}

/* Using a different personality function name causes link failures
   when trying to mix code using different exception handling
   models.  */
#ifdef SJLJ_EXCEPTIONS
#define PERSONALITY_FUNCTION	__gnu_objc_personality_sj0
#define __builtin_eh_return_data_regno(x) x
#elif defined(__SEH__)
#define PERSONALITY_FUNCTION	__gnu_objc_personality_imp
#else
#define PERSONALITY_FUNCTION	__gnu_objc_personality_v0
#endif

#ifdef __ARM_EABI_UNWINDER__

#define CONTINUE_UNWINDING \
  do								\
    {								\
      if (__gnu_unwind_frame(ue_header, context) != _URC_OK)	\
	return _URC_FAILURE;					\
      return _URC_CONTINUE_UNWIND;				\
    }								\
  while (0)

_Unwind_Reason_Code
PERSONALITY_FUNCTION (_Unwind_State state,
		      struct _Unwind_Exception *ue_header,
		      struct _Unwind_Context *context)
#else

#define CONTINUE_UNWINDING return _URC_CONTINUE_UNWIND

#ifdef __SEH__
static
#endif
_Unwind_Reason_Code
PERSONALITY_FUNCTION (int version,
		      _Unwind_Action actions,
		      _Unwind_Exception_Class exception_class,
		      struct _Unwind_Exception *ue_header,
		      struct _Unwind_Context *context)
#endif
{
  struct ObjcException *xh = (struct ObjcException *) ue_header;

  struct lsda_header_info info;
  const unsigned char *language_specific_data;
  const unsigned char *action_record;
  const unsigned char *p;
  _Unwind_Ptr landing_pad, ip;
  int handler_switch_value;
  int saw_cleanup = 0, saw_handler, foreign_exception;
  void *return_object;
  int ip_before_insn = 0;

#ifdef __ARM_EABI_UNWINDER__
  _Unwind_Action actions;
  
  switch (state & _US_ACTION_MASK)
    {
    case _US_VIRTUAL_UNWIND_FRAME:
      actions = _UA_SEARCH_PHASE;
      break;

    case _US_UNWIND_FRAME_STARTING:
      actions = _UA_CLEANUP_PHASE;
      if (!(state & _US_FORCE_UNWIND)
	  && ue_header->barrier_cache.sp == _Unwind_GetGR (context, 13))
	actions |= _UA_HANDLER_FRAME;
      break;

    case _US_UNWIND_FRAME_RESUME:
      CONTINUE_UNWINDING;
      break;

    default:
      abort();
    }
  actions |= state & _US_FORCE_UNWIND;

  /* TODO: Foreign exceptions need some attention (e.g. rethrowing
     doesn't work).  */
  foreign_exception = 0;

  /* The dwarf unwinder assumes the context structure holds things
     like the function and LSDA pointers.  The ARM implementation
     caches these in the exception header (UCB).  To avoid rewriting
     everything we make the virtual IP register point at the UCB.  */
  ip = (_Unwind_Ptr) ue_header;
  _Unwind_SetGR (context, 12, ip);

#else  /* !__ARM_EABI_UNWINDER.  */
  /* Interface version check.  */
  if (version != 1)
    return _URC_FATAL_PHASE1_ERROR;
  
  foreign_exception = (exception_class != __objc_exception_class);
#endif

  /* Shortcut for phase 2 found handler for domestic exception.  */
  if (actions == (_UA_CLEANUP_PHASE | _UA_HANDLER_FRAME)
      && !foreign_exception)
    {
#ifdef __ARM_EABI_UNWINDER__
      handler_switch_value = (int) ue_header->barrier_cache.bitpattern[1];
      landing_pad = (_Unwind_Ptr) ue_header->barrier_cache.bitpattern[3];
#else
      handler_switch_value = xh->handlerSwitchValue;
      landing_pad = xh->landingPad;
#endif
      goto install_context;
    }

  language_specific_data = (const unsigned char *)
    _Unwind_GetLanguageSpecificData (context);

  /* If no LSDA, then there are no handlers or cleanups.  */
  if (! language_specific_data)
    CONTINUE_UNWINDING;

  /* Parse the LSDA header.  */
  p = parse_lsda_header (context, language_specific_data, &info);
  info.ttype_base = base_of_encoded_value (info.ttype_encoding, context);
#ifdef HAVE_GETIPINFO
  ip = _Unwind_GetIPInfo (context, &ip_before_insn);
#else
  ip = _Unwind_GetIP (context);
#endif
  if (!ip_before_insn)
    --ip;
  landing_pad = 0;
  action_record = 0;
  handler_switch_value = 0;

#ifdef SJLJ_EXCEPTIONS
  /* The given "IP" is an index into the call-site table, with two
     exceptions -- -1 means no-action, and 0 means terminate.  But
     since we're using uleb128 values, we've not got random access to
     the array.  */
  if ((int) ip < 0)
    return _URC_CONTINUE_UNWIND;
  else
    {
      _uleb128_t cs_lp, cs_action;
      do
	{
	  p = read_uleb128 (p, &cs_lp);
	  p = read_uleb128 (p, &cs_action);
	}
      while (--ip);

      /* Can never have null landing pad for sjlj -- that would have
         been indicated by a -1 call site index.  */
      landing_pad = cs_lp + 1;
      if (cs_action)
	action_record = info.action_table + cs_action - 1;
      goto found_something;
    }
#else
  /* Search the call-site table for the action associated with this
     IP.  */
  while (p < info.action_table)
    {
      _Unwind_Ptr cs_start, cs_len, cs_lp;
      _uleb128_t cs_action;

      /* Note that all call-site encodings are "absolute"
	 displacements.  */
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_start);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_len);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_lp);
      p = read_uleb128 (p, &cs_action);

      /* The table is sorted, so if we've passed the ip, stop.  */
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
#endif /* SJLJ_EXCEPTIONS  */

  /* If ip is not present in the table, C++ would call terminate.  */
  /* ??? As with Java, it's perhaps better to tweek the LSDA to that
     no-action is mapped to no-entry.  */
  CONTINUE_UNWINDING;

 found_something:
  saw_cleanup = 0;
  saw_handler = 0;

  if (landing_pad == 0)
    {
      /* If ip is present, and has a null landing pad, there are no
	 cleanups or handlers to be run.  */
    }
  else if (action_record == 0)
    {
      /* If ip is present, has a non-null landing pad, and a null
         action table offset, then there are only cleanups present.
         Cleanups use a zero switch value, as set above.  */
      saw_cleanup = 1;
    }
  else
    {
      /* Otherwise we have a catch handler.  */
      _sleb128_t ar_filter, ar_disp;

      while (1)
	{
	  p = action_record;
	  p = read_sleb128 (p, &ar_filter);
	  read_sleb128 (p, &ar_disp);

	  if (ar_filter == 0)
	    {
	      /* Zero filter values are cleanups.  */
	      saw_cleanup = 1;
	    }

	  /* During forced unwinding, we only run cleanups.  With a
	     foreign exception class, we have no class info to
	     match.  */
	  else if ((actions & _UA_FORCE_UNWIND) || foreign_exception)
	    ;

	  else if (ar_filter > 0)
	    {
	      /* Positive filter values are handlers.  */
	      Class catch_type = get_ttype_entry (&info, ar_filter);

	      if ((*__objc_exception_matcher) (catch_type, xh->value))
		{
		  handler_switch_value = ar_filter;
		  saw_handler = 1;
		  break;
		}
	    }
	  else
	    {
	      /* Negative filter values are exception specifications,
	         which Objective-C does not use.  */
	      abort ();
	    }

	  if (ar_disp == 0)
	    break;
	  action_record = p + ar_disp;
	}
    }

  if (! saw_handler && ! saw_cleanup)
    CONTINUE_UNWINDING;

  if (actions & _UA_SEARCH_PHASE)
    {
      if (!saw_handler)
	CONTINUE_UNWINDING;

      /* For domestic exceptions, we cache data from phase 1 for phase
	 2.  */
      if (!foreign_exception)
        {
#ifdef __ARM_EABI_UNWINDER__
	  ue_header->barrier_cache.sp = _Unwind_GetGR (context, 13);
	  ue_header->barrier_cache.bitpattern[1] = (_uw) handler_switch_value;
	  ue_header->barrier_cache.bitpattern[3] = (_uw) landing_pad;
#else
          xh->handlerSwitchValue = handler_switch_value;
          xh->landingPad = landing_pad;
#endif
	}
      return _URC_HANDLER_FOUND;
    }

 install_context:
  if (saw_cleanup == 0)
    {
      return_object = xh->value;
      if (!(actions & _UA_SEARCH_PHASE))
	_Unwind_DeleteException(&xh->base);
    }
  
  _Unwind_SetGR (context, __builtin_eh_return_data_regno (0),
		 __builtin_extend_pointer (saw_cleanup ? xh : return_object));
  _Unwind_SetGR (context, __builtin_eh_return_data_regno (1),
		 handler_switch_value);
  _Unwind_SetIP (context, landing_pad);
  return _URC_INSTALL_CONTEXT;
}

static void
__objc_exception_cleanup (_Unwind_Reason_Code code __attribute__((unused)),
			  struct _Unwind_Exception *exc)
{
  free (exc);
}

void
objc_exception_throw (id exception)
{
  struct ObjcException *header = calloc (1, sizeof (*header));

  memcpy (&header->base.exception_class, &__objc_exception_class,
	  sizeof (__objc_exception_class));
  header->base.exception_cleanup = __objc_exception_cleanup;
  header->value = exception;

#ifdef SJLJ_EXCEPTIONS
  _Unwind_SjLj_RaiseException (&header->base);
#else
  _Unwind_RaiseException (&header->base);
#endif

  /* No exception handler was installed.  Call the uncaught exception
     handler if any is defined.  */
  if (__objc_uncaught_exception_handler != 0)
    {
      (*__objc_uncaught_exception_handler) (exception);
    }

  abort ();
}

#ifdef __SEH__
EXCEPTION_DISPOSITION
__gnu_objc_personality_seh0 (PEXCEPTION_RECORD ms_exc, void *this_frame,
			     PCONTEXT ms_orig_context,
			     PDISPATCHER_CONTEXT ms_disp)
{
  return _GCC_specific_handler (ms_exc, this_frame, ms_orig_context,
				ms_disp, __gnu_objc_personality_imp);
}
#endif /* SEH */
