/* The implementation of exception handling primitives for Objective-C.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled
   with GCC to produce an executable, this does not cause the resulting
   executable to be covered by the GNU General Public License.  This
   exception does not however invalidate any other reasons why the
   executable file might be covered by the GNU General Public License. */

#include <stdlib.h>
#include "config.h"
#include "objc/objc-api.h"
#include "unwind.h"
#include "unwind-pe.h"


/* This is the exception class we report -- "GNUCOBJC".  */
#define __objc_exception_class			\
  ((((((((_Unwind_Exception_Class) 'G'		\
         << 8 | (_Unwind_Exception_Class) 'N')	\
        << 8 | (_Unwind_Exception_Class) 'U')	\
       << 8 | (_Unwind_Exception_Class) 'C')	\
      << 8 | (_Unwind_Exception_Class) 'O')	\
     << 8 | (_Unwind_Exception_Class) 'B')	\
    << 8 | (_Unwind_Exception_Class) 'J')	\
   << 8 | (_Unwind_Exception_Class) 'C')

/* This is the object that is passed around by the Objective C runtime
   to represent the exception in flight.  */

struct ObjcException
{
  /* This bit is needed in order to interact with the unwind runtime.  */
  struct _Unwind_Exception base;

  /* The actual object we want to throw.  */
  id value;

  /* Cache some internal unwind data between phase 1 and phase 2.  */
  _Unwind_Ptr landingPad;
  int handlerSwitchValue;
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
  _Unwind_Word tmp;
  unsigned char lpstart_encoding;

  info->Start = (context ? _Unwind_GetRegionStart (context) : 0);

  /* Find @LPStart, the base to which landing pad offsets are relative.  */
  lpstart_encoding = *p++;
  if (lpstart_encoding != DW_EH_PE_omit)
    p = read_encoded_value (context, lpstart_encoding, p, &info->LPStart);
  else
    info->LPStart = info->Start;

  /* Find @TType, the base of the handler and exception spec type data.  */
  info->ttype_encoding = *p++;
  if (info->ttype_encoding != DW_EH_PE_omit)
    {
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

  /* NULL ptr means catch-all.  */
  if (ptr)
    return objc_get_class ((const char *) ptr);
  else
    return 0;
}

/* Like unto the method of the same name on Object, but takes an id.  */
/* ??? Does this bork the meta-type system?  Can/should we look up an
   isKindOf method on the id?  */

static int
isKindOf (id value, Class target)
{
  Class c;

  /* NULL target is catch-all.  */
  if (target == 0)
    return 1;

  for (c = value->class_pointer; c; c = class_get_super_class (c))
    if (c == target)
      return 1;
  return 0;
}

/* Using a different personality function name causes link failures
   when trying to mix code using different exception handling models.  */
#ifdef SJLJ_EXCEPTIONS
#define PERSONALITY_FUNCTION	__gnu_objc_personality_sj0
#define __builtin_eh_return_data_regno(x) x
#else
#define PERSONALITY_FUNCTION	__gnu_objc_personality_v0
#endif

_Unwind_Reason_Code
PERSONALITY_FUNCTION (int version,
		      _Unwind_Action actions,
		      _Unwind_Exception_Class exception_class,
		      struct _Unwind_Exception *ue_header,
		      struct _Unwind_Context *context)
{
  struct ObjcException *xh = (struct ObjcException *) ue_header;

  struct lsda_header_info info;
  const unsigned char *language_specific_data;
  const unsigned char *action_record;
  const unsigned char *p;
  _Unwind_Ptr landing_pad, ip;
  int handler_switch_value;
  int saw_cleanup, saw_handler;

  /* Interface version check.  */
  if (version != 1)
    return _URC_FATAL_PHASE1_ERROR;

  /* Shortcut for phase 2 found handler for domestic exception.  */
  if (actions == (_UA_CLEANUP_PHASE | _UA_HANDLER_FRAME)
      && exception_class == __objc_exception_class)
    {
      handler_switch_value = xh->handlerSwitchValue;
      landing_pad = xh->landingPad;
      goto install_context;
    }

  language_specific_data = (const unsigned char *)
    _Unwind_GetLanguageSpecificData (context);

  /* If no LSDA, then there are no handlers or cleanups.  */
  if (! language_specific_data)
    return _URC_CONTINUE_UNWIND;

  /* Parse the LSDA header.  */
  p = parse_lsda_header (context, language_specific_data, &info);
  info.ttype_base = base_of_encoded_value (info.ttype_encoding, context);
  ip = _Unwind_GetIP (context) - 1;
  landing_pad = 0;
  action_record = 0;
  handler_switch_value = 0;

#ifdef SJLJ_EXCEPTIONS
  /* The given "IP" is an index into the call-site table, with two
     exceptions -- -1 means no-action, and 0 means terminate.  But
     since we're using uleb128 values, we've not got random access
     to the array.  */
  if ((int) ip < 0)
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

      /* Can never have null landing pad for sjlj -- that would have
         been indicated by a -1 call site index.  */
      landing_pad = cs_lp + 1;
      if (cs_action)
	action_record = info.action_table + cs_action - 1;
      goto found_something;
    }
#else
  /* Search the call-site table for the action associated with this IP.  */
  while (p < info.action_table)
    {
      _Unwind_Ptr cs_start, cs_len, cs_lp;
      _Unwind_Word cs_action;

      /* Note that all call-site encodings are "absolute" displacements.  */
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
  /* ??? As with Java, it's perhaps better to tweek the LSDA to
     that no-action is mapped to no-entry.  */
  return _URC_CONTINUE_UNWIND;

 found_something:
  saw_cleanup = 0;
  saw_handler = 0;

  if (landing_pad == 0)
    {
      /* If ip is present, and has a null landing pad, there are
	 no cleanups or handlers to be run.  */
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
      _Unwind_Sword ar_filter, ar_disp;

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
	     foreign exception class, we have no class info to match.  */
	  else if ((actions & _UA_FORCE_UNWIND)
		   || exception_class != __objc_exception_class)
	    ;

	  else if (ar_filter > 0)
	    {
	      /* Positive filter values are handlers.  */

	      Class catch_type = get_ttype_entry (&info, ar_filter);

	      if (isKindOf (xh->value, catch_type))
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
    return _URC_CONTINUE_UNWIND;

  if (actions & _UA_SEARCH_PHASE)
    {
      if (!saw_handler)
	return _URC_CONTINUE_UNWIND;

      /* For domestic exceptions, we cache data from phase 1 for phase 2.  */
      if (exception_class == __objc_exception_class)
        {
          xh->handlerSwitchValue = handler_switch_value;
          xh->landingPad = landing_pad;
	}
      return _URC_HANDLER_FOUND;
    }

 install_context:
  _Unwind_SetGR (context, __builtin_eh_return_data_regno (0),
		 __builtin_extend_pointer (xh->value));
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
objc_exception_throw (id value)
{
  struct ObjcException *header = calloc (1, sizeof (*header));
  header->base.exception_class = __objc_exception_class;
  header->base.exception_cleanup = __objc_exception_cleanup;
  header->value = value;

#ifdef SJLJ_EXCEPTIONS
  _Unwind_SjLj_RaiseException (&header->base);
#else
  _Unwind_RaiseException (&header->base);
#endif

  /* Some sort of unwinding error.  */
  abort ();
}
