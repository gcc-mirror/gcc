/* Supporting functions for C exception handling.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldy@quesejoda.com>.
   Shamelessly stolen from the Java front end.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "tconfig.h"
#include "tsystem.h"
#include "unwind.h"
#include "unwind-pe.h"

typedef struct
{
  _Unwind_Ptr Start;
  _Unwind_Ptr LPStart;
  _Unwind_Ptr ttype_base;
  const unsigned char *TType;
  const unsigned char *action_table;
  unsigned char ttype_encoding;
  unsigned char call_site_encoding;
} lsda_header_info;

static const unsigned char *
parse_lsda_header (struct _Unwind_Context *context, const unsigned char *p,
		   lsda_header_info *info)
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

#ifdef __USING_SJLJ_EXCEPTIONS__
#define PERSONALITY_FUNCTION    __gcc_personality_sj0
#define __builtin_eh_return_data_regno(x) x
#else
#define PERSONALITY_FUNCTION    __gcc_personality_v0
#endif
#define PERSONALITY_FUNCTION    __gcc_personality_v0

_Unwind_Reason_Code
PERSONALITY_FUNCTION (int, _Unwind_Action, _Unwind_Exception_Class,
		      struct _Unwind_Exception *, struct _Unwind_Context *);

_Unwind_Reason_Code
PERSONALITY_FUNCTION (int version,
		      _Unwind_Action actions,
		      _Unwind_Exception_Class exception_class ATTRIBUTE_UNUSED,
		      struct _Unwind_Exception *ue_header,
		      struct _Unwind_Context *context)
{
  lsda_header_info info;
  const unsigned char *language_specific_data, *p, *action_record;
  _Unwind_Ptr landing_pad, ip;

  if (version != 1)
    return _URC_FATAL_PHASE1_ERROR;

  /* Currently we only support cleanups for C.  */
  if ((actions & _UA_CLEANUP_PHASE) == 0)
    return _URC_CONTINUE_UNWIND;

  language_specific_data = (const unsigned char *)
    _Unwind_GetLanguageSpecificData (context);

  /* If no LSDA, then there are no handlers or cleanups.  */
  if (! language_specific_data)
    return _URC_CONTINUE_UNWIND;

  /* Parse the LSDA header.  */
  p = parse_lsda_header (context, language_specific_data, &info);
  ip = _Unwind_GetIP (context) - 1;
  landing_pad = 0;

#ifdef __USING_SJLJ_EXCEPTIONS__
  /* The given "IP" is an index into the call-site table, with two
     exceptions -- -1 means no-action, and 0 means terminate.  But
     since we're using uleb128 values, we've not got random access
     to the array.  */
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
  
#endif

  /* IP is not in table.  No associated cleanups.  */
  /* ??? This is where C++ calls std::terminate to catch throw
     from a destructor.  */
  return _URC_CONTINUE_UNWIND;

 found_something:
  if (landing_pad == 0)
    {
      /* IP is present, but has a null landing pad.
	 No handler to be run.  */
      return _URC_CONTINUE_UNWIND;
    }

  _Unwind_SetGR (context, __builtin_eh_return_data_regno (0),
		 (_Unwind_Ptr) ue_header);
  _Unwind_SetGR (context, __builtin_eh_return_data_regno (1), 0);
  _Unwind_SetIP (context, landing_pad);
  return _URC_INSTALL_CONTEXT;
}
