/* Subroutines needed for unwinding stack frames via the libunwind API.
   Copyright (C) 2002, 2003
   Free Software Foundation, Inc.
   Contributed by David Mosberger-Tang <davidm@hpl.hp.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "tconfig.h"
#include "tsystem.h"
#include "unwind.h"

#ifndef __USING_SJLJ_EXCEPTIONS__

#define UNW_LOCAL_ONLY

#include <libunwind.h>

typedef struct {
  _Unwind_Personality_Fn personality;
} _Unwind_FrameState;

struct _Unwind_Context {
  unw_cursor_t cursor;
};


/* First come the helper-routines that are needed by unwind.inc.  */

static _Unwind_Reason_Code
uw_frame_state_for (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  unw_proc_info_t pi;

  if (unw_step (&context->cursor) <= 0)
    return _URC_END_OF_STACK;

  unw_get_proc_info(&context->cursor, &pi);
  fs->personality = (_Unwind_Personality_Fn) pi.handler;

  return _URC_NO_REASON;
}

#define uw_update_context(context,fs)	do { ; } while (0)

static inline _Unwind_Ptr
uw_identify_context (struct _Unwind_Context *context)
{
  unw_word_t ip;
  unw_get_reg (&context->cursor, UNW_REG_IP, &ip);
  return (_Unwind_Ptr) ip;
}

#define uw_init_context(context)		\
do						\
  {						\
    unw_context_t uc;				\
    unw_getcontext (&uc);			\
    unw_init_local (&(context)->cursor, &uc);	\
  }						\
while (0)

static inline void __attribute__ ((noreturn))
uw_install_context (struct _Unwind_Context *current __attribute__ ((unused)),
		    struct _Unwind_Context *target)
{
  unw_resume (&(target)->cursor);
  abort ();
}


/* Now come the helper-routines which may be called from an exception
   handler.  The interface for these routines are defined by the C++
   ABI.  See: http://www.codesourcery.com/cxx-abi/abi-eh.html */

_Unwind_Word
_Unwind_GetGR (struct _Unwind_Context *context, int index)
{
  unw_word_t ret;

  /* Note: here we depend on the fact that general registers are
     expected to start with register number 0!  */
  unw_get_reg (&context->cursor, index, &ret);
  return ret;
}

/* Get the value of the CFA as saved in CONTEXT.  */

_Unwind_Word
_Unwind_GetCFA (struct _Unwind_Context *context)
{
  /* ??? Is there any way to get this information?  */
  return NULL;
} 

/* Overwrite the saved value for register REG in CONTEXT with VAL.  */

void
_Unwind_SetGR (struct _Unwind_Context *context, int index, _Unwind_Word val)
{
  /* Note: here we depend on the fact that general registers are
     expected to start with register number 0!  */
  unw_set_reg (&context->cursor, index, val);
}

/* Retrieve the return address for CONTEXT.  */

inline _Unwind_Ptr
_Unwind_GetIP (struct _Unwind_Context *context)
{
  unw_word_t ret;

  unw_get_reg (&context->cursor, UNW_REG_IP, &ret);
  return ret;
}

/* Overwrite the return address for CONTEXT with VAL.  */

inline void
_Unwind_SetIP (struct _Unwind_Context *context, _Unwind_Ptr val)
{
  unw_set_reg (&context->cursor, UNW_REG_IP, val);
}

void *
_Unwind_GetLanguageSpecificData (struct _Unwind_Context *context)
{
  unw_proc_info_t pi;

  unw_get_proc_info(&context->cursor, &pi);
  return (void *) pi.lsda;
}

_Unwind_Ptr
_Unwind_GetRegionStart (struct _Unwind_Context *context)
{
  unw_proc_info_t pi;

  unw_get_proc_info(&context->cursor, &pi);
  return (_Unwind_Ptr) pi.start_ip;
}

void *
_Unwind_FindEnclosingFunction (void *pc)
{
  return NULL;
}

#include "unwind.inc"

#endif /* !__USING_SJLJ_EXCEPTIONS__ */
