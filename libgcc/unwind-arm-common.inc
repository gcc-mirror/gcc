/* Common unwinding code for ARM EABI and C6X.
   Copyright (C) 2004, 2005, 2009, 2011 Free Software Foundation, Inc.
   Contributed by Paul Brook

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "unwind.h"

/* We add a prototype for abort here to avoid creating a dependency on
   target headers.  */
extern void abort (void);

/* Definitions for C++ runtime support routines.  We make these weak
   declarations to avoid pulling in libsupc++ unnecessarily.  */
typedef unsigned char bool;

typedef struct _ZSt9type_info type_info; /* This names C++ type_info type */
enum __cxa_type_match_result
  {
    ctm_failed = 0,
    ctm_succeeded = 1,
    ctm_succeeded_with_ptr_to_base = 2
  };

void __attribute__((weak)) __cxa_call_unexpected(_Unwind_Control_Block *ucbp);
bool __attribute__((weak)) __cxa_begin_cleanup(_Unwind_Control_Block *ucbp);
enum __cxa_type_match_result __attribute__((weak)) __cxa_type_match
  (_Unwind_Control_Block *ucbp, const type_info *rttip,
   bool is_reference, void **matched_object);

_Unwind_Ptr __attribute__((weak))
__gnu_Unwind_Find_exidx (_Unwind_Ptr, int *);

#define EXIDX_CANTUNWIND 1
#define uint32_highbit (((_uw) 1) << 31)

#define UCB_FORCED_STOP_FN(ucbp) ((ucbp)->unwinder_cache.reserved1)
#define UCB_PR_ADDR(ucbp) ((ucbp)->unwinder_cache.reserved2)
#define UCB_SAVED_CALLSITE_ADDR(ucbp) ((ucbp)->unwinder_cache.reserved3)
#define UCB_FORCED_STOP_ARG(ucbp) ((ucbp)->unwinder_cache.reserved4)

/* Unwind descriptors.  */

typedef struct
{
  _uw16 length;
  _uw16 offset;
} EHT16;

typedef struct
{
  _uw length;
  _uw offset;
} EHT32;

/* An exception index table entry.  */

typedef struct __EIT_entry
{
  _uw fnoffset;
  _uw content;
} __EIT_entry;

/* Assembly helper functions.  */

/* Restore core register state.  Never returns.  */
void __attribute__((noreturn)) restore_core_regs (struct core_regs *);


/* Restore coprocessor state after phase1 unwinding.  */
static void restore_non_core_regs (phase1_vrs * vrs);

/* A better way to do this would probably be to compare the absolute address
   with a segment relative relocation of the same symbol.  */

extern int __text_start;
extern int __data_start;

/* The exception index table location.  */
extern __EIT_entry __exidx_start;
extern __EIT_entry __exidx_end;

/* Core unwinding functions.  */

/* Calculate the address encoded by a 31-bit self-relative offset at address
   P.  */
static inline _uw selfrel_offset31 (const _uw *p);

static _uw __gnu_unwind_get_pr_addr (int idx);

/* Perform a binary search for RETURN_ADDRESS in TABLE.  The table contains
   NREC entries.  */

static const __EIT_entry *
search_EIT_table (const __EIT_entry * table, int nrec, _uw return_address)
{
  _uw next_fn;
  _uw this_fn;
  int n, left, right;

  if (nrec == 0)
    return (__EIT_entry *) 0;

  left = 0;
  right = nrec - 1;

  while (1)
    {
      n = (left + right) / 2;
      this_fn = selfrel_offset31 (&table[n].fnoffset);
      if (n != nrec - 1)
	next_fn = selfrel_offset31 (&table[n + 1].fnoffset) - 1;
      else
	next_fn = (_uw)0 - 1;

      if (return_address < this_fn)
	{
	  if (n == left)
	    return (__EIT_entry *) 0;
	  right = n - 1;
	}
      else if (return_address <= next_fn)
	return &table[n];
      else
	left = n + 1;
    }
}

/* Find the exception index table eintry for the given address.
   Fill in the relevant fields of the UCB.
   Returns _URC_FAILURE if an error occurred, _URC_OK on success.  */

static _Unwind_Reason_Code
get_eit_entry (_Unwind_Control_Block *ucbp, _uw return_address)
{
  const __EIT_entry * eitp;
  int nrec;
  
  /* The return address is the address of the instruction following the
     call instruction (plus one in thumb mode).  If this was the last
     instruction in the function the address will lie in the following
     function.  Subtract 2 from the address so that it points within the call
     instruction itself.  */
  return_address -= 2;

  if (__gnu_Unwind_Find_exidx)
    {
      eitp = (const __EIT_entry *) __gnu_Unwind_Find_exidx (return_address,
							    &nrec);
      if (!eitp)
	{
	  UCB_PR_ADDR (ucbp) = 0;
	  return _URC_FAILURE;
	}
    }
  else
    {
      eitp = &__exidx_start;
      nrec = &__exidx_end - &__exidx_start;
    }

  eitp = search_EIT_table (eitp, nrec, return_address);

  if (!eitp)
    {
      UCB_PR_ADDR (ucbp) = 0;
      return _URC_FAILURE;
    }
  ucbp->pr_cache.fnstart = selfrel_offset31 (&eitp->fnoffset);

  /* Can this frame be unwound at all?  */
  if (eitp->content == EXIDX_CANTUNWIND)
    {
      UCB_PR_ADDR (ucbp) = 0;
      return _URC_END_OF_STACK;
    }

  /* Obtain the address of the "real" __EHT_Header word.  */

  if (eitp->content & uint32_highbit)
    {
      /* It is immediate data.  */
      ucbp->pr_cache.ehtp = (_Unwind_EHT_Header *)&eitp->content;
      ucbp->pr_cache.additional = 1;
    }
  else
    {
      /* The low 31 bits of the content field are a self-relative
	 offset to an _Unwind_EHT_Entry structure.  */
      ucbp->pr_cache.ehtp =
	(_Unwind_EHT_Header *) selfrel_offset31 (&eitp->content);
      ucbp->pr_cache.additional = 0;
    }

  /* Discover the personality routine address.  */
  if (*ucbp->pr_cache.ehtp & (1u << 31))
    {
      /* One of the predefined standard routines.  */
      _uw idx = (*(_uw *) ucbp->pr_cache.ehtp >> 24) & 0xf;
      UCB_PR_ADDR (ucbp) = __gnu_unwind_get_pr_addr (idx);
      if (UCB_PR_ADDR (ucbp) == 0)
	{
	  /* Failed */
	  return _URC_FAILURE;
	}
    } 
  else
    {
      /* Execute region offset to PR */
      UCB_PR_ADDR (ucbp) = selfrel_offset31 (ucbp->pr_cache.ehtp);
    }
  return _URC_OK;
}


/* Perform phase2 unwinding.  VRS is the initial virtual register state.  */

static void __attribute__((noreturn))
unwind_phase2 (_Unwind_Control_Block * ucbp, phase2_vrs * vrs)
{
  _Unwind_Reason_Code pr_result;

  do
    {
      /* Find the entry for this routine.  */
      if (get_eit_entry (ucbp, VRS_PC(vrs)) != _URC_OK)
	abort ();

      UCB_SAVED_CALLSITE_ADDR (ucbp) = VRS_PC(vrs);

      /* Call the pr to decide what to do.  */
      pr_result = ((personality_routine) UCB_PR_ADDR (ucbp))
	(_US_UNWIND_FRAME_STARTING, ucbp, (_Unwind_Context *) vrs);
    }
  while (pr_result == _URC_CONTINUE_UNWIND);
  
  if (pr_result != _URC_INSTALL_CONTEXT)
    abort();
  
  restore_core_regs (&vrs->core);
}

/* Perform phase2 forced unwinding.  */

static _Unwind_Reason_Code
unwind_phase2_forced (_Unwind_Control_Block *ucbp, phase2_vrs *entry_vrs,
		      int resuming)
{
  _Unwind_Stop_Fn stop_fn = (_Unwind_Stop_Fn) UCB_FORCED_STOP_FN (ucbp);
  void *stop_arg = (void *)UCB_FORCED_STOP_ARG (ucbp);
  _Unwind_Reason_Code pr_result = 0;
  /* We use phase1_vrs here even though we do not demand save, for the
     prev_sp field.  */
  phase1_vrs saved_vrs, next_vrs;

  /* Save the core registers.  */
  saved_vrs.core = entry_vrs->core;
  /* We don't need to demand-save the non-core registers, because we
     unwind in a single pass.  */
  saved_vrs.demand_save_flags = 0;

  /* Unwind until we reach a propagation barrier.  */
  do
    {
      _Unwind_State action;
      _Unwind_Reason_Code entry_code;
      _Unwind_Reason_Code stop_code;

      /* Find the entry for this routine.  */
      entry_code = get_eit_entry (ucbp, VRS_PC (&saved_vrs));

      if (resuming)
	{
	  action = _US_UNWIND_FRAME_RESUME | _US_FORCE_UNWIND;
	  resuming = 0;
	}
      else
	action = _US_UNWIND_FRAME_STARTING | _US_FORCE_UNWIND;

      if (entry_code == _URC_OK)
	{
	  UCB_SAVED_CALLSITE_ADDR (ucbp) = VRS_PC (&saved_vrs);

	  next_vrs = saved_vrs;

	  /* Call the pr to decide what to do.  */
	  pr_result = ((personality_routine) UCB_PR_ADDR (ucbp))
	    (action, ucbp, (void *) &next_vrs);

	  saved_vrs.prev_sp = VRS_SP (&next_vrs);
	}
      else
	{
	  /* Treat any failure as the end of unwinding, to cope more
	     gracefully with missing EH information.  Mixed EH and
	     non-EH within one object will usually result in failure,
	     because the .ARM.exidx tables do not indicate the end
	     of the code to which they apply; but mixed EH and non-EH
	     shared objects should return an unwind failure at the
	     entry of a non-EH shared object.  */
	  action |= _US_END_OF_STACK;

	  saved_vrs.prev_sp = VRS_SP (&saved_vrs);
	}

      stop_code = stop_fn (1, action, ucbp->exception_class, ucbp,
			   (void *)&saved_vrs, stop_arg);
      if (stop_code != _URC_NO_REASON)
	return _URC_FAILURE;

      if (entry_code != _URC_OK)
	return entry_code;

      saved_vrs = next_vrs;
    }
  while (pr_result == _URC_CONTINUE_UNWIND);

  if (pr_result != _URC_INSTALL_CONTEXT)
    {
      /* Some sort of failure has occurred in the pr and probably the
	 pr returned _URC_FAILURE.  */
      return _URC_FAILURE;
    }

  restore_core_regs (&saved_vrs.core);
}

/* This is a very limited implementation of _Unwind_GetCFA.  It returns
   the stack pointer as it is about to be unwound, and is only valid
   while calling the stop function during forced unwinding.  If the
   current personality routine result is going to run a cleanup, this
   will not be the CFA; but when the frame is really unwound, it will
   be.  */

_Unwind_Word
_Unwind_GetCFA (_Unwind_Context *context)
{
  return ((phase1_vrs *) context)->prev_sp;
}

/* Perform phase1 unwinding.  UCBP is the exception being thrown, and
   entry_VRS is the register state on entry to _Unwind_RaiseException.  */

_Unwind_Reason_Code
__gnu_Unwind_RaiseException (_Unwind_Control_Block *, phase2_vrs *);

_Unwind_Reason_Code
__gnu_Unwind_RaiseException (_Unwind_Control_Block * ucbp,
			     phase2_vrs * entry_vrs)
{
  phase1_vrs saved_vrs;
  _Unwind_Reason_Code pr_result;

  /* Set the pc to the call site.  */
  VRS_PC (entry_vrs) = VRS_RETURN(entry_vrs);

  /* Save the core registers.  */
  saved_vrs.core = entry_vrs->core;
  /* Set demand-save flags.  */
  saved_vrs.demand_save_flags = ~(_uw) 0;
  
  /* Unwind until we reach a propagation barrier.  */
  do
    {
      /* Find the entry for this routine.  */
      if (get_eit_entry (ucbp, VRS_PC (&saved_vrs)) != _URC_OK)
	return _URC_FAILURE;

      /* Call the pr to decide what to do.  */
      pr_result = ((personality_routine) UCB_PR_ADDR (ucbp))
	(_US_VIRTUAL_UNWIND_FRAME, ucbp, (void *) &saved_vrs);
    }
  while (pr_result == _URC_CONTINUE_UNWIND);

  /* We've unwound as far as we want to go, so restore the original
     register state.  */
  restore_non_core_regs (&saved_vrs);
  if (pr_result != _URC_HANDLER_FOUND)
    {
      /* Some sort of failure has occurred in the pr and probably the
	 pr returned _URC_FAILURE.  */
      return _URC_FAILURE;
    }
  
  unwind_phase2 (ucbp, entry_vrs);
}

/* Resume unwinding after a cleanup has been run.  UCBP is the exception
   being thrown and ENTRY_VRS is the register state on entry to
   _Unwind_Resume.  */
_Unwind_Reason_Code
__gnu_Unwind_ForcedUnwind (_Unwind_Control_Block *,
			   _Unwind_Stop_Fn, void *, phase2_vrs *);

_Unwind_Reason_Code
__gnu_Unwind_ForcedUnwind (_Unwind_Control_Block *ucbp,
			   _Unwind_Stop_Fn stop_fn, void *stop_arg,
			   phase2_vrs *entry_vrs)
{
  UCB_FORCED_STOP_FN (ucbp) = (_uw) stop_fn;
  UCB_FORCED_STOP_ARG (ucbp) = (_uw) stop_arg;

  /* Set the pc to the call site.  */
  VRS_PC (entry_vrs) = VRS_RETURN(entry_vrs);

  return unwind_phase2_forced (ucbp, entry_vrs, 0);
}

_Unwind_Reason_Code
__gnu_Unwind_Resume (_Unwind_Control_Block *, phase2_vrs *);

_Unwind_Reason_Code
__gnu_Unwind_Resume (_Unwind_Control_Block * ucbp, phase2_vrs * entry_vrs)
{
  _Unwind_Reason_Code pr_result;

  /* Recover the saved address.  */
  VRS_PC (entry_vrs) = UCB_SAVED_CALLSITE_ADDR (ucbp);

  if (UCB_FORCED_STOP_FN (ucbp))
    {
      unwind_phase2_forced (ucbp, entry_vrs, 1);

      /* We can't return failure at this point.  */
      abort ();
    }

  /* Call the cached PR.  */
  pr_result = ((personality_routine) UCB_PR_ADDR (ucbp))
	(_US_UNWIND_FRAME_RESUME, ucbp, (_Unwind_Context *) entry_vrs);

  switch (pr_result)
    {
    case _URC_INSTALL_CONTEXT:
      /* Upload the registers to enter the landing pad.  */
      restore_core_regs (&entry_vrs->core);

    case _URC_CONTINUE_UNWIND:
      /* Continue unwinding the next frame.  */
      unwind_phase2 (ucbp, entry_vrs);

    default:
      abort ();
    }
}

_Unwind_Reason_Code
__gnu_Unwind_Resume_or_Rethrow (_Unwind_Control_Block *, phase2_vrs *);

_Unwind_Reason_Code
__gnu_Unwind_Resume_or_Rethrow (_Unwind_Control_Block * ucbp,
				phase2_vrs * entry_vrs)
{
  if (!UCB_FORCED_STOP_FN (ucbp))
    return __gnu_Unwind_RaiseException (ucbp, entry_vrs);

  /* Set the pc to the call site.  */
  VRS_PC (entry_vrs) = VRS_RETURN (entry_vrs);
  /* Continue unwinding the next frame.  */
  return unwind_phase2_forced (ucbp, entry_vrs, 0);
}

/* Clean up an exception object when unwinding is complete.  */
void
_Unwind_Complete (_Unwind_Control_Block * ucbp __attribute__((unused)))
{
}


/* Free an exception.  */

void
_Unwind_DeleteException (_Unwind_Exception * exc)
{
  if (exc->exception_cleanup)
    (*exc->exception_cleanup) (_URC_FOREIGN_EXCEPTION_CAUGHT, exc);
}


/* Perform stack backtrace through unwind data.  */
_Unwind_Reason_Code
__gnu_Unwind_Backtrace(_Unwind_Trace_Fn trace, void * trace_argument,
		       phase2_vrs * entry_vrs);
_Unwind_Reason_Code
__gnu_Unwind_Backtrace(_Unwind_Trace_Fn trace, void * trace_argument,
		       phase2_vrs * entry_vrs)
{
  phase1_vrs saved_vrs;
  _Unwind_Reason_Code code;

  _Unwind_Control_Block ucb;
  _Unwind_Control_Block *ucbp = &ucb;

  /* Set the pc to the call site.  */
  VRS_PC (entry_vrs) = VRS_RETURN (entry_vrs);

  /* Save the core registers.  */
  saved_vrs.core = entry_vrs->core;
  /* Set demand-save flags.  */
  saved_vrs.demand_save_flags = ~(_uw) 0;
  
  do
    {
      /* Find the entry for this routine.  */
      if (get_eit_entry (ucbp, VRS_PC (&saved_vrs)) != _URC_OK)
	{
	  code = _URC_FAILURE;
	  break;
	}

      /* The dwarf unwinder assumes the context structure holds things
	 like the function and LSDA pointers.  The ARM implementation
	 caches these in the exception header (UCB).  To avoid
	 rewriting everything we make the virtual IP register point at
	 the UCB.  */
      _Unwind_SetGR((_Unwind_Context *)&saved_vrs, UNWIND_POINTER_REG, (_Unwind_Ptr) ucbp);

      /* Call trace function.  */
      if ((*trace) ((_Unwind_Context *) &saved_vrs, trace_argument) 
	  != _URC_NO_REASON)
	{
	  code = _URC_FAILURE;
	  break;
	}

      /* Call the pr to decide what to do.  */
      code = ((personality_routine) UCB_PR_ADDR (ucbp))
	(_US_VIRTUAL_UNWIND_FRAME | _US_FORCE_UNWIND, 
	 ucbp, (void *) &saved_vrs);
    }
  while (code != _URC_END_OF_STACK
	 && code != _URC_FAILURE);

  restore_non_core_regs (&saved_vrs);
  return code;
}


/* Common implementation for ARM ABI defined personality routines.
   ID is the index of the personality routine, other arguments are as defined
   by __aeabi_unwind_cpp_pr{0,1,2}.  */

static _Unwind_Reason_Code
__gnu_unwind_pr_common (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context,
			int id)
{
  __gnu_unwind_state uws;
  _uw *data;
  _uw offset;
  _uw len;
  _uw rtti_count;
  int phase2_call_unexpected_after_unwind = 0;
  int in_range = 0;
  int forced_unwind = state & _US_FORCE_UNWIND;

  state &= _US_ACTION_MASK;

  data = (_uw *) ucbp->pr_cache.ehtp;
  uws.data = *(data++);
  uws.next = data;
  if (id == 0)
    {
      uws.data <<= 8;
      uws.words_left = 0;
      uws.bytes_left = 3;
    }
  else if (id < 3)
    {
      uws.words_left = (uws.data >> 16) & 0xff;
      uws.data <<= 16;
      uws.bytes_left = 2;
      data += uws.words_left;
    }

  /* Restore the saved pointer.  */
  if (state == _US_UNWIND_FRAME_RESUME)
    data = (_uw *) ucbp->cleanup_cache.bitpattern[0];

  if ((ucbp->pr_cache.additional & 1) == 0)
    {
      /* Process descriptors.  */
      while (*data)
	{
	  _uw addr;
	  _uw fnstart;

	  if (id == 2)
	    {
	      len = ((EHT32 *) data)->length;
	      offset = ((EHT32 *) data)->offset;
	      data += 2;
	    }
	  else
	    {
	      len = ((EHT16 *) data)->length;
	      offset = ((EHT16 *) data)->offset;
	      data++;
	    }

	  fnstart = ucbp->pr_cache.fnstart + (offset & ~1);
	  addr = _Unwind_GetGR (context, R_PC);
	  in_range = (fnstart <= addr && addr < fnstart + (len & ~1));

	  switch (((offset & 1) << 1) | (len & 1))
	    {
	    case 0:
	      /* Cleanup.  */
	      if (state != _US_VIRTUAL_UNWIND_FRAME
		  && in_range)
		{
		  /* Cleanup in range, and we are running cleanups.  */
		  _uw lp;

		  /* Landing pad address is 31-bit pc-relative offset.  */
		  lp = selfrel_offset31 (data);
		  data++;
		  /* Save the exception data pointer.  */
		  ucbp->cleanup_cache.bitpattern[0] = (_uw) data;
		  if (!__cxa_begin_cleanup (ucbp))
		    return _URC_FAILURE;
		  /* Setup the VRS to enter the landing pad.  */
		  _Unwind_SetGR (context, R_PC, lp);
		  return _URC_INSTALL_CONTEXT;
		}
	      /* Cleanup not in range, or we are in stage 1.  */
	      data++;
	      break;

	    case 1:
	      /* Catch handler.  */
	      if (state == _US_VIRTUAL_UNWIND_FRAME)
		{
		  if (in_range)
		    {
		      /* Check for a barrier.  */
		      _uw rtti;
		      bool is_reference = (data[0] & uint32_highbit) != 0;
		      void *matched;
		      enum __cxa_type_match_result match_type;

		      /* Check for no-throw areas.  */
		      if (data[1] == (_uw) -2)
			return _URC_FAILURE;

		      /* The thrown object immediately follows the ECB.  */
		      matched = (void *)(ucbp + 1);
		      if (data[1] != (_uw) -1)
			{
			  /* Match a catch specification.  */
			  rtti = _Unwind_decode_typeinfo_ptr (0,
							      (_uw) &data[1]);
			  match_type = __cxa_type_match (ucbp,
							 (type_info *) rtti,
							 is_reference,
							 &matched);
			}
		      else
			match_type = ctm_succeeded;

		      if (match_type)
			{
			  ucbp->barrier_cache.sp =
			    _Unwind_GetGR (context, R_SP);
			  // ctm_succeeded_with_ptr_to_base really
			  // means _c_t_m indirected the pointer
			  // object.  We have to reconstruct the
			  // additional pointer layer by using a temporary.
			  if (match_type == ctm_succeeded_with_ptr_to_base)
			    {
			      ucbp->barrier_cache.bitpattern[2]
				= (_uw) matched;
			      ucbp->barrier_cache.bitpattern[0]
				= (_uw) &ucbp->barrier_cache.bitpattern[2];
			    }
			  else
			    ucbp->barrier_cache.bitpattern[0] = (_uw) matched;
			  ucbp->barrier_cache.bitpattern[1] = (_uw) data;
			  return _URC_HANDLER_FOUND;
			}
		    }
		  /* Handler out of range, or not matched.  */
		}
	      else if (ucbp->barrier_cache.sp == _Unwind_GetGR (context, R_SP)
		       && ucbp->barrier_cache.bitpattern[1] == (_uw) data)
		{
		  /* Matched a previous propagation barrier.  */
		  _uw lp;

		  /* Setup for entry to the handler.  */
		  lp = selfrel_offset31 (data);
		  _Unwind_SetGR (context, R_PC, lp);
		  _Unwind_SetGR (context, 0, (_uw) ucbp);
		  return _URC_INSTALL_CONTEXT;
		}
	      /* Catch handler not matched.  Advance to the next descriptor.  */
	      data += 2;
	      break;

	    case 2:
	      rtti_count = data[0] & 0x7fffffff;
	      /* Exception specification.  */
	      if (state == _US_VIRTUAL_UNWIND_FRAME)
		{
		  if (in_range && (!forced_unwind || !rtti_count))
		    {
		      /* Match against the exception specification.  */
		      _uw i;
		      _uw rtti;
		      void *matched;

		      for (i = 0; i < rtti_count; i++)
			{
			  matched = (void *)(ucbp + 1);
			  rtti = _Unwind_decode_typeinfo_ptr (0,
			      (_uw) &data[i + 1]);
			  if (__cxa_type_match (ucbp, (type_info *) rtti, 0,
						&matched))
			    break;
			}

		      if (i == rtti_count)
			{
			  /* Exception does not match the spec.  */
			  ucbp->barrier_cache.sp =
			    _Unwind_GetGR (context, R_SP);
			  ucbp->barrier_cache.bitpattern[0] = (_uw) matched;
			  ucbp->barrier_cache.bitpattern[1] = (_uw) data;
			  return _URC_HANDLER_FOUND;
			}
		    }
		  /* Handler out of range, or exception is permitted.  */
		}
	      else if (ucbp->barrier_cache.sp == _Unwind_GetGR (context, R_SP)
		       && ucbp->barrier_cache.bitpattern[1] == (_uw) data)
		{
		  /* Matched a previous propagation barrier.  */
		  _uw lp;
		  /* Record the RTTI list for __cxa_call_unexpected.  */
		  ucbp->barrier_cache.bitpattern[1] = rtti_count;
		  ucbp->barrier_cache.bitpattern[2] = 0;
		  ucbp->barrier_cache.bitpattern[3] = 4;
		  ucbp->barrier_cache.bitpattern[4] = (_uw) &data[1];

		  if (data[0] & uint32_highbit)
		    {
		      data += rtti_count + 1;
		      /* Setup for entry to the handler.  */
		      lp = selfrel_offset31 (data);
		      data++;
		      _Unwind_SetGR (context, R_PC, lp);
		      _Unwind_SetGR (context, 0, (_uw) ucbp);
		      return _URC_INSTALL_CONTEXT;
		    }
		  else
		    phase2_call_unexpected_after_unwind = 1;
		}
	      if (data[0] & uint32_highbit)
		data++;
	      data += rtti_count + 1;
	      break;

	    default:
	      /* Should never happen.  */
	      return _URC_FAILURE;
	    }
	  /* Finished processing this descriptor.  */
	}
    }

  if (id >= 3)
    {
      /* 24-bit ecoding */
      if (__gnu_unwind_24bit (context, uws.data, id == 4) != _URC_OK)
	return _URC_FAILURE;
    }
  else
    {
      if (__gnu_unwind_execute (context, &uws) != _URC_OK)
	return _URC_FAILURE;
    }
    
  if (phase2_call_unexpected_after_unwind)
    {
      /* Enter __cxa_unexpected as if called from the call site.  */
      _Unwind_SetGR (context, R_LR, _Unwind_GetGR (context, R_PC));
      _Unwind_SetGR (context, R_PC, (_uw) &__cxa_call_unexpected);
      return _URC_INSTALL_CONTEXT;
    }

  return _URC_CONTINUE_UNWIND;
}
