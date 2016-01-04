/* C6X EABI compliant unwinding routines.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.

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

/* Misc constants.  */
#define NUM_SAVED_REGS 32
#define R_B0 16
#define R_B3 (R_B0 + 3)
#define R_B15 (R_B0 + 15)
#define R_SP R_B15
#define R_LR R_B3
#define R_PC 33

#define VRS_PC(vrs) ((vrs)->core.pc)
#define VRS_SP(vrs) ((vrs)->core.reg[R_SP])
#define VRS_RETURN(vrs) ((vrs)->core.reg[R_B3])

struct core_regs
{
  _uw reg[NUM_SAVED_REGS];
  _uw pc;
};

typedef struct
{
  /* The first fields must be the same as a phase2_vrs.  */
  _uw demand_save_flags; /* Currently always zero.  */
  struct core_regs core;
  _uw prev_sp; /* Only valid during forced unwinding.  */
} phase1_vrs;

/* This must match the structure created by the assembly wrappers.  */
typedef struct
{
  _uw demand_save_flags;
  struct core_regs core;
} phase2_vrs;

/* Coprocessor register state manipulation functions.  */

/* Restore coprocessor state after phase1 unwinding.  */
static void
restore_non_core_regs (phase1_vrs * vrs __attribute__((unused)))
{
}

#include "unwind-arm-common.inc"

/* ABI defined personality routines.  */
extern _Unwind_Reason_Code __c6xabi_unwind_cpp_pr0 (_Unwind_State,
    _Unwind_Control_Block *, _Unwind_Context *);// __attribute__((weak));
extern _Unwind_Reason_Code __c6xabi_unwind_cpp_pr1 (_Unwind_State,
    _Unwind_Control_Block *, _Unwind_Context *) __attribute__((weak));
extern _Unwind_Reason_Code __c6xabi_unwind_cpp_pr2 (_Unwind_State,
    _Unwind_Control_Block *, _Unwind_Context *) __attribute__((weak));
extern _Unwind_Reason_Code __c6xabi_unwind_cpp_pr3 (_Unwind_State,
    _Unwind_Control_Block *, _Unwind_Context *) __attribute__((weak));
extern _Unwind_Reason_Code __c6xabi_unwind_cpp_pr4 (_Unwind_State,
    _Unwind_Control_Block *, _Unwind_Context *) __attribute__((weak));

/* ABI defined routine to store a virtual register to memory.  */

_Unwind_VRS_Result _Unwind_VRS_Get (_Unwind_Context *context,
				    _Unwind_VRS_RegClass regclass,
				    _uw regno,
				    _Unwind_VRS_DataRepresentation representation,
				    void *valuep)
{
  phase1_vrs *vrs = (phase1_vrs *) context;

  switch (regclass)
    {
    case _UVRSC_CORE:
      if (representation != _UVRSD_UINT32)
	return _UVRSR_FAILED;
      if (regno == R_PC)
	{
	  *(_uw *) valuep = vrs->core.pc;
	  return _UVRSR_OK;
	}
      if (regno >= NUM_SAVED_REGS)
	return _UVRSR_FAILED;
      *(_uw *) valuep = vrs->core.reg[regno];
      return _UVRSR_OK;

    default:
      return _UVRSR_FAILED;
    }
}


/* ABI defined function to load a virtual register from memory.  */

_Unwind_VRS_Result _Unwind_VRS_Set (_Unwind_Context *context,
				    _Unwind_VRS_RegClass regclass,
				    _uw regno,
				    _Unwind_VRS_DataRepresentation representation,
				    void *valuep)
{
  phase1_vrs *vrs = (phase1_vrs *) context;

  switch (regclass)
    {
    case _UVRSC_CORE:
      if (representation != _UVRSD_UINT32)
	return _UVRSR_FAILED;
      if (regno == R_PC)
	{
	  vrs->core.pc = *(_uw *) valuep;
	  return _UVRSR_OK;
	}
      if (regno >= NUM_SAVED_REGS)
	return _UVRSR_FAILED;

      vrs->core.reg[regno] = *(_uw *) valuep;
      return _UVRSR_OK;

    default:
      return _UVRSR_FAILED;
    }
}


/* Core unwinding functions.  */

/* Calculate the address encoded by a 31-bit self-relative offset at address
   P.  */
static inline _uw
selfrel_offset31 (const _uw *p)
{
  _uw offset;

  offset = *p << 1;
  return offset + (_uw) p;
}


static _uw
__gnu_unwind_get_pr_addr (int idx)
{
  switch (idx)
    {
    case 0:
      return (_uw) &__c6xabi_unwind_cpp_pr0;

    case 1:
      return (_uw) &__c6xabi_unwind_cpp_pr1;

    case 2:
      return (_uw) &__c6xabi_unwind_cpp_pr2;

    case 3:
      return (_uw) &__c6xabi_unwind_cpp_pr3;

    case 4:
      return (_uw) &__c6xabi_unwind_cpp_pr4;

    default:
       return 0;
    }
}


/* ABI defined personality routine entry points.  */

_Unwind_Reason_Code
__c6xabi_unwind_cpp_pr0 (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context)
{
  return __gnu_unwind_pr_common (state, ucbp, context, 0);
}

_Unwind_Reason_Code
__c6xabi_unwind_cpp_pr1 (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context)
{
  return __gnu_unwind_pr_common (state, ucbp, context, 1);
}

_Unwind_Reason_Code
__c6xabi_unwind_cpp_pr2 (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context)
{
  return __gnu_unwind_pr_common (state, ucbp, context, 2);
}

_Unwind_Reason_Code
__c6xabi_unwind_cpp_pr3 (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context)
{
  return __gnu_unwind_pr_common (state, ucbp, context, 3);
}

_Unwind_Reason_Code
__c6xabi_unwind_cpp_pr4 (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context)
{
  return __gnu_unwind_pr_common (state, ucbp, context, 4);
}
