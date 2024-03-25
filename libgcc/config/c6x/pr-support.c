/* C6X ABI compliant unwinding routines
   Copyright (C) 2011-2024 Free Software Foundation, Inc.
 
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

typedef struct _ZSt9type_info type_info; /* This names C++ type_info type */

/* Misc constants.  */
#define R_A0 0
#define R_A1 1
#define R_A2 2
#define R_A3 3
#define R_A4 4
#define R_A5 5
#define R_A6 6
#define R_A7 7
#define R_A8 8
#define R_A9 9
#define R_A10 10
#define R_A11 11
#define R_A12 12
#define R_A13 13
#define R_A14 14
#define R_A15 15
#define R_B0 16
#define R_B1 17
#define R_B2 18
#define R_B3 19
#define R_B4 20
#define R_B5 21
#define R_B6 22
#define R_B7 23
#define R_B8 24
#define R_B9 25
#define R_B10 26
#define R_B11 27
#define R_B12 28
#define R_B13 29
#define R_B14 30
#define R_B15 31

#define R_SP R_B15
#define R_PC 33

#define uint32_highbit (((_uw) 1) << 31)

void __attribute__((weak)) __cxa_call_unexpected(_Unwind_Control_Block *ucbp);

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

/* Calculate the address encoded by a 31-bit self-relative offset at address
   P.  Copy of routine in unwind-arm.c.  */

static inline _uw
selfrel_offset31 (const _uw *p)
{
  _uw offset;

  offset = *p;
  /* Sign extend to 32 bits.  */
  if (offset & (1 << 30))
    offset |= 1u << 31;

  return offset + (_uw) p;
}


/* Personality routine helper functions.  */

#define CODE_FINISH (0xe7)

/* Return the next byte of unwinding information, or CODE_FINISH if there is
   no data remaining.  */
static inline _uw8
next_unwind_byte (__gnu_unwind_state * uws)
{
  _uw8 b;

  if (uws->bytes_left == 0)
    {
      /* Load another word */
      if (uws->words_left == 0)
	return CODE_FINISH; /* Nothing left.  */
      uws->words_left--;
      uws->data = *(uws->next++);
      uws->bytes_left = 3;
    }
  else
    uws->bytes_left--;

  /* Extract the most significant byte.  */
  b = (uws->data >> 24) & 0xff;
  uws->data <<= 8;
  return b;
}

static void
unwind_restore_pair (_Unwind_Context * context, int reg, _uw *ptr)
{
#ifdef _BIG_ENDIAN
  _Unwind_VRS_Set (context, _UVRSC_CORE, reg, _UVRSD_UINT32, ptr + 1);
  _Unwind_VRS_Set (context, _UVRSC_CORE, reg + 1, _UVRSD_UINT32, ptr);
#else
  _Unwind_VRS_Set (context, _UVRSC_CORE, reg, _UVRSD_UINT32, ptr);
  _Unwind_VRS_Set (context, _UVRSC_CORE, reg + 1, _UVRSD_UINT32, ptr + 1);
#endif
}

static const int
unwind_frame_regs[13] = 
{
  R_A15, R_B15, R_B14, R_B13, R_B12, R_B11, R_B10, R_B3,
  R_A14, R_A13, R_A12, R_A11, R_A10
};

static void
pop_compact_frame (_Unwind_Context * context, _uw mask, _uw *ptr, int inc_sp)
{
  int size;
  _uw test;
  int i, regno, nregs;

  size = 0;
  nregs = __builtin_popcount (mask);
  for (i = 0; i < 13; i++)
    {
      test = 1 << i;
      if ((mask & test) == 0)
	continue;

      regno = unwind_frame_regs[12 - i];

      if (i < 12 && nregs > 2
	  && (mask & (test << 1)) != 0
	  && unwind_frame_regs[11 - i] == regno + 1
	  && (regno & 1) == 0)
	{
	  i++;
	  nregs--;
	}

      nregs--;
      size += 2;
    }

  if (!inc_sp)
    ptr -= size;

  /* SP points just past the end of the stack.  */
  ptr += 2;
  nregs = __builtin_popcount (mask);
  for (i = 0; i < 13; i++)
    {
      test = 1 << i;
      if ((mask & test) == 0)
	continue;

      regno = unwind_frame_regs[12 - i];

      if (i < 12 && nregs > 2
	  && (mask & (test << 1)) != 0
	  && unwind_frame_regs[11 - i] == regno + 1
	  && (regno & 1) == 0)
	{
	  /* Register pair.  */
	  unwind_restore_pair (context, regno, ptr);
	  i++;
	  nregs--;
	}
      else
	{
	  /* Single register with padding.  */
	  _Unwind_VRS_Set (context, _UVRSC_CORE, regno, _UVRSD_UINT32, ptr);
	}

      nregs--;
      ptr += 2;
    }

  ptr -= 2;
  if ((mask & (1 << 11)) == 0)
    _Unwind_VRS_Set (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &ptr);
}

static void
pop_frame (_Unwind_Context * context, _uw mask, _uw *ptr, int inc_sp)
{
  int i;
  int regno;
  int nregs;

  nregs = __builtin_popcount (mask);

  if (!inc_sp)
    ptr -= nregs;
  else if (nregs & 1)
    ptr++;

  ptr++;
  for (i = 0; i < 13; i++)
    {
      if ((mask & (1 << i)) == 0)
	continue;
      regno = unwind_frame_regs[12 - i];
      if (i < 12 && unwind_frame_regs[11 - i] == (regno + 1)
	  && (mask & (1 << (i + 1))) != 0
	  && (((_uw)ptr) & 4) == 0
	  && (regno & 1) == 0)
	{
	  unwind_restore_pair (context, regno, ptr);
	  i++;
	  ptr += 2;
	}
      else
	{
	  _Unwind_VRS_Set (context, _UVRSC_CORE, regno, _UVRSD_UINT32,
			   ptr);
	  ptr++;
	}
    }

  ptr--;
  if ((mask & (1 << 11)) == 0)
    _Unwind_VRS_Set (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &ptr);
}

/* Unwind a 24-bit encoded frame.  */
_Unwind_Reason_Code
__gnu_unwind_24bit (_Unwind_Context * context, _uw data, int compact)
{
  _uw offset;
  _uw mask;
  _uw *ptr;
  _uw tmp;
  int ret_reg = unwind_frame_regs[data & 0xf];

  if (ret_reg != R_B3)
    {
      _Unwind_VRS_Get (context, _UVRSC_CORE, unwind_frame_regs[data & 0xf],
		       _UVRSD_UINT32, &tmp);
      _Unwind_VRS_Set (context, _UVRSC_CORE, R_B3, _UVRSD_UINT32, &tmp);
    }

  mask = (data >> 4) & 0x1fff;

  offset = (data >> 17) & 0x7f;
  if (offset == 0x7f)
    _Unwind_VRS_Get (context, _UVRSC_CORE, R_A15, _UVRSD_UINT32, &ptr);
  else
    {
      _Unwind_VRS_Get (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &ptr);
      ptr += offset * 2;
    }


  if (compact)
    pop_compact_frame (context, mask, ptr, offset != 0x7f);
  else
    pop_frame (context, mask, ptr, offset != 0x7f);

  _Unwind_VRS_Get (context, _UVRSC_CORE, R_B3, _UVRSD_UINT32, &tmp);
  _Unwind_VRS_Set (context, _UVRSC_CORE, R_PC, _UVRSD_UINT32, &tmp);

  return _URC_OK;
}

static void
unwind_pop_rts (_Unwind_Context * context)
{
  _uw *ptr;

  _Unwind_VRS_Get (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &ptr);
#ifdef _BIG_ENDIAN
  _Unwind_VRS_Set (context, _UVRSC_CORE, R_B3, _UVRSD_UINT32, ptr + 1);
#else
  _Unwind_VRS_Set (context, _UVRSC_CORE, R_B3, _UVRSD_UINT32, ptr + 2);
#endif
  ptr += 3;
  unwind_restore_pair (context, R_A10, ptr);
  ptr += 2;
  unwind_restore_pair (context, R_B10, ptr);
  ptr += 2;
  unwind_restore_pair (context, R_A12, ptr);
  ptr += 2;
  unwind_restore_pair (context, R_B12, ptr);
  ptr += 2;
  unwind_restore_pair (context, R_A14, ptr);
  ptr += 2;
  _Unwind_VRS_Set (context, _UVRSC_CORE, R_B14, _UVRSD_UINT32, ptr);
  _Unwind_VRS_Set (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &ptr);
  /* PC will be set by implicit RETURN opcode.  */
}

/* Execute the unwinding instructions described by UWS.  */
_Unwind_Reason_Code
__gnu_unwind_execute (_Unwind_Context * context, __gnu_unwind_state * uws)
{
  _uw op;
  int inc_sp;
  _uw reg;
  _uw *ptr;

  inc_sp = 1;
  for (;;)
    {
      op = next_unwind_byte (uws);
      if (op == CODE_FINISH)
	{
	  /* Drop out of the loop.  */
	  break;
	}
      if ((op & 0xc0) == 0)
	{
	  /* sp += (imm6 << 3) + 8.  */
	  _uw offset;

	  offset = ((op & 0x3f) << 3) + 8;
	  _Unwind_VRS_Get (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &reg);
	  reg += offset;
	  _Unwind_VRS_Set (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &reg);
	  continue;
	}

      if (op == 0xd2)
	{
	  /* vsp = vsp + 0x204 + (uleb128 << 2).  */
	  int shift;

	  _Unwind_VRS_Get (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &reg);
	  op = next_unwind_byte (uws);
	  shift = 3;
	  while (op & 0x80)
	    {
	      reg += ((op & 0x7f) << shift);
	      shift += 7;
	      op = next_unwind_byte (uws);
	    }
	  reg += ((op & 0x7f) << shift) + 0x408;
	  _Unwind_VRS_Set (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &reg);
	  continue;
	}

      if ((op & 0xe0) == 0x80)
	{
	  /* POP bitmask */
	  _uw mask = ((op & 0x1f) << 8) | next_unwind_byte (uws);

	  if (mask == 0)
	    {
	      /* CANTUNWIND */
	      return _URC_FAILURE;
	    }

	  _Unwind_VRS_Get (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &ptr);
	  pop_frame (context, mask, ptr, inc_sp);
	  continue;
	}

      if ((op & 0xe0) == 0xa0)
	{
	  /* POP bitmask (compact) */
	  _uw mask = ((op & 0x1f) << 8) | next_unwind_byte (uws);

	  _Unwind_VRS_Get (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &ptr);
	  pop_compact_frame (context, mask, ptr, inc_sp);
	  continue;
	}

      if ((op & 0xf0) == 0xc0)
	{
	  /* POP registers */
	  int nregs = op & 0xf;

	  _Unwind_VRS_Get (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &ptr);
	  while (nregs > 0)
	    {
	      op = next_unwind_byte (uws);
	      if ((op >> 4) != 0xf)
		{
		  reg = unwind_frame_regs[op >> 4];
		  _Unwind_VRS_Set (context, _UVRSC_CORE, reg, _UVRSD_UINT32,
				   ptr);
		  nregs--;
		}
	      ptr--;
	      if ((op & 0xf) != 0xf)
		{
		  reg = unwind_frame_regs[op & 0xf];
		  _Unwind_VRS_Set (context, _UVRSC_CORE, reg, _UVRSD_UINT32,
				   ptr);
		  nregs--;
		}
	      ptr--;
	    }

	  continue;
	}

      if (op == 0xd0)
	{
	  /* MV FP, SP */
	  inc_sp = 0;
	  _Unwind_VRS_Get (context, _UVRSC_CORE, R_A15, _UVRSD_UINT32, &reg);
	  _Unwind_VRS_Set (context, _UVRSC_CORE, R_SP, _UVRSD_UINT32, &reg);
	  continue;
	}

      if (op == 0xd1)
	{
	  /* __cx6abi_pop_rts */
	  unwind_pop_rts (context);
	  break;
	}

      if ((op & 0xf0) == 0xe0)
	{
	  /* B3 = reg.  RETURN case already handled above.  */
	  int regno = unwind_frame_regs[op & 0xf];

	  _Unwind_VRS_Get (context, _UVRSC_CORE, regno, _UVRSD_UINT32, &reg);
	  _Unwind_VRS_Set (context, _UVRSC_CORE, R_B3, _UVRSD_UINT32, &reg);
	  continue;
	}
      
      /* Reserved.  */
      return _URC_FAILURE;
    }

  /* Implicit RETURN.  */
  _Unwind_VRS_Get (context, _UVRSC_CORE, R_B3, _UVRSD_UINT32, &reg);
  _Unwind_VRS_Set (context, _UVRSC_CORE, R_PC, _UVRSD_UINT32, &reg);
  return _URC_OK;
}


/* Execute the unwinding instructions associated with a frame.  UCBP and
   CONTEXT are the current exception object and virtual CPU state
   respectively.  */

_Unwind_Reason_Code
__gnu_unwind_frame (_Unwind_Control_Block * ucbp, _Unwind_Context * context)
{
  _uw *ptr;
  __gnu_unwind_state uws;

  ptr = (_uw *) ucbp->pr_cache.ehtp;
  /* Skip over the personality routine address.  */
  ptr++;
  /* Setup the unwinder state.  */
  uws.data = (*ptr) << 8;
  uws.next = ptr + 1;
  uws.bytes_left = 3;
  uws.words_left = ((*ptr) >> 24) & 0xff;

  return __gnu_unwind_execute (context, &uws);
}

/* Data segment base pointer corresponding to the function catching
   the exception.  */

_Unwind_Ptr
_Unwind_GetDataRelBase (_Unwind_Context *context)
{
  return _Unwind_GetGR (context, R_B14);
}

/* This should never be used.  */

_Unwind_Ptr
_Unwind_GetTextRelBase (_Unwind_Context *context __attribute__ ((unused)))
{
  abort ();
}

/* Only used by gcc personality routines, so can rely on a value they hid
   there earlier.  */
_Unwind_Ptr
_Unwind_GetRegionStart (_Unwind_Context *context)
{
  _Unwind_Control_Block *ucbp;
 
  ucbp = (_Unwind_Control_Block *) _Unwind_GetGR (context, UNWIND_POINTER_REG);
  return (_Unwind_Ptr) ucbp->pr_cache.fnstart;
}

void *
_Unwind_GetLanguageSpecificData (_Unwind_Context *context)
{
  _Unwind_Control_Block *ucbp;
  _uw *ptr;
 
  ucbp = (_Unwind_Control_Block *) _Unwind_GetGR (context, UNWIND_POINTER_REG);
  ptr = (_uw *) ucbp->pr_cache.ehtp;
  /* Skip the personality routine address.  */
  ptr++;
  /* Skip the unwind opcodes.  */
  ptr += (((*ptr) >> 24) & 0xff) + 1;

  return ptr;
}
