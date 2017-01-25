/* ARM EABI compliant unwinding routines.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

/* Misc constants.  */
#define R_IP	12
#define R_SP	13
#define R_LR	14
#define R_PC	15

#define VRS_PC(vrs) ((vrs)->core.r[R_PC])
#define VRS_SP(vrs) ((vrs)->core.r[R_SP])
#define VRS_RETURN(vrs) ((vrs)->core.r[R_LR])

struct core_regs
{
  _uw r[16];
};

/* We use normal integer types here to avoid the compiler generating
   coprocessor instructions.  */
struct vfp_regs
{
  _uw64 d[16];
  _uw pad;
};

struct vfpv3_regs
{
  /* Always populated via VSTM, so no need for the "pad" field from
     vfp_regs (which is used to store the format word for FSTMX).  */
  _uw64 d[16];
};

struct wmmxd_regs
{
  _uw64 wd[16];
};

struct wmmxc_regs
{
  _uw wc[4];
};

/* The ABI specifies that the unwind routines may only use core registers,
   except when actually manipulating coprocessor state.  This allows
   us to write one implementation that works on all platforms by
   demand-saving coprocessor registers.

   During unwinding we hold the coprocessor state in the actual hardware
   registers and allocate demand-save areas for use during phase1
   unwinding.  */

typedef struct
{
  /* The first fields must be the same as a phase2_vrs.  */
  _uw demand_save_flags;
  struct core_regs core;
  _uw prev_sp; /* Only valid during forced unwinding.  */
  struct vfp_regs vfp;
  struct vfpv3_regs vfp_regs_16_to_31;
  struct wmmxd_regs wmmxd;
  struct wmmxc_regs wmmxc;
} phase1_vrs;

#define DEMAND_SAVE_VFP 1	/* VFP state has been saved if not set */
#define DEMAND_SAVE_VFP_D 2	/* VFP state is for FLDMD/FSTMD if set */
#define DEMAND_SAVE_VFP_V3 4    /* VFPv3 state for regs 16 .. 31 has
                                   been saved if not set */
#define DEMAND_SAVE_WMMXD 8	/* iWMMXt data registers have been
				   saved if not set.  */
#define DEMAND_SAVE_WMMXC 16	/* iWMMXt control registers have been
				   saved if not set.  */

/* This must match the structure created by the assembly wrappers.  */
typedef struct
{
  _uw demand_save_flags;
  struct core_regs core;
} phase2_vrs;

/* Coprocessor register state manipulation functions.  */

/* Routines for FLDMX/FSTMX format...  */
void __gnu_Unwind_Save_VFP (struct vfp_regs * p);
void __gnu_Unwind_Restore_VFP (struct vfp_regs * p);
void __gnu_Unwind_Save_WMMXD (struct wmmxd_regs * p);
void __gnu_Unwind_Restore_WMMXD (struct wmmxd_regs * p);
void __gnu_Unwind_Save_WMMXC (struct wmmxc_regs * p);
void __gnu_Unwind_Restore_WMMXC (struct wmmxc_regs * p);

/* ...and those for FLDMD/FSTMD format...  */
void __gnu_Unwind_Save_VFP_D (struct vfp_regs * p);
void __gnu_Unwind_Restore_VFP_D (struct vfp_regs * p);

/* ...and those for VLDM/VSTM format, saving/restoring only registers
   16 through 31.  */
void __gnu_Unwind_Save_VFP_D_16_to_31 (struct vfpv3_regs * p);
void __gnu_Unwind_Restore_VFP_D_16_to_31 (struct vfpv3_regs * p);

/* Restore coprocessor state after phase1 unwinding.  */
static void
restore_non_core_regs (phase1_vrs * vrs)
{
  if ((vrs->demand_save_flags & DEMAND_SAVE_VFP) == 0)
    {
      if (vrs->demand_save_flags & DEMAND_SAVE_VFP_D)
        __gnu_Unwind_Restore_VFP_D (&vrs->vfp);
      else
        __gnu_Unwind_Restore_VFP (&vrs->vfp);
    }

  if ((vrs->demand_save_flags & DEMAND_SAVE_VFP_V3) == 0)
    __gnu_Unwind_Restore_VFP_D_16_to_31 (&vrs->vfp_regs_16_to_31);

  if ((vrs->demand_save_flags & DEMAND_SAVE_WMMXD) == 0)
    __gnu_Unwind_Restore_WMMXD (&vrs->wmmxd);
  if ((vrs->demand_save_flags & DEMAND_SAVE_WMMXC) == 0)
    __gnu_Unwind_Restore_WMMXC (&vrs->wmmxc);
}

#include "unwind-arm-common.inc"

/* ABI defined personality routines.  */
extern _Unwind_Reason_Code __aeabi_unwind_cpp_pr0 (_Unwind_State,
    _Unwind_Control_Block *, _Unwind_Context *);// __attribute__((weak));
extern _Unwind_Reason_Code __aeabi_unwind_cpp_pr1 (_Unwind_State,
    _Unwind_Control_Block *, _Unwind_Context *) __attribute__((weak));
extern _Unwind_Reason_Code __aeabi_unwind_cpp_pr2 (_Unwind_State,
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
      if (representation != _UVRSD_UINT32
	  || regno > 15)
	return _UVRSR_FAILED;
      *(_uw *) valuep = vrs->core.r[regno];
      return _UVRSR_OK;

    case _UVRSC_VFP:
    case _UVRSC_WMMXD:
    case _UVRSC_WMMXC:
      return _UVRSR_NOT_IMPLEMENTED;

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
      if (representation != _UVRSD_UINT32
	  || regno > 15)
	return _UVRSR_FAILED;

      vrs->core.r[regno] = *(_uw *) valuep;
      return _UVRSR_OK;

    case _UVRSC_VFP:
    case _UVRSC_WMMXD:
    case _UVRSC_WMMXC:
      return _UVRSR_NOT_IMPLEMENTED;

    default:
      return _UVRSR_FAILED;
    }
}


/* ABI defined function to pop registers off the stack.  */

_Unwind_VRS_Result _Unwind_VRS_Pop (_Unwind_Context *context,
				    _Unwind_VRS_RegClass regclass,
				    _uw discriminator,
				    _Unwind_VRS_DataRepresentation representation)
{
  phase1_vrs *vrs = (phase1_vrs *) context;

  switch (regclass)
    {
    case _UVRSC_CORE:
      {
	_uw *ptr;
	_uw mask;
	int i;

	if (representation != _UVRSD_UINT32)
	  return _UVRSR_FAILED;

	mask = discriminator & 0xffff;
	ptr = (_uw *) vrs->core.r[R_SP];
	/* Pop the requested registers.  */
	for (i = 0; i < 16; i++)
	  {
	    if (mask & (1 << i))
	      vrs->core.r[i] = *(ptr++);
	  }
	/* Writeback the stack pointer value if it wasn't restored.  */
	if ((mask & (1 << R_SP)) == 0)
	  vrs->core.r[R_SP] = (_uw) ptr;
      }
      return _UVRSR_OK;

    case _UVRSC_VFP:
      {
	_uw start = discriminator >> 16;
	_uw count = discriminator & 0xffff;
	struct vfp_regs tmp;
	struct vfpv3_regs tmp_16_to_31;
	int tmp_count;
	_uw *sp;
	_uw *dest;
        int num_vfpv3_regs = 0;

        /* We use an approximation here by bounding _UVRSD_DOUBLE
           register numbers at 32 always, since we can't detect if
           VFPv3 isn't present (in such a case the upper limit is 16).  */
	if ((representation != _UVRSD_VFPX && representation != _UVRSD_DOUBLE)
            || start + count > (representation == _UVRSD_VFPX ? 16 : 32)
            || (representation == _UVRSD_VFPX && start >= 16))
	  return _UVRSR_FAILED;

        /* Check if we're being asked to pop VFPv3-only registers
           (numbers 16 through 31).  */
	if (start >= 16)
          num_vfpv3_regs = count;
        else if (start + count > 16)
          num_vfpv3_regs = start + count - 16;

        if (num_vfpv3_regs && representation != _UVRSD_DOUBLE)
          return _UVRSR_FAILED;

	/* Demand-save coprocessor registers for stage1.  */
	if (start < 16 && (vrs->demand_save_flags & DEMAND_SAVE_VFP))
	  {
	    vrs->demand_save_flags &= ~DEMAND_SAVE_VFP;

            if (representation == _UVRSD_DOUBLE)
              {
                /* Save in FLDMD/FSTMD format.  */
	        vrs->demand_save_flags |= DEMAND_SAVE_VFP_D;
	        __gnu_Unwind_Save_VFP_D (&vrs->vfp);
              }
            else
              {
                /* Save in FLDMX/FSTMX format.  */
	        vrs->demand_save_flags &= ~DEMAND_SAVE_VFP_D;
	        __gnu_Unwind_Save_VFP (&vrs->vfp);
              }
	  }

        if (num_vfpv3_regs > 0
            && (vrs->demand_save_flags & DEMAND_SAVE_VFP_V3))
	  {
	    vrs->demand_save_flags &= ~DEMAND_SAVE_VFP_V3;
            __gnu_Unwind_Save_VFP_D_16_to_31 (&vrs->vfp_regs_16_to_31);
	  }

	/* Restore the registers from the stack.  Do this by saving the
	   current VFP registers to a memory area, moving the in-memory
	   values into that area, and restoring from the whole area.
	   For _UVRSD_VFPX we assume FSTMX standard format 1.  */
        if (representation == _UVRSD_VFPX)
  	  __gnu_Unwind_Save_VFP (&tmp);
        else
          {
	    /* Save registers 0 .. 15 if required.  */
            if (start < 16)
              __gnu_Unwind_Save_VFP_D (&tmp);

	    /* Save VFPv3 registers 16 .. 31 if required.  */
            if (num_vfpv3_regs)
  	      __gnu_Unwind_Save_VFP_D_16_to_31 (&tmp_16_to_31);
          }

	/* Work out how many registers below register 16 need popping.  */
	tmp_count = num_vfpv3_regs > 0 ? 16 - start : count;

	/* Copy registers below 16, if needed.
	   The stack address is only guaranteed to be word aligned, so
	   we can't use doubleword copies.  */
	sp = (_uw *) vrs->core.r[R_SP];
        if (tmp_count > 0)
          {
	    tmp_count *= 2;
	    dest = (_uw *) &tmp.d[start];
	    while (tmp_count--)
	      *(dest++) = *(sp++);
          }

	/* Copy VFPv3 registers numbered >= 16, if needed.  */
        if (num_vfpv3_regs > 0)
          {
            /* num_vfpv3_regs is needed below, so copy it.  */
            int tmp_count_2 = num_vfpv3_regs * 2;
            int vfpv3_start = start < 16 ? 16 : start;

	    dest = (_uw *) &tmp_16_to_31.d[vfpv3_start - 16];
	    while (tmp_count_2--)
	      *(dest++) = *(sp++);
          }

	/* Skip the format word space if using FLDMX/FSTMX format.  */
	if (representation == _UVRSD_VFPX)
	  sp++;

	/* Set the new stack pointer.  */
	vrs->core.r[R_SP] = (_uw) sp;

	/* Reload the registers.  */
        if (representation == _UVRSD_VFPX)
  	  __gnu_Unwind_Restore_VFP (&tmp);
        else
          {
	    /* Restore registers 0 .. 15 if required.  */
            if (start < 16)
              __gnu_Unwind_Restore_VFP_D (&tmp);

	    /* Restore VFPv3 registers 16 .. 31 if required.  */
            if (num_vfpv3_regs > 0)
  	      __gnu_Unwind_Restore_VFP_D_16_to_31 (&tmp_16_to_31);
          }
      }
      return _UVRSR_OK;

    case _UVRSC_WMMXD:
      {
	_uw start = discriminator >> 16;
	_uw count = discriminator & 0xffff;
	struct wmmxd_regs tmp;
	_uw *sp;
	_uw *dest;

	if ((representation != _UVRSD_UINT64) || start + count > 16)
	  return _UVRSR_FAILED;

	if (vrs->demand_save_flags & DEMAND_SAVE_WMMXD)
	  {
	    /* Demand-save resisters for stage1.  */
	    vrs->demand_save_flags &= ~DEMAND_SAVE_WMMXD;
	    __gnu_Unwind_Save_WMMXD (&vrs->wmmxd);
	  }

	/* Restore the registers from the stack.  Do this by saving the
	   current WMMXD registers to a memory area, moving the in-memory
	   values into that area, and restoring from the whole area.  */
	__gnu_Unwind_Save_WMMXD (&tmp);

	/* The stack address is only guaranteed to be word aligned, so
	   we can't use doubleword copies.  */
	sp = (_uw *) vrs->core.r[R_SP];
	dest = (_uw *) &tmp.wd[start];
	count *= 2;
	while (count--)
	  *(dest++) = *(sp++);

	/* Set the new stack pointer.  */
	vrs->core.r[R_SP] = (_uw) sp;

	/* Reload the registers.  */
	__gnu_Unwind_Restore_WMMXD (&tmp);
      }
      return _UVRSR_OK;

    case _UVRSC_WMMXC:
      {
	int i;
	struct wmmxc_regs tmp;
	_uw *sp;

	if ((representation != _UVRSD_UINT32) || discriminator > 16)
	  return _UVRSR_FAILED;

	if (vrs->demand_save_flags & DEMAND_SAVE_WMMXC)
	  {
	    /* Demand-save resisters for stage1.  */
	    vrs->demand_save_flags &= ~DEMAND_SAVE_WMMXC;
	    __gnu_Unwind_Save_WMMXC (&vrs->wmmxc);
	  }

	/* Restore the registers from the stack.  Do this by saving the
	   current WMMXC registers to a memory area, moving the in-memory
	   values into that area, and restoring from the whole area.  */
	__gnu_Unwind_Save_WMMXC (&tmp);

	sp = (_uw *) vrs->core.r[R_SP];
	for (i = 0; i < 4; i++)
	  if (discriminator & (1 << i))
	    tmp.wc[i] = *(sp++);

	/* Set the new stack pointer.  */
	vrs->core.r[R_SP] = (_uw) sp;

	/* Reload the registers.  */
	__gnu_Unwind_Restore_WMMXC (&tmp);
      }
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

  offset = *p;
  /* Sign extend to 32 bits.  */
  if (offset & (1 << 30))
    offset |= 1u << 31;
  else
    offset &= ~(1u << 31);

  return offset + (_uw) p;
}

static _uw
__gnu_unwind_get_pr_addr (int idx)
{
  switch (idx)
    {
    case 0:
      return (_uw) &__aeabi_unwind_cpp_pr0;

    case 1:
      return (_uw) &__aeabi_unwind_cpp_pr1;

    case 2:
      return (_uw) &__aeabi_unwind_cpp_pr2;

    default:
      return 0;
    } 
}

/* ABI defined personality routine entry points.  */

_Unwind_Reason_Code
__aeabi_unwind_cpp_pr0 (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context)
{
  return __gnu_unwind_pr_common (state, ucbp, context, 0);
}

_Unwind_Reason_Code
__aeabi_unwind_cpp_pr1 (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context)
{
  return __gnu_unwind_pr_common (state, ucbp, context, 1);
}

_Unwind_Reason_Code
__aeabi_unwind_cpp_pr2 (_Unwind_State state,
			_Unwind_Control_Block *ucbp,
			_Unwind_Context *context)
{
  return __gnu_unwind_pr_common (state, ucbp, context, 2);
}
