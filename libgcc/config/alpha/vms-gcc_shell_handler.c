/* Static condition handler for Alpha/VMS.
   Copyright (C) 2005-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

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

/* This file implements __gcc_shell_handler, the static VMS condition handler
   used as the indirection wrapper around user level handlers installed with
   establish_vms_condition_handler GCC builtin.

   [ABI] in comments refers to the "HP OpenVMS calling standard" document
   dated January 2005.  */

#include <vms/chfdef.h>
#include <vms/pdscdef.h>
#include <vms/ssdef.h>

typedef void * ADDR;
typedef unsigned long long REG;

#define REG_AT(addr) (*(REG *)(addr))

/* Compute pointer to procedure descriptor (Procedure Value) from Frame
   Pointer FP, according to the rules in [ABI-3.5.1 Current Procedure].  */
#define PV_FOR(FP) \
  (((FP) != 0) \
    ? (((REG_AT (FP) & 0x7) == 0) ? *(PDSCDEF **)(FP) : (PDSCDEF *)(FP)) : 0)

long
__gcc_shell_handler (struct chf$signal_array *sig_arr,
		     struct chf$mech_array *mech_arr);

/* Helper for __gcc_shell_handler.  Fetch the pointer to procedure currently
   registered as the VMS condition handler for the live function with a frame
   pointer FP.  */

static ADDR
get_dyn_handler_pointer (REG fp)
{
  /* From the frame pointer we find the procedure descriptor, and fetch
     the handler_data field from there.  This field contains the offset
     from FP at which the address of the currently installed handler is
     to be found.  */
  
  PDSCDEF * pd = PV_FOR (fp);
  /* Procedure descriptor pointer for the live subprogram with FP as the frame
     pointer, and to which _gcc_shell_handler is attached as a condition
     handler.  */

  REG handler_slot_offset;
  /* Offset from FP at which the address of the currently established real
     condition handler is to be found.  This offset is available from the
     handler_data field of the procedure descriptor.  */

  REG handler_data_offset;
  /* The handler_data field position in the procedure descriptor, which
     depends on the kind of procedure at hand.  */

  switch (pd->pdsc$w_flags & 0xf)
    {
    case PDSC$K_KIND_FP_STACK:    /* [3.4.2 PD for stack frame procedures]  */
      handler_data_offset = 40;
      break;
	
    case PDSC$K_KIND_FP_REGISTER: /* [3.4.5 PD for reg frame procedures]  */
      handler_data_offset = 32;
      break;
      
    default:
      handler_data_offset = 0;
      break;
    }

  /* If we couldn't determine the handler_data field position, give up.  */
  if (handler_data_offset == 0)
    return 0;

  /* Otherwise, fetch the fp offset at which the real handler address is to be
     found, then fetch and return the latter in turn.  */
     
  handler_slot_offset = REG_AT ((REG)pd + handler_data_offset);

  return (ADDR) REG_AT (fp + handler_slot_offset);
}

/* The static VMS condition handler for GCC code.  Fetch the address of the
   currently established condition handler, then resignal if there is none or
   call the handler with the VMS condition arguments.  */

long
__gcc_shell_handler (struct chf$signal_array *sig_arr,
		     struct chf$mech_array *mech_arr)
{
  long ret;
  long (*user_handler) (struct chf$signal_array *, struct chf$mech_array *);

  user_handler = get_dyn_handler_pointer (mech_arr->chf$q_mch_frame);
  if (!user_handler)
    ret = SS$_RESIGNAL;
  else
    ret = user_handler (sig_arr, mech_arr);

  return ret;
}
   
