/* Copyright (C) 2004-2016 Free Software Foundation, Inc.
   Contributed by Douglas B Rupp <rupp@gnat.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Locate the FDE entry for a given address, using VMS Starlet routines
   to avoid register/deregister calls at DSO load/unload.  */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include "unwind-ia64.h"

#include <ossddef.h>
#ifndef SS$_NORMAL
#define SS$_NORMAL 1
#endif

#define UNW_IVMS_MODE(HEADER) (((HEADER) >> 44) & 0x3L)

typedef struct
{
  unw_word start_offset;
  unw_word end_offset;
  unw_word info_offset;
  unw_word gp_value;
}  vms_unw_table_entry;

typedef unsigned long long uqword;

/* ENTRY is the unwind table entry found for a PC part of call chain we're
   unwinding through.  Return whether we should force the generic unwinder
   to resort to "fallback" processing.  */
   
static int
force_fallback_processing_for (void * pc, vms_unw_table_entry * entry)
{
  static int eh_debug = -1;

  uqword * unw_info_block = (uqword *)entry->info_offset;
  uqword header = *unw_info_block;

  /* We need to force fallback processing in two cases:

     1/ The exception dispatch frame, since only our fallback
        processing knows how to properly unwind through it, and

     2/ A bottom of stack frame, since only our fallback processing
        will ensure we don't try to unwind further past it, which
        would get us into unknown territory and likely cause a severe
        crash along the way.

     The two cases are indicated by non-default values for specific
     bits in the OS Specific Data (OSSD) General Information block
     associated with such frames.  */

  ossddef * ossd;

  if (eh_debug == -1)
    {
      char * EH_DEBUG = getenv ("EH_DEBUG");
      eh_debug = EH_DEBUG ? atoi (EH_DEBUG) : 0;
    }

  if (eh_debug)
    {
      printf ("pc @ 0x%p, block @ 0x%p, header = 0x%016llx\n",
	      pc, unw_info_block, header);
      printf ("mode = %d, length = %ld, handler = %d\n",
	      (int)UNW_IVMS_MODE (header), UNW_LENGTH (header),
	      UNW_FLAG_EHANDLER (header) || UNW_FLAG_EHANDLER (header));
    }

  /* An OSSD block is there for IVMS_MODE == 3 only.  */
  if (UNW_IVMS_MODE (header) != 3)
    return 0;

  /* The OSSD block is found past the header, unwind descriptor area
     and condition handler pointer, if any.  */  
  ossd = (ossddef *)
    /* Beware: uqword pointer arithmetic below.  */
    (unw_info_block
     + 1
     + UNW_LENGTH (header)
     + (UNW_FLAG_EHANDLER (header) || UNW_FLAG_EHANDLER (header)));

  /* "A General Information segment may be omitted if all of its fields
      would have their default values.  If a General Information segment
      is present, it must be the first in the OSSD area."  So ...  */
  
  if (eh_debug)
    printf ("ossd @ 0x%p\n", ossd);
      
  if (eh_debug && ossd->ossd$v_type == OSSD$K_GENERAL_INFO)
    printf ("exc_frame = %d - bot_frame = %d - base_frame = %d\n",
	    ossd->ossd$v_exception_frame, 
	    ossd->ossd$v_bottom_of_stack,
	    ossd->ossd$v_base_frame);
				
  return
    ossd->ossd$v_type == OSSD$K_GENERAL_INFO
    && (ossd->ossd$v_exception_frame
	|| ossd->ossd$v_bottom_of_stack || ossd->ossd$v_base_frame);
}

/* Return a pointer to the unwind table entry for the function
   containing PC, 0 if we cannot find an entry or if the one we find
   calls for fallback processing.  */

struct unw_table_entry *
_Unwind_FindTableEntry (void *pc, unw_word *segment_base,
                        unw_word *gp, struct unw_table_entry *ent)
{
  vms_unw_table_entry vueblock;

  if (SYS$GET_UNWIND_ENTRY_INFO (pc, &vueblock, 0) != SS$_NORMAL)
    return 0;

  /* If there is no unwind information, use fallback.  */
  if (vueblock.info_offset == 0)
    return 0;

  /* If we need to force fallback processing, just pretend there is
     no entry.  */
  if (force_fallback_processing_for (pc, &vueblock))
    return 0;

  *segment_base = 0; /* ??? Fixme. ??? */
  *gp = vueblock.gp_value;
  ent->start_offset = vueblock.start_offset;
  ent->end_offset = vueblock.end_offset;
  ent->info_offset = vueblock.info_offset;

  return ent;
}
