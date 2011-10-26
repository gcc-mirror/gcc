/* Copyright (c) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

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


/* 
 * This file defines an interface to allow a debugger easily to 
 * acquire all of the UPC processes at startup time.
 *
 * It exploits the interface used by MPICH http://www-unix.mcs.anl.gov/mpi/mpich/
 * to interface to debuggers.
 */

#ifndef _UPC_DEBUG_H_
#define _UPC_DEBUG_H_

/**************************************************************************
 * These functions are our interface to the debugger.
 */

/* A little struct to hold the target processor name and pid for
 * each process which forms part of the MPI program.
 *
 * For UPC we probably don't need the host_name or executable_name,
 * but it's easier to use the existing interface than bother to leave
 * them out. Provided we zero them the debugger will assume they're the
 * same as the initial program.
 *
 * DO NOT change the name of this structure or its fields. The debugger knows
 * them, and will be confused if you change them.
 */
typedef struct 
{
  char * host_name;           /* Something we can pass to inet_addr */
  char * executable_name;     /* The name of the image */
  int    pid;		      /* The pid of the process */
} MPIR_PROCDESC;

/* Array of procdescs for debugging purposes */
extern MPIR_PROCDESC *MPIR_proctable;
extern int MPIR_proctable_size;

/* Various global variables which a debugger can use for 
 * 1) finding out what the state of the program is at
 *    the time the magic breakpoint is hit.
 * 2) inform the process that it has been attached to and is
 *    now free to run.
 */
extern volatile int MPIR_debug_state;
extern volatile int MPIR_debug_gate;
extern const char * MPIR_debug_abort_string;
extern int          MPIR_being_debugged; /* Cause extra info on internal state
					  * to be maintained
					  */
 
/* Values for the debug_state, this seems to be all we need at the moment
 * but that may change... 
 */
#define MPIR_DEBUG_SPAWNED   1
#define MPIR_DEBUG_ABORTING  2

/* A function we call to tell the debugger that something worthwhile is happening.
 */
extern void MPIR_Breakpoint (void);

/*
 * Other functions whose mere presence in the executable provides information to the
 * debugger.
 */
extern void MPIR_i_am_starter (void);
extern void MPIR_ignore_queues (void);
extern void MPIR_force_to_main (void);

#endif /* _UPC_DEBUG_H_ */
