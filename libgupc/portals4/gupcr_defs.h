/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
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

/**
 * @file gupcr_defs.h
 * GUPC Runtime definitions
 */

#ifndef _GUPCR_DEFS_H_
#define _GUPCR_DEFS_H_

#include "gupcr_pts.h"

//begin lib_max_threads_def

/* Maximum number of THREADS supported in this implementation */
#define GUPCR_THREAD_SIZE 12
#define GUPCR_THREADS_MAX (1 << GUPCR_THREAD_SIZE)
//end lib_max_threads_def

#if GUPCR_PTS_PACKED_REP && (GUPCR_THREADS_SIZE > GUPCR_PTS_THREAD_SIZE)
#error GUPCR_THREADS_MAX exceeds the size of the packed sptr threads field.
#endif

/* The filename of the location where a runtime
   error was detected.  This is set by the various
   debug-enabled ('g') UPC runtime library routines.  */
extern const char *gupcr_err_filename;

/* The line number of the location where a runtime
   error was detected.  This is set by the various
   debug-enabled ('g') UPC runtime library routines.  */
extern unsigned int gupcr_err_linenum;

#define GUPCR_SET_ERR_LOC() \
  do \
    { \
      gupcr_err_filename = filename; \
      gupcr_err_linenum  = linenum; \
    } while (0)

#define GUPCR_CLEAR_ERR_LOC() \
  do \
    { \
      gupcr_err_filename = NULL; \
      gupcr_err_linenum  = 0; \
    } while (0)

/* The base address of the UPC shared section */
extern char GUPCR_SHARED_SECTION_START[1];

/* The ending address (plus one) of the UPC shared section */
extern char GUPCR_SHARED_SECTION_END[1];

/* The base address of the UPC compiled program info. section */
extern char GUPCR_PGM_INFO_SECTION_START[1];

/* The ending address (plus one) of the UPC compiled program info. section */
extern char GUPCR_PGM_INFO_SECTION_END[1];

#ifndef __UPC__
/* The value of THREADS when defined at run time */
extern int THREADS;

/* Current thread id */
extern int MYTHREAD;
#endif /* !__UPC__ */

/* OK to call finalize routines */
extern int gupcr_finalize_ok;

/* Prototype for the main finalize routine */
extern void gupcr_fini (void);

#endif /* gupcr_defs.h */
