/* Copyright (c) 2009, 2010, 2011
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


#include <upc.h>
#include "gasp_upc.h"
#include "upc_pupc.h"

#ifndef NULL
#define NULL (void *)0
#endif

/* The filename of the location where a runtime
   error was detected.  This is set by the various
   debug-enabled ('g') UPC runtime library routines.  */
extern GUPCR_THREAD_LOCAL const char *__upc_err_filename;

/* The line number of the location where a runtime
   error was detected.  This is set by the various
   debug-enabled ('g') UPC runtime library routines.  */
extern GUPCR_THREAD_LOCAL unsigned int __upc_err_linenum;

#define GUPCR_SET_ERR_LOC() \
  do \
    { \
      __upc_err_filename = filename; \
      __upc_err_linenum  = linenum; \
    } while (0)

#define GUPCR_CLEAR_ERR_LOC() \
  do \
    { \
      __upc_err_filename = NULL; \
      __upc_err_linenum  = 0; \
    } while (0)

shared void *
upc_global_allocg (size_t nblocks, size_t nbytes, const char *filename,
		   int linenum)
{
  shared void *result;
  p_start (GASP_UPC_GLOBAL_ALLOC, nblocks, nbytes);
  GUPCR_SET_ERR_LOC();
  result = upc_global_alloc (nblocks, nbytes);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_GLOBAL_ALLOC, nblocks, nbytes, &result);
  return result;
}

shared void *
upc_all_allocg (size_t nblocks, size_t nbytes, const char *filename, int linenum)
{
  shared void *result;
  p_start (GASP_UPC_ALL_ALLOC, nblocks, nbytes);
  GUPCR_SET_ERR_LOC();
  result = upc_all_alloc (nblocks, nbytes);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_ALL_ALLOC, nblocks, nbytes, &result);
  return result;
}

shared void *
upc_allocg (size_t nbytes, const char *filename, int linenum)
{
  shared void *val;
  p_start (GASP_UPC_ALLOC, nbytes);
  GUPCR_SET_ERR_LOC();
  val = upc_alloc (nbytes);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_ALLOC, nbytes, &val);
  return val;
}

void
upc_freeg (shared void *ptr, const char *filename, int linenum)
{
  p_start (GASP_UPC_FREE, &ptr);
  GUPCR_SET_ERR_LOC();
  upc_free (ptr);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_FREE, &ptr);
}
