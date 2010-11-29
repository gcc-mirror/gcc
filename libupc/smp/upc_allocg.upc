/* Copyright (C) 2005-2009 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>
   Original Implementation by Adam Leko <adam@leko.org>.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

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
