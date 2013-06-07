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

#include <upc.h>
#include <upc_collective.h>
#include <upc_coll.h>
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_sup.h"
#include "gupcr_portals.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"
#include "gupcr_coll_sup.h"

/**
 * @file gupcr_coll_init.upc
 * GUPC Portals4 collectives initialization.
 *
 * @addtogroup COLLECTIVES GUPCR Collectives Functions
 * @{
 */
int upc_coll_init_flag = 0;

/**
 * Collectives initialization function.
 *
 * Initialize necessary storage area for the broadcast/reduce
 * thread trees.
 */
void
upc_coll_init ()
{
  if (upc_coll_init_flag)
    gupcr_fatal_error ("multiple attempts to initialize collectives");
  upc_coll_init_flag = 1;

  /* Allocate the "all reduce" storage area.  */
  gupcr_reduce_storage = (gupcr_reduce_str_t)
    upc_all_alloc (THREADS, sizeof (struct gupcr_reduce_str));
  if (gupcr_reduce_storage == NULL)
    gupcr_fatal_error ("cannot allocate collectives reduce shared storage");
}

/* @} */
