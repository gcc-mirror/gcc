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

#ifndef _GUPCR_NODE_H_
#define _GUPCR_NODE_H_ 1

/**
 * @file gupcr_node.h
 * GUPC Node Local Memory.
 */

/**
 * @addtogroup NODE GUPCR Node Local Memory
 * @{
 */
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"
#include "gupcr_portals.h"
#include "gupcr_runtime.h"

//begin lib_node_local
/** Memory map for threads that share node local memory.  */
extern char **gupcr_node_map;
//end lib_node_local

/** Node local memory file/object prefix.  */
#define GUPCR_LOCAL_NAME_PREFIX "upc-mem"

char *gupcr_node_local_alloc (size_t);
void gupcr_node_init (void);
void gupcr_node_fini (void);
char *gupcr_mem_local_map (int, size_t);
void gupcr_mem_local_unlink (void);
void gupcr_mem_local_init (void);
/** @} */

#endif /* gupcr_node.h */
