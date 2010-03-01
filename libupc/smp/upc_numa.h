/* Copyright (C) 2008 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

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

#ifndef _UPC_NUMA_H_
#define _UPC_NUMA_H_

extern int __upc_numa_supported (void);
extern int __upc_numa_init (const upc_info_p,
                            const char **err_msg);
extern int __upc_numa_allocate (const upc_info_p u, const int thread_id,
				int *sched_affinity, int *mem_affinity,
				const char **err_msg);
extern void __upc_numa_sched_set (const upc_info_p, const int);
extern void __upc_numa_memory_affinity_set (const upc_info_p, const int);
extern void __upc_numa_memory_region_affinity_set (const upc_info_p u,
						   const int thread_id,
						   const void *region,
						   const size_t size);

#endif /* !_UPC_NUMA_H */
