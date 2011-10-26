/* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011
   Free Software Foundation, Inc. 
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

#ifndef _UPC_CONFIG_H_
#define _UPC_CONFIG_H_

#include "config.h"

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#ifdef __sgi__
#include <ulocks.h>
#include <sys/sysmp.h>
#endif

#ifdef _POSIX_PRIORITY_SCHEDULING
#define __USE_GNU
#include <sched.h>
#endif

#include <netdb.h>
#include <netinet/in.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>

#ifdef GUPCR_USE_PTHREADS
#include <pthread.h>
#define GUPCR_THREAD_LOCAL __thread
#else
#define GUPCR_THREAD_LOCAL
#endif

#define DEV_ZERO "/dev/zero"
#define OFFSET_ZERO ((off_t) 0)
/* Darwin has MAP_ANON defined for anonymous memory map */
#if !MAP_ANONYMOUS && MAP_ANON
#define MAP_ANONYMOUS MAP_ANON
#endif
#define MAP_ERROR ((void *) -1)

#define KILOBYTE 1024
#define C64K (64*KILOBYTE)
#define MEGABYTE (KILOBYTE*KILOBYTE)
#ifndef INT_MIN
/* __INT_MAX__ is predefined by the gcc compiler */
#  define INT_MIN (-__INT_MAX__ - 1)
#endif

//begin detect_target64
#if (defined(_LP64) && _LP64) \
    || (defined(_MIPS_SZPTR) && (_MIPS_SZPTR == 64)) \
    || (defined(_CRAYT3E))
#define GUPCR_TARGET64 1
#else
#define GUPCR_TARGET64 0
#endif
//end detect_target64

//begin mode_types
typedef unsigned int u_intQI_t __attribute__ ((__mode__(__QI__)));
typedef unsigned int u_intHI_t __attribute__ ((__mode__(__HI__)));
typedef unsigned int u_intSI_t __attribute__ ((__mode__(__SI__)));
typedef unsigned int u_intDI_t __attribute__ ((__mode__(__DI__)));
#if GUPCR_TARGET64
typedef unsigned int u_intTI_t __attribute__ ((__mode__(__TI__)));
#endif /* GUPCR_TARGET64 */
//end mode_types

//begin lib_config_vm
#if GUPCR_TARGET64
/* On 64-bit machines, use page size of 32M (25 bits) and a max per thread
   offset of 256G (38 bits).  This leaves 13 bits for the per thread
   number of pages.  */
#define GUPCR_VM_OFFSET_BITS 25 
#if GUPCR_PTS_VADDR_SIZE > 38
#define GUPCR_VM_MAX_PAGES_PER_THREAD \
	(1 << (38 - GUPCR_VM_OFFSET_BITS))
#else
#define GUPCR_VM_MAX_PAGES_PER_THREAD \
	(1 << (GUPCR_PTS_VADDR_SIZE - GUPCR_VM_OFFSET_BITS))
#endif
#else
/* On 32-bit machines, use page size of 4M (22 bits) and a max per thread
   offset of 4G (32 bits).  This leaves 10 bits for the per thread
   number of pages.  */
#define GUPCR_VM_OFFSET_BITS 22
#if GUPCR_PTS_VADDR_SIZE > 32
#define GUPCR_VM_MAX_PAGES_PER_THREAD \
	(1 << (32 - GUPCR_VM_OFFSET_BITS))
#else
#define GUPCR_VM_MAX_PAGES_PER_THREAD \
	(1 << (GUPCR_PTS_VADDR_SIZE - GUPCR_VM_OFFSET_BITS))
#endif
#endif /* GUPCR_TARGET64 */

/* Derive some VM specific constants. */
#define GUPCR_VM_PAGE_MASK (GUPCR_VM_MAX_PAGES_PER_THREAD - 1)
#define GUPCR_VM_PAGE_SIZE (1 << GUPCR_VM_OFFSET_BITS)
#define GUPCR_VM_OFFSET_MASK (GUPCR_VM_PAGE_SIZE - 1)
/* Declare a type sufficiently large to hold a page number.
   We can probably get by with a 'short' here, but it is
   safer to just use a full 'int'.*/
typedef unsigned int upc_page_num_t;

/* Each thread caches a mapping between global page number
   and local mapped address.  The global page number is
   hashed into a global map cache, which is N-way associative,
   where GUPCR_VM_GLOBAL_SET_SIZE defines the value of N.  */
#define GUPCR_VM_GLOBAL_MAP_BITS 6 
#define GUPCR_VM_GLOBAL_MAP_SIZE (1 << GUPCR_VM_GLOBAL_MAP_BITS)
#define GUPCR_VM_GLOBAL_MAP_MASK (GUPCR_VM_GLOBAL_MAP_SIZE - 1)
#define GUPCR_VM_GLOGAl_MAP_SET_SIZE 4
/* All 1's for the virtual page number in a global map entry (GME)
   indicates that the entry has not yet been mapped. */
#define GUPCR_VM_PAGE_INVALID -1U
//end lib_config_vm

//begin lib_min_max

/* helper functions */
#define GUPCR_MIN(x,y) (((x) < (y)) ? (x): (y))
#define GUPCR_MAX(x,y) (((x) > (y)) ? (x): (y))
#define GUPCR_ABS(x) (((x) > 0) ? (x): -(x))
#define GUPCR_ROUND(x, r) (((x) + (r) - 1)/(r)*(r))
//end lib_min_max

//begin lib_config_heap

/* Max. heap size
   Set here as 64 gigabytes on a 64-bit implementation
   and 1 gigabyte on other (eg, 32 bit) implementations. */
#define GUPCR_MAX_HEAP_SIZE (((sizeof (void *)*8) == 64) \
                              ? (64L * KILOBYTE * MEGABYTE) \
			      : ( 1L * KILOBYTE * MEGABYTE))

/* Per-thread space initially allocated to UPC user's heap */
#define GUPCR_DEFAULT_PER_THREAD_HEAP_SIZE (16*MEGABYTE)

/* Per-thread maximum stack size that will be added to the OS's
   default stack size, when creating pthreads.  */
#define GUPCR_DEFAULT_PER_THREAD_STACK_SIZE (16*MEGABYTE)

/* The minimum number of bytes to allocate */
#define GUPCR_HEAP_ALLOC_MIN 64

/* Heaps are increased by multiples of this chunk size.
   The chunk size should be an even multiple of the UPC VM page size.  */
#define GUPCR_HEAP_CHUNK_SIZE (1*GUPCR_VM_PAGE_SIZE)

/* an unlikely barrier id to be used for runtime synchronization */
#define GUPCR_RUNTIME_BARRIER_ID 0xBADF00D

/* a value used to tag each heap allocated item, checked by upc_free */
#define GUPCR_HEAP_ALLOC_TAG 0x0DDF00D
//end lib_config_heap

/* By default we let kernel schedule threads */
#define GUPCR_SCHED_POLICY_DEFAULT GUPCR_SCHED_POLICY_AUTO
#define GUPCR_MEM_POLICY_DEFAULT GUPCR_MEM_POLICY_AUTO

/* Enable GUM debug support via this environment variable.  */
#define GUM_DEBUG_ENV "GUM_DEBUG"

/* Specify the GUM port as "host:port" via this environment variable.  */
#define GUM_PORT_ENV "GUM_PORT"

/* Use this environment variable to specify the time (in seconds) that
   the UPC process should delay to give gdbserver
   a chance to attach to it.  */
#define GUM_ATTACH_DELAY_ENV "GUM_ATTACH_DELAY"

/* Specify the full pathname of gdbsever via this environment variable.  */
#define GUM_GDBSERVERPATH_ENV "GDBSERVERPATH"

/* Default GUM host */
#define GUM_HOST_DEFAULT "localhost"

/* Default GUM port */
#define GUM_PORT_DEFAULT 1234

/* Default gdbserver attach delay (in seconds)  */
#define GUM_ATTACH_DELAY_DEFAULT 10

/* GUM initialization routine called at start up */
extern void __upc_gum_init (int, int);

/*
 * Main entry for UPC programs.
 * The runtime will execute before calling the user's main
 * program.  Thus, the user's main program will renamed
 * inside of the <upc.h> file to 'upc_main'
 */
#define GUPCR_START main
#define GUPCR_MAIN upc_main

//begin lib_config_shared_section

/* The base address of the UPC shared section */
#define GUPCR_SHARED_SECTION_START __upc_shared_start
/* The ending address (plus one) of the UPC shared section */
#define GUPCR_SHARED_SECTION_END __upc_shared_end

/* The base address of the UPC compiled program info. section */
#define GUPCR_PGM_INFO_SECTION_START __upc_pgm_info_start
/* The ending address (plus one) of the UPC compiled program info. section */
#define GUPCR_PGM_INFO_SECTION_END __upc_pgm_info_end

/* The base address of an array of pointers to UPC initialization routines.  */
#define GUPCR_INIT_ARRAY_START __upc_init_array_start
/* The ending address (plus one) of pointers to UPC initialization routines */
#define GUPCR_INIT_ARRAY_END   __upc_init_array_end

//end lib_config_shared_section

#endif /* _UPC_CONFIG_H_ */
