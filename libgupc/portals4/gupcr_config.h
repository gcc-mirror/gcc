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
 * @file gupcr_config.h
 * GUPC Runtime configuration
 */

#ifndef _GUPCR_CONFIG_H_
#define _GUPCR_CONFIG_H_

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>
#include <signal.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <unistd.h>
#include <sys/types.h>
#include <time.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#endif
#include <sys/time.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>

#ifdef _POSIX_PRIORITY_SCHEDULING
#define __USE_GNU
#include <sched.h>
#endif

#include "config.h"

#define DEV_ZERO "/dev/zero"
#define OFFSET_ZERO ((off_t) 0)
/* Darwin has MAP_ANON defined for anonymous memory map.  */
#if !MAP_ANONYMOUS && MAP_ANON
#define MAP_ANONYMOUS MAP_ANON
#endif
#define MAP_ERROR ((void *) -1)

#define GUPCR_SPIN_THREAD_SLOTS 4
#define GUPCR_SPIN_SLOT_COUNT 64
#define GUPCR_SPIN_MAX_MULT 1024

#define KILOBYTE 1024
#define C64K (64*KILOBYTE)
#define MEGABYTE (KILOBYTE*KILOBYTE)
#ifndef INT_MIN
/** __INT_MAX__ is predefined by the gcc compiler.  */
#define INT_MIN (-__INT_MAX__ - 1)
#endif

//begin detect_target64
#if (defined(_LP64) && _LP64)
#define GUPCR_TARGET64 1
#else
#define GUPCR_TARGET64 0
#endif
//end detect_target64

//begin mode_types
typedef unsigned int u_intQI_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_intHI_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_intSI_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_intDI_t __attribute__ ((__mode__ (__DI__)));
#if GUPCR_TARGET64
typedef unsigned int u_intTI_t __attribute__ ((__mode__ (__TI__)));
#endif /* GUPCR_TARGET64 */
//end mode_types

//begin lib_min_max

/* helper functions */
#define GUPCR_MIN(x,y) (((x) < (y)) ? (x): (y))
#define GUPCR_MAX(x,y) (((x) > (y)) ? (x): (y))
#define GUPCR_ABS(x) (((x) > 0) ? (x): -(x))
#define GUPCR_ROUND(x, r) (((x) + (r) - 1)/(r)*(r))
//end lib_min_max

//begin lib_config_heap

/** Maximum heap size
    Set here as 64 gigabytes on a 64-bit implementation
    and 1 gigabyte on other (eg, 32 bit) implementations.  */
#define GUPCR_MAX_HEAP_SIZE (((sizeof (void *)*8) == 64) \
                              ? (64L * KILOBYTE * MEGABYTE) \
			      : ( 1L * KILOBYTE * MEGABYTE))

/** Default per thread UPC shared heap size.  */
#define GUPCR_DEFAULT_PER_THREAD_HEAP_SIZE (256*MEGABYTE)

/** The minimum number of bytes to allocate (128 bytes).

    This allows for 64 bytes of heap management overhead and 64
    bytes of allocation.  The allocation will be aligned to a 64
    byte boundary.  This is not space efficient, but is intended to
    provide a minimal alignment that agrees with most CPU cache line
    size requirements.  */
#define GUPCR_HEAP_ALLOC_MIN 128

/** The minimum number of bytes to allocate (in bits).  */
#define GUPCR_HEAP_ALLOC_MIN_BITS 7

/** The size of the heap management header block.  */
#define GUPCR_HEAP_ALLOC_OVERHEAD 64

/** The number of allocation pools per heap.  */
#define GUPCR_HEAP_NUM_POOLS (SIZE_T_BITS - GUPCR_HEAP_ALLOC_MIN_BITS)

/** An unlikely barrier id to be used for runtime synchronization */
#define GUPCR_RUNTIME_BARRIER_ID 0xBADF00D

/** A value used to tag each heap allocated item, checked by upc_free */
#define GUPCR_HEAP_ALLOC_TAG 0x0DDF00D

//end lib_config_heap

/*
 * Main entry for UPC programs.
 * The runtime will execute before calling the user's main
 * program.  Thus, the user's main program will renamed
 * inside of the <upc.h> file to 'upc_main'
 */
#define GUPCR_START main
#define GUPCR_MAIN upc_main

//begin lib_config_shared_section

/** The base address of the UPC shared section */
#define GUPCR_SHARED_SECTION_START __upc_shared_start
/** The ending address (plus one) of the UPC shared section */
#define GUPCR_SHARED_SECTION_END __upc_shared_end

/** The base address of the UPC compiled program info section */
#define GUPCR_PGM_INFO_SECTION_START __upc_pgm_info_start
/** The ending address (plus one) of the UPC compiled program info section */
#define GUPCR_PGM_INFO_SECTION_END __upc_pgm_info_end

/** The base address of an array of pointers to UPC initialization routines */
#define GUPCR_INIT_ARRAY_START __upc_init_array_start
/** The ending address (plus one) of pointers to UPC initialization routines */
#define GUPCR_INIT_ARRAY_END   __upc_init_array_end

//end lib_config_shared_section

#endif /* gupcr_config.h */
