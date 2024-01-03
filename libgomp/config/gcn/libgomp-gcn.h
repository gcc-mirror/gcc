/* Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by Tobias Burnus <tobias@codesourcery.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains defines and type definitions shared between the
   nvptx target's libgomp.a and the plugin-nvptx.c, but that is only
   needef for this target.  */

#ifndef LIBGOMP_GCN_H
#define LIBGOMP_GCN_H 1

#define DEFAULT_GCN_STACK_SIZE (32*1024)
#define DEFAULT_TEAM_ARENA_SIZE (64*1024)

/* These define the LDS location of data needed by OpenMP.  */
#define TEAM_ARENA_START 16  /* LDS offset of free pointer.  */
#define TEAM_ARENA_FREE  24  /* LDS offset of free pointer.  */
#define TEAM_ARENA_END   32  /* LDS offset of end pointer.  */
#define GCN_LOWLAT_HEAP  40  /* LDS offset of the OpenMP low-latency heap.  */

struct heap
{
  int64_t size;
  char data[0];
};

/* This struct defines the (unofficial) ABI-defined values the compiler
   expects to find in first bytes of the kernargs space.
   The plugin may choose to place additional data later in the kernargs
   memory allocation, but those are not in any fixed location.  */
struct kernargs_abi {
  /* Leave space for the real kernel arguments.
     OpenACC and OpenMP only use one pointer.  */
  int64_t dummy1;
  int64_t dummy2;

  /* A pointer to struct output, below, for console output data.  */
  int64_t out_ptr;		/* Offset 16.  */

  /* A pointer to struct heap.  */
  int64_t heap_ptr;		/* Offset 24.  */

  /* A pointer to the ephemeral memory areas.
     The team arena is only needed for OpenMP.
     Each should have enough space for all the teams and threads.  */
  int64_t arena_ptr;		/* Offset 32.  */
  int64_t stack_ptr;		/* Offset 40.  */
  int arena_size_per_team;	/* Offset 48.  */
  int stack_size_per_thread;	/* Offset 52.  */
};

/* This struct is also used in Newlib's libc/sys/amdgcn/write.c.  */
struct output
{
  int return_value;
  unsigned int next_output;
  struct printf_data {
    int written;
    char msg[128];
    int type;
    union {
      int64_t ivalue;
      double dvalue;
      char text[128];
      uint64_t value_u64[16];
    };
  } queue[1024];
  unsigned int consumed;
};

#if (__SIZEOF_SHORT__ != 2 \
     || __SIZEOF_SIZE_T__ != 8 \
     || __SIZEOF_POINTER__ != 8)
#error "Data-type conversion required for rev_offload"
#endif

#endif  /* LIBGOMP_GCN_H */
