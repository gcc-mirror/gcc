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

#ifndef LIBGOMP_NVPTX_H
#define LIBGOMP_NVPTX_H 1

#define GOMP_REV_OFFLOAD_VAR __gomp_rev_offload_var

struct rev_offload {
  uint64_t fn;
  uint64_t mapnum;
  uint64_t addrs;
  uint64_t sizes;
  uint64_t kinds;
  int32_t dev_num;
};

#if (__SIZEOF_SHORT__ != 2 \
     || __SIZEOF_SIZE_T__ != 8 \
     || __SIZEOF_POINTER__ != 8)
#error "Data-type conversion required for rev_offload"
#endif

#endif  /* LIBGOMP_NVPTX_H */

