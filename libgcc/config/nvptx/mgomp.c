/* Define shared memory arrays for -msoft-stack and -muniform-simt.

   Copyright (C) 2015-2023 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* OpenACC offloading does not use these symbols; thus, they are exposed
   only for the -mgomp multilib.  The same definitions are also provided
   in crt0.c for the case of non-offloading compilation.  32 is the maximum
   number of warps in a CTA.  */

#if defined(__nvptx_softstack__) && defined(__nvptx_unisimt__)
void *__nvptx_stacks[32] __attribute__((shared,nocommon));
unsigned __nvptx_uni[32] __attribute__((shared,nocommon));
#endif
