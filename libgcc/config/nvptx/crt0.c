/* Copyright (C) 2014-2023 Free Software Foundation, Inc.

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

int *__exitval_ptr;

extern void __attribute__((noreturn)) exit (int status);
extern int main (int, void **);

/* Always setup soft stacks to allow testing with -msoft-stack but without
   -mgomp.  32 is the maximum number of warps in a CTA: the definition here
   must match the external declaration emitted by the compiler.  */
void *__nvptx_stacks[32] __attribute__((shared,nocommon));

/* Likewise for -muniform-simt.  */
unsigned __nvptx_uni[32] __attribute__((shared,nocommon));

extern void __main (int *, int, void **) __attribute__((kernel));

void
__main (int *rval_ptr, int argc, void **argv)
{
  __exitval_ptr = rval_ptr;
  /* Store something non-zero, so the host knows something went wrong,
     if we fail to reach exit properly.   */
  if (rval_ptr)
    *rval_ptr = 255;

  static char stack[131072] __attribute__((aligned(8)));
  __nvptx_stacks[0] = stack + sizeof stack;
  __nvptx_uni[0] = 0;

  exit (main (argc, argv));
}
