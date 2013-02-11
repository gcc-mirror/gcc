/* Library support for -fsplit-stack.  */
/* Copyright (C) 2009-2013 Free Software Foundation, Inc.
   Contributed by Ian Lance Taylor <iant@google.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This is a libgcc internal header file for functions shared between
   generic-morestack.c and generic-morestack-thread.c.  The latter
   file is only used when linking with the pthread library.  */

/* The stack segment structure, defined in generic-morestack.c.  */

struct stack_segment;

/* The list of stack segments for this thread.  */

extern __thread struct stack_segment *__morestack_segments;

/* Print the string MSG/LEN, the errno number ERR, and a newline on
   stderr, without using printf.  Then crash.  */

extern void __morestack_fail (const char *msg, size_t len, int err)
  __attribute__ ((noreturn, visibility ("hidden")));

/* Release stack segments.  */

extern struct dynamic_allocation_blocks *
  __morestack_release_segments (struct stack_segment **, int)
  __attribute__ ((visibility ("hidden")));

/* Store the stack information in a processor dependent manner.  */

extern void __stack_split_initialize (void)
  __attribute__ ((visibility ("hidden")));
