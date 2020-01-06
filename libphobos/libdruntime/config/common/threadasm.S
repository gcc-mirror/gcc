/* Support code for fibers and multithreading.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

#if (__linux__ || __FreeBSD__ || __NetBSD__ || __DragonFly__) && __ELF__
/*
 * Mark the resulting object file as not requiring execution permissions on
 * stack memory. The absence of this section would mark the whole resulting
 * library as requiring an executable stack, making it impossible to
 * dynamically load druntime on several Linux platforms where this is
 * forbidden due to security policies.
 */
    .section .note.GNU-stack,"",%progbits
#endif

/* Let preprocessor tell us if C symbols have a prefix: __USER_LABEL_PREFIX__ */
#ifdef __USER_LABEL_PREFIX__
#define __CONCAT2(a, b) a ## b
#define __CONCAT(a, b) __CONCAT2(a, b)
#define CSYM(name) __CONCAT(__USER_LABEL_PREFIX__, name)
#else
#define CSYM(name) name
#endif
