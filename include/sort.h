/* Sorting algorithms.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Mark Mitchell <mark@codesourcery.com>.

This file is part of GNU CC.
   
GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef SORT_H
#define SORT_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <ansidecl.h>

/* Sort an array of pointers.  */

extern void sort_pointers PARAMS ((size_t, void **, void **));

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* SORT_H */


   
   
