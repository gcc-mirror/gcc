/* Copyright (C) 2017-2019 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

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

/* OSX uses too small default stack size (0.5MB), use a bigger default.
   Users can still override it through OMP_STACKSIZE or GOMP_STACKSIZE
   environment variables.  */
#define GOMP_DEFAULT_STACKSIZE	(2 * 1024 * 1024)
