/* Fake 'alloca' implementation.

   Copyright (C) 2025 Free Software Foundation, Inc.

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

/* For '-mfake-ptx-alloca', in case real PTX 'alloca' is not available.
   With this function defined, we don't get a link-time failure
   (unresolved symbol '__GCC_nvptx__PTX_alloca_not_supported'), but rather:
   successful execution, in case that 'alloca' is not attempted (if only used
   in error code paths, for example), and a run-time failure only in case that
   'alloca' is actually attempted.  */

void *
__GCC_nvptx__PTX_alloca_not_supported (__SIZE_TYPE__ size __attribute__ ((unused)))
{
  __builtin_printf ("GCC/nvptx: sorry, unimplemented:"
		    " dynamic stack allocation not supported\n");
  __builtin_abort ();
  return 0;
}
