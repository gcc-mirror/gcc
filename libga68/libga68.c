/* GNU Algol Compiler run-time.
   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   Under Section 7 of GPL version 3, you are granted additional permissions
   described in the GCC Runtime Library Exception, version 3.1, as published by
   the Free Software Foundation.

   You should have received a copy of the GNU General Public License and a copy
   of the GCC Runtime Library Exception along with this program; see the files
   COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "ga68.h"
#include <stdlib.h> /* for EXIT_SUCCESS */

/* argc and argv are preserved in the following objects.  */

int _libga68_argc;
char **_libga68_argv;

/* Entry point for Algol 68 programs.  */

void __algol68_main (void);

int
main (int argc, char **argv)
{
  _libga68_argc = argc;
  _libga68_argv = argv;

  _libga68_init_heap ();
  __algol68_main ();
  return EXIT_SUCCESS;
}
