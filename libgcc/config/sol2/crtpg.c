/* Register profiling startup and cleanup with Solaris CRTs.
   Copyright (C) 2015-2021 Free Software Foundation, Inc.

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

#include <stdlib.h>

extern void monstartup (char *, char *);
extern void _mcleanup (void);

extern char _start[], _etext[];

int __start_crt_compiler (int, char **);

/* Since Solaris 11.4, the system-provided CRTs provide a hook to invoke
   initialization code early during process startup.  __start_crt_compiler
   is documented in crt1.o(5).  We use it to perform initialization for
   profiling as a substitute for the earlier separate gcrt1.o.  */

int
__start_crt_compiler (int argc __attribute__ ((unused)),
		      char **argv __attribute__ ((unused)))
{
  monstartup (_start, _etext);
  atexit (_mcleanup);
  return 0;
}
