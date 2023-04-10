/* UnixArgs.cc record argc, argv as global variables.

Copyright (C) 2009-2022 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
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

#include <config.h>
#include <m2rts.h>

#define EXPORT(FUNC) m2pim ## _UnixArgs_ ## FUNC
#define M2EXPORT(FUNC) m2pim ## _M2_UnixArgs_ ## FUNC
#define M2LIBNAME "m2pim"

extern "C" int EXPORT(GetArgC) (void);
extern "C" char **EXPORT(GetArgV) (void);
extern "C" char **EXPORT(GetEnvV) (void);

static int UnixArgs_ArgC;
static char **UnixArgs_ArgV;
static char **UnixArgs_EnvV;


/* GetArgC returns argc.  */

extern "C" int
EXPORT(GetArgC) (void)
{
  return UnixArgs_ArgC;
}


/* GetArgV returns argv.  */

extern "C" char **
EXPORT(GetArgV) (void)
{
  return UnixArgs_ArgV;
}


/* GetEnvV returns envv.  */

extern "C" char **
EXPORT(GetEnvV) (void)
{
  return UnixArgs_EnvV;
}


/* GNU Modula-2 linking hooks.  */

extern "C" void
M2EXPORT(init) (int argc, char **argv, char **envp)
{
  UnixArgs_ArgC = argc;
  UnixArgs_ArgV = argv;
  UnixArgs_EnvV = envp;
}

extern "C" void
M2EXPORT(fini) (int, char **, char **)
{
}

extern "C" void
M2EXPORT(dep) (void)
{
}

extern "C" void __attribute__((__constructor__))
M2EXPORT(ctor) (void)
{
  m2pim_M2RTS_RegisterModule ("UnixArgs", M2LIBNAME,
			      M2EXPORT(init), M2EXPORT(fini),
			      M2EXPORT(dep));
}
