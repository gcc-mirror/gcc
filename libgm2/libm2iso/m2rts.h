/* m2rts.h provides a C interface to M2RTS.mod.

Copyright (C) 2019-2022 Free Software Foundation, Inc.
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


#define str(X)  #X

typedef void (*proc_con) (int, char **, char **);
typedef void (*proc_dep) (void);

extern "C" void m2iso_M2RTS_RequestDependant (const char *modulename, const char *libname, const char *dependancy);
extern "C" void m2iso_M2RTS_RegisterModule (const char *modulename, const char *libname,
					    proc_con init, proc_con fini, proc_dep dependencies);
extern "C" void m2pim_M2RTS_RegisterModule (const char *modulename, const char *libname,
					    proc_con init, proc_con fini, proc_dep dependencies);
extern "C" void M2RTS_RegisterModule (const char *modulename, const char *libname,
				      proc_con init, proc_con fini, proc_dep dependencies);
extern "C" void m2iso_M2_M2RTS_init (void);

extern "C" void m2iso_M2RTS_ConstructModules (const char *modulename, const char *libname,
					      int argc, char *argv[], char *envp[]);
extern "C" void m2iso_M2RTS_Terminate (void);
extern "C" void m2iso_M2RTS_DeconstructModules (void);

extern "C" void m2iso_M2RTS_HaltC (const char *desc, const char *filename,
				   const char *functionname, int line) __attribute__ ((noreturn));
