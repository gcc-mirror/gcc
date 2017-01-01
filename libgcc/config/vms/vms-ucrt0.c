/* VMS crt0 returning Unix style condition codes.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.
   Contributed by Douglas B. Rupp (rupp@gnat.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <stdlib.h>

/* Sanity check.  */
#if __INITIAL_POINTER_SIZE != 64
#error "vms-ucrt0.c must be compiled with -mpointer-size=64"
#endif

/* Lots of cheat to handle 32bits/64bits pointer conversions.
   We use 'long long' for 64 bits pointers and 'int' for 32 bits pointers.  */

extern void decc$main (void *, void *, void *, void *, unsigned int,
		       unsigned int, int *, int *, int *);
extern int main (int, char **, char **);
extern int _malloc32 (int);

#ifdef __ia64__
#define MAIN_ASM_NAME asm ("ELF$TFRADR")
#else
#define MAIN_ASM_NAME
#endif

int __main (void *, void *, void *, void *,
	    unsigned int, unsigned int) MAIN_ASM_NAME;

/* From errnodef.h, but we need to emulate the globalval.  */
extern int C$_EXIT1;

/* From stsdef.h  */
#define STS$V_MSG_NO 0x03
#define STS$M_INHIB_MSG 0x10000000
/* Symbol defined while main() is compiled to record the flags used.
   (Note that the symbol defines the value, ie extract the bits from the
    address).
   bit 0 set for 64 bit pointers
   bit 1 set for posix return value.  */
extern char __gcc_main_flags;

/* From ssdef.h  */
#define SS$_NORMAL 1
#define MAIN_FLAG_64BIT (1 << 0)
#define MAIN_FLAG_POSIX (1 << 1)

int
__main (void *progxfer, void *cli_util, void *imghdr, void *image_file_desc,
	unsigned int linkflag, unsigned int cliflag)
{
  int argc;
  int argv;
  int envp;
  int status;
  char **argv64;
  char **envp64;
  unsigned int flags = (unsigned __int64)&__gcc_main_flags;

  /* The argv and envp arrays are 32 bits pointers to 32 bits pointers.  */
  decc$main (progxfer, cli_util, imghdr, image_file_desc,
	     linkflag, cliflag, &argc, &argv, &envp);

  if (flags & MAIN_FLAG_64BIT)
    {
      int i;

      /* Reallocate argv and envp with 64 bit pointers.  */
      argv64 = (char **) _malloc32 (sizeof (char *) * (argc + 1));

      for (i = 0; i < argc; i++)
        argv64[i] = (char *) (__int64)(((int *) (__int64) argv)[i]);

      argv64[argc] = NULL;

      for (i = 0; ((int *) (__int64) envp)[i]; i++)
        ;
      envp64 = (char **) _malloc32 (sizeof (char *) * (i + 1));

      for (i = 0; ((int *) (__int64) envp)[i]; i++)
        envp64[i] = (char *)(__int64)(((int *) (__int64) envp)[i]);

      envp64[i] = NULL;
    }
  else
    {
      argv64 = (char **)(__int64)argv;
      envp64 = (char **)(__int64)envp;
    }

  status = main (argc, argv64, envp64);

  if (flags & MAIN_FLAG_POSIX)
    {
      /* Map into a range of 0 - 255.  */
      status &= 255;

      if (status != 0)
	{
	  int save_status = status;

	  status = (__int64) &C$_EXIT1 + ((status - 1) << STS$V_MSG_NO);

	  /* An exit failure status requires a "severe" error.  All
	     status values are defined in errno with a successful (1)
	     severity but can be changed to an error (2) severity by
	     adding 1.  In addition for compatibility with UNIX exit()
	     routines we inhibit a run-time error message from being
	     generated on exit(1).  */

	  if (save_status == 1)
	    {
	      status++;
	      status |= STS$M_INHIB_MSG;
	    }
	}
      else
	status = SS$_NORMAL;
    }

  return status;
}
