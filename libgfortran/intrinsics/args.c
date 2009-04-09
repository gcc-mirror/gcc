/* Implementation of the GETARG and IARGC g77, and
   corresponding F2003, intrinsics. 
   Copyright (C) 2004, 2005, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Bud Davis and Janne Blomqvist.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
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

#include "libgfortran.h"
#include <string.h>


/* Get a commandline argument.  */

extern void getarg_i4 (GFC_INTEGER_4 *, char *, gfc_charlen_type);
iexport_proto(getarg_i4);

void 
getarg_i4 (GFC_INTEGER_4 *pos, char  *val, gfc_charlen_type val_len)
{
  int argc;
  int arglen;
  char **argv;

  get_args (&argc, &argv);

  if (val_len < 1 || !val )
    return;   /* something is wrong , leave immediately */
  
  memset (val, ' ', val_len);

  if ((*pos) + 1 <= argc  && *pos >=0 )
    {
      arglen = strlen (argv[*pos]);
      if (arglen > val_len)
	arglen = val_len;
      memcpy (val, argv[*pos], arglen);
    }
}
iexport(getarg_i4);


/* INTEGER*8 wrapper of getarg.  */

extern void getarg_i8 (GFC_INTEGER_8 *, char *, gfc_charlen_type);
export_proto (getarg_i8);

void 
getarg_i8 (GFC_INTEGER_8 *pos, char  *val, gfc_charlen_type val_len)
{
  GFC_INTEGER_4 pos4 = (GFC_INTEGER_4) *pos;
  getarg_i4 (&pos4, val, val_len);
}


/* Return the number of commandline arguments.  The g77 info page 
   states that iargc does not include the specification of the
   program name itself.  */

extern GFC_INTEGER_4 iargc (void);
export_proto(iargc);

GFC_INTEGER_4
iargc (void)
{
  int argc;
  char **argv;

  get_args (&argc, &argv);

  return (argc - 1);
} 


/* F2003 intrinsic functions and subroutines related to command line
   arguments.

   - function command_argument_count() is converted to iargc by the compiler.

   - subroutine get_command([command, length, status]).

   - subroutine get_command_argument(number, [value, length, status]).
*/

/* These two status codes are specified in the standard. */
#define GFC_GC_SUCCESS 0
#define GFC_GC_VALUE_TOO_SHORT -1

/* Processor-specific status failure code. */
#define GFC_GC_FAILURE 42


extern void get_command_argument_i4 (GFC_INTEGER_4 *, char *, GFC_INTEGER_4 *,
				     GFC_INTEGER_4 *, gfc_charlen_type);
iexport_proto(get_command_argument_i4);

/* Get a single commandline argument.  */

void
get_command_argument_i4 (GFC_INTEGER_4 *number, char *value, 
			 GFC_INTEGER_4 *length, GFC_INTEGER_4 *status, 
			 gfc_charlen_type value_len)
{
  int argc, arglen = 0, stat_flag = GFC_GC_SUCCESS;
  char **argv;

  if (number == NULL )
    /* Should never happen.  */
    runtime_error ("Missing argument to get_command_argument");

  if (value == NULL && length == NULL && status == NULL)
    return; /* No need to do anything.  */

  get_args (&argc, &argv);

  if (*number < 0 || *number >= argc)
    stat_flag = GFC_GC_FAILURE;
  else
    arglen = strlen(argv[*number]);    

  if (value != NULL)
    {
      if (value_len < 1)
	stat_flag = GFC_GC_FAILURE;
      else
	memset (value, ' ', value_len);
    }

  if (value != NULL && stat_flag != GFC_GC_FAILURE)
    {
      if (arglen > value_len)
       {
	 arglen = value_len;
	 stat_flag = GFC_GC_VALUE_TOO_SHORT;
       }
      memcpy (value, argv[*number], arglen);
    }

  if (length != NULL)
    *length = arglen;

  if (status != NULL)
    *status = stat_flag;
}
iexport(get_command_argument_i4);


/* INTEGER*8 wrapper for get_command_argument.  */

extern void get_command_argument_i8 (GFC_INTEGER_8 *, char *, GFC_INTEGER_8 *, 
				     GFC_INTEGER_8 *, gfc_charlen_type);
export_proto(get_command_argument_i8);

void
get_command_argument_i8 (GFC_INTEGER_8 *number, char *value, 
			 GFC_INTEGER_8 *length, GFC_INTEGER_8 *status, 
			 gfc_charlen_type value_len)
{
  GFC_INTEGER_4 number4;
  GFC_INTEGER_4 length4;
  GFC_INTEGER_4 status4;

  number4 = (GFC_INTEGER_4) *number;
  get_command_argument_i4 (&number4, value, &length4, &status4, value_len);
  if (length)
    *length = length4;
  if (status)
    *status = status4;
}


/* Return the whole commandline.  */

extern void get_command_i4 (char *, GFC_INTEGER_4 *, GFC_INTEGER_4 *,
			    gfc_charlen_type);
iexport_proto(get_command_i4);

void
get_command_i4 (char *command, GFC_INTEGER_4 *length, GFC_INTEGER_4 *status,
		gfc_charlen_type command_len)
{
  int i, argc, arglen, thisarg;
  int stat_flag = GFC_GC_SUCCESS;
  int tot_len = 0;
  char **argv;

  if (command == NULL && length == NULL && status == NULL)
    return; /* No need to do anything.  */

  get_args (&argc, &argv);

  if (command != NULL)
    {
      /* Initialize the string to blanks.  */
      if (command_len < 1)
	stat_flag = GFC_GC_FAILURE;
      else
	memset (command, ' ', command_len);
    }

  for (i = 0; i < argc ; i++)
    {
      arglen = strlen(argv[i]);

      if (command != NULL && stat_flag == GFC_GC_SUCCESS)
	{
	  thisarg = arglen;
	  if (tot_len + thisarg > command_len)
	    {
	      thisarg = command_len - tot_len; /* Truncate.  */
	      stat_flag = GFC_GC_VALUE_TOO_SHORT;
	    }
	  /* Also a space before the next arg.  */
	  else if (i != argc - 1 && tot_len + arglen == command_len)
	    stat_flag = GFC_GC_VALUE_TOO_SHORT;

	  memcpy (&command[tot_len], argv[i], thisarg);
	}

      /* Add the legth of the argument.  */
      tot_len += arglen;
      if (i != argc - 1)
	tot_len++;
    }

  if (length != NULL)
    *length = tot_len;

  if (status != NULL)
    *status = stat_flag;
}
iexport(get_command_i4);


/* INTEGER*8 wrapper for get_command.  */

extern void get_command_i8 (char *, GFC_INTEGER_8 *, GFC_INTEGER_8 *,
			    gfc_charlen_type);
export_proto(get_command_i8);

void
get_command_i8 (char *command, GFC_INTEGER_8 *length, GFC_INTEGER_8 *status,
		gfc_charlen_type command_len)
{
  GFC_INTEGER_4 length4;
  GFC_INTEGER_4 status4;

  get_command_i4 (command, &length4, &status4, command_len);
  if (length)
    *length = length4;
  if (status)
    *status = status4;
}
