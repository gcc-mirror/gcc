/* Implementation of the SIGNAL and ALARM g77 intrinsics
   Copyright (C) 2005-2023 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

This file is part of the GNU Fortran runtime library (libgfortran).

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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <signal.h>

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#include <errno.h>

/* SIGNAL subroutine with PROCEDURE as handler  */
extern void signal_sub (int *, void (*)(int), int *);
iexport_proto(signal_sub);

void
signal_sub (int *number, void (*handler)(int), int *status)
{
  intptr_t ret;

  if (status != NULL)
    {
      ret = (intptr_t) signal (*number, handler);
      *status = (int) ret;
    }
  else
    signal (*number, handler);
}
iexport(signal_sub);


/* SIGNAL subroutine with INTEGER as handler  */
extern void signal_sub_int (int *, int *, int *);
iexport_proto(signal_sub_int);

void
signal_sub_int (int *number, int *handler, int *status)
{
  intptr_t ptr = *handler, ret;

  if (status != NULL)
    {
      ret = (intptr_t) signal (*number, (void (*)(int)) ptr);
      *status = (int) ret;
    }
  else
    signal (*number, (void (*)(int)) ptr);
}
iexport(signal_sub_int);


/* SIGNAL function with PROCEDURE as handler  */
extern int signal_func (int *, void (*)(int));
iexport_proto(signal_func);

int
signal_func (int *number, void (*handler)(int))
{
  int status;
  signal_sub (number, handler, &status);
  return status;
}
iexport(signal_func);


/* SIGNAL function with INTEGER as handler  */
extern int signal_func_int (int *, int *);
iexport_proto(signal_func_int);

int
signal_func_int (int *number, int *handler)
{
  int status;
  signal_sub_int (number, handler, &status);
  return status;
}
iexport(signal_func_int);



/* ALARM intrinsic with PROCEDURE as handler  */
extern void alarm_sub_i4 (int *, void (*)(int), GFC_INTEGER_4 *);
iexport_proto(alarm_sub_i4);

void
alarm_sub_i4 (int * seconds __attribute__ ((unused)),
	      void (*handler)(int) __attribute__ ((unused)),
	      GFC_INTEGER_4 *status)
{
#if defined (SIGALRM) && defined (HAVE_ALARM) 
  if (status != NULL)
    {
      if (signal (SIGALRM, handler) == SIG_ERR)
	*status = -1;
      else
	*status = alarm (*seconds);
    }
  else
    {
      signal (SIGALRM, handler);
      alarm (*seconds);
    }
#else
  errno = ENOSYS;
  if (status != NULL)
    *status = -1;
#endif
}
iexport(alarm_sub_i4);


extern void alarm_sub_i8 (int *, void (*)(int), GFC_INTEGER_8 *);
iexport_proto(alarm_sub_i8);

void
alarm_sub_i8 (int *seconds __attribute__ ((unused)),
	      void (*handler)(int) __attribute__ ((unused)),
	      GFC_INTEGER_8 *status)
{
#if defined (SIGALRM) && defined (HAVE_ALARM)
  if (status != NULL)
    {
      if (signal (SIGALRM, handler) == SIG_ERR)
	*status = -1;
      else
	*status = alarm (*seconds);
    }
  else
    {
      signal (SIGALRM, handler);
      alarm (*seconds);
    }
#else
  errno = ENOSYS;
  if (status != NULL)
    *status = -1;
#endif
}
iexport(alarm_sub_i8);


/* ALARM intrinsic with INTEGER as handler  */
extern void alarm_sub_int_i4 (int *, int *, GFC_INTEGER_4 *);
iexport_proto(alarm_sub_int_i4);

void
alarm_sub_int_i4 (int *seconds __attribute__ ((unused)),
		  int *handler __attribute__ ((unused)),
		  GFC_INTEGER_4 *status)
{
#if defined (SIGALRM) && defined (HAVE_ALARM)
  if (status != NULL)
    {
      if (signal (SIGALRM, (void (*)(int)) (intptr_t) *handler) == SIG_ERR)
	*status = -1;
      else
	*status = alarm (*seconds);
    }
  else
    {
      signal (SIGALRM, (void (*)(int)) (intptr_t) *handler);
      alarm (*seconds);
    }
#else
  errno = ENOSYS;
  if (status != NULL)
    *status = -1;
#endif
}
iexport(alarm_sub_int_i4);


extern void alarm_sub_int_i8 (int *, int *, GFC_INTEGER_8 *);
iexport_proto(alarm_sub_int_i8);

void
alarm_sub_int_i8 (int *seconds __attribute__ ((unused)),
		  int *handler __attribute__ ((unused)),
		  GFC_INTEGER_8 *status)
{
#if defined (SIGALRM) && defined (HAVE_ALARM)
  if (status != NULL)
    {
      if (signal (SIGALRM, (void (*)(int)) (intptr_t) *handler) == SIG_ERR)
	*status = -1;
      else
	*status = alarm (*seconds);
    }
  else
    {
      signal (SIGALRM, (void (*)(int)) (intptr_t) *handler);
      alarm (*seconds);
    }
#else
  errno = ENOSYS;
  if (status != NULL)
    *status = -1;
#endif
}
iexport(alarm_sub_int_i8);

