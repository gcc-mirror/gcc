/* Implementation of the SIGNAL and ALARM g77 intrinsics
   Copyright (C) 2005, 2007 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "libgfortran.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#include <errno.h>

#ifdef HAVE_INTPTR_T
# define INTPTR_T intptr_t
#else
# define INTPTR_T int
#endif

/* SIGNAL subroutine with PROCEDURE as handler  */
extern void signal_sub (int *, void (*)(int), int *);
iexport_proto(signal_sub);

void
signal_sub (int *number, void (*handler)(int), int *status)
{
#ifdef HAVE_SIGNAL
  INTPTR_T ret;

  if (status != NULL)
    {
      ret = (INTPTR_T) signal (*number, handler);
      *status = (int) ret;
    }
  else
    signal (*number, handler);
#else
  errno = ENOSYS;
  if (status != NULL)
    *status = -1;
#endif
}
iexport(signal_sub);


/* SIGNAL subroutine with INTEGER as handler  */
extern void signal_sub_int (int *, int *, int *);
iexport_proto(signal_sub_int);

void
signal_sub_int (int *number, int *handler, int *status)
{
#ifdef HAVE_SIGNAL
  INTPTR_T ptr = *handler, ret;

  if (status != NULL)
    {
      ret = (INTPTR_T) signal (*number, (void (*)(int)) ptr);
      *status = (int) ret;
    }
  else
    signal (*number, (void (*)(int)) ptr);
#else
  errno = ENOSYS;
  if (status != NULL)
    *status = -1;
#endif
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
#if defined (SIGALRM) && defined (HAVE_ALARM) && defined (HAVE_SIGNAL)
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
#if defined (SIGALRM) && defined (HAVE_ALARM) && defined (HAVE_SIGNAL)
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
#if defined (SIGALRM) && defined (HAVE_ALARM) && defined (HAVE_SIGNAL)
  if (status != NULL)
    {
      if (signal (SIGALRM, (void (*)(int)) (INTPTR_T) *handler) == SIG_ERR)
	*status = -1;
      else
	*status = alarm (*seconds);
    }
  else
    {
      signal (SIGALRM, (void (*)(int)) (INTPTR_T) *handler);
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
#if defined (SIGALRM) && defined (HAVE_ALARM) && defined (HAVE_SIGNAL)
  if (status != NULL)
    {
      if (signal (SIGALRM, (void (*)(int)) (INTPTR_T) *handler) == SIG_ERR)
	*status = -1;
      else
	*status = alarm (*seconds);
    }
  else
    {
      signal (SIGALRM, (void (*)(int)) (INTPTR_T) *handler);
      alarm (*seconds);
    }
#else
  errno = ENOSYS;
  if (status != NULL)
    *status = -1;
#endif
}
iexport(alarm_sub_int_i8);

