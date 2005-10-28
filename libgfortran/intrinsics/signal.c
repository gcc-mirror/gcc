/* Implementation of the SIGNAL and ALARM g77 intrinsics
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

#include "config.h"
#include "libgfortran.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include <errno.h>

/* SIGNAL subroutine with PROCEDURE as handler  */
extern void signal_sub (int *, void (*)(int), int *);
iexport_proto(signal_sub);

void
signal_sub (int *number, void (*handler)(int), int *status)
{
#ifdef HAVE_SIGNAL
  if (status != NULL)
    *status = (int) signal (*number, handler);
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
  if (status != NULL)
    *status = (int) signal (*number, (void (*)(int)) *handler);
  else
    signal (*number, (void (*)(int)) *handler);
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
extern void alarm_sub (int *, void (*)(int), int *);
iexport_proto(alarm_sub);

void
alarm_sub (int *seconds, void (*handler)(int), int *status)
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
iexport(alarm_sub);


/* ALARM intrinsic with INTEGER as handler  */
extern void alarm_sub_int (int *, int *, int *);
iexport_proto(alarm_sub_int);

void
alarm_sub_int (int *seconds, int *handler, int *status)
{
#if defined (SIGALRM) && defined (HAVE_ALARM) && defined (HAVE_SIGNAL)
  if (status != NULL)
    {
      if (signal (SIGALRM, (void (*)(int)) handler) == SIG_ERR)
	*status = -1;
      else
	*status = alarm (*seconds);
    }
  else
    {
      signal (SIGALRM, (void (*)(int)) handler);
      alarm (*seconds);
    }
#else
  errno = ENOSYS;
  if (status != NULL)
    *status = -1;
#endif
}
iexport(alarm_sub_int);

