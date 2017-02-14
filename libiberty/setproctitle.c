/* Set the title of a process.
   Copyright (C) 2010-2017 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_SYS_PRCTL_H
#include <sys/types.h>
#include <sys/prctl.h>
#endif
#include "ansidecl.h"

/*

@deftypefn Supplemental void setproctitle (const char *@var{fmt}, ...)

Set the title of a process to @var{fmt}. va args not supported for now,
but defined for compatibility with BSD. 

@end deftypefn

*/

void
setproctitle (const char *name ATTRIBUTE_UNUSED, ...)
{
#ifdef PR_SET_NAME
  /* On GNU/Linux this sets the top visible "comm", but not
     necessarily the name visible in ps. */
  prctl (PR_SET_NAME, name);
#endif
}
