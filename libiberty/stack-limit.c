/* Increase stack size limit if possible.
   Copyright (C) 2011-2022 Free Software Foundation, Inc.

This file is part of the libiberty library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

/*

@deftypefn Extension void stack_limit_increase (unsigned long @var{pref})

Attempt to increase stack size limit to @var{pref} bytes if possible.

@end deftypefn

*/

#include "config.h"
#include "ansidecl.h"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

void
stack_limit_increase (unsigned long pref ATTRIBUTE_UNUSED)
{
#if defined(HAVE_SETRLIMIT) && defined(HAVE_GETRLIMIT) \
    && defined(RLIMIT_STACK) && defined(RLIM_INFINITY)
  struct rlimit rlim;
  if (getrlimit (RLIMIT_STACK, &rlim) == 0
      && rlim.rlim_cur != RLIM_INFINITY
      && rlim.rlim_cur < pref
      && (rlim.rlim_max == RLIM_INFINITY || rlim.rlim_cur < rlim.rlim_max))
    {
      rlim.rlim_cur = pref;
      if (rlim.rlim_max != RLIM_INFINITY && rlim.rlim_cur > rlim.rlim_max)
	rlim.rlim_cur = rlim.rlim_max;
      setrlimit (RLIMIT_STACK, &rlim);
    }
#endif
}
