/* _Unwind_Frames_Extra with shadow stack for x86-64 and x86.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.

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

/* NB: We need _get_ssp and _inc_ssp from <cetintrin.h>.  But we can't
   include <x86intrin.h> which ends up including <mm_malloc.h>, which
   includes <stdlib.h> and <errno.h> unconditionally.  But we can't
   include any libc system headers unconditionally from libgcc.  Avoid
   including <mm_malloc.h> here by defining _IMMINTRIN_H_INCLUDED.  */
#define _IMMINTRIN_H_INCLUDED
#include <cetintrin.h>
#undef _IMMINTRIN_H_INCLUDED

/* Unwind the shadow stack for EH.  */
#undef _Unwind_Frames_Extra
#define _Unwind_Frames_Extra(x)			\
  do						\
    {						\
      _Unwind_Word ssp = _get_ssp ();		\
      if (ssp != 0)				\
	{					\
	  _Unwind_Word tmp = (x);		\
	  while (tmp > 255)			\
	    {					\
	      _inc_ssp (255);			\
	      tmp -= 255;			\
	    }					\
	  _inc_ssp (tmp);			\
	}					\
    }						\
    while (0)
