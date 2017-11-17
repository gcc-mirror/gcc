/* _Unwind_Frames_Extra with shadow stack for x86-64 and x86.
   Copyright (C) 2017 Free Software Foundation, Inc.

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

#ifdef __x86_64__
# define incssp(x) __builtin_ia32_incsspq ((x))
# define rdssp(x) __builtin_ia32_rdsspq (x)
#else
# define incssp(x) __builtin_ia32_incsspd ((x))
# define rdssp(x) __builtin_ia32_rdsspd (x)
#endif

/* Unwind the shadow stack for EH.  */
#undef _Unwind_Frames_Extra
#define _Unwind_Frames_Extra(x)			\
  do						\
    {						\
      unsigned long ssp = 0;			\
      ssp = rdssp (ssp);			\
      if (ssp != 0)				\
	{					\
	  unsigned long tmp = (x);		\
	  while (tmp > 255)			\
	    {					\
	      incssp (tmp);			\
	      tmp -= 255;			\
	    }					\
	  incssp (tmp);				\
	}					\
    }						\
    while (0)
