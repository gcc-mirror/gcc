/* Header file for the ARM EABI unwinder
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
   Contributed by Paul Brook

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
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

/* Language-independent unwinder header public defines.  This contains both
   ABI defined objects, and GNU support routines.  */

#ifndef UNWIND_ARM_H
#define UNWIND_ARM_H

#include "unwind-arm-common.h"

#define UNWIND_STACK_REG 13
/* Use IP as a scratch register within the personality routine.  */
#define UNWIND_POINTER_REG 12

#ifdef __cplusplus
extern "C" {
#endif
  /* Decode an R_ARM_TARGET2 relocation.  */
  static inline _Unwind_Word
  _Unwind_decode_typeinfo_ptr (_Unwind_Word base __attribute__ ((unused)),
                               _Unwind_Word ptr)
    {
      _Unwind_Word tmp;

      tmp = *(_Unwind_Word *) ptr;
      /* Zero values are always NULL.  */
      if (!tmp)
	return 0;

#if (defined(linux) && !defined(__uClinux__)) || defined(__NetBSD__) \
    || defined(__FreeBSD__) || defined(__fuchsia__)
      /* Pc-relative indirect.  */
#define _GLIBCXX_OVERRIDE_TTYPE_ENCODING (DW_EH_PE_pcrel | DW_EH_PE_indirect)
      tmp += ptr;
      tmp = *(_Unwind_Word *) tmp;
#elif defined(__symbian__) || defined(__uClinux__)
#define _GLIBCXX_OVERRIDE_TTYPE_ENCODING (DW_EH_PE_absptr)
      /* Absolute pointer.  Nothing more to do.  */
#else
#define _GLIBCXX_OVERRIDE_TTYPE_ENCODING (DW_EH_PE_pcrel)
      /* Pc-relative pointer.  */
      tmp += ptr;
#endif
      return tmp;
    }

  static inline _Unwind_Reason_Code
  __gnu_unwind_24bit (_Unwind_Context * context __attribute__ ((unused)),
                      _uw data __attribute__ ((unused)),
                      int compact __attribute__ ((unused)))
    {
      return _URC_FAILURE;
    }
#ifndef __FreeBSD__
  /* Return the address of the instruction, not the actual IP value.  */
#define _Unwind_GetIP(context) \
  (_Unwind_GetGR (context, 15) & ~(_Unwind_Word)1)

#define _Unwind_SetIP(context, val) \
  _Unwind_SetGR (context, 15, val | (_Unwind_GetGR (context, 15) & 1))
#else
  #undef _Unwind_GetIPInfo
  _Unwind_Ptr _Unwind_GetIP (struct _Unwind_Context *);
  _Unwind_Ptr _Unwind_GetIPInfo (struct _Unwind_Context *, int *);
  void _Unwind_SetIP (struct _Unwind_Context *, _Unwind_Ptr);
#endif

#ifdef __cplusplus
}   /* extern "C" */
#endif

#endif /* defined UNWIND_ARM_H */
