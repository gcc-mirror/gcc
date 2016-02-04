/* Header file for the C6X EABI unwinder
   Copyright (C) 2011-2016 Free Software Foundation, Inc.

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

#ifndef UNWIND_C6X_H
#define UNWIND_C6X_H

/* Not really the ARM EABI, but pretty close.  */
#include "unwind-arm-common.h"

#define UNWIND_STACK_REG 31
/* Use A0 as a scratch register within the personality routine.  */
#define UNWIND_POINTER_REG 0

#ifdef __cplusplus
extern "C" {
#endif
  _Unwind_Reason_Code __gnu_unwind_24bit (_Unwind_Context *, _uw, int);

  /* Decode an EH table reference to a typeinfo object.  */
  static inline _Unwind_Word
  _Unwind_decode_typeinfo_ptr (_Unwind_Ptr base, _Unwind_Word ptr)
    {
      _Unwind_Word tmp;

      tmp = *(_Unwind_Word *) ptr;
      /* Zero values are always NULL.  */
      if (!tmp)
	return 0;

      /* SB-relative indirect.  Propagate the bottom 2 bits, which can
	 contain referenceness information in gnu unwinding tables.  */
      tmp += base;
      tmp = *(_Unwind_Word *) (tmp & ~(_Unwind_Word)3) | (tmp & 3);
      return tmp;
    }

#define _Unwind_GetIP(context) \
  (_Unwind_GetGR (context, 33))


#define _Unwind_SetIP(context, val) \
  _Unwind_SetGR (context, 33, val)

#ifdef __cplusplus
}   /* extern "C" */
#endif

#endif /* defined UNWIND_ARM_H */
