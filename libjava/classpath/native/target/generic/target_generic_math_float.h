/* generic_math_int64.h - Native methods for 64bit math operations
   Copyright (C) 1998 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

/*
Description: generic target defintions of float/double constants/
             macros/functions
Systems    : all
*/

#ifndef __TARGET_GENERIC_MATH_FLOAT__
#define __TARGET_GENERIC_MATH_FLOAT__

/* check if target_native_math_float.h included */
#ifndef __TARGET_NATIVE_MATH_FLOAT__
  #error Do NOT INCLUDE generic target files! Include the corresponding native target files instead!
#endif

/****************************** Includes *******************************/
/* do not move; needed here because of some macro definitions */
#include "config.h"

#include <stdlib.h>
#include <assert.h>

#include <jni.h>

/****************** Conditional compilation switches *******************/

/***************************** Constants *******************************/

/***************************** Datatypes *******************************/

/***************************** Variables *******************************/

/****************************** Macros *********************************/

/* test float/double values for NaN,Inf */
#ifndef TARGET_NATIVE_MATH_FLOAT_FLOAT_ISNAN
  #include <math.h>
  #define TARGET_NATIVE_MATH_FLOAT_FLOAT_ISNAN(f) isnan(f)
#endif
#ifndef TARGET_NATIVE_MATH_FLOAT_FLOAT_ISINF
  #include <math.h>
  #define TARGET_NATIVE_MATH_FLOAT_FLOAT_ISINF(f) isinf(f)
#endif
#ifndef TARGET_NATIVE_MATH_FLOAT_FLOAT_FINITE
  #include <math.h>
  #define TARGET_NATIVE_MATH_FLOAT_FLOAT_FINITE(f) finite(f)
#endif

#ifndef TARGET_NATIVE_MATH_FLOAT_DOUBLE_ISNAN
  #include <math.h>
  #define TARGET_NATIVE_MATH_FLOAT_DOUBLE_ISNAN(d) isnan(d)
#endif
#ifndef TARGET_NATIVE_MATH_FLOAT_DOUBLE_ISINF
  #include <math.h>
  #define TARGET_NATIVE_MATH_FLOAT_DOUBLE_ISINF(d) isinf(d)
#endif
#ifndef TARGET_NATIVE_MATH_FLOAT_DOUBLE_FINITE
  #include <math.h>
  #define TARGET_NATIVE_MATH_FLOAT_DOUBLE_FINITE(d) finite(d)
#endif

/* division, modulo operations (used to avoid unexcepted exceptions on some
   targets; generic codes are direct operations without checks)
*/
#ifndef TARGET_NATIVE_MATH_FLOAT_FLOAT_DIV
  #define TARGET_NATIVE_MATH_FLOAT_FLOAT_DIV(f0,f1) ((f0)/(f1))
#endif
#ifndef TARGET_NATIVE_MATH_FLOAT_FLOAT_MOD
  #include <math.h>
  #define TARGET_NATIVE_MATH_FLOAT_FLOAT_MOD(f0,f1) ((jfloat)fmod((jdouble)(f0),(jdouble)(f1)))
#endif

#ifndef TARGET_NATIVE_MATH_FLOAT_DOUBLE_DIV
  #define TARGET_NATIVE_MATH_FLOAT_DOUBLE_DIV(d0,d1) ((d0)/(d1))
#endif
#ifndef TARGET_NATIVE_MATH_FLOAT_DOUBLE_MOD
  #include <math.h>
  #define TARGET_NATIVE_MATH_FLOAT_DOUBLE_MOD(d0,d1) fmod(d0,d1)
#endif

/***************************** Functions *******************************/

#ifdef __cplusplus
extern "C"
#endif

#ifdef __cplusplus
}
#endif

#endif /* __TARGET_GENERIC_MATH_FLOAT__ */

/* end of file */

