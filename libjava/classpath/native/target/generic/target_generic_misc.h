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
Description: generic target defintions of miscellaneous functions
Systems    : all
*/

#ifndef __TARGET_GENERIC_MISC__
#define __TARGET_GENERIC_MISC__

/* check if target_native_misc.h included */
#ifndef __TARGET_NATIVE_MISC__
  #error Do NOT INCLUDE generic target files! Include the corresponding native target files instead!
#endif

/****************************** Includes *******************************/
/* do not move; needed here because of some macro definitions */
#include "config.h"

#include <stdlib.h>
#include <assert.h>

#include "target_native.h"

/****************** Conditional compilation switches *******************/

/***************************** Constants *******************************/

/***************************** Datatypes *******************************/

/***************************** Variables *******************************/

/****************************** Macros *********************************/

/***********************************************************************\
* Name       : TARGET_NATIVE_MISC_FORMAT_STRING<n>
* Purpose    : format a string (with a fixed number of) arguments
* Input      : buffer     - buffer for string
*              bufferSize - size of buffer
*              format     - format string (like printf)
*              args       - optional arguments (GNU CPP only!)
* Output     : -
* Return     : -
* Side-effect: unknown
* Notes      : - this is a "safe" macro to format string; buffer-
*                overflows will be avoided. Direct usage of e. g.
*                snprintf() is not permitted because it is not ANSI C
*                (not portable!)
*              - do not use this routine in a function without
*                variable number of arguments (ellipses), because
*                va_list/va_start/va_end is used!
\***********************************************************************/

#ifndef TARGET_NATIVE_MISC_FORMAT_STRING0
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING0(buffer,bufferSize,format) \
    do { \
      snprintf(buffer,bufferSize,format); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING1
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING1(buffer,bufferSize,format,arg1) \
    do { \
      snprintf(buffer,bufferSize,format,arg1); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING2
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING2(buffer,bufferSize,format,arg1,arg2) \
    do { \
      snprintf(buffer,bufferSize,format,arg1,arg2); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING3
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING3(buffer,bufferSize,format,arg1,arg2,arg3) \
    do { \
      snprintf(buffer,bufferSize,format,arg1,arg2,arg3); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING4
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING4(buffer,bufferSize,format,arg1,arg2,arg3,arg4) \
    do { \
      snprintf(buffer,bufferSize,format,arg1,arg2,arg3,arg4); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING5
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING5(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5) \
    do { \
      snprintf(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING6
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING6(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5,arg6) \
    do { \
      snprintf(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5,arg6); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING7
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING7(buffer,bufferSize,format,arg1,arg2,arg3,arg14,arg5,arg6,arg7) \
    do { \
      snprintf(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5,arg6,arg7); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING8
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING8(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) \
    do { \
      snprintf(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8); \
    } while (0)
#endif
#ifndef TARGET_NATIVE_MISC_FORMAT_STRING9
  #include <stdarg.h>
  #define TARGET_NATIVE_MISC_FORMAT_STRING9(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) \
    do { \
      snprintf(buffer,bufferSize,format,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9); \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_FORMAT_STRING_ELLIPSE
* Purpose    : format a string with arguments
* Input      : buffer     - buffer for string
*              bufferSize - size of buffer
*              format     - format string (like printf)
* Output     : -
* Return     : -
* Side-effect: unknown
* Notes      : - this is a "safe" macro to format string; buffer-
*                overflows will be avoided. Direct usage of e. g.
*                snprintf() is not permitted because it is not ANSI C
*                (not portable!)
*              - do not use this routine in a function without
*                variable number of arguments (ellipses), because
*                va_list/va_start/va_end is used!
\***********************************************************************/

#ifndef TARGET_NATIVE_MISC_FORMAT_STRING_ELLIPSE
  #include <stdarg.h>
  #define TARGET_NATIVE_FORMAT_STRING_ELLIPSE(buffer,bufferSize,format) \
    do { \
      va_list __arguments; \
      \
      va_start(__arguments,format); \
      vsnprintf(buffer,bufferSize,format,__arguments); \
      va_end(__arguments); \
    } while (0)
#endif

/***************************** Functions *******************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
}
#endif

#endif /* __TARGET_GENERIC_MISC__ */

/* end of file */

