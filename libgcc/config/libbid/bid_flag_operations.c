/* Copyright (C) 2007  Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/*****************************************************************************
 *    Non-computational Operations on Flags:
 ****************************************************************************/

#include "bid_conf.h"
#include "bid_functions.h"
#include "bid_internal.h"

// Note the following definitions from bid_conf.h: if the status flags are
// global, they have a fixed name recognized by the library functions:
// __bid_IDEC_glbflags; pfpsf, defined as &__bid_IDEC_glbflags, can be used instead; no
// argument is passed for the status flags to the library functions; if the 
// status flags are local then they are passed as an arument, always by
// reference, to the library functions
//
// #if !DECIMAL_GLOBAL_EXCEPTION_FLAGS
//   #define _EXC_FLAGS_PARAM , _IDEC_flags *pfpsf
// #else
//   extern _IDEC_flags __bid_IDEC_glbflags;
//   #define _EXC_FLAGS_PARAM
//   #define pfpsf &__bid_IDEC_glbflags
// #endif

#if DECIMAL_CALL_BY_REFERENCE
void
__bid_lowerFlags (_IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // *pflagsmask is the logical OR of the flags to be cleared, e.g.
  // *pflagsmask =INVALID_EXCEPTION | ZERO_DIVIDE_EXCEPTION | OVERFLOW_EXCEPTION
  // UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION to clear all five IEEE 754R 
  // exception flags
  *pfpsf = *pfpsf & ~(*pflagsmask);
}
#else
void
__bid_lowerFlags (_IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  // flagsmask is the logical OR of the flags to be cleared, e.g.
  // flagsmask = INVALID_EXCEPTION | ZERO_DIVIDE_EXCEPTION | OVERFLOW_EXCEPTION 
  // UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION to clear all five IEEE 754R    
  // exception flags
  *pfpsf = *pfpsf & ~(flagsmask);
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
__bid_testFlags (_IDEC_flags * praised,
	   _IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // *praised is a pointer to the result, i.e. the logical OR of the flags 
  // selected by *pflagsmask that are set; e.g. if
  // *pflagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) then upon return 
  // *praised = INVALID_EXCEPTION | INEXACT_EXCEPTION
  *praised = *pfpsf & *pflagsmask;
}
#else
_IDEC_flags
__bid_testFlags (_IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  _IDEC_flags raised;
  // the raturn value raised is the logical OR of the flags  
  // selected by flagsmask, that are set; e.g. if
  // flagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION and
  // only the invalid and inexact flags are raised (set) then the return value
  // is raised = INVALID_EXCEPTION | INEXACT_EXCEPTION
  raised = *pfpsf & flagsmask;
  return (raised);
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
__bid_testSavedFlags (_IDEC_flags * praised, _IDEC_flags * psavedflags,
		_IDEC_flags * pflagsmask) {
  // *praised is a pointer to the result, i.e. the logical OR of the flags
  // selected by *pflagsmask that are set in *psavedflags; e.g. if
  // *pflagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in *psavedflags
  // then upon return *praised = INVALID_EXCEPTION | INEXACT_EXCEPTION
  // Note that the flags could be saved in a global variable, but this function
  // would still expect that value as an argument passed by reference
  *praised = *psavedflags & *pflagsmask;
}
#else
_IDEC_flags
__bid_testSavedFlags (_IDEC_flags savedflags, _IDEC_flags flagsmask) {
  _IDEC_flags raised;
  // the raturn value raised is the logical OR of the flags
  // selected by flagsmask, that are set in savedflags; e.g. if
  // flagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION and
  // only the invalid and inexact flags are raised (set) in savedflags
  // then the return value is raised = INVALID_EXCEPTION | INEXACT_EXCEPTION
  // Note that the flags could be saved in a global variable, but this function
  // would still expect that value as an argument passed by value
  raised = savedflags & flagsmask;
  return (raised);
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
__bid_restoreFlags (_IDEC_flags * pflagsvalues,
	      _IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // restore the status flags selected by *pflagsmask to the values speciafied
  // (as a logical OR) in *pflagsvalues; e.g. if
  // *pflagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in *pflagsvalues
  // then upon return the invalid status flag will be set, the underflow status
  // flag will be clear, and the inexact status flag will be set
  *pfpsf = *pfpsf & ~(*pflagsmask);	// clear flags that have to be restored
  *pfpsf = *pfpsf | (*pflagsvalues & *pflagsmask);	// restore flags
}
#else
void
__bid_restoreFlags (_IDEC_flags flagsvalues,
	      _IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  // restore the status flags selected by flagsmask to the values speciafied
  // (as a logical OR) in flagsvalues; e.g. if 
  // flagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in flagsvalues 
  // then upon return the invalid status flag will be set, the underflow status
  // flag will be clear, and the inexact status flag will be set
  *pfpsf = *pfpsf & ~flagsmask;	// clear flags that have to be restored
  *pfpsf = *pfpsf | (flagsvalues & flagsmask);	// restore flags
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
__bid_saveFlags (_IDEC_flags * pflagsvalues,
	   _IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // return in *pflagsvalues the status flags specified (as a logical OR) in
  // *pflagsmask; e.g. if
  // *pflagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in the status word,
  // then upon return the value in *pflagsvalues will have the invalid status 
  // flag set, the underflow status flag clear, and the inexact status flag set
  *pflagsvalues = *pfpsf & *pflagsmask;
}
#else
_IDEC_flags
__bid_saveFlags (_IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  _IDEC_flags flagsvalues;
  // return the status flags specified (as a logical OR) in flagsmask; e.g. if 
  // flagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in the status word,
  // then the return value will have the invalid status  flag set, the 
  // underflow status flag clear, and the inexact status flag set 
  flagsvalues = *pfpsf & flagsmask;
  return (flagsvalues);
}
#endif

// Note the following definitions from bid_conf.h (rearranged): if the rounding
// mode is global, it has a fixed name recognized by the library functions:
// __bid_IDEC_glbround; rnd_mode, defined as &__bid_IDEC_glbround, can be used instead; no
// argument is passed for the rounding mode to the library functions; if the
// rounding mode is local then it is passed as an arument, by reference or by
// value, to the library functions
//
// #if DECIMAL_CALL_BY_REFERENCE
//   #if !DECIMAL_GLOBAL_ROUNDING
//     #define _RND_MODE_PARAM , _IDEC_round *prnd_mode
//   #else
//     #define _RND_MODE_PARAM
//     #define rnd_mode __bid_IDEC_glbround
//   #endif
// #else
//   #if !DECIMAL_GLOBAL_ROUNDING
//     #define _RND_MODE_PARAM , _IDEC_round rnd_mode
//   #else
//     #define _RND_MODE_PARAM
//     #define rnd_mode __bid_IDEC_glbround
//   #endif
// #endif

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
    // #define _RND_MODE_PARAM , _IDEC_round *prnd_mode
void
__bid_getDecimalRoundingDirection (_IDEC_round * rounding_mode
			     _RND_MODE_PARAM) {
  // returns the current rounding mode
  *rounding_mode = *prnd_mode;
}
#else
    // #define _RND_MODE_PARAM
    // #define rnd_mode __bid_IDEC_glbround
void
__bid_getDecimalRoundingDirection (_IDEC_round * rounding_mode
			     _RND_MODE_PARAM) {
  // returns the current rounding mode
  *rounding_mode = rnd_mode;
}
#endif
#else
#if !DECIMAL_GLOBAL_ROUNDING
    // #define _RND_MODE_PARAM , _IDEC_round rnd_mode
_IDEC_round
__bid_getDecimalRoundingDirection (_IDEC_round rnd_mode) {
  // returns the current rounding mode
  return (rnd_mode);
}
#else
    // #define _RND_MODE_PARAM
    // #define rnd_mode __bid_IDEC_glbround
_IDEC_round
__bid_getDecimalRoundingDirection (void) {
  // returns the current rounding mode
  return (rnd_mode);
}
#endif
#endif

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
    // #define _RND_MODE_PARAM , _IDEC_round *prnd_mode
void
__bid_setDecimalRoundingDirection (_IDEC_round * rounding_mode
			     _RND_MODE_PARAM) {
  // sets the current rounding mode to the value in *rounding_mode
  *prnd_mode = *rounding_mode;
}
#else
    // #define _RND_MODE_PARAM
    // #define rnd_mode __bid_IDEC_glbround
void
__bid_setDecimalRoundingDirection (_IDEC_round * rounding_mode
			     _RND_MODE_PARAM) {
  // sets the global rounding mode to the value in *rounding_mode
  rnd_mode = *rounding_mode;
}
#endif
#else
#if !DECIMAL_GLOBAL_ROUNDING
    // #define _RND_MODE_PARAM , _IDEC_round rnd_mode
_IDEC_round
__bid_setDecimalRoundingDirection (_IDEC_round rounding_mode) {
  // sets the current rounding mode to the value in rounding_mode;
  // however, when arguments are passed by value and the rounding mode
  // is a local variable, this is not of any use
  return (rounding_mode);
}
#else
    // #define _RND_MODE_PARAM
    // #define rnd_mode __bid_IDEC_glbround
void
__bid_setDecimalRoundingDirection (_IDEC_round rounding_mode) {
  // sets the current rounding mode to the value in rounding_mode;
  rnd_mode = rounding_mode;
}
#endif
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
__bid_is754 (int *retval) {
  *retval = 0;
}
#else
int
__bid_is754 (void) {
  return 0;
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
__bid_is754R (int *retval) {
  *retval = 1;
}
#else
int
__bid_is754R (void) {
  return 1;
}
#endif
