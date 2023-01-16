/* Copyright (C) 2007-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*****************************************************************************
 *    Non-computational Operations on Flags:
 ****************************************************************************/

#include "bid_internal.h"

// Note the following definitions from bid_conf.h: if the status flags are
// global, they have a fixed name recognized by the library functions:
// _IDEC_glbflags; pfpsf, defined as &_IDEC_glbflags, can be used instead; no
// argument is passed for the status flags to the library functions; if the 
// status flags are local then they are passed as an arument, always by
// reference, to the library functions
//
// #if !DECIMAL_GLOBAL_EXCEPTION_FLAGS
//   #define _EXC_FLAGS_PARAM , _IDEC_flags *pfpsf
// #else
//   extern _IDEC_flags _IDEC_glbflags;
//   #define _EXC_FLAGS_PARAM
//   #define pfpsf &_IDEC_glbflags
// #endif

#if DECIMAL_CALL_BY_REFERENCE
void
signalException (_IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // *pflagsmask is the logical OR of the flags to be set, e.g.
  // *pflagsmask =INVALID_EXCEPTION | ZERO_DIVIDE_EXCEPTION | OVERFLOW_EXCEPTION
  // UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION to set all five IEEE 754R
  // exception flags
  *pfpsf = *pfpsf | (*pflagsmask & BID_IEEE_FLAGS);
}
#else
void
signalException (_IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  // flagsmask is the logical OR of the flags to be set, e.g.
  // flagsmask = INVALID_EXCEPTION | ZERO_DIVIDE_EXCEPTION | OVERFLOW_EXCEPTION
  // UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION to set all five IEEE 754R
  // exception flags
  *pfpsf = *pfpsf | (flagsmask & BID_IEEE_FLAGS);
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
lowerFlags (_IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // *pflagsmask is the logical OR of the flags to be cleared, e.g.
  // *pflagsmask =INVALID_EXCEPTION | ZERO_DIVIDE_EXCEPTION | OVERFLOW_EXCEPTION
  // UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION to clear all five IEEE 754R 
  // exception flags
  *pfpsf = *pfpsf & ~(*pflagsmask & BID_IEEE_FLAGS);
}
#else
void
lowerFlags (_IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  // flagsmask is the logical OR of the flags to be cleared, e.g.
  // flagsmask = INVALID_EXCEPTION | ZERO_DIVIDE_EXCEPTION | OVERFLOW_EXCEPTION 
  // UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION to clear all five IEEE 754R    
  // exception flags
  *pfpsf = *pfpsf & ~(flagsmask & BID_IEEE_FLAGS);
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
testFlags (_IDEC_flags * praised,
	   _IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // *praised is a pointer to the result, i.e. the logical OR of the flags 
  // selected by *pflagsmask that are set; e.g. if
  // *pflagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) then upon return 
  // *praised = INVALID_EXCEPTION | INEXACT_EXCEPTION
  *praised = *pfpsf & (*pflagsmask & BID_IEEE_FLAGS);
}
#else
_IDEC_flags
testFlags (_IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  _IDEC_flags raised;
  // the raturn value raised is the logical OR of the flags  
  // selected by flagsmask, that are set; e.g. if
  // flagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION and
  // only the invalid and inexact flags are raised (set) then the return value
  // is raised = INVALID_EXCEPTION | INEXACT_EXCEPTION
  raised = *pfpsf & (flagsmask & BID_IEEE_FLAGS);
  return (raised);
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
testSavedFlags (_IDEC_flags * praised, _IDEC_flags * psavedflags,
		_IDEC_flags * pflagsmask) {
  // *praised is a pointer to the result, i.e. the logical OR of the flags
  // selected by *pflagsmask that are set in *psavedflags; e.g. if
  // *pflagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in *psavedflags
  // then upon return *praised = INVALID_EXCEPTION | INEXACT_EXCEPTION
  // Note that the flags could be saved in a global variable, but this function
  // would still expect that value as an argument passed by reference
  *praised = *psavedflags & (*pflagsmask & BID_IEEE_FLAGS);
}
#else
_IDEC_flags
testSavedFlags (_IDEC_flags savedflags, _IDEC_flags flagsmask) {
  _IDEC_flags raised;
  // the raturn value raised is the logical OR of the flags
  // selected by flagsmask, that are set in savedflags; e.g. if
  // flagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION and
  // only the invalid and inexact flags are raised (set) in savedflags
  // then the return value is raised = INVALID_EXCEPTION | INEXACT_EXCEPTION
  // Note that the flags could be saved in a global variable, but this function
  // would still expect that value as an argument passed by value
  raised = savedflags & (flagsmask & BID_IEEE_FLAGS);
  return (raised);
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
restoreFlags (_IDEC_flags * pflagsvalues,
	      _IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // restore the status flags selected by *pflagsmask to the values speciafied
  // (as a logical OR) in *pflagsvalues; e.g. if
  // *pflagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in *pflagsvalues
  // then upon return the invalid status flag will be set, the underflow status
  // flag will be clear, and the inexact status flag will be set
  *pfpsf = *pfpsf & ~(*pflagsmask & BID_IEEE_FLAGS);
  // clear flags that have to be restored
  *pfpsf = *pfpsf | (*pflagsvalues & (*pflagsmask & BID_IEEE_FLAGS));
  // restore flags
}
#else
void
restoreFlags (_IDEC_flags flagsvalues,
	      _IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  // restore the status flags selected by flagsmask to the values speciafied
  // (as a logical OR) in flagsvalues; e.g. if 
  // flagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in flagsvalues 
  // then upon return the invalid status flag will be set, the underflow status
  // flag will be clear, and the inexact status flag will be set
  *pfpsf = *pfpsf & ~(flagsmask & BID_IEEE_FLAGS);
  // clear flags that have to be restored
  *pfpsf = *pfpsf | (flagsvalues & (flagsmask & BID_IEEE_FLAGS));
  // restore flags
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
saveFlags (_IDEC_flags * pflagsvalues,
	   _IDEC_flags * pflagsmask _EXC_FLAGS_PARAM) {
  // return in *pflagsvalues the status flags specified (as a logical OR) in
  // *pflagsmask; e.g. if
  // *pflagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in the status word,
  // then upon return the value in *pflagsvalues will have the invalid status 
  // flag set, the underflow status flag clear, and the inexact status flag set
  *pflagsvalues = *pfpsf & (*pflagsmask & BID_IEEE_FLAGS);
}
#else
_IDEC_flags
saveFlags (_IDEC_flags flagsmask _EXC_FLAGS_PARAM) {
  _IDEC_flags flagsvalues;
  // return the status flags specified (as a logical OR) in flagsmask; e.g. if 
  // flagsmask = INVALID_EXCEPTION | UNDERFLOW_EXCEPTION | INEXACT_EXCEPTION
  // and only the invalid and inexact flags are raised (set) in the status word,
  // then the return value will have the invalid status  flag set, the 
  // underflow status flag clear, and the inexact status flag set 
  flagsvalues = *pfpsf & (flagsmask & BID_IEEE_FLAGS);
  return (flagsvalues);
}
#endif

// Note the following definitions from bid_conf.h (rearranged): if the rounding
// mode is global, it has a fixed name recognized by the library functions:
// _IDEC_glbround; rnd_mode, defined as &_IDEC_glbround, can be used instead; no
// argument is passed for the rounding mode to the library functions; if the
// rounding mode is local then it is passed as an arument, by reference or by
// value, to the library functions
//
// #if DECIMAL_CALL_BY_REFERENCE
//   #if !DECIMAL_GLOBAL_ROUNDING
//     #define _RND_MODE_PARAM , _IDEC_round *prnd_mode
//   #else
//     #define _RND_MODE_PARAM
//     #define rnd_mode _IDEC_glbround
//   #endif
// #else
//   #if !DECIMAL_GLOBAL_ROUNDING
//     #define _RND_MODE_PARAM , _IDEC_round rnd_mode
//   #else
//     #define _RND_MODE_PARAM
//     #define rnd_mode _IDEC_glbround
//   #endif
// #endif

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
    // #define _RND_MODE_PARAM , _IDEC_round *prnd_mode
void
getDecimalRoundingDirection (_IDEC_round * rounding_mode
			     _RND_MODE_PARAM) {
  // returns the current rounding mode
  *rounding_mode = *prnd_mode;
}
#else
    // #define _RND_MODE_PARAM
    // #define rnd_mode _IDEC_glbround
void
getDecimalRoundingDirection (_IDEC_round * rounding_mode
			     _RND_MODE_PARAM) {
  // returns the current rounding mode
  *rounding_mode = rnd_mode;
}
#endif
#else
#if !DECIMAL_GLOBAL_ROUNDING
    // #define _RND_MODE_PARAM , _IDEC_round rnd_mode
_IDEC_round
getDecimalRoundingDirection (_IDEC_round rnd_mode) {
  // returns the current rounding mode
  return (rnd_mode);
}
#else
    // #define _RND_MODE_PARAM
    // #define rnd_mode _IDEC_glbround
_IDEC_round
getDecimalRoundingDirection (void) {
  // returns the current rounding mode
  return (rnd_mode);
}
#endif
#endif

#if DECIMAL_CALL_BY_REFERENCE
#if !DECIMAL_GLOBAL_ROUNDING
    // #define _RND_MODE_PARAM , _IDEC_round *prnd_mode
void
setDecimalRoundingDirection (_IDEC_round * rounding_mode
			     _RND_MODE_PARAM) {
  // sets the current rounding mode to the value in *rounding_mode, if valid
  if (*rounding_mode == ROUNDING_TO_NEAREST ||
      *rounding_mode == ROUNDING_DOWN ||
      *rounding_mode == ROUNDING_UP ||
      *rounding_mode == ROUNDING_TO_ZERO ||
      *rounding_mode == ROUNDING_TIES_AWAY) {
    *prnd_mode = *rounding_mode;
  }
}
#else
    // #define _RND_MODE_PARAM
    // #define rnd_mode _IDEC_glbround
void
setDecimalRoundingDirection (_IDEC_round * rounding_mode
			     ) {
  // sets the global rounding mode to the value in *rounding_mode, if valid
  if (*rounding_mode == ROUNDING_TO_NEAREST ||
      *rounding_mode == ROUNDING_DOWN ||
      *rounding_mode == ROUNDING_UP ||
      *rounding_mode == ROUNDING_TO_ZERO ||
      *rounding_mode == ROUNDING_TIES_AWAY) {
    rnd_mode = *rounding_mode;
  }
}
#endif
#else
#if !DECIMAL_GLOBAL_ROUNDING
    // #define _RND_MODE_PARAM , _IDEC_round rnd_mode
_IDEC_round
setDecimalRoundingDirection (_IDEC_round rounding_mode _RND_MODE_PARAM) {
  // sets the current rounding mode to the value in rounding_mode;
  // however, when arguments are passed by value and the rounding mode
  // is a local variable, this is not of any use
  if (rounding_mode == ROUNDING_TO_NEAREST ||
      rounding_mode == ROUNDING_DOWN ||
      rounding_mode == ROUNDING_UP ||
      rounding_mode == ROUNDING_TO_ZERO ||
      rounding_mode == ROUNDING_TIES_AWAY) {
    return (rounding_mode);
  }
  return (rnd_mode);
}
#else
    // #define _RND_MODE_PARAM
    // #define rnd_mode _IDEC_glbround
void
setDecimalRoundingDirection (_IDEC_round rounding_mode) {
  // sets the current rounding mode to the value in rounding_mode, if valid;
  if (rounding_mode == ROUNDING_TO_NEAREST ||
      rounding_mode == ROUNDING_DOWN ||
      rounding_mode == ROUNDING_UP ||
      rounding_mode == ROUNDING_TO_ZERO ||
      rounding_mode == ROUNDING_TIES_AWAY) {
    rnd_mode = rounding_mode;
  }
}
#endif
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
is754 (int *retval) {
  *retval = 0;
}
#else
int
is754 (void) {
  return 0;
}
#endif

#if DECIMAL_CALL_BY_REFERENCE
void
is754R (int *retval) {
  *retval = 1;
}
#else
int
is754R (void) {
  return 1;
}
#endif
