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

#ifndef _BID_CONF_H
#define _BID_CONF_H

#include "bid_intrinsics.h"

#ifdef IN_LIBGCC2
#if !defined ENABLE_DECIMAL_BID_FORMAT || !ENABLE_DECIMAL_BID_FORMAT
#error BID not enabled in libbid
#endif

#ifndef BID_BIG_ENDIAN
#define BID_BIG_ENDIAN LIBGCC2_FLOAT_WORDS_BIG_ENDIAN
#endif

#ifndef BID_THREAD
#if defined (HAVE_CC_TLS) && defined (USE_TLS)
#define BID_THREAD __thread
#endif
#endif

#define _intptr_t_defined
#define DECIMAL_CALL_BY_REFERENCE 0
#define DECIMAL_GLOBAL_ROUNDING 1
#define DECIMAL_GLOBAL_EXCEPTION_FLAGS 1
#endif	/* IN_LIBGCC2 */

// Configuration Options

#define SET_STATUS_FLAGS

#ifndef BID_THREAD
#define BID_THREAD
#endif

// If DECIMAL_CALL_BY_REFERENCE is defined then numerical arguments and results
// are passed by reference otherwise they are passed by value (except that
// a pointer is always passed to the status flags)

#ifndef DECIMAL_CALL_BY_REFERENCE
#define DECIMAL_CALL_BY_REFERENCE 0
#endif

// If DECIMAL_GLOBAL_ROUNDING is defined then the rounding mode is a global 
// variable __bid_IDEC_glbround, otherwise it is passed as a parameter when needed

#ifndef DECIMAL_GLOBAL_ROUNDING
#define DECIMAL_GLOBAL_ROUNDING 0
#endif

// If DECIMAL_GLOBAL_EXCEPTION_FLAGS is defined then the exception status flags
// are represented by a global variable __bid_IDEC_glbflags, otherwise they are 
// passed as a parameter when needed

#ifndef DECIMAL_GLOBAL_EXCEPTION_FLAGS
#define DECIMAL_GLOBAL_EXCEPTION_FLAGS 0
#endif

// If DECIMAL_ALTERNATE_EXCEPTION_HANDLING is defined then the exception masks
// are examined and exception handling information is provided to the caller 
// if alternate exception handling is necessary

#ifndef DECIMAL_ALTERNATE_EXCEPTION_HANDLING
#define DECIMAL_ALTERNATE_EXCEPTION_HANDLING 0
#endif

typedef unsigned int _IDEC_round;
typedef unsigned int _IDEC_flags; // could be a struct with diagnostic info

#if DECIMAL_ALTERNATE_EXCEPTION_HANDLING
  // If DECIMAL_GLOBAL_EXCEPTION_MASKS is defined then the exception mask bits
  // are represented by a global variable _IDEC_exceptionmasks, otherwise they
  // are passed as a parameter when needed; DECIMAL_GLOBAL_EXCEPTION_MASKS is 
  // ignored
  // if DECIMAL_ALTERNATE_EXCEPTION_HANDLING is not defined
  // **************************************************************************
#define DECIMAL_GLOBAL_EXCEPTION_MASKS 0
  // **************************************************************************

  // If DECIMAL_GLOBAL_EXCEPTION_INFO is defined then the alternate exception  
  // handling information is represented by a global data structure 
  // _IDEC_glbexcepthandling, otherwise it is passed by reference as a
  // parameter when needed; DECIMAL_GLOBAL_EXCEPTION_INFO is ignored
  // if DECIMAL_ALTERNATE_EXCEPTION_HANDLING is not defined
  // **************************************************************************
#define DECIMAL_GLOBAL_EXCEPTION_INFO 0
  // **************************************************************************
#endif

// Notes: 1) rnd_mode from _RND_MODE_ARG is used by the caller of a function
//           from this library, and can be any name
//        2) rnd_mode and prnd_mode from _RND_MODE_PARAM are fixed names 
//           and *must* be used in the library functions
//        3) __bid_IDEC_glbround is the fixed name for the global variable holding 
//           the rounding mode
#if !DECIMAL_GLOBAL_ROUNDING
#if DECIMAL_CALL_BY_REFERENCE
#define _RND_MODE_ARG , &rnd_mode
#define _RND_MODE_PARAM , _IDEC_round *prnd_mode
#else
#define _RND_MODE_ARG , rnd_mode
#define _RND_MODE_PARAM , _IDEC_round rnd_mode
#endif
#else
#define _RND_MODE_ARG
#define _RND_MODE_PARAM
#define rnd_mode __bid_IDEC_glbround
#endif

// Notes: 1) pfpsf from _EXC_FLAGS_ARG is used by the caller of a function
//           from this library, and can be any name 
//        2) pfpsf from _EXC_FLAGS_PARAM is a fixed name and *must* be used  
//           in the library functions
//        3) __bid_IDEC_glbflags is the fixed name for the global variable holding 
//           the floating-point status flags
#if !DECIMAL_GLOBAL_EXCEPTION_FLAGS
#define _EXC_FLAGS_ARG , pfpsf
#define _EXC_FLAGS_PARAM , _IDEC_flags *pfpsf
#else
#define _EXC_FLAGS_ARG
#define _EXC_FLAGS_PARAM
#define pfpsf &__bid_IDEC_glbflags
#endif

#if DECIMAL_GLOBAL_ROUNDING
extern BID_THREAD _IDEC_round __bid_IDEC_glbround;
#endif

#if DECIMAL_GLOBAL_EXCEPTION_FLAGS
extern BID_THREAD _IDEC_flags __bid_IDEC_glbflags;
#endif

#if DECIMAL_ALTERNATE_EXCEPTION_HANDLING
#if DECIMAL_GLOBAL_EXCEPTION_MASKS
extern _IDEC_exceptionmasks _IDEC_glbexceptionmasks;
#endif
#if DECIMAL_GLOBAL_EXCEPTION_INFO
extern _IDEC_excepthandling _IDEC_glbexcepthandling;
#endif
#endif

#if DECIMAL_ALTERNATE_EXCEPTION_HANDLING

  // Notes: 1) exc_mask from _EXC_MASKS_ARG is used by the caller of a function
  //           from this library, and can be any name
  //        2) exc_mask and pexc_mask from _EXC_MASKS_PARAM are fixed names
  //           and *must* be used in the library functions
  //        3) _IDEC_glbexceptionmasks is the fixed name for the global 
  //           variable holding the floating-point exception masks
#if !DECIMAL_GLOBAL_EXCEPTION_MASKS
#if DECIMAL_CALL_BY_REFERENCE
#define _EXC_MASKS_ARG , &exc_mask
#define _EXC_MASKS_PARAM , _IDEC_exceptionmasks *pexc_mask
#else
#define _EXC_MASKS_ARG , exc_mask
#define _EXC_MASKS_PARAM , _IDEC_exceptionmasks exc_mask
#endif
#else
#define _EXC_MASKS_ARG
#define _EXC_MASKS_PARAM
#define exc_mask _IDEC_glbexceptionmasks
#endif

  // Notes: 1) pexc_info from _EXC_INFO_ARG is used by the caller of a function
  //           from this library, and can be any name
  //        2) pexc_info from _EXC_INFO_PARAM is a fixed name and *must* be  
  //           used in the library functions
  //        3) _IDEC_glbexcepthandling is the fixed name for the global  
  //           variable holding the floating-point exception information
#if !DECIMAL_GLOBAL_EXCEPTION_INFO
#define _EXC_INFO_ARG , pexc_info
#define _EXC_INFO_PARAM , _IDEC_excepthandling *pexc_info
#else
#define _EXC_INFO_ARG
#define _EXC_INFO_PARAM
#define pexc_info &_IDEC_glbexcepthandling
#endif
#else
#define _EXC_MASKS_ARG
#define _EXC_MASKS_PARAM
#define _EXC_INFO_ARG
#define _EXC_INFO_PARAM
#endif


#ifndef BID_BIG_ENDIAN
#define BID_BIG_ENDIAN 0
#endif

#if BID_BIG_ENDIAN
#define BID_SWAP128(x) {  \
  UINT64 sw;              \
  sw = (x).w[1];          \
  (x).w[1] = (x).w[0];    \
  (x).w[0] = sw;          \
  }                       
#else
#define BID_SWAP128(x)  
#endif

#if DECIMAL_CALL_BY_REFERENCE
#define BID_RETURN_VAL(x) { *pres = (x); return; }
#if BID_BIG_ENDIAN && defined BID_128RES
#define BID_RETURN(x) { BID_SWAP128(x); *pres = (x); return; }
#else
#define BID_RETURN(x) { *pres = (x); return; }
#endif 
#else
#define BID_RETURN_VAL(x) return(x);
#if BID_BIG_ENDIAN && defined BID_128RES
#define BID_RETURN(x) { BID_SWAP128(x); return(x); }
#else
#define BID_RETURN(x) return(x);
#endif 
#endif 

#if DECIMAL_CALL_BY_REFERENCE
#define BIDECIMAL_CALL1(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1NR(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2(_FUNC, _RES, _OP1, _OP2) \
    _FUNC(&(_RES), &(_OP1), &(_OP2) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2NR(_FUNC, _RES, _OP1, _OP2) \
    _FUNC(&(_RES), &(_OP1), &(_OP2) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2_NORND(_FUNC, _RES, _OP1, _OP2) \
    _FUNC(&(_RES), &(_OP1), &(_OP2) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_RESREF(_FUNC, _RES, _OP1) \
    _FUNC((_RES), &(_OP1) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_RESARG(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), (_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_RESREF(_FUNC, _RES, _OP1) \
    _FUNC((_RES), &(_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_NOSTAT(_FUNC, _RES, _OP1) \
    _FUNC(&(_RES), &(_OP1) _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2_NORND_NOSTAT(_FUNC, _RES, _OP1, _OP2) \
    _FUNC(&(_RES), &(_OP1), &(_OP2) _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL3(_FUNC, _RES, _OP1, _OP2, _OP3) \
    _FUNC(&(_RES), &(_OP1), &(_OP2), &(_OP3) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#else
#define BIDECIMAL_CALL1(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1NR(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2(_FUNC, _RES, _OP1, _OP2) \
    _RES = _FUNC((_OP1), (_OP2) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2NR(_FUNC, _RES, _OP1, _OP2) \
    _RES = _FUNC((_OP1), (_OP2) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2_NORND(_FUNC, _RES, _OP1, _OP2) \
    _RES = _FUNC((_OP1), (_OP2) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_RESREF(_FUNC, _RES, _OP1) \
    _FUNC((_RES), _OP1 _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_RESARG(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_RESREF(_FUNC, _RES, _OP1) \
    _FUNC((_RES), _OP1 _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL1_NORND_NOSTAT(_FUNC, _RES, _OP1) \
    _RES = _FUNC((_OP1) _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL2_NORND_NOSTAT(_FUNC, _RES, _OP1, _OP2) \
    _RES = _FUNC((_OP1), (_OP2) _EXC_MASKS_ARG _EXC_INFO_ARG)
#define BIDECIMAL_CALL3(_FUNC, _RES, _OP1, _OP2, _OP3) \
    _RES = _FUNC((_OP1), (_OP2), (_OP3) _RND_MODE_ARG _EXC_FLAGS_ARG _EXC_MASKS_ARG _EXC_INFO_ARG)
#endif

#if BID_BIG_ENDIAN
#define HIGH_128W 0
#define LOW_128W  1
#else
#define HIGH_128W 1
#define LOW_128W  0
#endif

#if BID_BIG_ENDIAN
#define COPY_ARG_REF(arg_name) \
       UINT128 arg_name={ pbid_##arg_name->w[1], pbid_##arg_name->w[0]};
#define COPY_ARG_VAL(arg_name) \
       UINT128 arg_name={ bid_##arg_name.w[1], bid_##arg_name.w[0]};
#else
#define COPY_ARG_REF(arg_name) \
       UINT128 arg_name=*pbid_##arg_name;
#define COPY_ARG_VAL(arg_name) \
       UINT128 arg_name= bid_##arg_name;
#endif 

#define COPY_ARG_TYPE_REF(type, arg_name) \
       type arg_name=*pbid_##arg_name;
#define COPY_ARG_TYPE_VAL(type, arg_name) \
       type arg_name= bid_##arg_name;

#if !DECIMAL_GLOBAL_ROUNDING
  #define SET_RND_MODE() \
  _IDEC_round rnd_mode = *prnd_mode;
#else
  #define SET_RND_MODE()
#endif

#define PROLOG_REF(arg_name) \
       COPY_ARG_REF(arg_name) 

 #define PROLOG_VAL(arg_name) \
       COPY_ARG_VAL(arg_name) 

#define PROLOG_TYPE_REF(type, arg_name) \
       COPY_ARG_TYPE_REF(type, arg_name) 

#define PROLOG_TYPE_VAL(type, arg_name) \
      COPY_ARG_TYPE_VAL(type, arg_name) 

#define OTHER_PROLOG_REF()     
#define OTHER_PROLOG_VAL()     

#if DECIMAL_CALL_BY_REFERENCE
#define       BID128_FUNCTION_ARG1(fn_name, arg_name)\
      void fn_name (UINT128 * pres, \
           UINT128 *  \
           pbid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       BID128_FUNCTION_ARG1_NORND(fn_name, arg_name)\
      void fn_name (UINT128 * pres, \
           UINT128 *  \
           pbid_##arg_name _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name)   \
     OTHER_PROLOG_REF()     

#define       BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE(restype, fn_name, arg_name)\
      void fn_name (restype * pres, \
           UINT128 *  \
           pbid_##arg_name _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name)   \
     OTHER_PROLOG_REF()     

#define       BID128_FUNCTION_ARG2(fn_name, arg_name1, arg_name2)\
      void fn_name (UINT128 * pres, \
           UINT128 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_REF(arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       BID128_FUNCTION_ARG2_NORND_CUSTOMRESTYPE(restype, fn_name, arg_name1, arg_name2)\
      void fn_name (restype * pres, \
           UINT128 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_REF(arg_name2)   \
     OTHER_PROLOG_REF()     

#define       BID128_FUNCTION_ARG128_ARGTYPE2(fn_name, arg_name1, type2, arg_name2)\
      void fn_name (UINT128 * pres, \
           UINT128 *pbid_##arg_name1,  type2 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_TYPE_REF(type2, arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       TYPE0_FUNCTION_ARGTYPE1_ARGTYPE2(type0, fn_name, type1, arg_name1, type2, arg_name2)\
      void fn_name (type0 *pres, \
           type1 *pbid_##arg_name1,  type2 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name1)   \
     PROLOG_TYPE_REF(type2, arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       BID128_FUNCTION_ARGTYPE1_ARG128(fn_name, type1, arg_name1, arg_name2)\
      void fn_name (UINT128 * pres, \
           type1 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name1)   \
     PROLOG_REF(arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       TYPE0_FUNCTION_ARG128_ARGTYPE2(type0, fn_name, arg_name1, type2, arg_name2)\
      void fn_name (type0 *pres, \
           UINT128 *pbid_##arg_name1,  type2 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_TYPE_REF(type2, arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       TYPE0_FUNCTION_ARGTYPE1_ARG128(type0, fn_name, type1, arg_name1, arg_name2)\
      void fn_name (type0 *pres, \
           type1 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name1)   \
     PROLOG_REF(arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       TYPE0_FUNCTION_ARG128_ARG128(type0, fn_name, arg_name1, arg_name2)\
      void fn_name (type0 * pres, \
           UINT128 *pbid_##arg_name1,  UINT128 *pbid_##arg_name2  \
           _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name1)   \
     PROLOG_REF(arg_name2)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       TYPE0_FUNCTION_ARG1(type0, fn_name, arg_name)\
      void fn_name (type0 * pres, \
           UINT128 *  \
           pbid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_REF(arg_name)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       BID128_FUNCTION_ARGTYPE1(fn_name, type1, arg_name)\
      void fn_name (UINT128 * pres, \
           type1 *  \
           pbid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       TYPE0_FUNCTION_ARGTYPE1(type0, fn_name, type1, arg_name)\
      void fn_name (type0 * pres, \
           type1 *  \
           pbid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name)   \
     SET_RND_MODE()         \
     OTHER_PROLOG_REF()     

#define       TYPE0_FUNCTION_ARGTYPE1_NORND(type0, fn_name, type1, arg_name)\
      void fn_name (type0 * pres, \
           type1 *  \
           pbid_##arg_name _EXC_FLAGS_PARAM _EXC_MASKS_PARAM \
           _EXC_INFO_PARAM) {\
     PROLOG_TYPE_REF(type1, arg_name)   \
     OTHER_PROLOG_REF()
 
//////////////////////////////////////////
/////////////////////////////////////////
////////////////////////////////////////
 
#else
 
//////////////////////////////////////////
/////////////////////////////////////////
////////////////////////////////////////
 
#define       BID128_FUNCTION_ARG1(fn_name, arg_name)\
     UINT128                                     \
     fn_name (UINT128 bid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_VAL(arg_name)                      \
     OTHER_PROLOG_VAL()     

#define       BID128_FUNCTION_ARG1_NORND(fn_name, arg_name)\
     UINT128                                     \
     fn_name (UINT128 bid_##arg_name _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_VAL(arg_name)                      \
     OTHER_PROLOG_VAL()     

#define       BID128_FUNCTION_ARG1_NORND_CUSTOMRESTYPE(restype, fn_name, arg_name)\
     restype                                     \
     fn_name (UINT128 bid_##arg_name _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_VAL(arg_name)                      \
     OTHER_PROLOG_VAL()     

#define       BID128_FUNCTION_ARG2(fn_name, arg_name1, arg_name2)\
     UINT128                                     \
     fn_name (UINT128 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_VAL(arg_name2)                      \
     OTHER_PROLOG_VAL() 

#define       BID128_FUNCTION_ARG2_NORND_CUSTOMRESTYPE(restype, fn_name, arg_name1, arg_name2)\
     restype                                    \
     fn_name (UINT128 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_VAL(arg_name2)                      \
     OTHER_PROLOG_VAL() 

#define       BID128_FUNCTION_ARG128_ARGTYPE2(fn_name, arg_name1, type2, arg_name2)\
     UINT128                                     \
     fn_name (UINT128 bid_##arg_name1,      \
            type2 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_TYPE_VAL(type2, arg_name2)          \
     OTHER_PROLOG_VAL() 

#define       TYPE0_FUNCTION_ARGTYPE1_ARGTYPE2(type0, fn_name, type1, arg_name1, type2, arg_name2)\
     type0                                     \
     fn_name (type1 bid_##arg_name1,      \
            type2 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_TYPE_VAL(type1, arg_name1)                      \
     PROLOG_TYPE_VAL(type2, arg_name2)          \
     OTHER_PROLOG_VAL() 

#define       BID128_FUNCTION_ARGTYPE1_ARG128(fn_name, type1, arg_name1, arg_name2)\
     UINT128                                     \
     fn_name (type1 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_TYPE_VAL(type1, arg_name1)          \
     PROLOG_VAL(arg_name2)                      \
     OTHER_PROLOG_VAL() 

#define       TYPE0_FUNCTION_ARG128_ARGTYPE2(type0, fn_name, arg_name1, type2, arg_name2)\
     type0                                     \
     fn_name (UINT128 bid_##arg_name1,      \
            type2 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_TYPE_VAL(type2, arg_name2)          \
     OTHER_PROLOG_VAL() 

#define       TYPE0_FUNCTION_ARGTYPE1_ARG128(type0, fn_name, type1, arg_name1, arg_name2)\
     type0                                     \
     fn_name (type1 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_TYPE_VAL(type1, arg_name1)                      \
     PROLOG_VAL(arg_name2)          \
     OTHER_PROLOG_VAL() 

#define       TYPE0_FUNCTION_ARG128_ARG128(type0, fn_name, arg_name1, arg_name2)\
     type0                                     \
     fn_name (UINT128 bid_##arg_name1,      \
            UINT128 bid_##arg_name2 _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) {  \
     PROLOG_VAL(arg_name1)                      \
     PROLOG_VAL(arg_name2)                      \
     OTHER_PROLOG_VAL() 

#define       TYPE0_FUNCTION_ARG1(type0, fn_name, arg_name)\
     type0                                     \
     fn_name (UINT128 bid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_VAL(arg_name)                      \
     OTHER_PROLOG_VAL()     

#define       BID128_FUNCTION_ARGTYPE1(fn_name, type1, arg_name)\
     UINT128                                     \
     fn_name (type1 bid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_TYPE_VAL(type1, arg_name)                      \
     OTHER_PROLOG_VAL()     

#define       TYPE0_FUNCTION_ARGTYPE1(type0, fn_name, type1, arg_name)\
     type0                                     \
     fn_name (type1 bid_##arg_name _RND_MODE_PARAM _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_TYPE_VAL(type1, arg_name)                      \
     OTHER_PROLOG_VAL()     

#define       TYPE0_FUNCTION_ARGTYPE1_NORND(type0, fn_name, type1, arg_name)\
     type0                                     \
     fn_name (type1 bid_##arg_name _EXC_FLAGS_PARAM  \
           _EXC_MASKS_PARAM _EXC_INFO_PARAM) { \
     PROLOG_TYPE_VAL(type1, arg_name)                      \
     OTHER_PROLOG_VAL()
      
#endif 



#define   BID_TO_SMALL_UINT_CVT_FUNCTION(type0, fn_name, type1, arg_name, cvt_fn_name, type2, size_mask, invalid_res)\
    TYPE0_FUNCTION_ARGTYPE1_NORND(type0, fn_name, type1, arg_name)\
        type2 res;                                                    \
        _IDEC_flags saved_fpsc=*pfpsf;                                \
    BIDECIMAL_CALL1_NORND(cvt_fn_name, res, arg_name);            \
        if(res & size_mask) {                                         \
      *pfpsf = saved_fpsc | INVALID_EXCEPTION;                    \
          res = invalid_res; }                                        \
    BID_RETURN_VAL((type0)res);                                   \
                   } 
      
#define   BID_TO_SMALL_INT_CVT_FUNCTION(type0, fn_name, type1, arg_name, cvt_fn_name, type2, size_mask, invalid_res)\
    TYPE0_FUNCTION_ARGTYPE1_NORND(type0, fn_name, type1, arg_name)\
        type2 res, sgn_mask;                                          \
        _IDEC_flags saved_fpsc=*pfpsf;                                \
    BIDECIMAL_CALL1_NORND(cvt_fn_name, res, arg_name);            \
        sgn_mask = res & size_mask;                                   \
        if(sgn_mask && (sgn_mask != size_mask)) {                     \
      *pfpsf = saved_fpsc | INVALID_EXCEPTION;                    \
          res = invalid_res; }                                        \
    BID_RETURN_VAL((type0)res);                                   \
                   } 



#endif

