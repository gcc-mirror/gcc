/* except-gcc.h                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2009-2013, Intel Corporation
 *  All rights reserved.
 *  
 *  @copyright
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *  
 *  @copyright
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 **************************************************************************/

/**
 * @file except-gcc.h
 *
 * @brief ABI for gcc exception handling.
 *
 * @par Origin
 * The code below is generally copied from the Intel Itanium ABI (Intel
 * download 245370).
 */

#ifndef INCLUDED_EXCEPT_GCC_DOT_H
#define INCLUDED_EXCEPT_GCC_DOT_H

#ifndef __cplusplus
#   error except-gcc.h should be used in C++ code only.
#endif

#include <cilk/common.h>
#include <exception>
#include <typeinfo>

struct __cxa_exception;

__CILKRTS_BEGIN_EXTERN_C

/** Unwind reason code (Itanium ABI 6.1.2.1) */
typedef enum _Unwind_Reason_Code {
    _URC_NO_REASON = 0,
    _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
    _URC_FATAL_PHASE2_ERROR = 2,
    _URC_FATAL_PHASE1_ERROR = 3,
    _URC_NORMAL_STOP = 4,
    _URC_END_OF_STACK = 5,
    _URC_HANDLER_FOUND = 6,
    _URC_INSTALL_CONTEXT = 7,
    _URC_CONTINUE_UNWIND = 8
} _Unwind_Reason_Code;

typedef struct _Unwind_Exception _Unwind_Exception;

/** Exception cleanup function pointer (Itanium ABI 6.1.2.2) */
typedef void (*_Unwind_Exception_Cleanup_Fn)(_Unwind_Reason_Code reason,
                                             _Unwind_Exception *exc);

/**
 * @brief Exception undwinding information
 *
 * This is copied from the Intel Itanium ABI except that the
 * private fields are declared unsigned long for binary
 * compatibility with gcc/g++ on 32 bit machines.
 */
struct _Unwind_Exception
{
    uint64_t                     exception_class;
    _Unwind_Exception_Cleanup_Fn exception_cleanup;
    unsigned long                private_1;
    unsigned long                private_2;
};

/** Throw or rethrow an exception */
_Unwind_Reason_Code
_Unwind_RaiseException(_Unwind_Exception *exception_object);

/** Resume an exception other than by rethrowing it. */
void _Unwind_Resume(_Unwind_Exception *exception_object);

/** Delete an exception object */
void _Unwind_DeleteException(_Unwind_Exception *exception_object);

/**
 * C++ exception ABI.
 *  The following declarations are from
 *
 * http://www.codesourcery.com/public/cxx-abi/abi-eh.html#cxx-abi
 */

struct __cxa_exception {
    std::type_info *        exceptionType;
    void (*exceptionDestructor)(void *); 
    std::unexpected_handler unexpectedHandler;
    std::terminate_handler  terminateHandler;
    __cxa_exception *       nextException;

    int                     handlerCount;
    int                     handlerSwitchValue;
    const char *            actionRecord;
    const char *            languageSpecificData;
    void *                  catchTemp;
    void *                  adjustedPtr;

    _Unwind_Exception       unwindHeader;
};

static inline __cxa_exception *to_cxx(_Unwind_Exception *e)
{
    return ((__cxa_exception *)(e+1)) - 1;
}

typedef struct __cxa_eh_globals {
    __cxa_exception *caughtExceptions;
    unsigned int     uncaughtExceptions;
} __cxa_eh_globals;

__cxa_eh_globals*__cxa_get_globals(void) throw();

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_EXCEPT_GCC_DOT_H)
