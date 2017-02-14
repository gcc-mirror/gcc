/*
    Copyright (c) 2014-2016 Intel Corporation.  All Rights Reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of Intel Corporation nor the names of its
        contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/*! \file
    \brief The interface between compiler-generated host code and runtime library
*/

#ifndef COMPILER_IF_HOST_H_INCLUDED
#define COMPILER_IF_HOST_H_INCLUDED

#include "offload_host.h"

#define OFFLOAD_TARGET_ACQUIRE          OFFLOAD_PREFIX(target_acquire)
#define OFFLOAD_TARGET_ACQUIRE1         OFFLOAD_PREFIX(target_acquire1)
#define OFFLOAD_TARGET_ACQUIRE2         OFFLOAD_PREFIX(target_acquire2)
#define OFFLOAD_OFFLOAD                 OFFLOAD_PREFIX(offload)
#define OFFLOAD_OFFLOAD1                OFFLOAD_PREFIX(offload1)
#define OFFLOAD_OFFLOAD2                OFFLOAD_PREFIX(offload2)
#define OFFLOAD_OFFLOAD3                OFFLOAD_PREFIX(offload3)
#define OFFLOAD_CALL_COUNT              OFFLOAD_PREFIX(offload_call_count)


/*! \fn OFFLOAD_TARGET_ACQUIRE
    \brief Attempt to acquire the target.
    \param target_type   The type of target.
    \param target_number The device number.
    \param is_optional   Whether CPU fall-back is allowed.
    \param status        Address of variable to hold offload status.
    \param file          Filename in which this offload occurred.
    \param line          Line number in the file where this offload occurred.
*/
extern "C" OFFLOAD OFFLOAD_TARGET_ACQUIRE(
    TARGET_TYPE      target_type,
    int              target_number,
    int              is_optional,
    _Offload_status* status,
    const char*      file,
    uint64_t         line
);

/*! \fn OFFLOAD_TARGET_ACQUIRE1
    \brief Acquire the target for offload (OpenMP).
    \param device_number Device number or null if not specified.
    \param file          Filename in which this offload occurred
    \param line          Line number in the file where this offload occurred.
*/
extern "C" OFFLOAD OFFLOAD_TARGET_ACQUIRE1(
    const int*      device_number,
    const char*     file,
    uint64_t        line
);

/*! \fn OFFLOAD_TARGET_ACQUIRE2
    \brief Attempt to acquire the target.
    \param target_type   The type of target.
    \param target_number The device number.
    \param is_optional   Whether CPU fall-back is allowed.
    \param status        Address of variable to hold offload status.
    \param file          Filename in which this offload occurred.
    \param line          Line number in the file where this offload occurred.
    \param stream        Pointer to stream value.
*/
extern "C" OFFLOAD OFFLOAD_TARGET_ACQUIRE2(
    TARGET_TYPE      target_type,
    int              target_number,
    int              is_optional,
    _Offload_status* status,
    const char*      file,
    uint64_t         line,
    const void**     stream
);

/*! \fn OFFLOAD_OFFLOAD1
    \brief Run function on target using interface for old data persistence.
    \param o Offload descriptor created by OFFLOAD_TARGET_ACQUIRE.
    \param name Name of offload entry point.
    \param is_empty If no code to execute (e.g. offload_transfer)
    \param num_vars Number of variable descriptors.
    \param vars Pointer to VarDesc array.
    \param vars2 Pointer to VarDesc2 array.
    \param num_waits Number of "wait" values.
    \param waits Pointer to array of wait values.
    \param signal Pointer to signal value or NULL.
*/
extern "C" int OFFLOAD_OFFLOAD1(
    OFFLOAD o,
    const char *name,
    int is_empty,
    int num_vars,
    VarDesc *vars,
    VarDesc2 *vars2,
    int num_waits,
    const void** waits,
    const void** signal
);

/*! \fn OFFLOAD_OFFLOAD2
    \brief Run function on target using interface for new data persistence.
    \param o Offload descriptor created by OFFLOAD_TARGET_ACQUIRE.
    \param name Name of offload entry point.
    \param is_empty If no code to execute (e.g. offload_transfer)
    \param num_vars Number of variable descriptors.
    \param vars Pointer to VarDesc array.
    \param vars2 Pointer to VarDesc2 array.
    \param num_waits Number of "wait" values.
    \param waits Pointer to array of wait values.
    \param signal Pointer to signal value or NULL.
    \param entry_id A signature for the function doing the offload.
    \param stack_addr The stack frame address of the function doing offload.
*/
extern "C" int OFFLOAD_OFFLOAD2(
    OFFLOAD o,
    const char *name,
    int is_empty,
    int num_vars,
    VarDesc *vars,
    VarDesc2 *vars2,
    int num_waits,
    const void** waits,
    const void** signal,
    int entry_id,
    const void *stack_addr
);


/*! \fn OFFLOAD_OFFLOAD3
    \brief Run function on target, API introduced in 15.0 Update 1
    \brief when targetptr, preallocated feature was introduced.
    \param o Offload descriptor created by OFFLOAD_TARGET_ACQUIRE.
    \param name Name of offload entry point.
    \param is_empty If no code to execute (e.g. offload_transfer)
    \param num_vars Number of variable descriptors.
    \param vars Pointer to VarDesc array.
    \param vars2 Pointer to VarDesc2 array.
    \param num_waits Number of "wait" values.
    \param waits Pointer to array of wait values.
    \param signal Pointer to signal value or NULL.
    \param entry_id A signature for the function doing the offload.
    \param stack_addr The stack frame address of the function doing offload.
    \param offload_flags Flags to indicate Fortran traceback, OpenMP async.
    \param stream Pointer to stream value or NULL.
*/
extern "C" int OFFLOAD_OFFLOAD3(
    OFFLOAD ofld,
    const char *name,
    int is_empty,
    int num_vars,
    VarDesc *vars,
    VarDesc2 *vars2,
    int num_waits,
    const void** waits,
    const void** signal,
    int entry_id,
    const void *stack_addr,
    OffloadFlags offload_flags,
    const void** stream
);

// Run function on target (obsolete).
// @param o    OFFLOAD object
// @param name function name
extern "C" int OFFLOAD_OFFLOAD(
    OFFLOAD o,
    const char *name,
    int is_empty,
    int num_vars,
    VarDesc *vars,
    VarDesc2 *vars2,
    int num_waits,
    const void** waits,
    const void* signal,
    int entry_id = 0,
    const void *stack_addr = NULL
);

// Global counter on host.
// This variable is used if P2OPT_offload_do_data_persistence == 2.
// The variable used to identify offload constructs contained in one procedure.
// Call to OFFLOAD_CALL_COUNT() is inserted at HOST on entry of the routine.
extern "C" int  OFFLOAD_CALL_COUNT();

#endif // COMPILER_IF_HOST_H_INCLUDED
