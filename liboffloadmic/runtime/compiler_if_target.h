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
    \brief The interface between compiler-generated target code and runtime library
*/

#ifndef COMPILER_IF_TARGET_H_INCLUDED
#define COMPILER_IF_TARGET_H_INCLUDED

#include "offload_target.h"

#define OFFLOAD_TARGET_ENTER            OFFLOAD_PREFIX(target_enter)
#define OFFLOAD_TARGET_LEAVE            OFFLOAD_PREFIX(target_leave)
#define OFFLOAD_TARGET_MAIN             OFFLOAD_PREFIX(target_main)

/*! \fn OFFLOAD_TARGET_ENTER
    \brief Fill in variable addresses using VarDesc array.
    \brief Then call back the runtime library to fetch data.
    \param ofld         Offload descriptor created by runtime.
    \param var_desc_num Number of variable descriptors.
    \param var_desc     Pointer to VarDesc array.
    \param var_desc2    Pointer to VarDesc2 array.
*/
extern "C" void OFFLOAD_TARGET_ENTER(
    OFFLOAD ofld,
    int var_desc_num,
    VarDesc *var_desc,
    VarDesc2 *var_desc2
);

/*! \fn OFFLOAD_TARGET_LEAVE
    \brief Call back the runtime library to gather outputs using VarDesc array.
    \param ofld Offload descriptor created by OFFLOAD_TARGET_ACQUIRE.
*/
extern "C" void OFFLOAD_TARGET_LEAVE(
    OFFLOAD ofld
);

// Entry point for the target application.
extern "C" void OFFLOAD_TARGET_MAIN(void);

#endif // COMPILER_IF_TARGET_H_INCLUDED
