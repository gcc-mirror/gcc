/* cilk-ittnotify.h                  -*-C++-*-
 *
 *************************************************************************
 *
 *  Copyright (C) 2009-2016, Intel Corporation
 *  All rights reserved.
 *  
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
 *  
 *  *********************************************************************
 *  
 *  PLEASE NOTE: This file is a downstream copy of a file mainitained in
 *  a repository at cilkplus.org. Changes made to this file that are not
 *  submitted through the contribution process detailed at
 *  http://www.cilkplus.org/submit-cilk-contribution will be lost the next
 *  time that a new version is released. Changes only submitted to the
 *  GNU compiler collection or posted to the git repository at
 *  https://bitbucket.org/intelcilkruntime/intel-cilk-runtime.git are
 *  not tracked.
 *  
 *  We welcome your contributions to this open source project. Thank you
 *  for your assistance in helping us improve Cilk Plus.
 **************************************************************************/

#ifndef INCLUDED_CILK_ITTNOTIFY_DOT_H
#define INCLUDED_CILK_ITTNOTIFY_DOT_H

#ifdef __INTEL_COMPILER
#endif
#include <stdio.h>

// ITTNOTIFY does not support ARM at this time
#ifdef __arm__
#undef USE_ITTNOTIFY
#endif

#ifdef USE_ITTNOTIFY
#include <ittnotify.h>

#ifdef _WIN32
# define ITT_SYNC_CREATE(_address, _description)        \
    __itt_sync_createA(_address,                        \
                       "Intel Cilk Plus " _description, \
                       "",                              \
                       __itt_attr_barrier)
#else
# define ITT_SYNC_CREATE(_address, _description)        \
    __itt_sync_create(_address,                         \
                      "Intel Cilk Plus " _description,  \
                      "",                               \
                      __itt_attr_barrier)
#endif

#define ITT_SYNC_PREPARE(_address) __itt_sync_prepare(_address)
#define ITT_SYNC_ACQUIRED(_address) __itt_sync_acquired(_address)
#define ITT_SYNC_RELEASING(_address) __itt_sync_releasing(_address)
#define ITT_SYNC_DESTROY(_address) __itt_sync_destroy(_address)
// Note that we subtract 5 from the return address to find the CALL instruction
// to __cilkrts_sync
#if 1   // Disable renaming for now.  Piersol isn't ready yet
#define ITT_SYNC_SET_NAME_AND_PREPARE(_address, _sync_ret_address) __itt_sync_prepare(_address)
#else
#define ITT_SYNC_SET_NAME_AND_PREPARE(_address, _sync_ret_address) \
    if (NULL != __itt_sync_prepare_ptr) {   \
        if (0 == _sync_ret_address) \
            __itt_sync_renameA(_address, "");  \
        else    \
        {   \
            char buf[128];  \
            sprintf_s(buf, 128, "IP:0x%p", (DWORD_PTR)_sync_ret_address - 5); \
            __itt_sync_renameA(_address, buf); \
            _sync_ret_address = 0;  \
         }  \
        __itt_sync_prepare(_address);  \
    }
#endif
#else   // USE_ITTNOTIFY not defined, compile out all calls
#define ITT_SYNC_CREATE(_address, _description)
#define ITT_SYNC_PREPARE(_address)
#define ITT_SYNC_ACQUIRED(_address)
#define ITT_SYNC_RELEASING(_addresss)
#define ITT_SYNC_DESTROY(_address)
#define ITT_SYNC_SET_NAME_AND_PREPARE(_sync_address, _wait_address)
#endif

#endif // ! defined(INCLUDED_CILK_ITTNOTIFY_DOT_H)
