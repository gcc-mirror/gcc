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


#include "offload_orsl.h"
#include <stdlib.h>
#include "offload_host.h"
#include "orsl-lite/include/orsl-lite.h"

namespace ORSL {

static bool            is_enabled = false;
static const ORSLTag   my_tag = (const ORSLTag) "Offload";

void init()
{
    const char *env_var = getenv("OFFLOAD_ENABLE_ORSL");
    if (env_var != 0 && *env_var != '\0') {
        int64_t new_val;
        if (__offload_parse_int_string(env_var, new_val)) {
            is_enabled = new_val;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_int_value,
                             "OFFLOAD_ENABLE_ORSL");
        }
    }

    if (is_enabled) {
        OFFLOAD_DEBUG_TRACE(2, "ORSL is enabled\n");
    }
    else {
        OFFLOAD_DEBUG_TRACE(2, "ORSL is disabled\n");
    }
}

bool reserve(int device)
{
    if (is_enabled) {
        int pnum = mic_engines[device].get_physical_index();
        ORSLBusySet bset;

        bset.type = BUSY_SET_FULL;
        if (ORSLReserve(1, &pnum, &bset, my_tag) != 0) {
            return false;
        }
    }
    return true;
}

bool try_reserve(int device)
{
    if (is_enabled) {
        int pnum = mic_engines[device].get_physical_index();
        ORSLBusySet bset;

        bset.type = BUSY_SET_FULL;
        if (ORSLTryReserve(1, &pnum, &bset, my_tag) != 0) {
            return false;
        }
    }
    return true;
}

void release(int device)
{
    if (is_enabled) {
        int pnum = mic_engines[device].get_physical_index();
        ORSLBusySet bset;

        bset.type = BUSY_SET_FULL;
        if (ORSLRelease(1, &pnum, &bset, my_tag) != 0) {
            // should never get here
        }
    }
}

} // namespace ORSL
