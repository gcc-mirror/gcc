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


#include "offload_timer.h"
#include "offload_target.h"

#ifdef __INTEL_COMPILER
#include <ia32intrin.h>
#else // __INTEL_COMPILER
#include <x86intrin.h>
#endif // __INTEL_COMPILER



int timer_enabled = 0;

#ifdef TIMING_SUPPORT

#if defined(LINUX) || defined(FREEBSD)
static __thread OffloadTargetTimerData timer_data;
#else // WINNT
static __declspec(thread) OffloadTargetTimerData timer_data;
#endif // defined(LINUX) || defined(FREEBSD)


void offload_timer_start(
    OffloadTargetPhase p_type
)
{
    timer_data.phases[p_type].start = _rdtsc();
}

void offload_timer_stop(
    OffloadTargetPhase p_type
)
{
    timer_data.phases[p_type].total += _rdtsc() -
                                       timer_data.phases[p_type].start;
}

void offload_timer_init()
{
    memset(&timer_data, 0, sizeof(OffloadTargetTimerData));
}

void offload_timer_fill_target_data(
    void *buf
)
{
    uint64_t *data = (uint64_t*) buf;

    timer_data.frequency = mic_frequency;
    memcpy(data++, &(timer_data.frequency), sizeof(uint64_t));

    for (int i = 0; i < c_offload_target_max_phase; i++) {
        memcpy(data++, &(timer_data.phases[i].total), sizeof(uint64_t));
    }
}

#endif // TIMING_SUPPORT
