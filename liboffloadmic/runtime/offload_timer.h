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


#ifndef OFFLOAD_TIMER_H_INCLUDED
#define OFFLOAD_TIMER_H_INCLUDED

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include "liboffload_error_codes.h"

DLL_LOCAL extern int timer_enabled;

#ifdef TIMING_SUPPORT

struct OffloadTargetTimerData {
    uint64_t frequency;
    struct {
        uint64_t start;
        uint64_t total;
    } phases[c_offload_target_max_phase];
};

struct OffloadHostTimerData {
    // source file name and line number
    const char* file;
    int         line;

    // host timer data
    struct {
        uint64_t start;
        uint64_t total;
    } phases[c_offload_host_max_phase];

    uint64_t sent_bytes;
    uint64_t received_bytes;
    int card_number;
    int offload_number;

    // target timer data
    OffloadTargetTimerData target;

    // next element
    OffloadHostTimerData *next;
};

#if HOST_LIBRARY

DLL_LOCAL extern int offload_report_level;
DLL_LOCAL extern int offload_report_enabled;
#define OFFLOAD_REPORT_1 1
#define OFFLOAD_REPORT_2 2
#define OFFLOAD_REPORT_3 3
#define OFFLOAD_REPORT_ON 1
#define OFFLOAD_REPORT_OFF 0

#define OFFLOAD_TIMER_DATALEN() \
    ((timer_enabled || (offload_report_level && offload_report_enabled)) ? \
     ((1 + c_offload_target_max_phase) * sizeof(uint64_t)) : 0)

#define OFFLOAD_TIMER_START(timer_data, pnode) \
    if (timer_enabled || \
        (offload_report_level && offload_report_enabled)) { \
        offload_timer_start(timer_data, pnode); \
    }

#define OFFLOAD_TIMER_STOP(timer_data, pnode) \
    if (timer_enabled || \
        (offload_report_level && offload_report_enabled)) { \
        offload_timer_stop(timer_data, pnode); \
    }

#define OFFLOAD_TIMER_INIT(file, line) \
    offload_timer_init(file, line);

#define OFFLOAD_TIMER_TARGET_DATA(timer_data, data) \
    if (timer_enabled || \
        (offload_report_level && offload_report_enabled)) { \
        offload_timer_fill_target_data(timer_data, data); \
    }

#define OFFLOAD_TIMER_HOST_SDATA(timer_data, data) \
    if (offload_report_level && offload_report_enabled) { \
        offload_timer_fill_host_sdata(timer_data, data); \
    }

#define OFFLOAD_TIMER_HOST_RDATA(timer_data, data) \
    if (offload_report_level && offload_report_enabled) { \
        offload_timer_fill_host_rdata(timer_data, data); \
    }

#define OFFLOAD_TIMER_HOST_MIC_NUM(timer_data, data) \
    if (offload_report_level && offload_report_enabled) { \
        offload_timer_fill_host_mic_num(timer_data, data); \
    }

extern DLL_LOCAL void offload_timer_start(OffloadHostTimerData *,
                                OffloadHostPhase t_node);
extern DLL_LOCAL void offload_timer_stop(OffloadHostTimerData *,
                               OffloadHostPhase t_node);
extern DLL_LOCAL OffloadHostTimerData * offload_timer_init(const char *file, int line);
extern DLL_LOCAL void offload_timer_fill_target_data(OffloadHostTimerData *,
                                           void *data);
extern DLL_LOCAL void offload_timer_fill_host_sdata(OffloadHostTimerData *,
                                          uint64_t sent_bytes);
extern DLL_LOCAL void offload_timer_fill_host_rdata(OffloadHostTimerData *,
                                          uint64_t sent_bytes);
extern DLL_LOCAL void offload_timer_fill_host_mic_num(OffloadHostTimerData *,
                                            int card_number);

// Utility structure for starting/stopping timer
struct OffloadTimer {
    OffloadTimer(OffloadHostTimerData *data, OffloadHostPhase phase) :
        m_data(data),
        m_phase(phase)
    {
        OFFLOAD_TIMER_START(m_data, m_phase);
    }

    ~OffloadTimer()
    {
        OFFLOAD_TIMER_STOP(m_data, m_phase);
    }

private:
    OffloadHostTimerData*   m_data;
    OffloadHostPhase        m_phase;
};

#else

#define OFFLOAD_TIMER_DATALEN() \
    ((timer_enabled) ? \
     ((1 + c_offload_target_max_phase) * sizeof(uint64_t)) : 0)

#define OFFLOAD_TIMER_START(pnode) \
    if (timer_enabled) offload_timer_start(pnode);

#define OFFLOAD_TIMER_STOP(pnode) \
    if (timer_enabled) offload_timer_stop(pnode);

#define OFFLOAD_TIMER_INIT() \
    if (timer_enabled) offload_timer_init();

#define OFFLOAD_TIMER_TARGET_DATA(data) \
    if (timer_enabled) offload_timer_fill_target_data(data);

extern DLL_LOCAL void offload_timer_start(OffloadTargetPhase t_node);
extern DLL_LOCAL void offload_timer_stop(OffloadTargetPhase t_node);
extern DLL_LOCAL void offload_timer_init(void);
extern DLL_LOCAL void offload_timer_fill_target_data(void *data);

#endif // HOST_LIBRARY

#else // TIMING_SUPPORT

#define OFFLOAD_TIMER_START(...)
#define OFFLOAD_TIMER_STOP(...)
#define OFFLOAD_TIMER_INIT(...)
#define OFFLOAD_TIMER_TARGET_DATA(...)
#define OFFLOAD_TIMER_DATALEN(...)      (0)

#endif // TIMING_SUPPORT

#endif // OFFLOAD_TIMER_H_INCLUDED
