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


// The COI interface on the target

#include "coi_server.h"

#include "../offload_target.h"
#include "../offload_timer.h"
#ifdef MYO_SUPPORT
#include "../offload_myo_target.h"      // for __offload_myoLibInit/Fini
#endif // MYO_SUPPORT

#if !defined(CPU_COUNT)
// if CPU_COUNT is not defined count number of CPUs manually 
static
int my_cpu_count(cpu_set_t const *cpu_set) 
{
    int res = 0;
    for (int i = 0; i < sizeof(cpu_set_t) / sizeof(__cpu_mask); ++i) {
        res += __builtin_popcountl(cpu_set->__bits[i]);
    }
    return res;
}
// Map CPU_COUNT to our function
#define CPU_COUNT(x) my_cpu_count(x)

#endif

COINATIVELIBEXPORT
void server_compute(
    uint32_t  buffer_count,
    void**    buffers,
    uint64_t* buffers_len,
    void*     misc_data,
    uint16_t  misc_data_len,
    void*     return_data,
    uint16_t  return_data_len
)
{
    OffloadDescriptor::offload(buffer_count, buffers,
                               misc_data, misc_data_len,
                               return_data, return_data_len);
}

COINATIVELIBEXPORT
void server_init(
    uint32_t  buffer_count,
    void**    buffers,
    uint64_t* buffers_len,
    void*     misc_data,
    uint16_t  misc_data_len,
    void*     return_data,
    uint16_t  return_data_len
)
{
    struct init_data {
        int  device_index;
        int  devices_total;
        int  console_level;
        int  offload_report_level;
    } *data = (struct init_data*) misc_data;

    // set device index and number of total devices
    mic_index = data->device_index;
    mic_engines_total = data->devices_total;

    // initialize trace level
    console_enabled = data->console_level;
    offload_report_level = data->offload_report_level;

    // return back the process id
    *((pid_t*) return_data) = getpid();
}

COINATIVELIBEXPORT
void server_var_table_size(
    uint32_t  buffer_count,
    void**    buffers,
    uint64_t* buffers_len,
    void*     misc_data,
    uint16_t  misc_data_len,
    void*     return_data,
    uint16_t  return_data_len
)
{
    struct Params {
        int64_t nelems;
        int64_t length;
    } *params;

    params = static_cast<Params*>(return_data);
    params->length = __offload_vars.table_size(params->nelems);
}

COINATIVELIBEXPORT
void server_var_table_copy(
    uint32_t  buffer_count,
    void**    buffers,
    uint64_t* buffers_len,
    void*     misc_data,
    uint16_t  misc_data_len,
    void*     return_data,
    uint16_t  return_data_len
)
{
    __offload_vars.table_copy(buffers[0], *static_cast<int64_t*>(misc_data));
}

COINATIVELIBEXPORT
void server_set_stream_affinity(
    uint32_t  buffer_count,
    void**    buffers,
    uint64_t* buffers_len,
    void*     misc_data,
    uint16_t  misc_data_len,
    void*     return_data,
    uint16_t  return_data_len
)
{
  /* kmp affinity is not supported by GCC.  */
}

#ifdef MYO_SUPPORT
// temporary workaround for blocking behavior of myoiLibInit/Fini calls
COINATIVELIBEXPORT
void server_myoinit(
    uint32_t  buffer_count,
    void**    buffers,
    uint64_t* buffers_len,
    void*     misc_data,
    uint16_t  misc_data_len,
    void*     return_data,
    uint16_t  return_data_len
)
{
    __offload_myoLibInit();
}

COINATIVELIBEXPORT
void server_myofini(
    uint32_t  buffer_count,
    void**    buffers,
    uint64_t* buffers_len,
    void*     misc_data,
    uint16_t  misc_data_len,
    void*     return_data,
    uint16_t  return_data_len
)
{
    __offload_myoLibFini();
}
#endif // MYO_SUPPORT
