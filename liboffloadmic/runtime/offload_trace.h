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


// The parts of the offload library common to host and target
#include "offload_util.h"

DLL_LOCAL void offload_stage_print(int stage, int offload_number, ...);

enum OffloadTraceStage {
    // Total time spent on the target
    c_offload_start = 0,
    c_offload_init,
    c_offload_register,
    c_offload_init_func,
    c_offload_create_buf_host,
    c_offload_create_buf_mic,
    c_offload_send_pointer_data,
    c_offload_sent_pointer_data,
    c_offload_gather_copyin_data,
    c_offload_copyin_data,
    c_offload_compute,
    c_offload_receive_pointer_data,
    c_offload_received_pointer_data,
    c_offload_start_target_func,
    c_offload_var,
    c_offload_scatter_copyin_data,
    c_offload_gather_copyout_data,
    c_offload_scatter_copyout_data,
    c_offload_copyout_data,
    c_offload_signal,
    c_offload_wait,
    c_offload_unregister,
    c_offload_destroy,
    c_offload_finish,
    c_offload_myoinit,
    c_offload_myoregister,
    c_offload_mic_myo_shared,
    c_offload_mic_myo_fptr,
    c_offload_myosharedmalloc,
    c_offload_myosharedfree,
    c_offload_myosharedalignedmalloc,
    c_offload_myosharedalignedfree,
    c_offload_myoacquire,
    c_offload_myorelease,
    c_offload_myofini,
    c_offload_myosupportsfeature,
    c_offload_myosharedarenacreate,
    c_offload_myosharedalignedarenamalloc,
    c_offload_myosharedalignedarenafree,
    c_offload_myoarenaacquire,
    c_offload_myoarenarelease,
    c_offload_stream
};

enum OffloadWaitKind {
    c_offload_wait_signal = 0,
    c_offload_wait_stream,
    c_offload_wait_all_streams
};
