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


// The interface between offload library and the COI API on the target

#ifndef COI_SERVER_H_INCLUDED
#define COI_SERVER_H_INCLUDED

#include <common/COIEngine_common.h>
#include <common/COIPerf_common.h>
#include <sink/COIProcess_sink.h>
#include <sink/COIPipeline_sink.h>
#include <sink/COIBuffer_sink.h>
#include <list>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "../liboffload_error_codes.h"

// wrappers for COI API
#define PipelineStartExecutingRunFunctions() \
    { \
        COIRESULT res = COIPipelineStartExecutingRunFunctions(); \
        if (res != COI_SUCCESS) { \
            LIBOFFLOAD_ERROR(c_pipeline_start_run_funcs, mic_index, res); \
            exit(1); \
        } \
    }

#define ProcessWaitForShutdown() \
    { \
        COIRESULT res = COIProcessWaitForShutdown(); \
        if (res != COI_SUCCESS) { \
            LIBOFFLOAD_ERROR(c_process_wait_shutdown, mic_index, res); \
            exit(1); \
        } \
    }

#define BufferAddRef(buf) \
    { \
        COIRESULT res = COIBufferAddRef(buf); \
        if (res != COI_SUCCESS) { \
            LIBOFFLOAD_ERROR(c_buf_add_ref, mic_index, res); \
            exit(1); \
        } \
    }

#define BufferReleaseRef(buf) \
    { \
        COIRESULT res = COIBufferReleaseRef(buf); \
        if (res != COI_SUCCESS) { \
            LIBOFFLOAD_ERROR(c_buf_release_ref, mic_index, res); \
            exit(1); \
        } \
    }

#define EngineGetIndex(index) \
    { \
        COI_ISA_TYPE isa_type; \
        COIRESULT res = COIEngineGetIndex(&isa_type, index); \
        if (res != COI_SUCCESS) { \
            LIBOFFLOAD_ERROR(c_get_engine_index, mic_index, res); \
            exit(1); \
        } \
    }

#endif // COI_SERVER_H_INCLUDED
