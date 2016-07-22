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


// The interface between offload library and the COI API on the host

#ifndef COI_CLIENT_H_INCLUDED
#define COI_CLIENT_H_INCLUDED

#include <common/COIPerf_common.h>
#include <common/COIMacros_common.h>
#include <source/COIEngine_source.h>
#include <source/COIProcess_source.h>
#include <source/COIPipeline_source.h>
#include <source/COIBuffer_source.h>
#include <source/COIEvent_source.h>

#include <string.h>

#include "../liboffload_error_codes.h"
#include "../offload_util.h"

#define MIC_ENGINES_MAX     128

#if MIC_ENGINES_MAX < COI_MAX_ISA_MIC_DEVICES
#error MIC_ENGINES_MAX need to be increased
#endif

// COI library interface
namespace COI {

DLL_LOCAL extern bool init(void);
DLL_LOCAL extern void fini(void);

DLL_LOCAL extern bool is_available;

// pointers to functions from COI library
DLL_LOCAL extern COIRESULT (*EngineGetCount)(COI_ISA_TYPE, uint32_t*);
DLL_LOCAL extern COIRESULT (*EngineGetHandle)(COI_ISA_TYPE, uint32_t, COIENGINE*);

DLL_LOCAL extern COIRESULT (*ProcessCreateFromMemory)(COIENGINE, const char*,
                                           const void*, uint64_t, int,
                                           const char**, uint8_t,
                                           const char**, uint8_t,
                                           const char*, uint64_t,
                                           const char*,
                                           const char*, uint64_t,
                                           COIPROCESS*);
DLL_LOCAL extern COIRESULT (*ProcessCreateFromFile)(COIENGINE, const char*, int,
                                          const char**, uint8_t,
                                          const char**,
                                          uint8_t,
                                          const char*,
                                          uint64_t,
                                          const char*,
                                          COIPROCESS*);
DLL_LOCAL extern COIRESULT (*ProcessSetCacheSize)(COIPROCESS, uint64_t, uint32_t,
                                                uint64_t, uint32_t, uint32_t,
                                                const COIEVENT*, COIEVENT*);
DLL_LOCAL extern COIRESULT (*ProcessDestroy)(COIPROCESS, int32_t, uint8_t,
                                  int8_t*, uint32_t*);
DLL_LOCAL extern COIRESULT (*ProcessGetFunctionHandles)(COIPROCESS, uint32_t,
                                             const char**,
                                             COIFUNCTION*);
DLL_LOCAL extern COIRESULT (*ProcessLoadLibraryFromMemory)(COIPROCESS,
                                                const void*,
                                                uint64_t,
                                                const char*,
                                                const char*,
                                                const char*,
                                                uint64_t,
                                                uint32_t,
                                                COILIBRARY*);

DLL_LOCAL extern COIRESULT (*ProcessUnloadLibrary)(COIPROCESS,
                                                COILIBRARY);

DLL_LOCAL extern COIRESULT (*ProcessRegisterLibraries)(uint32_t,
                                            const void**,
                                            const uint64_t*,
                                            const char**,
                                            const uint64_t*);

DLL_LOCAL extern COIRESULT (*PipelineCreate)(COIPROCESS, COI_CPU_MASK, uint32_t,
                                  COIPIPELINE*);
DLL_LOCAL extern COIRESULT (*PipelineDestroy)(COIPIPELINE);
DLL_LOCAL extern COIRESULT (*PipelineRunFunction)(COIPIPELINE, COIFUNCTION,
                                       uint32_t, const COIBUFFER*,
                                       const COI_ACCESS_FLAGS*,
                                       uint32_t, const COIEVENT*,
                                       const void*, uint16_t, void*,
                                       uint16_t, COIEVENT*);

DLL_LOCAL extern COIRESULT (*BufferCreate)(uint64_t, COI_BUFFER_TYPE, uint32_t,
                                const void*, uint32_t,
                                const COIPROCESS*, COIBUFFER*);
DLL_LOCAL extern COIRESULT (*BufferCreateFromMemory)(uint64_t, COI_BUFFER_TYPE,
                                          uint32_t, void*,
                                          uint32_t, const COIPROCESS*,
                                          COIBUFFER*);
DLL_LOCAL extern COIRESULT (*BufferDestroy)(COIBUFFER);
DLL_LOCAL extern COIRESULT (*BufferMap)(COIBUFFER, uint64_t, uint64_t,
                             COI_MAP_TYPE, uint32_t, const COIEVENT*,
                             COIEVENT*, COIMAPINSTANCE*, void**);
DLL_LOCAL extern COIRESULT (*BufferUnmap)(COIMAPINSTANCE, uint32_t,
                               const COIEVENT*, COIEVENT*);
DLL_LOCAL extern COIRESULT (*BufferWrite)(COIBUFFER, uint64_t, const void*,
                               uint64_t, COI_COPY_TYPE, uint32_t,
                               const COIEVENT*, COIEVENT*);
DLL_LOCAL extern COIRESULT (*BufferRead)(COIBUFFER, uint64_t, void*, uint64_t,
                              COI_COPY_TYPE, uint32_t,
                              const COIEVENT*, COIEVENT*);
DLL_LOCAL extern COIRESULT (*BufferReadMultiD)(COIBUFFER, uint64_t,
                            void *, void *, COI_COPY_TYPE,
                            uint32_t, const   COIEVENT*, COIEVENT*);
DLL_LOCAL extern COIRESULT (*BufferWriteMultiD)(COIBUFFER, const   COIPROCESS,
                            uint64_t, void *, void *,
                            COI_COPY_TYPE, uint32_t, const   COIEVENT*, COIEVENT*);

DLL_LOCAL extern COIRESULT (*BufferCopy)(COIBUFFER, COIBUFFER, uint64_t, uint64_t,
                              uint64_t, COI_COPY_TYPE, uint32_t,
                              const COIEVENT*, COIEVENT*);
DLL_LOCAL extern COIRESULT (*BufferGetSinkAddress)(COIBUFFER, uint64_t*);
DLL_LOCAL extern COIRESULT (*BufferSetState)(COIBUFFER, COIPROCESS, COI_BUFFER_STATE,
                                   COI_BUFFER_MOVE_FLAG, uint32_t,
                                   const   COIEVENT*, COIEVENT*);

DLL_LOCAL extern COIRESULT (*EventWait)(uint16_t, const COIEVENT*, int32_t,
                           uint8_t, uint32_t*, uint32_t*);

DLL_LOCAL extern uint64_t (*PerfGetCycleFrequency)(void);

DLL_LOCAL extern COIRESULT (*ProcessConfigureDMA)(const uint64_t, const int);

extern COIRESULT (*PipelineClearCPUMask)(COI_CPU_MASK);

extern COIRESULT (*PipelineSetCPUMask)(COIPROCESS, uint32_t,
                                        uint8_t, COI_CPU_MASK);
extern COIRESULT (*EngineGetInfo)(COIENGINE, uint32_t, COI_ENGINE_INFO*);

extern COIRESULT (*EventRegisterCallback)(
    const COIEVENT,
    void (*)(COIEVENT, const COIRESULT, const void*),
    const void*,
    const uint64_t);
    
const int DMA_MODE_READ_WRITE = 1; 
} // namespace COI

#endif // COI_CLIENT_H_INCLUDED
