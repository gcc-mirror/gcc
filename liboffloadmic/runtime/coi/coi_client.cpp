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


// The COI host interface

#include "coi_client.h"
#include "../offload_common.h"

namespace COI {

#define COI_VERSION1    "COI_1.0"
#define COI_VERSION2    "COI_2.0"

bool            is_available;
static void*    lib_handle;

// pointers to functions from COI library
COIRESULT (*EngineGetCount)(COI_ISA_TYPE, uint32_t*);
COIRESULT (*EngineGetHandle)(COI_ISA_TYPE, uint32_t, COIENGINE*);

COIRESULT (*ProcessCreateFromMemory)(COIENGINE, const char*, const void*,
                                     uint64_t, int, const char**, uint8_t,
                                     const char**, uint8_t, const char*,
                                     uint64_t, const char*, const char*,
                                     uint64_t, COIPROCESS*);
COIRESULT (*ProcessCreateFromFile)(COIENGINE, const char*,
                                     int, const char**, uint8_t,
                                     const char**, uint8_t, const char*,
                                     uint64_t, const char*,COIPROCESS*);
COIRESULT (*ProcessSetCacheSize)(COIPROCESS, uint64_t, uint32_t,
                                 uint64_t, uint32_t, uint32_t,
                                 const COIEVENT*, COIEVENT*);
COIRESULT (*ProcessDestroy)(COIPROCESS, int32_t, uint8_t, int8_t*, uint32_t*);
COIRESULT (*ProcessGetFunctionHandles)(COIPROCESS, uint32_t, const char**,
                                       COIFUNCTION*);
COIRESULT (*ProcessLoadLibraryFromMemory)(COIPROCESS, const void*, uint64_t,
                                          const char*, const char*,
                                          const char*, uint64_t, uint32_t,
                                          COILIBRARY*);
COIRESULT (*ProcessUnloadLibrary)(COIPROCESS,
                                  COILIBRARY);
COIRESULT (*ProcessRegisterLibraries)(uint32_t, const void**, const uint64_t*,
                                      const char**, const uint64_t*);

COIRESULT (*PipelineCreate)(COIPROCESS, COI_CPU_MASK, uint32_t, COIPIPELINE*);
COIRESULT (*PipelineDestroy)(COIPIPELINE);
COIRESULT (*PipelineRunFunction)(COIPIPELINE, COIFUNCTION, uint32_t,
                                 const COIBUFFER*, const COI_ACCESS_FLAGS*,
                                 uint32_t, const COIEVENT*, const void*,
                                 uint16_t, void*, uint16_t, COIEVENT*);

COIRESULT (*BufferCreate)(uint64_t, COI_BUFFER_TYPE, uint32_t, const void*,
                          uint32_t, const COIPROCESS*, COIBUFFER*);
COIRESULT (*BufferCreateFromMemory)(uint64_t, COI_BUFFER_TYPE, uint32_t,
                                    void*, uint32_t, const COIPROCESS*,
                                    COIBUFFER*);
COIRESULT (*BufferDestroy)(COIBUFFER);
COIRESULT (*BufferMap)(COIBUFFER, uint64_t, uint64_t, COI_MAP_TYPE, uint32_t,
                       const COIEVENT*, COIEVENT*, COIMAPINSTANCE*, void**);
COIRESULT (*BufferUnmap)(COIMAPINSTANCE, uint32_t, const COIEVENT*, COIEVENT*);
COIRESULT (*BufferWrite)(COIBUFFER, uint64_t, const void*, uint64_t,
                         COI_COPY_TYPE, uint32_t, const COIEVENT*, COIEVENT*);
COIRESULT (*BufferRead)(COIBUFFER, uint64_t, void*, uint64_t, COI_COPY_TYPE,
                        uint32_t, const COIEVENT*, COIEVENT*);
COIRESULT (*BufferReadMultiD)(COIBUFFER, uint64_t,
                        void *, void *, COI_COPY_TYPE,
                        uint32_t, const   COIEVENT*, COIEVENT*);
COIRESULT (*BufferWriteMultiD)(COIBUFFER, const   COIPROCESS,
                       uint64_t, void *, void *,
                       COI_COPY_TYPE, uint32_t, const   COIEVENT*, COIEVENT*);

COIRESULT (*BufferCopy)(COIBUFFER, COIBUFFER, uint64_t, uint64_t, uint64_t,
                        COI_COPY_TYPE, uint32_t, const COIEVENT*, COIEVENT*);
COIRESULT (*BufferGetSinkAddress)(COIBUFFER, uint64_t*);
COIRESULT (*BufferSetState)(COIBUFFER, COIPROCESS, COI_BUFFER_STATE,
                            COI_BUFFER_MOVE_FLAG, uint32_t,
                            const   COIEVENT*, COIEVENT*);

COIRESULT (*EventWait)(uint16_t, const COIEVENT*, int32_t, uint8_t, uint32_t*,
                       uint32_t*);

uint64_t  (*PerfGetCycleFrequency)(void);

COIRESULT (*PipelineClearCPUMask) (COI_CPU_MASK);

COIRESULT (*PipelineSetCPUMask) (COIPROCESS, uint32_t,
                                        uint8_t, COI_CPU_MASK);
COIRESULT (*EngineGetInfo)(COIENGINE, uint32_t, COI_ENGINE_INFO*);

COIRESULT (*EventRegisterCallback)(
    const COIEVENT,
    void (*)(COIEVENT, const COIRESULT, const void*),
    const void*,
    const uint64_t);

COIRESULT (*ProcessConfigureDMA)(const uint64_t, const int);

bool init(void)
{
#ifndef TARGET_WINNT
    const char *lib_name = "libcoi_host.so.0";
#else // TARGET_WINNT
    const char *lib_name = "coi_host.dll";
#endif // TARGET_WINNT

    OFFLOAD_DEBUG_TRACE(2, "Loading COI library %s ...\n", lib_name);
    lib_handle = DL_open(lib_name);
    if (lib_handle == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to load the library\n");
        return false;
    }

    EngineGetCount =
        (COIRESULT (*)(COI_ISA_TYPE, uint32_t*))
            DL_sym(lib_handle, "COIEngineGetCount", COI_VERSION1);
    if (EngineGetCount == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIEngineGetCount");
        fini();
        return false;
    }

    EngineGetHandle =
        (COIRESULT (*)(COI_ISA_TYPE, uint32_t, COIENGINE*))
            DL_sym(lib_handle, "COIEngineGetHandle", COI_VERSION1);
    if (EngineGetHandle == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIEngineGetHandle");
        fini();
        return false;
    }

    ProcessCreateFromMemory =
        (COIRESULT (*)(COIENGINE, const char*, const void*, uint64_t, int,
                       const char**, uint8_t, const char**, uint8_t,
                       const char*, uint64_t, const char*, const char*,
                       uint64_t, COIPROCESS*))
            DL_sym(lib_handle, "COIProcessCreateFromMemory", COI_VERSION1);
    if (ProcessCreateFromMemory == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIProcessCreateFromMemory");
        fini();
        return false;
    }

    ProcessSetCacheSize =
           (COIRESULT (*)(COIPROCESS, uint64_t, uint32_t,
                                 uint64_t, uint32_t, uint32_t,
                                 const COIEVENT*, COIEVENT*))
               DL_sym(lib_handle, "COIProcessSetCacheSize", COI_VERSION1);
    if (ProcessSetCacheSize == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIProcessSetCacheSize");
#if 0  // for now disable as ProcessSetCacheSize is not available on < MPSS 3.4
        fini();
        return false;
#endif
    }

    ProcessCreateFromFile =
           (COIRESULT (*)(COIENGINE, const char*, int, const char**, uint8_t,
                          const char**, uint8_t, const char*, uint64_t,
                          const char*, COIPROCESS*))
            DL_sym(lib_handle, "COIProcessCreateFromFile", COI_VERSION1);
    if (ProcessCreateFromFile == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIProcessCreateFromFile");
        fini();
        return false;
    }

    ProcessDestroy =
        (COIRESULT (*)(COIPROCESS, int32_t, uint8_t, int8_t*,
                       uint32_t*))
            DL_sym(lib_handle, "COIProcessDestroy", COI_VERSION1);
    if (ProcessDestroy == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIProcessDestroy");
        fini();
        return false;
    }

    ProcessGetFunctionHandles =
        (COIRESULT (*)(COIPROCESS, uint32_t, const char**, COIFUNCTION*))
            DL_sym(lib_handle, "COIProcessGetFunctionHandles", COI_VERSION1);
    if (ProcessGetFunctionHandles == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIProcessGetFunctionHandles");
        fini();
        return false;
    }

    ProcessLoadLibraryFromMemory =
        (COIRESULT (*)(COIPROCESS, const void*, uint64_t, const char*,
                       const char*, const char*, uint64_t, uint32_t,
                       COILIBRARY*))
            DL_sym(lib_handle, "COIProcessLoadLibraryFromMemory", COI_VERSION2);
    if (ProcessLoadLibraryFromMemory == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIProcessLoadLibraryFromMemory");
        fini();
        return false;
    }

    ProcessUnloadLibrary =
        (COIRESULT (*)(COIPROCESS,
                       COILIBRARY))
            DL_sym(lib_handle, "COIProcessUnloadLibrary", COI_VERSION1);
    if (ProcessUnloadLibrary == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIProcessUnloadLibrary");
        fini();
        return false;
    }

    ProcessRegisterLibraries =
        (COIRESULT (*)(uint32_t, const void**, const uint64_t*, const char**,
                       const uint64_t*))
            DL_sym(lib_handle, "COIProcessRegisterLibraries", COI_VERSION1);
    if (ProcessRegisterLibraries == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIProcessRegisterLibraries");
        fini();
        return false;
    }

    PipelineCreate =
        (COIRESULT (*)(COIPROCESS, COI_CPU_MASK, uint32_t, COIPIPELINE*))
            DL_sym(lib_handle, "COIPipelineCreate", COI_VERSION1);
    if (PipelineCreate == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIPipelineCreate");
        fini();
        return false;
    }

    PipelineDestroy =
        (COIRESULT (*)(COIPIPELINE))
            DL_sym(lib_handle, "COIPipelineDestroy", COI_VERSION1);
    if (PipelineDestroy == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIPipelineDestroy");
        fini();
        return false;
    }

    PipelineRunFunction =
        (COIRESULT (*)(COIPIPELINE, COIFUNCTION, uint32_t, const COIBUFFER*,
                       const COI_ACCESS_FLAGS*, uint32_t, const COIEVENT*,
                       const void*, uint16_t, void*, uint16_t, COIEVENT*))
            DL_sym(lib_handle, "COIPipelineRunFunction", COI_VERSION1);
    if (PipelineRunFunction == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIPipelineRunFunction");
        fini();
        return false;
    }

    BufferCreate =
        (COIRESULT (*)(uint64_t, COI_BUFFER_TYPE, uint32_t, const void*,
                       uint32_t, const COIPROCESS*, COIBUFFER*))
            DL_sym(lib_handle, "COIBufferCreate", COI_VERSION1);
    if (BufferCreate == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferCreate");
        fini();
        return false;
    }

    BufferCreateFromMemory =
        (COIRESULT (*)(uint64_t, COI_BUFFER_TYPE, uint32_t, void*,
                       uint32_t, const COIPROCESS*, COIBUFFER*))
            DL_sym(lib_handle, "COIBufferCreateFromMemory", COI_VERSION1);
    if (BufferCreateFromMemory == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferCreateFromMemory");
        fini();
        return false;
    }

    BufferDestroy =
        (COIRESULT (*)(COIBUFFER))
            DL_sym(lib_handle, "COIBufferDestroy", COI_VERSION1);
    if (BufferDestroy == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferDestroy");
        fini();
        return false;
    }

    BufferMap =
        (COIRESULT (*)(COIBUFFER, uint64_t, uint64_t, COI_MAP_TYPE, uint32_t,
                       const COIEVENT*, COIEVENT*, COIMAPINSTANCE*,
                       void**))
            DL_sym(lib_handle, "COIBufferMap", COI_VERSION1);
    if (BufferMap == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferMap");
        fini();
        return false;
    }

    BufferUnmap =
        (COIRESULT (*)(COIMAPINSTANCE, uint32_t, const COIEVENT*,
                       COIEVENT*))
            DL_sym(lib_handle, "COIBufferUnmap", COI_VERSION1);
    if (BufferUnmap == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferUnmap");
        fini();
        return false;
    }

    BufferWrite =
        (COIRESULT (*)(COIBUFFER, uint64_t, const void*, uint64_t,
                       COI_COPY_TYPE, uint32_t, const COIEVENT*,
                       COIEVENT*))
            DL_sym(lib_handle, "COIBufferWrite", COI_VERSION1);
    if (BufferWrite == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferWrite");
        fini();
        return false;
    }

    BufferRead =
        (COIRESULT (*)(COIBUFFER, uint64_t, void*, uint64_t,
                                     COI_COPY_TYPE, uint32_t,
                                     const COIEVENT*, COIEVENT*))
            DL_sym(lib_handle, "COIBufferRead", COI_VERSION1);
    if (BufferRead == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferRead");
        fini();
        return false;
    }

    BufferReadMultiD =
        (COIRESULT (*)(COIBUFFER, uint64_t,
                       void *, void *, COI_COPY_TYPE,
                       uint32_t, const   COIEVENT*, COIEVENT*))
            DL_sym(lib_handle, "COIBufferReadMultiD", COI_VERSION1);
    // We  accept that coi library has no COIBufferReadMultiD routine.
    // So there is no check for zero value

    BufferWriteMultiD =
        (COIRESULT (*)(COIBUFFER, const   COIPROCESS,
                       uint64_t, void *, void *,
                       COI_COPY_TYPE, uint32_t, const   COIEVENT*, COIEVENT*))
            DL_sym(lib_handle, "COIBufferWriteMultiD", COI_VERSION1);
    // We  accept that coi library has no COIBufferWriteMultiD routine.
    // So there is no check for zero value

    BufferCopy =
        (COIRESULT (*)(COIBUFFER, COIBUFFER, uint64_t, uint64_t, uint64_t,
                       COI_COPY_TYPE, uint32_t, const COIEVENT*,
                       COIEVENT*))
            DL_sym(lib_handle, "COIBufferCopy", COI_VERSION1);
    if (BufferCopy == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferCopy");
        fini();
        return false;
    }

    BufferGetSinkAddress =
        (COIRESULT (*)(COIBUFFER, uint64_t*))
            DL_sym(lib_handle, "COIBufferGetSinkAddress", COI_VERSION1);
    if (BufferGetSinkAddress == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferGetSinkAddress");
        fini();
        return false;
    }

    BufferSetState =
        (COIRESULT(*)(COIBUFFER, COIPROCESS, COI_BUFFER_STATE,
                      COI_BUFFER_MOVE_FLAG, uint32_t, const COIEVENT*,
                      COIEVENT*))
            DL_sym(lib_handle, "COIBufferSetState", COI_VERSION1);
    if (BufferSetState == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIBufferSetState");
        fini();
        return false;
    }

    EventWait =
        (COIRESULT (*)(uint16_t, const COIEVENT*, int32_t, uint8_t,
                       uint32_t*, uint32_t*))
            DL_sym(lib_handle, "COIEventWait", COI_VERSION1);
    if (EventWait == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIEventWait");
        fini();
        return false;
    }

    PerfGetCycleFrequency =
        (uint64_t (*)(void))
            DL_sym(lib_handle, "COIPerfGetCycleFrequency", COI_VERSION1);
    if (PerfGetCycleFrequency == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIPerfGetCycleFrequency");
        fini();
        return false;
    }

    PipelineClearCPUMask =
        (COIRESULT (*)(COI_CPU_MASK))
            DL_sym(lib_handle, "COIPipelineClearCPUMask", COI_VERSION1);
    if (PipelineClearCPUMask == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIPipelineClearCPUMask");
        fini();
        return false;
    }

    PipelineSetCPUMask =
        (COIRESULT (*)(COIPROCESS, uint32_t,uint8_t, COI_CPU_MASK))
            DL_sym(lib_handle, "COIPipelineSetCPUMask", COI_VERSION1);
    if (PipelineSetCPUMask == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIPipelineSetCPUMask");
        fini();
        return false;
    }

    EngineGetInfo =
        (COIRESULT (*)(COIENGINE, uint32_t, COI_ENGINE_INFO*))
            DL_sym(lib_handle, "COIEngineGetInfo", COI_VERSION1);
    if (EngineGetInfo == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in COI library\n",
                            "COIEngineGetInfo");
        fini();
        return false;
    }
    
    EventRegisterCallback =
        (COIRESULT (*)(COIEVENT,
         void (*)(COIEVENT, const COIRESULT, const void*),
         const void*,
         const uint64_t))
            DL_sym(lib_handle, "COIEventRegisterCallback", COI_VERSION1);

    ProcessConfigureDMA =
        (COIRESULT (*)(const uint64_t, const int))
            DL_sym(lib_handle, "COIProcessConfigureDMA", COI_VERSION1);
    
    is_available = true;

    return true;
}

void fini(void)
{
    is_available = false;

    if (lib_handle != 0) {
#ifndef TARGET_WINNT
        DL_close(lib_handle);
#endif // TARGET_WINNT
        lib_handle = 0;
    }
}

} // namespace COI
