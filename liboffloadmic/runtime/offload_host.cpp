/*
    Copyright (c) 2014-2015 Intel Corporation.  All Rights Reserved.

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


// Forward declaration as the following 2 functions are declared as friend
// in offload_engine.h.
// CLANG does not like static to been after friend declaration.
static void __offload_init_library_once(void);
static void __offload_fini_library(void);

#include "offload_host.h"
#ifdef MYO_SUPPORT
#include "offload_myo_host.h"
#endif

#include <malloc.h>
#ifndef TARGET_WINNT
#include <alloca.h>
#include <elf.h>
#endif // TARGET_WINNT
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <algorithm>
#include <bitset>

#if defined(HOST_WINNT)
#define PATH_SEPARATOR ";"
#else
#define PATH_SEPARATOR ":"
#endif

#define GET_OFFLOAD_NUMBER(timer_data) \
    timer_data? timer_data->offload_number : 0

static void (*task_completion_callback)(void *);

extern "C" {
#ifdef TARGET_WINNT
// Windows does not support imports from libraries without actually
// including them as dependence.  We don't want to include in the
// dependence since is it used only for Fortran when traceback is enabled.
// Chose to implement it with GetProcAddress.
#define FORTRAN_TRACE_BACK  win_for__continue_traceback
int win_for__continue_traceback( _Offload_result coi_offload_result )
{
    HINSTANCE hDLL;
    int (* TraceBackRoutine)(_Offload_result value);

    hDLL = LoadLibrary("libifcoremd.dll");
    if (hDLL != 0) {
        TraceBackRoutine = (int (*)(_Offload_result)) GetProcAddress(hDLL,
                                                 "for__continue_traceback");
        if (TraceBackRoutine != 0) {
            return TraceBackRoutine(coi_offload_result);
        }
        else {
            OFFLOAD_TRACE(3,
            "Cannot find for__continue_traceback routine in libifcorert.dll\n");
            exit(1);
        }
    }
    else {
        OFFLOAD_TRACE(3, "Cannot load libifcorert.dll\n");
        exit(1);
    }
    return 0;
}

#else // TARGET_WINNT

#define FORTRAN_TRACE_BACK for__continue_traceback

// for__continue_traceback is provided as a dummy to resolve link time symbols
// for C/C++ programs.  For Fortran the actual fortran library function in
// libifcore.so is used.
#pragma weak for__continue_traceback
int for__continue_traceback( _Offload_result coi_offload_result )
{
     OFFLOAD_TRACE(3,
          "liboffload function for_continue_traceback should not be called.\n");
     exit(1);
}
#endif //TARGET_WINNT
}  // extern "C"

#ifdef TARGET_WINNT
// Small subset of ELF declarations for Windows which is needed to compile
// this file. ELF header is used to understand what binary type is contained
// in the target image - shared library or executable.

typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef uint64_t Elf64_Addr;
typedef uint64_t Elf64_Off;

#define EI_NIDENT   16

#define ET_EXEC     2
#define ET_DYN      3

typedef struct
{
    unsigned char e_ident[EI_NIDENT];
    Elf64_Half    e_type;
    Elf64_Half    e_machine;
    Elf64_Word    e_version;
    Elf64_Addr    e_entry;
    Elf64_Off     e_phoff;
    Elf64_Off     e_shoff;
    Elf64_Word    e_flags;
    Elf64_Half    e_ehsize;
    Elf64_Half    e_phentsize;
    Elf64_Half    e_phnum;
    Elf64_Half    e_shentsize;
    Elf64_Half    e_shnum;
    Elf64_Half    e_shstrndx;
} Elf64_Ehdr;
#endif // TARGET_WINNT

// Host console and file logging
const char *prefix;
int console_enabled = 0;
int offload_number = 0;

static const char *htrace_envname = "H_TRACE";
static const char *offload_report_envname = "OFFLOAD_REPORT";
static const char *timer_envname = "H_TIME";

// location of offload_main executable
// To be used if the main application has no offload and is not built
// with -offload but dynamic library linked in has offload pragma
char*        mic_device_main = 0;

// DMA channel count used by COI and set via
// OFFLOAD_DMA_CHANNEL_COUNT environment variable
uint32_t mic_dma_channel_count;

// Trace information
static const char* vardesc_direction_as_string[] = {
    "NOCOPY",
    "IN",
    "OUT",
    "INOUT"
};
static const char* vardesc_type_as_string[] = {
    "unknown",
    "data",
    "data_ptr",
    "func_ptr",
    "void_ptr",
    "string_ptr",
    "dv",
    "dv_data",
    "dv_data_slice",
    "dv_ptr",
    "dv_ptr_data",
    "dv_ptr_data_slice",
    "cean_var",
    "cean_var_ptr",
    "c_data_ptr_array",
    "c_func_ptr_array",
    "c_void_ptr_array",
    "c_string_ptr_array"
};

Engine*         mic_engines = 0;
uint32_t        mic_engines_total = 0;
pthread_key_t   mic_thread_key;
MicEnvVar       mic_env_vars;
uint64_t        cpu_frequency = 0;

// MIC_STACKSIZE
uint32_t mic_stack_size = 12 * 1024 * 1024;

// MIC_BUFFERSIZE
uint64_t mic_buffer_size = 0;

// Preallocated 4K page memory size for buffers on MIC
uint64_t mic_4k_buffer_size = 0;

// Preallocated 2M page memory size for buffers on MIC
uint64_t mic_2m_buffer_size = 0;


// MIC_LD_LIBRARY_PATH
char* mic_library_path = 0;

// MIC_PROXY_IO
bool mic_proxy_io = true;

// MIC_PROXY_FS_ROOT
char* mic_proxy_fs_root = 0;

// Threshold for creating buffers with large pages. Buffer is created
// with large pages hint if its size exceeds the threshold value.
// By default large pages are disabled right now (by setting default
// value for threshold to MAX) due to HSD 4114629.
uint64_t __offload_use_2mb_buffers = 0xffffffffffffffffULL;
static const char *mic_use_2mb_buffers_envname  =
    "MIC_USE_2MB_BUFFERS";

static uint64_t __offload_use_async_buffer_write = 2 * 1024 * 1024;
static const char *mic_use_async_buffer_write_envname  =
    "MIC_USE_ASYNC_BUFFER_WRITE";

static uint64_t __offload_use_async_buffer_read = 2 * 1024 * 1024;
static const char *mic_use_async_buffer_read_envname  =
    "MIC_USE_ASYNC_BUFFER_READ";

// device initialization type
OffloadInitType __offload_init_type = c_init_on_offload_all;
static const char *offload_init_envname = "OFFLOAD_INIT";

// active wait
static bool __offload_active_wait = true;
static const char *offload_active_wait_envname = "OFFLOAD_ACTIVE_WAIT";

// OMP_DEFAULT_DEVICE
int __omp_device_num = 0;
static const char *omp_device_num_envname = "OMP_DEFAULT_DEVICE";

//OFFLOAD_PARALLEL_COPY
static bool __offload_parallel_copy = false;
static const char *parallel_copy_envname = "OFFLOAD_PARALLEL_COPY";

//Use COI interface for noncontiguous transfer if it exists.
static bool __offload_use_coi_noncontiguous_transfer = false;
static const char *use_coi_noncontiguous_transfer_envname =
                       "MIC_USE_COI_MULTI_D";

// The list of pending target libraries
static bool            __target_libs;
static TargetImageList __target_libs_list;
static mutex_t         __target_libs_lock;
static mutex_t         stack_alloc_lock;

// Target executable
TargetImage*           __target_exe;

// Print readable offload flags
static void trace_offload_flags(
    OffloadHostTimerData* timer_data,
    OffloadFlags offload_flags
)
{
    // Sized big enough for all flag names
    char fbuffer[256];
    bool first = true;
    if (!OFFLOAD_DO_TRACE && (console_enabled >= 1)) {
        sprintf(fbuffer, "   OffloadFlags=(");
        if (offload_flags.bits.fortran_traceback) {
            sprintf(fbuffer+strlen(fbuffer), "fortran_traceback");
            first = false;
        }
        if (offload_flags.bits.omp_async) {
            sprintf(fbuffer+strlen(fbuffer), first ? "omp_async" : ",omp_async");
            first = false;
        }
        OFFLOAD_DEBUG_TRACE_1(1,
            GET_OFFLOAD_NUMBER(timer_data), c_offload_init_func,
            "%s)\n", fbuffer);
    }
}

// Print readable varDesc flags
static void trace_varDesc_flags(
    OffloadHostTimerData* timer_data,
    varDescFlags offload_flags
)
{
    // SIzed big enough for all flag names
    char fbuffer[256];
    bool first = true;
    if (!OFFLOAD_DO_TRACE && (console_enabled >= 1)) {
        sprintf(fbuffer, "              varDescFlags=(");
        if (offload_flags.is_static) {
            sprintf(fbuffer+strlen(fbuffer), "is_static");
            first = false;
        }
        if (offload_flags.is_static_dstn) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "is_static_dstn" : ",is_static_dstn");
            first = false;
        }
        if (offload_flags.has_length) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "has_length" : ",has_length");
            first = false;
        }
        if (offload_flags.is_stack_buf) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "is_stack_buf" : ",is_stack_buf");
            first = false;
        }
        if (offload_flags.targetptr) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "targetptr" : ",targetptr");
            first = false;
        }
        if (offload_flags.preallocated) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "preallocated" : ",preallocated");
            first = false;
        }
        if (offload_flags.is_pointer) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "is_pointer" : ",is_pointer");
            first = false;
        }
        if (offload_flags.sink_addr) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "sink_addr" : ",sink_addr");
            first = false;
        }
        if (offload_flags.alloc_disp) {
            sprintf(fbuffer+strlen(fbuffer),
               first ? "alloc_disp" : ",alloc_disp");
            first = false;
        }
        if (offload_flags.is_noncont_src) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "is_noncont_src" : ",is_noncont_src");
            first = false;
        }
        if (offload_flags.is_noncont_dst) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "is_noncont_dst" : ",is_noncont_dst");
            first = false;
        }
        if (offload_flags.always_copy) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "always_copy" : ",always_copy");
            first = false;
        }
        if (offload_flags.always_delete) {
            sprintf(fbuffer+strlen(fbuffer),
                first ? "always_delete" : ",always_delete");
            first = false;
        }
        OFFLOAD_DEBUG_TRACE_1(1,
            GET_OFFLOAD_NUMBER(timer_data), c_offload_init_func,
            "%s)\n", fbuffer);
    }
}

static char * offload_get_src_base(void * ptr, uint8_t type)
{
    char *base;
    if (VAR_TYPE_IS_PTR(type)) {
        base = *static_cast<char**>(ptr);
    }
    else if (VAR_TYPE_IS_SCALAR(type)) {
        base = static_cast<char*>(ptr);
    }
    else if (VAR_TYPE_IS_DV_DATA_SLICE(type) || VAR_TYPE_IS_DV_DATA(type)) {
        ArrDesc *dvp;
        if (VAR_TYPE_IS_DV_DATA_SLICE(type)) {
            const Arr_Desc *ap = static_cast<const Arr_Desc*>(ptr);
            dvp = (type == c_dv_data_slice) ?
                  reinterpret_cast<ArrDesc*>(ap->base) :
                  *reinterpret_cast<ArrDesc**>(ap->base);
        }
        else {
            dvp = (type == c_dv_data) ?
                  static_cast<ArrDesc*>(ptr) :
                  *static_cast<ArrDesc**>(ptr);
        }
        base = reinterpret_cast<char*>(dvp->Base);
    }
    else {
        base = NULL;
    }
    return base;
}

void OffloadDescriptor::report_coi_error(error_types msg, COIRESULT res)
{
    // special case for the 'process died' error
    if (res == COI_PROCESS_DIED) {
        m_device.fini_process(true);
    }
    else {
        switch (msg) {
            case c_buf_create:
                if (res == COI_OUT_OF_MEMORY) {
                    msg = c_buf_create_out_of_mem;
                }
                /* fallthru */

            case c_buf_create_from_mem:
            case c_buf_get_address:
            case c_pipeline_create:
            case c_pipeline_run_func:
                LIBOFFLOAD_ERROR(msg, m_device.get_logical_index(), res);
                break;

            case c_buf_read:
            case c_buf_write:
            case c_buf_copy:
            case c_buf_map:
            case c_buf_unmap:
            case c_buf_destroy:
            case c_buf_set_state:
                LIBOFFLOAD_ERROR(msg, res);
                break;

            default:
                break;
        }
    }

    exit(1);
}

_Offload_result OffloadDescriptor::translate_coi_error(COIRESULT res) const
{
    switch (res) {
        case COI_SUCCESS:
            return OFFLOAD_SUCCESS;

        case COI_PROCESS_DIED:
            return OFFLOAD_PROCESS_DIED;

        case COI_OUT_OF_MEMORY:
            return OFFLOAD_OUT_OF_MEMORY;

        default:
            return OFFLOAD_ERROR;
    }
}

// is_targetptr == 0 && is_prealloc == 0 - allocation of pointer data;
// is_targetptr == 1 && is_prealloc == 0 - allocation of target memory:
//    allocate memory at target; use its value as base in target table.
// is_targetptr == 1 && is_prealloc == 1 - use preallocated target memory:
//    base - is address at target of preallocated memory; use its value as
//    base in target table.

bool OffloadDescriptor::alloc_ptr_data(
    PtrData* &ptr_data,
    void *base,
    int64_t disp,
    int64_t size,
    int64_t alloc_disp,
    int align,
    bool is_targptr,
    bool is_prealloc,
    bool pin
)
{
    // total length of base
    int64_t length = size;
    bool is_new;
    COIBUFFER targptr_buf;
    COIRESULT res;
    uint32_t buffer_flags = 0;
    char * base_disp = reinterpret_cast<char *>(base) + disp;

    // create buffer with large pages if data length exceeds
    // large page threshold
    if (length >= __offload_use_2mb_buffers) {
        buffer_flags = COI_OPTIMIZE_HUGE_PAGE_SIZE;
    }
    // Allocate memory at target for targetptr without preallocated as we need
    // its address as base argument in call to m_device.insert_ptr_data
    if (is_targptr && !is_prealloc) {
        length = alloc_disp ? length : size + disp;
        res = COI::BufferCreate(
            length,
            COI_BUFFER_NORMAL,
            buffer_flags,
            0,
            1,
            &m_device.get_process(),
            &targptr_buf);
        if (res != COI_SUCCESS) {
            if (m_status != 0) {
                m_status->result = translate_coi_error(res);
            }
            else if (m_is_mandatory) {
                report_coi_error(c_buf_create, res);
            }
            return false;
        }

        res = COI::BufferGetSinkAddress(
                       targptr_buf, reinterpret_cast<uint64_t *>(&base));
        if (res != COI_SUCCESS) {
            if (m_status != 0) {
                m_status->result = translate_coi_error(res);
            }
            else if (m_is_mandatory) {
                report_coi_error(c_buf_get_address, res);
            }
            return false;
        }
    }

    OFFLOAD_TRACE(3, "Creating association for data: addr %p, length %lld\n",
                  alloc_disp ? base : base_disp,
                  alloc_disp ? length : size + disp);
                  
    // add new entry

    ptr_data = is_targptr ?
               m_device.find_targetptr_data(base_disp) :
               m_device.find_ptr_data(base_disp);
    // if ptr_data is found just need to check it for overlapping
    if (ptr_data) {
        is_new = false;
        base = base_disp;
    }
    else {
        // If association is not found we must create it.
        length = alloc_disp ? length : size + disp;
        ptr_data = is_targptr ?
               m_device.insert_targetptr_data(base, length, is_new) :
               m_device.insert_ptr_data(base, length, is_new);
    }
    if (is_new) {

        OFFLOAD_TRACE(3, "Added new association\n");

        if (length > 0) {
            OffloadTimer timer(get_timer_data(), c_offload_host_alloc_buffers);

            // align should be a power of 2
            if (!pin && !is_targptr &&
                align > 0 && (align & (align - 1)) == 0) {
                // offset within mic_buffer. Can do offset optimization
                // only when source address alignment satisfies requested
                // alignment on the target (cq172736).
                if ((reinterpret_cast<intptr_t>(base) & (align - 1)) == 0) {
                    ptr_data->mic_offset =
                        reinterpret_cast<intptr_t>(base) & 4095;
                }
            }

            // buffer size and flags
            uint64_t buffer_size = length + ptr_data->mic_offset;

            // For targetptr there is no CPU buffer
            if (pin || !is_targptr) {
                // create CPU buffer
                OFFLOAD_DEBUG_TRACE_1(3,
                          GET_OFFLOAD_NUMBER(get_timer_data()),
                          c_offload_create_buf_host,
                          "Creating buffer from source memory %p, "
                          "length %lld\n", base, length);

                // result is not checked because we can continue without cpu
                // buffer. In this case we will use COIBufferRead/Write
                // instead of COIBufferCopy.

                COI::BufferCreateFromMemory(length,
                                        COI_BUFFER_NORMAL,
                                        0,
                                        base,
                                        1,
                                        &m_device.get_process(),
                                        &ptr_data->cpu_buf);
            }

            // create MIC buffer
            if (is_prealloc) {
                OFFLOAD_DEBUG_TRACE_1(3,
                          GET_OFFLOAD_NUMBER(get_timer_data()),
                          c_offload_create_buf_mic,
                          "Creating buffer from sink memory: size %lld, offset %d, "
                          "flags =0x%x\n", buffer_size,
                          ptr_data->mic_offset, buffer_flags);
                res = COI::BufferCreateFromMemory(ptr_data->cpu_addr.length(),
                                                  COI_BUFFER_NORMAL,
                                                  COI_SINK_MEMORY,
                                                  base,
                                                  1,
                                                  &m_device.get_process(),
                                                  &ptr_data->mic_buf);
                if (res != COI_SUCCESS) {
                    if (m_status != 0) {
                        m_status->result = translate_coi_error(res);
                    }
                    else if (m_is_mandatory) {
                        report_coi_error(c_buf_create, res);
                    }
                    ptr_data->alloc_ptr_data_lock.unlock();
                    return false;
                }
            }
            else if (is_targptr) {
                ptr_data->mic_buf = targptr_buf;
            }
            else if (!pin) {
                OFFLOAD_DEBUG_TRACE_1(3,
                          GET_OFFLOAD_NUMBER(get_timer_data()),
                          c_offload_create_buf_mic,
                          "Creating buffer for sink: size %lld, offset %d, "
                          "flags =0x%x\n", buffer_size,
                          ptr_data->mic_offset, buffer_flags);
                res = COI::BufferCreate(buffer_size,
                                        COI_BUFFER_NORMAL,
                                        buffer_flags,
                                        0,
                                        1,
                                        &m_device.get_process(),
                                        &ptr_data->mic_buf);
                if (res != COI_SUCCESS) {
                    if (m_status != 0) {
                        m_status->result = translate_coi_error(res);
                    }
                    else if (m_is_mandatory) {
                        report_coi_error(c_buf_create, res);
                    }
                    ptr_data->alloc_ptr_data_lock.unlock();
                    return false;
                }
            }

            if (!pin) {
                // make buffer valid on the device.
                res = COI::BufferSetState(ptr_data->mic_buf,
                    m_device.get_process(),
                    COI_BUFFER_VALID,
                    COI_BUFFER_NO_MOVE,
                    0, 0, 0);
                if (res != COI_SUCCESS) {
                    if (m_status != 0) {
                        m_status->result = translate_coi_error(res);
                    }
                    else if (m_is_mandatory) {
                        report_coi_error(c_buf_set_state, res);
                    }
                    ptr_data->alloc_ptr_data_lock.unlock();
                    return false;
                }

                res = COI::BufferSetState(ptr_data->mic_buf,
                    COI_PROCESS_SOURCE,
                    COI_BUFFER_INVALID,
                    COI_BUFFER_NO_MOVE,
                0, 0, 0);
                if (res != COI_SUCCESS) {
                    if (m_status != 0) {
                        m_status->result = translate_coi_error(res);
                    }
                    else if (m_is_mandatory) {
                        report_coi_error(c_buf_set_state, res);
                    }
                    ptr_data->alloc_ptr_data_lock.unlock();
                    return false;
                }
            }
        }
        ptr_data->alloc_disp = alloc_disp;
        ptr_data->alloc_ptr_data_lock.unlock();
    }
    else {
        mutex_locker_t locker(ptr_data->alloc_ptr_data_lock);

        OFFLOAD_TRACE(3, "Found existing association: addr %p, length %lld, "
                      "is_static %d\n",
                      ptr_data->cpu_addr.start(), ptr_data->cpu_addr.length(),
                      ptr_data->is_static);

        // This is not a new entry. Make sure that provided address range fits
        // into existing one.
        MemRange addr_range(base, length);
        if (!ptr_data->cpu_addr.contains(addr_range)) {
            LIBOFFLOAD_ERROR(c_bad_ptr_mem_alloc, base, length,
                           const_cast<void *>(ptr_data->cpu_addr.start()),
                           ptr_data->cpu_addr.length());
            exit(1);
        }

        // if the entry is associated with static data it may not have buffers
        // created because they are created on demand.
        if (ptr_data->is_static && !init_static_ptr_data(ptr_data)) {
            return false;
        }
    }

    return true;
}

bool OffloadDescriptor::find_ptr_data(
    PtrData* &ptr_data,
    void *in_base,
    int64_t disp,
    int64_t size,
    bool is_targetptr,
    bool report_error
)
{
    // total length of base
    int64_t length = size;
    char *base = reinterpret_cast<char *>(in_base) + disp;
    
    OFFLOAD_TRACE(3, "Looking for association for data: addr %p, "
                  "length %lld\n", base, length);

    // find existing association in pointer table
    ptr_data = is_targetptr ?
               m_device.find_targetptr_data(base) :
               m_device.find_ptr_data(base);
    if (ptr_data == 0) {
        if (report_error) {
            LIBOFFLOAD_ERROR(c_no_ptr_data, base);
            exit(1);
        }
        OFFLOAD_TRACE(3, "Association does not exist\n");
        return true;
    }

    OFFLOAD_TRACE(3, "Found association: base %p, length %lld, is_static %d\n",
                  ptr_data->cpu_addr.start(), ptr_data->cpu_addr.length(),
                  ptr_data->is_static);

    // make sure that provided address range fits into existing one
    MemRange addr_range(base, length);
    if (!ptr_data->cpu_addr.contains(addr_range)) {
        if (report_error) {
            LIBOFFLOAD_ERROR(c_bad_ptr_mem_range, base, length,
                           const_cast<void *>(ptr_data->cpu_addr.start()),
                           ptr_data->cpu_addr.length());
            exit(1);
        }
        OFFLOAD_TRACE(3, "Existing association partially overlaps with "
                      "data address range\n");
        ptr_data = 0;
        return true;
    }

    // if the entry is associated with static data it may not have buffers
    // created because they are created on demand.
    if (ptr_data->is_static && !init_static_ptr_data(ptr_data)) {
        return false;
    }

    return true;
}

bool OffloadDescriptor::init_static_ptr_data(PtrData *ptr_data)
{
    OffloadTimer timer(get_timer_data(), c_offload_host_alloc_buffers);

    if (ptr_data->cpu_buf == 0) {
        OFFLOAD_TRACE(3, "Creating buffer from source memory %llx\n",
                      ptr_data->cpu_addr.start());

        COIRESULT res = COI::BufferCreateFromMemory(
            ptr_data->cpu_addr.length(),
            COI_BUFFER_NORMAL,
            0,
            const_cast<void*>(ptr_data->cpu_addr.start()),
            1, &m_device.get_process(),
            &ptr_data->cpu_buf);

        if (res != COI_SUCCESS) {
            if (m_status != 0) {
                m_status->result = translate_coi_error(res);
                return false;
            }
            report_coi_error(c_buf_create_from_mem, res);
        }
    }

    if (ptr_data->mic_buf == 0) {
        OFFLOAD_TRACE(3, "Creating buffer from sink memory %llx\n",
                      ptr_data->mic_addr);

        COIRESULT res = COI::BufferCreateFromMemory(
            ptr_data->cpu_addr.length(),
            COI_BUFFER_NORMAL,
            COI_SINK_MEMORY,
            reinterpret_cast<void*>(ptr_data->mic_addr),
            1, &m_device.get_process(),
            &ptr_data->mic_buf);

        if (res != COI_SUCCESS) {
            if (m_status != 0) {
                m_status->result = translate_coi_error(res);
                return false;
            }
            report_coi_error(c_buf_create_from_mem, res);
        }
    }

    return true;
}

bool OffloadDescriptor::init_mic_address(PtrData *ptr_data)
{
    if (ptr_data->mic_buf != 0 && ptr_data->mic_addr == 0) {
        COIRESULT res = COI::BufferGetSinkAddress(ptr_data->mic_buf,
                                                  &ptr_data->mic_addr);
        if (res != COI_SUCCESS) {
            if (m_status != 0) {
                m_status->result = translate_coi_error(res);
            }
            else if (m_is_mandatory) {
                report_coi_error(c_buf_get_address, res);
            }
            return false;
        }
    }
    return true;
}

bool OffloadDescriptor::nullify_target_stack(
    COIBUFFER targ_buf,
    uint64_t size
)
{
    char * ptr = (char*)malloc(size);
    if (ptr == NULL)
      LIBOFFLOAD_ERROR(c_malloc);
    COIRESULT res;

    memset(ptr, 0, size);
    res = COI::BufferWrite(
        targ_buf,
        0,
        ptr,
        size,
        COI_COPY_UNSPECIFIED,
        0, 0, 0);
    free(ptr);
    if (res != COI_SUCCESS) {
        if (m_status != 0) {
            m_status->result = translate_coi_error(res);
            return false;
        }
        report_coi_error(c_buf_write, res);
    }
    return true;
}

bool OffloadDescriptor::offload_stack_memory_manager(
    const void * stack_begin,
    int  routine_id,
    int  buf_size,
    int  align,
    bool *is_new)
{
    mutex_locker_t locker(stack_alloc_lock);

    PersistData * new_el;
    PersistDataList::iterator it_begin = m_device.m_persist_list.begin();
    PersistDataList::iterator it_end;
    int erase = 0;
    uint64_t cur_thread_id = m_device.get_thread_id();

    *is_new = false;

    for (PersistDataList::iterator it = m_device.m_persist_list.begin();
        it != m_device.m_persist_list.end(); it++) {
        PersistData cur_el = *it;

        if (stack_begin > it->stack_cpu_addr) {
            // this stack data must be destroyed
            if (cur_thread_id == cur_el.thread_id) {
                m_destroy_stack.push_front(cur_el.stack_ptr_data);
                it_end = it;
                erase++;
            }
        }
        else if (stack_begin == it->stack_cpu_addr) {
            if (routine_id != it-> routine_id) {
                // this stack data must be destroyed
                m_destroy_stack.push_front(cur_el.stack_ptr_data);
                it_end = it;
                erase++;
                break;
            }
            else {
                // stack data is reused
                m_stack_ptr_data = it->stack_ptr_data;
                if (erase > 0) {
                    // all obsolete stack sections must be erased from the list
                    m_device.m_persist_list.erase(it_begin, ++it_end);

                    m_in_datalen +=
                        erase * sizeof(new_el->stack_ptr_data->mic_addr);
                }
                OFFLOAD_TRACE(3, "Reuse of stack buffer with addr %p\n",
                                 m_stack_ptr_data->mic_addr);
                return true;
            }
        }
        else if (stack_begin < it->stack_cpu_addr &&
                 cur_thread_id == cur_el.thread_id) {
            break;
        }
    }

    if (erase > 0) {
        // all obsolete stack sections must be erased from the list
        m_device.m_persist_list.erase(it_begin, ++it_end);
        m_in_datalen += erase * sizeof(new_el->stack_ptr_data->mic_addr);
    }
    // new stack table is created
    new_el = new PersistData(stack_begin, routine_id, buf_size, cur_thread_id);
    // create MIC buffer
    COIRESULT res;
    uint32_t buffer_flags = 0;

    // create buffer with large pages if data length exceeds
    // large page threshold
    if (buf_size >= __offload_use_2mb_buffers) {
        buffer_flags = COI_OPTIMIZE_HUGE_PAGE_SIZE;
    }
    res = COI::BufferCreate(buf_size,
        COI_BUFFER_NORMAL,
        buffer_flags,
        0,
        1,
        &m_device.get_process(),
        &new_el->stack_ptr_data->mic_buf);
    if (res != COI_SUCCESS) {
        if (m_status != 0) {
            m_status->result = translate_coi_error(res);
        }
        else if (m_is_mandatory) {
            report_coi_error(c_buf_create, res);
        }
        return false;
    }
    // make buffer valid on the device.
    res = COI::BufferSetState(new_el->stack_ptr_data->mic_buf,
        m_device.get_process(),
        COI_BUFFER_VALID,
        COI_BUFFER_NO_MOVE,
        0, 0, 0);
    if (res != COI_SUCCESS) {
        if (m_status != 0) {
            m_status->result = translate_coi_error(res);
        }
        else if (m_is_mandatory) {
            report_coi_error(c_buf_set_state, res);
        }
        return false;
    }
    res = COI::BufferSetState(new_el->stack_ptr_data->mic_buf,
        COI_PROCESS_SOURCE,
        COI_BUFFER_INVALID,
        COI_BUFFER_NO_MOVE,
        0, 0, 0);
    if (res != COI_SUCCESS) {
        if (m_status != 0) {
            m_status->result = translate_coi_error(res);
        }
        else if (m_is_mandatory) {
            report_coi_error(c_buf_set_state, res);
        }
        return false;
    }
    // persistence algorithm requires target stack initialy to be nullified
    if (!nullify_target_stack(new_el->stack_ptr_data->mic_buf, buf_size)) {
        return false;
    }

    m_stack_ptr_data = new_el->stack_ptr_data;
    init_mic_address(m_stack_ptr_data);
    OFFLOAD_TRACE(3, "Allocating stack buffer with addr %p\n",
                      m_stack_ptr_data->mic_addr);
    m_device.m_persist_list.push_front(*new_el);
    init_mic_address(new_el->stack_ptr_data);
    *is_new = true;
    return true;
}

bool OffloadDescriptor::setup_descriptors(
    VarDesc *vars,
    VarDesc2 *vars2,
    int vars_total,
    int entry_id,
    const void *stack_addr
)
{
    COIRESULT res;

    OffloadTimer timer(get_timer_data(), c_offload_host_setup_buffers);

    // make a copy of variable descriptors
    m_vars_total = vars_total;
    if (vars_total > 0) {
        m_vars = (VarDesc*) malloc(m_vars_total * sizeof(VarDesc));
        if (m_vars == NULL)
          LIBOFFLOAD_ERROR(c_malloc);
        memcpy(m_vars, vars, m_vars_total * sizeof(VarDesc));
        m_vars_extra = (VarExtra*) malloc(m_vars_total * sizeof(VarExtra));
        if (m_vars_extra == NULL)
          LIBOFFLOAD_ERROR(c_malloc);
    }

    // dependencies
    m_in_deps_allocated = m_vars_total + 1;   
    m_in_deps = (COIEVENT*) malloc(sizeof(COIEVENT) * m_in_deps_allocated);
    if (m_in_deps == NULL)
      LIBOFFLOAD_ERROR(c_malloc);
    if (m_vars_total > 0) {
        m_out_deps_allocated = m_vars_total;
        m_out_deps = (COIEVENT*) malloc(sizeof(COIEVENT) * m_out_deps_allocated);
        if (m_out_deps == NULL)
          LIBOFFLOAD_ERROR(c_malloc);
    }

    // copyin/copyout data length
    m_in_datalen = 0;
    m_out_datalen = 0;

    // First pass over variable descriptors
    // - Calculate size of the input and output non-pointer data
    // - Allocate buffers for input and output pointers
    for (int i = 0; i < m_vars_total; i++) {
        void*   alloc_base = NULL;
        int64_t alloc_disp = 0;
        int64_t alloc_size = 0;
        bool    src_is_for_mic = (m_vars[i].direction.out ||
                                  m_vars[i].into == NULL);

        const char *var_sname = "";
        if (vars2 != NULL && i < vars_total) {
            if (vars2[i].sname != NULL) {
                var_sname = vars2[i].sname;
            }
        }
        OFFLOAD_TRACE(2, "   VarDesc %d, var=%s, %s, %s\n",
            i, var_sname,
            vardesc_direction_as_string[m_vars[i].direction.bits],
            vardesc_type_as_string[m_vars[i].type.src]);
        if (vars2 != NULL && i < vars_total && vars2[i].dname != NULL) {
            OFFLOAD_TRACE(2, "              into=%s, %s\n", vars2[i].dname,
                vardesc_type_as_string[m_vars[i].type.dst]);
        }
        OFFLOAD_TRACE(2,
            "              type_src=%d, type_dstn=%d, direction=%d, "
            "alloc_if=%d, free_if=%d, align=%d, mic_offset=%d, flags=0x%x, "
            "offset=%lld, size=%lld, count/disp=%lld, ptr=%p, into=%p\n",
            m_vars[i].type.src,
            m_vars[i].type.dst,
            m_vars[i].direction.bits,
            m_vars[i].alloc_if,
            m_vars[i].free_if,
            m_vars[i].align,
            m_vars[i].mic_offset,
            m_vars[i].flags.bits,
            m_vars[i].offset,
            m_vars[i].size,
            m_vars[i].count,
            m_vars[i].ptr,
            m_vars[i].into);
        // If any varDesc flags bits set, show them
        if (console_enabled >= 1 && m_vars[i].flags.bits != 0) {
            trace_varDesc_flags(get_timer_data(), m_vars[i].flags);
        }

        // preallocated implies targetptr
        if (m_vars[i].flags.preallocated) {
            // targetptr preallocated alloc_if(1) may not be used with
            // an in clause
            if (m_vars[i].direction.in && m_vars[i].alloc_if) {
                LIBOFFLOAD_ERROR(c_in_with_preallocated);
                exit(1);
            }
            m_vars[i].flags.targetptr = 1;
        }
        if (m_vars[i].alloc != NULL) {
            // array descriptor
            const Arr_Desc *ap =
                static_cast<const Arr_Desc*>(m_vars[i].alloc);

            // debug dump
            ARRAY_DESC_DUMP("    ", "ALLOC", ap, 0, 1);

            __arr_data_offset_and_length(ap, alloc_disp, alloc_size);

            alloc_base = reinterpret_cast<void*>(ap->base);
        }

        m_vars_extra[i].alloc = m_vars[i].alloc;
        m_vars_extra[i].cpu_disp = 0;
        m_vars_extra[i].cpu_offset = 0;
        m_vars_extra[i].src_data = 0;
        m_vars_extra[i].read_rng_src = 0;
        m_vars_extra[i].read_rng_dst = 0;
        m_vars_extra[i].omp_last_event_type = c_last_not;        
        // flag is_arr_ptr_el is 1 only for var_descs generated
        // for c_data_ptr_array type
        if (i < vars_total) {
            m_vars_extra[i].is_arr_ptr_el = 0;
        }

        switch (m_vars[i].type.src) {
            case c_data_ptr_array:
                {
                    const Arr_Desc *ap;
                    const VarDesc3 *vd3 =
                        static_cast<const VarDesc3*>(m_vars[i].ptr);
                    int flags = vd3->array_fields;
                    OFFLOAD_TRACE(2,
                        "              pointer array flags = %04x\n", flags);
                    OFFLOAD_TRACE(2,
                        "              pointer array type is %s\n",
                        vardesc_type_as_string[flags & 0x3f]);
                    ap = static_cast<const Arr_Desc*>(vd3->ptr_array);
                    ARRAY_DESC_DUMP("              ", "ptr array", ap,
                                    m_vars[i].flags.is_pointer, 1);
                    if (m_vars[i].into) {
                        ap = static_cast<const Arr_Desc*>(m_vars[i].into);
                        ARRAY_DESC_DUMP(
                            "              ", "into array", ap, 0, 1);
                    }
                    if ((flags & (1<<flag_align_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>(vd3->align_array);
                        ARRAY_DESC_DUMP(
                            "              ", "align array", ap, 0, 1);
                    }
                    if ((flags & (1<<flag_alloc_if_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>(vd3->alloc_if_array);
                        ARRAY_DESC_DUMP(
                            "              ", "alloc_if array", ap, 0, 1);
                    }
                    if ((flags & (1<<flag_free_if_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>(vd3->free_if_array);
                        ARRAY_DESC_DUMP(
                            "              ", "free_if array", ap, 0, 1);
                    }
                    if ((flags & (1<<flag_extent_start_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>(vd3->extent_start);
                        ARRAY_DESC_DUMP(
                            "              ", "extent_start array", ap, 0, 1);
                    } else if ((flags &
                        (1<<flag_extent_start_is_scalar)) != 0) {
                        OFFLOAD_TRACE(2,
                            "              extent_start scalar = %d\n",
                            (int64_t)vd3->extent_start);
                    }
                    if ((flags & (1<<flag_extent_elements_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>
                            (vd3->extent_elements);
                        ARRAY_DESC_DUMP("              ",
                                        "extent_elements array", ap, 0, 1);
                    } else if ((flags &
                        (1<<flag_extent_elements_is_scalar)) != 0) {
                        OFFLOAD_TRACE(2,
                            "              extent_elements scalar = %d\n",
                            (int64_t)vd3->extent_elements);
                    }
                    if ((flags & (1<<flag_into_start_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>(vd3->into_start);
                        ARRAY_DESC_DUMP(
                            "              ", "into_start array", ap, 0, 1);
                    } else if ((flags &
                        (1<<flag_into_start_is_scalar)) != 0) {
                        OFFLOAD_TRACE(2,
                            "              into_start scalar = %d\n",
                            (int64_t)vd3->into_start);
                    }
                    if ((flags & (1<<flag_into_elements_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>(vd3->into_elements);
                        ARRAY_DESC_DUMP(
                            "              ", "into_elements array", ap, 0, 1);
                    } else if ((flags &
                        (1<<flag_into_elements_is_scalar)) != 0) {
                        OFFLOAD_TRACE(2,
                            "              into_elements scalar = %d\n",
                            (int64_t)vd3->into_elements);
                    }
                    if ((flags & (1<<flag_alloc_start_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>(vd3->alloc_start);
                        ARRAY_DESC_DUMP(
                            "              ", "alloc_start array", ap, 0, 1);
                    } else if ((flags &
                        (1<<flag_alloc_start_is_scalar)) != 0) {
                        OFFLOAD_TRACE(2,
                            "              alloc_start scalar = %d\n",
                            (int64_t)vd3->alloc_start);
                    }
                    if ((flags & (1<<flag_alloc_elements_is_array)) != 0) {
                        ap = static_cast<const Arr_Desc*>(vd3->alloc_elements);
                        ARRAY_DESC_DUMP("              ",
                                        "alloc_elements array", ap, 0, 1);
                    } else if ((flags &
                        (1<<flag_alloc_elements_is_scalar)) != 0) {
                        OFFLOAD_TRACE(2,
                            "              alloc_elements scalar = %d\n",
                            (int64_t)vd3->alloc_elements);
                    }
                }
                if (!gen_var_descs_for_pointer_array(i)) {
                    return false;
                }
                break;

            case c_data:
            case c_void_ptr:
            case c_cean_var:
                // In all uses later
                // VarDesc.size will have the length of the data to be
                // transferred
                // VarDesc.disp will have an offset from base
                if (m_vars[i].type.src == c_cean_var) {
                    // array descriptor
                    const Arr_Desc *ap =
                        static_cast<const Arr_Desc*>(m_vars[i].ptr);

                    // debug dump
                    ARRAY_DESC_DUMP("", "IN/OUT", ap, 0, !src_is_for_mic);

                    // offset and length are derived from the array descriptor
                    __arr_data_offset_and_length(ap, m_vars[i].disp,
                                                 m_vars[i].size);
                    if (!is_arr_desc_contiguous(ap)) {
                        m_vars[i].flags.is_noncont_src = 1;
                        m_vars_extra[i].read_rng_src =
                            init_read_ranges_arr_desc(ap);
                    }
                    // all necessary information about length and offset is
                    // transferred in var descriptor. There is no need to send
                    // array descriptor to the target side.
                    m_vars[i].ptr = reinterpret_cast<void*>(ap->base);
                }
                else {
                    m_vars[i].size *= m_vars[i].count;
                    m_vars[i].disp = 0;
                }

                if (m_vars[i].direction.bits) {
                    // make sure that transfer size > 0
                    if (m_vars[i].size <= 0) {
                        LIBOFFLOAD_ERROR(c_zero_or_neg_transfer_size);
                        exit(1);
                    }

                    if (m_vars[i].flags.is_static) {
                        PtrData *ptr_data;

                        // find data associated with variable
                        if (!find_ptr_data(ptr_data,
                                           m_vars[i].ptr,
                                           m_vars[i].disp,
                                           m_vars[i].size,
                                           false, false)) {
                            return false;
                        }

                        if (ptr_data != 0) {
                            // offset to base from the beginning of the buffer
                            // memory
                            m_vars[i].offset =
                                (char*) m_vars[i].ptr -
                                (char*) ptr_data->cpu_addr.start();
                        }
                        else {
                            m_vars[i].flags.is_static = false;
                            if (m_vars[i].into == NULL) {
                                m_vars[i].flags.is_static_dstn = false;
                            }
                        }
                        m_vars_extra[i].src_data = ptr_data;
                    }

                    if (m_is_openmp) {
                        if (m_vars[i].flags.is_static) {
                            // Static data is transferred either by omp target
                            // update construct which passes zeros for
                            // alloc_if and free_if or by always modifier.
                            if (!m_vars[i].flags.always_copy &&
                                (m_vars[i].alloc_if || m_vars[i].free_if)) {
                                m_vars[i].direction.bits = c_parameter_nocopy;
                            }
                        }
                        else {
                            AutoData *auto_data;
                            if (m_vars[i].alloc_if) {
                                auto_data = m_device.insert_auto_data(
                                    m_vars[i].ptr, m_vars[i].size);
                                auto_data->add_reference();
                            }
                            else {
                                // TODO: what should be done if var is not in
                                // the table?
                                auto_data = m_device.find_auto_data(
                                    m_vars[i].ptr);
                            }

                            // For automatic variables data is transferred:
                            // - if always modifier is used OR
                            // - if alloc_if == 0 && free_if == 0 OR
                            // - if reference count is 1
                            if (!m_vars[i].flags.always_copy &&
                                (m_vars[i].alloc_if || m_vars[i].free_if) &&
                                auto_data != 0 &&
                                auto_data->get_reference() != 1) {
                                m_vars[i].direction.bits = c_parameter_nocopy;
                            }

                            // save data for later use
                            m_vars_extra[i].auto_data = auto_data;
                        }
                    }

                    if (m_vars[i].direction.in &&
                        !m_vars[i].flags.is_static) {
                        m_in_datalen += m_vars[i].size;

                        // for non-static target destination defined as CEAN
                        // expression we pass to target its size and dist
                        if (m_vars[i].into == NULL &&
                            m_vars[i].type.src == c_cean_var) {
                            m_in_datalen += 2 * sizeof(uint64_t);
                        }
                        m_need_runfunction = true;
                    }
                    if (m_vars[i].direction.out &&
                        !m_vars[i].flags.is_static) {
                        m_out_datalen += m_vars[i].size;
                        m_need_runfunction = true;
                    }
                }
                break;

            case c_dv:
                if (m_vars[i].direction.bits ||
                    m_vars[i].alloc_if ||
                    m_vars[i].free_if) {
                    ArrDesc *dvp = static_cast<ArrDesc*>(m_vars[i].ptr);

                    // debug dump
                    __dv_desc_dump("IN/OUT", dvp);

                    // send dope vector contents excluding base
                    m_in_datalen += m_vars[i].size - sizeof(uint64_t);
                    m_need_runfunction = true;
                }
                break;

            case c_string_ptr:
                if ((m_vars[i].direction.bits ||
                     m_vars[i].alloc_if ||
                     m_vars[i].free_if) &&
                    m_vars[i].size == 0) {
                    m_vars[i].size = 1;
                    m_vars[i].count =
                        strlen(*static_cast<char**>(m_vars[i].ptr)) + 1;
                }
                /* fallthru */

            case c_data_ptr:
                if (m_vars[i].flags.is_stack_buf &&
                    !m_vars[i].direction.bits &&
                    m_vars[i].alloc_if) {
                    // this var_desc is for stack buffer
                    bool is_new;

                    if (!offload_stack_memory_manager(
                            stack_addr, entry_id,
                            m_vars[i].count, m_vars[i].align, &is_new)) {
                        return false;
                    }
                    if (is_new) {
                        m_compute_buffers.push_back(
                            m_stack_ptr_data->mic_buf);
                        m_device.m_persist_list.front().cpu_stack_addr =
                            static_cast<char*>(m_vars[i].ptr);
                    }
                    else {
                        m_vars[i].flags.sink_addr = 1;
                        m_in_datalen += sizeof(m_stack_ptr_data->mic_addr);
                    }
                    m_vars[i].size = m_destroy_stack.size();
                    m_vars_extra[i].src_data = m_stack_ptr_data;

                    // need to add or remove references for stack buffer at target
                    if (is_new || m_destroy_stack.size()) {
                        m_need_runfunction = true;
                    }

                    break;
                }
                /* fallthru */

            case c_cean_var_ptr:
            case c_dv_ptr:
                if (m_vars[i].type.src == c_cean_var_ptr) {
                    // array descriptor
                    const Arr_Desc *ap =
                        static_cast<const Arr_Desc*>(m_vars[i].ptr);

                    // debug dump
                    ARRAY_DESC_DUMP("", "IN/OUT", ap, 1, !src_is_for_mic);

                    // offset and length are derived from the array descriptor
                    __arr_data_offset_and_length(ap, m_vars[i].disp,
                                                 m_vars[i].size);

                    if (!is_arr_desc_contiguous(ap)) {
                        m_vars[i].flags.is_noncont_src = 1;
                        m_vars_extra[i].read_rng_src =
                            init_read_ranges_arr_desc(ap);
                    }
                    // all necessary information about length and offset is
                    // transferred in var descriptor. There is no need to send
                    // array descriptor to the target side.
                    m_vars[i].ptr = reinterpret_cast<void*>(ap->base);
                }
                else if (m_vars[i].type.src == c_dv_ptr) {
                    // need to send DV to the device unless it is 'nocopy'
                    if (m_vars[i].direction.bits ||
                        m_vars[i].alloc_if ||
                        m_vars[i].free_if) {
                        ArrDesc *dvp = *static_cast<ArrDesc**>(m_vars[i].ptr);

                        // debug dump
                        __dv_desc_dump("IN/OUT", dvp);

                        m_vars[i].direction.bits = c_parameter_in;
                    }

                    // no displacement
                    m_vars[i].disp = 0;
                }
                else {
                    // c_data_ptr or c_string_ptr
                    m_vars[i].size *= m_vars[i].count;
                    m_vars[i].disp = 0;
                }

                if (m_vars[i].direction.bits ||
                    m_vars[i].alloc_if ||
                    m_vars[i].free_if) {
                    PtrData *ptr_data;

                    // check that buffer length > 0
                    if (m_vars[i].alloc_if &&
                        m_vars[i].disp + m_vars[i].size <
                        (m_is_openmp ? 0 : 1)) {
                        LIBOFFLOAD_ERROR(c_zero_or_neg_ptr_len);
                        exit(1);
                    }

                    // base address
                    void *base = *static_cast<void**>(m_vars[i].ptr);

                    // allocate buffer if we have no INTO and don't need
                    // allocation for the ptr at target
                    if (src_is_for_mic) {
                        if (m_vars[i].flags.is_stack_buf) {
                            // for stack persistent objects ptr data is created
                            // by var_desc with number 0.
                            // Its ptr_data is stored at m_stack_ptr_data
                            ptr_data = m_stack_ptr_data;
                            m_vars[i].flags.sink_addr = 1;
                        }
                        else if (m_vars[i].alloc_if) {
                            if (m_vars[i].flags.preallocated) {
                                m_out_datalen += sizeof(void*);
                                m_need_runfunction = true;
                                break;
                            }
                            // add new entry
                            if (!alloc_ptr_data(
                                    ptr_data,
                                    reinterpret_cast<char *>(base) + alloc_disp,
                                    (alloc_base != NULL) ?
                                        alloc_disp : m_vars[i].disp,
                                    (alloc_base != NULL) ?
                                        alloc_size : m_vars[i].size,
                                    alloc_disp,
                                    (alloc_base != NULL) ?
                                        0 : m_vars[i].align,
                                    m_vars[i].flags.targetptr,
                                    0,
                                    m_vars[i].flags.pin)) {
                                return false;
                            }
                            if (m_vars[i].flags.targetptr) {
                                if (!init_mic_address(ptr_data)) {
                                    return false;
                                }
                                *static_cast<void**>(m_vars[i].ptr) = base =
                                  reinterpret_cast<void*>(ptr_data->mic_addr);
                            }
                            if (ptr_data->add_reference() == 0 &&
                                ptr_data->mic_buf != 0) {
                                // add buffer to the list of buffers that
                                // are passed to dispatch call
                                m_compute_buffers.push_back(
                                    ptr_data->mic_buf);
                            }
                            else if (!m_vars[i].flags.pin &&
                                     !m_vars[i].flags.preallocated) {
                                // will send buffer address to device
                                m_vars[i].flags.sink_addr = 1;
                            }

                            if (!m_vars[i].flags.pin &&
                                !ptr_data->is_static) {
                                // need to add reference for buffer
                                m_need_runfunction = true;
                            }
                        }
                        else {
                            bool error_if_not_found = true;
                            if (m_is_openmp) {
                                // For omp target update variable is ignored
                                // if it does not exist.
                                if (m_vars[i].flags.always_copy ||
                                    (!m_vars[i].alloc_if &&
                                     !m_vars[i].free_if)) {
                                    error_if_not_found = false;
                                }
                            }

                            // use existing association from pointer table
                            if (!find_ptr_data(ptr_data,
                                               base,
                                               m_vars[i].disp,
                                               m_vars[i].size,
                                               m_vars[i].flags.targetptr,
                                               error_if_not_found)) {
                                return false;
                            }

                            if (m_is_openmp) {
                                // make var nocopy if it does not exist
                                if (ptr_data == 0) {
                                    m_vars[i].direction.bits =
                                        c_parameter_nocopy;
                                }
                            }

                            if (ptr_data != 0) {
                                m_vars[i].flags.sink_addr = 1;
                            }
                        }

                        if (ptr_data != 0) {
                            if (m_is_openmp) {
                                // data is transferred only if
                                // alloc_if == 0 && free_if == 0
                                // or reference count is 1
                                if (!m_vars[i].flags.always_copy &&
                                    ((m_vars[i].alloc_if ||
                                      m_vars[i].free_if) &&
                                     ptr_data->get_reference() != 1)) {
                                    m_vars[i].direction.bits =
                                        c_parameter_nocopy;
                                }
                            }

                            if (ptr_data->alloc_disp != 0) {
                                m_vars[i].flags.alloc_disp = 1;
                                m_in_datalen += sizeof(alloc_disp);
                            }

                            if (m_vars[i].flags.sink_addr) {
                                // get buffers's address on the sink
                                if (!init_mic_address(ptr_data)) {
                                    return false;
                                }

                                m_in_datalen += sizeof(ptr_data->mic_addr);
                            }

                            if (!m_vars[i].flags.pin &&
                                !ptr_data->is_static && m_vars[i].free_if) {
                                // need to decrement buffer reference on target
                                m_need_runfunction = true;
                            }

                            // offset to base from the beginning of the buffer
                            // memory
                            m_vars[i].offset = (char*) base -
                                (char*) ptr_data->cpu_addr.start();

                            // copy other pointer properties to var descriptor
                            m_vars[i].mic_offset = ptr_data->mic_offset;
                            m_vars[i].flags.is_static = ptr_data->is_static;
                        }
                    }
                    else {
                        if (!find_ptr_data(ptr_data,
                                           base,
                                           m_vars[i].disp,
                                           m_vars[i].size,
                                           false, false)) {
                            return false;
                        }
                        if (ptr_data) {
                            m_vars[i].offset =
                                (char*) base -
                                (char*) ptr_data->cpu_addr.start();
                        }
                    }

                    // save pointer data
                    m_vars_extra[i].src_data = ptr_data;
                }
                break;

            case c_func_ptr:
                if (m_vars[i].direction.in) {
                    m_in_datalen += __offload_funcs.max_name_length();
                }
                if (m_vars[i].direction.out) {
                    m_out_datalen += __offload_funcs.max_name_length();
                }
                m_need_runfunction = true;
                break;

            case c_dv_data:
            case c_dv_ptr_data:
            case c_dv_data_slice:
            case c_dv_ptr_data_slice:
                ArrDesc *dvp;
                if (VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.src)) {
                    const Arr_Desc *ap;
                    ap = static_cast<const Arr_Desc*>(m_vars[i].ptr);

                    dvp = (m_vars[i].type.src == c_dv_data_slice) ?
                          reinterpret_cast<ArrDesc*>(ap->base) :
                          *reinterpret_cast<ArrDesc**>(ap->base);
                }
                else {
                    dvp = (m_vars[i].type.src == c_dv_data) ?
                          static_cast<ArrDesc*>(m_vars[i].ptr) :
                          *static_cast<ArrDesc**>(m_vars[i].ptr);
                }

                // if allocatable dope vector isn't allocated don't
                // transfer its data
                if (!__dv_is_allocated(dvp)) {
                    m_vars[i].direction.bits = c_parameter_nocopy;
                    m_vars[i].alloc_if = 0;
                    m_vars[i].free_if = 0;
                }
                if (m_vars[i].direction.bits ||
                    m_vars[i].alloc_if ||
                    m_vars[i].free_if) {
                    const Arr_Desc *ap;

                    if (VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.src)) {
                        ap = static_cast<const Arr_Desc*>(m_vars[i].ptr);

                        // debug dump
                        ARRAY_DESC_DUMP("", "IN/OUT", ap, 0, !src_is_for_mic);
                    }
                    if (!__dv_is_contiguous(dvp)) {
                        m_vars[i].flags.is_noncont_src = 1;
                        m_vars_extra[i].read_rng_src =
                            init_read_ranges_dv(dvp);
                    }

                    // size and displacement
                    if (VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.src)) {
                        // offset and length are derived from the
                        // array descriptor
                        __arr_data_offset_and_length(ap,
                                                     m_vars[i].disp,
                                                     m_vars[i].size);
                        if (m_vars[i].direction.bits) {
                            if (!is_arr_desc_contiguous(ap)) {
                                if (m_vars[i].flags.is_noncont_src) {
                                    LIBOFFLOAD_ERROR(c_slice_of_noncont_array);
                                    return false;
                                }
                                m_vars[i].flags.is_noncont_src = 1;
                                m_vars_extra[i].read_rng_src =
                                    init_read_ranges_arr_desc(ap);
                            }
                        }
                    }
                    else {
                        if (m_vars[i].flags.has_length) {
                            m_vars[i].size =
                                __dv_data_length(dvp, m_vars[i].count);
                        }
                        else {
                            m_vars[i].size = __dv_data_length(dvp);
                        }
                        m_vars[i].disp = 0;
                    }

                    // check that length >= 0
                    if (m_vars[i].alloc_if &&
                        (m_vars[i].disp + m_vars[i].size < 0)) {
                        LIBOFFLOAD_ERROR(c_zero_or_neg_ptr_len);
                        exit(1);
                    }

                    // base address
                    void *base = reinterpret_cast<void*>(dvp->Base);
                    PtrData *ptr_data;

                    // allocate buffer if we have no INTO and don't need
                    // allocation for the ptr at target
                    if (src_is_for_mic) {
                        if (m_vars[i].alloc_if) {
                            // add new entry
                            if (!alloc_ptr_data(
                                    ptr_data,
                                    reinterpret_cast<char *>(base) + alloc_disp,
                                    (alloc_base != NULL) ?
                                        alloc_disp : m_vars[i].disp,
                                    (alloc_base != NULL) ?
                                        alloc_size : m_vars[i].size,
                                    alloc_disp,
                                    (alloc_base != NULL) ?
                                        0 : m_vars[i].align,
                                    m_vars[i].flags.targetptr,
                                    m_vars[i].flags.preallocated,
                                    m_vars[i].flags.pin)) {
                                return false;
                            }

                            if (ptr_data->add_reference() == 0 &&
                                ptr_data->mic_buf != 0) {
                                // add buffer to the list of buffers
                                // that are passed to dispatch call
                                m_compute_buffers.push_back(
                                    ptr_data->mic_buf);
                            }
                            else {
                                // will send buffer address to device
                                m_vars[i].flags.sink_addr = 1;
                            }

                            if (!ptr_data->is_static) {
                                // need to add reference for buffer
                                m_need_runfunction = true;
                            }
                        }
                        else {
                            bool error_if_not_found = true;
                            if (m_is_openmp) {
                                // For omp target update variable is ignored
                                // if it does not exist.
                                if (m_vars[i].flags.always_copy ||
                                    (!m_vars[i].alloc_if &&
                                     !m_vars[i].free_if)) {
                                    error_if_not_found = false;
                                }
                            }

                            // use existing association from pointer table
                            if (!find_ptr_data(ptr_data,
                                               base,
                                               m_vars[i].disp,
                                               m_vars[i].size,
                                               m_vars[i].flags.targetptr,
                                               error_if_not_found)) {
                                return false;
                            }

                            if (m_is_openmp) {
                                // make var nocopy if it does not exist
                                if (ptr_data == 0) {
                                    m_vars[i].direction.bits =
                                        c_parameter_nocopy;
                                }
                            }

                            if (ptr_data != 0) {
                                // need to update base in dope vector on device
                                m_vars[i].flags.sink_addr = 1;
                            }
                        }

                        if (ptr_data != 0) {
                            if (m_is_openmp) {
                                // data is transferred if
                                // - if always modifier is used OR
                                // - if alloc_if == 0 && free_if == 0 OR
                                // - if reference count is 1
                                if (!m_vars[i].flags.always_copy &&
                                    (m_vars[i].alloc_if ||
                                     m_vars[i].free_if) &&
                                    ptr_data->get_reference() != 1) {
                                    m_vars[i].direction.bits =
                                        c_parameter_nocopy;
                                }
                            }

                            if (ptr_data->alloc_disp != 0) {
                                m_vars[i].flags.alloc_disp = 1;
                                m_in_datalen += sizeof(alloc_disp);
                            }

                            if (m_vars[i].flags.sink_addr) {
                                // get buffers's address on the sink
                                if (!init_mic_address(ptr_data)) {
                                    return false;
                                }

                                m_in_datalen += sizeof(ptr_data->mic_addr);
                            }

                            if (!ptr_data->is_static && m_vars[i].free_if) {
                                // need to decrement buffer reference on target
                                m_need_runfunction = true;
                            }

                            // offset to base from the beginning of the buffer
                            // memory
                            m_vars[i].offset =
                                (char*) base -
                                (char*) ptr_data->cpu_addr.start();

                            // copy other pointer properties to var descriptor
                            m_vars[i].mic_offset = ptr_data->mic_offset;
                            m_vars[i].flags.is_static = ptr_data->is_static;
                        }
                    }
                    else { // !src_is_for_mic
                        if (!find_ptr_data(ptr_data,
                                           base,
                                           m_vars[i].disp,
                                           m_vars[i].size,
                                           false, false)) {
                            return false;
                        }
                        m_vars[i].offset = !ptr_data ? 0 :
                                (char*) base -
                                (char*) ptr_data->cpu_addr.start();
                    }

                    // save pointer data
                    m_vars_extra[i].src_data = ptr_data;
                }
                break;

            default:
                LIBOFFLOAD_ERROR(c_unknown_var_type, m_vars[i].type.src);
                LIBOFFLOAD_ABORT;
        }
        if (m_vars[i].type.src == c_data_ptr_array) {
            continue;
        }

        if (src_is_for_mic && m_vars[i].flags.is_stack_buf) {
            m_vars[i].offset = static_cast<char*>(m_vars[i].ptr) -
                m_device.m_persist_list.front().cpu_stack_addr;
        }
        // if source is used at CPU save its offset and disp
        if (m_vars[i].into == NULL || m_vars[i].direction.in) {
            m_vars_extra[i].cpu_offset = m_vars[i].offset;
            m_vars_extra[i].cpu_disp   = m_vars[i].disp;
        }

        // If "into" is define we need to do the similar work for it
        if (!m_vars[i].into) {
            continue;
        }

        int64_t into_disp =0, into_offset = 0;

        switch (m_vars[i].type.dst) {
            case c_data_ptr_array:
                break;
            case c_data:
            case c_void_ptr:
            case c_cean_var: {
                int64_t size = m_vars[i].size;

                if (m_vars[i].type.dst == c_cean_var) {
                    // array descriptor
                    const Arr_Desc *ap =
                        static_cast<const Arr_Desc*>(m_vars[i].into);

                    // debug dump
                    ARRAY_DESC_DUMP("    ", "INTO", ap, 0, src_is_for_mic);

                    // offset and length are derived from the array descriptor
                    __arr_data_offset_and_length(ap, into_disp, size);

                    if (!is_arr_desc_contiguous(ap)) {
                        m_vars[i].flags.is_noncont_dst = 1;
                        m_vars_extra[i].read_rng_dst =
                            init_read_ranges_arr_desc(ap);
                        if (!cean_ranges_match(
                            m_vars_extra[i].read_rng_src,
                            m_vars_extra[i].read_rng_dst)) {
                            LIBOFFLOAD_ERROR(c_ranges_dont_match);
                            exit(1);
                        }
                    }
                    m_vars[i].into = reinterpret_cast<void*>(ap->base);
                }

                int64_t size_src = m_vars_extra[i].read_rng_src ?
                    cean_get_transf_size(m_vars_extra[i].read_rng_src) :
                    m_vars[i].size;
                int64_t size_dst = m_vars_extra[i].read_rng_dst ?
                    cean_get_transf_size(m_vars_extra[i].read_rng_dst) :
                    size;
                // It's supposed that "into" size must be not less
                // than src size
                if (size_src > size_dst) {
                    LIBOFFLOAD_ERROR(c_different_src_and_dstn_sizes,
                                     size_src, size_dst);
                    exit(1);
                }

                if (m_vars[i].direction.bits) {
                    if (m_vars[i].flags.is_static_dstn) {
                        PtrData *ptr_data;

                        // find data associated with variable
                        if (!find_ptr_data(ptr_data, m_vars[i].into,
                                           into_disp, size, false, false)) {
                            return false;
                        }
                        if (ptr_data != 0) {
                            // offset to base from the beginning of the buffer
                            // memory
                            into_offset =
                                (char*) m_vars[i].into -
                                (char*) ptr_data->cpu_addr.start();
                        }
                        else {
                            m_vars[i].flags.is_static_dstn = false;
                        }
                        m_vars_extra[i].dst_data = ptr_data;
                    }
                }

                if (m_vars[i].direction.in &&
                    !m_vars[i].flags.is_static_dstn) {
                    m_in_datalen += m_vars[i].size;

                    // for non-static target destination defined as CEAN
                    // expression we pass to target its size and dist
                    if (m_vars[i].type.dst == c_cean_var) {
                        m_in_datalen += 2 * sizeof(uint64_t);
                    }
                    m_need_runfunction = true;
                }
                break;
            }

            case c_dv:
                if (m_vars[i].direction.bits ||
                    m_vars[i].alloc_if ||
                    m_vars[i].free_if) {
                    ArrDesc *dvp = static_cast<ArrDesc*>(m_vars[i].into);

                    // debug dump
                    __dv_desc_dump("INTO", dvp);

                    // send dope vector contents excluding base
                    m_in_datalen += m_vars[i].size - sizeof(uint64_t);
                    m_need_runfunction = true;
                }
                break;

            case c_string_ptr:
            case c_data_ptr:
            case c_cean_var_ptr:
            case c_dv_ptr: {
                int64_t size = m_vars[i].size;

                if (m_vars[i].type.dst == c_cean_var_ptr) {
                    // array descriptor
                    const Arr_Desc *ap =
                        static_cast<const Arr_Desc*>(m_vars[i].into);

                    // debug dump
                    ARRAY_DESC_DUMP("    ", "INTO", ap, 1, src_is_for_mic);

                    // offset and length are derived from the array descriptor
                    __arr_data_offset_and_length(ap, into_disp, size);

                    if (!is_arr_desc_contiguous(ap)) {
                        m_vars[i].flags.is_noncont_src = 1;
                        m_vars_extra[i].read_rng_dst =
                            init_read_ranges_arr_desc(ap);
                        if (!cean_ranges_match(
                            m_vars_extra[i].read_rng_src,
                            m_vars_extra[i].read_rng_dst)) {
                            LIBOFFLOAD_ERROR(c_ranges_dont_match);
                        }
                    }
                    m_vars[i].into = reinterpret_cast<char**>(ap->base);
                }
                else if (m_vars[i].type.dst == c_dv_ptr) {
                    // need to send DV to the device unless it is 'nocopy'
                    if (m_vars[i].direction.bits ||
                        m_vars[i].alloc_if ||
                        m_vars[i].free_if) {
                        ArrDesc *dvp = *static_cast<ArrDesc**>(m_vars[i].into);

                        // debug dump
                        __dv_desc_dump("INTO", dvp);

                        m_vars[i].direction.bits = c_parameter_in;
                    }
                }

                int64_t size_src = m_vars_extra[i].read_rng_src ?
                    cean_get_transf_size(m_vars_extra[i].read_rng_src) :
                    m_vars[i].size;
                int64_t size_dst = m_vars_extra[i].read_rng_dst ?
                    cean_get_transf_size(m_vars_extra[i].read_rng_dst) :
                    size;
                // It's supposed that "into" size must be not less than
                // src size
                if (size_src > size_dst) {
                    LIBOFFLOAD_ERROR(c_different_src_and_dstn_sizes,
                                     size_src, size_dst);
                    exit(1);
                }

                if (m_vars[i].direction.bits) {
                    PtrData *ptr_data;

                    // base address
                    void *base = *static_cast<void**>(m_vars[i].into);

                    if (m_vars[i].direction.in) {
                        // allocate buffer
                        if (m_vars[i].flags.is_stack_buf) {
                            // for stack persistent objects ptr data is created
                            // by var_desc with number 0.
                            // Its ptr_data is stored at m_stack_ptr_data
                            ptr_data = m_stack_ptr_data;
                            m_vars[i].flags.sink_addr = 1;
                        }
                        else if (m_vars[i].alloc_if) {
                            if (m_vars[i].flags.preallocated) {
                                m_out_datalen += sizeof(void*);
                                m_need_runfunction = true;
                                break;
                            }
                            // add new entry
                            if (!alloc_ptr_data(
                                    ptr_data,
                                    reinterpret_cast<char *>(base) + alloc_disp,
                                    (alloc_base != NULL) ?
                                        alloc_disp : into_disp,
                                    (alloc_base != NULL) ?
                                        alloc_size : size,
                                    alloc_disp,
                                    (alloc_base != NULL) ?
                                        0 : m_vars[i].align,
                                    m_vars[i].flags.targetptr,
                                    m_vars[i].flags.preallocated,
                                    m_vars[i].flags.pin)) {
                                return false;
                            }
                            if (m_vars[i].flags.targetptr) {
                                if (!init_mic_address(ptr_data)) {
                                    return false;
                                }
                                *static_cast<void**>(m_vars[i].into) = base =
                                    reinterpret_cast<void*>(ptr_data->mic_addr);
                            }
                            if (ptr_data->add_reference() == 0 &&
                                ptr_data->mic_buf != 0) {
                                // add buffer to the list of buffers that
                                // are passed to dispatch call
                                m_compute_buffers.push_back(
                                    ptr_data->mic_buf);
                            }
                            else {
                                // will send buffer address to device
                                m_vars[i].flags.sink_addr = 1;
                            }

                            if (!ptr_data->is_static) {
                                // need to add reference for buffer
                                m_need_runfunction = true;
                            }
                        }
                        else {
                            // use existing association from pointer table
                            if (!find_ptr_data(ptr_data, base, into_disp,
                                    size, m_vars[i].flags.targetptr, true)) {
                                return false;
                            }
                            m_vars[i].flags.sink_addr = 1;
                        }

                        if (ptr_data->alloc_disp != 0) {
                            m_vars[i].flags.alloc_disp = 1;
                            m_in_datalen += sizeof(alloc_disp);
                        }

                        if (m_vars[i].flags.sink_addr) {
                            // get buffers's address on the sink
                            if (!init_mic_address(ptr_data)) {
                                return false;
                            }

                            m_in_datalen += sizeof(ptr_data->mic_addr);
                        }

                        if (!ptr_data->is_static && m_vars[i].free_if) {
                            // need to decrement buffer reference on target
                            m_need_runfunction = true;
                        }

                        // copy other pointer properties to var descriptor
                        m_vars[i].mic_offset = ptr_data->mic_offset;
                        m_vars[i].flags.is_static_dstn = ptr_data->is_static;
                    }
                    else {
                        if (!find_ptr_data(ptr_data,
                                           base,
                                           into_disp,
                                           m_vars[i].size,
                                           false, false)) {
                            return false;
                        }
                    }
                    if (ptr_data) {
                        into_offset = ptr_data ?
                            (char*) base -
                            (char*) ptr_data->cpu_addr.start() :
                            0;
                    }
                    // save pointer data
                    m_vars_extra[i].dst_data = ptr_data;
                }
                break;
            }

            case c_func_ptr:
                break;

            case c_dv_data:
            case c_dv_ptr_data:
            case c_dv_data_slice:
            case c_dv_ptr_data_slice:
                if (m_vars[i].direction.bits ||
                    m_vars[i].alloc_if ||
                    m_vars[i].free_if) {
                    const Arr_Desc *ap;
                    ArrDesc *dvp;
                    PtrData *ptr_data;
                    int64_t disp;
                    int64_t size;

                    if (VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.dst)) {
                        ap = static_cast<const Arr_Desc*>(m_vars[i].into);

                        // debug dump
                        ARRAY_DESC_DUMP("    ", "INTO", ap, 0, src_is_for_mic);

                        dvp = (m_vars[i].type.dst == c_dv_data_slice) ?
                              reinterpret_cast<ArrDesc*>(ap->base) :
                              *reinterpret_cast<ArrDesc**>(ap->base);
                    }
                    else {
                        dvp = (m_vars[i].type.dst == c_dv_data) ?
                              static_cast<ArrDesc*>(m_vars[i].into) :
                              *static_cast<ArrDesc**>(m_vars[i].into);
                    }
                    if (!__dv_is_contiguous(dvp)) {
                        m_vars[i].flags.is_noncont_dst = 1;
                        m_vars_extra[i].read_rng_dst =
                            init_read_ranges_dv(dvp);
                    }
                    // size and displacement
                    if (VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.dst)) {
                        // offset and length are derived from the array
                        // descriptor
                        __arr_data_offset_and_length(ap, into_disp, size);
                        if (m_vars[i].direction.bits) {
                            if (!is_arr_desc_contiguous(ap)) {
                                if (m_vars[i].flags.is_noncont_dst) {
                                    LIBOFFLOAD_ERROR(c_slice_of_noncont_array);
                                    return false;
                                }
                                m_vars[i].flags.is_noncont_dst = 1;
                                m_vars_extra[i].read_rng_dst =
                                    init_read_ranges_arr_desc(ap);
                                if (!cean_ranges_match(
                                    m_vars_extra[i].read_rng_src,
                                    m_vars_extra[i].read_rng_dst)) {
                                    LIBOFFLOAD_ERROR(c_ranges_dont_match);
                                }
                            }
                        }
                    }
                    else {
                        if (m_vars[i].flags.has_length) {
                            size = __dv_data_length(dvp, m_vars[i].count);
                        }
                        else {
                            size = __dv_data_length(dvp);
                        }
                        disp = 0;
                    }

                    int64_t size_src =
                        m_vars_extra[i].read_rng_src ?
                        cean_get_transf_size(m_vars_extra[i].read_rng_src) :
                        m_vars[i].size;
                    int64_t size_dst =
                        m_vars_extra[i].read_rng_dst ?
                        cean_get_transf_size(m_vars_extra[i].read_rng_dst) :
                        size;
                    // It's supposed that "into" size must be not less
                    // than src size
                    if (size_src > size_dst) {
                        LIBOFFLOAD_ERROR(c_different_src_and_dstn_sizes,
                            size_src, size_dst);
                        exit(1);
                    }

                    // base address
                    void *base = reinterpret_cast<void*>(dvp->Base);

                    // allocate buffer
                    if (m_vars[i].direction.in) {
                        if (m_vars[i].alloc_if) {
                            // add new entry
                            if (!alloc_ptr_data(
                                    ptr_data,
                                    reinterpret_cast<char *>(base) + alloc_disp,
                                    (alloc_base != NULL) ?
                                        alloc_disp : into_disp,
                                    (alloc_base != NULL) ?
                                        alloc_size : size,
                                    alloc_disp,
                                    (alloc_base != NULL) ?
                                        0 : m_vars[i].align,
                                    m_vars[i].flags.targetptr,
                                    m_vars[i].flags.preallocated,
                                    m_vars[i].flags.pin)) {
                                return false;
                            }
                            if (ptr_data->add_reference() == 0 &&
                                ptr_data->mic_buf !=0) {
                                // add buffer to the list of buffers
                                // that are passed to dispatch call
                                m_compute_buffers.push_back(
                                    ptr_data->mic_buf);
                            }
                            else {
                                // will send buffer address to device
                                m_vars[i].flags.sink_addr = 1;
                            }

                            if (!ptr_data->is_static) {
                                // need to add reference for buffer
                                m_need_runfunction = true;
                            }
                        }
                        else {
                            // use existing association from pointer table
                            if (!find_ptr_data(ptr_data, base, into_disp,
                                size, m_vars[i].flags.targetptr, true)) {
                                return false;
                            }

                            // need to update base in dope vector on device
                            m_vars[i].flags.sink_addr = 1;
                        }

                        if (ptr_data->alloc_disp != 0) {
                            m_vars[i].flags.alloc_disp = 1;
                            m_in_datalen += sizeof(alloc_disp);
                        }

                        if (m_vars[i].flags.sink_addr) {
                            // get buffers's address on the sink
                            if (!init_mic_address(ptr_data)) {
                                return false;
                            }
                            m_in_datalen += sizeof(ptr_data->mic_addr);
                        }

                        if (!ptr_data->is_static && m_vars[i].free_if) {
                            // need to decrement buffer reference on target
                            m_need_runfunction = true;
                        }

                        // offset to base from the beginning of the buffer
                        // memory
                        into_offset =
                            (char*) base - (char*) ptr_data->cpu_addr.start();

                        // copy other pointer properties to var descriptor
                        m_vars[i].mic_offset = ptr_data->mic_offset;
                        m_vars[i].flags.is_static_dstn = ptr_data->is_static;
                    }
                    else { // src_is_for_mic
                        if (!find_ptr_data(ptr_data,
                                           base,
                                           into_disp,
                                           size,
                                           false, false)) {
                            return false;
                        }
                        into_offset = !ptr_data ?
                            0 :
                            (char*) base - (char*) ptr_data->cpu_addr.start();
                    }

                    // save pointer data
                    m_vars_extra[i].dst_data = ptr_data;
                }
                break;

            default:
                LIBOFFLOAD_ERROR(c_unknown_var_type, m_vars[i].type.src);
                LIBOFFLOAD_ABORT;
        }
        // if into is used at CPU save its offset and disp
        if (m_vars[i].direction.out) {
            m_vars_extra[i].cpu_offset = into_offset;
            m_vars_extra[i].cpu_disp   = into_disp;
        }
        else {
            if (m_vars[i].flags.is_stack_buf) {
                into_offset = static_cast<char*>(m_vars[i].into) -
                    m_device.m_persist_list.front().cpu_stack_addr;
            }
            m_vars[i].offset = into_offset;
            m_vars[i].disp   = into_disp;
        }
    }

    return true;
}

bool OffloadDescriptor::setup_misc_data(const char *name)
{
    OffloadTimer timer(get_timer_data(), c_offload_host_setup_misc_data);

    // we can skip run functon call together with wait if offloaded
    // region is empty and there is no user defined non-pointer IN/OUT data
    if (m_need_runfunction) {
        // variable descriptors are sent as input data
        m_in_datalen += m_vars_total * sizeof(VarDesc);

        // timer data is sent as a part of the output data
        m_out_datalen += OFFLOAD_TIMER_DATALEN();

        // max from input data and output data length
        uint64_t data_len = m_in_datalen > m_out_datalen ? m_in_datalen :
                                                           m_out_datalen;

        // Misc data has the following layout
        //     <Function Descriptor>
        //     <Function Name>
        //     <In/Out Data>            (optional)
        //
        // We can transfer copyin/copyout data in misc/return data which can
        // be passed to run function call if its size does not exceed
        // COI_PIPELINE_MAX_IN_MISC_DATA_LEN. Otherwise we have to allocate
        // buffer for it.

        m_func_desc_size = sizeof(FunctionDescriptor) + strlen(name) + 1;
        m_func_desc_size = (m_func_desc_size + 7) & ~7;

        int misc_data_offset = 0;
        int misc_data_size = 0;
        if (data_len > 0) {
            if (m_func_desc_size +
                m_in_datalen <= COI_PIPELINE_MAX_IN_MISC_DATA_LEN &&
                m_out_datalen <= COI_PIPELINE_MAX_IN_MISC_DATA_LEN) {
                // use misc/return data for copyin/copyout
                misc_data_offset = m_func_desc_size;
                misc_data_size = data_len;
            }
            else {
                OffloadTimer timer_buf(get_timer_data(),
                                       c_offload_host_alloc_data_buffer);

                // send/receive data using buffer
                COIRESULT res = COI::BufferCreate(data_len,
                                                  COI_BUFFER_NORMAL,
                                                  0, 0,
                                                  1, &m_device.get_process(),
                                                  &m_inout_buf);
                if (res != COI_SUCCESS) {
                    if (m_status != 0) {
                        m_status->result = translate_coi_error(res);
                        return false;
                    }
                    report_coi_error(c_buf_create, res);
                }

                m_compute_buffers.push_back(m_inout_buf);
                m_destroy_buffers.push_back(m_inout_buf);
            }
        }

        // initialize function descriptor
        m_func_desc = (FunctionDescriptor*) calloc(1, m_func_desc_size
						      + misc_data_size);
        if (m_func_desc == NULL)
          LIBOFFLOAD_ERROR(c_malloc);
        m_func_desc->console_enabled = console_enabled;
        m_func_desc->timer_enabled = offload_report_enabled &&
            (timer_enabled || offload_report_level);
        m_func_desc->offload_report_level = offload_report_enabled ?
                                              offload_report_level : 0;
        m_func_desc->offload_number = GET_OFFLOAD_NUMBER(get_timer_data());
        m_func_desc->in_datalen = m_in_datalen;
        m_func_desc->out_datalen = m_out_datalen;
        m_func_desc->vars_num = m_vars_total;
        m_func_desc->data_offset = misc_data_offset;

        // append entry name
        strcpy(m_func_desc->data, name);
    }

    return true;
}

void OffloadDescriptor::setup_omp_async_info()
{
    OFFLOAD_TRACE(2, "setup_omp_async_info\n");
    OmpAsyncLastEventType event_type = m_need_runfunction ?
                                   c_last_runfunc : c_last_write;
    int last_in = m_need_runfunction ? 0 : -1;
    int i;

    for (i = m_vars_total - 1; i >=0; i--) {
                switch (m_vars[i].type.dst) {
                    case c_data:
                    case c_void_ptr:
                    case c_cean_var:
                        if (m_vars[i].direction.out &&
                            m_vars[i].flags.is_static_dstn) {
                            event_type = c_last_read;
                        }
                        else if (last_in < 0 && m_vars[i].direction.in &&
                            m_vars[i].flags.is_static_dstn) {
                            last_in = i;
                        }
                        break;
                    case c_string_ptr:
                    case c_data_ptr:
                    case c_cean_var_ptr:
                    case c_dv_ptr:
                    case c_dv_data:
                    case c_dv_ptr_data:
                    case c_dv_data_slice:
                    case c_dv_ptr_data_slice:
                    
                        if (m_vars[i].direction.out) {                        
                            event_type = c_last_read;
                        }
                        else if (last_in < 0 && m_vars[i].direction.in) {
                            last_in = i;
                        }
                        break;
                    default:
                        break;
                }
             if (event_type == c_last_read) {
                 break;
             }
        }
        
        if (event_type == c_last_read) {
            m_vars_extra[i].omp_last_event_type = c_last_read;
        }
        else if (event_type == c_last_write) {
            m_vars_extra[last_in].omp_last_event_type = c_last_write;        
        }
        m_omp_async_last_event_type = event_type;
    OFFLOAD_TRACE(2, "setup_omp_async_info: event_type=%d\n",
                  m_omp_async_last_event_type);
}

extern "C" {
    void offload_proxy_task_completed_ooo(
        COIEVENT e,
        const COIRESULT r,
        const void *info
    )
    {
	task_completion_callback ((void *) info);
    }
}

void OffloadDescriptor::register_omp_event_call_back(
    const COIEVENT *event,
    const void *info)
{
    OFFLOAD_TRACE(2, "register_omp_event_call_back(event=%p, info=%p)\n",
                  event, info);
    if (COI::EventRegisterCallback) {
        COI::EventRegisterCallback(
                 *event,
                 &offload_proxy_task_completed_ooo,
                 info, 0);
        OFFLOAD_TRACE(2,
            "COI::EventRegisterCallback found; callback registered\n");
    }
}
        
bool OffloadDescriptor::wait_dependencies(
    const void    **waits,
    int             num_waits,
    _Offload_stream handle
)
{
    OffloadTimer timer(get_timer_data(), c_offload_host_wait_deps);
    bool ret = true;
    OffloadDescriptor *task;
    if (num_waits == 0) {
        return true;
    }

    // wait for streams
    if (num_waits == -1) {
        Stream * stream;
        // some specific stream of the device
        if (handle != 0) {
            stream = Stream::find_stream(handle, false);

            // the stream was not created or was destroyed
            if (!stream) {
                LIBOFFLOAD_ERROR(c_offload_no_stream, m_device.get_logical_index());
                LIBOFFLOAD_ABORT;
            }
            task = stream->get_last_offload();

            // offload was completed by previous offload_wait pragma
            // or wait clause
            if (task == 0) {
                return true;
            }
            if (!task->offload_finish(0)) { //arg is 0 for is_traceback
                ret = false;
            }
            task->cleanup();
            stream->set_last_offload(NULL);
            delete task;
        }
        // all streams of the device or over all devices
        else {
            StreamMap stream_map = Stream::all_streams;
            for (StreamMap::iterator it = stream_map.begin();
                it != stream_map.end(); it++) {
                Stream * stream = it->second;

                if (!m_wait_all_devices &&
                    stream->get_device() != m_device.get_logical_index()) {
                    continue;
                }
                // get associated async task
                OffloadDescriptor *task = stream->get_last_offload();

                // offload was completed by offload_wait pragma or wait clause
                if (task == 0) {
                    continue;
                }
                if (!task->offload_finish(0)) { //arg is 0 for is_traceback
                    ret = false;
                }
                task->cleanup();
                stream->set_last_offload(NULL);
                delete task;
            }
            // no uncompleted streams
            return true;
        }
    }
    else {
        // if handle is equal to no_stream it's wait for signals
        for (int i = 0; i < num_waits; i++) {
            _Offload_stream stream_handle;
            Stream *stream;
            task = m_device.find_signal(waits[i], true);
            if (task == 0) {
                LIBOFFLOAD_ERROR(c_offload1, m_device.get_logical_index(),
                    waits[i]);
                LIBOFFLOAD_ABORT;
            }
            else if (task == SIGNAL_IS_REMOVED) {
                continue;
            }
            if (!task->offload_finish(0)) { //arg is 0 for is_traceback
                ret = false;
            }
            task->cleanup();
            // if the offload both has signal and is last offload of its
            // stream, we must wipe out the "last_offload" reference as
            // the offload already is finished.
            stream_handle = task->m_stream;
            if (stream_handle != -1) {
                stream = Stream::find_stream(stream_handle, false);
                if (stream && stream->get_last_offload() == task) {
                    stream->set_last_offload(NULL);
                }
            }
            delete task;
        }
    }
    return ret;
}

bool OffloadDescriptor::offload_wrap(
    const char *name,
    bool is_empty,
    VarDesc *vars,
    VarDesc2 *vars2,
    int vars_total,
    const void **waits,
    int num_waits,
    const void **signal,
    int entry_id,
    const void *stack_addr,
    OffloadFlags offload_flags
)
{
    OffloadWaitKind wait_kind = c_offload_wait_signal;
    bool is_traceback = offload_flags.bits.fortran_traceback;

    // define kind of wait if any;
    // there can be one off the following kind:
    // 1. c_offload_wait_signal for "offload_wait wait(signal)"
    // 2. c_offload_wait_stream for "offload_wait stream(stream)"
    // 3. c_offload_wait_all_streams for "offload_wait stream(0)"
    if (num_waits == -1) {
        wait_kind = (m_stream == 0) ?
                    c_offload_wait_all_streams :
                    c_offload_wait_stream;
    }
    char buf[35];
    const char *stream_str;

    if (m_stream == no_stream || num_waits >= 0) {
        stream_str = "none";
    }
    else if (m_stream == 0) {
        stream_str = "all";
    }
    else {
        sprintf(buf, "%#llx", m_stream);
        stream_str = buf;
    }

    if (signal == 0) {
        OFFLOAD_DEBUG_TRACE_1(1,
                      GET_OFFLOAD_NUMBER(get_timer_data()),
                      c_offload_init_func,
                      "Offload function %s, is_empty=%d, #varDescs=%d, "
                      "signal=none, stream=%s, #waits=%d%c",
                      name, is_empty, vars_total, stream_str, num_waits,
                      num_waits == 0 ? '\n' : ' ');
        // Breaks the norm of using OFFLOAD_DEBUG_TRACE to print the waits
        // since the number of waits is not fixed.
        if (!OFFLOAD_DO_TRACE && (console_enabled >= 1)) {
            if (num_waits) {
                printf("(");
                if (m_stream == no_stream) {
                    printf("%p", waits[0]);
                    for (int i = 1; i < num_waits; i++) {
                        printf(", %p", waits[i]);
                    }
                }
                else if (m_stream != 0) {
                    printf("%#x", m_stream);
                }
                else {
                    printf(" all streams");
                }
                printf(")");
            }
            printf("\n");
            fflush(NULL);
        }
        // stream in wait is reported further in OFFLOAD_REPORT for waits
        if (m_stream != no_stream && num_waits == 0) {
            OFFLOAD_REPORT(3, GET_OFFLOAD_NUMBER(get_timer_data()),
                           c_offload_stream,
                           "%d\n", m_stream);
        }
        OFFLOAD_REPORT(3, GET_OFFLOAD_NUMBER(get_timer_data()),
                      c_offload_signal,
                      "none %d\n", 0);
    }
    else {
        OFFLOAD_DEBUG_TRACE_1(1,
                      GET_OFFLOAD_NUMBER(get_timer_data()),
                      c_offload_init_func,
                      "Offload function %s, is_empty=%d, #varDescs=%d, "
                      "signal=%p, stream=%s, #waits=%d%c",
                      name, is_empty, vars_total, *signal, stream_str, num_waits,
                      num_waits == 0 ? '\n' : ' ');
        // Breaks the norm of using OFFLOAD_DEBUG_TRACE to print the waits
        // since the number of waits is not fixed.
        if (!OFFLOAD_DO_TRACE && (console_enabled >= 1)) {
            if (num_waits) {
                printf("(");
                if (m_stream == no_stream) {
                    printf("%p", waits[0]);
                    for (int i = 1; i < num_waits; i++) {
                        printf(", %p", waits[i]);
                    }
                    printf(")");
                }
                else if (m_stream != 0) {
                    printf("%#x", m_stream);
                }
                else {
                    printf(" all streams");
                }
                printf(")");
            }
            printf("\n");
            fflush(NULL);
        }
        // stream in wait is reported further in OFFLOAD_REPORT for waits
        if (m_stream != no_stream && num_waits == 0) {
            OFFLOAD_REPORT(3, GET_OFFLOAD_NUMBER(get_timer_data()),
                           c_offload_stream,
                           "%d\n", m_stream);
        }
        OFFLOAD_REPORT(3, GET_OFFLOAD_NUMBER(get_timer_data()),
                      c_offload_signal,
                      "%d\n", signal);
    }
    if (console_enabled >= 1 && offload_flags.flags != 0) {
        trace_offload_flags(get_timer_data(), offload_flags);
    }

    OFFLOAD_REPORT(3, GET_OFFLOAD_NUMBER(get_timer_data()),
                   c_offload_wait, "%d\n",
                   wait_kind, num_waits,
                   (wait_kind == c_offload_wait_signal) ?
                   waits :
                   reinterpret_cast<const void **>(m_stream));

    if (m_status != 0) {
        m_status->result = OFFLOAD_SUCCESS;
        m_status->device_number = m_device.get_logical_index();
    }

    m_initial_need_runfunction = m_need_runfunction = !is_empty;

    // wait for dependencies to finish
    if (!wait_dependencies(waits, num_waits, m_stream)) {
        cleanup();
        return false;
    }

    // setup buffers
    if (!setup_descriptors(vars, vars2, vars_total, entry_id, stack_addr)) {
        cleanup();
        return false;
    }

    if (offload_flags.bits.omp_async) {
        setup_omp_async_info();
    }
    
    // initiate send for pointers. Want to do it as early as possible.
    if (!send_pointer_data(signal != 0 || offload_flags.bits.omp_async,
                           signal)) {
        cleanup();
        return false;
    }

    // setup misc data for run function
    if (!setup_misc_data(name)) {
        cleanup();
        return false;
    }

    // gather copyin data into buffer
    if (!gather_copyin_data()) {
        cleanup();
        return false;
    }

    // Start the computation
    if (!compute(signal)) {
        cleanup();
        return false;
    }

    // initiate receive for pointers
    if (!receive_pointer_data(signal != 0 || offload_flags.bits.omp_async,
                              true, signal)) {
        cleanup();
        return false;
    }
    if (offload_flags.bits.omp_async) {
        return true;
    }
    // if there is a signal or stream save descriptor for the later use.
    // num_waits == -1 is for offload_wait and there is nothing to save
    if (num_waits != -1 && (signal != 0 || m_stream != no_stream)) {
        if (signal != 0) {
            m_device.add_signal(*signal, this);
        }

        if (m_stream != no_stream && m_stream != 0) {
            Stream* stream = Stream::find_stream(m_stream, false);
            if (stream) {
                stream->set_last_offload(this);
            }
            else {
                LIBOFFLOAD_ERROR(c_offload_no_stream, m_device.get_logical_index());
                LIBOFFLOAD_ABORT;
            }
        }
        // if there is a clause with alloc_if(1) and preallocated need to call
        // offload_finish after runfunction
        if (!m_preallocated_alloc) {
            return true;
        }
    }

    // wait for the offload to finish.
    if (!offload_finish(is_traceback)) {
        cleanup();
        return false;
    }

    cleanup();
    return true;
}

bool OffloadDescriptor::offload(
    const char *name,
    bool is_empty,
    VarDesc *vars,
    VarDesc2 *vars2,
    int vars_total,
    const void **waits,
    int num_waits,
    const void **signal,
    int entry_id,
    const void *stack_addr,
    OffloadFlags offload_flags
)
{
    bool res;
    res = offload_wrap(name, is_empty, vars, vars2, vars_total,
                       waits, num_waits, signal, entry_id,
                       stack_addr, offload_flags);
    if (res == false && !m_traceback_called) {
        if (offload_flags.bits.fortran_traceback) {
            OFFLOAD_TRACE(3,
                "Calling Fortran library to continue traceback from MIC\n");
            FORTRAN_TRACE_BACK(m_status->result);
            m_traceback_called = true;
        }
    }
    return res;
}

bool OffloadDescriptor::offload_finish(
    bool is_traceback
)
{
    COIRESULT res;

    // wait for compute dependencies to become signaled
    if (m_in_deps_total > 0) {
        OffloadTimer timer(get_timer_data(), c_offload_host_wait_compute);

        if (__offload_active_wait) {
            // keep CPU busy
            do {
                res = COI::EventWait(m_in_deps_total, m_in_deps, 0, 1, 0, 0);
            }
            while (res == COI_TIME_OUT_REACHED);
        }
        else {
            res = COI::EventWait(m_in_deps_total, m_in_deps, -1, 1, 0, 0);
        }

        if (res != COI_SUCCESS) {
            if (m_status != 0 && !m_traceback_called) {
                m_status->result = translate_coi_error(res);
                if (is_traceback) {
                    OFFLOAD_TRACE(3,
                    "Calling Fortran library to continue traceback from MIC\n");
                    FORTRAN_TRACE_BACK(m_status->result);
                    m_traceback_called = true;
                }
                return false;
            }

            if (is_traceback && !m_traceback_called) {
                OFFLOAD_TRACE(3,
                  "Calling Fortran library to continue traceback from MIC\n");
                FORTRAN_TRACE_BACK(OFFLOAD_ERROR);
                m_traceback_called = true;
            }

            report_coi_error(c_event_wait, res);
        }
    }

    // scatter copyout data received from target
    if (!scatter_copyout_data()) {
        return false;
    }

    if (m_out_with_preallocated &&
        !receive_pointer_data(m_out_deps_total > 0, false, NULL)) {
        cleanup();
        return false;
    }

    // wait for receive dependencies to become signaled
    if (m_out_deps_total > 0) {
        OffloadTimer timer(get_timer_data(), c_offload_host_wait_buffers_reads);

        if (__offload_active_wait) {
            // keep CPU busy
            do {
                res = COI::EventWait(m_out_deps_total, m_out_deps, 0, 1, 0, 0);
            }
            while (res == COI_TIME_OUT_REACHED);
        }
        else {
            res = COI::EventWait(m_out_deps_total, m_out_deps, -1, 1, 0, 0);
        }

        if (res != COI_SUCCESS) {
            if (m_status != 0) {
                m_status->result = translate_coi_error(res);
                return false;
            }
            report_coi_error(c_event_wait, res);
        }
    }

    // destroy buffers
    {
        OffloadTimer timer(get_timer_data(), c_offload_host_destroy_buffers);

        for (BufferList::const_iterator it = m_destroy_buffers.begin();
             it != m_destroy_buffers.end(); it++) {
            res = COI::BufferDestroy(*it);
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_destroy, res);
            }
        }
    }

    return true;
}

void OffloadDescriptor::cleanup()
{
    // release device in orsl
    ORSL::release(m_device.get_logical_index());

    OFFLOAD_TIMER_STOP(get_timer_data(), c_offload_host_total_offload);

    // report stuff
    Offload_Report_Epilog(get_timer_data());
}

bool OffloadDescriptor::is_signaled()
{
    bool signaled = true;
    COIRESULT res;

    // check compute and receive dependencies
    if (m_in_deps_total > 0) {
        res = COI::EventWait(m_in_deps_total, m_in_deps, 0, 1, 0, 0);
        signaled = signaled && (res == COI_SUCCESS);
    }
    if (m_out_deps_total > 0) {
        res = COI::EventWait(m_out_deps_total, m_out_deps, 0, 1, 0, 0);
        signaled = signaled && (res == COI_SUCCESS);
    }

    return signaled;
}

static Arr_Desc * make_arr_desc(
    void*   ptr_val,
    int64_t extent_start_val,
    int64_t extent_elements_val,
    int64_t size
)
{
    Arr_Desc *res;
    res = (Arr_Desc *)malloc(sizeof(Arr_Desc));
    if (res == NULL)
      LIBOFFLOAD_ERROR(c_malloc);
    res->base = reinterpret_cast<int64_t>(ptr_val);
    res->rank = 1;
    res->dim[0].size = size;
    res->dim[0].lindex = 0;
    res->dim[0].lower = extent_start_val;
    res->dim[0].upper = extent_elements_val + extent_start_val - 1;
    res->dim[0].stride = 1;
    return res;
}

// Send pointer data if source or destination or both of them are
// noncontiguous. There is guarantee that length of destination enough for
// transferred data.
bool OffloadDescriptor::send_noncontiguous_pointer_data(
    int i,
    PtrData* src_data,
    PtrData* dst_data,
    COIEVENT *event,
    uint64_t &data_sent,
    uint32_t in_deps_amount,
    COIEVENT *in_deps
    )
{
    int64_t offset_src, offset_dst;
    int64_t length_src, length_dst;
    int64_t length_src_cur, length_dst_cur;
    int64_t send_size;
    COIRESULT res;
    bool dst_is_empty = true;
    bool src_is_empty = true;

    data_sent = 0;

    // Set length_src and length_dst
    length_src = (m_vars_extra[i].read_rng_src) ?
        m_vars_extra[i].read_rng_src->range_size : m_vars[i].size;
    length_dst = !m_vars[i].into ? length_src :
                     (m_vars_extra[i].read_rng_dst) ?
                     m_vars_extra[i].read_rng_dst->range_size : m_vars[i].size;
    send_size = (length_src < length_dst) ? length_src : length_dst;

    // If BufferWriteMultiD is defined we can set values of required arguments
    // and transfer noncontiguous data via call to the COI routine.
    if (__offload_use_coi_noncontiguous_transfer && COI::BufferWriteMultiD) {
        struct Arr_Desc* arr_desc_dst;
        struct Arr_Desc* arr_desc_src;
        int64_t size_src, size_dst;
        char *base = offload_get_src_base(static_cast<char*>(m_vars[i].ptr),
            m_vars[i].type.src);
        COIBUFFER dst_buf = m_vars[i].into ?
            m_vars_extra[i].dst_data->mic_buf :
            m_vars_extra[i].src_data->mic_buf;

        offset_src = (m_vars_extra[i].read_rng_src)?
            m_vars_extra[i].read_rng_src->init_offset : m_vars_extra[i].cpu_disp;
        size_src = m_vars_extra[i].read_rng_src ?
            cean_get_transf_size(m_vars_extra[i].read_rng_src) :
            m_vars[i].size;

        offset_dst = (m_vars_extra[i].read_rng_dst)?
            m_vars_extra[i].read_rng_dst->init_offset : m_vars[i].disp;
        size_dst = m_vars_extra[i].read_rng_dst ?
            cean_get_transf_size(m_vars_extra[i].read_rng_dst) : m_vars[i].size;

        int64_t el_size = (!m_vars[i].into ||
            (m_vars_extra[i].read_rng_src && m_vars_extra[i].read_rng_dst)) ?
            1 :
            m_vars_extra[i].read_rng_src ?
            m_vars_extra[i].read_rng_src->arr_desc->dim[
                m_vars_extra[i].read_rng_src->arr_desc->rank - 1].size :
            m_vars_extra[i].read_rng_dst->arr_desc->dim[
                m_vars_extra[i].read_rng_dst->arr_desc->rank - 1].size;

        arr_desc_src = (m_vars_extra[i].read_rng_src) ?
                m_vars_extra[i].read_rng_src->arr_desc :
                make_arr_desc(NULL, // don't required for source
                    offset_src/el_size, size_src/el_size, el_size);

        arr_desc_dst = !m_vars[i].into ?
                arr_desc_src :
                (m_vars_extra[i].read_rng_dst) ?
                    m_vars_extra[i].read_rng_dst->arr_desc :
                    make_arr_desc(NULL,
                        offset_dst/el_size, size_src/el_size, el_size);

        int64_t alloc_disp = m_vars[i].into ?
                    m_vars_extra[i].dst_data->alloc_disp :
                    m_vars_extra[i].src_data->alloc_disp;

        arr_desc_src->base = reinterpret_cast<int64_t>(base);
        arr_desc_dst->base = 0;

        res = COI::BufferWriteMultiD(
            dst_buf,              // in_DestBuffer,
            m_device.get_process(),      // DestProcess,
            m_vars[i].offset + m_vars[i].mic_offset -
            alloc_disp,           // Offset
            (void*)arr_desc_dst,         // descriptor of DestArray
            (void*)arr_desc_src,         // descriptor of SrcArray
            COI_COPY_UNSPECIFIED, // Type
            in_deps_amount,       // Number of in Dependencies
            in_deps,              // array of in Dependencies
            event);               // out Dependency
        if (res != COI_SUCCESS) {
            if (m_status != 0) {
                m_status->result = translate_coi_error(res);
                return false;
            }
            report_coi_error(c_buf_copy, res);
        }
        return(true);
    }

    // if event is defined we must multiplate it for all contiguous intervals
    // that will be Copied/Write.
    // Take in account that we already have 1 event.
    if (event) {
        m_in_deps_allocated += (length_src / send_size) *
                                ((m_vars_extra[i].read_rng_src) ?
                                m_vars_extra[i].read_rng_src->range_max_number : 1) ;
        m_in_deps    =
            (COIEVENT*)realloc(m_in_deps, sizeof(COIEVENT) * m_in_deps_allocated);
        m_in_deps_total--; 
    }

    // consequently get contiguous ranges,
    // define corresponded destination offset and send data
    do {
        if (src_is_empty) {
            if (m_vars_extra[i].read_rng_src) {
                if (!get_next_range(m_vars_extra[i].read_rng_src,
                         &offset_src)) {
                    // source ranges are over - nothing to send
                    break;
                }
            }
            else if (data_sent == 0) {
                offset_src = m_vars_extra[i].cpu_disp;
            }
            else {
                break;
            }
            length_src_cur = length_src;
        }
        else {
            // if source is contiguous or its contiguous range is greater
            // than destination one
            offset_src += send_size;
        }
        length_src_cur -= send_size;
        src_is_empty = length_src_cur == 0;

        if (dst_is_empty) {
            if (m_vars[i].into) {
                if (m_vars_extra[i].read_rng_dst) {
                    if (!get_next_range(m_vars_extra[i].read_rng_dst,
                             &offset_dst)) {
                        // destination ranges are over
                        LIBOFFLOAD_ERROR(c_destination_is_over);
                        return false;
                    }
                }
                // into is contiguous.
                else {
                    offset_dst = m_vars[i].disp;
                }
                length_dst_cur = length_dst;
            }
            // same as source
            else {
                offset_dst = offset_src;
                length_dst_cur = length_src;
            }
        }
        else {
            // if destination is contiguous or its contiguous range is greater
            // than source one
            offset_dst += send_size;
        }
        length_dst_cur -= send_size;
        dst_is_empty = length_dst_cur == 0;
        
        if (event) {
            event =  &m_in_deps[m_in_deps_total++];
        }
        if (src_data != 0 && src_data->cpu_buf != 0) {
            res = COI::BufferCopy(
                dst_data->mic_buf,
                src_data->cpu_buf,
                m_vars[i].mic_offset +
                m_vars[i].offset + offset_dst,
                m_vars_extra[i].cpu_offset + offset_src,
                send_size,
                COI_COPY_UNSPECIFIED,
                in_deps_amount, in_deps,
                event);
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_copy, res);
            }
        }
        else {
            char *base = offload_get_src_base(m_vars[i].ptr,
                m_vars[i].type.src);

            res = COI::BufferWrite(
                dst_data->mic_buf,
                m_vars[i].mic_offset +
                m_vars[i].offset + offset_dst,
                base + offset_src,
                send_size,
                COI_COPY_UNSPECIFIED,
                in_deps_amount, in_deps,
                event);
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_write, res);
            }
        }
        data_sent += send_size;
    }
    while (true);
    return true;
}

bool OffloadDescriptor::send_pointer_data(bool is_async, void* info)
{
    OffloadTimer timer(get_timer_data(), c_offload_host_send_pointers);

    bool should_use_async_buffer_write = m_initial_need_runfunction;
    uint64_t ptr_sent = 0;
    COIRESULT res;
    uint32_t in_deps_amount = 0;
    COIEVENT *in_deps = NULL;

    // For offload_transfer and offload with empty body without signal:
    // - if there is only one buffer copy - send data synchronously
    // - if there are multiple buffer copy and
    // __offload_parallel_copy is false - send data synchronously
    // - if there are multiple buffer copy and
    // __offload_parallel_copy is true - send data asynchronously
    // It concerns only big size data - greater than __offload_use_async_buffer_write.
    // Data of size less than __offload_use_async_buffer_write are sent synchronously.
    // Synchronous transfer results in better performance in COI.
    // __offload_parallel_copy is false by default but can be changed
    // via environment variable OFFLOAD_PARALLEL_COPY
    if (!m_initial_need_runfunction && __offload_parallel_copy) {
        int big_size_count = 0;
        for (int i = 0; i < m_vars_total; i++) {
            if (m_vars[i].direction.in &&
                m_vars[i].size >= __offload_use_async_buffer_write) {
                switch (m_vars[i].type.dst) {
                    case c_data:
                    case c_void_ptr:
                    case c_cean_var:
                        if (m_vars[i].flags.is_static_dstn) {
                            big_size_count++;
                        }
                        break;
                    case c_string_ptr:
                    case c_data_ptr:
                    case c_cean_var_ptr:
                    case c_dv_ptr:
                    case c_dv_data:
                    case c_dv_ptr_data:
                    case c_dv_data_slice:
                    case c_dv_ptr_data_slice:
                        big_size_count++;
                        break;
                    default:
                        break;
                }
            }
        }
        if (big_size_count > 1) {
            should_use_async_buffer_write = true;
        }
    }

    if (m_stream != no_stream && m_vars_total != 0) {
       get_stream_in_dependencies(in_deps_amount, in_deps);
    }

    // Initiate send for pointer data
    for (int i = 0; i < m_vars_total; i++) {
        uint64_t sent_data = m_vars[i].size;
        uint32_t in_deps_amount_save;
        COIEVENT *in_deps_save;
                
        if (m_vars_extra[i].omp_last_event_type == c_last_write) {
            in_deps_amount_save = in_deps_amount;
            in_deps_save = in_deps;
            in_deps_amount = m_in_deps_total;
            if (in_deps_amount > 0) {
               in_deps = (COIEVENT*) malloc(sizeof(COIEVENT) * in_deps_amount);
               if (in_deps == NULL)
                   LIBOFFLOAD_ERROR(c_malloc);
               memcpy(in_deps, m_in_deps,in_deps_amount * sizeof(COIEVENT));
            }                
        }
        switch (m_vars[i].type.dst) {
            case c_data_ptr_array:
                break;
            case c_data:
            case c_void_ptr:
            case c_cean_var:
                if (m_vars[i].direction.in &&
                    m_vars[i].flags.is_static_dstn) {
                    COIEVENT *event =
                        (is_async ||
                         (should_use_async_buffer_write &&
                          m_vars[i].size >= __offload_use_async_buffer_write)) ?
                        &m_in_deps[m_in_deps_total++] : 0;
                    PtrData* dst_data = m_vars[i].into ?
                                            m_vars_extra[i].dst_data :
                                            m_vars_extra[i].src_data;
                    PtrData* src_data =
                        VAR_TYPE_IS_PTR(m_vars[i].type.src) ||
                        VAR_TYPE_IS_SCALAR(m_vars[i].type.src) &&
                        m_vars[i].flags.is_static ?
                           m_vars_extra[i].src_data : 0;

                    if (m_vars[i].flags.is_noncont_src ||
                        m_vars[i].flags.is_noncont_dst) {
                        if (!send_noncontiguous_pointer_data(
                                i, src_data, dst_data, event, sent_data,
                                in_deps_amount, in_deps)) {
                            return false;
                        }
                    }
                    else if (src_data != 0 && src_data->cpu_buf != 0) {
                        res = COI::BufferCopy(
                            dst_data->mic_buf,
                            src_data->cpu_buf,
                            m_vars[i].mic_offset +
                            m_vars[i].offset + m_vars[i].disp,
                            m_vars_extra[i].cpu_offset +
                            m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount, in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_copy, res);
                        }
                    }
                    else {
                        char *base = offload_get_src_base(m_vars[i].ptr,
                                                          m_vars[i].type.src);
                        res = COI::BufferWrite(
                            dst_data->mic_buf,
                            m_vars[i].mic_offset +
                            m_vars[i].offset + m_vars[i].disp,
                            base + m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount, in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_write, res);
                        }
                    }
                    ptr_sent += sent_data;
                }
                break;

            case c_string_ptr:
            case c_data_ptr:
            case c_cean_var_ptr:
            case c_dv_ptr:
                if (m_vars[i].direction.in && m_vars[i].size > 0) {
                    COIEVENT *event =
                        (is_async ||
                         (should_use_async_buffer_write &&
                          m_vars[i].size >= __offload_use_async_buffer_write)) ?
                        &m_in_deps[m_in_deps_total++] : 0;
                    PtrData* dst_data = m_vars[i].into ?
                                            m_vars_extra[i].dst_data :
                                            m_vars_extra[i].src_data;
                    PtrData* src_data =
                        VAR_TYPE_IS_PTR(m_vars[i].type.src) ||
                        VAR_TYPE_IS_SCALAR(m_vars[i].type.src) &&
                        m_vars[i].flags.is_static ?
                            m_vars_extra[i].src_data : 0;

                    if (m_vars[i].flags.is_noncont_src ||
                        m_vars[i].flags.is_noncont_dst) {
                        send_noncontiguous_pointer_data(
                            i, src_data, dst_data, event, sent_data,
                            in_deps_amount, in_deps);
                    }
                    else if (src_data != 0 && src_data->cpu_buf != 0) {
                        res = COI::BufferCopy(
                            dst_data->mic_buf,
                            src_data->cpu_buf,
                            m_vars[i].mic_offset +
                            m_vars[i].offset + m_vars[i].disp,
                            m_vars_extra[i].cpu_offset +
                            m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount, in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_copy, res);
                        }
                    }
                    else {
                        char *base = offload_get_src_base(m_vars[i].ptr,
                                                          m_vars[i].type.src);
                        res = COI::BufferWrite(
                            dst_data->mic_buf,
                            m_vars[i].mic_offset +
                            m_vars[i].offset + m_vars[i].disp,
                            base + m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount, in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_write, res);
                        }
                    }

                    ptr_sent += sent_data;
                }
                break;

            case c_dv_data:
            case c_dv_ptr_data:
                if (m_vars[i].direction.in &&
                    m_vars[i].size > 0) {
                    PtrData *ptr_data = m_vars[i].into ?
                                        m_vars_extra[i].dst_data :
                                        m_vars_extra[i].src_data;
                    PtrData* src_data = m_vars_extra[i].src_data;

                    COIEVENT *event =
                        (is_async ||
                         (should_use_async_buffer_write &&
                          m_vars[i].size >= __offload_use_async_buffer_write)) ?
                        &m_in_deps[m_in_deps_total++] : 0;

                    if (m_vars[i].flags.is_noncont_src ||
                        m_vars[i].flags.is_noncont_dst) {
                        send_noncontiguous_pointer_data(
                            i, src_data, ptr_data, event, sent_data,
                            in_deps_amount, in_deps);
                    }
                    else if (src_data && src_data->cpu_buf != 0) {
                        res = COI::BufferCopy(
                            ptr_data->mic_buf,
                            src_data->cpu_buf,
                            m_vars[i].offset + ptr_data->mic_offset +
                            m_vars[i].disp,
                            m_vars_extra[i].cpu_offset +
                            m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount, in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_copy, res);
                        }
                    }
                    else {
                        char *base = offload_get_src_base(m_vars[i].ptr,
                                                          m_vars[i].type.src);
                        res = COI::BufferWrite(
                            ptr_data->mic_buf,
                            ptr_data->mic_offset +
                            m_vars[i].offset + m_vars[i].disp,
                            base + m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount, in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_write, res);
                        }
                    }
                    ptr_sent += sent_data;
                }
                break;

            case c_dv_data_slice:
            case c_dv_ptr_data_slice:
                if (m_vars[i].direction.in &&
                    m_vars[i].size > 0) {
                    PtrData *dst_data = m_vars[i].into ?
                                        m_vars_extra[i].dst_data :
                                        m_vars_extra[i].src_data;
                    PtrData* src_data =
                        (VAR_TYPE_IS_PTR(m_vars[i].type.src) ||
                        VAR_TYPE_IS_DV_DATA(m_vars[i].type.src) ||
                        VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.src) ||
                        VAR_TYPE_IS_SCALAR(m_vars[i].type.src) &&
                        m_vars[i].flags.is_static) ?
                            m_vars_extra[i].src_data : 0;
                    COIEVENT *event =
                        (is_async ||
                         (should_use_async_buffer_write &&
                          m_vars[i].size >= __offload_use_async_buffer_write)) ?
                        &m_in_deps[m_in_deps_total++] : 0;
                    if (m_vars[i].flags.is_noncont_src ||
                        m_vars[i].flags.is_noncont_dst) {
                        send_noncontiguous_pointer_data(
                            i, src_data, dst_data, event, sent_data,
                            in_deps_amount, in_deps);
                    }
                    else if (src_data && src_data->cpu_buf != 0) {
                        res = COI::BufferCopy(
                            dst_data->mic_buf,
                            src_data->cpu_buf,
                            m_vars[i].offset +
                            dst_data->mic_offset +
                            m_vars[i].disp,
                            m_vars_extra[i].cpu_offset +
                            m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount, in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_copy, res);
                        }
                    }
                    else {
                        char *base = offload_get_src_base(m_vars[i].ptr,
                                                          m_vars[i].type.src);
                        res = COI::BufferWrite(
                            dst_data->mic_buf,
                            dst_data->mic_offset +
                            m_vars[i].offset + m_vars[i].disp,
                            base + m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount, in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_write, res);
                        }
                    }

                    ptr_sent += sent_data;
                }
                break;

            default:
                break;
        }
        if (m_vars_extra[i].omp_last_event_type == c_last_write) {
            in_deps_amount = in_deps_amount_save;
            in_deps = in_deps_save;
            register_omp_event_call_back(&m_in_deps[m_in_deps_total - 1], info);                
        }
        // alloc field isn't used at target.
        // We can reuse it for offset of array pointers.
        if (m_vars_extra[i].is_arr_ptr_el) {
            m_vars[i].ptr_arr_offset = m_vars_extra[i].ptr_arr_offset;
        }
    }

    if (m_status) {
        m_status->data_sent += ptr_sent;
    }

    OFFLOAD_TIMER_HOST_SDATA(get_timer_data(), ptr_sent);
    OFFLOAD_DEBUG_TRACE_1(1, GET_OFFLOAD_NUMBER(get_timer_data()),
                  c_offload_sent_pointer_data,
                  "Total pointer data sent to target: [%lld] bytes\n",
                  ptr_sent);

    return true;
}

bool OffloadDescriptor::gather_copyin_data()
{
    OffloadTimer timer(get_timer_data(), c_offload_host_gather_inputs);

    if (m_need_runfunction && m_in_datalen > 0) {
        COIMAPINSTANCE map_inst;
        char *data;

        // init marshaller
        if (m_inout_buf != 0) {
            OffloadTimer timer_map(get_timer_data(),
                                   c_offload_host_map_in_data_buffer);

            COIRESULT res = COI::BufferMap(m_inout_buf, 0, m_in_datalen,
                                           COI_MAP_WRITE_ENTIRE_BUFFER,
                                           0, 0, 0, &map_inst,
                                           reinterpret_cast<void**>(&data));
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_map, res);
            }
        }
        else {
            data = (char*) m_func_desc + m_func_desc->data_offset;
        }

        // send variable descriptors
        memcpy(data, m_vars, m_vars_total * sizeof(VarDesc));
        data += m_vars_total * sizeof(VarDesc);

        // init marshaller
        m_in.init_buffer(data, m_in_datalen);

        // Gather copy data into buffer
        for (int i = 0; i < m_vars_total; i++) {
            bool src_is_for_mic = (m_vars[i].direction.out ||
                                   m_vars[i].into == NULL);
            PtrData* ptr_data = src_is_for_mic ?
                                m_vars_extra[i].src_data :
                                m_vars_extra[i].dst_data;
            if (m_vars[i].flags.alloc_disp) {
                m_in.send_data(&ptr_data->alloc_disp,
                               sizeof(ptr_data->alloc_disp));
            }

            // send sink address to the target
            if (m_vars[i].flags.sink_addr) {
                m_in.send_data(&ptr_data->mic_addr,
                               sizeof(ptr_data->mic_addr));
            }

            switch (m_vars[i].type.dst) {
                case c_data_ptr_array:
                    break;
                case c_data:
                case c_void_ptr:
                case c_cean_var:
                    if (m_vars[i].direction.in &&
                        !m_vars[i].flags.is_static_dstn) {

                        char *ptr = offload_get_src_base(m_vars[i].ptr,
                                                         m_vars[i].type.src);
                        if (m_vars[i].type.dst == c_cean_var) {
                            // offset and length are derived from the array
                            // descriptor
                            int64_t size = m_vars[i].size;
                            int64_t disp = m_vars[i].disp;
                            m_in.send_data(reinterpret_cast<char*>(&size),
                                           sizeof(int64_t));
                            m_in.send_data(reinterpret_cast<char*>(&disp),
                                           sizeof(int64_t));
                        }

                        m_in.send_data(ptr + m_vars_extra[i].cpu_disp,
                                       m_vars[i].size);
                    }
                    break;

                case c_dv:
                    if (m_vars[i].direction.bits ||
                        m_vars[i].alloc_if ||
                        m_vars[i].free_if) {
                        // send dope vector excluding base
                        char *ptr = static_cast<char*>(m_vars[i].ptr);
                        m_in.send_data(ptr + sizeof(uint64_t),
                                       m_vars[i].size - sizeof(uint64_t));
                    }
                    break;

                case c_data_ptr:
                    // send to target addresses of obsolete
                    // stacks to be released
                    if (m_vars[i].flags.is_stack_buf &&
                        !m_vars[i].direction.bits &&
                        m_vars[i].alloc_if &&
                        m_vars[i].size != 0) {
                        for (PtrDataList::iterator it =
                            m_destroy_stack.begin();
                            it != m_destroy_stack.end(); it++) {
                            PtrData * ptr_data = *it;
                            m_in.send_data(&(ptr_data->mic_addr),
                                sizeof(ptr_data->mic_addr));
                        }
                    }
                    break;
                case c_func_ptr:
                    if (m_vars[i].direction.in) {
                        m_in.send_func_ptr(*((const void**) m_vars[i].ptr));
                    }
                    break;

                default:
                    break;
            }
        }

        if (m_status) {
            m_status->data_sent += m_in.get_tfr_size();
        }

        if (m_func_desc->data_offset == 0) {
            OffloadTimer timer_unmap(get_timer_data(),
                                     c_offload_host_unmap_in_data_buffer);
            COIRESULT res = COI::BufferUnmap(map_inst, 0, 0, 0);
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_unmap, res);
            }
        }
    }

    OFFLOAD_TIMER_HOST_SDATA(get_timer_data(), m_in.get_tfr_size());
    OFFLOAD_DEBUG_TRACE_1(1,
                  GET_OFFLOAD_NUMBER(get_timer_data()), c_offload_copyin_data,
                  "Total copyin data sent to target: [%lld] bytes\n",
                  m_in.get_tfr_size());

    return true;
}

bool OffloadDescriptor::compute(void *info)
{
    OffloadTimer timer(get_timer_data(), c_offload_host_start_compute);

    if (m_need_runfunction) {
        OFFLOAD_DEBUG_TRACE_1(2, GET_OFFLOAD_NUMBER(get_timer_data()),
                              c_offload_compute, "Compute task on MIC\n");

        void* misc = m_func_desc;
        int   misc_len = m_func_desc_size;
        void* ret = 0;
        int   ret_len = 0;

        if (m_func_desc->data_offset != 0) {
            misc_len += m_in_datalen;

            if (m_out_datalen > 0) {
                ret = (char*) m_func_desc + m_func_desc->data_offset;
                ret_len = m_out_datalen;
            }
        }

        // dispatch task
        COIRESULT res;
        COIEVENT event;
        uint32_t in_deps_amount = m_in_deps_total;
        COIEVENT *in_deps = m_in_deps_total > 0 ? m_in_deps : 0;

        if (0 == m_in_deps_total && m_stream != no_stream) {
            get_stream_in_dependencies(in_deps_amount, in_deps);
        }

        res = m_device.compute(m_stream,
                               m_compute_buffers,
                               misc, misc_len,
                               ret, ret_len,
                               in_deps_amount,
                               in_deps,
                               &event);

        if (res != COI_SUCCESS) {
            if (m_status != 0) {
                m_status->result = translate_coi_error(res);
                return false;
            }
            report_coi_error(c_pipeline_run_func, res);
        }

        if (m_omp_async_last_event_type == c_last_runfunc) {
            register_omp_event_call_back(&event, info);
        }

        m_in_deps_total = 1;
        m_in_deps[0] = event;
    }

    return true;
}

// receive pointer data if source or destination or both of them are
// noncontiguous. There is guarantee that length of destination enough for
// transferred data.
bool OffloadDescriptor::receive_noncontiguous_pointer_data(
    int i,
    COIBUFFER dst_buf,
    COIEVENT *event,
    uint64_t &received_data,
    uint32_t in_deps_amount,
    COIEVENT *in_deps
)
{
    int64_t offset_src, offset_dst;
    int64_t length_src, length_dst;
    int64_t length_src_cur, length_dst_cur;
    int64_t receive_size;
    COIRESULT res;
    bool dst_is_empty = true;
    bool src_is_empty = true;

    char *base = offload_get_src_base(
                     m_vars[i].into ?
                     static_cast<char*>(m_vars[i].into) :
                     static_cast<char*>(m_vars[i].ptr),
                     m_vars[i].type.dst);
    received_data = 0;

    // Set length_src and length_dst
    length_src = (m_vars_extra[i].read_rng_src) ?
        m_vars_extra[i].read_rng_src->range_size : m_vars[i].size;
    length_dst = !m_vars[i].into ? length_src :
                     (m_vars_extra[i].read_rng_dst) ?
                     m_vars_extra[i].read_rng_dst->range_size : m_vars[i].size;
    receive_size = (length_src < length_dst) ? length_src : length_dst;

    // If BufferReadMultiD is defined we can set values of required arguments
    // and transfer noncontiguous data via call to the COI routine.
    if (__offload_use_coi_noncontiguous_transfer && COI::BufferReadMultiD) {
        struct Arr_Desc* arr_desc_dst;
        struct Arr_Desc* arr_desc_src;
        int64_t size_src, size_dst;

        offset_src = (m_vars_extra[i].read_rng_src)?
            m_vars_extra[i].read_rng_src->init_offset : m_vars[i].disp;
        size_src = m_vars_extra[i].read_rng_src ?
            cean_get_transf_size(m_vars_extra[i].read_rng_src) :
            m_vars[i].size;

        offset_dst = (m_vars_extra[i].read_rng_dst)?
            m_vars_extra[i].read_rng_dst->init_offset : m_vars_extra[i].cpu_disp;
        size_dst = m_vars_extra[i].read_rng_dst ?
            cean_get_transf_size(m_vars_extra[i].read_rng_dst) : m_vars[i].size;

        int64_t el_size = (!m_vars[i].into ||
                           (m_vars_extra[i].read_rng_src &&
                            m_vars_extra[i].read_rng_dst)) ?
                            1 :
                            m_vars_extra[i].read_rng_src ?
                                m_vars_extra[i].read_rng_src->arr_desc->dim[
                m_vars_extra[i].read_rng_src->arr_desc->rank - 1].size :
        m_vars_extra[i].read_rng_dst->arr_desc->dim[
            m_vars_extra[i].read_rng_dst->arr_desc->rank - 1].size;
            arr_desc_src = (m_vars_extra[i].read_rng_src) ?
                m_vars_extra[i].read_rng_src->arr_desc :
            make_arr_desc(NULL, // don't required for source
                offset_src/el_size, size_src/el_size,
                el_size);
            arr_desc_dst = !m_vars[i].into ? arr_desc_src :
                (m_vars_extra[i].read_rng_dst) ?
                m_vars_extra[i].read_rng_dst->arr_desc :
            make_arr_desc(NULL,
                offset_dst/el_size, size_src/el_size, el_size);

            arr_desc_dst->base = reinterpret_cast<int64_t>(base);

            res = COI::BufferReadMultiD(
                m_vars_extra[i].src_data->mic_buf,      // SourceBuffer
                m_vars[i].offset + m_vars[i].mic_offset -
                m_vars_extra[i].src_data->alloc_disp,         // Offset
                (void*)arr_desc_dst,                 // descriptor of DestArray
                (void*)arr_desc_src,                 // descriptor of SrcArray
                COI_COPY_UNSPECIFIED,         // Type
                in_deps_amount,               // Number of in Dependencies
                in_deps,                      // array of in Dependencies
                event);                       // out Dependency
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_copy, res);
            }
            return(true);
    }
    // if event is defined we must multiplate for all contiguous intervals
    // that will be Copied/Read.
    // Take in account that we already have 1 event.
    if (event) {
        m_out_deps_allocated += (length_src / receive_size) *
                                ((m_vars_extra[i].read_rng_src) ?
                                m_vars_extra[i].read_rng_src->range_max_number : 1) ;
        m_out_deps    =
            (COIEVENT*)realloc(m_out_deps, sizeof(COIEVENT) * m_out_deps_allocated);
        m_out_deps_total--; 
    }
 
    // consequently get contiguous ranges,
    // define corresponded destination offset and receive data
    do {
        // get sorce offset
        if (src_is_empty) {
            if (m_vars_extra[i].read_rng_src) {
                if (!get_next_range(m_vars_extra[i].read_rng_src,
                         &offset_src)) {
                    // source ranges are over - nothing to send
                    break;
                }
            }
            else if (received_data == 0) {
                offset_src = m_vars[i].disp;
            }
            else {
                break;
            }
            length_src_cur = length_src;
        }
        else {
            // if source is contiguous or its contiguous range is greater
            // than destination one
            offset_src += receive_size;
        }
        length_src_cur -= receive_size;
        src_is_empty = length_src_cur == 0;

        // get destination offset
        if (dst_is_empty) {
            if (m_vars[i].into) {
                if (m_vars_extra[i].read_rng_dst) {
                    if (!get_next_range(m_vars_extra[i].read_rng_dst,
                             &offset_dst)) {
                        // destination ranges are over
                        LIBOFFLOAD_ERROR(c_destination_is_over);
                        return false;
                    }
                }
                // destination is contiguous.
                else {
                    offset_dst = m_vars_extra[i].cpu_disp;
                }
                length_dst_cur = length_dst;
            }
            // same as source
            else {
                offset_dst = offset_src;
                length_dst_cur = length_src;
            }
        }
        else {
            // if destination is contiguous or its contiguous range is greater
            // than source one
            offset_dst += receive_size;
        }
        length_dst_cur -= receive_size;
        dst_is_empty = length_dst_cur == 0;
        if (event) {
            event =  &m_out_deps[m_out_deps_total++];
        }
        if (dst_buf != 0) {
            res = COI::BufferCopy(
                dst_buf,
                m_vars_extra[i].src_data->mic_buf,
                m_vars_extra[i].cpu_offset + offset_dst,
                m_vars[i].offset + offset_src +
                m_vars[i].mic_offset,
                receive_size,
                COI_COPY_UNSPECIFIED,
                in_deps_amount,
                in_deps,
                event);
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_copy, res);
            }
        }
        else {
            res = COI::BufferRead(
                m_vars_extra[i].src_data->mic_buf,
                m_vars[i].offset + offset_src +
                m_vars[i].mic_offset,
                base + offset_dst,
                receive_size,
                COI_COPY_UNSPECIFIED,
                in_deps_amount,
                in_deps,
                event);
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_read, res);
            }
        }
        received_data += receive_size;
    }
    while (true);
    return true;
}

bool OffloadDescriptor::receive_pointer_data(bool is_async,
                                             bool first_run, void *info)
{
    OffloadTimer timer(get_timer_data(), c_offload_host_start_buffers_reads);

    bool should_use_async_buffer_read = m_initial_need_runfunction;
    uint64_t ptr_received = 0;
    COIRESULT res;

    // For offload_transfer and offload with empty body without signal:
    // - if there is only one buffer copy - get data synchronously
    // - if there are multiple buffer copy and
    //      __offload_parallel_copy is false - get data synchronously
    // - if there are multiple buffer copy
    //      and __offload_parallel_copy is true - get data asynchronously
    // It concerns only data with size greater than __offload_use_async_buffer_read.
    // Data of size less than __offload_use_async_buffer_read are received synchronously.
    // Synchronous transfer results in better performance in COI.
    // __offload_parallel_copy is false by default but can be changed
    // via environment variable OFFLOAD_PARALLEL_COPY
    if (!m_initial_need_runfunction && __offload_parallel_copy) {
        int big_size_count = 0;

        for (int i = 0; i < m_vars_total; i++) {
            if (m_vars[i].direction.out &&
                m_vars[i].size >= __offload_use_async_buffer_read) {
                // preallocated OUT only at second run
                if (first_run == m_vars[i].flags.preallocated) {
                    continue;
                }
                switch (m_vars[i].type.src) {
                    case c_data:
                    case c_void_ptr:
                    case c_cean_var:
                        if (m_vars[i].flags.is_static) {
                            big_size_count++;
                        }
                        break;
                    case c_string_ptr:
                    case c_data_ptr:
                    case c_cean_var_ptr:
                    case c_dv_data:
                    case c_dv_ptr_data:
                    case c_dv_data_slice:
                    case c_dv_ptr_data_slice:
                    case c_dv_ptr:
                        big_size_count++;
                        break;
                    default:
                        break;
                }
            }
        }
        if (big_size_count > 1) {
            should_use_async_buffer_read = true;
        }
    }
    uint32_t in_deps_amount = m_in_deps_total;
    COIEVENT *in_deps = m_in_deps_total > 0 ? m_in_deps : 0;

    if (0 == m_in_deps_total  &&
        m_stream != no_stream &&
        m_vars_total != 0) {
        get_stream_in_dependencies(in_deps_amount, in_deps);
    }

    for (int i = 0; i < m_vars_total; i++) {
        uint64_t received_data = m_vars[i].size;
        uint32_t in_deps_amount_save;
        COIEVENT *in_deps_save; 
              
         if (m_vars_extra[i].omp_last_event_type == c_last_read) {
            in_deps_amount_save = in_deps_amount;
            in_deps_save = in_deps;
            
            in_deps_amount += m_out_deps_total;
            if (in_deps_amount > 0) {
               in_deps = (COIEVENT*) malloc(sizeof(COIEVENT) * in_deps_amount);
               if (in_deps == NULL)
                   LIBOFFLOAD_ERROR(c_malloc);
               memcpy(in_deps, in_deps_save,
                      in_deps_amount_save * sizeof(COIEVENT));
               memcpy(in_deps + in_deps_amount_save * sizeof(COIEVENT),
                      m_out_deps,
                      m_out_deps_total * sizeof(COIEVENT));                      
            }                
        }   
        // At first run don't receive by preallocated target pointer as the
        //pointer value will be ready later after call to scatter_copyout_data
        if (first_run && m_vars[i].alloc_if && m_vars[i].flags.preallocated) {
            m_preallocated_alloc = true;
            // need one more call to OffloadDescriptor::receive_pointer_data
            if (m_vars[i].direction.out) {
                m_out_with_preallocated = true;
            }
            continue;
        }
        switch (m_vars[i].type.src) {
            case c_data_ptr_array:
                break;
            case c_data:
            case c_void_ptr:
            case c_cean_var:
                if (m_vars[i].direction.out &&
                    m_vars[i].flags.is_static) {
                    COIEVENT *event =
                        (is_async ||
                         m_in_deps_total > 0 ||
                         (should_use_async_buffer_read &&
                          m_vars[i].size >= __offload_use_async_buffer_read)) ?
                        &m_out_deps[m_out_deps_total++] : 0;
                    PtrData *ptr_data = NULL;
                    COIBUFFER dst_buf = NULL; // buffer at host
                    char *base;

                    if (VAR_TYPE_IS_PTR(m_vars[i].type.dst)) {
                        ptr_data = m_vars[i].into ?
                                   m_vars_extra[i].dst_data :
                                   m_vars_extra[i].src_data;
                    }
                    else if (VAR_TYPE_IS_SCALAR(m_vars[i].type.dst)) {
                        if (m_vars[i].flags.is_static_dstn) {
                            ptr_data = m_vars[i].into ?
                                       m_vars_extra[i].dst_data :
                                       m_vars_extra[i].src_data;
                        }
                    }
                    dst_buf = ptr_data ? ptr_data->cpu_buf : NULL;
                    if (dst_buf == NULL) {
                        base = offload_get_src_base(
                            m_vars[i].into ?
                            static_cast<char*>(m_vars[i].into) :
                            static_cast<char*>(m_vars[i].ptr),
                            m_vars[i].type.dst);
                    }

                    if (m_vars[i].flags.is_noncont_src ||
                        m_vars[i].flags.is_noncont_dst) {
                        receive_noncontiguous_pointer_data(
                            i, dst_buf, event, received_data,
                            in_deps_amount, in_deps);
                    }
                    else if (dst_buf != 0) {
                        res = COI::BufferCopy(
                            dst_buf,
                            m_vars_extra[i].src_data->mic_buf,
                            m_vars_extra[i].cpu_offset +
                            m_vars_extra[i].cpu_disp,
                            m_vars[i].offset + m_vars[i].disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount,
                            in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_copy, res);
                        }
                    }
                    else {
                       res = COI::BufferRead(
                            m_vars_extra[i].src_data->mic_buf,
                            m_vars[i].offset + m_vars[i].disp,
                            base + m_vars_extra[i].cpu_offset +
                            m_vars_extra[i].cpu_disp,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount,
                            in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_read, res);
                        }
                    }
                    ptr_received += received_data;
                }
                break;

            case c_string_ptr:
            case c_data_ptr:
            case c_cean_var_ptr:
            case c_dv_data:
            case c_dv_ptr_data:
            case c_dv_data_slice:
            case c_dv_ptr_data_slice:
            case c_dv_ptr: {
                COIBUFFER dst_buf = NULL; // buffer on host
                if (m_vars[i].direction.out && m_vars[i].size > 0) {
                    COIEVENT *event =
                        (is_async ||
                         m_in_deps_total > 0 ||
                         (should_use_async_buffer_read &&
                          m_vars[i].size >= __offload_use_async_buffer_read)) ?
                        &m_out_deps[m_out_deps_total++] : 0;

                    uint64_t dst_offset = 0;
                    char *base = static_cast<char*>(m_vars[i].ptr);

                    if (VAR_TYPE_IS_PTR(m_vars[i].type.dst)) {
                        PtrData *ptr_data = m_vars[i].into ?
                                            m_vars_extra[i].dst_data :
                                            m_vars_extra[i].src_data;
                        dst_buf = ptr_data ? ptr_data->cpu_buf : NULL;
                        if (dst_buf == NULL) {
                            base = m_vars[i].into ?
                                   *static_cast<char**>(m_vars[i].into) :
                                   *static_cast<char**>(m_vars[i].ptr);
                        }
                        dst_offset = m_vars_extra[i].cpu_offset +
                                     m_vars_extra[i].cpu_disp;
                    }
                    else if (VAR_TYPE_IS_SCALAR(m_vars[i].type.dst)) {
                        if (m_vars[i].flags.is_static_dstn) {
                            dst_buf = m_vars[i].into ?
                                        m_vars_extra[i].dst_data->cpu_buf :
                                        m_vars_extra[i].src_data->cpu_buf;
                        }
                        if (dst_buf == NULL) {
                            base = offload_get_src_base(
                                m_vars[i].into ?
                                static_cast<char*>(m_vars[i].into) :
                                static_cast<char*>(m_vars[i].ptr),
                                m_vars[i].type.dst);
                        }
                        dst_offset = m_vars_extra[i].cpu_offset +
                                     m_vars_extra[i].cpu_disp;
                    }
                    else if (VAR_TYPE_IS_DV_DATA(m_vars[i].type.dst) ||
                             VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.dst)) {
                        PtrData *ptr_data = m_vars[i].into != 0 ?
                                            m_vars_extra[i].dst_data :
                                            m_vars_extra[i].src_data;
                        dst_buf = ptr_data != 0 ? ptr_data->cpu_buf : 0;
                        if (dst_buf == NULL) {
                            base = offload_get_src_base(
                                m_vars[i].into ?
                                static_cast<char*>(m_vars[i].into) :
                                static_cast<char*>(m_vars[i].ptr),
                                m_vars[i].type.dst);

                        }
                        dst_offset = m_vars_extra[i].cpu_offset +
                                     m_vars_extra[i].cpu_disp;
                    }

                    if (m_vars[i].flags.is_noncont_src ||
                        m_vars[i].flags.is_noncont_dst) {
                        receive_noncontiguous_pointer_data(
                            i, dst_buf, event, received_data,
                            in_deps_amount,
                            in_deps);
                    }
                    else if (dst_buf != 0) {
                        res = COI::BufferCopy(
                            dst_buf,
                            m_vars_extra[i].src_data->mic_buf,
                            dst_offset,
                            m_vars[i].offset + m_vars[i].disp +
                                m_vars[i].mic_offset,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount,
                            in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_copy, res);
                        }
                    }
                    else {
                        res = COI::BufferRead(
                            m_vars_extra[i].src_data->mic_buf,
                            m_vars[i].offset + m_vars[i].disp +
                                m_vars[i].mic_offset,
                            base + dst_offset,
                            m_vars[i].size,
                            COI_COPY_UNSPECIFIED,
                            in_deps_amount,
                            in_deps,
                            event);
                        if (res != COI_SUCCESS) {
                            if (m_status != 0) {
                                m_status->result = translate_coi_error(res);
                                return false;
                            }
                            report_coi_error(c_buf_read, res);
                        }
                    }
                    ptr_received += received_data;
                }
                break;
            }

            default:
                break;
        }

        if (m_vars_extra[i].omp_last_event_type == c_last_read) {
            in_deps_amount = in_deps_amount_save;
            in_deps = in_deps_save;
            register_omp_event_call_back(&m_out_deps[m_out_deps_total - 1], info);                
        }
        // destroy buffers for obsolete stacks
        if (m_destroy_stack.size() != 0) {
            for (PtrDataList::iterator it = m_destroy_stack.begin();
                it != m_destroy_stack.end(); it++) {
                PtrData *ptr_data = *it;
                m_destroy_buffers.push_back(ptr_data->mic_buf);
                OFFLOAD_TRACE(3, "Removing stack buffer with addr %p\n",
                                  ptr_data->mic_addr);
            }
            m_destroy_stack.clear();
        }
        if (m_vars[i].free_if) {
            // remove association for automatic variables
            if (m_is_openmp && !m_vars[i].flags.is_static &&
                (m_vars[i].type.src == c_data ||
                 m_vars[i].type.src == c_void_ptr ||
                 m_vars[i].type.src == c_cean_var)) {
                AutoData *auto_data = m_vars_extra[i].auto_data;
                if (auto_data != 0) {
                    if (m_vars[i].flags.always_delete) {
                        auto_data->nullify_reference();
                    }
                    else if(auto_data->remove_reference() == 0) {
                        m_device.remove_auto_data(auto_data->cpu_addr.start());
                    }
                }
            }

            // destroy buffers
            if (m_vars[i].direction.out || m_vars[i].into == NULL) {
                if (!VAR_TYPE_IS_PTR(m_vars[i].type.src) &&
                    !VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.src) &&
                    !VAR_TYPE_IS_DV_DATA(m_vars[i].type.src)) {
                    continue;
                }

                PtrData *ptr_data = m_vars_extra[i].src_data;
                if (ptr_data->remove_reference() == 0) {
                    // destroy buffers
                    if (ptr_data->cpu_buf != 0) {
                        m_destroy_buffers.push_back(ptr_data->cpu_buf);
                    }
                    if (ptr_data->mic_buf != 0) {
                        m_destroy_buffers.push_back(ptr_data->mic_buf);
                    }
                    OFFLOAD_TRACE(3, "Removing association for addr %p\n",
                                  ptr_data->cpu_addr.start());

                    // remove association from map
                    if (m_vars[i].flags.targetptr) {
                        m_device.remove_targetptr_data(ptr_data->cpu_addr.start());
                    }
                    else {
                        m_device.remove_ptr_data(ptr_data->cpu_addr.start());
                    }
                }
            }
            else if (VAR_TYPE_IS_PTR(m_vars[i].type.dst) ||
                     VAR_TYPE_IS_DV_DATA_SLICE(m_vars[i].type.dst) ||
                     VAR_TYPE_IS_DV_DATA(m_vars[i].type.dst)) {
                PtrData *ptr_data = m_vars_extra[i].dst_data;
                if (ptr_data->remove_reference() == 0) {
                    // destroy buffers
                    if (ptr_data->cpu_buf != 0) {
                        m_destroy_buffers.push_back(ptr_data->cpu_buf);
                    }
                    if (ptr_data->mic_buf != 0) {
                        m_destroy_buffers.push_back(ptr_data->mic_buf);
                    }
                    OFFLOAD_TRACE(3, "Removing association for addr %p\n",
                                  ptr_data->cpu_addr.start());

                    // remove association from map
                    if (m_vars[i].flags.targetptr) {
                        m_device.remove_targetptr_data(ptr_data->cpu_addr.start());
                    }
                    else {
                        m_device.remove_ptr_data(ptr_data->cpu_addr.start());
                    }
                }
            }
        }
    }

    if (m_status) {
        m_status->data_received += ptr_received;
    }

    OFFLOAD_TIMER_HOST_RDATA(get_timer_data(), ptr_received);
    OFFLOAD_DEBUG_TRACE_1(1, GET_OFFLOAD_NUMBER(get_timer_data()),
                  c_offload_received_pointer_data,
                  "Total pointer data received from target: [%lld] bytes\n",
                  ptr_received);

    return true;
}

bool OffloadDescriptor::scatter_copyout_data()
{
    OffloadTimer timer(get_timer_data(), c_offload_host_scatter_outputs);

    if (m_need_runfunction && m_out_datalen > 0) {

        // total size that need to be transferred from target to host
        COIMAPINSTANCE map_inst;
        COIRESULT res;
        char *data;

        // output data buffer
        if (m_func_desc->data_offset == 0) {
            OffloadTimer timer_map(get_timer_data(),
                                   c_offload_host_map_out_data_buffer);

            COIRESULT res = COI::BufferMap(m_inout_buf, 0, m_out_datalen,
                                           COI_MAP_READ_ONLY, 0, 0, 0,
                                           &map_inst,
                                            reinterpret_cast<void**>(&data));
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_map, res);
            }
        }
        else {
            data = (char*) m_func_desc + m_func_desc->data_offset;
        }

        // get timing data
        OFFLOAD_TIMER_TARGET_DATA(get_timer_data(), data);
        data += OFFLOAD_TIMER_DATALEN();

        // initialize output marshaller
        m_out.init_buffer(data, m_out_datalen);

        for (int i = 0; i < m_vars_total; i++) {
            bool src_is_for_mic = (m_vars[i].direction.out ||
                                   m_vars[i].into == NULL);

            if (m_vars[i].type.src != c_data_ptr_array &&
                m_vars[i].flags.preallocated && m_vars[i].alloc_if) {
                PtrData *ptr_data;
                void *ptr_value;
                void ** cpu_ptr = src_is_for_mic ?
                                  reinterpret_cast<void**>(m_vars[i].ptr) :
                                  reinterpret_cast<void**>(m_vars[i].into);
                void*   alloc_base = NULL;
                int64_t alloc_disp = 0;
                int64_t alloc_size;
                if (m_vars_extra[i].alloc != NULL) {
                    // array descriptor
                    const Arr_Desc *ap =
                        static_cast<const Arr_Desc*>(m_vars_extra[i].alloc);

                    __arr_data_offset_and_length(ap, alloc_disp, alloc_size);

                    alloc_base = reinterpret_cast<void*>(ap->base);
                }

                // get pointer to target memory
                m_out.receive_data(&ptr_value, sizeof(void*));

                // add new entry
                if (!alloc_ptr_data(
                    ptr_data,
                    ptr_value,
                    (alloc_base != NULL) ?
                        alloc_disp : m_vars[i].disp,
                    (alloc_base != NULL) ?
                        alloc_size : m_vars[i].size,
                    alloc_disp,
                    0,
                    m_vars[i].flags.targetptr,
                    m_vars[i].flags.preallocated,
                    m_vars[i].flags.pin)) {
                    return false;
                }

                ptr_data->add_reference();
                *cpu_ptr = ptr_value;
                if (src_is_for_mic) {
                    m_vars_extra[i].src_data = ptr_data;
                }
                else {
                    m_vars_extra[i].dst_data = ptr_data;
                }
                m_vars[i].offset = (char*) ptr_value -
                                   (char*) ptr_data->cpu_addr.start();
            }

            switch (m_vars[i].type.src) {
                case c_data_ptr_array:
                    break;
                case c_data:
                case c_void_ptr:
                case c_cean_var:
                    if (m_vars[i].direction.out &&
                        !m_vars[i].flags.is_static) {

                        if (m_vars[i].into) {
                            char *ptr = offload_get_src_base(
                                static_cast<char*>(m_vars[i].into),
                                m_vars[i].type.dst);
                            m_out.receive_data(ptr + m_vars_extra[i].cpu_disp,
                                               m_vars[i].size);
                        }
                        else {
                            m_out.receive_data(
                                static_cast<char*>(m_vars[i].ptr) +
                                    m_vars_extra[i].cpu_disp,
                                m_vars[i].size);
                        }
                    }
                    break;

                case c_func_ptr:
                    if (m_vars[i].direction.out) {
                        m_out.receive_func_ptr((const void**) m_vars[i].ptr);
                    }
                    break;

                default:
                    break;
            }
        }

        if (m_status) {
            m_status->data_received += m_out.get_tfr_size();
        }

        if (m_func_desc->data_offset == 0) {
            OffloadTimer timer_unmap(get_timer_data(),
                                     c_offload_host_unmap_out_data_buffer);

            COIRESULT res = COI::BufferUnmap(map_inst, 0, 0, 0);
            if (res != COI_SUCCESS) {
                if (m_status != 0) {
                    m_status->result = translate_coi_error(res);
                    return false;
                }
                report_coi_error(c_buf_unmap, res);
            }
        }
    }

    OFFLOAD_TIMER_HOST_RDATA(get_timer_data(), m_out.get_tfr_size());
    OFFLOAD_TRACE(1, "Total copyout data received from target: [%lld] bytes\n",
                  m_out.get_tfr_size());

    return true;
}

static void get_arr_desc_numbers(
    const Arr_Desc *ap,
    int64_t el_size,
    int64_t &offset,
    int64_t &size,
    int     &el_number,
    CeanReadRanges* &ptr_ranges
)
{
    if (is_arr_desc_contiguous(ap)) {
        ptr_ranges = NULL;
        __arr_data_offset_and_length(ap, offset, size);
        el_number = size / el_size;
    }
    else {
        ptr_ranges = init_read_ranges_arr_desc(ap);
        el_number = (ptr_ranges->range_size / el_size) *
                    ptr_ranges->range_max_number;
        size = ptr_ranges->range_size;
    }
}

bool OffloadDescriptor::gen_var_descs_for_pointer_array(int i)
{
    int             pointers_number;
    int             tmp_val;
    int             new_index = m_vars_total;
    const Arr_Desc *ap;
    const VarDesc3 *vd3 = static_cast<const VarDesc3*>(m_vars[i].ptr);
    int             flags = vd3->array_fields;
    bool            src_is_for_mic = (m_vars[i].direction.out ||
                                      m_vars[i].into == NULL);

    ReadArrElements<void *>  ptr;
    ReadArrElements<void *>  into;
    ReadArrElements<int64_t> ext_start;
    ReadArrElements<int64_t> ext_elements;
    ReadArrElements<int64_t> align;
    ReadArrElements<int64_t> alloc_if;
    ReadArrElements<int64_t> free_if;
    ReadArrElements<int64_t> into_start;
    ReadArrElements<int64_t> into_elem;
    ReadArrElements<int64_t> alloc_start;
    ReadArrElements<int64_t> alloc_elem;


    ap = static_cast<const Arr_Desc*>(vd3->ptr_array);

    // "pointers_number" for total number of transferred pointers.
    // For each of them we create new var_desc and put it at the bottom
    // of the var_desc's array
    get_arr_desc_numbers(ap, sizeof(void *), ptr.offset, ptr.size,
        pointers_number, ptr.ranges);
    ptr.base = (m_vars[i].flags.is_pointer) ?
               *(reinterpret_cast<char**>(ap->base)) :
               reinterpret_cast<char*>(ap->base);

    // 2. prepare memory for new var_descs
    m_vars_total += pointers_number;
    m_vars       = (VarDesc*)realloc(m_vars, m_vars_total * sizeof(VarDesc));
    if (m_vars == NULL)
      LIBOFFLOAD_ERROR(c_malloc);
    m_vars_extra =
        (VarExtra*)realloc(m_vars_extra, m_vars_total * sizeof(VarExtra));
    if (m_vars_extra == NULL)
      LIBOFFLOAD_ERROR(c_malloc);
    m_in_deps    =
        (COIEVENT*)realloc(m_in_deps, sizeof(COIEVENT) * (m_vars_total + 1));
    if (m_in_deps == NULL)
      LIBOFFLOAD_ERROR(c_malloc);
    m_out_deps   =
        (COIEVENT*)realloc(m_out_deps, sizeof(COIEVENT) * m_vars_total);
    if (m_out_deps == NULL)
      LIBOFFLOAD_ERROR(c_malloc);

    // 3. Prepare for reading new var_desc's fields
    //    EXTENT START
    if ((flags & (1<<flag_extent_start_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->extent_start);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size, ext_start.offset,
            ext_start.size, tmp_val, ext_start.ranges);
        ext_start.base = reinterpret_cast<char*>(ap->base);
        ext_start.el_size = ap->dim[ap->rank - 1].size;

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "extent start");
            return false;
        }
    }
    else if ((flags & (1<<flag_extent_start_is_scalar)) != 0) {
        ext_start.val = (int64_t)vd3->extent_start;
    }
    else {
        ext_start.val = 0;
    }

    //    EXTENT ELEMENTS NUMBER
    if ((flags & (1<<flag_extent_elements_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->extent_elements);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size,
            ext_elements.offset, ext_elements.size,
            tmp_val, ext_elements.ranges);
        ext_elements.base = reinterpret_cast<char*>(ap->base);
        ext_elements.el_size = ap->dim[ap->rank - 1].size;

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "extent elements");
            return false;
        }
    }
    else if ((flags & (1<<flag_extent_elements_is_scalar)) != 0) {
        ext_elements.val = (int64_t)vd3->extent_elements;
    }
    else {
        ext_elements.val = m_vars[i].count;
    }

    //    ALLOC_IF
    if ((flags & (1<<flag_alloc_if_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->alloc_if_array);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size, alloc_if.offset,
            alloc_if.size, tmp_val, alloc_if.ranges);
        alloc_if.base = reinterpret_cast<char*>(ap->base);
        alloc_if.el_size = ap->dim[ap->rank - 1].size;

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "alloc_if");
            return false;
        }
    }
    else {
        alloc_if.val = m_vars[i].alloc_if;
    }

    //    FREE_IF
    if ((flags & (1<<flag_free_if_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->free_if_array);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size, free_if.offset,
            free_if.size, tmp_val, free_if.ranges);
        free_if.base = reinterpret_cast<char*>(ap->base);
        free_if.el_size = ap->dim[ap->rank - 1].size;

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "free_if");
            return false;
        }
    }
    else {
        free_if.val = m_vars[i].free_if;
    }

    //    ALIGN

    if ((flags & (1<<flag_align_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->align_array);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size, align.offset,
            align.size, tmp_val, align.ranges);
        align.base = reinterpret_cast<char*>(ap->base);
        align.el_size = ap->dim[ap->rank - 1].size;

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "align");
            return false;
        }
    }
    else {
        align.val = m_vars[i].align;
    }

    // 3.1 INTO

    if (m_vars[i].into) {
        ap = static_cast<const Arr_Desc*>(m_vars[i].into);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size, into.offset,
            into.size, tmp_val, into.ranges);
        into.base = reinterpret_cast<char*>(ap->base);

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "into");
            return false;
        }
    }

    // 3.2 INTO_START

    if ((flags & (1<<flag_into_start_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->into_start);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size, into_start.offset,
            into_start.size, tmp_val, into_start.ranges);
        into_start.base = reinterpret_cast<char*>(ap->base);
        into_start.el_size = ap->dim[ap->rank - 1].size;

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "into_extent start");
            return false;
        }
    }
    else if ((flags & (1<<flag_into_start_is_scalar)) != 0) {
        into_start.val = (int64_t)vd3->into_start;
    }
    else {
        into_start.val = 0;
    }

    // 3.3 INTO_ELEMENTS

    if ((flags & (1<<flag_into_elements_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->into_elements);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size, into_elem.offset,
            into_elem.size, tmp_val, into_elem.ranges);
        into_elem.base = reinterpret_cast<char*>(ap->base);
        into_elem.el_size = ap->dim[ap->rank - 1].size;

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "into_extent elements");
            return false;
        }
    }
    else if ((flags & (1<<flag_into_elements_is_scalar)) != 0) {
        into_elem.val = (int64_t)vd3->into_elements;
    }
    else {
        into_elem.val = m_vars[i].count;
    }

    //    alloc_start

    if ((flags & (1<<flag_alloc_start_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->alloc_start);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size,
            alloc_start.offset, alloc_start.size, tmp_val,
            alloc_start.ranges);
        alloc_start.base = reinterpret_cast<char*>(ap->base);
        alloc_start.el_size = ap->dim[ap->rank - 1].size;

        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "alloc_extent start");
            return false;
        }
    }
    else if ((flags & (1<<flag_alloc_start_is_scalar)) != 0) {
        alloc_start.val = (int64_t)vd3->alloc_start;
    }
    else {
        alloc_start.val = 0;
    }

    //    alloc_elem

    if ((flags & (1<<flag_alloc_elements_is_array)) != 0) {
        ap = static_cast<const Arr_Desc*>(vd3->alloc_elements);
        get_arr_desc_numbers(ap, ap->dim[ap->rank - 1].size, alloc_elem.offset,
            alloc_elem.size, tmp_val, alloc_elem.ranges);
        alloc_elem.base = reinterpret_cast<char*>(ap->base);
        alloc_elem.el_size = ap->dim[ap->rank - 1].size;
        if (tmp_val < pointers_number) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch,
                             "alloc_extent elements");
            return false;
        }
    }
    else if ((flags & (1<<flag_alloc_elements_is_scalar)) != 0) {
        alloc_elem.val = (int64_t)vd3->alloc_elements;
    }
    else {
        alloc_elem.val = 0;
    }

    for (int k = 0; k < pointers_number; k++) {
        int type = flags & 0x3f;
        int type_src, type_dst;
        //  Get new values
        // type_src, type_dst
        type_src = type_dst = (type == c_data_ptr_array) ?
                              c_data_ptr   : (type == c_func_ptr_array) ?
                              c_func_ptr   : (type == c_void_ptr_array) ?
                              c_void_ptr   : (type == c_string_ptr_array) ?
                              c_string_ptr : 0;

        // Get ptr val
        if (!ptr.read_next(true)) {
            break;
        }
        else {
            ptr.val = (void*)(ptr.base + ptr.offset);
        }

        // !!! If we got error at phase of reading - it's an internal
        // !!! error, as we must detect mismatch before

        // Get into val
        if (m_vars[i].into) {
            if (!into.read_next(true)) {
                LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "into");
                LIBOFFLOAD_ABORT;
            }
            else {
                into.val = (void*)(into.base + into.offset);
            }
        }

        // Get other components of the clause
        if (!ext_start.read_next(flags & (1<<flag_extent_start_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "extent start");
            LIBOFFLOAD_ABORT;
        }
        if (!ext_elements.read_next(
                flags & (1<<flag_extent_elements_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "extent elements");
            LIBOFFLOAD_ABORT;
        }
        if (!alloc_if.read_next(flags & (1<<flag_alloc_if_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "alloc_if");
            LIBOFFLOAD_ABORT;
        }
        if (!free_if.read_next(flags & (1<<flag_free_if_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "free_if");
            LIBOFFLOAD_ABORT;
        }
        if (!align.read_next(flags & (1<<flag_align_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "align");
            LIBOFFLOAD_ABORT;
        }
        if (!into_start.read_next(flags & (1<<flag_into_start_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "into_extent start");
            LIBOFFLOAD_ABORT;
        }
        if (!into_elem.read_next(flags & (1<<flag_into_elements_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "into_extent elements");
            LIBOFFLOAD_ABORT;
        }
        if (!alloc_start.read_next(flags & (1<<flag_alloc_start_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "alloc_extent start");
            LIBOFFLOAD_ABORT;
        }
        if (!alloc_elem.read_next(
                 flags & (1<<flag_alloc_elements_is_array))) {
            LIBOFFLOAD_ERROR(c_pointer_array_mismatch, "alloc_extent elements");
            LIBOFFLOAD_ABORT;
        }

        m_vars[new_index + k].direction.bits = m_vars[i].direction.bits;
        m_vars[new_index + k].alloc_if = alloc_if.val;
        m_vars[new_index + k].free_if = free_if.val;
        m_vars[new_index + k].align = align.val;
        m_vars[new_index + k].mic_offset = 0;
        m_vars[new_index + k].flags.bits = m_vars[i].flags.bits;
        m_vars[new_index + k].offset = 0;
        m_vars[new_index + k].size = m_vars[i].size;
        m_vars[new_index + k].flags.targetptr = m_vars[i].flags.targetptr;
        m_vars[new_index + k].flags.preallocated =
                                             m_vars[i].flags.preallocated;

        if (ext_start.val == 0) {
            m_vars[new_index + k].count = ext_elements.val;
            m_vars[new_index + k].ptr = ptr.val;
            if (type_src == c_string_ptr) {
                m_vars[new_index + k].size = 0;
            }
        }
        else {
            m_vars[new_index + k].count = 0;
            m_vars[new_index + k].ptr =
                static_cast<void*>(make_arr_desc(
                ptr.val,
                ext_start.val,
                ext_elements.val,
                m_vars[i].size));

            type_src = type_src == c_data_ptr ? c_cean_var_ptr :
                                   c_string_ptr ? c_cean_var_ptr :
                                   type_src;
            if (!m_vars[i].into) {
                type_dst = type_src;
            }
        }

        if (m_vars[i].into && into_elem.val != 0) {
            m_vars[new_index + k].into =
                static_cast<void*>(make_arr_desc(
                into.val,
                into_start.val,
                into_elem.val,
                m_vars[i].size));
            type_dst = (type == c_data_ptr_array) ? c_cean_var_ptr :
                       (type == c_string_ptr_array) ? c_cean_var_ptr :
                        type_src;
        }
        else {
            m_vars[new_index + k].into = NULL;
        }

        if (alloc_elem.val != 0) {
            m_vars[new_index + k].alloc =
                static_cast<void*>(make_arr_desc(
                ptr.val,
                alloc_start.val,
                alloc_elem.val,
                m_vars[i].size));
        }
        else {
            m_vars[new_index + k].alloc = NULL;
        }

        m_vars[new_index + k].type.src = type_src;
        m_vars[new_index + k].type.dst = type_dst;

        m_vars_extra[new_index + k].alloc = m_vars[new_index + k].alloc;
        m_vars_extra[new_index + k].is_arr_ptr_el = 1;
        m_vars_extra[new_index + k].ptr_arr_offset =
            src_is_for_mic ? ptr.offset : into.offset;
    }
    // count and alloc fields are useless at target. They can be reused
    // for pointer arrays.
    m_vars[i].count = pointers_number;
    m_vars[i].ptr_arr_offset = new_index;
    return true;
}

// Gets in dependencies of the previous offload via the stream "m_stream".
// Out argument in_deps_amount - address of amount of the dependencies
// Out argument in_deps - array of dependencies.
// Description of the dependencies scheme for streams :
// ----------------------------------------------------
// Every offload forms DAG consisted of 3 nodes:
// for in-transfers, runfunction and out-transfers.
// Every node has in-dependencies and out-dependencies
// Out-dependencies of previous node forms in-dependencies of current node.
// In-dependencies of 1-st node (of in-transfers) without streams is equal
// to NULL. For streams in-dependencies of 1-st node is equal to list of out
// dependencies of last node of previous offload via this stream.
// So we can say that DAGs of 2 consequent offloads via the same stream are
// connected by the way described above.
void OffloadDescriptor::get_stream_in_dependencies(
    uint32_t &in_deps_amount,
    COIEVENT* &in_deps
)
{
    if (m_stream != no_stream && m_stream != 0) {
        Stream * stream = Stream::find_stream(m_stream, false);
        if (!stream) {
            LIBOFFLOAD_ERROR(c_offload_no_stream,
                             m_device.get_logical_index());
            LIBOFFLOAD_ABORT;
        }
        OffloadDescriptor* offload = stream->get_last_offload();

        // if it's the first offload in the stream
        if (!offload) {
            return;
        }
        // if last offload has out-tranfers
        if (offload->m_out_deps_total) {
            in_deps_amount = offload->m_out_deps_total;
            in_deps = offload->m_out_deps;
        }
        // last offload only sends pointer data or run function or both of them
        // and has no out-transfers
        else if (offload->m_in_deps_total) {
            in_deps_amount = offload->m_in_deps_total;
            in_deps = offload->m_in_deps;
        }
    }
}

static void __offload_fini_library(void)
{
    OFFLOAD_DEBUG_TRACE(2, "Cleanup offload library ...\n");
    if (mic_engines_total > 0) {
        delete[] mic_engines;
        mic_engines_total = 0;

        if (mic_proxy_fs_root != 0) {
            free(mic_proxy_fs_root);
            mic_proxy_fs_root = 0;
        }

        if (mic_library_path != 0) {
            free(mic_library_path);
            mic_library_path = 0;
        }

        // destroy thread key
        thread_key_delete(mic_thread_key);
    }

    // unload COI library
    if (COI::is_available) {
        COI::fini();
    }

    OFFLOAD_DEBUG_TRACE(2, "Cleanup offload library ... done\n");
}

static void __offload_init_library_once(void)
{
    COIRESULT res;
    uint32_t num_devices;
    std::bitset<MIC_ENGINES_MAX> devices;
    prefix = report_get_message_str(c_report_host);

    // initialize trace
    const char *env_var = getenv(htrace_envname);
    if (env_var != 0 && *env_var != '\0') {
        int64_t new_val;
        if (__offload_parse_int_string(env_var, new_val)) {
            console_enabled = new_val & 0x0f;
        }
    }

    env_var = getenv(offload_report_envname);
    if (env_var != 0 && *env_var != '\0') {
        int64_t env_val;
        if (__offload_parse_int_string(env_var, env_val)) {
            if (env_val == OFFLOAD_REPORT_1 ||
                env_val == OFFLOAD_REPORT_2 ||
                env_val == OFFLOAD_REPORT_3) {
                offload_report_level = env_val;
            }
            else {
                LIBOFFLOAD_ERROR(c_invalid_env_report_value,
                                 offload_report_envname);
            }
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_int_value,
                             offload_report_envname);
        }
    }
    else if (!offload_report_level) {
        env_var = getenv(timer_envname);
        if (env_var != 0 && *env_var != '\0') {
            timer_enabled = atoi(env_var);
        }
    }

    // initialize COI
    if (!COI::init()) {
        return;
    }

    // get number of devices installed in the system
    res = COI::EngineGetCount(COI_ISA_MIC, &num_devices);
    if (res != COI_SUCCESS) {
        return;
    }

    if (num_devices > MIC_ENGINES_MAX) {
        num_devices = MIC_ENGINES_MAX;
    }

    // fill in the list of devices that can be used for offloading
    env_var = getenv("OFFLOAD_DEVICES");
    if (env_var != 0) {
        if (strcasecmp(env_var, "none") != 0) {
            // value is composed of comma separated physical device indexes
            char *buf = strdup(env_var);
	    if (buf == NULL)
	      LIBOFFLOAD_ERROR(c_malloc);
            char *str, *ptr;
            for (str = strtok_r(buf, ",", &ptr); str != 0;
                 str = strtok_r(0, ",", &ptr)) {
                // convert string to an int
                int64_t num;
                if (!__offload_parse_int_string(str, num)) {
                    LIBOFFLOAD_ERROR(c_mic_init5);

                    // fallback to using all installed devices
                    devices.reset();
                    for (int i = 0; i < num_devices; i++) {
                        devices.set(i);
                    }
                    break;
                }
                if (num < 0 || num >= num_devices) {
                    LIBOFFLOAD_ERROR(c_mic_init6, num);
                    continue;
                }
                devices.set(num);
            }
            free(buf);
        }
    }
    else {
        // use all available devices
        for (int i = 0; i < num_devices; i++) {
            COIENGINE engine;
            res = COI::EngineGetHandle(COI_ISA_MIC, i, &engine);
            if (res == COI_SUCCESS) {
                devices.set(i);
            }
        }
    }

    mic_engines_total = devices.count();

    // no need to continue if there are no devices to offload to
    if (mic_engines_total <= 0) {
        return;
    }

    // initialize indexes for available devices
    mic_engines = new Engine[mic_engines_total];
    for (int p_idx = 0, l_idx = 0; p_idx < num_devices; p_idx++) {
        if (devices[p_idx]) {
            mic_engines[l_idx].set_indexes(l_idx, p_idx);
            l_idx++;
        }
    }

    // Get DMA channel count to pass it to COI
    env_var = getenv("OFFLOAD_DMA_CHANNEL_COUNT");
    if (env_var != 0) {
        int64_t new_val;
        if (__offload_parse_int_string(env_var, new_val)) {
            mic_dma_channel_count = new_val;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_value,
                             "OFFLOAD_DMA_CHANNEL_COUNT");
        }
    }

    // Set COI_HOST_THREAD_AFFINITY if OFFLOAD_HOST_THREAD_AFFINITY is set.
    // Use putenv instead of setenv as Windows has no setenv.
    // Note: putenv requires its argument can't be freed or modified.
    // So no free after call to putenv or elsewhere.
    env_var = getenv("OFFLOAD_HOST_THREAD_AFFINITY");
    if (env_var != 0) {
        char * new_env_var =
                   (char*) malloc(sizeof("COI_HOST_THREAD_AFFINITY=") +
                                  strlen(env_var));
	if (new_env_var == NULL)
	  LIBOFFLOAD_ERROR(c_malloc);
        sprintf(new_env_var, "COI_HOST_THREAD_AFFINITY=%s", env_var);
        putenv(new_env_var);
    }

    // library search path for device binaries
    env_var = getenv("MIC_LD_LIBRARY_PATH");
    if (env_var != 0) {
        mic_library_path = strdup(env_var);
	if (mic_library_path == NULL)
	  LIBOFFLOAD_ERROR(c_malloc);
    }


    // find target executable to be used if main application is not an
    // offload build application.
    const char *base_name = "offload_main";
    if (mic_library_path != 0) {
        char *buf = strdup(mic_library_path);
	if (buf == NULL)
	  LIBOFFLOAD_ERROR(c_malloc);
        char *try_name = (char*) alloca(strlen(mic_library_path) +
                strlen(base_name) + 2);
        char *dir, *ptr;

        for (dir = strtok_r(buf, PATH_SEPARATOR, &ptr); dir != 0;
             dir = strtok_r(0, PATH_SEPARATOR, &ptr)) {
            // compose a full path
            sprintf(try_name, "%s/%s", dir, base_name);

            // check if such file exists
            struct stat st;
            if (stat(try_name, &st) == 0 && S_ISREG(st.st_mode)) {
                mic_device_main = strdup(try_name);
		if (mic_device_main == NULL)
		  LIBOFFLOAD_ERROR(c_malloc);
                break;
            }
        }

        free(buf);
    }

    // memory size reserved for COI buffers
    env_var = getenv("MIC_BUFFERSIZE");
    if (env_var != 0) {
        uint64_t new_size;
        if (__offload_parse_size_string(env_var, new_size)) {
            mic_buffer_size = new_size;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_value, "MIC_BUFFERSIZE");
        }
    }

    // memory size reserved for 4K pages for COI buffers
    env_var = getenv("MIC_4K_BUFFER_RESERVE_SIZE");
    if (env_var != 0) {
        uint64_t new_size;
        if (__offload_parse_size_string(env_var, new_size)) {
            mic_4k_buffer_size = new_size;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_value, "MIC_4K_BUFFER_RESERVE_SIZE");
        }
    }

    // memory size reserved for 2M pages for COI buffers
    env_var = getenv("MIC_2M_BUFFER_RESERVE_SIZE");
    if (env_var != 0) {
        uint64_t new_size;
        if (__offload_parse_size_string(env_var, new_size)) {
            mic_2m_buffer_size = new_size;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_value, "MIC_2M_BUFFER_RESERVE_SIZE");
        }
    }

    // determine stacksize for the pipeline on the device
    env_var = getenv("MIC_STACKSIZE");
    if (env_var != 0 && *env_var != '\0') {
        uint64_t new_size;
        if (__offload_parse_size_string(env_var, new_size) &&
            (new_size >= 16384) && ((new_size & 4095) == 0)) {
            mic_stack_size = new_size;
        }
        else {
            LIBOFFLOAD_ERROR(c_mic_init3);
        }
    }

    // proxy I/O
    env_var = getenv("MIC_PROXY_IO");
    if (env_var != 0 && *env_var != '\0') {
        int64_t new_val;
        if (__offload_parse_int_string(env_var, new_val)) {
            mic_proxy_io = new_val;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_int_value, "MIC_PROXY_IO");
        }
    }
    env_var = getenv("MIC_PROXY_FS_ROOT");
    if (env_var != 0 && *env_var != '\0') {
        mic_proxy_fs_root = strdup(env_var);
	if (mic_proxy_fs_root == NULL)
	  LIBOFFLOAD_ERROR(c_malloc);
    }

    // Prepare environment for the target process using the following
    // rules
    // - If MIC_ENV_PREFIX is set then any environment variable on the
    //   host which has that prefix are copied to the device without
    //   the prefix.
    //   All other host environment variables are ignored.
    // - If MIC_ENV_PREFIX is not set or if MIC_ENV_PREFIX="" then host
    //   environment is duplicated.
    env_var = getenv("MIC_ENV_PREFIX");
    if (env_var != 0 && *env_var != '\0') {
        mic_env_vars.set_prefix(env_var);

        int len = strlen(env_var);
        for (int i = 0; environ[i] != 0; i++) {
            if (strncmp(environ[i], env_var, len) == 0 &&
                strncmp(environ[i], "MIC_LD_LIBRARY_PATH", 19) != 0 &&
                environ[i][len] != '=') {
                mic_env_vars.analyze_env_var(environ[i]);
            }
        }
    }

    // create key for thread data
    if (thread_key_create(&mic_thread_key, Engine::destroy_thread_data)) {
        LIBOFFLOAD_ERROR(c_mic_init4, errno);
        return;
    }

    // cpu frequency
    cpu_frequency = COI::PerfGetCycleFrequency();

    env_var = getenv(mic_use_2mb_buffers_envname);
    if (env_var != 0 && *env_var != '\0') {
        uint64_t new_size;
        if (__offload_parse_size_string(env_var, new_size)) {
            __offload_use_2mb_buffers = new_size;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_value,
                             mic_use_2mb_buffers_envname);
        }
    }

    env_var = getenv(mic_use_async_buffer_write_envname);
    if (env_var != 0 && *env_var != '\0') {
        uint64_t new_size;
        if (__offload_parse_size_string(env_var, new_size)) {
            __offload_use_async_buffer_write = new_size;
        }
    }

    env_var = getenv(mic_use_async_buffer_read_envname);
    if (env_var != 0 && *env_var != '\0') {
        uint64_t new_size;
        if (__offload_parse_size_string(env_var, new_size)) {
            __offload_use_async_buffer_read = new_size;
        }
    }

    // mic initialization type
    env_var = getenv(offload_init_envname);
    if (env_var != 0 && *env_var != '\0') {
        if (strcmp(env_var, "on_offload") == 0) {
            __offload_init_type = c_init_on_offload;
        }
        else if (strcmp(env_var, "on_offload_all") == 0) {
            __offload_init_type = c_init_on_offload_all;
        }
        else if (strcmp(env_var, "on_start") == 0) {
            __offload_init_type = c_init_on_start;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_value, offload_init_envname);
        }
    }

    // active wait
    env_var = getenv(offload_active_wait_envname);
    if (env_var != 0 && *env_var != '\0') {
        int64_t new_val;
        if (__offload_parse_int_string(env_var, new_val)) {
            __offload_active_wait = new_val;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_int_value,
                             offload_active_wait_envname);
        }
    }

    // omp device num
    env_var = getenv(omp_device_num_envname);
    if (env_var != 0 && *env_var != '\0') {
        int64_t new_val;
        if (__offload_parse_int_string(env_var, new_val) && new_val >= 0) {
            __omp_device_num = new_val;
        }
        else {
            LIBOFFLOAD_ERROR(c_omp_invalid_device_num_env,
                             omp_device_num_envname);
        }
    }

    // parallel copy of offload_transfer
    env_var = getenv(parallel_copy_envname);
    if (env_var != 0 && *env_var != '\0') {
        int64_t new_val;
        if (__offload_parse_int_string(env_var, new_val) && new_val >= 0) {
            __offload_parallel_copy = new_val;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_value,
                             parallel_copy_envname);
        }
    }

    // use COI interface for noncontiguous arrays transfer
    env_var = getenv(use_coi_noncontiguous_transfer_envname);
    if (env_var != 0 && *env_var != '\0') {
        uint64_t new_size;
        if (__offload_parse_size_string(env_var, new_size)) {
            __offload_use_coi_noncontiguous_transfer = new_size;
        }
        else {
            LIBOFFLOAD_ERROR(c_invalid_env_var_value,
                             use_coi_noncontiguous_transfer_envname);
        }
    }

    // init ORSL
    ORSL::init();
}

extern int __offload_init_library(void)
{
    // do one time intialization
    static OffloadOnceControl ctrl = OFFLOAD_ONCE_CONTROL_INIT;
    __offload_run_once(&ctrl, __offload_init_library_once);

    // offload is available if COI is available and the number of devices > 0
    bool is_available = COI::is_available && (mic_engines_total > 0);

    // register pending libraries if there are any
    if (is_available && __target_libs) {
        mutex_locker_t locker(__target_libs_lock);

        for (TargetImageList::iterator it = __target_libs_list.begin();
             it != __target_libs_list.end(); it++) {
            // Register library in COI
            COI::ProcessRegisterLibraries(1, &it->data, &it->size,
                                          &it->origin, &it->offset);

            // add lib to all engines
            for (int i = 0; i < mic_engines_total; i++) {
                mic_engines[i].add_lib(*it);
            }
        }

        __target_libs = false;
        __target_libs_list.clear();
    }

    return is_available;
}

extern "C" bool __offload_target_image_is_executable(const void *target_image)
{
    const struct Image *image = static_cast<const struct Image*>(target_image);

    // decode image
    const char *name = image->data;
    const void *data = image->data + strlen(image->data) + 1;

    // determine image type
    const Elf64_Ehdr *hdr = static_cast<const Elf64_Ehdr*>(data);
    return (hdr->e_type == ET_EXEC);
}

extern "C" bool __offload_register_image(const void *target_image)
{
    const struct Image *image = static_cast<const struct Image*>(target_image);

    // decode image
    const char *name = image->data;
    const void *data = image->data + strlen(image->data) + 1;
    uint64_t    size = image->size;
    char *origin     = (char *) malloc(strlen(image->data) + 1);
    uint64_t    offset = 0;
    const char *host_name = image->data;
    int        i;

    if (origin == NULL)
        LIBOFFLOAD_ERROR(c_malloc);

    // The origin name is the name of the file on the host
    // this is used by Vtune, since it is a fat binary we
    // use the host file name of the fat binary.
    // Driver prepends the host file name ending with "?"
    // to the image->data name so need to extract the string
    i = 0;
    while (*host_name != '\0'  && *host_name != '?') {
       origin[i] = *host_name;
       host_name++;
       i++;
    }
    origin[i] = '\0';
    // Implies the host name does not exist which really should
    // not occur. Allow this since only consumer is Vtune.
    if ((i == 0) || (*host_name != '?')) {
       free(origin);
       origin = 0;
    }

    // our actions depend on the image type
    const Elf64_Ehdr *hdr = static_cast<const Elf64_Ehdr*>(data);
    switch (hdr->e_type) {
        case ET_EXEC:
            // Each offload application is supposed to have only one target
            // image representing target executable.
            // No thread synchronization is required here as the initialization
            // code is always executed in a single thread.
            if (__target_exe != 0) {
                LIBOFFLOAD_ERROR(c_multiple_target_exes);
                exit(1);
            }
            __target_exe = new TargetImage(name, data, size, origin, offset);

            // Registration code for execs is always called from the context
            // of main and thus we can safely call any function here,
            // including LoadLibrary API on windows. This is the place where
            // we do the offload library initialization.
            if (__offload_init_library()) {
                // initialize engine if init_type is on_start
                if (__offload_init_type == c_init_on_start) {
                    for (int i = 0; i < mic_engines_total; i++) {
                        mic_engines[i].init();
                    }
                }
            }
            return mic_engines_total > 0;

        case ET_DYN:
        {
		char *fullname = origin;
                // We add the library to a list of pending libraries
                __target_libs_lock.lock();
                __target_libs = true;
                __target_libs_list.push_back(
                    TargetImage(name, data, size, fullname, offset));
                __target_libs_lock.unlock();
                // If __target_exe is set, then main has started running
                // If not main, then we can't do anything useful here
                // because this registration code is called from DllMain
                // context (on windows).
                if (__target_exe != 0) {
                    // There is no need to delay loading the library
                    if (!__offload_init_library()) {
                        // Couldn't validate library as a fat offload library
                        LIBOFFLOAD_ERROR(c_unknown_binary_type);
                        exit(1);
                    }
                }
            return true;
        }

        default:
            // something is definitely wrong, issue an error and exit
            LIBOFFLOAD_ERROR(c_unknown_binary_type);
            exit(1);
    }
}

extern "C" void __offload_unregister_image(const void *target_image)
{
    // Target image is packed as follows:
    //      8 bytes                - size of the target binary
    //      null-terminated string - binary name
    //      <size> bytes           - binary contents
    const struct Image {
         int64_t size;
         char data[];
    } *image = static_cast<const struct Image*>(target_image);

    // decode image
    const char *name = image->data;
    const void *data = image->data + strlen(image->data) + 1;

    // our actions depend on the image type
    const Elf64_Ehdr *hdr = static_cast<const Elf64_Ehdr*>(data);
    if (hdr->e_type == ET_EXEC) {
        // We are executing exec's desctructors.
        // It is time to do a library cleanup.
        if (timer_enabled) {
            Offload_Timer_Print();
        }

#ifdef MYO_SUPPORT
        __offload_myoFini();
#endif // MYO_SUPPORT

        __offload_fini_library();
    }
    else if (hdr->e_type == ET_DYN) {
        for (int i = 0; i < mic_engines_total; i++) {
           mic_engines[i].unload_library(data, name);
        }

    }
}

extern "C" void __offload_register_task_callback(void (*cb)(void *))
{
    task_completion_callback = cb;
}

// Runtime trace interface for user programs

void __offload_console_trace(int level)
{
    console_enabled = level;
}

// User-visible offload API

int _Offload_number_of_devices(void)
{
    __offload_init_library();
    return mic_engines_total;
}

int _Offload_get_device_number(void)
{
    return -1;
}

int _Offload_get_physical_device_number(void)
{
    return -1;
}

int _Offload_signaled(int index, void *signal)
{
    __offload_init_library();

    // check index value
    if (index < 0) {
        LIBOFFLOAD_ERROR(c_offload_signaled1, index);
        LIBOFFLOAD_ABORT;
    }

    index %= mic_engines_total;

    // find associated async task
    OffloadDescriptor *task =
        mic_engines[index].find_signal(signal, false);
    if (task == 0) {
        LIBOFFLOAD_ERROR(c_offload_signaled2, signal);
        LIBOFFLOAD_ABORT;
    }
    // if signal is removed by wait completing
    else if (task == SIGNAL_IS_REMOVED) {
        return (true);
    }
    return task->is_signaled();
}

void _Offload_report(int val)
{
    if (val == OFFLOAD_REPORT_ON ||
        val == OFFLOAD_REPORT_OFF) {
        offload_report_enabled = val;
    }
}

int _Offload_find_associated_mic_memory(
    int          target,
    const void*  cpu_addr,
    void**       cpu_base_addr,
    uint64_t*    buf_length,
    void**       mic_addr,
    uint64_t*    mic_buf_start_offset,
    int*         is_static
)
{
    __offload_init_library();

    // check target value
    if (target < 0) {
        LIBOFFLOAD_ERROR(c_offload_signaled1, target);
        LIBOFFLOAD_ABORT;
    }
    target %= mic_engines_total;

    // find existing association in pointer table
    PtrData* ptr_data = mic_engines[target].find_ptr_data(cpu_addr);
    if (ptr_data == 0) {
        OFFLOAD_TRACE(3, "Association does not exist\n");
        return 0;
    }

    OFFLOAD_TRACE(3, "Found association: base %p, length %lld, is_static %d\n",
                  ptr_data->cpu_addr.start(), ptr_data->cpu_addr.length(),
                  ptr_data->is_static);

    if (ptr_data->mic_buf != 0 && ptr_data->mic_addr == 0) {
        COIRESULT res = COI::BufferGetSinkAddress(ptr_data->mic_buf,
                                                  &ptr_data->mic_addr);
        if (res != COI_SUCCESS) {
            return 0;
        }
    }
    *cpu_base_addr = const_cast<void *>(ptr_data->cpu_addr.start());
    *buf_length = ptr_data->cpu_addr.length() - ptr_data->alloc_disp;
    *mic_addr = (void *)(ptr_data->mic_addr + ptr_data->mic_offset);
    *mic_buf_start_offset = ptr_data->alloc_disp;
    *is_static = ptr_data->is_static;
    return ptr_data->is_static ? 1 : ptr_data->get_reference();
}

_Offload_stream _Offload_stream_create(
    int device,           // MIC device number
    int number_of_cpus    // Cores allocated to the stream
    )
{
    __offload_init_library();

    // check target value
    if (device < 0) {
        LIBOFFLOAD_ERROR(c_offload_signaled1, device);
        LIBOFFLOAD_ABORT;
    }
    device %= mic_engines_total;

    // Create new stream and get its handle
    _Offload_stream handle = Stream::add_stream(device, number_of_cpus);
    if (handle == 0) {
        OFFLOAD_TRACE(3, "Can't create stream\n");
        return 0;
    }

    // create pipeline associated with the new stream
    mic_engines[device].get_pipeline(handle);

    return(handle);
}

int _Offload_stream_destroy(
    int             device,   // MIC device number
    _Offload_stream handle    // stream to destroy
    )
{
    __offload_init_library();

    // check target value
    if (device < 0) {
        LIBOFFLOAD_ERROR(c_offload_signaled1, device);
        LIBOFFLOAD_ABORT;
    }
    device %= mic_engines_total;

    mic_engines[device].stream_destroy(handle);

    return(true);
}

int _Offload_stream_completed(int device, _Offload_stream handler)
{
    __offload_init_library();

    // check index value
    if (device < 0) {
        LIBOFFLOAD_ERROR(c_offload_signaled1, device);
        LIBOFFLOAD_ABORT;
    }

    device %= mic_engines_total;

    // get stream
    Stream * stream;

    if (handler != 0) {
        stream =  Stream::find_stream(handler, false);

        // the stream was not created or was destroyed
        if (!stream) {
            LIBOFFLOAD_ERROR(c_offload_no_stream, device);
            LIBOFFLOAD_ABORT;
        }

        // find associated async task
        OffloadDescriptor *task = stream->get_last_offload();

        // offload was completed by offload_wait pragma or wait clause
        if (task == 0) {
            return(true);
        }
        return task->is_signaled();
    }
    // zero handler is for all streams at the device
    else {
        StreamMap stream_map = Stream::all_streams;
        for (StreamMap::iterator it = stream_map.begin();
            it != stream_map.end(); it++) {
            Stream * stream = it->second;
            // find associated async task
            OffloadDescriptor *task = stream->get_last_offload();

            // offload was completed by offload_wait pragma or wait clause
            if (task == 0) {
                return(true);
            }
            // if even one stream is not completed result is false
            if (!task->is_signaled()) {
                return false;
            }
        }
        // no uncompleted streams
        return true;
    }
}

// IDB support
int   __dbg_is_attached = 0;
int   __dbg_target_id = -1;
pid_t __dbg_target_so_pid = -1;
char  __dbg_target_exe_name[MAX_TARGET_NAME] = {0};
const int __dbg_api_major_version = 1;
const int __dbg_api_minor_version = 0;

void __dbg_target_so_loaded()
{
}
void __dbg_target_so_unloaded()
{
}
