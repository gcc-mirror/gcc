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


/*! \file
    \brief The parts of the runtime library used only on the host
*/

#ifndef OFFLOAD_HOST_H_INCLUDED
#define OFFLOAD_HOST_H_INCLUDED

#ifndef TARGET_WINNT
#include <unistd.h>
#endif // TARGET_WINNT
#include "offload_common.h"
#include "offload_util.h"
#include "offload_engine.h"
#include "offload_env.h"
#include "offload_orsl.h"
#include "coi/coi_client.h"

// MIC engines.
DLL_LOCAL extern Engine*  mic_engines;
DLL_LOCAL extern uint32_t mic_engines_total;

// DMA channel count used by COI and set via
// OFFLOAD_DMA_CHANNEL_COUNT environment variable
DLL_LOCAL extern uint32_t mic_dma_channel_count;

//! The target image is packed as follows.
/*!      1. 8 bytes containing the size of the target binary          */
/*!      2. a null-terminated string which is the binary name         */
/*!      3. <size> number of bytes that are the contents of the image */
/*!      The address of symbol __offload_target_image
             is the address of this structure.                        */
struct Image {
     int64_t size; //!< Size in bytes of the target binary name and contents
     char data[];  //!< The name and contents of the target image
};

// The offload descriptor.
class OffloadDescriptor
{
public:
    enum  OmpAsyncLastEventType {
        c_last_not,     // not last event
        c_last_write,   // the last event that is write
        c_last_read,    // the last event that is read
        c_last_runfunc  // the last event that is runfunction
    };
    
    OffloadDescriptor(
        int index,
        _Offload_status *status,
        bool is_mandatory,
        bool is_openmp,
        OffloadHostTimerData * timer_data
    ) :
        m_device(mic_engines[index == -1 ? 0 : index % mic_engines_total]),
        m_is_mandatory(is_mandatory),
        m_is_openmp(is_openmp),
        m_inout_buf(0),
        m_func_desc(0),
        m_func_desc_size(0),
        m_num_in_dependencies(0),
        m_p_in_dependencies(0),
        m_in_deps(0),
        m_in_deps_total(0),
        m_in_deps_allocated(0),        
        m_out_deps(0),
        m_out_deps_total(0),
        m_out_deps_allocated(0),
        m_vars(0),
        m_vars_extra(0),
        m_status(status),
        m_timer_data(timer_data),
        m_out_with_preallocated(false),
        m_preallocated_alloc(false),
        m_traceback_called(false),
        m_stream(-1),
        m_signal(0),
        m_has_signal(0),
        m_omp_async_last_event_type(c_last_not)
    {
        m_wait_all_devices = index == -1;
    }

    ~OffloadDescriptor()
    {
        if (m_in_deps != 0) {
            free(m_in_deps);
        }
        if (m_out_deps != 0) {
            free(m_out_deps);
        }
        if (m_func_desc != 0) {
            free(m_func_desc);
        }
        if (m_vars != 0) {
            free(m_vars);
            free(m_vars_extra);
        }
    }

    bool offload(const char *name, bool is_empty,
                 VarDesc *vars, VarDesc2 *vars2, int vars_total,
                 const void **waits, int num_waits, const void **signal,
                 int entry_id, const void *stack_addr,
                 OffloadFlags offload_flags);

    bool offload_finish(bool is_traceback);

    bool is_signaled();

    OffloadHostTimerData* get_timer_data() const {
        return m_timer_data;
    }

    void set_stream(_Offload_stream stream) {
        m_stream = stream;
    }

    _Offload_stream get_stream() {
        return(m_stream);
    }

    Engine& get_device() {
        return m_device;
    }

    void* get_signal() {
        return(m_signal);
    }

    void set_signal(const void* signal) {
        m_has_signal = 1;
        m_signal = const_cast<void*>(signal);
    }

    void cleanup();

    uint32_t  m_event_count;
    bool      m_has_signal;

private:
    bool offload_wrap(const char *name, bool is_empty,
                 VarDesc *vars, VarDesc2 *vars2, int vars_total,
                 const void **waits, int num_waits, const void **signal,
                 int entry_id, const void *stack_addr,
                 OffloadFlags offload_flags);
    bool wait_dependencies(const void **waits, int num_waits,
                           _Offload_stream stream);
    bool setup_descriptors(VarDesc *vars, VarDesc2 *vars2, int vars_total,
                           int entry_id, const void *stack_addr);
    bool setup_misc_data(const char *name);
    bool send_pointer_data(bool is_async, void* info);
    bool send_noncontiguous_pointer_data(
        int i,
        PtrData* src_buf,
        PtrData* dst_buf,
        COIEVENT *event,
        uint64_t  &sent_data,
        uint32_t in_deps_amount,
        COIEVENT *in_deps
        );
    bool receive_noncontiguous_pointer_data(
        int i,
        COIBUFFER dst_buf,
        COIEVENT *event,
        uint64_t  &received_data,
        uint32_t in_deps_amount,
        COIEVENT *in_deps
        );

    bool gather_copyin_data();

    bool compute(void *);

    bool receive_pointer_data(bool is_async, bool first_run, void * info);
    bool scatter_copyout_data();

    bool find_ptr_data(PtrData* &ptr_data, void *base, int64_t disp,
                       int64_t length, bool is_targptr,
                       bool error_does_not_exist = true);

    void find_device_ptr( int64_t* &device_ptr,
                       void *host_ptr);

    bool alloc_ptr_data(PtrData* &ptr_data, void *base, int64_t disp,
                        int64_t length, int64_t alloc_disp, int align,
                        bool is_targptr, bool is_prealloc, bool pin);
    bool create_preallocated_buffer(PtrData* ptr_data, void *base);
    bool init_static_ptr_data(PtrData *ptr_data);
    bool init_mic_address(PtrData *ptr_data);
    bool offload_stack_memory_manager(
        const void * stack_begin,
        int routine_id,
        int buf_size,
        int align,
        bool thread_specific_function_locals,
        bool *is_new);
    char *get_this_threads_cpu_stack_addr(
        const void * stack_begin,
        int routine_id,
        bool thread_specific_function_locals);
    PtrData *get_this_threads_mic_stack_addr(
        const void * stack_begin,
        int routine_id,
        bool thread_specific_function_locals);
    bool nullify_target_stack(COIBUFFER targ_buf, uint64_t size);

    bool gen_var_descs_for_pointer_array(int i);

    void get_stream_in_dependencies(uint32_t &in_deps_amount,
                                    COIEVENT* &in_deps);

    void report_coi_error(error_types msg, COIRESULT res);
    _Offload_result translate_coi_error(COIRESULT res) const;

    void setup_omp_async_info();

    void setup_use_device_ptr(int i);

    void register_event_call_back(void (*)(
                                      COIEVENT,
                                      const COIRESULT,
                                      const void*),
                                  const COIEVENT *event,
                                  const void *info);

    void register_omp_event_call_back(const COIEVENT *event, const void *info);

private:
    typedef std::list<COIBUFFER> BufferList;

    // extra data associated with each variable descriptor
    struct VarExtra {
        PtrData* src_data;
        PtrData* dst_data;
        AutoData* auto_data;
        int64_t cpu_disp;
        int64_t cpu_offset;
        void *alloc;
        union {
            CeanReadRanges *read_rng_src;
            NonContigDesc  *noncont_desc;
        };
        CeanReadRanges *read_rng_dst;
        int64_t ptr_arr_offset;
        bool is_arr_ptr_el;
        OmpAsyncLastEventType omp_last_event_type;
        int64_t pointer_offset;
        uint16_t type_src;
        uint16_t type_dst;
    };

    template<typename T> class ReadArrElements {
    public:
        ReadArrElements():
            ranges(NULL),
            el_size(sizeof(T)),
            offset(0),
            count(0),
            is_empty(true),
            base(NULL)
        {}

        bool read_next(bool flag)
        {
            if (flag != 0) {
                if (is_empty) {
                    if (ranges) {
                        if (!get_next_range(ranges, &offset)) {
                            // ranges are over
                            return false;
                        }
                    }
                    // all contiguous elements are over
                    else if (count != 0) {
                        return false;
                    }

                    length_cur = size;
                }
                else {
                    offset += el_size;
                }
                val = (T)get_el_value(base, offset, el_size);
                length_cur -= el_size;
                count++;
                is_empty = length_cur == 0;
            }
            return true;
        }
    public:
        CeanReadRanges * ranges;
        T       val;
        int     el_size;
        int64_t size,
                offset,
                length_cur;
        bool    is_empty;
        int     count;
        char   *base;
    };

    // ptr_data for persistent auto objects
    PtrData*    m_stack_ptr_data;
    PtrDataList m_destroy_stack;

    // Engine
    Engine& m_device;

    // true for offload_wait target(mic) stream(0)
    bool m_wait_all_devices;

    // if true offload is mandatory
    bool m_is_mandatory;

    // if true offload has openmp origin
    const bool m_is_openmp;

    // The Marshaller for the inputs of the offloaded region.
    Marshaller m_in;

    // The Marshaller for the outputs of the offloaded region.
    Marshaller m_out;

    // List of buffers that are passed to dispatch call
    BufferList m_compute_buffers;

    // List of buffers that need to be destroyed at the end of offload
    BufferList m_destroy_buffers;

    // Variable descriptors
    VarDesc*  m_vars;
    VarExtra* m_vars_extra;
    int       m_vars_total;

    // Pointer to a user-specified status variable
    _Offload_status *m_status;

    // Function descriptor
    FunctionDescriptor* m_func_desc;
    uint32_t            m_func_desc_size;

    // Buffer for transferring copyin/copyout data
    COIBUFFER m_inout_buf;


    // Dependencies
    COIEVENT *m_in_deps;
    uint32_t  m_in_deps_total;
    uint32_t  m_in_deps_allocated;    
    COIEVENT *m_out_deps;
    uint32_t  m_out_deps_total;
    uint32_t  m_out_deps_allocated;

    // 2 variables defines input dependencies for current COI API.
    // The calls to routines as BufferWrite/PipelineRunFunction/BufferRead
    // is supposed to have input dependencies.
    // 2 variables below defines the number and vector of dependencies
    // in every current moment of offload.
    // So any phase of offload can use its values as input dependencies 
    // for the COI API that the phase calls.
    // It means that all phases (of Write, RunFunction,Read) must keep
    // the variables correct to be used by following phase.
    // If some consequent offloads are connected (i.e. by the same stream)
    // the final 2 variables of the offload is used as initial inputs
    // for the next offload.
    uint32_t  m_num_in_dependencies;
    COIEVENT *m_p_in_dependencies;

    // Stream
    _Offload_stream m_stream;

    // Signal
    void* m_signal;

    // Timer data
    OffloadHostTimerData *m_timer_data;

    // copyin/copyout data length
    uint64_t m_in_datalen;
    uint64_t m_out_datalen;

    // a boolean value calculated in setup_descriptors. If true we need to do
    // a run function on the target. Otherwise it may be optimized away.
    bool m_need_runfunction;

    // initialized value of m_need_runfunction;
    // is used to recognize offload_transfer
    bool m_initial_need_runfunction;

    // a Boolean value set to true when OUT clauses with preallocated targetptr
    // is encountered to indicate that call receive_pointer_data needs to be
    // invoked again after call to scatter_copyout_data.
    bool m_out_with_preallocated;

    // a Boolean value set to true if an alloc_if(1) is used with preallocated
    // targetptr to indicate the need to scatter_copyout_data even for
    // async offload
    bool m_preallocated_alloc;

    // a Boolean value set to true if traceback routine is called
    bool m_traceback_called;  

    OmpAsyncLastEventType m_omp_async_last_event_type;
};

// Initialization types for MIC
enum OffloadInitType {
    c_init_on_start,         // all devices before entering main
    c_init_on_offload,       // single device before starting the first offload
    c_init_on_offload_all    // all devices before starting the first offload
};

// Determines if MIC code is an executable or a shared library
extern "C" bool __offload_target_image_is_executable(const void *target_image);

// Initializes library and registers specified offload image.
extern "C" bool __offload_register_image(const void* image);
extern "C" void __offload_unregister_image(const void* image);

// Registers asynchronous task completion callback
extern "C" void __offload_register_task_callback(void (*cb)(void *));

// Initializes offload runtime library.
DLL_LOCAL extern int __offload_init_library(void);

// thread data for associating pipelines with threads
DLL_LOCAL extern pthread_key_t mic_thread_key;

// location of offload_main executable
// To be used if the main application has no offload and is not built
// with -offload but dynamic library linked in has offload pragma
DLL_LOCAL extern char* mic_device_main;

// Environment variables for devices
DLL_LOCAL extern MicEnvVar mic_env_vars;

// CPU frequency
DLL_LOCAL extern uint64_t cpu_frequency;

// LD_LIBRARY_PATH for KNC libraries
DLL_LOCAL extern char* knc_library_path;

// LD_LIBRARY_PATH for KNL libraries
DLL_LOCAL extern char* knl_library_path;

// stack size for target
DLL_LOCAL extern uint32_t mic_stack_size;

// Preallocated memory size for buffers on MIC
DLL_LOCAL extern uint64_t mic_buffer_size;

// Preallocated 4K page memory size for buffers on MIC
DLL_LOCAL extern uint64_t mic_4k_buffer_size;

// Preallocated 2M page memory size for buffers on MIC
DLL_LOCAL extern uint64_t mic_2m_buffer_size;

// Setting controlling inout proxy
DLL_LOCAL extern bool  mic_proxy_io;
DLL_LOCAL extern char* mic_proxy_fs_root;

// Threshold for creating buffers with large pages
DLL_LOCAL extern uint64_t __offload_use_2mb_buffers;

// offload initialization type
DLL_LOCAL extern OffloadInitType __offload_init_type;

// Device number to offload to when device is not explicitly specified.
DLL_LOCAL extern int __omp_device_num;

// target executable
DLL_LOCAL extern TargetImage* __target_exe;

// is true if last loaded image is dll
DLL_LOCAL extern bool __current_image_is_dll;
// is true if myo library is loaded when dll is loaded
DLL_LOCAL extern bool __myo_init_in_so;

// IDB support

// Called by the offload runtime after initialization of offload infrastructure
// has been completed.
extern "C" void  __dbg_target_so_loaded();

// Called by the offload runtime when the offload infrastructure is about to be
// shut down, currently at application exit.
extern "C" void  __dbg_target_so_unloaded();

// Null-terminated string containing path to the process image of the hosting
// application (offload_main)
#define MAX_TARGET_NAME 512
extern "C" char  __dbg_target_exe_name[MAX_TARGET_NAME];

// Integer specifying the process id
extern "C" pid_t __dbg_target_so_pid;

// Integer specifying the 0-based device number
extern "C" int   __dbg_target_id;

// Set to non-zero by the host-side debugger to enable offload debugging
// support
extern "C" int   __dbg_is_attached;

// Major version of the debugger support API
extern "C" const int __dbg_api_major_version;

// Minor version of the debugger support API
extern "C" const int __dbg_api_minor_version;

#endif // OFFLOAD_HOST_H_INCLUDED
