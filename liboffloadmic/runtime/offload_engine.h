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


#ifndef OFFLOAD_ENGINE_H_INCLUDED
#define OFFLOAD_ENGINE_H_INCLUDED

#include <limits.h>
#include <bitset>
#include <list>
#include <set>
#include <map>
#include "offload_common.h"
#include "coi/coi_client.h"

#define SIGNAL_HAS_COMPLETED ((OffloadDescriptor *)-1)
const int64_t no_stream = -1;

// Address range
class MemRange {
public:
    MemRange() : m_start(0), m_length(0) {}
    MemRange(const void *addr, uint64_t len) : m_start(addr), m_length(len) {}

    const void* start() const {
        return m_start;
    }

    const void* end() const {
        return static_cast<const char*>(m_start) + m_length;
    }

    uint64_t length() const {
        return m_length;
    }

    // returns true if given range overlaps with another one
    bool overlaps(const MemRange &o) const {
        // Two address ranges A[start, end) and B[start,end) overlap
        // if A.start < B.end and A.end > B.start.
        return start() < o.end() && end() > o.start();
    }

    // returns true if given range contains the other range
    bool contains(const MemRange &o) const {
        return start() <= o.start() && o.end() <= end();
    }

private:
    const void* m_start;
    uint64_t    m_length;
};

// Data associated with a pointer variable
class PtrData {
public:
    PtrData(const void *addr, uint64_t len) :
        cpu_addr(addr, len), cpu_buf(0),
        mic_addr(0), alloc_disp(0), mic_buf(0), mic_offset(0),
        ref_count(0), is_static(false), is_omp_associate(false)
    {}

    //
    // Copy constructor
    //
    PtrData(const PtrData& ptr):
        cpu_addr(ptr.cpu_addr), cpu_buf(ptr.cpu_buf),
        mic_addr(ptr.mic_addr), alloc_disp(ptr.alloc_disp),
        mic_buf(ptr.mic_buf), mic_offset(ptr.mic_offset),
        ref_count(ptr.ref_count), is_static(ptr.is_static),
        is_omp_associate(ptr.is_omp_associate),
        var_alloc_type(0)
    {}

    bool operator<(const PtrData &o) const {
        // Variables are sorted by the CPU start address.
        // Overlapping memory ranges are considered equal.
        return (cpu_addr.start() < o.cpu_addr.start()) &&
               !cpu_addr.overlaps(o.cpu_addr);
    }

    long add_reference() {
        if (is_omp_associate || (is_static && !var_alloc_type)) {
            return LONG_MAX;
        }
#ifndef TARGET_WINNT
        return __sync_fetch_and_add(&ref_count, 1);
#else // TARGET_WINNT
        return _InterlockedIncrement(&ref_count) - 1;
#endif // TARGET_WINNT
    }

    long remove_reference() {
        if (is_omp_associate || (is_static && !var_alloc_type)) {
            return LONG_MAX;
        }
#ifndef TARGET_WINNT
        return __sync_sub_and_fetch(&ref_count, 1);
#else // TARGET_WINNT
        return _InterlockedDecrement(&ref_count);
#endif // TARGET_WINNT
    }

    long get_reference() const {
        if (is_omp_associate || (is_static && !var_alloc_type)) {
            return LONG_MAX;
        }
        return ref_count;
    }

public:
    // CPU address range
    const MemRange  cpu_addr;

    // CPU and MIC buffers
    COIBUFFER       cpu_buf;
    COIBUFFER       mic_buf;

    // placeholder for buffer address on mic
    uint64_t        mic_addr;

    uint64_t        alloc_disp;

    // additional offset to pointer data on MIC for improving bandwidth for
    // data which is not 4K aligned
    uint32_t        mic_offset;

    // if true buffers are created from static memory
    bool            is_static;

    // true if MIC buffer created by omp_target_associate
    bool            is_omp_associate;

    bool            var_alloc_type;
    mutex_t         alloc_ptr_data_lock;

private:
    // reference count for the entry
    long            ref_count;
};

typedef std::list<PtrData*> PtrDataList;

class PtrDataTable {
public:
    typedef std::set<PtrData> PtrSet;

    PtrData* find_ptr_data(const void *ptr) {
        m_ptr_lock.lock();
        PtrSet::iterator res = list.find(PtrData(ptr, 0));

        m_ptr_lock.unlock();
        if (res == list.end()) {
            return 0;
        }
        return const_cast<PtrData*>(res.operator->());
    }

    PtrData* insert_ptr_data(const void *ptr, uint64_t len, bool &is_new) {
        m_ptr_lock.lock();
        std::pair<PtrSet::iterator, bool> res =
            list.insert(PtrData(ptr, len));

        PtrData* ptr_data = const_cast<PtrData*>(res.first.operator->());
        m_ptr_lock.unlock();

        is_new = res.second;
        if (is_new) {
            // It's necessary to lock as soon as possible.
            // unlock must be done at call site of insert_ptr_data at
            // branch for is_new
            ptr_data->alloc_ptr_data_lock.lock();
        }
        return ptr_data;
    }

    void remove_ptr_data(const void *ptr) {
        m_ptr_lock.lock();
        list.erase(PtrData(ptr, 0));
        m_ptr_lock.unlock();
    }
private:

    PtrSet list;
    mutex_t     m_ptr_lock;
};

// Data associated with automatic variable
class AutoData {
public:
    AutoData(const void *addr, uint64_t len) :
        cpu_addr(addr, len), ref_count(0)
    {}

    bool operator<(const AutoData &o) const {
        // Variables are sorted by the CPU start address.
        // Overlapping memory ranges are considered equal.
        return (cpu_addr.start() < o.cpu_addr.start()) &&
               !cpu_addr.overlaps(o.cpu_addr);
    }

    long add_reference() {
#ifndef TARGET_WINNT
        return __sync_fetch_and_add(&ref_count, 1);
#else // TARGET_WINNT
        return _InterlockedIncrement(&ref_count) - 1;
#endif // TARGET_WINNT
    }

    long remove_reference() {
#ifndef TARGET_WINNT
        return __sync_sub_and_fetch(&ref_count, 1);
#else // TARGET_WINNT
        return _InterlockedDecrement(&ref_count);
#endif // TARGET_WINNT
    }
    
    long nullify_reference() {
#ifndef TARGET_WINNT
        return __sync_lock_test_and_set(&ref_count, 0);
#else // TARGET_WINNT
        return _InterlockedExchange(&ref_count,0);
#endif // TARGET_WINNT
    }
    
    long get_reference() const {
        return ref_count;
    }

public:
    // CPU address range
    const MemRange cpu_addr;

private:
    // reference count for the entry
    long ref_count;
};

// Set of autimatic variables
typedef std::set<AutoData> AutoSet;

// Target image data
struct TargetImage
{
    TargetImage(const char *_name, const void *_data, uint64_t _size,
                const char *_origin, uint64_t _offset) :
        name(_name), data(_data), size(_size),
        origin(_origin), offset(_offset)
    {}

    // library name
    const char* name;

    // contents and size
    const void* data;
    uint64_t    size;

    // file of origin and offset within that file
    const char* origin;
    uint64_t    offset;
};

typedef std::list<TargetImage> TargetImageList;

// dynamic library and Image associated with lib
struct DynLib
{
    DynLib(const char *_name, const void *_data,
           COILIBRARY _lib) :
        name(_name), data(_data), lib(_lib)
    {}
    // library name
    const char* name;

    // contents
    const void* data;
 
    COILIBRARY lib;
};
typedef std::list<DynLib> DynLibList;

// Data associated with persistent auto objects
struct PersistData
{
    PersistData(const void *addr, uint64_t routine_num,
                uint64_t size, uint64_t thread) :
        stack_cpu_addr(addr), routine_id(routine_num), thread_id(thread)
    {
        stack_ptr_data = new PtrData(0, size);
    }
    // 1-st key value - beginning of the stack at CPU
    const void *   stack_cpu_addr;
    // 2-nd key value - identifier of routine invocation at CPU
    uint64_t   routine_id;
    // 3-rd key value - thread identifier
    uint64_t   thread_id;

    // corresponded PtrData; only stack_ptr_data->mic_buf is used
    PtrData * stack_ptr_data;
    // used to get offset of the variable in stack buffer
    char * cpu_stack_addr;
};

typedef std::list<PersistData> PersistDataList;

// Data associated with stream
struct Stream
{
    Stream(int device, int num_of_cpus) :
       m_number_of_cpus(num_of_cpus), m_pipeline(0), m_last_offload(0),
       m_device(device)
    {}
    ~Stream() {
        if (m_pipeline) {
             COI::PipelineDestroy(m_pipeline);
        }
    }

    COIPIPELINE get_pipeline(void) {
        return(m_pipeline);
    }

    int get_device(void) {
        return(m_device);
    }

    int get_cpu_number(void) {
        return(m_number_of_cpus);
    }

    void set_pipeline(COIPIPELINE pipeline) {
        m_pipeline = pipeline;
    }

    OffloadDescriptor* get_last_offload(void) {
        return(m_last_offload);
    }

    void set_last_offload(OffloadDescriptor*   last_offload) {
        m_last_offload = last_offload;
    }

    static Stream* find_stream(uint64_t handle, bool remove);

    static _Offload_stream  add_stream(int device, int number_of_cpus) {
        _Offload_stream result;
        m_stream_lock.lock();
        result = ++m_streams_count;
        all_streams[m_streams_count] = new Stream(device, number_of_cpus);
        m_stream_lock.unlock();
        return(result);
    }

    static uint64_t get_streams_count() {
        return m_streams_count;
    }

    typedef std::map<uint64_t, Stream*> StreamMap;

    static uint64_t  m_streams_count;
    static StreamMap all_streams;
    static mutex_t   m_stream_lock;

    int m_device;

    // number of cpus
    int m_number_of_cpus;

    // The pipeline associated with the stream
    COIPIPELINE         m_pipeline;

    // The last offload occured via the stream
    OffloadDescriptor*  m_last_offload;

    // Cpus used by the stream
    std::bitset<COI_MAX_HW_THREADS> m_stream_cpus;
};

typedef std::map<uint64_t, Stream*> StreamMap;
typedef std::bitset<COI_MAX_HW_THREADS> micLcpuMask;

// ordered by count double linked list of cpus used by streams
typedef struct CpuEl{
    uint64_t      count; // number of streams using the cpu
    struct CpuEl* prev;  // cpu with the same or lesser count
    struct CpuEl* next;  // cpu with the same or greater count
} CpuEl;

// class representing a single engine
struct Engine {
    friend void __offload_init_library_once(void);
    friend void __offload_fini_library(void);

#define CPU_INDEX(x) (x - m_cpus)
#define check_result(res, tag, ...) \
    { \
        if (res == COI_PROCESS_DIED) { \
            fini_process(true); \
            exit(1); \
        } \
        if (res != COI_SUCCESS) { \
            __liboffload_error_support(tag, __VA_ARGS__); \
            exit(1); \
        } \
    }

    int get_logical_index() const {
        return m_index;
    }

    int get_physical_index() const {
        return m_physical_index;
    }

    const COIPROCESS& get_process() const {
        return m_process;
    }

    bool get_ready() {
        return m_ready;
    }

    uint64_t get_thread_id(void);

    // initialize device
    void init(void);

    // unload library
    void unload_library(const void *data, const char *name);

    // add new library
    void add_lib(const TargetImage &lib)
    {
        m_lock.lock();
        m_ready = false;
        m_images.push_back(lib);
        m_lock.unlock();
    }

    COIRESULT compute(
        _Offload_stream     stream,
        const std::list<COIBUFFER> &buffers,
        const void*         data,
        uint16_t            data_size,
        void*               ret,
        uint16_t            ret_size,
        uint32_t            num_deps,
        const COIEVENT*     deps,
        COIEVENT*           event
    );

#ifdef MYO_SUPPORT
    // temporary workaround for blocking behavior for myoiLibInit/Fini calls
    void init_myo(COIEVENT *event) {
        COIRESULT res;
        res = COI::PipelineRunFunction(get_pipeline(),
                                       m_funcs[c_func_myo_init],
                                       0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       event);
        check_result(res, c_pipeline_run_func, m_index, res);
    }

    void fini_myo(COIEVENT *event) {
        COIRESULT res;
        res = COI::PipelineRunFunction(get_pipeline(),
                                       m_funcs[c_func_myo_fini],
                                       0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       event);
        check_result(res, c_pipeline_run_func, m_index, res);
    }
#endif // MYO_SUPPORT

    //
    // Memory association table
    //
    PtrData* find_ptr_data(const void *ptr) {
        return m_ptr_set.find_ptr_data(ptr);
    }

    PtrData* find_targetptr_data(const void *ptr) {
        return m_targetptr_set.find_ptr_data(ptr);
    }

    PtrData* insert_ptr_data(const void *ptr, uint64_t len, bool &is_new) {
        return m_ptr_set.insert_ptr_data(ptr, len, is_new);
    }

    PtrData* insert_targetptr_data(const void *ptr, uint64_t len,
                                   bool &is_new) {
        return m_targetptr_set.insert_ptr_data(ptr, len, is_new);
    }

    void remove_ptr_data(const void *ptr) {
        m_ptr_set.remove_ptr_data(ptr);
    }

    void remove_targetptr_data(const void *ptr) {
        m_targetptr_set.remove_ptr_data(ptr);
    }

    //
    // Automatic variables
    //
    AutoData* find_auto_data(const void *ptr) {
        AutoSet &auto_vars = get_auto_vars();
        AutoSet::iterator res = auto_vars.find(AutoData(ptr, 0));
        if (res == auto_vars.end()) {
            return 0;
        }
        return const_cast<AutoData*>(res.operator->());
    }

    AutoData* insert_auto_data(const void *ptr, uint64_t len) {
        AutoSet &auto_vars = get_auto_vars();
        std::pair<AutoSet::iterator, bool> res =
            auto_vars.insert(AutoData(ptr, len));
        return const_cast<AutoData*>(res.first.operator->());
    }

    void remove_auto_data(const void *ptr) {
        get_auto_vars().erase(AutoData(ptr, 0));
    }

    //
    // Signals
    //
    void add_signal(const void *signal, OffloadDescriptor *desc) {
        m_signal_lock.lock();
        m_signal_map[signal] = desc;
        m_signal_lock.unlock();
    }

    OffloadDescriptor* find_signal(const void *signal, bool remove) {
        OffloadDescriptor *desc = 0;

        m_signal_lock.lock();
        {
            SignalMap::iterator it = m_signal_map.find(signal);
            if (it != m_signal_map.end()) {
                desc = it->second;
                if (remove) {
                    it->second = SIGNAL_HAS_COMPLETED;
                }
            }
        }
        m_signal_lock.unlock();

        return desc;
    }

    void complete_signaled_ofld(const void *signal) {

        m_signal_lock.lock();
        {
            SignalMap::iterator it = m_signal_map.find(signal);
            if (it != m_signal_map.end()) {
                it->second = SIGNAL_HAS_COMPLETED;
            }
        }
        m_signal_lock.unlock();
    }

    void stream_destroy(_Offload_stream handle);

    void move_cpu_el_after(CpuEl* cpu_what, CpuEl* cpu_after);
    void print_stream_cpu_list(const char *);

    COIPIPELINE get_pipeline(_Offload_stream stream);

    StreamMap get_stream_map() {
        return m_stream_map;
    }

    // stop device process
    void fini_process(bool verbose);

    // list of stacks active at the engine
    PersistDataList m_persist_list;

private:
    Engine() : m_index(-1), m_physical_index(-1), m_process(0), m_ready(false),
               m_proc_number(0), m_assigned_cpus(0), m_cpus(0), m_cpu_head(0)
    {}

    ~Engine() {
        m_ready = false;
        for (StreamMap::iterator it = m_stream_map.begin();
             it != m_stream_map.end(); it++) {
            Stream * stream = it->second;
            delete stream;
        }
        if (m_process != 0) {
            fini_process(false);
        }
        if (m_assigned_cpus) {
            delete m_assigned_cpus;
        }
    }

    // set indexes
    void set_indexes(int logical_index, int physical_index) {
        m_index = logical_index;
        m_physical_index = physical_index;
    }

    // set CPU mask
    void set_cpu_mask(micLcpuMask *cpu_mask)
    {
        m_assigned_cpus = cpu_mask;
    }

    // start process on device
    void init_process();

    void load_libraries(void);
    void init_ptr_data(void);

    // performs library intialization on the device side
    pid_t init_device(void);

private:
    // get pipeline associated with a calling thread
    COIPIPELINE get_pipeline(void);

    // get automatic vars set associated with the calling thread
    AutoSet& get_auto_vars(void);

    // destructor for thread data
    static void destroy_thread_data(void *data);

private:
    typedef std::set<PtrData> PtrSet;
    typedef std::map<const void*, OffloadDescriptor*> SignalMap;

    // device indexes
    int         m_index;
    int         m_physical_index;

    // cpu mask
    micLcpuMask *m_assigned_cpus;

    // number of COI pipes created for the engine
    long        m_proc_number;

    // process handle
    COIPROCESS  m_process;

    // If false, device either has not been initialized or new libraries
    // have been added.
    bool        m_ready;
    mutex_t     m_lock;

    // List of libraries to be loaded
    TargetImageList m_images;

    // var tables
    PtrDataTable m_ptr_set;
    PtrDataTable m_targetptr_set;

    // signals
    SignalMap m_signal_map;
    mutex_t   m_signal_lock;

    // streams
    StreamMap   m_stream_map;
    mutex_t     m_stream_lock;
    int         m_num_cores;
    int         m_num_threads;
    CpuEl*      m_cpus;
    CpuEl*      m_cpu_head;

    // List of dynamic libraries to be registred
    DynLibList m_dyn_libs;

    // constants for accessing device function handles
    enum {
        c_func_compute = 0,
#ifdef MYO_SUPPORT
        c_func_myo_init,
        c_func_myo_fini,
#endif // MYO_SUPPORT
        c_func_init,
        c_func_var_table_size,
        c_func_var_table_copy,
        c_func_set_stream_affinity,
        c_funcs_total
    };
    static const char* m_func_names[c_funcs_total];

    // device function handles
    COIFUNCTION m_funcs[c_funcs_total];

    // int -> name mapping for device signals
    static const int   c_signal_max = 32;
    static const char* c_signal_names[c_signal_max];
};

#endif // OFFLOAD_ENGINE_H_INCLUDED
