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


#include "offload_engine.h"
#include <signal.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <algorithm>
#include <vector>

#include "offload_host.h"
#include "offload_table.h"
#include "offload_iterator.h"

#if defined(HOST_WINNT)
#define PATH_SEPARATOR ";"
#else
#define PATH_SEPARATOR ":"
#endif

// Static members of Stream class must be described somewhere.
// This members describe the list of all streams defined in programm
// via call to _Offload_stream_create.
uint64_t  Stream::m_streams_count = 0;
StreamMap Stream::all_streams;
mutex_t   Stream::m_stream_lock;
char*     mic_library_path = 0;

const char* Engine::m_func_names[Engine::c_funcs_total] =
{
    "server_compute",
#ifdef MYO_SUPPORT
    "server_myoinit",
    "server_myofini",
#endif // MYO_SUPPORT
    "server_init",
    "server_var_table_size",
    "server_var_table_copy",
    "server_set_stream_affinity"
};

// Symbolic representation of system signals. Fix for CQ233593
const char* Engine::c_signal_names[Engine::c_signal_max] =
{
    "Unknown SIGNAL",
    "SIGHUP",    /*  1, Hangup (POSIX).  */
    "SIGINT",    /*  2, Interrupt (ANSI).  */
    "SIGQUIT",   /*  3, Quit (POSIX).  */
    "SIGILL",    /*  4, Illegal instruction (ANSI).  */
    "SIGTRAP",   /*  5, Trace trap (POSIX).  */
    "SIGABRT",   /*  6, Abort (ANSI).  */
    "SIGBUS",    /*  7, BUS error (4.2 BSD).  */
    "SIGFPE",    /*  8, Floating-point exception (ANSI).  */
    "SIGKILL",   /*  9, Kill, unblockable (POSIX).  */
    "SIGUSR1",   /* 10, User-defined signal 1 (POSIX).  */
    "SIGSEGV",   /* 11, Segmentation violation (ANSI).  */
    "SIGUSR2",   /* 12, User-defined signal 2 (POSIX).  */
    "SIGPIPE",   /* 13, Broken pipe (POSIX).  */
    "SIGALRM",   /* 14, Alarm clock (POSIX).  */
    "SIGTERM",   /* 15, Termination (ANSI).  */
    "SIGSTKFLT", /* 16, Stack fault.  */
    "SIGCHLD",   /* 17, Child status has changed (POSIX).  */
    "SIGCONT",   /* 18, Continue (POSIX).  */
    "SIGSTOP",   /* 19, Stop, unblockable (POSIX).  */
    "SIGTSTP",   /* 20, Keyboard stop (POSIX).  */
    "SIGTTIN",   /* 21, Background read from tty (POSIX).  */
    "SIGTTOU",   /* 22, Background write to tty (POSIX).  */
    "SIGURG",    /* 23, Urgent condition on socket (4.2 BSD).  */
    "SIGXCPU",   /* 24, CPU limit exceeded (4.2 BSD).  */
    "SIGXFSZ",   /* 25, File size limit exceeded (4.2 BSD).  */
    "SIGVTALRM", /* 26, Virtual alarm clock (4.2 BSD).  */
    "SIGPROF",   /* 27, Profiling alarm clock (4.2 BSD).  */
    "SIGWINCH",  /* 28, Window size change (4.3 BSD, Sun).  */
    "SIGIO",     /* 29, I/O now possible (4.2 BSD).  */
    "SIGPWR",    /* 30, Power failure restart (System V).  */
    "SIGSYS"     /* 31, Bad system call.  */
};

void Engine::init(void)
{
    if (!m_ready) {
        mutex_locker_t locker(m_lock);

        if (!m_ready) {
            // start process if not done yet
            if (m_process == 0) {
                init_process();
            }

            // load penging images
            load_libraries();

            // and (re)build pointer table
            init_ptr_data();

            // it is ready now
            m_ready = true;
           
            //  Inform the debugger 
            if (__dbg_is_attached) {
                __dbg_target_so_loaded();
            }
        }
    }
}

void Engine::print_stream_cpu_list(const char * str)
{
    int count = 0;
    char buffer[1024];
    CpuEl* cpu_el = m_cpu_head;
       
    OFFLOAD_DEBUG_TRACE(3,
                  "%s : cpu list as Index(Count) for the streams is :\n", str);
    buffer[0] = 0;
    for (int i = 0; i < m_num_threads; i++) {
         cpu_el = m_cpus + i;
         if (m_assigned_cpus == 0 || (*m_assigned_cpus)[i]) {
             count++;
             sprintf(buffer + strlen(buffer), "%d(%d) ", CPU_INDEX(cpu_el), cpu_el->count);
             if (count % 20 == 0) {
                 OFFLOAD_DEBUG_TRACE(3, "%s\n", buffer);
                 buffer[0] = 0;           
             } 
         }
    }
    if (count % 20 != 0) {
        OFFLOAD_DEBUG_TRACE(3, "%s\n", buffer);
    }
}

void Engine::init_process(void)
{
    COIENGINE engine;
    COIRESULT res;
    const char **environ;
    char buf[4096];  // For exe path name
    char* mic_device_main = 0;

    // create environment for the target process
    environ = (const char**) mic_env_vars.create_environ_for_card(m_index);
    if (environ != 0) {
        for (const char **p = environ; *p != 0; p++) {
            OFFLOAD_DEBUG_TRACE(3, "Env Var for card %d: %s\n", m_index, *p);
        }
    }

    // Create execution context in the specified device
    OFFLOAD_DEBUG_TRACE(2, "Getting device %d (engine %d) handle\n", m_index,
                        m_physical_index);
    res = COI::EngineGetHandle(COI_ISA_MIC, m_physical_index, &engine);
    check_result(res, c_get_engine_handle, m_index, res);

    // Get engine info on threads and cores.
    // The values of core number and thread number will be used later at stream
    // creation by call to _Offload_stream_create(device,number_of_cpus).

    COI_ENGINE_INFO engine_info;

    res = COI::EngineGetInfo(engine, sizeof(COI_ENGINE_INFO), &engine_info);
    check_result(res, c_get_engine_info, m_index, res);
    if (mic_library_path == 0 ) {
       if (engine_info.ISA == COI_DEVICE_KNC) {
          mic_library_path = knc_library_path;
       }
       else if (engine_info.ISA == COI_DEVICE_KNL) {
          mic_library_path = knl_library_path;
       }
       else {
          LIBOFFLOAD_ERROR(c_unknown_mic_device_type);
       }
    }

    // m_cpus is the list of all available threads.
    // At the begining all threads made available through OFFLOAD_DEVICES
    // or all threads existed at the engine if OFFLOAD_DEVICES isn't set.
    // m_cpu_head points to the head of the m_cpus list.
    // m_cpus is ordered by number of streams using the thread.
    // m_cpu_head points to the least used thread.
    // After creating and destroying a stream the m_cpus list must be fixed
    // to be ordered.

    m_cpus = (CpuEl*)malloc(engine_info.NumThreads * sizeof(CpuEl));
    if (m_cpus == NULL)
        LIBOFFLOAD_ERROR(c_malloc);
    memset(m_cpus, 0, engine_info.NumThreads * sizeof(CpuEl));
    CpuEl* prev_cpu = NULL;

    for (int i = 0; i < engine_info.NumThreads; i++) {
         if (m_assigned_cpus == 0 || (*m_assigned_cpus)[i]) {
             if (prev_cpu) {
                 prev_cpu->next = m_cpus + i;
             }
             else {
                 m_cpu_head = m_cpus + i;
             }
             m_cpus[i].prev = prev_cpu;
             m_cpus[i].count = 0;
             prev_cpu = m_cpus + i;
         }
    }

    // The following values will be used at pipeline creation for streams
    m_num_cores = engine_info.NumCores;
    m_num_threads = engine_info.NumThreads;

    print_stream_cpu_list("init_process");

    // Check if OFFLOAD_DMA_CHANNEL_COUNT is set to 2
    // Only the value 2 is supported in 16.0
    if (mic_dma_channel_count == 2) {
        if (COI::ProcessConfigureDMA) {
            // Set DMA channels using COI API
            COI::ProcessConfigureDMA(2, COI::DMA_MODE_READ_WRITE);
        }
        else {
            // Set environment variable COI_DMA_CHANNEL_COUNT
            // use putenv instead of setenv as Windows has no setenv.
            // Note: putenv requires its argument can't be freed or modified.
            // So no free after call to putenv or elsewhere.
            char * env_var = strdup("COI_DMA_CHANNEL_COUNT=2");
            if (env_var == NULL)
                LIBOFFLOAD_ERROR(c_malloc);
            putenv(env_var);
        }
    }

    // Target executable is not available then use compiler provided offload_main
    if (__target_exe == 0) {
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
       if (mic_device_main == 0) {
          LIBOFFLOAD_ERROR(c_report_no_target_exe, "offload_main");
          exit(1);
       }

       OFFLOAD_DEBUG_TRACE(2,
           "Loading target executable %s\n",mic_device_main);

       res = COI::ProcessCreateFromFile(
           engine,                 // in_Engine
           mic_device_main,        // in_pBinaryName
           0,                      // in_Argc
           0,                      // in_ppArgv
           environ == 0,           // in_DupEnv
           environ,                // in_ppAdditionalEnv
           mic_proxy_io,           // in_ProxyActive
           mic_proxy_fs_root,      // in_ProxyfsRoot
           mic_buffer_size,        // in_BufferSpace
           mic_library_path,       // in_LibrarySearchPath
           &m_process              // out_pProcess
       );
    }
    else {
    // Target executable should be available by the time when we
    // attempt to initialize the device

       //  Need the full path of the FAT exe for VTUNE
       {
#ifndef TARGET_WINNT
          ssize_t len = readlink("/proc/self/exe", buf,1000);
#else
          int len = GetModuleFileName(NULL, buf,1000);
#endif // TARGET_WINNT
          if  (len == -1) {
             LIBOFFLOAD_ERROR(c_report_no_host_exe);
             exit(1);
          }
          else if (len > 999) {
             LIBOFFLOAD_ERROR(c_report_path_buff_overflow);
             exit(1);
          }
          buf[len] = '\0';
       }

       OFFLOAD_DEBUG_TRACE(2,
           "Loading target executable \"%s\" from %p, size %lld, host file %s\n",
           __target_exe->name, __target_exe->data, __target_exe->size,
           buf);

       res = COI::ProcessCreateFromMemory(
           engine,                 // in_Engine
           __target_exe->name,     // in_pBinaryName
           __target_exe->data,     // in_pBinaryBuffer
           __target_exe->size,     // in_BinaryBufferLength,
           0,                      // in_Argc
           0,                      // in_ppArgv
           environ == 0,           // in_DupEnv
           environ,                // in_ppAdditionalEnv
           mic_proxy_io,           // in_ProxyActive
           mic_proxy_fs_root,      // in_ProxyfsRoot
           mic_buffer_size,        // in_BufferSpace
           mic_library_path,       // in_LibrarySearchPath
           buf,                    // in_FileOfOrigin
           -1,                     // in_FileOfOriginOffset use -1 to indicate to
                                   // COI that is is a FAT binary
           &m_process              // out_pProcess
       );
    }
    check_result(res, c_process_create, m_index, res);

    if ((mic_4k_buffer_size != 0) || (mic_2m_buffer_size !=0)) {
       // available only in MPSS 4.2 and greater
       if (COI::ProcessSetCacheSize != 0 ) { 
          int flags;
          //  Need compiler to use MPSS 3.2 or greater to get these
          // definition so currently hardcoding it
          //  COI_CACHE_ACTION_GROW_NOW && COI_CACHE_MODE_ONDEMAND_SYNC;
          flags = 0x00020002; 
          res = COI::ProcessSetCacheSize(
               m_process,             // in_Process
               mic_2m_buffer_size,    // in_HugePagePoolSize
               flags,                 // inHugeFlags
               mic_4k_buffer_size,    // in_SmallPagePoolSize
               flags,                 // inSmallFlags
               0,                     // in_NumDependencies
               0,                     // in_pDependencies
               0                      // out_PCompletion
          );
          OFFLOAD_DEBUG_TRACE(2,
              "Reserve target buffers 4K pages = %d  2M pages = %d\n",
                  mic_4k_buffer_size, mic_2m_buffer_size);
           check_result(res, c_process_set_cache_size, m_index, res);
       }
       else {
             OFFLOAD_DEBUG_TRACE(2,
                 "Reserve target buffers not supported in current MPSS\n");
       }
    }

    // get function handles
    res = COI::ProcessGetFunctionHandles(m_process, c_funcs_total,
                                         m_func_names, m_funcs);
    check_result(res, c_process_get_func_handles, m_index, res);

    // initialize device side
    pid_t pid = init_device();

    // For IDB
    if (__dbg_is_attached) {
        // TODO: we have in-memory executable now.
        // Check with IDB team what should we provide them now?
        if (__target_exe == 0) {
            strcpy(__dbg_target_exe_name, "offload_main");
        }
        else {
            if (strlen(__target_exe->name) < MAX_TARGET_NAME) {
                strcpy(__dbg_target_exe_name, __target_exe->name);
            }
        }
        __dbg_target_so_pid = pid;
        __dbg_target_id = m_physical_index;
       // The call to __dbg_target_so_loaded() is moved
       // to Engine:init so all the libraries are loaded before
       // informing debugger so debugger can access them.
       // __dbg_target_so_loaded();
    }
}

void Engine::fini_process(bool verbose)
{
    if (m_process != 0) {
        uint32_t sig;
        int8_t ret;

        // destroy target process
        OFFLOAD_DEBUG_TRACE(2, "Destroying process on the device %d\n",
                            m_index);

        COIRESULT res = COI::ProcessDestroy(m_process, -1, 0, &ret, &sig);
        m_process = 0;

        if (res == COI_SUCCESS) {
            OFFLOAD_DEBUG_TRACE(3, "Device process: signal %d, exit code %d\n",
                                sig, ret);
            if (verbose) {
                if (sig != 0) {
                    LIBOFFLOAD_ERROR(
                        c_mic_process_exit_sig, m_index, sig,
                        c_signal_names[sig >= c_signal_max ? 0 : sig]);
                }
                else {
                    LIBOFFLOAD_ERROR(c_mic_process_exit_ret, m_index, ret);
                }
            }

            // for idb
            if (__dbg_is_attached) {
                __dbg_target_so_unloaded();
            }
        }
        else {
            if (verbose) {
                LIBOFFLOAD_ERROR(c_mic_process_exit, m_index);
            }
        }
    }
}

void Engine::load_libraries()
{
    // load libraries collected so far
    for (TargetImageList::iterator it = m_images.begin();
         it != m_images.end(); it++) {
        OFFLOAD_DEBUG_TRACE(2,
            "Loading library \"%s\" from %p, size %llu, host file %s\n",
                                    it->name, it->data, it->size, it->origin);

        // load library to the device
        COILIBRARY lib;
        COIRESULT res;
        res = COI::ProcessLoadLibraryFromMemory(m_process,
                                                it->data,
                                                it->size,
                                                it->name,
                                                mic_library_path,
                                                it->origin,
                                                (it->origin) ? -1 : 0,
                                                COI_LOADLIBRARY_V1_FLAGS,
                                                &lib);
        m_dyn_libs.push_front(DynLib(it->name, it->data, lib));

        if (res != COI_SUCCESS && res != COI_ALREADY_EXISTS) {
            check_result(res, c_load_library, it->origin, m_index, res);
        }
    }
    m_images.clear();
}

void Engine::unload_library(const void *data, const char *name)
{
    if (m_process == 0) {
       return;
    }
    for (DynLibList::iterator it = m_dyn_libs.begin();
         it != m_dyn_libs.end(); it++) {
         if (it->data == data) {
            COIRESULT res;
            OFFLOAD_DEBUG_TRACE(2,
               "Unloading library \"%s\"\n",name);
            res = COI::ProcessUnloadLibrary(m_process,it->lib);
            m_dyn_libs.erase(it);
            if (res != COI_SUCCESS) {
                check_result(res, c_unload_library, m_index, res);
            }
            return;
         }
    }
}

static bool target_entry_cmp(
    const VarList::BufEntry &l,
    const VarList::BufEntry &r
)
{
    const char *l_name = reinterpret_cast<const char*>(l.name);
    const char *r_name = reinterpret_cast<const char*>(r.name);
    return strcmp(l_name, r_name) < 0;
}

static bool host_entry_cmp(
    const VarTable::Entry *l,
    const VarTable::Entry *r
)
{
    return strcmp(l->name, r->name) < 0;
}

void Engine::init_ptr_data(void)
{
    COIRESULT res;
    COIEVENT event;

    // Prepare table of host entries
    std::vector<const VarTable::Entry*> host_table(
                                         Iterator(__offload_vars.get_head()),
                                         Iterator());

    // no need to do anything further is host table is empty
    if (host_table.size() <= 0) {
        return;
    }

    // Get var table entries from the target.
    // First we need to get size for the buffer to copy data
    struct {
        int64_t nelems;
        int64_t length;
    } params;

    res = COI::PipelineRunFunction(get_pipeline(),
                                   m_funcs[c_func_var_table_size],
                                   0, 0, 0,
                                   0, 0,
                                   0, 0,
                                   &params, sizeof(params),
                                   &event);
    check_result(res, c_pipeline_run_func, m_index, res);

    res = COI::EventWait(1, &event, -1, 1, 0, 0);
    check_result(res, c_event_wait, res);

    if (params.length == 0) {
        return;
    }

    // create buffer for target entries and copy data to host
    COIBUFFER buffer;
    res = COI::BufferCreate(params.length, COI_BUFFER_NORMAL, 0, 0, 1,
                            &m_process, &buffer);
    check_result(res, c_buf_create, m_index, res);

    COI_ACCESS_FLAGS flags = COI_SINK_WRITE;
    res = COI::PipelineRunFunction(get_pipeline(),
                                   m_funcs[c_func_var_table_copy],
                                   1, &buffer, &flags,
                                   0, 0,
                                   &params.nelems, sizeof(params.nelems),
                                   0, 0,
                                   &event);
    check_result(res, c_pipeline_run_func, m_index, res);

    res = COI::EventWait(1, &event, -1, 1, 0, 0);
    check_result(res, c_event_wait, res);

    // patch names in target data
    VarList::BufEntry *target_table;
    COIMAPINSTANCE map_inst;
    res = COI::BufferMap(buffer, 0, params.length, COI_MAP_READ_ONLY, 0, 0,
                         0, &map_inst,
                         reinterpret_cast<void**>(&target_table));
    check_result(res, c_buf_map, res);

    VarList::table_patch_names(target_table, params.nelems);

    // and sort entries
    std::sort(target_table, target_table + params.nelems, target_entry_cmp);
    std::sort(host_table.begin(), host_table.end(), host_entry_cmp);

    // merge host and target entries and enter matching vars map
    std::vector<const VarTable::Entry*>::const_iterator hi =
        host_table.begin();
    std::vector<const VarTable::Entry*>::const_iterator he =
        host_table.end();
    const VarList::BufEntry *ti = target_table;
    const VarList::BufEntry *te = target_table + params.nelems;

    while (hi != he && ti != te) {
        int res = strcmp((*hi)->name, reinterpret_cast<const char*>(ti->name));
        if (res == 0) {
            bool is_new;
            // add matching entry to var map
            PtrData *ptr = insert_ptr_data((*hi)->addr, (*hi)->size, is_new);

            // store address for new entries
            if (is_new) {
                ptr->mic_addr = ti->addr;
                ptr->is_static = true;
                ptr->var_alloc_type = (*hi)->var_alloc_type;
            }
            ptr->alloc_ptr_data_lock.unlock();
            hi++;
            ti++;
        }
        else if (res < 0) {
            hi++;
        }
        else {
            ti++;
        }
    }

    // cleanup
    res = COI::BufferUnmap(map_inst, 0, 0, 0);
    check_result(res, c_buf_unmap, res);

    res = COI::BufferDestroy(buffer);
    check_result(res, c_buf_destroy, res);
}

COIRESULT Engine::compute(
    _Offload_stream stream,
    const std::list<COIBUFFER> &buffers,
    const void*         data,
    uint16_t            data_size,
    void*               ret,
    uint16_t            ret_size,
    uint32_t            num_deps,
    const COIEVENT*     deps,
    COIEVENT*           event
) /* const */
{
    COIBUFFER *bufs;
    COI_ACCESS_FLAGS *flags;
    COIRESULT res;

    // convert buffers list to array
    int num_bufs = buffers.size();
    if (num_bufs > 0) {
        bufs = (COIBUFFER*) alloca(num_bufs * sizeof(COIBUFFER));
        flags = (COI_ACCESS_FLAGS*) alloca(num_bufs *
                                           sizeof(COI_ACCESS_FLAGS));

        int i = 0;
        for (std::list<COIBUFFER>::const_iterator it = buffers.begin();
             it != buffers.end(); it++) {
            bufs[i] = *it;

            // TODO: this should be fixed
            flags[i++] = COI_SINK_WRITE;
        }
    }
    else {
        bufs = 0;
        flags = 0;
    }
    COIPIPELINE pipeline = (stream == no_stream) ?
                           get_pipeline() :
                           get_pipeline(stream);
    // start computation
    res = COI::PipelineRunFunction(pipeline,
                                   m_funcs[c_func_compute],
                                   num_bufs, bufs, flags,
                                   num_deps, deps,
                                   data, data_size,
                                   ret, ret_size,
                                   event);
    return res;
}

pid_t Engine::init_device(void)
{
    struct init_data {
        int  device_index;
        int  devices_total;
        int  console_level;
        int  offload_report_level;
    } data;
    COIRESULT res;
    COIEVENT event;
    pid_t pid;

    OFFLOAD_DEBUG_TRACE_1(2, 0, c_offload_init,
                          "Initializing device with logical index %d "
                          "and physical index %d\n",
                           m_index, m_physical_index);

    // setup misc data
    data.device_index = m_index;
    data.devices_total = mic_engines_total;
    data.console_level = console_enabled;
    data.offload_report_level = offload_report_level;

    res = COI::PipelineRunFunction(get_pipeline(),
                                   m_funcs[c_func_init],
                                   0, 0, 0, 0, 0,
                                   &data, sizeof(data),
                                   &pid, sizeof(pid),
                                   &event);
    check_result(res, c_pipeline_run_func, m_index, res);

    res = COI::EventWait(1, &event, -1, 1, 0, 0);
    check_result(res, c_event_wait, res);

    OFFLOAD_DEBUG_TRACE(2, "Device process pid is %d\n", pid);

    return pid;
}

// data associated with each thread
struct Thread {
    Thread(long* addr_coipipe_counter) {
        m_addr_coipipe_counter = addr_coipipe_counter;
        memset(m_pipelines, 0, sizeof(m_pipelines));
    }

    ~Thread() {
#ifndef TARGET_WINNT
        __sync_sub_and_fetch(m_addr_coipipe_counter, 1);
#else // TARGET_WINNT
        _InterlockedDecrement(m_addr_coipipe_counter);
#endif // TARGET_WINNT
        for (int i = 0; i < mic_engines_total; i++) {
            if (m_pipelines[i] != 0) {
                COI::PipelineDestroy(m_pipelines[i]);
            }
        }
    }

    COIPIPELINE get_pipeline(int index) const {
        return m_pipelines[index];
    }

    void set_pipeline(int index, COIPIPELINE pipeline) {
        m_pipelines[index] = pipeline;
    }

    AutoSet& get_auto_vars() {
        return m_auto_vars;
    }

private:
    long*       m_addr_coipipe_counter;
    AutoSet     m_auto_vars;
    COIPIPELINE m_pipelines[MIC_ENGINES_MAX];
};

COIPIPELINE Engine::get_pipeline(void)
{
    Thread* thread = (Thread*) thread_getspecific(mic_thread_key);
    if (thread == 0) {
        thread = new Thread(&m_proc_number);
        thread_setspecific(mic_thread_key, thread);
    }

    COIPIPELINE pipeline = thread->get_pipeline(m_index);
    if (pipeline == 0) {
        COIRESULT res;
        int proc_num;

#ifndef TARGET_WINNT
        proc_num = __sync_fetch_and_add(&m_proc_number, 1);
#else // TARGET_WINNT
        proc_num = _InterlockedIncrement(&m_proc_number);
#endif // TARGET_WINNT

        if (proc_num > COI_PIPELINE_MAX_PIPELINES) {
            LIBOFFLOAD_ERROR(c_coipipe_max_number, COI_PIPELINE_MAX_PIPELINES);
            LIBOFFLOAD_ABORT;
        }

        // Create pipeline for this thread
        if (m_assigned_cpus == 0) {
            // If m_assigned_cpus is NULL, it implies all threads
            // Create the pipeline with no CPU mask
            res = COI::PipelineCreate(m_process, 0, mic_stack_size, &pipeline);
        } else {
            // Create COI CPU mask
            COI_CPU_MASK  in_Mask;
            res = COI::PipelineClearCPUMask(in_Mask);
            check_result(res, c_clear_cpu_mask, m_index, res);

            int threads_per_core = m_num_threads / m_num_cores;

            // Available threads are defined by examining of m_assigned_cpus bitset.
            // We skip thread 0.
            for (int i = 1; i < m_num_threads; i++) {
                // For available thread i m_assigned_cpus[i] is equal to 1
                if ((*m_assigned_cpus)[i]) {
                    COI_CPU_MASK_SET(i, in_Mask);
                }
            }
            OFFLOAD_DEBUG_TRACE(2, "COIPipelineCreate Mask for this CPU thread\n"
                               "%016lx %016lx %016lx %016lx\n%016lx %016lx %016lx %016lx\n"
                               "%016lx %016lx %016lx %016lx\n%016lx %016lx %016lx %016lx\n",
                               in_Mask[0], in_Mask[1], in_Mask[2], in_Mask[3],
                               in_Mask[4], in_Mask[5], in_Mask[6], in_Mask[7],
                               in_Mask[8], in_Mask[9], in_Mask[10], in_Mask[11],
                               in_Mask[12], in_Mask[13], in_Mask[14], in_Mask[15]);

            // Create the pipeline with allowable CPUs
            res = COI::PipelineCreate(m_process, in_Mask, mic_stack_size, &pipeline);
        }
        check_result(res, c_pipeline_create, m_index, res);
        thread->set_pipeline(m_index, pipeline);
    }
    return pipeline;
}

Stream* Stream::find_stream(uint64_t handle, bool remove)
{
    Stream *stream = 0;

    m_stream_lock.lock();
    {
        StreamMap::iterator it = all_streams.find(handle);
        if (it != all_streams.end()) {
            stream = it->second;
            if (remove) {
                all_streams.erase(it);
            }
        }
    }
    m_stream_lock.unlock();
    return stream;
}

void Engine::move_cpu_el_after(CpuEl* cpu_what, CpuEl* cpu_after)
{
    if (cpu_what == cpu_after) {
        return;
    }
    CpuEl* cpu_prev = cpu_what->prev;

    // remove cpu_what
    if (!cpu_prev) {
        m_cpu_head = cpu_what->next;
    }
    else {
        cpu_prev->next = cpu_what->next;
    }
    if (cpu_what->next) {
        cpu_what->next->prev = cpu_prev;
    }

    // insert cpu_what after cpu_after
    cpu_what->prev = cpu_after;
    cpu_what->next = cpu_after->next;
    if (cpu_after->next) {
        cpu_after->next->prev = cpu_what;
    }
    cpu_after->next = cpu_what;
}

COIPIPELINE Engine::get_pipeline(_Offload_stream handle)
{
    Stream * stream = Stream::find_stream(handle, false);

    if (!stream) {
        LIBOFFLOAD_ERROR(c_offload_no_stream, m_index);
        LIBOFFLOAD_ABORT;
    }

    COIPIPELINE pipeline = stream->get_pipeline();

    if (pipeline == 0) {
        COIRESULT     res;
        int           proc_num;
        COI_CPU_MASK  in_Mask ;

#ifndef TARGET_WINNT
        proc_num = __sync_fetch_and_add(&m_proc_number, 1);
#else // TARGET_WINNT
        proc_num = _InterlockedIncrement(&m_proc_number);
#endif // TARGET_WINNT

        if (proc_num > COI_PIPELINE_MAX_PIPELINES) {
            LIBOFFLOAD_ERROR(c_coipipe_max_number, COI_PIPELINE_MAX_PIPELINES);
            LIBOFFLOAD_ABORT;
        }

        m_stream_lock.lock();

        // start process if not done yet
        if (m_process == 0) {
            init_process();
        }

        // create CPUmask
        res = COI::PipelineClearCPUMask(in_Mask);
        check_result(res, c_clear_cpu_mask, m_index, res);

        int stream_cpu_num = stream->get_cpu_number();

        stream->m_stream_cpus.reset();

        int threads_per_core = m_num_threads / m_num_cores;


        // Available threads is taken from m_cpus list.
        // m_cpu_head points to the head of m_cpus.
        // the elements of m_cpus is ordered by the number of usage in streams.

        CpuEl *cpu_el = m_cpu_head;
        CpuEl *cpu_used_el, *cpu_used_prev, *cpu_prev;

        for (int i = 0; i < stream_cpu_num; i++) {
            COI_CPU_MASK_SET(CPU_INDEX(cpu_el), in_Mask);
            stream->m_stream_cpus.set(CPU_INDEX(cpu_el));
            //If the number of availabale threads is less than stream_cpu_num,
            // the stream_cpu_num is restricted to this number.
            if (!cpu_el->next) {
                break;
            }
            if (i + 1 < stream_cpu_num) {
                cpu_el = cpu_el->next;
            }
        }

        // assertion : cpu_el points to the last used thread
        cpu_used_el = cpu_el;
        while (cpu_used_el) {
            cpu_used_el->count++;
            cpu_el = cpu_prev = cpu_used_el;
            cpu_used_prev = cpu_used_el->prev;
            if (!cpu_el->next) {
                cpu_used_el = cpu_used_prev;
                continue;
            }
            
            while (cpu_el) {
                if (cpu_used_el->count < cpu_el->count) {
                    break;
                }
                // Equal used threads are ordered by thread number to
                // assign to a stream as contiguous threads as possible.
                else if (cpu_used_el->count == cpu_el->count &&
                         CPU_INDEX(cpu_used_el) <  CPU_INDEX(cpu_el)) {
                     break;               
                }
                cpu_prev = cpu_el;
                cpu_el = cpu_el->next;
            }
            if (cpu_used_el != cpu_prev) {
                move_cpu_el_after(cpu_used_el, cpu_prev);
            }
            cpu_used_el = cpu_used_prev;
        }
        print_stream_cpu_list("get_pipeline");

        // create pipeline for this thread
        OFFLOAD_DEBUG_TRACE(2, "COIPipelineCreate Mask for this Stream\n"
                               "%016lx %016lx %016lx %016lx\n%016lx %016lx %016lx %016lx\n"
                               "%016lx %016lx %016lx %016lx\n%016lx %016lx %016lx %016lx\n",
                               in_Mask[0], in_Mask[1], in_Mask[2], in_Mask[3],
                               in_Mask[4], in_Mask[5], in_Mask[6], in_Mask[7],
                               in_Mask[8], in_Mask[9], in_Mask[10], in_Mask[11],
                               in_Mask[12], in_Mask[13], in_Mask[14], in_Mask[15]);
        res = COI::PipelineCreate(m_process, in_Mask,
                                  mic_stack_size, &pipeline);
        check_result(res, c_pipeline_create, m_index, res);

        // Set stream's affinities
        {
            struct affinity_spec affinity_spec;
            char* affinity_type;
            int i;

            // "compact" by default
            affinity_spec.affinity_type = affinity_compact;

            // Check if user has specified type of affinity
            if ((affinity_type = getenv("OFFLOAD_STREAM_AFFINITY")) !=
                                        NULL)
            {
                char affinity_str[16];
                int affinity_str_len;

                OFFLOAD_DEBUG_TRACE(2,
                    "User has specified OFFLOAD_STREAM_AFFINITY=%s\n",
                    affinity_type);

                // Set type of affinity requested
                affinity_str_len = strlen(affinity_type);
                for (i=0; i<affinity_str_len && i<15; i++)
                {
                    affinity_str[i] = tolower(affinity_type[i]);
                }
                affinity_str[i] = '\0';
                if (strcmp(affinity_str, "compact") == 0) {
                    affinity_spec.affinity_type = affinity_compact;
                    OFFLOAD_DEBUG_TRACE(2, "Setting affinity=compact\n");
                } else if (strcmp(affinity_str, "scatter") == 0) {
                    affinity_spec.affinity_type = affinity_scatter;
                    OFFLOAD_DEBUG_TRACE(2, "Setting affinity=scatter\n");
                } else {
                    LIBOFFLOAD_ERROR(c_incorrect_affinity, affinity_str);
                    affinity_spec.affinity_type = affinity_compact;
                    OFFLOAD_DEBUG_TRACE(2, "Setting affinity=compact\n");
                }
            }
            // Make flat copy of sink mask because COI's mask is opaque
            for (i=0; i<16; i++) {
                affinity_spec.sink_mask[i] = in_Mask[i];
            }
            // Set number of cores and threads
            affinity_spec.num_cores = m_num_cores;
            affinity_spec.num_threads = m_num_threads;

            COIEVENT event;
            res = COI::PipelineRunFunction(pipeline,
                                   m_funcs[c_func_set_stream_affinity],
                                   0, 0, 0,
                                   0, 0,
                                   &affinity_spec, sizeof(affinity_spec),
                                   0, 0,
                                   &event);
            check_result(res, c_pipeline_run_func, m_index, res);
    
            res = COI::EventWait(1, &event, -1, 1, 0, 0);
            check_result(res, c_event_wait, res);
        }

        m_stream_lock.unlock();
        stream->set_pipeline(pipeline);
    }
    return pipeline;
}

void Engine::stream_destroy(_Offload_stream handle)
{
    // get stream
    Stream * stream =  Stream::find_stream(handle, true);

    if (stream) {
        // return cpus for future use
        for (int i = 0; i < m_num_threads; i++) {
            if (stream->m_stream_cpus.test(i)) {
                CpuEl *cpu_el = m_cpus + i;
                CpuEl *cpu_first_el = cpu_el;
                // decrease count of thread "i" and move its CpuEl to the
                // proper place into the ordered list
                cpu_el->count--;
                while (cpu_el->prev) {
                    if (cpu_first_el->count > cpu_el->prev->count) {
                        break;
                    }
                    else if (cpu_first_el->count == cpu_el->prev->count &&
                             CPU_INDEX(cpu_first_el) > CPU_INDEX(cpu_el->prev)) {
                        break;
                    }
                    cpu_el = cpu_el->prev;
                }
                cpu_el = cpu_el->prev;
                // If cpu_el for thread "i" must be moved in the list
                if (cpu_first_el != cpu_el) {
                    // Thread "i" is used the least times. It must be set as
                    // the m_cpu_head.
                    if (!cpu_el) {
                        if (!cpu_first_el->prev) {
                            continue;
                        }
                        // remove cpu_el.
                        cpu_first_el->prev->next = cpu_first_el->next;
                        if (cpu_first_el->next) {
                            cpu_first_el->next->prev = cpu_first_el->prev;
                        }
                        // make cpu_first_el as new m_cpu_head
                        cpu_first_el->prev = NULL;
                        cpu_first_el->next = m_cpu_head;
                        m_cpu_head->prev = cpu_first_el;
                        m_cpu_head = cpu_first_el;
                    }
                    else {
                        move_cpu_el_after(cpu_first_el, cpu_el);
                    }
                }
            }
        }
        print_stream_cpu_list("stream_destroy");        
        delete stream;
    }
    else {
        LIBOFFLOAD_ERROR(c_offload_no_stream, m_index);
        LIBOFFLOAD_ABORT;
    }
}

uint64_t Engine::get_thread_id(void)
{
    Thread* thread = (Thread*) thread_getspecific(mic_thread_key);
    if (thread == 0) {
        thread = new Thread(&m_proc_number);
        thread_setspecific(mic_thread_key, thread);
    }

    return reinterpret_cast<uint64_t>(thread);
}

AutoSet& Engine::get_auto_vars(void)
{
    Thread* thread = (Thread*) thread_getspecific(mic_thread_key);
    if (thread == 0) {
        thread = new Thread(&m_proc_number);
        thread_setspecific(mic_thread_key, thread);
    }

    return thread->get_auto_vars();
}

void Engine::destroy_thread_data(void *data)
{
    delete static_cast<Thread*>(data);
}
