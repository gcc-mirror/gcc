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


#if defined(LINUX) || defined(FREEBSD)
#include <mm_malloc.h>
#endif

#include "offload_myo_host.h"
#include <errno.h>
#include <malloc.h>
#include "offload_host.h"
//#include "offload_util.h"

#define MYO_VERSION1    "MYO_1.0"

extern "C" void __cilkrts_cilk_for_32(void*, void*, uint32_t, int32_t);
extern "C" void __cilkrts_cilk_for_64(void*, void*, uint64_t, int32_t);

#ifndef TARGET_WINNT
#pragma weak __cilkrts_cilk_for_32
#pragma weak __cilkrts_cilk_for_64
#endif // TARGET_WINNT

static void __offload_myoProcessDeferredTables();

class MyoWrapper {
public:
    MyoWrapper() : m_lib_handle(0), m_is_available(false)
    {}

    bool is_available() const {
        return m_is_available;
    }

    bool LoadLibrary(void);

    // unloads the library
    void UnloadLibrary(void) {
//        if (m_lib_handle != 0) {
//            DL_close(m_lib_handle);
//            m_lib_handle = 0;
//        }
    }

    // Wrappers for MYO client functions
    void LibInit(void *arg, void *func) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myoinit,
                                 "%s(%p, %p)\n", __func__, arg, func);
        CheckResult(__func__, m_lib_init(arg, func));
    }

    void LibFini(void) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myofini, "%s()\n", __func__);
        m_lib_fini();
    }

    void* SharedMalloc(size_t size) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myosharedmalloc,
                                 "%s(%lld)\n", __func__, size);
        return m_shared_malloc(size);
    }

    void SharedFree(void *ptr) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myosharedfree,
                                 "%s(%p)\n", __func__, ptr);
        m_shared_free(ptr);
    }

    void* SharedAlignedMalloc(size_t size, size_t align) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myosharedalignedmalloc,
                                 "%s(%lld, %lld)\n", __func__, size, align);
        return m_shared_aligned_malloc(size, align);
    }

    void SharedAlignedFree(void *ptr) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myosharedalignedfree,
                              "%s(%p)\n", __func__, ptr);
        m_shared_aligned_free(ptr);
    }

    void Acquire(void) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myoacquire,
                              "%s()\n", __func__);
        CheckResult(__func__, m_acquire());
    }

    void Release(void) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myorelease,
                            "%s()\n", __func__);
        CheckResult(__func__, m_release());
    }

    void HostVarTablePropagate(void *table, int num_entries) const {
        OFFLOAD_DEBUG_TRACE(4, "%s(%p, %d)\n", __func__, table, num_entries);
        CheckResult(__func__, m_host_var_table_propagate(table, num_entries));
    }

    void HostFptrTableRegister(void *table, int num_entries,
                               int ordered) const {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myoregister,
                            "%s(%p, %d, %d)\n", __func__, table,
                            num_entries, ordered);
        CheckResult(__func__,
                    m_host_fptr_table_register(table, num_entries, ordered));
    }

    void RemoteThunkCall(void *thunk, void *args, int device) {
        OFFLOAD_DEBUG_TRACE(4, "%s(%p, %p, %d)\n", __func__, thunk, args,
                            device);
        CheckResult(__func__, m_remote_thunk_call(thunk, args, device));
    }

    MyoiRFuncCallHandle RemoteCall(const char *func, void *args, int device) const {
        OFFLOAD_DEBUG_TRACE(4, "%s(%s, %p, %d)\n", __func__, func, args,
                            device);
        return m_remote_call(func, args, device);
    }

    void GetResult(MyoiRFuncCallHandle handle) const {
        OFFLOAD_DEBUG_TRACE(4, "%s(%p)\n", __func__, handle);
        CheckResult(__func__, m_get_result(handle));
    }

    bool PostInitFuncSupported() const {
        OFFLOAD_DEBUG_TRACE(4, "%s()\n", __func__);
        if (m_feature_available) {
            return m_feature_available(MYO_FEATURE_POST_LIB_INIT) ==
                       MYO_SUCCESS;
        } else {
            return false;
        }
    }

    void CreateVtableArena();

    MyoArena GetVtableArena()const {
        return m_vtable_arena;
    }

    void ArenaCreate(
        MyoOwnershipType ownership,
        int consistency,
        MyoArena* arena
    ) const
    {
        OFFLOAD_DEBUG_TRACE(4, "%s(%d, %d, %p)\n",
            __func__, ownership, consistency, arena);
        CheckResult(__func__, m_arena_create(ownership, consistency, arena));
    }

    void* SharedAlignedArenaMalloc(
        MyoArena arena,
        size_t size,
        size_t align
    ) const
    {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myosharedalignedarenamalloc,
                                 "%s(%u, %lld, %lld)\n",
                                 __func__, arena, size, align);
        return m_arena_aligned_malloc(arena, size, align);
    }

    void* SharedAlignedArenaFree(
        MyoArena arena,
        void* ptr
    ) const
    {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myosharedalignedarenafree,
                                 "%s(%u, %p)\n", __func__, arena, ptr);
        return m_arena_aligned_free(arena, ptr);
    }

    void ArenaAcquire(
        MyoArena arena
    ) const
    {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myoarenaacquire,
                              "%s()\n", __func__);
        CheckResult(__func__, m_arena_acquire(arena));
    }

    void ArenaRelease(
        MyoArena arena
    ) const
    {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_myoarenarelease,
                            "%s()\n", __func__);
        CheckResult(__func__, m_arena_release(arena));
    }

private:
    void CheckResult(const char *func, MyoError error) const {
        if (error != MYO_SUCCESS) {
             LIBOFFLOAD_ERROR(c_myowrapper_checkresult, func, error);
            exit(1);
        }
    }

private:
    void*    m_lib_handle;
    bool     m_is_available;
    int      m_post_init_func;
    MyoArena m_vtable_arena;

    // pointers to functions from myo library
    MyoError (*m_lib_init)(void*, void*);
    void     (*m_lib_fini)(void);
    void*    (*m_shared_malloc)(size_t);
    void     (*m_shared_free)(void*);
    void*    (*m_shared_aligned_malloc)(size_t, size_t);
    void     (*m_shared_aligned_free)(void*);
    MyoError (*m_acquire)(void);
    MyoError (*m_release)(void);
    MyoError (*m_host_var_table_propagate)(void*, int);
    MyoError (*m_host_fptr_table_register)(void*, int, int);
    MyoError (*m_remote_thunk_call)(void*, void*, int);
    MyoiRFuncCallHandle (*m_remote_call)(const char*, void*, int);
    MyoError (*m_get_result)(MyoiRFuncCallHandle);
    MyoError (*m_arena_create)(MyoOwnershipType, int, MyoArena*);
    void*    (*m_arena_aligned_malloc)(MyoArena, size_t, size_t);
    void*    (*m_arena_aligned_free)(MyoArena, void*);
    MyoError (*m_arena_acquire)(MyoArena);
    MyoError (*m_arena_release)(MyoArena);
    // Placeholder until MYO headers support enum type for feature
    MyoError (*m_feature_available)(int feature);
};

DLL_LOCAL bool MyoWrapper::LoadLibrary(void)
{
#ifndef TARGET_WINNT
    const char *lib_name = "libmyo-client.so";
#else // TARGET_WINNT
    const char *lib_name = "myo-client.dll";
#endif // TARGET_WINNT

    OFFLOAD_DEBUG_TRACE(2, "Loading MYO library %s ...\n", lib_name);

    m_lib_handle = DL_open(lib_name);
    if (m_lib_handle == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to load the library. errno = %d\n",
                            errno);
        return false;
    }

    m_lib_init = (MyoError (*)(void*, void*))
        DL_sym(m_lib_handle, "myoiLibInit", MYO_VERSION1);
    if (m_lib_init == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoiLibInit");
        UnloadLibrary();
        return false;
    }

    m_lib_fini = (void (*)(void))
        DL_sym(m_lib_handle, "myoiLibFini", MYO_VERSION1);
    if (m_lib_fini == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoiLibFini");
        UnloadLibrary();
        return false;
    }

    m_shared_malloc = (void* (*)(size_t))
        DL_sym(m_lib_handle, "myoSharedMalloc", MYO_VERSION1);
    if (m_shared_malloc == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoSharedMalloc");
        UnloadLibrary();
        return false;
    }

    m_shared_free = (void (*)(void*))
        DL_sym(m_lib_handle, "myoSharedFree", MYO_VERSION1);
    if (m_shared_free == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoSharedFree");
        UnloadLibrary();
        return false;
    }

    m_shared_aligned_malloc = (void* (*)(size_t, size_t))
        DL_sym(m_lib_handle, "myoSharedAlignedMalloc", MYO_VERSION1);
    if (m_shared_aligned_malloc == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoSharedAlignedMalloc");
        UnloadLibrary();
        return false;
    }

    m_shared_aligned_free = (void (*)(void*))
        DL_sym(m_lib_handle, "myoSharedAlignedFree", MYO_VERSION1);
    if (m_shared_aligned_free == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoSharedAlignedFree");
        UnloadLibrary();
        return false;
    }

    m_acquire = (MyoError (*)(void))
        DL_sym(m_lib_handle, "myoAcquire", MYO_VERSION1);
    if (m_acquire == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoAcquire");
        UnloadLibrary();
        return false;
    }

    m_release = (MyoError (*)(void))
        DL_sym(m_lib_handle, "myoRelease", MYO_VERSION1);
    if (m_release == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoRelease");
        UnloadLibrary();
        return false;
    }

    m_host_var_table_propagate = (MyoError (*)(void*, int))
        DL_sym(m_lib_handle, "myoiHostVarTablePropagate", MYO_VERSION1);
    if (m_host_var_table_propagate == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoiHostVarTablePropagate");
        UnloadLibrary();
        return false;
    }

    m_host_fptr_table_register = (MyoError (*)(void*, int, int))
        DL_sym(m_lib_handle, "myoiHostFptrTableRegister", MYO_VERSION1);
    if (m_host_fptr_table_register == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoiHostFptrTableRegister");
        UnloadLibrary();
        return false;
    }

    m_remote_thunk_call = (MyoError (*)(void*, void*, int))
        DL_sym(m_lib_handle, "myoiRemoteThunkCall", MYO_VERSION1);
    if (m_remote_thunk_call == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoiRemoteThunkCall");
        UnloadLibrary();
        return false;
    }

    m_remote_call = (MyoiRFuncCallHandle (*)(const char*, void*, int))
        DL_sym(m_lib_handle, "myoiRemoteCall", MYO_VERSION1);
    if (m_remote_call == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoiRemoteCall");
        UnloadLibrary();
        return false;
    }

    m_get_result = (MyoError (*)(MyoiRFuncCallHandle))
        DL_sym(m_lib_handle, "myoiGetResult", MYO_VERSION1);
    if (m_get_result == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoiGetResult");
        UnloadLibrary();
        return false;
    }

    m_arena_create = (MyoError (*)(MyoOwnershipType, int, MyoArena*))
        DL_sym(m_lib_handle, "myoArenaCreate", MYO_VERSION1);
    if (m_arena_create == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoArenaCreate");
        UnloadLibrary();
        return false;
    }

    m_arena_aligned_malloc = (void* (*)(MyoArena, size_t, size_t))
        DL_sym(m_lib_handle, "myoArenaAlignedMalloc", MYO_VERSION1);
    if (m_arena_aligned_malloc == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoArenaAlignedMalloc");
        UnloadLibrary();
        return false;
    }

    m_arena_aligned_free = (void* (*)(MyoArena, void*))
        DL_sym(m_lib_handle, "myoArenaAlignedFree", MYO_VERSION1);
    if (m_arena_aligned_free == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoArenaAlignedFree");
        UnloadLibrary();
        return false;
    }

    m_arena_acquire = (MyoError (*)(MyoArena))
        DL_sym(m_lib_handle, "myoArenaAcquire", MYO_VERSION1);
    if (m_acquire == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoArenaAcquire");
        UnloadLibrary();
        return false;
    }

    m_arena_release = (MyoError (*)(MyoArena))
        DL_sym(m_lib_handle, "myoArenaRelease", MYO_VERSION1);
    if (m_release == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoArenaRelease");
        UnloadLibrary();
        return false;
    }

    // Check for "feature-available" API added in MPSS 3.3.
    // Not finding it is not an error.
    m_feature_available = (MyoError (*)(int))
        DL_sym(m_lib_handle, "myoiSupportsFeature", MYO_VERSION1);
    if (m_feature_available == 0) {
        OFFLOAD_DEBUG_TRACE(2, "Failed to find %s in MYO library\n",
                            "myoiSupportsFeature");
    }    

    OFFLOAD_DEBUG_TRACE(2, "The library was successfully loaded\n");

    // Create arena if supported
    CreateVtableArena();
    OFFLOAD_DEBUG_TRACE(3, "Vtable arena created\n");

    m_is_available = true;

    return true;
}

static bool myo_is_available;
static MyoWrapper myo_wrapper;

void MyoWrapper::CreateVtableArena()
{
    MyoArena* vtable_arena;

    // Check if this MYO supports arenas for vtables
    if (myo_wrapper.PostInitFuncSupported()) {
        // Create arena for vtables
	    vtable_arena = (MyoArena *)myo_wrapper.SharedMalloc(sizeof(MyoArena));
        myo_wrapper.ArenaCreate(
            MYO_ARENA_OURS, MYO_NO_CONSISTENCY, vtable_arena);
        m_vtable_arena = *vtable_arena;
        OFFLOAD_DEBUG_TRACE(4, "created arena = %d\n", m_vtable_arena);
    } else {
        m_vtable_arena = 0;
    }
}

struct MyoTable
{
    MyoTable(SharedTableEntry *tab, int len) : var_tab(tab), var_tab_len(len)
    {}

    SharedTableEntry*   var_tab;
    int                 var_tab_len;
};

typedef std::list<MyoTable> MyoTableList;
static MyoTableList __myo_table_list;
static mutex_t      __myo_table_lock;
static bool         __myo_tables = false;

static void __offload_myo_shared_vtable_process(SharedTableEntry *entry);
static void __offload_myo_shared_table_process(SharedTableEntry *entry);
static void __offload_myo_shared_init_table_process(InitTableEntry* entry);
static void __offload_myo_fptr_table_process(FptrTableEntry *entry);
static void __offload_propagate_shared_vars();

static void __offload_myoLoadLibrary_once(void)
{
    if (__offload_init_library()) {
        myo_wrapper.LoadLibrary();
    }
}

static bool __offload_myoLoadLibrary(void)
{
    OFFLOAD_DEBUG_TRACE(4, "__offload_myoLoadLibrary\n");
    static OffloadOnceControl ctrl = OFFLOAD_ONCE_CONTROL_INIT;
    __offload_run_once(&ctrl, __offload_myoLoadLibrary_once);

    return myo_wrapper.is_available();
}

static void __offload_myoInit_once(void)
{
    if (!__offload_myoLoadLibrary()) {
        return;
    }

    // initialize all devices
    for (int i = 0; i < mic_engines_total; i++) {
        mic_engines[i].init();
    }

    // load and initialize MYO library
    OFFLOAD_DEBUG_TRACE(2, "Initializing MYO library ...\n");

    COIEVENT events[MIC_ENGINES_MAX];

    // One entry per device + 
    // A pair of entries for the Host postInit func +
    // A pair of entries for the MIC postInit func +
    // end marker
    MyoiUserParams params[MIC_ENGINES_MAX+5];

    // Load target library to all devices and
    // create libinit parameters for all devices
    for (int i = 0; i < mic_engines_total; i++) {
        mic_engines[i].init_myo(&events[i]);

        params[i].type = MYOI_USERPARAMS_DEVID;
        params[i].nodeid = mic_engines[i].get_physical_index() + 1;
        OFFLOAD_DEBUG_TRACE(2, "params[%d] = { %d, %d }\n",
            i, params[i].type, params[i].nodeid);
    }

    // Check if V2 myoLibInit is available
    if (myo_wrapper.PostInitFuncSupported()) {
        // Set the host post libInit function indicator
        params[mic_engines_total].type =
            MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC;
        params[mic_engines_total].nodeid =
            MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_HOST_NODE;
        OFFLOAD_DEBUG_TRACE(2, "params[%d] = { %d, %d }\n",
            mic_engines_total,
            params[mic_engines_total].type, params[mic_engines_total].nodeid);
    
        // Set the host post libInit host function address
        ((MyoiUserParamsPostLibInit*)(&params[mic_engines_total+1]))->
            postLibInitHostFuncAddress =
                (void (*)())&__offload_propagate_shared_vars;
        OFFLOAD_DEBUG_TRACE(2, "params[%d] = { %p }\n",
            mic_engines_total+1,
            ((MyoiUserParamsPostLibInit*)(&params[mic_engines_total+1]))->
                postLibInitHostFuncAddress);
    
        // Set the target post libInit function indicator
        params[mic_engines_total+2].type =
            MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC;
        params[mic_engines_total+2].nodeid =
            MYOI_USERPARAMS_POST_MYO_LIB_INIT_FUNC_ALL_NODES;
    
        // Set the target post libInit target function name
        ((MyoiUserParamsPostLibInit*)(&params[mic_engines_total+3]))->
            postLibInitRemoveFuncName = "--vtable_initializer--";
        OFFLOAD_DEBUG_TRACE(2, "params[%d] = { %s }\n",
            mic_engines_total+3,
            ((MyoiUserParamsPostLibInit*)(&params[mic_engines_total+1]))->
                postLibInitRemoveFuncName);
    
        params[mic_engines_total+4].type = MYOI_USERPARAMS_LAST_MSG;
        params[mic_engines_total+4].nodeid = 0;
        OFFLOAD_DEBUG_TRACE(2, "params[%d] = { %d, %d }\n",
            mic_engines_total+4,
            params[mic_engines_total+4].type,
            params[mic_engines_total+4].nodeid);
    } else {
        params[mic_engines_total].type = MYOI_USERPARAMS_LAST_MSG;
        params[mic_engines_total].nodeid = 0;
        OFFLOAD_DEBUG_TRACE(2, "params[%d] = { %d, %d }\n",
            mic_engines_total,
            params[mic_engines_total].type, params[mic_engines_total].nodeid);
    }

    // initialize myo runtime on host
    myo_wrapper.LibInit(params, 0);

    // wait for the target init calls to finish
    COIRESULT res;
    res = COI::EventWait(mic_engines_total, events, -1, 1, 0, 0);
    if (res != COI_SUCCESS) {
        LIBOFFLOAD_ERROR(c_event_wait, res);
        exit(1);
    }

    myo_is_available = true;
    OFFLOAD_DEBUG_TRACE(2, "setting myo_is_available=%d\n", myo_is_available);

    OFFLOAD_DEBUG_TRACE(2, "Initializing MYO library ... done\n");
}

static bool __offload_myoInit(void)
{
    static OffloadOnceControl ctrl = OFFLOAD_ONCE_CONTROL_INIT;
    __offload_run_once(&ctrl, __offload_myoInit_once);

    // Check if using V1 myoLibInit
    if (!myo_wrapper.PostInitFuncSupported()) {
        __offload_propagate_shared_vars();
    }

    return myo_is_available;
}

static void __offload_propagate_shared_vars()
{
    // Propagate pending shared var tables
    if (__myo_tables) {
        mutex_locker_t locker(__myo_table_lock);

        if (__myo_tables) {
            //  Give tables with MYO so it can propagate to target
            for(MyoTableList::const_iterator it = __myo_table_list.begin();
                it != __myo_table_list.end(); ++it) {
#ifdef TARGET_WINNT
                for (SharedTableEntry *entry = it->var_tab;
                     entry->varName != MYO_TABLE_END_MARKER(); entry++) {
                    if (entry->varName == 0) {
                        continue;
                    }
                    myo_wrapper.HostVarTablePropagate(entry, 1);
                    OFFLOAD_DEBUG_TRACE(2, "HostVarTablePropagate(%s, 1)\n",
                        entry->varName);
                }
#else // TARGET_WINNT
                myo_wrapper.HostVarTablePropagate(it->var_tab,
                                                  it->var_tab_len);
#endif // TARGET_WINNT
            }

            __myo_table_list.clear();
            __myo_tables = false;
        }
    }
}

static bool shared_table_entries(
    SharedTableEntry *entry
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, entry);

    for (; entry->varName != MYO_TABLE_END_MARKER(); entry++) {
#ifdef TARGET_WINNT
        if (entry->varName == 0) {
            continue;
        }
#endif // TARGET_WINNT

        return true;
    }

    return false;
}

static bool fptr_table_entries(
    FptrTableEntry *entry
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, entry);

    for (; entry->funcName != MYO_TABLE_END_MARKER(); entry++) {
#ifdef TARGET_WINNT
        if (entry->funcName == 0) {
            continue;
        }
#endif // TARGET_WINNT

        return true;
    }

    return false;
}

extern "C" void __offload_myoRegisterTables(
    InitTableEntry* init_table,
    SharedTableEntry *shared_table,
    FptrTableEntry *fptr_table
)
{
    // check whether we need to initialize MYO library. It is
    // initialized only if at least one myo table is not empty
    if (shared_table_entries(shared_table) || fptr_table_entries(fptr_table)) {
        // make sure myo library is loaded
        __offload_myoLoadLibrary();

        // register tables
        __offload_myo_shared_table_process(shared_table);
        __offload_myo_fptr_table_process(fptr_table);
        __offload_myo_shared_init_table_process(init_table);
    }
}

extern "C" bool __offload_myoProcessTables(
    const void* image,
    MYOInitTableList::Node *init_table,
    MYOVarTableList::Node  *shared_table,
    MYOVarTableList::Node  *shared_vtable,
    MYOFuncTableList::Node *fptr_table
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);

    // Collect the tables in this .dll/.so
    __offload_myoRegisterTables1(
        init_table, shared_table, shared_vtable, fptr_table);

    // Now check what type of module we are dealing with
    if (__offload_target_image_is_executable(image)) {
        OFFLOAD_DEBUG_TRACE(2, "Main encountered\n");
        OFFLOAD_DEBUG_TRACE(2, "MYO initialization not deferred\n");
        // MYO tables across dlls have been collected
        // Now init MYO and process the tables
        __offload_myoProcessDeferredTables();
        // Return true to indicate that atexit needs to be calld by ofldbegin
        return true;
    } else {
        // This is a shared library, either auto-loaded or dynamically loaded
        // If __target_exe is set, then main has started running
        if (__target_exe != 0) {
            // Main is running: this is a dynamic load of a shared library
            // Finish processing the tables in this library
            OFFLOAD_DEBUG_TRACE(2,
                "Dynamically loaded shared library encountered\n");
            OFFLOAD_DEBUG_TRACE(2,
                "MYO initialization not deferred\n");
            __offload_myoProcessDeferredTables();
        } else {
            // Main is not running: this is an auto-loaded shared library
            // Tables have been collected, nothing else to do
            OFFLOAD_DEBUG_TRACE(2,
                "Auto-loaded shared library encountered\n");
            OFFLOAD_DEBUG_TRACE(2, "Deferring initialization of MYO\n");
        }
        return false;
    }
}

// Process contents of all Var tables
void MYOVarTableList::process()
{
    OFFLOAD_DEBUG_TRACE(2, "Process MYO Var tables:\n");

    m_lock.lock();

    for (Node *n = m_head; n != 0; n = n->next) {
        __offload_myo_shared_table_process(
            (SharedTableEntry*)n->table.entries);
    }
    for (Node *n = m_head; n != 0; n = n->next) {
        remove_table(n);
    }

    m_lock.unlock();
}

// Process contents of all Var tables
void MYOVarTableList::process_vtable()
{
    OFFLOAD_DEBUG_TRACE(2, "Process MYO Vtable tables:\n");

    m_lock.lock();

    for (Node *n = m_head; n != 0; n = n->next) {
        __offload_myo_shared_vtable_process(
            (SharedTableEntry*)n->table.entries);
    }
    for (Node *n = m_head; n != 0; n = n->next) {
        remove_table(n);
    }

    m_lock.unlock();
}

// Process contents of all Func tables
void MYOFuncTableList::process()
{
    OFFLOAD_DEBUG_TRACE(2, "Process MYO Func tables:\n");

    m_lock.lock();

    for (Node *n = m_head; n != 0; n = n->next) {
        __offload_myo_fptr_table_process(
            (FptrTableEntry*)n->table.entries);
    }
    for (Node *n = m_head; n != 0; n = n->next) {
        remove_table(n);
    }

    m_lock.unlock();
}

// Process contents of all Init tables
void MYOInitTableList::process()
{
    OFFLOAD_DEBUG_TRACE(2, "Process MYO Init tables:\n");

    m_lock.lock();

    for (Node *n = m_head; n != 0; n = n->next) {
        __offload_myo_shared_init_table_process(
            (InitTableEntry*)n->table.entries);
    }
    for (Node *n = m_head; n != 0; n = n->next) {
        remove_table(n);
    }

    m_lock.unlock();
}

static void __offload_myoProcessDeferredTables()
{
    OFFLOAD_DEBUG_TRACE(3, "%s()\n", __func__);

    // Debug dumps of MYO tables
    if (console_enabled >= 2) {
        __offload_myo_var_tables.dump();
        __offload_myo_vtable_tables.dump();
        __offload_myo_func_tables.dump();
        __offload_myo_init_tables.dump();
    }

    if (!__offload_myo_var_tables.is_empty() ||
        !__offload_myo_vtable_tables.is_empty() ||
        !__offload_myo_func_tables.is_empty() ||
        !__offload_myo_init_tables.is_empty())
    {
        OFFLOAD_DEBUG_TRACE(3, "MYO usage detected in program\n");

        // Make sure myo library is loaded
        __offload_myoLoadLibrary();
        OFFLOAD_DEBUG_TRACE(3, "Initialized MYO\n");

        __offload_myo_var_tables.process();
        __offload_myo_vtable_tables.process_vtable();
        __offload_myo_func_tables.process();
        __offload_myo_init_tables.process();
        OFFLOAD_DEBUG_TRACE(3, "Finished processing MYO tables\n");
    } else {
        OFFLOAD_DEBUG_TRACE(3,
            "MYO tables are empty; Will not initialize MYO\n");
    }
}

DLL_LOCAL void __offload_myoFini(void)
{
    if (myo_is_available) {
        OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);

        COIEVENT events[MIC_ENGINES_MAX];

        // kick off myoiLibFini calls on all devices
        for (int i = 0; i < mic_engines_total; i++) {
            mic_engines[i].fini_myo(&events[i]);
        }

        // cleanup myo runtime on host
        myo_wrapper.LibFini();

        // wait for the target fini calls to finish
        COIRESULT res;
        res = COI::EventWait(mic_engines_total, events, -1, 1, 0, 0);
        if (res != COI_SUCCESS) {
            LIBOFFLOAD_ERROR(c_event_wait, res);
            exit(1);
        }
    }
}

static void __offload_myo_shared_table_process(
    SharedTableEntry *entry
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, entry);

    SharedTableEntry *start = entry;
    int entries = 0;

    // allocate shared memory for vars
    for (; entry->varName != MYO_TABLE_END_MARKER(); entry++) {
#ifdef TARGET_WINNT
        if (entry->varName == 0) {
            OFFLOAD_DEBUG_TRACE(4,
                "skip registering a NULL MyoSharedTable entry\n");
            continue;
        }
#endif // TARGET_WINNT

        OFFLOAD_DEBUG_TRACE(4, "registering MyoSharedTable entry for %s @%p\n",
                            entry->varName, entry);

        // Invoke the function to create shared memory
        reinterpret_cast<void(*)(void)>(entry->sharedAddr)();
        entries++;
    }

    // and table to the list if it is not empty
    if (entries > 0) {
        mutex_locker_t locker(__myo_table_lock);
        __myo_table_list.push_back(MyoTable(start, entries));
        __myo_tables = true;
    }
}

static void __offload_myo_shared_vtable_process(
    SharedTableEntry *entry
)
{
    SharedTableEntry *start = entry;
    int entries = 0;

    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, entry);

    // allocate shared memory for vtables
    for (; entry->varName != MYO_TABLE_END_MARKER(); entry++) {
#ifdef TARGET_WINNT
        if (entry->varName == 0) {
            OFFLOAD_DEBUG_TRACE(4,
                "skip registering a NULL MyoSharedVTable entry\n");
            continue;
        }
#endif // TARGET_WINNT

        OFFLOAD_DEBUG_TRACE(4,
            "registering MyoSharedVTable entry for %s @%p\n",
                            entry->varName, entry);

        // Invoke the function to create shared memory
        reinterpret_cast<void(*)(MyoArena)>(entry->sharedAddr)(
                                                myo_wrapper.GetVtableArena());
        entries++;
    }

    // add table to the list if it is not empty
    if (entries > 0) {
        mutex_locker_t locker(__myo_table_lock);
        __myo_table_list.push_back(MyoTable(start, entries));
        __myo_tables = true;
    }
}

void __offload_myo_shared_init_table_process(InitTableEntry* entry)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, entry);

#ifdef TARGET_WINNT
    for (; entry->funcName != MYO_TABLE_END_MARKER(); entry++) {
        if (entry->funcName == 0) {
            OFFLOAD_DEBUG_TRACE(4,
                "skip registering a NULL MyoSharedInit entry\n");
            continue;
        }

        //  Invoke the function to init the shared memory
        OFFLOAD_DEBUG_TRACE(4, "execute MyoSharedInit routine for %s\n",
            entry->funcName);
        entry->func(myo_wrapper.GetVtableArena());
    }
#else // TARGET_WINNT
    for (; entry->func != 0; entry++) {
        // Invoke the function to init the shared memory
        entry->func(myo_wrapper.GetVtableArena());
    }
#endif // TARGET_WINNT
}

static void __offload_myo_fptr_table_process(
    FptrTableEntry *entry
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, entry);

    FptrTableEntry *start = entry;
    int entries = 0;

    for (; entry->funcName != MYO_TABLE_END_MARKER(); entry++) {
#ifdef TARGET_WINNT
        if (entry->funcName == 0) {
            OFFLOAD_DEBUG_TRACE(4,
                "skip registering a NULL MyoFptrTable entry\n");
            continue;
        }
#endif // TARGET_WINNT

        if (!myo_wrapper.is_available()) {
            *(static_cast<void**>(entry->localThunkAddr)) = entry->funcAddr;
        }

        OFFLOAD_DEBUG_TRACE(4, "registering MyoFptrTable entry for %s @%p\n",
                            entry->funcName, entry);

#ifdef TARGET_WINNT
        if (myo_wrapper.is_available()) {
            myo_wrapper.HostFptrTableRegister(entry, 1, false);
        }
#endif // TARGET_WINNT

        entries++;
    }

#ifndef TARGET_WINNT
    if (myo_wrapper.is_available() && entries > 0) {
        myo_wrapper.HostFptrTableRegister(start, entries, false);
    }
#endif // TARGET_WINNT
}

extern "C" int __offload_myoIsAvailable(int target_number)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%d)\n", __func__, target_number);

    if (target_number >= -2) {
        bool is_default_number = (target_number == -2);

        if (__offload_myoInit()) {
            if (target_number >= 0) {
                // User provided the device number
                int num = target_number % mic_engines_total;

                // reserve device in ORSL
                target_number = ORSL::reserve(num) ? num : -1;
            }
            else {
                // try to use device 0
                target_number = ORSL::reserve(0) ? 0 : -1;
            }

            // make sure device is initialized
            if (target_number >= 0) {
                mic_engines[target_number].init();
            }
        }
        else {
            // fallback to CPU
            target_number = -1;
        }

        if (target_number < 0 && !is_default_number) {
            LIBOFFLOAD_ERROR(c_device_is_not_available);
            exit(1);
        }
    }
    else {
        LIBOFFLOAD_ERROR(c_invalid_device_number);
        exit(1);
    }

    return target_number;
}

extern "C" void __offload_myoiRemoteIThunkCall(
    void *thunk,
    void *arg,
    int target_number
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p, %p, %d)\n", __func__, thunk, arg,
                        target_number);

    myo_wrapper.Release();
    myo_wrapper.RemoteThunkCall(thunk, arg, target_number);
    myo_wrapper.Acquire();

    ORSL::release(target_number);
}

extern "C" void* _Offload_shared_malloc(size_t size)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%lld)\n", __func__, size);

    if (__offload_myoLoadLibrary()) {
        return myo_wrapper.SharedMalloc(size);
    }
    else {
        return malloc(size);
    }
}

extern "C" void _Offload_shared_free(void *ptr)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, ptr);

    if (__offload_myoLoadLibrary()) {
        myo_wrapper.SharedFree(ptr);
    }
    else {
        free(ptr);
    }
}

extern "C" void* _Offload_shared_aligned_malloc(size_t size, size_t align)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%lld, %lld)\n", __func__, size, align);

    if (__offload_myoLoadLibrary()) {
        return myo_wrapper.SharedAlignedMalloc(size, align);
    }
    else {
        if (align < sizeof(void*)) {
            align = sizeof(void*);
        }
        return _mm_malloc(size, align);
    }
}

extern "C" void _Offload_shared_aligned_free(void *ptr)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, ptr);

    if (__offload_myoLoadLibrary()) {
        myo_wrapper.SharedAlignedFree(ptr);
    }
    else {
        _mm_free(ptr);
    }
}

extern "C" void _Offload_shared_arena_create(
    MyoOwnershipType ownership,
    int consistency,
    MyoArena* arena
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%d, %d, %p)\n",
        __func__, ownership, consistency, arena);

    if (__offload_myoLoadLibrary()) {
        myo_wrapper.ArenaCreate(ownership, consistency, arena);
    }
}

extern "C" void* _Offload_shared_aligned_arena_malloc(
    MyoArena arena,
    size_t size,
    size_t align
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%u, %lld, %lld)\n",
        __func__, arena, size, align);

    if (__offload_myoLoadLibrary()) {
        void *p = myo_wrapper.SharedAlignedArenaMalloc(arena, size, align);
        OFFLOAD_DEBUG_TRACE(3, "%s(%u, %lld, %lld)->%p\n",
            __func__, arena, size, align, p);
        return p;
    }
    else {
        if (align < sizeof(void*)) {
            align = sizeof(void*);
        }
        return _mm_malloc(size, align);
    }
}

extern "C" void _Offload_shared_aligned_arena_free(
    MyoArena arena,
    void *ptr
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%u, %p)\n", __func__, arena, ptr);

    if (__offload_myoLoadLibrary()) {
        myo_wrapper.SharedAlignedArenaFree(arena, ptr);
    }
    else {
        _mm_free(ptr);
    }
}

extern "C" void _Offload_shared_arena_acquire(
    MyoArena arena
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%u)\n", __func__, arena);

    if (__offload_myoLoadLibrary()) {
        myo_wrapper.ArenaAcquire(arena);
    }
}

extern "C" void _Offload_shared_arena_release(
    MyoArena arena
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%u)\n", __func__, arena);

    if (__offload_myoLoadLibrary()) {
        myo_wrapper.ArenaRelease(arena);
    }
}

extern "C" void __intel_cilk_for_32_offload(
    int size,
    void (*copy_constructor)(void*, void*),
    int target_number,
    void *raddr,
    void *closure_object,
    unsigned int iters,
    unsigned int grain_size)
{
    OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);

    target_number = __offload_myoIsAvailable(target_number);
    if (target_number >= 0) {
        struct S {
            void *M1;
            unsigned int M2;
            unsigned int M3;
            char closure[];
        } *args;

        args = (struct S*) _Offload_shared_malloc(sizeof(struct S) + size);
        if (args == NULL)
          LIBOFFLOAD_ERROR(c_malloc);
        args->M1 = raddr;
        args->M2 = iters;
        args->M3 = grain_size;

        if (copy_constructor == 0) {
            memcpy(args->closure, closure_object, size);
        }
        else {
            copy_constructor(args->closure, closure_object);
        }

        myo_wrapper.Release();
        myo_wrapper.GetResult(
            myo_wrapper.RemoteCall("__intel_cilk_for_32_offload",
                                   args, target_number)
        );
        myo_wrapper.Acquire();

        _Offload_shared_free(args);

        ORSL::release(target_number);
    }
    else {
        __cilkrts_cilk_for_32(raddr,
                              closure_object,
                              iters,
                              grain_size);
    }
}

extern "C" void __intel_cilk_for_64_offload(
    int size,
    void (*copy_constructor)(void*, void*),
    int target_number,
    void *raddr,
    void *closure_object,
    uint64_t iters,
    uint64_t grain_size)
{
    OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);

    target_number = __offload_myoIsAvailable(target_number);
    if (target_number >= 0) {
        struct S {
            void *M1;
            uint64_t M2;
            uint64_t M3;
            char closure[];
        } *args;

        args = (struct S*) _Offload_shared_malloc(sizeof(struct S) + size);
        if (args == NULL)
          LIBOFFLOAD_ERROR(c_malloc);
        args->M1 = raddr;
        args->M2 = iters;
        args->M3 = grain_size;

        if (copy_constructor == 0) {
            memcpy(args->closure, closure_object, size);
        }
        else {
            copy_constructor(args->closure, closure_object);
        }

        myo_wrapper.Release();
        myo_wrapper.GetResult(
            myo_wrapper.RemoteCall("__intel_cilk_for_64_offload", args,
                                   target_number)
        );
        myo_wrapper.Acquire();

        _Offload_shared_free(args);

        ORSL::release(target_number);
    }
    else {
        __cilkrts_cilk_for_64(raddr,
                              closure_object,
                              iters,
                              grain_size);
    }
}
