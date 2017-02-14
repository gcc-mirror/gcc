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


#ifndef OFFLOAD_UTIL_H_INCLUDED
#define OFFLOAD_UTIL_H_INCLUDED

#include <stdlib.h>

#ifdef TARGET_WINNT
    #define DLL_LOCAL
#else
    #define DLL_LOCAL  __attribute__((visibility("hidden")))
#endif

#ifdef TARGET_WINNT
// Don't use <stdint.h> as compiling with VS2010 makes ofldbegin.obj
// incompatible with STL library of versions older than VS2010.
typedef unsigned long long int  uint64_t;
typedef signed long long int    int64_t;
#include <windows.h>
#include <process.h>
#else // TARGET_WINNT
#include <stdint.h>
#include <dlfcn.h>
#include <pthread.h>
#endif // TARGET_WINNT

#ifdef TARGET_WINNT
typedef unsigned pthread_key_t;
typedef int pid_t;

#define __func__ __FUNCTION__
#define strtok_r(s,d,p) strtok_s(s,d,p)
#define strcasecmp(a,b) stricmp(a,b)

#define thread_key_create(key, destructor) \
    (((*key = TlsAlloc()) > 0) ? 0 : GetLastError())
#define thread_key_delete(key) TlsFree(key)

#ifndef S_ISREG
#define S_ISREG(mode)  (((mode) & S_IFMT) == S_IFREG)
#endif

void*   thread_getspecific(pthread_key_t key);
int     thread_setspecific(pthread_key_t key, const void *value);
#else
#define thread_key_create(key, destructor) \
            pthread_key_create((key), (destructor))
#define thread_key_delete(key)  pthread_key_delete(key)
#define thread_getspecific(key) pthread_getspecific(key)
#define thread_setspecific(key, value) pthread_setspecific(key, value)
#endif // TARGET_WINNT

// Mutex implementation
struct mutex_t {
    mutex_t() {
#ifdef TARGET_WINNT
        InitializeCriticalSection(&m_lock);
#else // TARGET_WINNT
        pthread_mutex_init(&m_lock, 0);
#endif // TARGET_WINNT
    }

    ~mutex_t() {
#ifdef TARGET_WINNT
        DeleteCriticalSection(&m_lock);
#else // TARGET_WINNT
        pthread_mutex_destroy(&m_lock);
#endif // TARGET_WINNT
    }

    void lock() {
#ifdef TARGET_WINNT
        EnterCriticalSection(&m_lock);
#else // TARGET_WINNT
        pthread_mutex_lock(&m_lock);
#endif // TARGET_WINNT
    }

    void unlock() {
#ifdef TARGET_WINNT
        LeaveCriticalSection(&m_lock);
#else // TARGET_WINNT
        pthread_mutex_unlock(&m_lock);
#endif // TARGET_WINNT
    }

private:
#ifdef TARGET_WINNT
    CRITICAL_SECTION    m_lock;
#else
    pthread_mutex_t     m_lock;
#endif
};

struct mutex_locker_t {
    mutex_locker_t(mutex_t &mutex) : m_mutex(mutex) {
        m_mutex.lock();
    }

    ~mutex_locker_t() {
        m_mutex.unlock();
    }

private:
    mutex_t &m_mutex;
};

// Dynamic loader interface
#ifdef TARGET_WINNT
struct Dl_info
{
    char        dli_fname[MAX_PATH];
    void       *dli_fbase;
    char        dli_sname[MAX_PATH];
    const void *dli_saddr;
};

void*   DL_open(const char *path);
#define DL_close(handle)        FreeLibrary((HMODULE) (handle))
int     DL_addr(const void *addr, Dl_info *info);
#else
#define DL_open(path)           dlopen((path), RTLD_NOW)
#define DL_close(handle)        dlclose(handle)
#define DL_addr(addr, info)     dladdr((addr), (info))
#endif // TARGET_WINNT

DLL_LOCAL extern void* DL_sym(void *handle, const char *name, const char *version);

// One-time initialization API
#ifdef TARGET_WINNT
typedef INIT_ONCE                   OffloadOnceControl;
#define OFFLOAD_ONCE_CONTROL_INIT   INIT_ONCE_STATIC_INIT

extern void __offload_run_once(OffloadOnceControl *ctrl, void (*func)(void));
#else
typedef pthread_once_t              OffloadOnceControl;
#define OFFLOAD_ONCE_CONTROL_INIT   PTHREAD_ONCE_INIT

#define __offload_run_once(ctrl, func) pthread_once(ctrl, func)
#endif // TARGET_WINNT

// Parses size specification string.
DLL_LOCAL extern bool __offload_parse_size_string(const char *str, uint64_t &new_size);

// Parses string with integer value
DLL_LOCAL extern bool __offload_parse_int_string(const char *str, int64_t &value);

// get value by its base, offset and size
DLL_LOCAL int64_t get_el_value(
    char   *base,
    int64_t offset,
    int64_t size
);
#endif // OFFLOAD_UTIL_H_INCLUDED
