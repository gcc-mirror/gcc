// GNU D Compiler thread support for emulated TLS routines.
// Copyright (C) 2019-2021 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

module gcc.gthread;
import gcc.config;

extern (C) nothrow @nogc:

alias GthreadDestroyFn = extern (C) void function(void*);
alias GthreadOnceFn = extern (C) void function();

static if (GNU_Thread_Model == ThreadModel.Posix)
{
    import core.sys.posix.pthread;

    alias __gthread_key_create = pthread_key_create;
    alias __gthread_key_delete = pthread_key_delete;
    alias __gthread_getspecific = pthread_getspecific;
    alias __gthread_setspecific = pthread_setspecific;
    alias __gthread_once = pthread_once;
    alias __gthread_key_t = pthread_key_t;
    alias __gthread_once_t = pthread_once_t;
    enum GTHREAD_ONCE_INIT = PTHREAD_ONCE_INIT;

    // TODO: FreeBSD and Solaris exposes a dummy POSIX threads
    // interface that will need to be handled here.
    extern (D) int __gthread_active_p()
    {
        return 1;
    }
}
else static if (GNU_Thread_Model == ThreadModel.Single)
{
    alias __gthread_key_t = int;
    alias __gthread_once_t = int;
    enum GTHREAD_ONCE_INIT = 0;

    extern (D) int __gthread_key_create(__gthread_key_t*, GthreadDestroyFn)
    {
        return 0;
    }

    extern (D) int __gthread_key_delete(__gthread_key_t)
    {
        return 0;
    }

    extern (D) void* __gthread_getspecific(__gthread_key_t)
    {
        return null;
    }

    extern (D) int __gthread_setspecific(__gthread_key_t, void*)
    {
        return 0;
    }

    extern (D) int __gthread_once(__gthread_once_t*, GthreadOnceFn)
    {
        return 0;
    }

    extern (D) int __gthread_active_p()
    {
        return 0;
    }
}
else static if (GNU_Thread_Model == ThreadModel.Win32)
{
    import core.stdc.config;

    struct __gthread_once_t
    {
        int done;
        c_long started;
    }

    int __gthr_win32_key_create(__gthread_key_t* keyp, GthreadDestroyFn dtor);
    int __gthr_win32_key_delete(__gthread_key_t key);
    void* __gthr_win32_getspecific(__gthread_key_t key);
    int __gthr_win32_setspecific(__gthread_key_t key, const void* ptr);
    int __gthr_win32_once(__gthread_once_t* once, GthreadOnceFn);

    alias __gthread_key_create = __gthr_win32_key_create;
    alias __gthread_key_delete = __gthr_win32_key_delete;
    alias __gthread_getspecific = __gthr_win32_getspecific;
    alias __gthread_setspecific = __gthr_win32_setspecific;
    alias __gthread_once = __gthr_win32_once;
    enum GTHREAD_ONCE_INIT = __gthread_once_t(0, -1);
    alias __gthread_key_t = c_ulong;

    version (MinGW)
    {
        // Mingw runtime >= v0.3 provides a magic variable that is set to nonzero
        // if -mthreads option was specified, or 0 otherwise.
        extern __gshared int _CRT_MT;
    }

    extern (D) int __gthread_active_p()
    {
        version (MinGW)
            return _CRT_MT;
        else
            return 1;
    }
}
else
{
    static assert(false, "Not implemented");
}
