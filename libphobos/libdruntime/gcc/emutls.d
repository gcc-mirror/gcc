// GNU D Compiler emulated TLS routines.
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

// This code is based on the libgcc emutls.c emulated TLS support.

module gcc.emutls;

import core.atomic, core.stdc.stdlib, core.stdc.string, core.sync.mutex;
import rt.util.container.array, rt.util.container.hashtab;
import core.internal.traits : classInstanceAlignment;
import gcc.builtins, gcc.gthread;

version (GNU_EMUTLS): private:

alias word = __builtin_machine_uint;
alias pointer = __builtin_pointer_uint;
alias TlsArray = Array!(void**);

/*
 * TLS control data emitted by GCC for every TLS variable.
 */
struct __emutls_object
{
    word size;
    word align_;
    union
    {
        pointer offset;
        void* ptr;
    }

    ubyte* templ;
}

// Per-thread key to obtain the per-thread TLS variable array
__gshared __gthread_key_t emutlsKey;
// Largest, currently assigned TLS variable offset
__gshared pointer emutlsMaxOffset = 0;
// Contains the size of the TLS variables (for GC)
__gshared Array!word emutlsSizes;
// Contains the TLS variable array for single-threaded apps
__gshared TlsArray singleArray;
// List of all currently alive TlsArrays (for GC)
__gshared HashTab!(TlsArray*, TlsArray*) emutlsArrays;

// emutlsMutex Mutex + @nogc handling
enum mutexAlign = classInstanceAlignment!Mutex;
enum mutexClassInstanceSize = __traits(classInstanceSize, Mutex);
__gshared align(mutexAlign) void[mutexClassInstanceSize] _emutlsMutex;

@property Mutex emutlsMutex() nothrow @nogc
{
    return cast(Mutex) _emutlsMutex.ptr;
}

/*
 * Global (de)initialization functions
 */
extern (C) void _d_emutls_init() nothrow @nogc
{
    memcpy(_emutlsMutex.ptr, typeid(Mutex).initializer.ptr, _emutlsMutex.length);
    (cast(Mutex) _emutlsMutex.ptr).__ctor();

    if (__gthread_key_create(&emutlsKey, &emutlsDestroyThread) != 0)
        abort();
}

__gshared __gthread_once_t initOnce = GTHREAD_ONCE_INIT;

/*
 * emutls main entrypoint, called by GCC for each TLS variable access.
 */
extern (C) void* __emutls_get_address(shared __emutls_object* obj) nothrow @nogc
{
    pointer offset;
    if (__gthread_active_p())
    {
        // Obtain the offset index into the TLS array (same for all-threads)
        // for requested var. If it is unset, obtain a new offset index.
        offset = atomicLoad!(MemoryOrder.acq, pointer)(obj.offset);
        if (__builtin_expect(offset == 0, 0))
        {
            __gthread_once(&initOnce, &_d_emutls_init);
            emutlsMutex.lock_nothrow();

            offset = obj.offset;
            if (offset == 0)
            {
                offset = ++emutlsMaxOffset;

                emutlsSizes.ensureLength(offset);
                // Note: it's important that we copy any data from obj and
                // do not keep an reference to obj itself: If a library is
                // unloaded, its tls variables are not removed from the arrays
                // and the GC will still scan these. If we then try to reference
                // a pointer to the data segment of an unloaded library, this
                // will crash.
                emutlsSizes[offset - 1] = obj.size;

                atomicStore!(MemoryOrder.rel, pointer)(obj.offset, offset);
            }
            emutlsMutex.unlock_nothrow();
        }
    }
    // For single-threaded systems, don't synchronize
    else
    {
        if (__builtin_expect(obj.offset == 0, 0))
        {
            offset = ++emutlsMaxOffset;

            emutlsSizes.ensureLength(offset);
            emutlsSizes[offset - 1] = obj.size;

            obj.offset = offset;
        }
    }

    TlsArray* arr;
    if (__gthread_active_p())
        arr = cast(TlsArray*) __gthread_getspecific(emutlsKey);
    else
        arr = &singleArray;

    // This will always be false for singleArray
    if (__builtin_expect(arr == null, 0))
    {
        arr = mallocTlsArray(offset);
        __gthread_setspecific(emutlsKey, arr);
        emutlsMutex.lock_nothrow();
        emutlsArrays[arr] = arr;
        emutlsMutex.unlock_nothrow();
    }
    // Check if we have to grow the per-thread array
    else if (__builtin_expect(offset > arr.length, 0))
    {
        (*arr).ensureLength(offset);
    }

    // Offset 0 is used as a not-initialized marker above. In the
    // TLS array, we start at 0.
    auto index = offset - 1;

    // Get the per-thread pointer from the TLS array
    void** ret = (*arr)[index];
    if (__builtin_expect(ret == null, 0))
    {
        // Initial access, have to allocate the storage
        ret = emutlsAlloc(obj);
        (*arr)[index] = ret;
    }

    return ret;
}

// 1:1 copy from libgcc emutls.c
extern (C) void __emutls_register_common(__emutls_object* obj, word size, word align_, ubyte* templ) nothrow @nogc
{
    if (obj.size < size)
    {
        obj.size = size;
        obj.templ = null;
    }
    if (obj.align_ < align_)
        obj.align_ = align_;
    if (templ && size == obj.size)
        obj.templ = templ;
}

// 1:1 copy from libgcc emutls.c
void** emutlsAlloc(shared __emutls_object* obj) nothrow @nogc
{
    void* ptr;
    void* ret;
    enum pointerSize = (void*).sizeof;

    /* We could use here posix_memalign if available and adjust
     emutls_destroy accordingly.  */
    if ((cast() obj).align_ <= pointerSize)
    {
        ptr = malloc((cast() obj).size + pointerSize);
        if (ptr == null)
            abort();
        (cast(void**) ptr)[0] = ptr;
        ret = ptr + pointerSize;
    }
    else
    {
        ptr = malloc(obj.size + pointerSize + obj.align_ - 1);
        if (ptr == null)
            abort();
        ret = cast(void*)((cast(pointer)(ptr + pointerSize + obj.align_ - 1)) & ~cast(
                pointer)(obj.align_ - 1));
        (cast(void**) ret)[-1] = ptr;
    }

    if (obj.templ)
        memcpy(ret, cast(ubyte*) obj.templ, cast() obj.size);
    else
        memset(ret, 0, cast() obj.size);

    return cast(void**) ret;
}

/*
 * When a thread has finished, remove the TLS array from the GC
 * scan list emutlsArrays, free all allocated TLS variables and
 * finally free the array.
 */
extern (C) void emutlsDestroyThread(void* ptr) nothrow @nogc
{
    auto arr = cast(TlsArray*) ptr;
    emutlsMutex.lock_nothrow();
    emutlsArrays.remove(arr);
    emutlsMutex.unlock_nothrow();

    foreach (entry; *arr)
    {
        if (entry)
            free(entry[-1]);
    }

    free(arr);
}

/*
 * Allocate a new TLS array, set length according to offset.
 */
TlsArray* mallocTlsArray(pointer offset = 0) nothrow @nogc
{
    static assert(TlsArray.alignof == (void*).alignof);
    void[] data = malloc(TlsArray.sizeof)[0 .. TlsArray.sizeof];
    if (data.ptr == null)
        abort();

    static immutable TlsArray init = TlsArray.init;
    memcpy(data.ptr, &init, data.length);
    (cast(TlsArray*) data).length = 32;
    return cast(TlsArray*) data.ptr;
}

/*
 * Make sure array is large enough to hold an entry for offset.
 * Note: the array index will be offset - 1!
 */
void ensureLength(Value)(ref Array!(Value) arr, size_t offset) nothrow @nogc
{
    // index is offset-1
    if (offset > arr.length)
    {
        auto newSize = arr.length * 2;
        if (offset > newSize)
            newSize = offset + 32;
        arr.length = newSize;
    }
}

// Public interface
public:
void _d_emutls_scan(scope void delegate(void* pbeg, void* pend) nothrow cb) nothrow
{
    void scanArray(scope TlsArray* arr) nothrow
    {
        foreach (index, entry; *arr)
        {
            auto ptr = cast(void*) entry;
            if (ptr)
                cb(ptr, ptr + emutlsSizes[index]);
        }
    }

    __gthread_once(&initOnce, &_d_emutls_init);
    emutlsMutex.lock_nothrow();
    // this code is effectively nothrow
    try
    {
        foreach (arr, value; emutlsArrays)
        {
            scanArray(arr);
        }
    }
    catch (Exception)
    {
    }
    emutlsMutex.unlock_nothrow();
    scanArray(&singleArray);
}

// Call this after druntime has been unloaded
void _d_emutls_destroy() nothrow @nogc
{
    if (__gthread_key_delete(emutlsKey) != 0)
        abort();

    (cast(Mutex) _emutlsMutex.ptr).__dtor();
    destroy(emutlsArrays);
}
