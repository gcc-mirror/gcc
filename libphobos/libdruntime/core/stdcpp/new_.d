/**
 * D binding to C++ <new>
 *
 * Copyright: Copyright (c) 2019 D Language Foundation
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/new_.d)
 */

module core.stdcpp.new_;

import core.stdcpp.xutility : __cpp_sized_deallocation, __cpp_aligned_new;
import core.stdcpp.exception : exception;

// TODO: this really should come from __traits(getTargetInfo, "defaultNewAlignment")
version (D_LP64)
    enum size_t __STDCPP_DEFAULT_NEW_ALIGNMENT__ = 16;
else
    enum size_t __STDCPP_DEFAULT_NEW_ALIGNMENT__ = 8;

extern (C++, "std")
{
    ///
    struct nothrow_t {}

    ///
    enum align_val_t : size_t { defaultAlignment = __STDCPP_DEFAULT_NEW_ALIGNMENT__ }

    ///
    class bad_alloc : exception
    {
    @nogc:
        ///
        this() { super("bad allocation", 1); }
    }
}


///
T* cpp_new(T, Args...)(auto ref Args args) if (!is(T == class))
{
    import core.lifetime : emplace, forward;

    T* mem = cast(T*)__cpp_new(T.sizeof);
    return mem.emplace(forward!args);
}

///
T cpp_new(T, Args...)(auto ref Args args) if (is(T == class))
{
    import core.lifetime : emplace, forward;

    T mem = cast(T)__cpp_new(__traits(classInstanceSize, T));
    return mem.emplace(forward!args);
}

///
void cpp_delete(T)(T* ptr) if (!is(T == class))
{
    destroy!false(*ptr);
    __cpp_delete(ptr);
}

///
void cpp_delete(T)(T instance) if (is(T == class))
{
    destroy!false(instance);
    __cpp_delete(cast(void*) instance);
}


// raw C++ functions
extern(C++):
@nogc:

/// Binding for ::operator new(std::size_t count)
pragma(mangle, __new_mangle)
void* __cpp_new(size_t count);

/// Binding for ::operator new(std::size_t count, const std::nothrow_t&)
pragma(mangle, __new_nothrow_mangle)
void* __cpp_new_nothrow(size_t count, ref const(nothrow_t) = std_nothrow) nothrow;

/// Binding for ::operator delete(void* ptr)
pragma(mangle, __delete_mangle)
void __cpp_delete(void* ptr);

/// Binding for ::operator delete(void* ptr, const std::nothrow_t& tag)
pragma(mangle, __delete_nothrow_mangle)
void __cpp_delete_nothrow(void* ptr, ref const(nothrow_t) = std_nothrow) nothrow;

static if (__cpp_sized_deallocation)
{
    /// Binding for ::operator delete(void* ptr, size_t size)
    pragma(mangle, __delete_size_mangle)
    void __cpp_delete_size(void* ptr, size_t size);
}
static if (__cpp_aligned_new)
{
    /// Binding for ::operator new(std::size_t count, std::align_val_t al)
    pragma(mangle, __new_align_mangle)
    void* __cpp_new_aligned(size_t count, align_val_t alignment);

    /// Binding for ::operator new(std::size_t count, std::align_val_t al, const std::nothrow_t&)
    pragma(mangle, __new_aligned_nothrow_mangle)
    void* __cpp_new_aligned_nothrow(size_t count, align_val_t alignment, ref const(nothrow_t) = std_nothrow) nothrow;

    /// Binding for ::operator delete(void* ptr, std::align_val_t al)
    pragma(mangle, __delete_align_mangle)
    void __cpp_delete_aligned(void* ptr, align_val_t alignment);

    /// Binding for ::operator delete(void* ptr, std::align_val_t al, const std::nothrow_t& tag)
    pragma(mangle, __delete_align_nothrow_mangle)
    void __cpp_delete_align_nothrow(void* ptr, align_val_t alignment, ref const(nothrow_t) = std_nothrow) nothrow;

    /// Binding for ::operator delete(void* ptr, size_t size, std::align_val_t al)
    pragma(mangle, __delete_size_align_mangle)
    void __cpp_delete_size_aligned(void* ptr, size_t size, align_val_t alignment);
}

private:
extern (D):

__gshared immutable nothrow_t std_nothrow;

// we have to hard-code the mangling for the global new/delete operators
version (CppRuntime_Microsoft)
{
    version (D_LP64)
    {
        enum __new_mangle                   = "??2@YAPEAX_K@Z";
        enum __new_nothrow_mangle           = "??2@YAPEAX_KAEBUnothrow_t@std@@@Z";
        enum __delete_mangle                = "??3@YAXPEAX@Z";
        enum __delete_nothrow_mangle        = "??3@YAXPEAXAEBUnothrow_t@std@@@Z";
        enum __delete_size_mangle           = "??3@YAXPEAX_K@Z";
        enum __new_align_mangle             = "??2@YAPEAX_KW4align_val_t@std@@@Z";
        enum __new_aligned_nothrow_mangle   = "??2@YAPEAX_KW4align_val_t@std@@AEBUnothrow_t@1@@Z";
        enum __delete_align_mangle          = "??3@YAXPEAXW4align_val_t@std@@@Z";
        enum __delete_align_nothrow_mangle  = "??3@YAXPEAXW4align_val_t@std@@AEBUnothrow_t@1@@Z";
        enum __delete_size_align_mangle     = "??3@YAXPEAX_KW4align_val_t@std@@@Z";
    }
    else
    {
        enum __new_mangle                   = "??2@YAPAXI@Z";
        enum __new_nothrow_mangle           = "??2@YAPAXIABUnothrow_t@std@@@Z";
        enum __delete_mangle                = "??3@YAXPAX@Z";
        enum __delete_nothrow_mangle        = "??3@YAXPAXABUnothrow_t@std@@@Z";
        enum __delete_size_mangle           = "??3@YAXPAXI@Z";
        enum __new_align_mangle             = "??2@YAPAXIW4align_val_t@std@@@Z";
        enum __new_aligned_nothrow_mangle   = "??2@YAPAXIW4align_val_t@std@@ABUnothrow_t@1@@Z";
        enum __delete_align_mangle          = "??3@YAXPAXW4align_val_t@std@@@Z";
        enum __delete_align_nothrow_mangle  = "??3@YAXPAXW4align_val_t@std@@ABUnothrow_t@1@@Z";
        enum __delete_size_align_mangle     = "??3@YAXPAXIW4align_val_t@std@@@Z";
    }
}
else
{
    version (D_LP64)
    {
        enum __new_mangle                   = "_Znwm";
        enum __new_nothrow_mangle           = "_ZnwmRKSt9nothrow_t";
        enum __delete_mangle                = "_ZdlPv";
        enum __delete_nothrow_mangle        = "_ZdlPvRKSt9nothrow_t";
        enum __delete_size_mangle           = "_ZdlPvm";
        enum __new_align_mangle             = "_ZnwmSt11align_val_t";
        enum __new_aligned_nothrow_mangle   = "_ZnwmSt11align_val_tRKSt9nothrow_t";
        enum __delete_align_mangle          = "_ZdlPvSt11align_val_t";
        enum __delete_align_nothrow_mangle  = "_ZdlPvSt11align_val_tRKSt9nothrow_t";
        enum __delete_size_align_mangle     = "_ZdlPvmSt11align_val_t";
    }
    else
    {
        enum __new_mangle                   = "_Znwj";
        enum __new_nothrow_mangle           = "_ZnwjRKSt9nothrow_t";
        enum __delete_mangle                = "_ZdlPv";
        enum __delete_nothrow_mangle        = "_ZdlPvRKSt9nothrow_t";
        enum __delete_size_mangle           = "_ZdlPvj";
        enum __new_align_mangle             = "_ZnwjSt11align_val_t";
        enum __new_aligned_nothrow_mangle   = "_ZnwjSt11align_val_tRKSt9nothrow_t";
        enum __delete_align_mangle          = "_ZdlPvSt11align_val_t";
        enum __delete_align_nothrow_mangle  = "_ZdlPvSt11align_val_tRKSt9nothrow_t";
        enum __delete_size_align_mangle     = "_ZdlPvjSt11align_val_t";
    }
}
