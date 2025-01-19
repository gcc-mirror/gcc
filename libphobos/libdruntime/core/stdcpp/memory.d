/**
* D binding to C++ <memory>.
*
* Copyright: Copyright (c) 2019 D Language Foundation
* License: Distributed under the
*      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
*    (See accompanying file LICENSE)
* Authors:   Manu Evans
* Source:    $(DRUNTIMESRC core/stdcpp/memory.d)
*/

module core.stdcpp.memory;

public import core.stdcpp.allocator;

import core.stdcpp.xutility : StdNamespace;

extern(C++, (StdNamespace)):

///
unique_ptr!T make_unique(T, Args...)(auto ref Args args)
{
    import core.lifetime : forward;
    import core.stdcpp.new_ : cpp_new;

    return unique_ptr!T(cpp_new!T(forward!args));
}

///
struct default_delete(T)
{
    ///
    alias pointer = ClassOrPtr!T;

    ///
    void opCall()(pointer ptr) const
    {
        import core.stdcpp.new_ : cpp_delete;

        cpp_delete(ptr);
    }
}

///
extern(C++, class)
struct unique_ptr(T, Deleter = default_delete!T)
{
extern(D):
    ///
    this(this) @disable;

    ///
    ~this()
    {
        reset();
    }

    ///
    ref unique_ptr opAssign(typeof(null))
    {
        reset();
        return this;
    }

    ///
    void reset(pointer p = null)
    {
        pointer t = __ptr();
        __ptr() = p;
        if (t)
            get_deleter()(t);
    }

nothrow pure @safe @nogc:
    ///
    alias pointer = ClassOrPtr!T;
    ///
    alias element_type = T;
    ///
    alias deleter_type = Deleter;

    ///
    this(pointer ptr)
    {
        __ptr() = ptr;
    }

    ///
    inout(pointer) get() inout nothrow
    {
        return __ptr();
    }

    ///
    bool opCast(T : bool)() const nothrow
    {
        return __ptr() != null;
    }

    ///
    pointer release() nothrow
    {
        pointer t = __ptr();
        __ptr() = null;
        return t;
    }

//    void swap(ref unique_ptr u) nothrow
//    {
//        __ptr_.swap(__u.__ptr_);
//    }

    version (CppRuntime_Microsoft)
    {
        ///
        ref inout(deleter_type) get_deleter() inout nothrow { return _Mypair._Myval1; }

    private:
        import core.stdcpp.xutility : _Compressed_pair;

        ref pointer __ptr() nothrow { return _Mypair._Myval2; }
        inout(pointer) __ptr() inout nothrow { return _Mypair._Myval2; }

        _Compressed_pair!(Deleter, pointer) _Mypair;
    }
    else version (CppRuntime_GNU)
    {
        ///
        ref inout(deleter_type) get_deleter() inout nothrow { return _M_t.get!1; }

    private:
        import core.stdcpp.tuple : tuple, get;

        ref pointer __ptr() nothrow { return _M_t.get!0; }
        inout(pointer) __ptr() inout nothrow { return _M_t.get!0; }

        tuple!(pointer, Deleter) _M_t;
    }
    else version (CppRuntime_LLVM)
    {
        ///
        ref inout(deleter_type) get_deleter() inout nothrow { return __ptr_.second; }

    private:
        import core.stdcpp.xutility : __compressed_pair;

        ref pointer __ptr() nothrow { return __ptr_.first; }
        inout(pointer) __ptr() inout nothrow { return __ptr_.first; }

        __compressed_pair!(pointer, deleter_type) __ptr_;
    }
}


private:

template ClassOrPtr(T)
{
    static if (is(T == class))
        alias ClassOrPtr = T;
    else
        alias ClassOrPtr = T*;
}
