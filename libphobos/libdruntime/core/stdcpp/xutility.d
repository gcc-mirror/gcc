/**
 * D header file for interaction with Microsoft C++ <xutility>
 *
 * Copyright: Copyright (c) 2018 D Language Foundation
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/xutility.d)
 */

module core.stdcpp.xutility;

@nogc:

version (CppRuntime_LLVM)
{
    import core.internal.traits : AliasSeq;
    enum StdNamespace = AliasSeq!("std", "__1");
}
else
{
    enum StdNamespace = "std";
}

/**
 * Possible values of the `__cplusplus` macro provided by C++ compilers
 *
 * For foolproofness, use ordering comparison, e.g. `__cplusplus >= CppStdRevision.cpp17`.
 */
enum CppStdRevision : uint
{
    cpp98 = 199711,
    cpp11 = 201103,
    cpp14 = 201402,
    cpp17 = 201703,
    cpp20 = 202002,
    cpp23 = 202302,
}

/**
 * Returns the target C++ version, encoded as C++ compilers do
 *
 * C++ compilers provide a `__cplusplus` macro which returns an integer
 * representing the targetted standard. This manifest provides the same
 * interface, retrieved from the compiler via a `__traits`.
 */
enum __cplusplus = __traits(getTargetInfo, "cppStd");

// wrangle C++ features
enum __cpp_sized_deallocation = __cplusplus >= CppStdRevision.cpp14 || is(typeof(_MSC_VER)) ? 201309 : 0;
enum __cpp_aligned_new = __cplusplus >= CppStdRevision.cpp17 ? 201606 : 0;


version (CppRuntime_Microsoft)
{
    import core.stdcpp.type_traits : is_empty;

    version (_MSC_VER_1200)
        enum _MSC_VER = 1200;
    else version (_MSC_VER_1300)
        enum _MSC_VER = 1300;
    else version (_MSC_VER_1310)
        enum _MSC_VER = 1310;
    else version (_MSC_VER_1400)
        enum _MSC_VER = 1400;
    else version (_MSC_VER_1500)
        enum _MSC_VER = 1500;
    else version (_MSC_VER_1600)
        enum _MSC_VER = 1600;
    else version (_MSC_VER_1700)
        enum _MSC_VER = 1700;
    else version (_MSC_VER_1800)
        enum _MSC_VER = 1800;
    else version (_MSC_VER_1900)
        enum _MSC_VER = 1900;
    else version (_MSC_VER_1910)
        enum _MSC_VER = 1910;
    else version (_MSC_VER_1911)
        enum _MSC_VER = 1911;
    else version (_MSC_VER_1912)
        enum _MSC_VER = 1912;
    else version (_MSC_VER_1913)
        enum _MSC_VER = 1913;
    else version (_MSC_VER_1914)
        enum _MSC_VER = 1914;
    else version (_MSC_VER_1915)
        enum _MSC_VER = 1915;
    else version (_MSC_VER_1916)
        enum _MSC_VER = 1916;
    else version (_MSC_VER_1920)
        enum _MSC_VER = 1920;
    else version (_MSC_VER_1921)
        enum _MSC_VER = 1921;
    else version (_MSC_VER_1922)
        enum _MSC_VER = 1922;
    else version (_MSC_VER_1923)
        enum _MSC_VER = 1923;
    else
        enum _MSC_VER = 1923; // assume most recent compiler version

    // Client code can mixin the set of MSVC linker directives
    mixin template MSVCLinkDirectives(bool failMismatch = false)
    {
        import core.stdcpp.xutility : __CXXLIB__, _ITERATOR_DEBUG_LEVEL;

        static if (__CXXLIB__ == "libcmtd")
        {
            pragma(lib, "libcpmtd");
            static if (failMismatch)
                pragma(linkerDirective, "/FAILIFMISMATCH:RuntimeLibrary=MTd_StaticDebug");
        }
        else static if (__CXXLIB__ == "msvcrtd")
        {
            pragma(lib, "msvcprtd");
            static if (failMismatch)
                pragma(linkerDirective, "/FAILIFMISMATCH:RuntimeLibrary=MDd_DynamicDebug");
        }
        else static if (__CXXLIB__ == "libcmt")
        {
            pragma(lib, "libcpmt");
            static if (failMismatch)
                pragma(linkerDirective, "/FAILIFMISMATCH:RuntimeLibrary=MT_StaticRelease");
        }
        else static if (__CXXLIB__ == "msvcrt")
        {
            pragma(lib, "msvcprt");
            static if (failMismatch)
                pragma(linkerDirective, "/FAILIFMISMATCH:RuntimeLibrary=MD_DynamicRelease");
        }
        static if (failMismatch)
            pragma(linkerDirective, "/FAILIFMISMATCH:_ITERATOR_DEBUG_LEVEL=" ~ ('0' + _ITERATOR_DEBUG_LEVEL));
    }

    // HACK: should we guess _DEBUG for `debug` builds?
    version (NDEBUG) {}
    else debug version = _DEBUG;

    // By specific user request
    version (_ITERATOR_DEBUG_LEVEL_0)
        enum _ITERATOR_DEBUG_LEVEL = 0;
    else version (_ITERATOR_DEBUG_LEVEL_1)
        enum _ITERATOR_DEBUG_LEVEL = 1;
    else version (_ITERATOR_DEBUG_LEVEL_2)
        enum _ITERATOR_DEBUG_LEVEL = 2;
    else
    {
        // Match the C Runtime
        static if (__CXXLIB__ == "libcmtd" || __CXXLIB__ == "msvcrtd")
            enum _ITERATOR_DEBUG_LEVEL = 2;
        else static if (__CXXLIB__ == "libcmt" || __CXXLIB__ == "msvcrt" ||
                        __CXXLIB__ == "msvcrt100" || __CXXLIB__ == "msvcrt110" || __CXXLIB__ == "msvcrt120")
            enum _ITERATOR_DEBUG_LEVEL = 0;
        else
        {
            static if (__CXXLIB__.length > 0)
                pragma(msg, "Unrecognised C++ runtime library '" ~ __CXXLIB__ ~ "'");

            // No runtime specified; as a best-guess, -release will produce code that matches the MSVC release CRT
            version (_DEBUG)
                enum _ITERATOR_DEBUG_LEVEL = 2;
            else
                enum _ITERATOR_DEBUG_LEVEL = 0;
        }
    }

    // convenient alias for the C++ std library name
    enum __CXXLIB__ = __traits(getTargetInfo, "cppRuntimeLibrary");

extern(C++, "std"):
package:
    enum _LOCK_DEBUG = 3;

    extern(C++, class) struct _Lockit
    {
        this(int) nothrow @nogc @safe;
        ~this() nothrow @nogc @safe;

    private:
        int _Locktype;
    }
    void dummyDtor() { assert(false); }
    pragma(linkerDirective, "/ALTERNATENAME:" ~ _Lockit.__dtor.mangleof ~ "=" ~ dummyDtor.mangleof);

    struct _Container_base0
    {
    extern(D):
        void _Orphan_all()() nothrow @nogc @safe {}
        void _Swap_all()(ref _Container_base0) nothrow @nogc @safe {}
        void _Swap_proxy_and_iterators()(ref _Container_base0) nothrow {}
    }
    struct _Iterator_base0
    {
    extern(D):
        void _Adopt()(const(void)*) nothrow @nogc @safe {}
        const(_Container_base0)* _Getcont()() const nothrow @nogc @safe { return null; }

        enum bool _Unwrap_when_unverified = true;
    }

    struct _Container_proxy
    {
        const(_Container_base12)* _Mycont;
        _Iterator_base12* _Myfirstiter;
    }

    struct _Container_base12
    {
    extern(D):
        inout(_Iterator_base12*)*_Getpfirst()() inout nothrow @nogc @safe
        {
            return _Myproxy == null ? null : &_Myproxy._Myfirstiter;
        }
        void _Orphan_all()() nothrow @nogc @safe
        {
            static if (_ITERATOR_DEBUG_LEVEL == 2)
            {
                if (_Myproxy != null)
                {
                    auto _Lock = _Lockit(_LOCK_DEBUG);
                    for (_Iterator_base12 **_Pnext = &_Myproxy._Myfirstiter; *_Pnext != null; *_Pnext = (*_Pnext)._Mynextiter)
                        (*_Pnext)._Myproxy = null;
                    _Myproxy._Myfirstiter = null;
                }
            }
        }
//        void _Swap_all()(ref _Container_base12) nothrow @nogc;

        void _Swap_proxy_and_iterators()(ref _Container_base12 _Right) nothrow
        {
            static if (_ITERATOR_DEBUG_LEVEL == 2)
                auto _Lock = _Lockit(_LOCK_DEBUG);

            _Container_proxy* _Temp = _Myproxy;
            _Myproxy = _Right._Myproxy;
            _Right._Myproxy = _Temp;

            if (_Myproxy)
                _Myproxy._Mycont = &this;

            if (_Right._Myproxy)
                _Right._Myproxy._Mycont = &_Right;
        }

        _Container_proxy* _Myproxy;
    }

    struct _Iterator_base12
    {
    extern(D):
        void _Adopt()(_Container_base12 *_Parent) nothrow @nogc @safe
        {
            if (_Parent == null)
            {
                static if (_ITERATOR_DEBUG_LEVEL == 2)
                {
                    auto _Lock = _Lockit(_LOCK_DEBUG);
                    _Orphan_me();
                }
            }
            else
            {
                _Container_proxy *_Parent_proxy = _Parent._Myproxy;

                static if (_ITERATOR_DEBUG_LEVEL == 2)
                {
                    if (_Myproxy != _Parent_proxy)
                    {
                        auto _Lock = _Lockit(_LOCK_DEBUG);
                        _Orphan_me();
                        _Mynextiter = _Parent_proxy._Myfirstiter;
                        _Parent_proxy._Myfirstiter = &this;
                        _Myproxy = _Parent_proxy;
                    }
                }
                else
                    _Myproxy = _Parent_proxy;
            }
        }
        void _Clrcont()() nothrow @nogc @safe
        {
            _Myproxy = null;
        }
        const(_Container_base12)* _Getcont()() const nothrow @nogc @safe
        {
            return _Myproxy == null ? null : _Myproxy._Mycont;
        }
        inout(_Iterator_base12*)*_Getpnext()() inout nothrow @nogc @safe
        {
            return &_Mynextiter;
        }
        void _Orphan_me()() nothrow @nogc @safe
        {
            static if (_ITERATOR_DEBUG_LEVEL == 2)
            {
                if (_Myproxy != null)
                {
                    _Iterator_base12 **_Pnext = &_Myproxy._Myfirstiter;
                    while (*_Pnext != null && *_Pnext != &this)
                        _Pnext = &(*_Pnext)._Mynextiter;
                    assert(*_Pnext, "ITERATOR LIST CORRUPTED!");
                    *_Pnext = _Mynextiter;
                    _Myproxy = null;
                }
            }
        }

        enum bool _Unwrap_when_unverified = _ITERATOR_DEBUG_LEVEL == 0;

        _Container_proxy *_Myproxy;
        _Iterator_base12 *_Mynextiter;
    }

    static if (_ITERATOR_DEBUG_LEVEL == 0)
    {
        alias _Container_base = _Container_base0;
        alias _Iterator_base = _Iterator_base0;
    }
    else
    {
        alias _Container_base = _Container_base12;
        alias _Iterator_base = _Iterator_base12;
    }

    extern (C++, class) struct _Compressed_pair(_Ty1, _Ty2, bool Ty1Empty = is_empty!_Ty1.value)
    {
    pragma (inline, true):
    extern(D):
    pure nothrow @nogc:
        enum _HasFirst = !Ty1Empty;

        ref inout(_Ty1) first() inout @safe { return _Myval1; }
        ref inout(_Ty2) second() inout @safe { return _Myval2; }

        static if (!Ty1Empty)
            _Ty1 _Myval1;
        else
        {
            @property ref inout(_Ty1) _Myval1() inout @trusted { return *_GetBase(); }
            private inout(_Ty1)* _GetBase() inout @trusted { return cast(inout(_Ty1)*)&this; }
        }
        _Ty2 _Myval2;
    }

    // these are all [[noreturn]]
    void _Xbad_alloc() nothrow;
    void _Xinvalid_argument(const(char)* message) nothrow;
    void _Xlength_error(const(char)* message) nothrow;
    void _Xout_of_range(const(char)* message) nothrow;
    void _Xoverflow_error(const(char)* message) nothrow;
    void _Xruntime_error(const(char)* message) nothrow;
}
else version (CppRuntime_LLVM)
{
    import core.stdcpp.type_traits : is_empty;

extern(C++, "std"):

    extern (C++, class) struct __compressed_pair(_T1, _T2)
    {
    pragma (inline, true):
    extern(D):
        enum Ty1Empty = is_empty!_T1.value;
        enum Ty2Empty = is_empty!_T2.value;

        ref inout(_T1) first() inout nothrow @safe @nogc { return __value1_; }
        ref inout(_T2) second() inout nothrow @safe @nogc { return __value2_; }

    private:
        private inout(_T1)* __get_base1() inout { return cast(inout(_T1)*)&this; }
        private inout(_T2)* __get_base2() inout { return cast(inout(_T2)*)&__get_base1()[Ty1Empty ? 0 : 1]; }

        static if (!Ty1Empty)
            _T1 __value1_;
        else
            @property ref inout(_T1) __value1_() inout nothrow @trusted @nogc { return *__get_base1(); }
        static if (!Ty2Empty)
            _T2 __value2_;
        else
            @property ref inout(_T2) __value2_() inout nothrow @trusted @nogc { return *__get_base2(); }
    }
}
version (CppRuntime_GNU)
{
    import core.atomic;

    alias _Atomic_word = int;

    void __atomic_add_dispatch()(_Atomic_word* __mem, int __val) nothrow @nogc @safe
    {
        version (__GTHREADS)
        {
            // TODO: check __gthread_active_p()
//            if (__gthread_active_p())
                __atomic_add(__mem, __val);
//            }
//            else
//            __atomic_add_single(__mem, __val);
        }
        else
            __atomic_add_single(__mem, __val);
    }

    void __atomic_add()(_Atomic_word* __mem, int __val) nothrow @nogc @safe
    {
        atomicFetchAdd!(MemoryOrder.acq_rel)(*__mem, __val);
    }

    void __atomic_add_single()(_Atomic_word* __mem, int __val) nothrow @nogc @safe
    {
        *__mem += __val;
    }

    _Atomic_word __exchange_and_add_dispatch()(_Atomic_word* __mem, int __val) nothrow @nogc @safe
    {
        version (__GTHREADS)
        {
            // TODO: check __gthread_active_p()
            return __exchange_and_add(__mem, __val);

//            if (__gthread_active_p())
//                return __exchange_and_add(__mem, __val);
//            else
//                return __exchange_and_add_single(__mem, __val);
        }
        else
            return __exchange_and_add_single(__mem, __val);
    }

    _Atomic_word __exchange_and_add()(_Atomic_word* __mem, int __val) nothrow @nogc @safe
    {
        return atomicFetchAdd!(MemoryOrder.acq_rel)(*__mem, __val);
    }

    _Atomic_word __exchange_and_add_single()(_Atomic_word* __mem, int __val) nothrow @nogc @safe
    {
        _Atomic_word __result = *__mem;
        *__mem += __val;
        return __result;
    }
}
