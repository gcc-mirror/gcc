/**
 * D header file for interaction with C++ std::string.
 *
 * Copyright: Copyright (c) 2019 D Language Foundation
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Guillaume Chatelet
 *            Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/string.d)
 */

module core.stdcpp.string;

import core.stdcpp.allocator;
import core.stdcpp.xutility : StdNamespace;
import core.stdc.stddef : wchar_t;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin)
{
    // Apple decided to rock a different ABI... good for them!
    version = _LIBCPP_ABI_ALTERNATE_STRING_LAYOUT;
}

version (CppRuntime_Gcc)
{
    version (_GLIBCXX_USE_CXX98_ABI)
    {
        private enum StringNamespace = "std";
        version = __GTHREADS;
    }
    else
    {
        import core.internal.traits : AliasSeq;
        private enum StringNamespace = AliasSeq!("std", "__cxx11");
    }
}
else
    alias StringNamespace = StdNamespace;

enum DefaultConstruct { value }

/// Constructor argument for default construction
enum Default = DefaultConstruct();

@nogc:

/**
 * Character traits classes specify character properties and provide specific
 * semantics for certain operations on characters and sequences of characters.
 */
extern(C++, (StdNamespace)) struct char_traits(CharT)
{
    alias char_type = CharT;

    static size_t length(const(char_type)* s) @trusted pure nothrow @nogc
    {
        static if (is(char_type == char) || is(char_type == ubyte))
        {
            import core.stdc.string : strlen;
            return strlen(s);
        }
        else
        {
            size_t len = 0;
            for (; *s != char_type(0); ++s)
                ++len;
            return len;
        }
    }

    static char_type* move(char_type* s1, const char_type* s2, size_t n) @trusted pure nothrow @nogc
    {
        import core.stdc.string : memmove;
        import core.stdc.wchar_ : wmemmove;
        import core.stdc.stddef : wchar_t;

        if (n == 0)
            return s1;

        version (CRuntime_Microsoft)
        {
            enum crt = __traits(getTargetInfo, "cppRuntimeLibrary");
            static if (crt.length >= 6 && crt[0 .. 6] == "msvcrt")
                enum use_wmemmove = false; // https://issues.dlang.org/show_bug.cgi?id=20456
            else
                enum use_wmemmove = true;
        }
        else
            enum use_wmemmove = true;

        static if (use_wmemmove
                && (is(char_type == wchar_t)
                    || is(char_type == ushort) && wchar_t.sizeof == ushort.sizeof // Windows
                    || is(char_type == uint) && wchar_t.sizeof == uint.sizeof)) // POSIX
            return cast(char_type*) wmemmove(s1, s2, n);
        else
            return cast(char_type*) memmove(s1, s2, n * char_type.sizeof);
    }
}

// I don't think we can have these here, otherwise symbols are emit to druntime, and we don't want that...
//alias std_string = basic_string!char;
//alias std_u16string = basic_string!wchar; // TODO: can't mangle these yet either...
//alias std_u32string = basic_string!dchar;
//alias std_wstring = basic_string!wchar_t; // TODO: we can't mangle wchar_t properly (yet?)

/**
 * D language counterpart to C++ std::basic_string.
 *
 * C++ reference: $(LINK2 https://en.cppreference.com/w/cpp/string/basic_string)
 */
extern(C++, class)
extern(C++, (StringNamespace))
struct basic_string(T, Traits = char_traits!T, Alloc = allocator!T)
{
extern(D):
@nogc:

    ///
    enum size_type npos = size_type.max;

    ///
    alias size_type = size_t;
    ///
    alias difference_type = ptrdiff_t;
    ///
    alias value_type = T;
    ///
    alias traits_type = Traits;
    ///
    alias allocator_type = Alloc;
    ///
    alias pointer = value_type*;
    ///
    alias const_pointer = const(value_type)*;

    ///
    alias toString = as_array;

    /// MSVC allocates on default initialisation in debug, which can't be modelled by D `struct`
    @disable this();

    ///
    alias length = size;
    ///
    alias opDollar = length;
    ///
    bool empty() const nothrow @trusted                                     { return size() == 0; }

    ///
    size_t[2] opSlice(size_t dim : 0)(size_t start, size_t end) const pure nothrow @safe @nogc { return [start, end]; }

    ///
    ref inout(T) opIndex(size_t index) inout pure nothrow @safe @nogc       { return as_array[index]; }
    ///
    inout(T)[] opIndex(size_t[2] slice) inout pure nothrow @safe @nogc      { return as_array[slice[0] .. slice[1]]; }
    ///
    inout(T)[] opIndex() inout pure nothrow @safe @nogc                     { return as_array(); }

    /// Two `basic_string`s are equal if they represent the same sequence of code units.
    bool opEquals(scope const ref basic_string s) const pure nothrow @safe  { return as_array == s.as_array; }
    /// ditto
    bool opEquals(scope const T[] s) const pure nothrow @safe               { return as_array == s; }

    /// Performs lexicographical comparison.
    int opCmp(scope const ref basic_string rhs) const pure nothrow @safe    { return __cmp(as_array, rhs.as_array); }
    /// ditto
    int opCmp(scope const T[] rhs) const pure nothrow @safe                 { return __cmp(as_array, rhs); }

    /// Hash to allow `basic_string`s to be used as keys for built-in associative arrays.
    /// **The result will generally not be the same as C++ `std::hash<std::basic_string<T>>`.**
    size_t toHash() const @nogc nothrow pure @safe                          { return .hashOf(as_array); }

    ///
    void clear()                                                            { eos(0); } // TODO: bounds-check
    ///
    void resize(size_type n, T c = T(0)) @trusted
    {
        if (n <= size())
            eos(n);
        else
            append(n - size(), c);
    }

    ///
    ref inout(T) front() inout nothrow @safe                                { return this[0]; }
    ///
    ref inout(T) back() inout nothrow @safe                                 { return this[$-1]; }

    ///
    const(T)* c_str() const nothrow @safe                                   { return data(); }

    // Modifiers
    ///
    ref basic_string opAssign()(auto ref basic_string str)                  { return assign(str); }
//    ref basic_string assign(size_type n, T c);
    ///
    ref basic_string opAssign(const(T)[] str)                               { return assign(str); }
    ///
    ref basic_string opAssign(T c)                                          { return assign((&c)[0 .. 1]); }

    ///
    ref basic_string opIndexAssign(T c, size_t index)                       { as_array[index] = c; return this; }
    ///
    ref basic_string opIndexAssign(T c, size_t[2] slice)                    { as_array[slice[0] .. slice[1]] = c; return this; }
    ///
    ref basic_string opIndexAssign(const(T)[] str, size_t[2] slice)         { as_array[slice[0] .. slice[1]] = str[]; return this; }
    ///
    ref basic_string opIndexAssign(T c)                                     { as_array[] = c; return this; }
    ///
    ref basic_string opIndexAssign(const(T)[] str)                          { as_array[] = str[]; return this; }

    ///
    ref basic_string opIndexOpAssign(string op)(T c, size_t index)          { mixin("as_array[index] " ~ op ~ "= c;"); return this; }
    ///
    ref basic_string opIndexOpAssign(string op)(T c, size_t[2] slice)       { mixin("as_array[slice[0] .. slice[1]] " ~ op ~ "= c;"); return this; }
    ///
    ref basic_string opIndexOpAssign(string op)(const(T)[] str, size_t[2] slice)    { mixin("as_array[slice[0] .. slice[1]] " ~ op ~ "= str[];"); return this; }
    ///
    ref basic_string opIndexOpAssign(string op)(T c)                        { mixin("as_array[] " ~ op ~ "= c;"); return this; }
    ///
    ref basic_string opIndexOpAssign(string op)(const(T)[] str)             { mixin("as_array[] " ~ op ~ "= str[];"); return this; }
    ///
    ref basic_string append(T c)                                            { return append((&c)[0 .. 1]); }
    ///
    ref basic_string opOpAssign(string op : "~")(const(T)[] str)            { return append(str); }
    ///
    ref basic_string opOpAssign(string op : "~")(T c)                       { return append((&c)[0 .. 1]); }

    ///
    ref basic_string insert(size_type pos, ref const(basic_string) str)     { return insert(pos, str.data(), str.size()); }
    ///
    ref basic_string insert(size_type pos, ref const(basic_string) str, size_type subpos, size_type sublen) @trusted
    {
        const _strsz = str.size();
        assert(subpos <= _strsz);
//        if (subpos > _strsz)
//            throw new RangeError("subpos exceeds length of str");
        return insert(pos, str.data() + subpos, min(sublen, _strsz - subpos));
    }
    ///
    ref basic_string insert(S : size_type)(S pos, const(T)* s)
    {
        // This overload is declared as a template to give precedence to the slice overload const(T)[] in case of conflict.
        assert(s);
        return insert(pos, s, traits_type.length(s));
    }
    ///
    ref basic_string insert(size_type pos, const(T)[] s)                    { insert(pos, &s[0], s.length); return this; }

    ///
    ref basic_string erase(size_type pos = 0) // TODO: bounds-check
    {
//        _My_data._Check_offset(pos);
        eos(pos);
        return this;
    }
    ///
    ref basic_string erase(size_type pos, size_type len) // TODO: bounds-check
    {
//        _My_data._Check_offset(pos);
        T[] str = as_array();
        size_type new_len = str.length - len;
        this[pos .. new_len] = this[pos + len .. str.length]; // TODO: should be memmove!
        eos(new_len);
        return this;
    }

    ///
    ref basic_string replace()(size_type pos, size_type len, auto ref basic_string str)     { return replace(pos, len, str.data(), str.size()); }
    ///
    ref basic_string replace()(size_type pos, size_type len, auto ref basic_string str,
                            size_type subpos, size_type sublen=npos)
    {
        size_type strsz = str.size();
        assert(subpos <= strsz);
//        if (subpos > strsz)
//            throw new RangeError("subpos exceeds size of str");
        return replace(pos, len, str.data() + subpos, min(sublen, strsz - subpos));
    }
    ///
    ref basic_string replace(size_type pos, size_type len, const(value_type)[] s)           { return replace(pos, len, s.ptr, s.length); }
    ///
    ref basic_string replace(S : size_type)(S pos, size_type len, const(value_type)* s)
    {
        // This overload is declared as a template to give precedence to the slice overload const(T)[] in case of conflict.
        assert(s !is null, "string::replace received null");
        return replace(pos, len, s, traits_type.length(s));
    }

    ///
    void push_back(T c) @trusted                                            { append((&c)[0 .. 1]); }
    ///
    void pop_back()                                                         { erase(size() - 1); }

    version (CppRuntime_Microsoft)
    {
        //----------------------------------------------------------------------------------
        // Microsoft runtime
        //----------------------------------------------------------------------------------

        ///
        this(DefaultConstruct)                                              { _Alloc_proxy(); _Tidy_init(); }
        ///
        this(const(T)[] str)                                                { _Alloc_proxy(); _Tidy_init(); assign(str); }
        ///
        this(const(T)[] str, ref const(allocator_type) al)                  { _Alloc_proxy(); _AssignAllocator(al); _Tidy_init(); assign(str); }
        ///
        this(this)
        {
            _Alloc_proxy();
            if (_Get_data()._IsAllocated())
            {
                T[] _Str = _Get_data()._Mystr;
                _Tidy_init();
                assign(_Str);
            }
        }

        ///
        ~this()                                                             { _Tidy_deallocate(); }

        ///
        ref inout(Alloc) get_allocator() inout                              { return _Getal(); }

        ///
        size_type max_size() const nothrow @safe                            { return ((size_t.max / T.sizeof) - 1) / 2; } // HACK: clone the windows version precisely?

        ///
        size_type size() const nothrow @safe                                { return _Get_data()._Mysize; }
        ///
        size_type capacity() const nothrow @safe                            { return _Get_data()._Myres; }
        ///
        inout(T)* data() inout @safe                                        { return _Get_data()._Myptr; }
        ///
        inout(T)[] as_array() scope return inout nothrow @trusted           { return _Get_data()._Myptr[0 .. _Get_data()._Mysize]; }
        ///
        ref inout(T) at(size_type i) inout nothrow @trusted                 { return _Get_data()._Myptr[0 .. _Get_data()._Mysize][i]; }

        ///
        ref basic_string assign(const(T)[] str)
        {
            size_type _Count = str.length;
            auto _My_data = &_Get_data();
            if (_Count <= _My_data._Myres)
            {
                T* _Old_ptr = _My_data._Myptr;
                _My_data._Mysize = _Count;
                _Old_ptr[0 .. _Count] = str[]; // TODO: this needs to be a memmove(), does that work here?
                _Old_ptr[_Count] = T(0);
                return this;
            }
            return _Reallocate_for(_Count, (T* _New_ptr, size_type _Count, const(T)* _Ptr) nothrow {
                _New_ptr[0 .. _Count] = _Ptr[0 .. _Count];
                _New_ptr[_Count] = T(0);
            }, str.ptr);
        }

        ///
        ref basic_string assign(const ref basic_string str)
        {
            if (&this != &str)
                assign(str.as_array);
            return this;
        }

        ///
        ref basic_string append(const(T)[] str)
        {
            size_type _Count = str.length;
            auto _My_data = &_Get_data();
            size_type _Old_size = _My_data._Mysize;
            if (_Count <= _My_data._Myres - _Old_size)
            {
                pointer _Old_ptr = _My_data._Myptr;
                _My_data._Mysize = _Old_size + _Count;
                _Old_ptr[_Old_size .. _Old_size + _Count] = str[]; // TODO: this needs to be a memmove(), does that work here?
                _Old_ptr[_Old_size + _Count] = T(0);
                return this;
            }
            return _Reallocate_grow_by(_Count, (T* _New_ptr, const(T)[] _Old_str, const(T)[] _Str) {
                _New_ptr[0 .. _Old_str.length] = _Old_str[];
                _New_ptr[_Old_str.length .. _Old_str.length + _Str.length] = _Str[];
                _New_ptr[_Old_str.length + _Str.length] = T(0);
            }, str);
        }

        ///
        ref basic_string append(size_type n, T c)
        {
            alias _Count = n;
            alias _Ch = c;
            auto _My_data = &_Get_data();
            const size_type _Old_size = _My_data._Mysize;
            if (_Count <= _My_data._Myres - _Old_size)
            {
                _My_data._Mysize = _Old_size + _Count;
                pointer _Old_ptr = _My_data._Myptr();
                _Old_ptr[_Old_size .. _Old_size + _Count] = _Ch;
                _Old_ptr[_Old_size + _Count] = T(0);
                return this;
            }

            return _Reallocate_grow_by(_Count, (T* _New_ptr, const(T)[] _Old_str, size_type _Count, T _Ch) {
                _New_ptr[0 .. _Old_str.length] = _Old_str[];
                _New_ptr[_Old_str.length .. _Old_str.length + _Count] = _Ch;
                _New_ptr[_Old_str.length + _Count] = T(0);
            }, _Count, _Ch);
        }

        ///
        void reserve(size_type _Newcap = 0)
        {
            // determine new minimum length of allocated storage

            auto _My_data = &_Get_data();

            if (_My_data._Mysize > _Newcap)
            {
                // requested capacity is not large enough for current size, ignore
                return; // nothing to do
            }

            if (_My_data._Myres == _Newcap)
            {
                // we're already at the requested capacity
                return; // nothing to do
            }

            if (_My_data._Myres < _Newcap)
            {
                // reallocate to grow
                const size_type _Old_size = _My_data._Mysize;
                _Reallocate_grow_by(
                    _Newcap - _Old_size, (T* _New_ptr, const(T)[] _Old_str) {
                        _New_ptr[0 .. _Old_str.length] = _Old_str[];
                        _New_ptr[_Old_str.length] = _Old_str.ptr[_Old_str.length];
                    });

                _My_data._Mysize = _Old_size;
                return;
            }

            if (_My_data._BUF_SIZE > _Newcap && _My_data._Large_string_engaged())
            {
                // deallocate everything; switch back to "small" mode
                _Become_small();
                return;
            }

            // ignore requests to reserve to [_BUF_SIZE, _Myres)
        }

        ///
        void shrink_to_fit()
        {
            // reduce capacity

            auto _My_data = &_Get_data();
            if (!_My_data._Large_string_engaged())
            {
                // can't shrink from small mode
                return;
            }

            if (_My_data._Mysize < _My_data._BUF_SIZE)
            {
                _Become_small();
                return;
            }

            const size_type _Target_capacity = min(_My_data._Mysize | _My_data._ALLOC_MASK, max_size());
            if (_Target_capacity < _My_data._Myres)
            {
                // worth shrinking, do it
                auto _Al = &_Getal();
                pointer _New_ptr = _Al.allocate(_Target_capacity + 1); // throws
                _Base._Orphan_all();
                _New_ptr[0 .. _My_data._Mysize + 1] = _My_data._Bx._Ptr[0 .. _My_data._Mysize + 1];
                _Al.deallocate(_My_data._Bx._Ptr, _My_data._Myres + 1);
                _My_data._Bx._Ptr = _New_ptr;
                _My_data._Myres = _Target_capacity;
            }
        }

        ///
        ref basic_string insert(size_type pos, const(T)* s, size_type n)
        {
            // insert [_Ptr, _Ptr + _Count) at _Off
            alias _Off = pos;
            alias _Ptr = s;
            alias _Count = n;
            auto _My_data = &_Get_data();
//            _My_data._Check_offset(_Off);
            const size_type _Old_size = _My_data._Mysize;
            if (_Count <= _My_data._Myres - _Old_size)
            {
                _My_data._Mysize = _Old_size + _Count;
                T* _Old_ptr = _My_data._Myptr();
                T* _Insert_at = _Old_ptr + _Off;
                // the range [_Ptr, _Ptr + _Ptr_shifted_after) is left alone by moving the suffix out,
                // while the range [_Ptr + _Ptr_shifted_after, _Ptr + _Count) shifts down by _Count
                size_type _Ptr_shifted_after;
                if (_Ptr + _Count <= _Insert_at || _Ptr > _Old_ptr + _Old_size)
                {
                    // inserted content is before the shifted region, or does not alias
                    _Ptr_shifted_after = _Count; // none of _Ptr's data shifts
                }
                else if (_Insert_at <= _Ptr)
                {
                    // all of [_Ptr, _Ptr + _Count) shifts
                    _Ptr_shifted_after = 0;
                }
                else
                {
                    // [_Ptr, _Ptr + _Count) contains _Insert_at, so only the part after _Insert_at shifts
                    _Ptr_shifted_after = cast(size_type)(_Insert_at - _Ptr);
                }

                _Traits.move(_Insert_at + _Count, _Insert_at, _Old_size - _Off + 1); // move suffix + null down
                _Insert_at[0 .. _Ptr_shifted_after] = _Ptr[0 .. _Ptr_shifted_after];
                (_Insert_at + _Ptr_shifted_after)[0 .. _Count - _Ptr_shifted_after] = (_Ptr + _Count + _Ptr_shifted_after)[0 .. _Count - _Ptr_shifted_after];
                return this;
            }

            return _Reallocate_grow_by(
                _Count,
                (T* _New_ptr, const(T)[] _Old_str, size_type _Off, const(T)* _Ptr, size_type _Count) {
                    _New_ptr[0 .. _Off] = _Old_str[0 .. _Off];
                    _New_ptr[_Off .. _Off + _Count] = _Ptr[0 .. _Count];
                    _New_ptr[_Off + _Count .. _Old_str.length + _Count + 1] = _Old_str.ptr[_Off .. _Old_str.length + 1];
                },
                _Off, _Ptr, _Count);
        }

        ///
        ref basic_string insert(size_type pos, size_type n, T c)
        {
            // insert _Count * _Ch at _Off
            alias _Off = pos;
            alias _Count = n;
            alias _Ch = c;
            auto _My_data = &_Get_data();
//            _My_data._Check_offset(_Off);
            const size_type _Old_size = _My_data._Mysize;
            if (_Count <= _My_data._Myres - _Old_size)
            {
                _My_data._Mysize = _Old_size + _Count;
                T* _Old_ptr = _My_data._Myptr();
                T* _Insert_at = _Old_ptr + _Off;
                _Traits.move(_Insert_at + _Count, _Insert_at, _Old_size - _Off + 1); // move suffix + null down
                _Insert_at[0 .. _Count] = _Ch; // fill hole
                return this;
            }

            return _Reallocate_grow_by(
                _Count,
                (T* _New_ptr, const(T)[] _Old_str, size_type _Off, size_type _Count, T _Ch)
                {
                    _New_ptr[0 .. _Off] = _Old_str[0 .. _Off];
                    _New_ptr[_Off .. _Off + _Count] = _Ch;
                    _New_ptr[_Off + _Count .. _Old_str.length + 1] = _Old_str.ptr[_Off .. _Old_str.length + 1];
                },
                _Off, _Count, _Ch);
        }

        ///
        ref basic_string replace(size_type pos, size_type len, const(T)* s, size_type slen)
        {
            // replace [_Off, _Off + _N0) with [_Ptr, _Ptr + _Count)
            alias _Off = pos;
            alias _N0 = len;
            alias _Ptr = s;
            alias _Count = slen;
            auto _My_data = &_Get_data();
//            _Mypair._Myval2._Check_offset(_Off);
            _N0 = _My_data._Clamp_suffix_size(_Off, _N0);
            if (_N0 == _Count)
            {
                // size doesn't change, so a single move does the trick
                _Traits.move(_My_data._Myptr() + _Off, _Ptr, _Count);
                return this;
            }

            const size_type _Old_size = _My_data._Mysize;
            const size_type _Suffix_size = _Old_size - _N0 - _Off + 1;
            if (_Count < _N0)
            {
                // suffix shifts backwards; we don't have to move anything out of the way
                _My_data._Mysize = _Old_size - (_N0 - _Count);
                T* _Old_ptr = _My_data._Myptr();
                T* _Insert_at = _Old_ptr + _Off;
                _Traits.move(_Insert_at, _Ptr, _Count);
                _Traits.move(_Insert_at + _Count, _Insert_at + _N0, _Suffix_size);
                return this;
            }

            const size_type _Growth = cast(size_type)(_Count - _N0);
            if (_Growth <= _My_data._Myres - _Old_size)
            {
                // growth fits
                _My_data._Mysize = _Old_size + _Growth;
                T* _Old_ptr = _My_data._Myptr();
                T* _Insert_at = _Old_ptr + _Off;
                T* _Suffix_at = _Insert_at + _N0;

                size_type _Ptr_shifted_after; // see rationale in insert
                if (_Ptr + _Count <= _Insert_at || _Ptr > _Old_ptr + _Old_size)
                    _Ptr_shifted_after = _Count;
                else if (_Suffix_at <= _Ptr)
                    _Ptr_shifted_after = 0;
                else
                    _Ptr_shifted_after = cast(size_type)(_Suffix_at - _Ptr);

                _Traits.move(_Suffix_at + _Growth, _Suffix_at, _Suffix_size);
                // next case must be move, in case _Ptr begins before _Insert_at and contains part of the hole;
                // this case doesn't occur in insert because the new content must come from outside the removed
                // content there (because in insert there is no removed content)
                _Traits.move(_Insert_at, _Ptr, _Ptr_shifted_after);
                // the next case can be copy, because it comes from the chunk moved out of the way in the
                // first move, and the hole we're filling can't alias the chunk we moved out of the way
                _Insert_at[_Ptr_shifted_after .. _Count] = _Ptr[_Growth + _Ptr_shifted_after .. _Growth + _Count];
                return this;
            }

            return _Reallocate_grow_by(
                _Growth,
                (T* _New_ptr, const(T)[] _Old_str, size_type _Off, size_type _N0, const(T)* _Ptr, size_type _Count) {
                    _New_ptr[0 .. _Off] = _Old_str[0 .. _Off];
                    _New_ptr[_Off .. _Count] = _Ptr[0 .. _Count];
                    const __n = _Old_str.length - _N0 - _Off + 1;
                    (_New_ptr + _Off + _Count)[0 .. __n] = (_Old_str.ptr + _Off + _N0)[0 .. __n];
                },
                _Off, _N0, _Ptr, _Count);
        }

        ///
        ref basic_string replace(size_type _Off, size_type _N0, size_type _Count, T _Ch)
        {
            // replace [_Off, _Off + _N0) with _Count * _Ch
            auto _My_data = &_Get_data();
//            _My_data._Check_offset(_Off);
            _N0 = _My_data._Clamp_suffix_size(_Off, _N0);
            if (_Count == _N0)
            {
                _My_data._Myptr()[_Off .. _Off + _Count] = _Ch;
                return this;
            }

            const size_type _Old_size = _My_data._Mysize;
            if (_Count < _N0 || _Count - _N0 <= _My_data._Myres - _Old_size)
            {
                // either we are shrinking, or the growth fits
                _My_data._Mysize = _Old_size + _Count - _N0; // may temporarily overflow;
                                                                    // OK because size_type must be unsigned
                T* _Old_ptr = _My_data._Myptr();
                T* _Insert_at = _Old_ptr + _Off;
                _Traits.move(_Insert_at + _Count, _Insert_at + _N0, _Old_size - _N0 - _Off + 1);
                _Insert_at[0 .. _Count] = _Ch;
                return this;
            }

            return _Reallocate_grow_by(
                _Count - _N0,
                (T* _New_ptr, const(T)[] _Old_str, size_type _Off, size_type _N0, size_type _Count, T _Ch) {
                    _New_ptr[0 .. _Off] = _Old_str[0 .. _Off];
                    _New_ptr[_Off .. _Off + _Count] = _Ch;
                    const __n = _Old_str.length - _N0 - _Off + 1;
                    (_New_ptr + _Off + _Count)[0 .. __n] = (_Old_str.ptr + _Off + _N0)[0 .. __n];
                },
                _Off, _N0, _Count, _Ch);
        }

        ///
        void swap(ref basic_string _Right)
        {
            import core.internal.lifetime : swap;
            import core.stdcpp.type_traits : is_empty;

            if (&this != &_Right)
            {
                static if (!is_empty!allocator_type.value
                        && allocator_traits!allocator_type.propagate_on_container_swap)
                {
                    swap(_Getal(), _Right._Getal());
                }

                static if (_ITERATOR_DEBUG_LEVEL != 0)
                {
                    auto _My_data = &_Get_data();
                    const bool _My_large = _My_data._Large_string_engaged();
                    const bool _Right_large = _Right._Get_data()._Large_string_engaged();
                    if (!_My_large)
                        _Base._Orphan_all();

                    if (!_Right_large)
                        _Right._Base._Orphan_all();

                    if (_My_large || _Right_large)
                        _My_data._Base._Swap_proxy_and_iterators(_Right._Get_data()._Base);
                } // _ITERATOR_DEBUG_LEVEL != 0
            }

            _Swap_data!_Can_memcpy_val(_Right);
        }

    private:
        import core.stdcpp.xutility : MSVCLinkDirectives;
        import core.stdcpp.xutility : _Container_base;

        alias _Traits = traits_type;
        alias _Scary_val = _String_val!T;

        enum bool _Can_memcpy_val = is(_Traits == char_traits!E, E) && is(pointer == U*, U);
        // This offset skips over the _Container_base members, if any
        enum size_t _Memcpy_val_offset = _Size_after_ebco_v!_Container_base;
        enum size_t _Memcpy_val_size   = _Scary_val.sizeof - _Memcpy_val_offset;

        // Make sure the object files wont link against mismatching objects
        mixin MSVCLinkDirectives!true;

        pragma (inline, true)
        {
            void eos(size_type offset) nothrow                              { _Get_data()._Myptr[_Get_data()._Mysize = offset] = T(0); }

            ref inout(_Base.Alloc) _Getal() inout nothrow @safe             { return _Base._Mypair._Myval1; }
            ref inout(_Base.ValTy) _Get_data() inout nothrow @safe          { return _Base._Mypair._Myval2; }
        }

        void _Alloc_proxy() nothrow
        {
            static if (_ITERATOR_DEBUG_LEVEL > 0)
                _Base._Alloc_proxy();
        }

        void _AssignAllocator(ref const(allocator_type) al) nothrow
        {
            static if (_Base._Mypair._HasFirst)
                _Getal() = al;
        }

        void _Become_small()
        {
            // release any held storage and return to small string mode
            // pre: *this is in large string mode
            // pre: this is small enough to return to small string mode
            auto _My_data = &_Get_data();
            _Base._Orphan_all();
            pointer _Ptr = _My_data._Bx._Ptr;
            auto _Al = &_Getal();
            _My_data._Bx._Buf[0 .. _My_data._Mysize + 1] = _Ptr[0 .. _My_data._Mysize + 1];
            _Al.deallocate(_Ptr, _My_data._Myres + 1);
            _My_data._Myres = _My_data._BUF_SIZE - 1;
        }

        void _Tidy_init() nothrow
        {
            auto _My_data = &_Get_data();
            _My_data._Mysize = 0;
            _My_data._Myres = _My_data._BUF_SIZE - 1;
            _My_data._Bx._Buf[0] = T(0);
        }

        size_type _Calculate_growth(size_type _Requested) const nothrow
        {
            auto _My_data = &_Get_data();
            size_type _Masked = _Requested | _My_data._ALLOC_MASK;
            size_type _Old = _My_data._Myres;
            size_type _Expanded = _Old + _Old / 2;
            return _Masked > _Expanded ? _Masked : _Expanded;
        }

        ref basic_string _Reallocate_for(_ArgTys...)(size_type _New_size, void function(pointer, size_type, _ArgTys) nothrow @nogc _Fn, _ArgTys _Args)
        {
            auto _My_data = &_Get_data();
            size_type _Old_capacity = _My_data._Myres;
            size_type _New_capacity = _Calculate_growth(_New_size);
            auto _Al = &_Getal();
            pointer _New_ptr = _Al.allocate(_New_capacity + 1); // throws
            _Base._Orphan_all();
            _My_data._Mysize = _New_size;
            _My_data._Myres = _New_capacity;
            _Fn(_New_ptr, _New_size, _Args);
            if (_My_data._BUF_SIZE <= _Old_capacity)
                _Al.deallocate(_My_data._Bx._Ptr, _Old_capacity + 1);
            _My_data._Bx._Ptr = _New_ptr;
            return this;
        }

        ref basic_string _Reallocate_grow_by(_ArgTys...)(size_type _Size_increase, void function(pointer, const(T)[], _ArgTys) nothrow @nogc _Fn, _ArgTys _Args)
        {
            auto _My_data = &_Get_data();
            size_type _Old_size = _My_data._Mysize;
            size_type _New_size = _Old_size + _Size_increase;
            size_type _Old_capacity = _My_data._Myres;
            size_type _New_capacity = _Calculate_growth(_New_size);
            auto _Al = &_Getal();
            pointer _New_ptr = _Al.allocate(_New_capacity + 1); // throws
            _Base._Orphan_all();
            _My_data._Mysize = _New_size;
            _My_data._Myres = _New_capacity;
            if (_My_data._BUF_SIZE <= _Old_capacity)
            {
                pointer _Old_ptr = _My_data._Bx._Ptr;
                _Fn(_New_ptr, _Old_ptr[0 .. _Old_size], _Args);
                _Al.deallocate(_Old_ptr, _Old_capacity + 1);
            }
            else
                _Fn(_New_ptr, _My_data._Bx._Buf[0 .. _Old_size], _Args);
            _My_data._Bx._Ptr = _New_ptr;
            return this;
        }

        void _Tidy_deallocate()
        {
            _Base._Orphan_all();
            auto _My_data = &_Get_data();
            if (_My_data._BUF_SIZE <= _My_data._Myres)
            {
                pointer _Ptr = _My_data._Bx._Ptr;
                auto _Al = &_Getal();
                _Al.deallocate(_Ptr, _My_data._Myres + 1);
            }
            _My_data._Mysize = 0;
            _My_data._Myres = _My_data._BUF_SIZE - 1;
            _My_data._Bx._Buf[0] = T(0);
        }

        void _Swap_data(bool _memcpy : true)(ref basic_string _Right)
        {
            import core.stdc.string : memcpy;

            // exchange _String_val instances with _Right, memcpy optimization
            auto _My_data = &_Get_data();
            auto _My_data_mem = cast(ubyte*)_My_data + _Memcpy_val_offset;
            auto _Right_data_mem = cast(ubyte*)(&_Right._Get_data()) + _Memcpy_val_offset;
            ubyte[_Memcpy_val_size] _Temp_mem;
            memcpy(_Temp_mem.ptr, _My_data_mem, _Memcpy_val_size);
            memcpy(_My_data_mem, _Right_data_mem, _Memcpy_val_size);
            memcpy(_Right_data_mem, _Temp_mem.ptr, _Memcpy_val_size);
        }

        void _Swap_data(bool _memcpy : false)(ref basic_string _Right)
        {
            import core.lifetime : swap;

            // exchange _String_val instances with _Right, general case
            auto _My_data = &_Get_data();
            auto _Right_data = &_Right._Get_data();
            const bool _My_large = _My_data._Large_string_engaged();
            const bool _Right_large = _Right_data._Large_string_engaged();
            if (_My_large)
            {
                if (_Right_large) // swap buffers, iterators preserved
                    swap(_My_data._Bx._Ptr, _Right_data._Bx._Ptr);
                else // swap large with small
                    _Swap_bx_large_with_small(*_My_data, *_Right_data);
            }
            else
            {
                if (_Right_large) // swap small with large
                    _Swap_bx_large_with_small(*_Right_data, *_My_data);
                else
                {
                    enum _BUF_SIZE = _My_data._BUF_SIZE;
                    T[_BUF_SIZE] _Temp_buf;
                    _Temp_buf[0 .. _BUF_SIZE] = _My_data._Bx._Buf[0 .. _BUF_SIZE];
                    _My_data._Bx._Buf[0 .. _BUF_SIZE] = _Right_data._Bx._Buf[0 .. _BUF_SIZE];
                    _Right_data._Bx._Buf[0 .. _BUF_SIZE] = _Temp_buf[0 .. _BUF_SIZE];
                }
            }

            swap(_My_data._Mysize, _Right_data._Mysize);
            swap(_My_data._Myres, _Right_data._Myres);
        }

        void _Swap_bx_large_with_small(ref _Scary_val _Starts_large, ref _Scary_val _Starts_small)
        {
            // exchange a string in large mode with one in small mode
            pointer _Ptr = _Starts_large._Bx._Ptr;
            _Starts_large._Bx._Buf[] = _Starts_small._Bx._Buf[];
            _Starts_small._Bx._Ptr = _Ptr;
        }

        _String_alloc!(_String_base_types!(T, Alloc)) _Base;
    }
    else version (CppRuntime_Gcc)
    {
        version (_GLIBCXX_USE_CXX98_ABI)
        {
            //----------------------------------------------------------------------------------
            // Old GCC/libstdc++ ref-counted implementation
            //----------------------------------------------------------------------------------

            ///
            this(DefaultConstruct)
            {
                version (_GLIBCXX_FULLY_DYNAMIC_STRING)
                    static_assert(false, "DO WE NEED THIS?");
                else
                    _M_data = _S_empty_rep()._M_refdata();
            }
            ///
            this(const(T)[] str, ref const(allocator_type) al)                  { _M_assign_allocator(al); this(str); }
            ///
            this(const(T)[] str)
            {
                _M_data = _S_construct(str.ptr, str.ptr + str.length, _M_get_allocator);
            }
            ///
            this(const ref basic_string str)
            {
                import core.stdcpp.type_traits : is_empty;

                static if (!is_empty!allocator_type.value)
                    _M_Alloc = str.get_allocator();
                _M_data = str._M_rep()._M_grab(get_allocator(), str.get_allocator());
            }

            ///
            ~this()                                                             { _M_rep()._M_dispose(get_allocator()); }

            ///
            ref inout(Alloc) get_allocator() inout                              { return _M_get_allocator(); }

            ///
            size_type max_size() const nothrow @safe                            { return _Rep._S_max_size; }

            ///
            size_type size() const nothrow @safe                                { return _M_rep()._M_length; }
            ///
            size_type capacity() const nothrow                                  { return _M_rep()._M_capacity; }
            ///
            inout(T)* data() inout @safe                                        { return _M_data; }
            ///
            inout(T)[] as_array() inout nothrow @trusted                        { return _M_data[0 .. _M_rep()._M_length]; }
            ///
            ref inout(T) at(size_type i) inout nothrow                          { return _M_data[0 .. _M_rep()._M_length][i]; }

            ///
            ref basic_string assign(const(T)[] str)
            {
                const(T)* __s = str.ptr;
                size_t __n = str.length;
//                __glibcxx_requires_string_len(__s, __n);
                _M_check_length(size(), __n, "basic_string::assign");
                if (_M_disjunct(__s) || _M_rep()._M_is_shared())
                    return _M_replace_safe(size_type(0), this.size(), __s, __n);
                else
                {
                    const size_type __pos = __s - _M_data;
                    if (__pos >= __n)
                        _S_copy(_M_data, __s, __n);
                    else if (__pos)
                        _S_move(_M_data, __s, __n);
                    _M_rep()._M_set_length_and_sharable(__n);
                    return this;
                }
            }

            ///
            ref basic_string assign(const ref basic_string str)
            {
                if (_M_rep() != str._M_rep())
                {
                    // XXX MT
                    allocator_type __a = this.get_allocator();
                    T* __tmp = str._M_rep()._M_grab(__a, str.get_allocator());
                    _M_rep()._M_dispose(__a);
                    _M_data = __tmp;
                }
                return this;
            }

            ///
            ref basic_string append(const(T)[] str)
            {
                const(T)* __s = str.ptr;
                size_t __n = str.length;
//                __glibcxx_requires_string_len(__s, __n);
                if (__n)
                {
                    _M_check_length(size_type(0), __n, "basic_string::append");
                    const size_type __len = __n + size();
                    if (__len > capacity() || _M_rep()._M_is_shared())
                    {
                        if (_M_disjunct(__s))
                            reserve(__len);
                        else
                        {
                            const size_type __off = __s - _M_data;
                            reserve(__len);
                            __s = _M_data + __off;
                        }
                    }
                    _S_copy(_M_data + size(), __s, __n);
                    _M_rep()._M_set_length_and_sharable(__len);
                }
                return this;
            }

            ///
            ref basic_string append(size_type __n, T __c)
            {
                if (__n)
                {
                    _M_check_length(size_type(0), __n, "basic_string::append");
                    const size_type __len = __n + size();
                    if (__len > capacity() || _M_rep()._M_is_shared())
                        reserve(__len);
                    const __sz = size();
                    _M_data[__sz .. __sz + __n] = __c;
                    _M_rep()._M_set_length_and_sharable(__len);
                }
                return this;
            }

            ///
            void reserve(size_type __res = 0)
            {
                if (__res != capacity() || _M_rep()._M_is_shared())
                {
                    // Make sure we don't shrink below the current size
                    if (__res < size())
                        __res = size();
                    allocator_type __a = get_allocator();
                    T* __tmp = _M_rep()._M_clone(__a, __res - size());
                    _M_rep()._M_dispose(__a);
                    _M_data = __tmp;
                }
            }

            ///
            void shrink_to_fit() nothrow
            {
                if (capacity() > size())
                {
                    try reserve(0);
                    catch (Throwable) {}
                }
            }

            ///
            ref basic_string insert(size_type __pos, const(T)* __s, size_type __n)
            {
//                __glibcxx_requires_string_len(__s, __n);
                cast(void) _M_check(__pos, "basic_string::insert");
                _M_check_length(size_type(0), __n, "basic_string::insert");
                if (_M_disjunct(__s) || _M_rep()._M_is_shared())
                    return _M_replace_safe(__pos, size_type(0), __s, __n);
                else
                {
                    // Work in-place.
                    const size_type __off = __s - _M_data;
                    _M_mutate(__pos, 0, __n);
                    __s = _M_data + __off;
                    T* __p = _M_data + __pos;
                    if (__s  + __n <= __p)
                        __p[0 .. __n] = __s[0 .. __n];
                    else if (__s >= __p)
                        __p[0 .. __n] = (__s + __n)[0 .. __n];
                    else
                    {
                        const size_type __nleft = __p - __s;
                        __p[0 .. __nleft] = __s[0.. __nleft];
                        (__p + __nleft)[0 .. __n - __nleft] = (__p + __n)[0 .. __n - __nleft];
                    }
                    return this;
                }
            }

            ///
            ref basic_string insert(size_type pos, size_type n, T c)
            {
                return _M_replace_aux(_M_check(pos, "basic_string::insert"), size_type(0), n, c);
            }

            ///
            ref basic_string replace(size_type __pos, size_type __n1, const(T)* __s, size_type __n2)
            {
//                __glibcxx_requires_string_len(__s, __n2);
                cast(void) _M_check(__pos, "basic_string::replace");
                __n1 = _M_limit(__pos, __n1);
                _M_check_length(__n1, __n2, "basic_string::replace");
                bool __left;
                if (_M_disjunct(__s) || _M_rep()._M_is_shared())
                    return _M_replace_safe(__pos, __n1, __s, __n2);
                else if ((__left = __s + __n2 <= _M_data + __pos) == true || _M_data + __pos + __n1 <= __s)
                {
                    // Work in-place: non-overlapping case.
                    size_type __off = __s - _M_data;
                    __left ? __off : (__off += __n2 - __n1);
                    _M_mutate(__pos, __n1, __n2);
                    (_M_data + __pos)[0 .. __n2] = (_M_data + __off)[0 .. __n2];
                    return this;
                }
                else
                {
                    // Todo: overlapping case.
                    auto __tmp = basic_string(__s[0 .. __n2]);
                    return _M_replace_safe(__pos, __n1, __tmp._M_data, __n2);
                }
            }

            ///
            ref basic_string replace(size_type pos, size_type n1, size_type n2, T c)
            {
                return _M_replace_aux(_M_check(pos, "basic_string::replace"), _M_limit(pos, n1), n2, c);
            }

            ///
            void swap(ref basic_string __s)
            {
                if (_M_rep()._M_is_leaked())
                    _M_rep()._M_set_sharable();
                if (__s._M_rep()._M_is_leaked())
                    __s._M_rep()._M_set_sharable();
                if (this.get_allocator() == __s.get_allocator())
                {
                    T* __tmp = _M_data;
                    _M_data = __s._M_data;
                    __s._M_data = __tmp;
                }
                // The code below can usually be optimized away.
                else
                {
                    import core.lifetime : move;

                    auto __tmp1 = basic_string(this[], __s.get_allocator());
                    auto __tmp2 = basic_string(__s[], this.get_allocator());
                    this = move(__tmp2);
                    __s = move(__tmp1);
                }
            }

        private:
            import core.stdcpp.type_traits : is_empty;

            version (__GTHREADS)
            {
                import core.atomic;
                alias _Atomic_word = int; // should we use atomic!int?
            }
            else
                alias _Atomic_word = int;

            struct _Rep_base
            {
                size_type       _M_length;
                size_type       _M_capacity;
                _Atomic_word    _M_refcount;
            }

            struct _Rep
            {
                _Rep_base base;
                alias base this;

                alias _Raw_bytes_alloc = Alloc.rebind!char;

                enum size_type _S_max_size = (((npos - _Rep_base.sizeof) / T.sizeof) - 1) / 4;
                enum T _S_terminal = T(0);

                __gshared size_type[(_Rep_base.sizeof + T.sizeof + size_type.sizeof - 1) / size_type.sizeof] _S_empty_rep_storage;

                static ref _Rep _S_empty_rep() nothrow @trusted { return *cast(_Rep*)_S_empty_rep_storage.ptr; }

                void _M_set_sharable() nothrow
                {
                    _M_refcount = 0;
                }

                void _M_set_length_and_sharable(size_type __n) nothrow
                {
                    if (&this != &_S_empty_rep())
                    {
                        _M_set_sharable();
                        _M_length = __n;
                        _M_refdata()[__n] = _S_terminal;
                    }
                }

                bool _M_is_leaked() const nothrow
                {
                    import core.atomic : atomicLoad;

                    version (__GTHREADS)
                        return atomicLoad!(MemoryOrder.raw)(this._M_refcount) < 0;
                    else
                        return _M_refcount < 0;
                }
//
                bool _M_is_shared() const nothrow
                {
                    import core.atomic : atomicLoad;

                    version (__GTHREADS)
                        return atomicLoad!(MemoryOrder.acq)(this._M_refcount) > 0;
                    else
                        return _M_refcount > 0;
                }

                T* _M_refdata() nothrow @trusted    { return cast(T*)(&this + 1); }

                T* _M_grab(ref allocator_type __alloc1, const ref allocator_type __alloc2)
                {
                    return (!_M_is_leaked() && __alloc1 == __alloc2)
                          ? _M_refcopy() : _M_clone(__alloc1);
                }

                static _Rep* _S_create(size_type __capacity, size_type __old_capacity, ref Alloc __alloc)
                {
                    assert(__capacity <= _S_max_size);
//                    if (__capacity > _S_max_size)
//                        __throw_length_error(__N("basic_string::_S_create"));

                    enum __pagesize = 4096;
                    enum __malloc_header_size = 4 * pointer.sizeof;

                    if (__capacity > __old_capacity && __capacity < 2 * __old_capacity)
                        __capacity = 2 * __old_capacity;

                    size_type __size = (__capacity + 1) * T.sizeof + _Rep.sizeof;

                    const size_type __adj_size = __size + __malloc_header_size;
                    if (__adj_size > __pagesize && __capacity > __old_capacity)
                    {
                        const size_type __extra = __pagesize - __adj_size % __pagesize;
                        __capacity += __extra / T.sizeof;
                        if (__capacity > _S_max_size)
                            __capacity = _S_max_size;
                        __size = (__capacity + 1) * T.sizeof + _Rep.sizeof;
                    }

                    _Rep* __p = cast(_Rep*)_Raw_bytes_alloc(__alloc).allocate(__size);
                    *__p = _Rep.init;
                    __p._M_capacity = __capacity;
                    __p._M_set_sharable();
                    return __p;
                }

                void _M_dispose(ref Alloc __a)
                {
                    import core.stdcpp.xutility : __exchange_and_add_dispatch;

                    if (&this != &_S_empty_rep())
                    {
                        // Be race-detector-friendly.  For more info see bits/c++config.
//                        _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&this._M_refcount);
                        // Decrement of _M_refcount is acq_rel, because:
                        // - all but last decrements need to release to synchronize with
                        //   the last decrement that will delete the object.
                        // - the last decrement needs to acquire to synchronize with
                        //   all the previous decrements.
                        // - last but one decrement needs to release to synchronize with
                        //   the acquire load in _M_is_shared that will conclude that
                        //   the object is not shared anymore.
                        if (__exchange_and_add_dispatch(&this._M_refcount, -1) <= 0)
                        {
//                            _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&this._M_refcount);
                            _M_destroy(__a);
                        }
                    }
                }

                void _M_destroy(ref Alloc __a)
                {
                    const size_type __size = _Rep_base.sizeof + (_M_capacity + 1) * T.sizeof;
                    _Raw_bytes_alloc(__a).deallocate(cast(char*)&this, __size);
                }

                T* _M_refcopy() nothrow @trusted
                {
                    import core.stdcpp.xutility : __atomic_add_dispatch;

                    if (&this != &_S_empty_rep())
                        __atomic_add_dispatch(&this._M_refcount, 1);
                    return _M_refdata();
                    // XXX MT
                }

                T* _M_clone(ref Alloc __alloc, size_type __res = 0)
                {
                    const size_type __requested_cap = _M_length + __res;
                    _Rep* __r = _S_create(__requested_cap, _M_capacity, __alloc);
                    if (_M_length)
                        _S_copy(__r._M_refdata(), _M_refdata(), _M_length);

                    __r._M_set_length_and_sharable(_M_length);
                    return __r._M_refdata();
                }
            }

            static if (!is_empty!allocator_type.value)
                allocator_type _M_Alloc;
            T* _M_p; // The actual data.

            alias _M_data = _M_p;

            pragma (inline, true)
            {
                void eos(size_type offset)
                {
                    _M_mutate(offset, size() - offset, size_type(0));
                }

                ref inout(allocator_type) _M_get_allocator() inout
                {
                    static if (!is_empty!allocator_type.value)
                        return _M_Alloc;
                    else
                        return *cast(inout(allocator_type)*)&this;
                }

                _Rep* _M_rep() const nothrow @trusted   { return &(cast(_Rep*)_M_data)[-1]; }

                size_type _M_limit(size_type __pos, size_type __off) const @safe nothrow @nogc pure
                {
                    const bool __testoff =  __off < size() - __pos;
                    return __testoff ? __off : size() - __pos;
                }
            }

            size_type _M_check(size_type __pos, const char* __s) const
            {
                assert(__pos <= size());
//                if (__pos > size())
//                    __throw_out_of_range_fmt(__N("%s: __pos (which is %zu) > "
//                                                    "this->size() (which is %zu)"),
//                                                __s, __pos, this->size());
                return __pos;
            }

            static ref _Rep _S_empty_rep() nothrow
            {
                return _Rep._S_empty_rep();
            }

            static T* _S_construct(const(T)* __beg, const(T)* __end, ref Alloc __a)
            {
                version (_GLIBCXX_FULLY_DYNAMIC_STRING) {} else
                {
                    if (__beg == __end && __a == Alloc())
                        return _S_empty_rep()._M_refdata();
                }

                const size_type __dnew = __end - __beg;

                _Rep* __r = _Rep._S_create(__dnew, size_type(0), __a);
                _S_copy(__r._M_refdata(), __beg, __end - __beg);
                __r._M_set_length_and_sharable(__dnew);
                return __r._M_refdata();
            }

            ref basic_string _M_replace_safe(size_type __pos1, size_type __n1, const(T)* __s, size_type __n2)
            {
                _M_mutate(__pos1, __n1, __n2);
                if (__n2)
                    _S_copy(_M_data + __pos1, __s, __n2);
                return this;
            }

            ref basic_string _M_replace_aux(size_type __pos1, size_type __n1, size_type __n2, T __c)
            {
                _M_check_length(__n1, __n2, "basic_string::_M_replace_aux");
                _M_mutate(__pos1, __n1, __n2);
                if (__n2)
                    _M_data[__pos1 .. __pos1 + __n2] = __c;
                return this;
            }

            void _M_mutate(size_type __pos, size_type __len1, size_type __len2)
            {
                const size_type __old_size = size();
                const size_type __new_size = __old_size + __len2 - __len1;
                const size_type __how_much = __old_size - __pos - __len1;

                if (__new_size > capacity() || _M_rep()._M_is_shared())
                {
                    allocator_type __a = get_allocator();
                    _Rep* __r = _Rep._S_create(__new_size, capacity(), __a);

                    if (__pos)
                        _S_copy(__r._M_refdata(), _M_data, __pos);
                    if (__how_much)
                        _S_copy(__r._M_refdata() + __pos + __len2, _M_data + __pos + __len1, __how_much);

                    allocator_type* __al = cast() &__a;
                    _M_rep()._M_dispose(*__al);
                    _M_data = __r._M_refdata();
                }
                else if (__how_much && __len1 != __len2)
                    _S_move(_M_data + __pos + __len2, _M_data + __pos + __len1, __how_much);
                _M_rep()._M_set_length_and_sharable(__new_size);
            }
        }
        else
        {
            pragma(msg, "libstdc++ std::__cxx11::basic_string is not yet supported; the struct contains an interior pointer which breaks D move semantics!");

            //----------------------------------------------------------------------------------
            // GCC/libstdc++ modern implementation
            //----------------------------------------------------------------------------------

            ///
            this(DefaultConstruct)                                              { _M_p = _M_local_data(); _M_set_length(0); }
            ///
            this(const(T)[] str, ref const(allocator_type) al)                  { _M_assign_allocator(al); this(str); }
            ///
            this(const(T)[] str)
            {
                _M_p = _M_local_data();
                _M_construct(str.ptr, str.length);
            }
            ///
            this(this)
            {
                assert(false);
                // TODO: how do I know if it was local before?!
            }

            ///
            ~this()                                                             { _M_dispose(); }

            ///
            ref inout(Alloc) get_allocator() inout                              { return _M_get_allocator(); }

            ///
            size_type max_size() const nothrow @safe                            { return ((size_t.max / T.sizeof) - 1) / 2; }

            ///
            size_type size() const nothrow @safe                                { return _M_string_length; }
            ///
            size_type capacity() const nothrow                                  { return _M_is_local ? _S_local_capacity : _M_allocated_capacity; }
            ///
            inout(T)* data() inout @safe                                        { return _M_data; }
            ///
            inout(T)[] as_array() inout nothrow @trusted                        { return _M_data[0 .. _M_string_length]; }
            ///
            ref inout(T) at(size_type i) inout nothrow                          { return _M_data[0 .. _M_string_length][i]; }

            ///
            ref basic_string assign(const(T)[] str)
            {
//                __glibcxx_requires_string_len(str.ptr, str.length);
                return _M_replace(size_type(0), size(), str.ptr, str.length);
            }

            ///
            ref basic_string assign(const ref basic_string str)
            {
                if (&this != &str)
                    assign(str.as_array);
                return this;
            }

            ///
            ref basic_string append(const(T)[] str)
            {
//                __glibcxx_requires_string_len(str.ptr, str.length);
                _M_check_length(size_type(0), str.length, "basic_string::append");
                return _M_append(str.ptr, str.length);
            }

            ///
            ref basic_string append(size_type n, T c)
            {
                return _M_replace_aux(size(), size_type(0), n, c);
            }

            ///
            void reserve(size_type __res = 0)
            {
                // Make sure we don't shrink below the current size.
                if (__res < length())
                    __res = length();

                const size_type __capacity = capacity();
                if (__res != __capacity)
                {
                    if (__res > __capacity || __res > size_type(_S_local_capacity))
                    {
                        pointer __tmp = _M_create(__res, __capacity);
                        _S_copy(__tmp, _M_data, length() + 1);
                        _M_dispose();
                        _M_data = __tmp;
                        _M_capacity = __res;
                    }
                    else if (!_M_is_local())
                    {
                        _S_copy(_M_local_data(), _M_data, length() + 1);
                        _M_destroy(__capacity);
                        _M_data = _M_local_data();
                    }
                }
            }

            ///
            void shrink_to_fit() nothrow
            {
                if (capacity() > size())
                {
                    try reserve(0);
                    catch (Throwable) {}
                }
            }

            ///
            ref basic_string insert(size_type pos, const(T)* s, size_type n)
            {
                return replace(pos, size_type(0), s, n);
            }

            ///
            ref basic_string insert(size_type pos, size_type n, T c)
            {
                return _M_replace_aux(_M_check(pos, "basic_string::insert"), size_type(0), n, c);
            }

            ///
            ref basic_string replace(size_type pos, size_type n1, const(T)* s, size_type n2)
            {
//                __glibcxx_requires_string_len(s, n2);
                return _M_replace(_M_check(pos, "basic_string::replace"), _M_limit(pos, n1), s, n2);
            }

            ///
            ref basic_string replace(size_type pos, size_type n1, size_type n2, T c)
            {
                return _M_replace_aux(_M_check(pos, "basic_string::replace"), _M_limit(pos, n1), n2, c);
            }

            ///
            void swap(ref basic_string __s)
            {
                if (&this == &__s)
                    return;

                __alloc_on_swap(__s._M_get_allocator());

                if (_M_is_local())
                {
                    if (__s._M_is_local())
                    {
                        if (length() && __s.length())
                        {
                            T[_S_local_capacity + 1] __tmp_data;
                            __tmp_data[] = __s._M_local_buf[];
                            __s._M_local_buf[] = _M_local_buf[];
                            _M_local_buf[] = __tmp_data[];
                        }
                        else if (__s.length())
                        {
                            _M_local_buf[] = __s._M_local_buf[];
                            _M_length = __s.length();
                            __s._M_set_length(0);
                            return;
                        }
                        else if (length())
                        {
                            __s._M_local_buf[] = _M_local_buf[];
                            __s._M_length = length();
                            _M_set_length(0);
                            return;
                        }
                    }
                    else
                    {
                        const size_type __tmp_capacity = __s._M_allocated_capacity;
                        __s._M_local_buf[] = _M_local_buf[];
                        _M_data = __s._M_data;
                        __s._M_data = __s._M_local_buf.ptr;
                        _M_capacity = __tmp_capacity;
                    }
                }
                else
                {
                    const size_type __tmp_capacity = _M_allocated_capacity;
                    if (__s._M_is_local())
                    {
                        _M_local_buf[] = __s._M_local_buf[];
                        __s._M_data = _M_data;
                        _M_data = _M_local_buf.ptr;
                    }
                    else
                    {
                        pointer __tmp_ptr = _M_data;
                        _M_data = __s._M_data;
                        __s._M_data = __tmp_ptr;
                        _M_capacity = __s._M_allocated_capacity;
                    }
                    __s._M_capacity = __tmp_capacity;
                }

                const size_type __tmp_length = length();
                _M_length = __s.length();
                __s._M_length = __tmp_length;
            }

        private:
//            import core.exception : RangeError;
            import core.stdcpp.type_traits : is_empty;

            static if (!is_empty!allocator_type.value)
                allocator_type _M_Alloc;
            pointer _M_p; // The actual data.
            size_type _M_string_length;

            enum size_type _S_local_capacity = 15 / T.sizeof;
            union
            {
                T[_S_local_capacity + 1]    _M_local_buf;
                size_type                   _M_allocated_capacity;
            }

            alias _M_length = _M_string_length;
            alias _M_capacity = _M_allocated_capacity;
            alias _M_data = _M_p;

            pragma (inline, true)
            {
                void eos(size_type offset) nothrow                              { _M_set_length(offset); }

                inout(pointer) _M_local_data() inout                            { return _M_local_buf.ptr; }
                bool _M_is_local() const                                        { return _M_data == _M_local_data; }

                ref inout(allocator_type) _M_get_allocator() inout
                {
                    static if (!is_empty!allocator_type.value)
                        return _M_Alloc;
                    else
                        return *cast(inout(allocator_type)*)&this;
                }

                void _M_set_length(size_type __n)
                {
                    _M_length = __n;
                    _M_data[__n] = T(0);
                }

                size_type _M_check(size_type __pos, const char* __s) const
                {
                    assert(__pos <= size());
//                    if (__pos > size())
//                        __throw_out_of_range_fmt(__N("%s: __pos (which is %zu) > "
//                                   "this->size() (which is %zu)"),
//                               __s, __pos, this->size());
                    return __pos;
                }

                // NB: _M_limit doesn't check for a bad __pos value.
                size_type _M_limit(size_type __pos, size_type __off) const nothrow pure @nogc @safe
                {
                    const bool __testoff =  __off < size() - __pos;
                    return __testoff ? __off : size() - __pos;
                }

                void __alloc_on_swap()(ref allocator_type __a)
                if (!is_empty!allocator_type.value)
                {
                    import core.internal.lifetime : swap;

                    static if (allocator_traits!allocator_type.propagate_on_container_swap)
                      swap(_M_get_allocator(), __a);
                }

                void __alloc_on_swap()(ref allocator_type __a)
                if (is_empty!allocator_type.value)
                {
                    import core.internal.lifetime : swap;
                    import core.lifetime : move;

                    static if (allocator_traits!allocator_type.propagate_on_container_swap)
                    {
                        static if (is(typeof(_M_get_allocator().opAssign(move(__a)))))
                            swap(_M_get_allocator(), __a);
                    }
                }
            }

            void _M_construct(const(T)* __beg, size_type __dnew)
            {
                if (__dnew > _S_local_capacity)
                {
                    _M_data = _M_create(__dnew, size_type(0));
                    _M_capacity = __dnew;
                }
                _M_data[0 .. __dnew] = __beg[0 .. __dnew];
                _M_set_length(__dnew);
            }

            pointer _M_create(ref size_type __capacity, size_type __old_capacity)
            {
                assert(__capacity <= max_size());
//                if (__capacity > max_size())
//                    throw new RangeError("Length exceeds `max_size()`"); // std::__throw_length_error(__N("basic_string::_M_create"));
                if (__capacity > __old_capacity && __capacity < 2 * __old_capacity)
                {
                    __capacity = 2 * __old_capacity;
                    if (__capacity > max_size())
                        __capacity = max_size();
                }
                return _M_get_allocator().allocate(__capacity + 1);
            }

            ref basic_string _M_replace(size_type __pos, size_type __len1, const(T)* __s, const size_type __len2)
            {
                _M_check_length(__len1, __len2, "basic_string::_M_replace");

                const size_type __old_size = size();
                const size_type __new_size = __old_size + __len2 - __len1;

                if (__new_size <= capacity())
                {
                    pointer __p = _M_data + __pos;

                    const size_type __how_much = __old_size - __pos - __len1;
                    if (_M_disjunct(__s))
                    {
                        if (__how_much && __len1 != __len2)
                            _S_move(__p + __len2, __p + __len1, __how_much);
                        if (__len2)
                            _S_copy(__p, __s, __len2);
                    }
                    else
                    {
                        // Work in-place.
                        if (__len2 && __len2 <= __len1)
                            _S_move(__p, __s, __len2);
                        if (__how_much && __len1 != __len2)
                            _S_move(__p + __len2, __p + __len1, __how_much);
                        if (__len2 > __len1)
                        {
                            if (__s + __len2 <= __p + __len1)
                                _S_move(__p, __s, __len2);
                            else if (__s >= __p + __len1)
                                _S_copy(__p, __s + __len2 - __len1, __len2);
                            else
                            {
                                const size_type __nleft = (__p + __len1) - __s;
                                _S_move(__p, __s, __nleft);
                                _S_copy(__p + __nleft, __p + __len2,
                                        __len2 - __nleft);
                            }
                        }
                    }
                }
                else
                    _M_mutate(__pos, __len1, __s, __len2);

                _M_set_length(__new_size);
                return this;
            }

            ref basic_string _M_replace_aux(size_type __pos1, size_type __n1, size_type __n2, T __c)
            {
                _M_check_length(__n1, __n2, "basic_string::_M_replace_aux");

                const size_type __old_size = size();
                const size_type __new_size = __old_size + __n2 - __n1;

                if (__new_size <= capacity())
                {
                    pointer __p = _M_data + __pos1;

                    const size_type __how_much = __old_size - __pos1 - __n1;
                    if (__how_much && __n1 != __n2)
                        _S_move(__p + __n2, __p + __n1, __how_much);
                }
                else
                    _M_mutate(__pos1, __n1, null, __n2);

                if (__n2)
                    _M_data[__pos1 .. __pos1 + __n2] = __c;

                _M_set_length(__new_size);
                return this;
            }

            ref basic_string _M_append(const(T)* __s, size_type __n)
            {
                const size_type __len = __n + size();
                if (__len <= capacity())
                {
                    if (__n)
                        _S_copy(_M_data + size(), __s, __n);
                }
                else
                    _M_mutate(size(), size_type(0), __s, __n);
                _M_set_length(__len);
                return this;
            }

            void _M_mutate(size_type __pos, size_type __len1, const(T)* __s, size_type __len2)
            {
                const size_type __how_much = length() - __pos - __len1;

                size_type __new_capacity = length() + __len2 - __len1;
                pointer __r = _M_create(__new_capacity, capacity());

                if (__pos)
                    _S_copy(__r, _M_data, __pos);
                if (__s && __len2)
                    _S_copy(__r + __pos, __s, __len2);
                if (__how_much)
                    _S_copy(__r + __pos + __len2,
                            _M_data + __pos + __len1, __how_much);

                _M_dispose();
                _M_data = __r;
                _M_capacity = __new_capacity;
            }

            void _M_dispose()
            {
                if (!_M_is_local)
                    _M_destroy(_M_allocated_capacity);
            }

            void _M_destroy(size_type __size)
            {
                _M_get_allocator().deallocate(_M_data, __size + 1);
            }
        }

        // common GCC/stdlibc++ code

        void _M_check_length(size_type __n1, size_type __n2, const char* __s) const
        {
            assert (!(max_size() - (size() - __n1) < __n2));
//            if (max_size() - (size() - __n1) < __n2)
//            __throw_length_error(__N(__s));
        }

        void _M_assign_allocator(ref const(allocator_type) al) nothrow
        {
            static if (!is_empty!allocator_type.value)
                _M_Alloc = al;
        }

        bool _M_disjunct(const(T)* __s) const nothrow
        {
            return __s < _M_data || _M_data + size() < __s;
        }

        static void _S_move(T* __d, const(T)* __s, size_type __n)
        {
            if (__d == __s)
                return;
            if (__d < __s)
            {
                for (size_t i = 0; i < __n; ++i)
                    __d[i] = __s[i];
            }
            else
            {
                for (ptrdiff_t i = __n - 1; i >= 0; --i)
                    __d[i] = __s[i];
            }
        }
        static void _S_copy(T* __d, const(T)* __s, size_type __n)
        {
            __d[0 .. __n] = __s[0 .. __n];
        }
    }
    else version (CppRuntime_Clang)
    {
        //----------------------------------------------------------------------------------
        // Clang/libc++ implementation
        //----------------------------------------------------------------------------------

        ///
        this(DefaultConstruct)                                              { __zero(); }
        ///
        this(const(T)[] str, ref const(allocator_type) al)                  { __assign_allocator(al); this(str); }
        ///
        this(const(T)[] str)                                                { __init(str.ptr, str.length); }
        ///
        this(this)
        {
            if (__is_long())
                __init(__get_long_pointer(), __get_long_size());
        }

        ///
        ~this()
        {
//            __get_db()->__erase_c(this); // TODO: support `_LIBCPP_DEBUG_LEVEL >= 2` ??
            if (__is_long())
                __alloc().deallocate(__get_long_pointer(), __get_long_cap());
        }

        ///
        ref inout(Alloc) get_allocator() inout                              { return __alloc(); }

        ///
        size_type max_size() const nothrow @safe
        {
            size_type __m = size_t.max; // TODO: __alloc_traits::max_size(__alloc());
            version (BigEndian)
                return (__m <= ~__long_mask ? __m : __m/2) - __alignment;
            else
                return __m - __alignment;
        }

        ///
        size_type size() const nothrow                                      { return __is_long() ? __get_long_size() : __get_short_size(); }
        ///
        size_type capacity() const nothrow                                  { return (__is_long() ? __get_long_cap() : __min_cap) - 1; }
        ///
        inout(T)* data() inout @trusted                                     { return __get_pointer(); }
        ///
        inout(T)[] as_array() scope return inout nothrow @trusted           { return __get_pointer()[0 .. size()]; }
        ///
        ref inout(T) at(size_type i) inout nothrow @trusted                 { return __get_pointer()[0 .. size()][i]; }

        ///
        ref basic_string assign(const(T)[] str)
        {
            const(value_type)* __s = str.ptr;
            size_type __n = str.length;
            size_type __cap = capacity();
            if (__cap >= __n)
            {
                value_type* __p = __get_pointer();
                __p[0 .. __n] = __s[0 .. __n]; // TODO: is memmove?
                __p[__n] = value_type(0);
                __set_size(__n);
//                __invalidate_iterators_past(__n); // TODO: support `_LIBCPP_DEBUG_LEVEL >= 2` ??
            }
            else
            {
                size_type __sz = size();
                __grow_by_and_replace(__cap, __n - __cap, __sz, 0, __sz, __n, __s);
            }
            return this;
        }

        ///
        ref basic_string assign(const ref basic_string str)
        {
            if (&this != &str)
                assign(str.as_array);
            return this;
        }

        ///
        ref basic_string append(const(T)[] str)
        {
            const(value_type)* __s = str.ptr;
            size_type __n = str.length;
            size_type __cap = capacity();
            size_type __sz = size();
            if (__cap - __sz >= __n)
            {
                if (__n)
                {
                    value_type* __p = __get_pointer();
                    (__p + __sz)[0 .. __n] = __s[0 .. __n];
                    __sz += __n;
                    __set_size(__sz);
                    __p[__sz] = value_type(0);
                }
            }
            else
                __grow_by_and_replace(__cap, __sz + __n - __cap, __sz, __sz, 0, __n, __s);
            return this;
        }

        ///
        ref basic_string append(size_type __n, value_type __c)
        {
            if (__n)
            {
                size_type __cap = capacity();
                size_type __sz = size();
                if (__cap - __sz < __n)
                    __grow_by(__cap, __sz + __n - __cap, __sz, __sz, 0);
                pointer __p = __get_pointer();
                __p[__sz .. __sz + __n] = __c;
                __sz += __n;
                __set_size(__sz);
                __p[__sz] = value_type(0);
            }
            return this;
        }

        ///
        void reserve(size_type __res_arg = 0)
        {
            assert(__res_arg <= max_size());
//            if (__res_arg > max_size())
//                __throw_length_error();
            size_type __cap = capacity();
            size_type __sz = size();
            __res_arg = max(__res_arg, __sz);
            __res_arg = __recommend(__res_arg);
            if (__res_arg != __cap)
            {
                pointer __new_data, __p;
                bool __was_long, __now_long;
                if (__res_arg == __min_cap - 1)
                {
                    __was_long = true;
                    __now_long = false;
                    __new_data = __get_short_pointer();
                    __p = __get_long_pointer();
                }
                else
                {
                    if (__res_arg > __cap)
                        __new_data = __alloc().allocate(__res_arg+1);
                    else
                    {
                        try
                            __new_data = __alloc().allocate(__res_arg+1);
                        catch (Throwable)
                            return;
                    }
                    __now_long = true;
                    __was_long = __is_long();
                    __p = __get_pointer();
                }
                __new_data[0 .. size()+1] = __p[0 .. size()+1];
                if (__was_long)
                    __alloc().deallocate(__p, __cap+1);
                if (__now_long)
                {
                    __set_long_cap(__res_arg+1);
                    __set_long_size(__sz);
                    __set_long_pointer(__new_data);
                }
                else
                    __set_short_size(__sz);
//                __invalidate_all_iterators(); // TODO:
            }
        }

        ///
        void shrink_to_fit()
        {
            reserve();
        }

        ///
        ref basic_string insert(size_type __pos, const(value_type)* __s, size_type __n)
        {
            assert(__n == 0 || __s != null, "string::insert received null");
            size_type __sz = size();
            assert(__pos <= __sz);
//            if (__pos > __sz)
//                this->__throw_out_of_range();
            size_type __cap = capacity();
            if (__cap - __sz >= __n)
            {
                if (__n)
                {
                    value_type* __p = __get_pointer();
                    size_type __n_move = __sz - __pos;
                    if (__n_move != 0)
                    {
                        if (__p + __pos <= __s && __s < __p + __sz)
                            __s += __n;
                        traits_type.move(__p + __pos + __n, __p + __pos, __n_move);
                    }
                    traits_type.move(__p + __pos, __s, __n);
                    __sz += __n;
                    __set_size(__sz);
                    __p[__sz] = value_type(0);
                }
            }
            else
                __grow_by_and_replace(__cap, __sz + __n - __cap, __sz, __pos, 0, __n, __s);
            return this;
        }

        ///
        ref basic_string insert(size_type pos, size_type n, value_type c)
        {
            alias __pos = pos;
            alias __n = n;
            alias __c = c;
            size_type __sz = size();
            assert(__pos <= __sz);
//            if (__pos > __sz)
//                __throw_out_of_range();
            if (__n)
            {
                size_type __cap = capacity();
                value_type* __p;
                if (__cap - __sz >= __n)
                {
                    __p = __get_pointer();
                    size_type __n_move = __sz - __pos;
                    if (__n_move != 0)
                        traits_type.move(__p + __pos + __n, __p + __pos, __n_move);
                }
                else
                {
                    __grow_by(__cap, __sz + __n - __cap, __sz, __pos, 0, __n);
                    __p = __get_long_pointer();
                }
                __p[__pos .. __pos + __n] = __c;
                __sz += __n;
                __set_size(__sz);
                __p[__sz] = value_type(0);
            }
            return this;
        }

        ///
        ref basic_string replace(size_type __pos, size_type __n1, const(T)* __s, size_type __n2)
        {
            assert(__n2 == 0 || __s != null, "string::replace received null");
            size_type __sz = size();
            assert(__pos <= __sz);
//            if (__pos > __sz)
//                __throw_out_of_range();
            __n1 = min(__n1, __sz - __pos);
            size_type __cap = capacity();
            if (__cap - __sz + __n1 >= __n2)
            {
                value_type* __p = __get_pointer();
                if (__n1 != __n2)
                {
                    size_type __n_move = __sz - __pos - __n1;
                    if (__n_move != 0)
                    {
                        if (__n1 > __n2)
                        {
                            traits_type.move(__p + __pos, __s, __n2);
                            traits_type.move(__p + __pos + __n2, __p + __pos + __n1, __n_move);
                            goto __finish;
                        }
                        if (__p + __pos < __s && __s < __p + __sz)
                        {
                            if (__p + __pos + __n1 <= __s)
                                __s += __n2 - __n1;
                            else // __p + __pos < __s < __p + __pos + __n1
                            {
                                traits_type.move(__p + __pos, __s, __n1);
                                __pos += __n1;
                                __s += __n2;
                                __n2 -= __n1;
                                __n1 = 0;
                            }
                        }
                        traits_type.move(__p + __pos + __n2, __p + __pos + __n1, __n_move);
                    }
                }
                traits_type.move(__p + __pos, __s, __n2);
        __finish:
        // __sz += __n2 - __n1; in this and the below function below can cause unsigned integer overflow,
        // but this is a safe operation, so we disable the check.
                __sz += __n2 - __n1;
                __set_size(__sz);
//                __invalidate_iterators_past(__sz); // TODO
                __p[__sz] = value_type(0);
            }
            else
                __grow_by_and_replace(__cap, __sz - __n1 + __n2 - __cap, __sz, __pos, __n1, __n2, __s);
            return this;
        }

        ///
        ref basic_string replace(size_type __pos, size_type __n1, size_type __n2, value_type __c)
        {
            size_type __sz = size();
            assert(__pos <= __sz);
//            if (__pos > __sz)
//                __throw_out_of_range();
            __n1 = min(__n1, __sz - __pos);
            size_type __cap = capacity();
            value_type* __p;
            if (__cap - __sz + __n1 >= __n2)
            {
                __p = __get_pointer();
                if (__n1 != __n2)
                {
                    size_type __n_move = __sz - __pos - __n1;
                    if (__n_move != 0)
                        traits_type.move(__p + __pos + __n2, __p + __pos + __n1, __n_move);
                }
            }
            else
            {
                __grow_by(__cap, __sz - __n1 + __n2 - __cap, __sz, __pos, __n1, __n2);
                __p = __get_long_pointer();
            }
            __p[__pos .. __pos + __n2] = __c;
            __sz += __n2 - __n1;
            __set_size(__sz);
//            __invalidate_iterators_past(__sz); // TODO
            __p[__sz] = value_type(0);
            return this;
        }

        ///
        void swap(ref basic_string __str)
        {
            import core.internal.lifetime : swap;
//            static if (_LIBCPP_DEBUG_LEVEL >= 2)
//            {
//                if (!__is_long())
//                    __get_db().__invalidate_all(&this);
//                if (!__str.__is_long())
//                    __get_db().__invalidate_all(&__str);
//                __get_db().swap(&this, &__str);
//            }
            assert(
                __alloc_traits.propagate_on_container_swap ||
                __alloc_traits.is_always_equal ||
                __alloc() == __str.__alloc(), "swapping non-equal allocators");
            swap(__r_.first(), __str.__r_.first());
            __swap_allocator(__alloc(), __str.__alloc());
        }

    private:
//        import core.exception : RangeError;
        import core.stdcpp.xutility : __compressed_pair;

        alias __alloc_traits = allocator_traits!allocator_type;

        enum __alignment = 16;

        version (_LIBCPP_ABI_ALTERNATE_STRING_LAYOUT)
        {
            struct __long
            {
                pointer   __data_;
                size_type __size_;
                size_type __cap_;
            }

            version (BigEndian)
            {
                enum size_type __short_mask = 0x01;
                enum size_type __long_mask  = 0x1;
            }
            else
            {
                enum size_type __short_mask = 0x80;
                enum size_type __long_mask  = ~(size_type(~0) >> 1);
            }

            enum size_type __min_cap = (__long.sizeof - 1)/value_type.sizeof > 2 ? (__long.sizeof - 1)/value_type.sizeof : 2;

            struct __short
            {
                value_type[__min_cap] __data_;
                struct
                {
                    static if (value_type.sizeof > 1)
                        ubyte[value_type.sizeof-1] __xx; // __padding<value_type>
                    ubyte __size_;
                }
            }
        }
        else
        {
            struct __long
            {
                size_type __cap_;
                size_type __size_;
                pointer   __data_;
            }

            version (BigEndian)
            {
                enum size_type __short_mask = 0x80;
                enum size_type __long_mask  = ~(size_type(~0) >> 1);
            }
            else
            {
                enum size_type __short_mask = 0x01;
                enum size_type __long_mask  = 0x1;
            }

            enum size_type __min_cap = (__long.sizeof - 1)/value_type.sizeof > 2 ? (__long.sizeof - 1)/value_type.sizeof : 2;

            struct __short
            {
                union
                {
                    ubyte __size_;
                    value_type __lx;
                }
                value_type[__min_cap] __data_;
            }
        }

        union __ulx { __long __lx; __short __lxx; }
        enum __n_words = __ulx.sizeof / size_type.sizeof;

        struct __raw
        {
            size_type[__n_words] __words;
        }

        struct __rep
        {
            union
            {
                __long  __l;
                __short __s;
                __raw   __r;
            }
        }

        __compressed_pair!(__rep, allocator_type) __r_;

        pragma (inline, true)
        {
            void eos(size_type offset) nothrow
            {
                __set_size(offset);
//                __invalidate_iterators_past(__sz); // TODO: support `_LIBCPP_DEBUG_LEVEL >= 2` ??
                __get_pointer()[offset] = value_type(0);
            }

            version (_LIBCPP_ABI_ALTERNATE_STRING_LAYOUT)
            {
                version (BigEndian)
                {
                    void __set_short_size(size_type __s) nothrow @safe          { __r_.first().__s.__size_ = cast(ubyte)(__s << 1); }
                    size_type __get_short_size() const nothrow @safe            { return __r_.first().__s.__size_ >> 1; }
                }
                else
                {
                    void __set_short_size(size_type __s) nothrow @safe          { __r_.first().__s.__size_ = cast(ubyte)(__s);}
                    size_type __get_short_size() const nothrow @safe            { return __r_.first().__s.__size_;}
                }
            }
            else
            {
                version (BigEndian)
                {
                    void __set_short_size(size_type __s) nothrow @safe          { __r_.first().__s.__size_ = cast(ubyte)(__s); }
                    size_type __get_short_size() const nothrow @safe            { return __r_.first().__s.__size_; }
                }
                else
                {
                    void __set_short_size(size_type __s) nothrow @safe          { __r_.first().__s.__size_ = cast(ubyte)(__s << 1); }
                    size_type __get_short_size() const nothrow @safe            { return __r_.first().__s.__size_ >> 1; }
                }
            }
            void __set_long_size(size_type __s) nothrow                         { __r_.first().__l.__size_ = __s; }
            size_type __get_long_size() const nothrow @trusted                  { return __r_.first().__l.__size_; }
            void __set_size(size_type __s) nothrow                              { if (__is_long()) __set_long_size(__s); else __set_short_size(__s); }

            void __set_long_cap(size_type __s) nothrow                          { __r_.first().__l.__cap_  = __long_mask | __s; }
            size_type __get_long_cap() const nothrow                            { return __r_.first().__l.__cap_ & size_type(~__long_mask); }

            void __set_long_pointer(pointer __p) nothrow                        { __r_.first().__l.__data_ = __p; }
            inout(T)* __get_long_pointer() inout nothrow                        { return __r_.first().__l.__data_; }
            inout(T)* __get_short_pointer() inout nothrow @safe                 { return &__r_.first().__s.__data_[0]; }
            inout(T)* __get_pointer() inout nothrow                             { return __is_long() ? __get_long_pointer() : __get_short_pointer(); }

            bool __is_long() const nothrow @safe                                { return (__r_.first().__s.__size_ & __short_mask) != 0; }

            void __zero() nothrow @safe                                         { __r_.first().__r.__words[] = 0; }

            ref inout(allocator_type) __alloc() inout nothrow @safe             { return __r_.second(); }

            void __init(const(value_type)* __s, size_type __sz)                 { return __init(__s, __sz, __sz); }
        }

        void __assign_allocator(ref const(allocator_type) al) nothrow
        {
            static if (!__r_.Ty2Empty)
                __alloc() = al;
        }

        void __init(const(value_type)* __s, size_type __sz, size_type __reserve)
        {
            assert(__reserve <= max_size());
//            if (__reserve > max_size())
//                throw new RangeError("Length exceeds `max_size()`"); // this->__throw_length_error();
            pointer __p;
            if (__reserve < __min_cap)
            {
                __set_short_size(__sz);
                __p = __get_short_pointer();
            }
            else
            {
                size_type __cap = __recommend(__reserve);
                __p = __alloc().allocate(__cap+1, null);
                __set_long_pointer(__p);
                __set_long_cap(__cap+1);
                __set_long_size(__sz);
            }
            __p[0 .. __sz] = __s[0 .. __sz];
            __p[__sz] = value_type(0);
        }

        static size_type __recommend(size_type __s) nothrow @safe
        {
            static size_type __align_it(size_type __a)(size_type __s) nothrow @safe { return (__s + (__a-1)) & ~(__a-1); }
            if (__s < __min_cap) return __min_cap - 1;
            size_type __guess = __align_it!(value_type.sizeof < __alignment ? __alignment/value_type.sizeof : 1)(__s+1) - 1;
            if (__guess == __min_cap) ++__guess;
            return __guess;
        }

        void __grow_by_and_replace(size_type __old_cap, size_type __delta_cap, size_type __old_sz, size_type __n_copy,
                                 size_type __n_del, size_type __n_add, const(value_type)* __p_new_stuff)
        {
            size_type __ms = max_size();
            assert(__delta_cap <= __ms - __old_cap - 1);
//            if (__delta_cap > __ms - __old_cap - 1)
//                throw new RangeError("Length exceeds `max_size()`"); // this->__throw_length_error();
            pointer __old_p = __get_pointer();
            size_type __cap = __old_cap < __ms / 2 - __alignment ?
                __recommend(max(__old_cap + __delta_cap, 2 * __old_cap)) :
            __ms - 1;
            pointer __p = __alloc().allocate(__cap+1);
//            __invalidate_all_iterators(); // TODO: support `_LIBCPP_DEBUG_LEVEL >= 2` ??
            if (__n_copy != 0)
                __p[0 .. __n_copy] = __old_p[0 .. __n_copy];
            if (__n_add != 0)
                (__p + __n_copy)[0 .. __n_add] = __p_new_stuff[0 .. __n_add];
            size_type __sec_cp_sz = __old_sz - __n_del - __n_copy;
            if (__sec_cp_sz != 0)
                (__p + __n_copy + __n_add)[0 .. __sec_cp_sz] = (__old_p + __n_copy + __n_del)[0 .. __sec_cp_sz];
            if (__old_cap+1 != __min_cap)
                __alloc().deallocate(__old_p, __old_cap+1);
            __set_long_pointer(__p);
            __set_long_cap(__cap+1);
            __old_sz = __n_copy + __n_add + __sec_cp_sz;
            __set_long_size(__old_sz);
            __p[__old_sz] = value_type(0);
        }

        void __grow_by(size_type __old_cap, size_type __delta_cap, size_type __old_sz,
                        size_type __n_copy,  size_type __n_del, size_type __n_add = 0)
        {
            size_type __ms = max_size();
            assert(__delta_cap <= __ms - __old_cap);
//            if (__delta_cap > __ms - __old_cap)
//                __throw_length_error();
            pointer __old_p = __get_pointer();
            size_type __cap = __old_cap < __ms / 2 - __alignment ?
                                  __recommend(max(__old_cap + __delta_cap, 2 * __old_cap)) :
                                  __ms - 1;
            pointer __p = __alloc().allocate(__cap+1);
//            __invalidate_all_iterators(); // TODO:
            if (__n_copy != 0)
                __p[0 .. __n_copy] = __old_p[0 .. __n_copy];
            size_type __sec_cp_sz = __old_sz - __n_del - __n_copy;
            if (__sec_cp_sz != 0)
                (__p + __n_copy + __n_add)[0 .. __sec_cp_sz] = (__old_p + __n_copy + __n_del)[0 .. __sec_cp_sz];
            if (__old_cap+1 != __min_cap)
                __alloc().deallocate(__old_p, __old_cap+1);
            __set_long_pointer(__p);
            __set_long_cap(__cap+1);
        }
    }
    else
    {
        static assert(false, "C++ runtime not supported");
    }
}


// platform detail
private:
version (CppRuntime_Microsoft)
{
    import core.stdcpp.xutility : _ITERATOR_DEBUG_LEVEL;

extern(C++, (StdNamespace)):
    extern (C++) struct _String_base_types(_Elem, _Alloc)
    {
        alias Ty = _Elem;
        alias Alloc = _Alloc;
    }

    extern (C++, class) struct _String_alloc(_Alloc_types)
    {
        import core.stdcpp.xutility : _Compressed_pair;

        alias Ty = _Alloc_types.Ty;
        alias Alloc = _Alloc_types.Alloc;
        alias ValTy = _String_val!Ty;

    extern(D) @safe @nogc:
        pragma(inline, true)
        {
            ref inout(Alloc) _Getal() return inout pure nothrow { return _Mypair._Myval1; }
            ref inout(ValTy) _Get_data() return inout pure nothrow { return _Mypair._Myval2; }
        }

        void _Orphan_all() nothrow { _Get_data._Base._Orphan_all(); }

        static if (_ITERATOR_DEBUG_LEVEL > 0)
        {
            import core.stdcpp.xutility : _Container_proxy;

            ~this()
            {
                _Free_proxy();
            }

            pragma(inline, true)
            ref inout(_Container_proxy*) _Myproxy() inout pure nothrow { return _Get_data._Base._Myproxy; }

            void _Alloc_proxy() nothrow @trusted
            {
                import core.lifetime : emplace;

                alias _Alproxy = Alloc.rebind!_Container_proxy;
                try // TODO: or should we make allocator<T>::allocate() `nothrow`?
                    _Myproxy() = _Alproxy(_Getal()).allocate(1);
                catch (Throwable)
                    assert(false, "Failed to allocate iterator debug container proxy");
                emplace!_Container_proxy(_Myproxy());
                _Myproxy()._Mycont = &_Get_data()._Base;
            }
            void _Free_proxy() nothrow @trusted
            {
                alias _Alproxy = Alloc.rebind!_Container_proxy;
                _Orphan_all();
                destroy!false(*_Myproxy());
                try // TODO: or should we make allocator<T>::deallocate() `nothrow`?
                    _Alproxy(_Getal()).deallocate(_Myproxy(), 1);
                catch (Throwable)
                    assert(false, "Failed to deallocate iterator debug container proxy");
                _Myproxy() = null;
            }
        }

        _Compressed_pair!(Alloc, ValTy) _Mypair;
    }

    extern (C++, class) struct _String_val(T)
    {
        import core.stdcpp.xutility : _Container_base;
        import core.stdcpp.type_traits : is_empty;

        enum _BUF_SIZE = 16 / T.sizeof < 1 ? 1 : 16 / T.sizeof;
        enum _ALLOC_MASK = T.sizeof <= 1 ? 15 : T.sizeof <= 2 ? 7 : T.sizeof <= 4 ? 3 : T.sizeof <= 8 ? 1 : 0;

        static if (!is_empty!_Container_base.value)
            _Container_base _Base;
        else
            ref inout(_Container_base) _Base() inout { return *cast(inout(_Container_base)*)&this; }

        union _Bxty
        {
            T[_BUF_SIZE] _Buf;
            T* _Ptr;
        }

        _Bxty _Bx;
        size_t _Mysize = 0;             // current length of string
        size_t _Myres = _BUF_SIZE - 1;  // current storage reserved for string

    pragma (inline, true):
    extern (D):
    pure nothrow @nogc:
        bool _IsAllocated() const @safe                 { return _BUF_SIZE <= _Myres; }
        alias _Large_string_engaged = _IsAllocated;
        @property inout(T)* _Myptr() inout @trusted     { return _BUF_SIZE <= _Myres ? _Bx._Ptr : _Bx._Buf.ptr; }
        @property inout(T)[] _Mystr() inout @trusted    { return _BUF_SIZE <= _Myres ? _Bx._Ptr[0 .. _Mysize] : _Bx._Buf[0 .. _Mysize]; }

        auto _Clamp_suffix_size(T)(const T _Off, const T _Size) const
        {
            // trims _Size to the longest it can be assuming a string at/after _Off
            return min(_Size, _Mysize - _Off);
        }
    }

    template _Size_after_ebco_v(_Ty)
    {
        import core.stdcpp.type_traits : is_empty;

        enum size_t _Size_after_ebco_v = is_empty!_Ty.value ? 0 : _Ty.sizeof; // get _Ty's size after being EBCO'd
    }
}

auto ref T max(T)(auto ref T a, auto ref T b) { return b > a ? b : a; }
auto ref T min(T)(auto ref T a, auto ref T b) { return b < a ? b : a; }
