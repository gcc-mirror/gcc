/**
 * D header file for interaction with C++ std::vector.
 *
 * Copyright: Copyright (c) 2018 D Language Foundation
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Guillaume Chatelet
 *            Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/vector.d)
 */

module core.stdcpp.vector;

///////////////////////////////////////////////////////////////////////////////
// std::vector declaration.
//
// Current caveats :
// - missing noexcept
// - nothrow @trusted @nogc for most functions depend on knowledge
//   of T's construction/destruction/assignment semantics
///////////////////////////////////////////////////////////////////////////////

import core.stdcpp.allocator;

enum DefaultConstruct { value }

/// Constructor argument for default construction
enum Default = DefaultConstruct();

extern(C++, "std"):

alias vector(T) = vector!(T, allocator!T);
extern(C++, class) struct vector(T, Alloc)
{
    import core.lifetime : forward, move, core_emplace = emplace;

    static assert(!is(T == bool), "vector!bool not supported!");
extern(D):

    ///
    alias size_type = size_t;
    ///
    alias difference_type = ptrdiff_t;
    ///
    alias value_type = T;
    ///
    alias allocator_type = Alloc;
    ///
    alias pointer = T*;
    ///
    alias const_pointer = const(T)*;

    /// MSVC allocates on default initialisation in debug, which can't be modelled by D `struct`
    @disable this();

    ///
    alias length = size;
    ///
    alias opDollar = length;

    ///
    size_t[2] opSlice(size_t dim : 0)(size_t start, size_t end) const pure nothrow @safe @nogc { return [start, end]; }

    ///
    ref inout(T) opIndex(size_t index) inout pure nothrow @safe @nogc       { return as_array[index]; }
    ///
    inout(T)[] opIndex(size_t[2] slice) inout pure nothrow @safe @nogc      { return as_array[slice[0] .. slice[1]]; }
    ///
    inout(T)[] opIndex() inout pure nothrow @safe @nogc                     { return as_array(); }

    ///
    ref vector opAssign(U)(auto ref vector!(U, Alloc) s)                    { opAssign(s.as_array); return this; }
    ///
    ref vector opAssign(T[] array)
    {
        clear();
        reserve(array.length);
        insert(0, array);
        return this;
    }

    ///
    void opIndexAssign()(auto ref T val, size_t index)                      { as_array[index] = val; }
    ///
    void opIndexAssign()(auto ref T val, size_t[2] slice)                   { as_array[slice[0] .. slice[1]] = val; }
    ///
    void opIndexAssign(T[] val, size_t[2] slice)                            { as_array[slice[0] .. slice[1]] = val[]; }
    ///
    void opIndexAssign()(auto ref T val)                                    { as_array[] = val; }
    ///
    void opIndexAssign(T[] val)                                             { as_array[] = val[]; }

    ///
    void opIndexOpAssign(string op)(auto ref T val, size_t index)           { mixin("as_array[index] " ~ op ~ "= val;"); }
    ///
    void opIndexOpAssign(string op)(auto ref T val, size_t[2] slice)        { mixin("as_array[slice[0] .. slice[1]] " ~ op ~ "= val;"); }
    ///
    void opIndexOpAssign(string op)(T[] val, size_t[2] slice)               { mixin("as_array[slice[0] .. slice[1]] " ~ op ~ "= val[];"); }
    ///
    void opIndexOpAssign(string op)(auto ref T val)                         { mixin("as_array[] " ~ op ~ "= val;"); }
    ///
    void opIndexOpAssign(string op)(T[] val)                                { mixin("as_array[] " ~ op ~ "= val[];"); }

    ///
    ref inout(T) front() inout pure nothrow @safe @nogc                     { return as_array[0]; }
    ///
    ref inout(T) back() inout pure nothrow @safe @nogc                      { return as_array[$-1]; }

    ///
    ref vector opOpAssign(string op : "~")(auto ref T item)                 { push_back(forward!item); return this; }
    ///
    ref vector opOpAssign(string op : "~")(T[] array)                       { insert(length, array); return this; }

    ///
    void append(T[] array)                                                  { insert(length, array); }

    /// Performs elementwise equality check.
    bool opEquals(this This, That)(auto ref That rhs)
    if (is(immutable That == immutable vector))                             { return as_array == rhs.as_array; }

    /// Performs lexicographical comparison.
    static if (is(typeof((ref T a, ref T b) => a < b)))
    int opCmp(this This, That)(auto ref That rhs)
    if (is(immutable That == immutable vector))                             { return __cmp(as_array, rhs.as_array); }

    /// Hash to allow `vector`s to be used as keys for built-in associative arrays.
    /// **The result will generally not be the same as C++ `std::hash<std::vector<T>>`.**
    size_t toHash() const                                                   { return .hashOf(as_array); }

    // Modifiers
    ///
    void push_back(U)(auto ref U element)
    {
        emplace_back(forward!element);
    }

    version (CppRuntime_Microsoft)
    {
        //----------------------------------------------------------------------------------
        // Microsoft runtime
        //----------------------------------------------------------------------------------

        ///
        this(DefaultConstruct) @nogc                                        { _Alloc_proxy(); }
        ///
        this()(size_t count)
        {
            _Alloc_proxy();
            _Buy(count);
            scope(failure) _Tidy();
            _Get_data()._Mylast = _Udefault(_Get_data()._Myfirst, count);
        }
        ///
        this()(size_t count, auto ref T val)
        {
            _Alloc_proxy();
            _Buy(count);
            scope(failure) _Tidy();
            _Get_data()._Mylast = _Ufill(_Get_data()._Myfirst, count, val);
        }
        ///
        this()(T[] array)
        {
            _Alloc_proxy();
            _Buy(array.length);
            scope(failure) _Tidy();
            _Get_data()._Mylast = _Utransfer!false(array.ptr, array.ptr + array.length, _Get_data()._Myfirst);
        }
        ///
        this(this)
        {
            _Alloc_proxy();
            pointer _First = _Get_data()._Myfirst;
            pointer _Last = _Get_data()._Mylast;
            _Buy(_Last - _First);
            scope(failure) _Tidy();
            _Get_data()._Mylast = _Utransfer!false(_First, _Last, _Get_data()._Myfirst);
        }

        ///
        ~this()                                                             { _Tidy(); }

        ///
        ref inout(Alloc) get_allocator() inout pure nothrow @safe @nogc     { return _Getal(); }

        ///
        size_type max_size() const pure nothrow @safe @nogc                 { return ((size_t.max / T.sizeof) - 1) / 2; } // HACK: clone the windows version precisely?

        ///
        size_type size() const pure nothrow @safe @nogc                     { return _Get_data()._Mylast - _Get_data()._Myfirst; }
        ///
        size_type capacity() const pure nothrow @safe @nogc                 { return _Get_data()._Myend - _Get_data()._Myfirst; }
        ///
        bool empty() const pure nothrow @safe @nogc                         { return _Get_data()._Myfirst == _Get_data()._Mylast; }
        ///
        inout(T)* data() inout pure nothrow @safe @nogc                     { return _Get_data()._Myfirst; }
        ///
        inout(T)[] as_array() inout pure nothrow @trusted @nogc             { return _Get_data()._Myfirst[0 .. size()]; }
        ///
        ref inout(T) at(size_type i) inout pure nothrow @trusted @nogc      { return _Get_data()._Myfirst[0 .. size()][i]; }

        ///
        ref T emplace_back(Args...)(auto ref Args args)
        {
            if (_Has_unused_capacity())
                return _Emplace_back_with_unused_capacity(forward!args);
            return *_Emplace_reallocate(_Get_data()._Mylast, forward!args);
        }

        ///
        void reserve(const size_type newCapacity)
        {
            if (newCapacity > capacity())
            {
//                if (newCapacity > max_size())
//                    _Xlength();
                _Reallocate_exactly(newCapacity);
            }
        }

        ///
        void shrink_to_fit()
        {
            if (_Has_unused_capacity())
            {
                if (empty())
                    _Tidy();
                else
                    _Reallocate_exactly(size());
            }
        }

        ///
        void pop_back()
        {
            static if (_ITERATOR_DEBUG_LEVEL == 2)
            {
                assert(!empty(), "vector empty before pop");
                _Orphan_range(_Get_data()._Mylast - 1, _Get_data()._Mylast);
            }
            destroy!false(_Get_data()._Mylast[-1]);
            --_Get_data()._Mylast;
        }

        ///
        void clear()
        {
            _Base._Orphan_all();
            _Destroy(_Get_data()._Myfirst, _Get_data()._Mylast);
            _Get_data()._Mylast = _Get_data()._Myfirst;
        }

        ///
        void resize()(const size_type newsize)
        {
            static assert(is(typeof({static T i;})), T.stringof ~ ".this() is annotated with @disable.");
            _Resize(newsize, (pointer _Dest, size_type _Count) => _Udefault(_Dest, _Count));
        }

        ///
        void resize()(const size_type newsize, auto ref T val)
        {
            _Resize(newsize, (pointer _Dest, size_type _Count) => _Ufill(_Dest, _Count, forward!val));
        }

        void emplace(Args...)(size_t offset, auto ref Args args)
        {
            pointer _Whereptr = _Get_data()._Myfirst + offset;
            pointer _Oldlast = _Get_data()._Mylast;
            if (_Has_unused_capacity())
            {
                if (_Whereptr == _Oldlast)
                    _Emplace_back_with_unused_capacity(forward!args);
                else
                {
                    T _Obj = T(forward!args);
                    static if (_ITERATOR_DEBUG_LEVEL == 2)
                        _Orphan_range(_Whereptr, _Oldlast);
                    move(_Oldlast[-1], *_Oldlast);
                    ++_Get_data()._Mylast;
                    _Move_backward_unchecked(_Whereptr, _Oldlast - 1, _Oldlast);
                    move(_Obj, *_Whereptr);
                }
                return;
            }
            _Emplace_reallocate(_Whereptr, forward!args);
        }

        ///
        void insert(size_t offset, T[] array)
        {
            pointer _Where = _Get_data()._Myfirst + offset;
            pointer _First = array.ptr;
            pointer _Last = _First + array.length;

            const size_type _Count = array.length;
            const size_type _Whereoff = offset;
            const bool _One_at_back = _Count == 1 && _Get_data()._Myfirst + _Whereoff == _Get_data()._Mylast;

            if (_Count == 0)
            {
                // nothing to do, avoid invalidating iterators
            }
            else if (_Count > _Unused_capacity())
            {   // reallocate
                const size_type _Oldsize = size();

//                if (_Count > max_size() - _Oldsize)
//                    _Xlength();

                const size_type _Newsize = _Oldsize + _Count;
                const size_type _Newcapacity = _Calculate_growth(_Newsize);

                pointer _Newvec = _Getal().allocate(_Newcapacity);
                pointer _Constructed_last = _Newvec + _Whereoff + _Count;
                pointer _Constructed_first = _Constructed_last;

                try
                {
                    _Utransfer!false(_First, _Last, _Newvec + _Whereoff);
                    _Constructed_first = _Newvec + _Whereoff;

                    if (_One_at_back)
                    {
                        _Utransfer!(true, true)(_Get_data()._Myfirst, _Get_data()._Mylast, _Newvec);
                    }
                    else
                    {
                        _Utransfer!true(_Get_data()._Myfirst, _Where, _Newvec);
                        _Constructed_first = _Newvec;
                        _Utransfer!true(_Where, _Get_data()._Mylast, _Newvec + _Whereoff + _Count);
                    }
                }
                catch (Throwable e)
                {
                    _Destroy(_Constructed_first, _Constructed_last);
                    _Getal().deallocate(_Newvec, _Newcapacity);
                    throw e;
                }

                _Change_array(_Newvec, _Newsize, _Newcapacity);
            }
            else
            {   // Attempt to provide the strong guarantee for EmplaceConstructible failure.
                // If we encounter copy/move construction/assignment failure, provide the basic guarantee.
                // (For one-at-back, this provides the strong guarantee.)

                pointer _Oldlast = _Get_data()._Mylast;
                const size_type _Affected_elements = cast(size_type)(_Oldlast - _Where);

                if (_Count < _Affected_elements)
                {    // some affected elements must be assigned
                    _Get_data()._Mylast = _Utransfer!true(_Oldlast - _Count, _Oldlast, _Oldlast);
                    _Move_backward_unchecked(_Where, _Oldlast - _Count, _Oldlast);
                    _Destroy(_Where, _Where + _Count);

                    try
                    {
                        _Utransfer!false(_First, _Last, _Where);
                    }
                    catch (Throwable e)
                    {
                        // glue the broken pieces back together
                        try
                        {
                            _Utransfer!true(_Where + _Count, _Where + 2 * _Count, _Where);
                        }
                        catch (Throwable e)
                        {
                            // vaporize the detached piece
                            static if (_ITERATOR_DEBUG_LEVEL == 2)
                                _Orphan_range(_Where, _Oldlast);
                            _Destroy(_Where + _Count, _Get_data()._Mylast);
                            _Get_data()._Mylast = _Where;
                            throw e;
                        }

                        _Move_unchecked(_Where + 2 * _Count, _Get_data()._Mylast, _Where + _Count);
                        _Destroy(_Oldlast, _Get_data()._Mylast);
                        _Get_data()._Mylast = _Oldlast;
                        throw e;
                    }
                }
                else
                {   // affected elements don't overlap before/after
                    pointer _Relocated = _Where + _Count;
                    _Get_data()._Mylast = _Utransfer!true(_Where, _Oldlast, _Relocated);
                    _Destroy(_Where, _Oldlast);

                    try
                    {
                        _Utransfer!false(_First, _Last, _Where);
                    }
                    catch (Throwable e)
                    {
                        // glue the broken pieces back together
                        try
                        {
                            _Utransfer!true(_Relocated, _Get_data()._Mylast, _Where);
                        }
                        catch (Throwable e)
                        {
                            // vaporize the detached piece
                            static if (_ITERATOR_DEBUG_LEVEL == 2)
                                _Orphan_range(_Where, _Oldlast);
                            _Destroy(_Relocated, _Get_data()._Mylast);
                            _Get_data()._Mylast = _Where;
                            throw e;
                        }

                        _Destroy(_Relocated, _Get_data()._Mylast);
                        _Get_data()._Mylast = _Oldlast;
                        throw e;
                    }
                }
                static if (_ITERATOR_DEBUG_LEVEL == 2)
                    _Orphan_range(_Where, _Oldlast);
            }
        }

    private:
        import core.stdcpp.xutility : MSVCLinkDirectives;

        // Make sure the object files wont link against mismatching objects
        mixin MSVCLinkDirectives!true;

        pragma(inline, true)
        {
            ref inout(_Base.Alloc) _Getal() inout pure nothrow @safe @nogc       { return _Base._Mypair._Myval1; }
            ref inout(_Base.ValTy) _Get_data() inout pure nothrow @safe @nogc    { return _Base._Mypair._Myval2; }
        }

        void _Alloc_proxy() @nogc
        {
            static if (_ITERATOR_DEBUG_LEVEL > 0)
                _Base._Alloc_proxy();
        }

        void _AssignAllocator(ref const(allocator_type) al) nothrow @nogc
        {
            static if (_Base._Mypair._HasFirst)
                _Getal() = al;
        }

        bool _Buy(size_type _Newcapacity) @trusted @nogc
        {
            _Get_data()._Myfirst = null;
            _Get_data()._Mylast = null;
            _Get_data()._Myend = null;

            if (_Newcapacity == 0)
                return false;

            // TODO: how to handle this in D? kinda like a range exception...
//            if (_Newcapacity > max_size())
//                _Xlength();

            _Get_data()._Myfirst = _Getal().allocate(_Newcapacity);
            _Get_data()._Mylast = _Get_data()._Myfirst;
            _Get_data()._Myend = _Get_data()._Myfirst + _Newcapacity;

            return true;
        }

        static void _Destroy(pointer _First, pointer _Last)
        {
            for (; _First != _Last; ++_First)
                destroy!false(*_First);
        }

        void _Tidy()
        {
            _Base._Orphan_all();
            if (_Get_data()._Myfirst)
            {
                _Destroy(_Get_data()._Myfirst, _Get_data()._Mylast);
                _Getal().deallocate(_Get_data()._Myfirst, capacity());
                _Get_data()._Myfirst = null;
                _Get_data()._Mylast = null;
                _Get_data()._Myend = null;
            }
        }

        size_type _Unused_capacity() const pure nothrow @safe @nogc
        {
            return _Get_data()._Myend - _Get_data()._Mylast;
        }

        bool _Has_unused_capacity() const pure nothrow @safe @nogc
        {
            return _Get_data()._Myend != _Get_data()._Mylast;
        }

        ref T _Emplace_back_with_unused_capacity(Args...)(auto ref Args args)
        {
            core_emplace(_Get_data()._Mylast, forward!args);
            static if (_ITERATOR_DEBUG_LEVEL == 2)
                _Orphan_range(_Get_data()._Mylast, _Get_data()._Mylast);
            return *_Get_data()._Mylast++;
        }

        pointer _Emplace_reallocate(_Valty...)(pointer _Whereptr, auto ref _Valty _Val)
        {
            const size_type _Whereoff = _Whereptr - _Get_data()._Myfirst;
            const size_type _Oldsize = size();

            // TODO: what should we do in D? kinda like a range overflow?
//            if (_Oldsize == max_size())
//                _Xlength();

            const size_type _Newsize = _Oldsize + 1;
            const size_type _Newcapacity = _Calculate_growth(_Newsize);

            pointer _Newvec = _Getal().allocate(_Newcapacity);
            pointer _Constructed_last = _Newvec + _Whereoff + 1;
            pointer _Constructed_first = _Constructed_last;

            try
            {
                core_emplace(_Newvec + _Whereoff, forward!_Val);
                _Constructed_first = _Newvec + _Whereoff;
                if (_Whereptr == _Get_data()._Mylast)
                    _Utransfer!(true, true)(_Get_data()._Myfirst, _Get_data()._Mylast, _Newvec);
                else
                {
                    _Utransfer!true(_Get_data()._Myfirst, _Whereptr, _Newvec);
                    _Constructed_first = _Newvec;
                    _Utransfer!true(_Whereptr, _Get_data()._Mylast, _Newvec + _Whereoff + 1);
                }
            }
            catch (Throwable e)
            {
                _Destroy(_Constructed_first, _Constructed_last);
                _Getal().deallocate(_Newvec, _Newcapacity);
                throw e;
            }

            _Change_array(_Newvec, _Newsize, _Newcapacity);
            return _Get_data()._Myfirst + _Whereoff;
        }

        void _Resize(_Lambda)(const size_type _Newsize, _Lambda _Udefault_or_fill)
        {
            const size_type _Oldsize = size();
            const size_type _Oldcapacity = capacity();

            if (_Newsize > _Oldcapacity)
            {
//                if (_Newsize > max_size())
//                    _Xlength();

                const size_type _Newcapacity = _Calculate_growth(_Newsize);

                pointer _Newvec = _Getal().allocate(_Newcapacity);
                pointer _Appended_first = _Newvec + _Oldsize;
                pointer _Appended_last = _Appended_first;

                try
                {
                    _Appended_last = _Udefault_or_fill(_Appended_first, _Newsize - _Oldsize);
                    _Utransfer!(true, true)(_Get_data()._Myfirst, _Get_data()._Mylast, _Newvec);
                }
                catch (Throwable e)
                {
                    _Destroy(_Appended_first, _Appended_last);
                    _Getal().deallocate(_Newvec, _Newcapacity);
                    throw e;
                }
                _Change_array(_Newvec, _Newsize, _Newcapacity);
            }
            else if (_Newsize > _Oldsize)
            {
                pointer _Oldlast = _Get_data()._Mylast;
                _Get_data()._Mylast = _Udefault_or_fill(_Oldlast, _Newsize - _Oldsize);
                static if (_ITERATOR_DEBUG_LEVEL == 2)
                    _Orphan_range(_Oldlast, _Oldlast);
            }
            else if (_Newsize == _Oldsize)
            {
                // nothing to do, avoid invalidating iterators
            }
            else
            {
                pointer _Newlast = _Get_data()._Myfirst + _Newsize;
                static if (_ITERATOR_DEBUG_LEVEL == 2)
                    _Orphan_range(_Newlast, _Get_data()._Mylast);
                _Destroy(_Newlast, _Get_data()._Mylast);
                _Get_data()._Mylast = _Newlast;
            }
        }

        void _Reallocate_exactly(const size_type _Newcapacity)
        {
            import core.lifetime : moveEmplace;

            const size_type _Size = size();
            pointer _Newvec = _Getal().allocate(_Newcapacity);

            try
            {
                for (size_t i = _Size; i > 0; )
                {
                    --i;
                    moveEmplace(_Get_data()._Myfirst[i], _Newvec[i]);
                }
            }
            catch (Throwable e)
            {
                _Getal().deallocate(_Newvec, _Newcapacity);
                throw e;
            }

            _Change_array(_Newvec, _Size, _Newcapacity);
        }

        void _Change_array(pointer _Newvec, const size_type _Newsize, const size_type _Newcapacity)
        {
            _Base._Orphan_all();

            if (_Get_data()._Myfirst != null)
            {
                _Destroy(_Get_data()._Myfirst, _Get_data()._Mylast);
                _Getal().deallocate(_Get_data()._Myfirst, capacity());
            }

            _Get_data()._Myfirst = _Newvec;
            _Get_data()._Mylast = _Newvec + _Newsize;
            _Get_data()._Myend = _Newvec + _Newcapacity;
        }

        size_type _Calculate_growth(const size_type _Newsize) const pure nothrow @nogc @safe
        {
            const size_type _Oldcapacity = capacity();
            if (_Oldcapacity > max_size() - _Oldcapacity/2)
                return _Newsize;
            const size_type _Geometric = _Oldcapacity + _Oldcapacity/2;
            if (_Geometric < _Newsize)
                return _Newsize;
            return _Geometric;
        }

        struct _Uninitialized_backout
        {
            this() @disable;
            this(pointer _Dest)
            {
                _First = _Dest;
                _Last = _Dest;
            }
            ~this()
            {
                _Destroy(_First, _Last);
            }
            void _Emplace_back(Args...)(auto ref Args args)
            {
                core_emplace(_Last, forward!args);
                ++_Last;
            }
            pointer _Release()
            {
                _First = _Last;
                return _Last;
            }
        private:
            pointer _First;
            pointer _Last;
        }
        pointer _Utransfer(bool _move, bool _ifNothrow = false)(pointer _First, pointer _Last, pointer _Dest)
        {
            // TODO: if copy/move are trivial, then we can memcpy/memmove
            auto _Backout = _Uninitialized_backout(_Dest);
            for (; _First != _Last; ++_First)
            {
                static if (_move && (!_ifNothrow || true)) // isNothrow!T (move in D is always nothrow! ...until opPostMove)
                    _Backout._Emplace_back(move(*_First));
                else
                    _Backout._Emplace_back(*_First);
            }
            return _Backout._Release();
        }
        pointer _Ufill()(pointer _Dest, size_t _Count, auto ref T val)
        {
            // TODO: if T.sizeof == 1 and no elaborate constructor, fast-path to memset
            // TODO: if copy ctor/postblit are nothrow, just range assign
            auto _Backout = _Uninitialized_backout(_Dest);
            for (; 0 < _Count; --_Count)
                _Backout._Emplace_back(val);
            return _Backout._Release();
        }
        pointer _Udefault()(pointer _Dest, size_t _Count)
        {
            // TODO: if zero init, then fast-path to zeromem
            auto _Backout = _Uninitialized_backout(_Dest);
            for (; 0 < _Count; --_Count)
                _Backout._Emplace_back();
            return _Backout._Release();
        }
        pointer _Move_unchecked(pointer _First, pointer _Last, pointer _Dest)
        {
            // TODO: can `memmove` if conditions are right...
            for (; _First != _Last; ++_Dest, ++_First)
                move(*_First, *_Dest);
            return _Dest;
        }
        pointer _Move_backward_unchecked(pointer _First, pointer _Last, pointer _Dest)
        {
            while (_First != _Last)
                move(*--_Last, *--_Dest);
            return _Dest;
        }

        static if (_ITERATOR_DEBUG_LEVEL == 2)
        {
            void _Orphan_range(pointer _First, pointer _Last) const @nogc
            {
                import core.stdcpp.xutility : _Lockit, _LOCK_DEBUG;

                alias const_iterator = _Base.const_iterator;
                auto _Lock = _Lockit(_LOCK_DEBUG);

                const_iterator** _Pnext = cast(const_iterator**)_Get_data()._Base._Getpfirst();
                if (!_Pnext)
                    return;

                while (*_Pnext)
                {
                    if ((*_Pnext)._Ptr < _First || _Last < (*_Pnext)._Ptr)
                    {
                        _Pnext = cast(const_iterator**)(*_Pnext)._Base._Getpnext();
                    }
                    else
                    {
                        (*_Pnext)._Base._Clrcont();
                        *_Pnext = *cast(const_iterator**)(*_Pnext)._Base._Getpnext();
                    }
                }
            }
        }

        _Vector_alloc!(_Vec_base_types!(T, Alloc)) _Base;
    }
    else version (None)
    {
        size_type size() const pure nothrow @safe @nogc                     { return 0; }
        size_type capacity() const pure nothrow @safe @nogc                 { return 0; }
        bool empty() const pure nothrow @safe @nogc                         { return true; }

        inout(T)* data() inout pure nothrow @safe @nogc                     { return null; }
        inout(T)[] as_array() inout pure nothrow @trusted @nogc             { return null; }
        ref inout(T) at(size_type i) inout pure nothrow @trusted @nogc      { data()[0]; }
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

    extern (C++, struct) struct _Vec_base_types(_Ty, _Alloc0)
    {
        alias Ty = _Ty;
        alias Alloc = _Alloc0;
    }

    extern (C++, class) struct _Vector_alloc(_Alloc_types)
    {
        import core.stdcpp.xutility : _Compressed_pair;
    extern(D):
    @nogc:

        alias Ty = _Alloc_types.Ty;
        alias Alloc = _Alloc_types.Alloc;
        alias ValTy = _Vector_val!Ty;

        void _Orphan_all() nothrow @safe
        {
            static if (is(typeof(ValTy._Base)))
                _Mypair._Myval2._Base._Orphan_all();
        }

        static if (_ITERATOR_DEBUG_LEVEL != 0)
        {
            import core.stdcpp.xutility : _Container_proxy;

            alias const_iterator = _Vector_const_iterator!(ValTy);

            ~this()
            {
                _Free_proxy();
            }

            void _Alloc_proxy() @trusted
            {
                import core.lifetime : emplace;

                alias _Alproxy = Alloc.rebind!_Container_proxy;
                _Alproxy _Proxy_allocator = _Alproxy(_Mypair._Myval1);
                _Mypair._Myval2._Base._Myproxy = _Proxy_allocator.allocate(1);
                emplace(_Mypair._Myval2._Base._Myproxy);
                _Mypair._Myval2._Base._Myproxy._Mycont = &_Mypair._Myval2._Base;
            }
            void _Free_proxy()
            {
                alias _Alproxy = Alloc.rebind!_Container_proxy;
                _Alproxy _Proxy_allocator = _Alproxy(_Mypair._Myval1);
                _Orphan_all();
                destroy!false(_Mypair._Myval2._Base._Myproxy);
                _Proxy_allocator.deallocate(_Mypair._Myval2._Base._Myproxy, 1);
                _Mypair._Myval2._Base._Myproxy = null;
            }
        }

        _Compressed_pair!(Alloc, ValTy) _Mypair;
    }

    extern (C++, class) struct _Vector_val(T)
    {
        import core.stdcpp.xutility : _Container_base;
        import core.stdcpp.type_traits : is_empty;

        alias pointer = T*;

        static if (!is_empty!_Container_base.value)
            _Container_base _Base;

        pointer _Myfirst;   // pointer to beginning of array
        pointer _Mylast;    // pointer to current end of sequence
        pointer _Myend;     // pointer to end of array
    }

    static if (_ITERATOR_DEBUG_LEVEL > 0)
    {
        extern (C++, class) struct _Vector_const_iterator(_Myvec)
        {
            import core.stdcpp.xutility : _Iterator_base;
            import core.stdcpp.type_traits : is_empty;

            static if (!is_empty!_Iterator_base.value)
                _Iterator_base _Base;
            _Myvec.pointer _Ptr;
        }
    }
}
