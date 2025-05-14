/**
 * $(SCRIPT inhibitQuickIndex = 1;)
 * $(DIVC quickindex,
 * $(BOOKTABLE,
 * $(TR $(TH Category) $(TH Symbols))
 * $(TR $(TD Arrays) $(TD
 *     $(MYREF assumeSafeAppend)
 *     $(MYREF capacity)
 *     $(A #.dup.2, $(TT dup))
 *     $(MYREF idup)
 *     $(MYREF reserve)
 * ))
 * $(TR $(TD Associative arrays) $(TD
 *     $(MYREF byKey)
 *     $(MYREF byKeyValue)
 *     $(MYREF byValue)
 *     $(MYREF clear)
 *     $(MYREF dup)
 *     $(MYREF get)
 *     $(MYREF keys)
 *     $(MYREF rehash)
 *     $(MYREF require)
 *     $(MYREF update)
 *     $(MYREF values)
 * ))
 * $(TR $(TD General) $(TD
 *     $(MYREF destroy)
 *     $(MYREF hashOf)
 *     $(MYREF imported)
 *     $(MYREF noreturn)
 * ))
 * $(TR $(TD Classes) $(TD
 *     $(MYREF Error)
 *     $(MYREF Exception)
 *     $(MYREF Object)
 *     $(MYREF opEquals)
 *     $(MYREF Throwable)
 * ))
 * $(TR $(TD Type info) $(TD
 *     $(MYREF Interface)
 *     $(MYREF ModuleInfo)
 *     $(MYREF OffsetTypeInfo)
 *     $(MYREF RTInfoImpl)
 *     $(MYREF rtinfoNoPointers)
 *     $(MYREF TypeInfo)
 *     $(MYREF TypeInfo_Class)
 * ))
 * ))
 *
 * Forms the symbols available to all D programs. Includes Object, which is
 * the root of the class object hierarchy.  This module is implicitly
 * imported.
 *
 * Copyright: Copyright Digital Mars 2000 - 2011.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly
 * Source: $(DRUNTIMESRC object.d)
 */

module object;

alias size_t = typeof(int.sizeof);
alias ptrdiff_t = typeof(cast(void*)0 - cast(void*)0);

alias sizediff_t = ptrdiff_t; // For backwards compatibility only.
/**
 * Bottom type.
 * See $(DDSUBLINK spec/type, noreturn, `noreturn`).
 */
alias noreturn = typeof(*null);

alias hash_t = size_t; // For backwards compatibility only.
alias equals_t = bool; // For backwards compatibility only.

alias string  = immutable(char)[];
alias wstring = immutable(wchar)[];
alias dstring = immutable(dchar)[];

version (D_ObjectiveC)
{
    deprecated("explicitly import `selector` instead using: `import core.attribute : selector;`")
        public import core.attribute : selector;
}
version (Posix) public import core.attribute : gnuAbiTag;

// Some ABIs use a complex varargs implementation requiring TypeInfo.argTypes().
version (GNU)
{
    // No TypeInfo-based core.vararg.va_arg().
}
else version (X86_64)
{
    version (DigitalMars) version = WithArgTypes;
    else version (Windows) { /* no need for Win64 ABI */ }
    else version = WithArgTypes;
}
else version (AArch64)
{
    // Apple uses a trivial varargs implementation
    version (OSX) {}
    else version (iOS) {}
    else version (TVOS) {}
    else version (WatchOS) {}
    else version = WithArgTypes;
}

/**
 * All D class objects inherit from Object.
 */
class Object
{
    /**
     * Convert Object to a human readable string.
     */
    string toString()
    {
        return typeid(this).name;
    }

    @system unittest
    {
        enum unittest_sym_name = __traits(identifier, __traits(parent, (){}));
        enum fqn_unittest = "object.Object." ~ unittest_sym_name; // object.__unittest_LX_CY

        class C {}

        Object obj = new Object;
        C c = new C;

        assert(obj.toString() == "object.Object");
        assert(c.toString() == fqn_unittest ~ ".C");
    }

    /**
     * Compute hash function for Object.
     */
    size_t toHash() @trusted nothrow
    {
        // BUG: this prevents a compacting GC from working, needs to be fixed
        size_t addr = cast(size_t) cast(void*) this;
        // The bottom log2((void*).alignof) bits of the address will always
        // be 0. Moreover it is likely that each Object is allocated with a
        // separate call to malloc. The alignment of malloc differs from
        // platform to platform, but rather than having special cases for
        // each platform it is safe to use a shift of 4. To minimize
        // collisions in the low bits it is more important for the shift to
        // not be too small than for the shift to not be too big.
        return addr ^ (addr >>> 4);
    }

    /**
     * Compare with another Object obj.
     * Returns:
     *  $(TABLE
     *  $(TR $(TD this &lt; obj) $(TD &lt; 0))
     *  $(TR $(TD this == obj) $(TD 0))
     *  $(TR $(TD this &gt; obj) $(TD &gt; 0))
     *  )
     */
    int opCmp(Object o)
    {
        // BUG: this prevents a compacting GC from working, needs to be fixed
        //return cast(int)cast(void*)this - cast(int)cast(void*)o;

        throw new Exception("need opCmp for class " ~ typeid(this).name);
        //return this !is o;
    }

    @system unittest
    {
        Object obj = new Object;

        bool gotCaught;
        try
        {
            obj.opCmp(new Object);
        }
        catch (Exception e)
        {
            gotCaught = true;
            assert(e.msg == "need opCmp for class object.Object");
        }
        assert(gotCaught);
    }

    /**
     * Test whether $(D this) is equal to $(D o).
     * The default implementation only compares by identity (using the $(D is) operator).
     * Generally, overrides and overloads for $(D opEquals) should attempt to compare objects by their contents.
     * A class will most likely want to add an overload that takes your specific type as the argument
     * and does the content comparison. Then you can override this and forward it to your specific
     * typed overload with a cast. Remember to check for `null` on the typed overload.
     *
     * Examples:
     * ---
     * class Child {
     *    int contents;
     *    // the typed overload first. It can use all the attribute you want
     *    bool opEquals(const Child c) const @safe pure nothrow @nogc
     *    {
     *        if (c is null)
     *            return false;
     *        return this.contents == c.contents;
     *    }
     *
     *    // and now the generic override forwards with a cast
     *    override bool opEquals(Object o)
     *    {
     *        return this.opEquals(cast(Child) o);
     *    }
     * }
     * ---
     */
    bool opEquals(Object o)
    {
        return this is o;
    }

    interface Monitor
    {
        void lock();
        void unlock();
    }

    /**
     * Create instance of class specified by the fully qualified name
     * classname.
     * The class must either have no constructors or have
     * a default constructor.
     * Returns:
     *   null if failed
     * Example:
     * ---
     * module foo.bar;
     *
     * class C
     * {
     *     this() { x = 10; }
     *     int x;
     * }
     *
     * void main()
     * {
     *     auto c = cast(C)Object.factory("foo.bar.C");
     *     assert(c !is null && c.x == 10);
     * }
     * ---
     */
    static Object factory(string classname)
    {
        auto ci = TypeInfo_Class.find(classname);
        if (ci)
        {
            return ci.create();
        }
        return null;
    }

    @system unittest
    {
        Object valid_obj = Object.factory("object.Object");
        Object invalid_obj = Object.factory("object.__this_class_doesnt_exist__");

        assert(valid_obj !is null);
        assert(invalid_obj is null);
    }
}

/++
    Implementation for class opEquals override. Calls the class-defined methods after a null check.
    Please note this is not nogc right now, even if your implementation is, because of
    the typeinfo name string compare. This is because of dmd's dll implementation. However,
    it can infer to @safe if your class' opEquals is.
+/
bool opEquals(LHS, RHS)(LHS lhs, RHS rhs)
if ((is(LHS : const Object) || is(LHS : const shared Object)) &&
    (is(RHS : const Object) || is(RHS : const shared Object)))
{
    static if (__traits(compiles, lhs.opEquals(rhs)) && __traits(compiles, rhs.opEquals(lhs)))
    {
        // If aliased to the same object or both null => equal
        if (lhs is rhs) return true;

        // If either is null => non-equal
        if (lhs is null || rhs is null) return false;

        if (!lhs.opEquals(rhs)) return false;

        // If same exact type => one call to method opEquals
        if (typeid(lhs) is typeid(rhs) ||
            !__ctfe && typeid(lhs).opEquals(typeid(rhs)))
                /* CTFE doesn't like typeid much. 'is' works, but opEquals doesn't:
                https://issues.dlang.org/show_bug.cgi?id=7147
                But CTFE also guarantees that equal TypeInfos are
                always identical. So, no opEquals needed during CTFE. */
        {
            return true;
        }

        // General case => symmetric calls to method opEquals
        return rhs.opEquals(lhs);
    }
    else
    {
        // this is a compatibility hack for the old const cast behavior
        // if none of the new overloads compile, we'll go back plain Object,
        // including casting away const. It does this through the pointer
        // to bypass any opCast that may be present on the original class.
        return .opEquals!(Object, Object)(*cast(Object*) &lhs, *cast(Object*) &rhs);

    }
}

/// If aliased to the same object or both null => equal
@system unittest // this one is not @safe because it goes through the Object base method
{
    class F { int flag; this(int flag) { this.flag = flag; } }

    F f;
    assert(f == f); // both null
    f = new F(1);
    assert(f == f); // both aliased to the same object
}

/// If either is null => non-equal
@system unittest
{
    class F { int flag; this(int flag) { this.flag = flag; } }
    F f;
    assert(!(new F(0) == f));
    assert(!(f == new F(0)));
}

/// If same exact type => one call to method opEquals
/// This test passes `@safe` because it defines a new opEquals with `@safe`
@safe unittest
{
    class F
    {
        int flag;

        this(int flag)
        {
            this.flag = flag;
        }

        bool opEquals(const F o) const @safe nothrow pure
        {
            return flag == o.flag;
        }
    }

    F f;
    assert(new F(0) == new F(0));
    assert(!(new F(0) == new F(1)));
}

/// General case => symmetric calls to method opEquals
@safe unittest
{
    int fEquals, gEquals;

    class Base
    {
        int flag;
        this(int flag)
        {
            this.flag = flag;
        }
    }

    class F : Base
    {
        this(int flag) { super(flag); }

        bool opEquals(const Base o) @safe
        {
            fEquals++;
            return flag == o.flag;
        }
    }

    class G : Base
    {
        this(int flag) { super(flag); }

        bool opEquals(const Base o) @safe
        {
            gEquals++;
            return flag == o.flag;
        }
    }

    assert(new F(1) == new G(1));
    assert(fEquals == 1);
    assert(gEquals == 1);
}

/++
    This test shows an example for a comprehensive inheritance equality chain too.
+/
unittest
{
    static class Base
    {
        int member;

        this(int member) pure @safe nothrow @nogc
        {
            this.member = member;
        }

        override bool opEquals(Object rhs) const
        {
            return this.opEquals(cast(Base) rhs);
        }

        bool opEquals(const Base rhs) const @nogc pure nothrow @safe
        {
            if (rhs is null)
                return false;
            return this.member == rhs.member;
        }
    }

    // works through the direct class with attributes enabled, except for pure and nogc in the current TypeInfo implementation
    bool testThroughBase() nothrow @safe
    {
        Base b1 = new Base(0);
        Base b2 = new Base(0);
        assert(b1 == b2);
        Base b3 = new Base(1);
        assert(b1 != b3);
        return true;
    }

    static assert(testThroughBase());

    // also works through the base class interface thanks to the override, but no more attributes
    bool testThroughObject()
    {
        Object o1 = new Base(0);
        Object o2 = new Base(0);
        assert(o1 == o2);
        Object o3 = new Base(1);
        assert(o1 != o3);
        return true;
    }

    static assert(testThroughObject());

    // Each time you make a child, you want to override all old opEquals
    // and add a new overload for the new child.
    static class Child : Base
    {
        int member2;

        this(int member, int member2) pure @safe nothrow @nogc
        {
            super(member);
            this.member2 = member2;
        }

        // override the whole chain so it works consistently though any base
        override bool opEquals(Object rhs) const
        {
            return this.opEquals(cast(Child) rhs);
        }
        override bool opEquals(const Base rhs) const
        {
            return this.opEquals(cast(const Child) rhs);
        }
        // and then add the new overload, if necessary, to handle new members
        bool opEquals(const Child rhs) const @nogc pure nothrow @safe
        {
            if (rhs is null)
                return false;
            // can call back to the devirtualized base test with implicit conversion
            // then compare the new member too. or we could have just compared the base
            // member directly here as well.
            return Base.opEquals(rhs) && this.member2 == rhs.member2;
        }

        // a mixin template, of course, could automate this.
    }

    bool testThroughChild()
    {
        Child a = new Child(0, 0);
        Child b = new Child(0, 1);
        assert(a != b);

        Base ba = a;
        Base bb = b;
        assert(ba != bb);

        Object oa = a;
        Object ob = b;
        assert(oa != ob);

        return true;
    }

    static assert(testThroughChild());
}

// To cover const Object opEquals
@system unittest
{
    const Object obj1 = new Object;
    const Object obj2 = new Object;

    assert(obj1 == obj1);
    assert(obj1 != obj2);
}

// https://issues.dlang.org/show_bug.cgi?id=23291
@system unittest
{
    static shared class C { bool opEquals(const(shared(C)) rhs) const shared  { return true;}}
    const(C) c = new C();
    const(C)[] a = [c];
    const(C)[] b = [c];
    assert(a[0] == b[0]);
}

private extern(C) void _d_setSameMutex(shared Object ownee, shared Object owner) nothrow;

/** Makes ownee use owner's mutex.
 * This will initialize owner's mutex if it hasn't been set yet.
 * Params:
 * ownee = object to change
 * owner = source object
 */
void setSameMutex(shared Object ownee, shared Object owner)
{
    import core.atomic : atomicLoad;
    _d_setSameMutex(atomicLoad(ownee), atomicLoad(owner));
}

@system unittest
{
    shared Object obj1 = new Object;
    synchronized class C
    {
        void bar() {}
    }
    shared C obj2 = new shared(C);
    obj2.bar();

    assert(obj1.__monitor != obj2.__monitor);
    assert(obj1.__monitor is null);

    setSameMutex(obj1, obj2);
    assert(obj1.__monitor == obj2.__monitor);
    assert(obj1.__monitor !is null);
}

/**
 * Information about an interface.
 * When an object is accessed via an interface, an Interface* appears as the
 * first entry in its vtbl.
 */
struct Interface
{
    /// Class info returned by `typeid` for this interface (not for containing class)
    TypeInfo_Class   classinfo;
    void*[]     vtbl;
    size_t      offset;     /// offset to Interface 'this' from Object 'this'
}

/**
 * Array of pairs giving the offset and type information for each
 * member in an aggregate.
 */
struct OffsetTypeInfo
{
    size_t   offset;    /// Offset of member from start of object
    TypeInfo ti;        /// TypeInfo for this member
}

/**
 * Runtime type information about a type.
 * Can be retrieved for any type using a
 * $(GLINK2 expression,TypeidExpression, TypeidExpression).
 */
class TypeInfo
{
    override string toString() const @safe nothrow
    {
        return typeid(this).name;
    }

    override size_t toHash() @trusted const nothrow
    {
        return hashOf(this.toString());
    }

    override int opCmp(Object rhs)
    {
        if (this is rhs)
            return 0;
        auto ti = cast(TypeInfo) rhs;
        if (ti is null)
            return 1;
        return __cmp(this.toString(), ti.toString());
    }

    @system unittest
    {
        assert(typeid(void) <= typeid(void));
        assert(typeid(void).opCmp(null));
        assert(!typeid(void).opCmp(typeid(void)));
    }

    override bool opEquals(Object o)
    {
        return opEquals(cast(TypeInfo) o);
    }

    bool opEquals(const TypeInfo ti) @safe nothrow const
    {
        /* TypeInfo instances are singletons, but duplicates can exist
         * across DLL's. Therefore, comparing for a name match is
         * sufficient.
         */
        if (this is ti)
            return true;
        return ti && this.toString() == ti.toString();
    }

    @system unittest
    {
        auto anotherObj = new Object();

        assert(typeid(void).opEquals(typeid(void)));
        assert(typeid(void) != anotherObj); // calling .opEquals here directly is a type mismatch
    }

    /**
     * Computes a hash of the instance of a type.
     * Params:
     *    p = pointer to start of instance of the type
     * Returns:
     *    the hash
     * Bugs:
     *    fix https://issues.dlang.org/show_bug.cgi?id=12516 e.g. by changing this to a truly safe interface.
     */
    size_t getHash(scope const void* p) @trusted nothrow const
    {
        // by default, do not assume anything about the type
        return 0;
    }

    /// Compares two instances for equality.
    bool equals(in void* p1, in void* p2) const { return p1 == p2; }

    /// Compares two instances for &lt;, ==, or &gt;.
    int compare(in void* p1, in void* p2) const { return _xopCmp(p1, p2); }

    /// Returns size of the type.
    @property size_t tsize() nothrow pure const @safe @nogc { return 0; }

    /// Swaps two instances of the type.
    void swap(void* p1, void* p2) const
    {
        size_t remaining = tsize;
        // If the type might contain pointers perform the swap in pointer-sized
        // chunks in case a garbage collection pass interrupts this function.
        if ((cast(size_t) p1 | cast(size_t) p2) % (void*).alignof == 0)
        {
            while (remaining >= (void*).sizeof)
            {
                void* tmp = *cast(void**) p1;
                *cast(void**) p1 = *cast(void**) p2;
                *cast(void**) p2 = tmp;
                p1 += (void*).sizeof;
                p2 += (void*).sizeof;
                remaining -= (void*).sizeof;
            }
        }
        for (size_t i = 0; i < remaining; i++)
        {
            byte t = (cast(byte *)p1)[i];
            (cast(byte*)p1)[i] = (cast(byte*)p2)[i];
            (cast(byte*)p2)[i] = t;
        }
    }

    @system unittest
    {
        class _TypeInfo_Dummy : TypeInfo
        {
            override const(void)[] initializer() const { return []; }
            @property override size_t tsize() nothrow pure const @safe @nogc { return tsize_val; }

            size_t tsize_val;
        }
        auto dummy = new _TypeInfo_Dummy();
        cast(void)dummy.initializer(); // For coverage completeness

        int a = 2, b = -2;
        dummy.swap(&a, &b);
        // does nothing because tsize is 0
        assert(a == 2);
        assert(b == -2);

        dummy.tsize_val = int.sizeof;
        dummy.swap(&a, &b);
        assert(a == -2);
        assert(b == 2);

        void* ptr_a = null, ptr_b = cast(void*)1;
        dummy.tsize_val = (void*).sizeof;
        dummy.swap(&ptr_a, &ptr_b);
        assert(ptr_a is cast(void*)1);
        assert(ptr_b is null);
    }

    /** Get TypeInfo for 'next' type, as defined by what kind of type this is,
    null if none. */
    @property inout(TypeInfo) next() nothrow pure inout @nogc { return null; }

    /**
     * Return default initializer.  If the type should be initialized to all
     * zeros, an array with a null ptr and a length equal to the type size will
     * be returned. For static arrays, this returns the default initializer for
     * a single element of the array, use `tsize` to get the correct size.
     */
    abstract const(void)[] initializer() nothrow pure const @safe @nogc;

    /** Get flags for type: 1 means GC should scan for pointers,
    2 means arg of this type is passed in SIMD register(s) if available */
    @property uint flags() nothrow pure const @safe @nogc { return 0; }

    /// Get type information on the contents of the type; null if not available
    const(OffsetTypeInfo)[] offTi() const { return null; }
    /// Run the destructor on the object and all its sub-objects
    void destroy(void* p) const {}
    /// Run the postblit on the object and all its sub-objects
    void postblit(void* p) const {}


    /// Return alignment of type
    @property size_t talign() nothrow pure const @safe @nogc { return tsize; }

    /** Return internal info on arguments fitting into 8byte.
     * See X86-64 ABI 3.2.3
     */
    version (WithArgTypes) int argTypes(out TypeInfo arg1, out TypeInfo arg2) @safe nothrow
    {
        arg1 = this;
        return 0;
    }

    /** Return info used by the garbage collector to do precise collection.
     */
    @property immutable(void)* rtInfo() nothrow pure const @trusted @nogc { return rtinfoHasPointers; } // better safe than sorry
}

@system unittest
{
    class _TypeInfo_Dummy : TypeInfo
    {
        override const(void)[] initializer() const { return []; }
    }
    auto dummy = new _TypeInfo_Dummy();
    cast(void)dummy.initializer(); // For coverage completeness

    assert(dummy.rtInfo() is rtinfoHasPointers);
    assert(typeid(void).rtInfo() is rtinfoNoPointers);

    assert(dummy.tsize() == 0);

    bool gotCaught;
    try
    {
        dummy.compare(null, null);
    } catch (Error e)
    {
        gotCaught = true;
        assert(e.msg == "TypeInfo.compare is not implemented");
    }
    assert(gotCaught);

    assert(dummy.equals(null, null));
    assert(!dummy.equals(cast(void*)1, null));
}

@system unittest
{
    assert(typeid(void).next() is null);
    assert(typeid(void).offTi() is null);
    assert(typeid(void).tsize() == 1);

    version (WithArgTypes)
    {
        TypeInfo ti1;
        TypeInfo ti2;
        assert(typeid(void).argTypes(ti1, ti2) == 0);
        assert(typeid(void) is ti1);

        assert(ti1 !is null);
        assert(ti2 is null);
    }
}

@system unittest
{
    class _ZypeInfo_Dummy : TypeInfo
    {
        override const(void)[] initializer() const { return []; }
    }
    auto dummy2 = new _ZypeInfo_Dummy();
    cast(void)dummy2.initializer(); // For coverage completeness

    assert(typeid(void) > dummy2);
    assert(dummy2 < typeid(void));
}

@safe unittest
{
    enum unittest_sym_name = __traits(identifier, __traits(parent, (){}));
    enum fqn_unittest = "object." ~ unittest_sym_name; // object.__unittest_LX_CY

    class _TypeInfo_Dummy : TypeInfo
    {
        override const(void)[] initializer() const { return []; }
    }

    auto dummy = new _TypeInfo_Dummy();
    cast(void)dummy.initializer(); // For coverage completeness

    assert(dummy.toString() == fqn_unittest ~ "._TypeInfo_Dummy");
    assert(dummy.toHash() == hashOf(dummy.toString()));
    assert(dummy.getHash(null) == 0);
}

class TypeInfo_Enum : TypeInfo
{
    override string toString() const pure { return name; }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_Enum)o;
        return c && this.name == c.name &&
                    this.base == c.base;
    }

    @system unittest
    {
        enum E { A, B, C }
        enum EE { A, B, C }

        assert(typeid(E).opEquals(typeid(E)));
        assert(!typeid(E).opEquals(typeid(EE)));
    }

    override size_t getHash(scope const void* p) const { return base.getHash(p); }

    @system unittest
    {
        enum E { A, B, C }
        E e1 = E.A;
        E e2 = E.B;

        assert(typeid(E).getHash(&e1) == hashOf(E.A));
        assert(typeid(E).getHash(&e2) == hashOf(E.B));

        enum ES : string { A = "foo", B = "bar" }
        ES es1 = ES.A;
        ES es2 = ES.B;

        assert(typeid(ES).getHash(&es1) == hashOf("foo"));
        assert(typeid(ES).getHash(&es2) == hashOf("bar"));
    }

    override bool equals(in void* p1, in void* p2) const { return base.equals(p1, p2); }

    @system unittest
    {
        enum E { A, B, C }

        E e1 = E.A;
        E e2 = E.B;

        assert(typeid(E).equals(&e1, &e1));
        assert(!typeid(E).equals(&e1, &e2));
    }

    override int compare(in void* p1, in void* p2) const { return base.compare(p1, p2); }

    @system unittest
    {
        enum E { A, B, C }

        E e1 = E.A;
        E e2 = E.B;

        assert(typeid(E).compare(&e1, &e1) == 0);
        assert(typeid(E).compare(&e1, &e2) < 0);
        assert(typeid(E).compare(&e2, &e1) > 0);
    }

    override @property size_t tsize() nothrow pure const { return base.tsize; }

    @safe unittest
    {
        enum E { A, B, C }
        enum ES : string { A = "a", B = "b", C = "c"}

        assert(typeid(E).tsize == E.sizeof);
        assert(typeid(ES).tsize == ES.sizeof);
        assert(typeid(E).tsize != ES.sizeof);
    }

    override void swap(void* p1, void* p2) const { return base.swap(p1, p2); }

    @system unittest
    {
        enum E { A, B, C }

        E e1 = E.A;
        E e2 = E.B;

        typeid(E).swap(&e1, &e2);
        assert(e1 == E.B);
        assert(e2 == E.A);
    }

    override @property inout(TypeInfo) next() nothrow pure inout { return base.next; }

    @system unittest
    {
        enum E { A, B, C }

        assert(typeid(E).next is null);
    }

    override @property uint flags() nothrow pure const { return base.flags; }

    @safe unittest
    {
        enum E { A, B, C }

        assert(typeid(E).flags == 0);
    }

    override const(OffsetTypeInfo)[] offTi() const { return base.offTi; }

    @system unittest
    {
        enum E { A, B, C }

        assert(typeid(E).offTi is null);
    }

    override void destroy(void* p) const { return base.destroy(p); }
    override void postblit(void* p) const { return base.postblit(p); }

    override const(void)[] initializer() const
    {
        return m_init.length ? m_init : base.initializer();
    }

    override @property size_t talign() nothrow pure const { return base.talign; }

    version (WithArgTypes) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        return base.argTypes(arg1, arg2);
    }

    override @property immutable(void)* rtInfo() const { return base.rtInfo; }

    TypeInfo base;
    string   name;
    void[]   m_init;
}

@safe unittest
{
    enum unittest_sym_name = __traits(identifier, __traits(parent, (){}));
    enum fqn_unittest = "object." ~ unittest_sym_name; // object.__unittest_LX_CY

    enum E { A, B, C }
    enum EE { A, B, C }

    assert(typeid(E).toString() == fqn_unittest ~ ".E");
}


@safe unittest // https://issues.dlang.org/show_bug.cgi?id=12233
{
    static assert(is(typeof(TypeInfo.init) == TypeInfo));
    assert(TypeInfo.init is null);
}


// Please make sure to keep this in sync with TypeInfo_P (src/rt/typeinfo/ti_ptr.d)
class TypeInfo_Pointer : TypeInfo
{
    override string toString() const { return m_next.toString() ~ "*"; }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_Pointer)o;
        return c && this.m_next == c.m_next;
    }

    override size_t getHash(scope const void* p) @trusted const
    {
        size_t addr = cast(size_t) *cast(const void**)p;
        return addr ^ (addr >> 4);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        return *cast(void**)p1 == *cast(void**)p2;
    }

    override int compare(in void* p1, in void* p2) const
    {
        const v1 = *cast(void**) p1, v2 = *cast(void**) p2;
        return (v1 > v2) - (v1 < v2);
    }

    override @property size_t tsize() nothrow pure const
    {
        return (void*).sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. (void*).sizeof];
    }

    override void swap(void* p1, void* p2) const
    {
        void* tmp = *cast(void**)p1;
        *cast(void**)p1 = *cast(void**)p2;
        *cast(void**)p2 = tmp;
    }

    override @property inout(TypeInfo) next() nothrow pure inout { return m_next; }
    override @property uint flags() nothrow pure const { return 1; }

    TypeInfo m_next;
}

class TypeInfo_Array : TypeInfo
{
    override string toString() const { return value.toString() ~ "[]"; }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_Array)o;
        return c && this.value == c.value;
    }

    override size_t getHash(scope const void* p) @trusted const
    {
        void[] a = *cast(void[]*)p;
        return getArrayHash(value, a.ptr, a.length);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        void[] a1 = *cast(void[]*)p1;
        void[] a2 = *cast(void[]*)p2;
        if (a1.length != a2.length)
            return false;
        size_t sz = value.tsize;
        for (size_t i = 0; i < a1.length; i++)
        {
            if (!value.equals(a1.ptr + i * sz, a2.ptr + i * sz))
                return false;
        }
        return true;
    }

    override int compare(in void* p1, in void* p2) const
    {
        void[] a1 = *cast(void[]*)p1;
        void[] a2 = *cast(void[]*)p2;
        size_t sz = value.tsize;
        size_t len = a1.length;

        if (a2.length < len)
            len = a2.length;
        for (size_t u = 0; u < len; u++)
        {
            immutable int result = value.compare(a1.ptr + u * sz, a2.ptr + u * sz);
            if (result)
                return result;
        }
        return (a1.length > a2.length) - (a1.length < a2.length);
    }

    override @property size_t tsize() nothrow pure const
    {
        return (void[]).sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. (void[]).sizeof];
    }

    override void swap(void* p1, void* p2) const
    {
        void[] tmp = *cast(void[]*)p1;
        *cast(void[]*)p1 = *cast(void[]*)p2;
        *cast(void[]*)p2 = tmp;
    }

    TypeInfo value;

    override @property inout(TypeInfo) next() nothrow pure inout
    {
        return value;
    }

    override @property uint flags() nothrow pure const { return 1; }

    override @property size_t talign() nothrow pure const
    {
        return (void[]).alignof;
    }

    version (WithArgTypes) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        arg1 = typeid(size_t);
        arg2 = typeid(void*);
        return 0;
    }

    override @property immutable(void)* rtInfo() nothrow pure const @safe { return RTInfo!(void[]); }
}

class TypeInfo_StaticArray : TypeInfo
{
    override string toString() const
    {
        import core.internal.string : unsignedToTempString;

        char[20] tmpBuff = void;
        const lenString = unsignedToTempString(len, tmpBuff);

        return (() @trusted => cast(string) (value.toString() ~ "[" ~ lenString ~ "]"))();
    }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_StaticArray)o;
        return c && this.len == c.len &&
                    this.value == c.value;
    }

    override size_t getHash(scope const void* p) @trusted const
    {
        return getArrayHash(value, p, len);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        size_t sz = value.tsize;

        for (size_t u = 0; u < len; u++)
        {
            if (!value.equals(p1 + u * sz, p2 + u * sz))
                return false;
        }
        return true;
    }

    override int compare(in void* p1, in void* p2) const
    {
        size_t sz = value.tsize;

        for (size_t u = 0; u < len; u++)
        {
            immutable int result = value.compare(p1 + u * sz, p2 + u * sz);
            if (result)
                return result;
        }
        return 0;
    }

    override @property size_t tsize() nothrow pure const
    {
        return len * value.tsize;
    }

    override void swap(void* p1, void* p2) const
    {
        import core.stdc.string : memcpy;

        size_t remaining = value.tsize * len;
        void[size_t.sizeof * 4] buffer = void;
        while (remaining > buffer.length)
        {
            memcpy(buffer.ptr, p1, buffer.length);
            memcpy(p1, p2, buffer.length);
            memcpy(p2, buffer.ptr, buffer.length);
            p1 += buffer.length;
            p2 += buffer.length;
            remaining -= buffer.length;
        }
        memcpy(buffer.ptr, p1, remaining);
        memcpy(p1, p2, remaining);
        memcpy(p2, buffer.ptr, remaining);
    }

    override const(void)[] initializer() nothrow pure const
    {
        return value.initializer();
    }

    override @property inout(TypeInfo) next() nothrow pure inout { return value; }
    override @property uint flags() nothrow pure const { return value.flags; }

    override void destroy(void* p) const
    {
        immutable sz = value.tsize;
        p += sz * len;
        foreach (i; 0 .. len)
        {
            p -= sz;
            value.destroy(p);
        }
    }

    override void postblit(void* p) const
    {
        immutable sz = value.tsize;
        foreach (i; 0 .. len)
        {
            value.postblit(p);
            p += sz;
        }
    }

    TypeInfo value;
    size_t   len;

    override @property size_t talign() nothrow pure const
    {
        return value.talign;
    }

    version (WithArgTypes) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        arg1 = typeid(void*);
        return 0;
    }

    // just return the rtInfo of the element, we have no generic type T to run RTInfo!T on
    override @property immutable(void)* rtInfo() nothrow pure const @safe { return value.rtInfo(); }
}

// https://issues.dlang.org/show_bug.cgi?id=21315
@system unittest
{
    int[16] a, b;
    foreach (int i; 0 .. 16)
    {
        a[i] = i;
        b[i] = ~i;
    }
    typeid(int[16]).swap(&a, &b);
    foreach (int i; 0 .. 16)
    {
        assert(a[i] == ~i);
        assert(b[i] == i);
    }
}

class TypeInfo_AssociativeArray : TypeInfo
{
    override string toString() const
    {
        return value.toString() ~ "[" ~ key.toString() ~ "]";
    }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_AssociativeArray)o;
        return c && this.key == c.key &&
                    this.value == c.value;
    }

    override bool equals(in void* p1, in void* p2) @trusted const
    {
        return !!_aaEqual(this, *cast(const AA*) p1, *cast(const AA*) p2);
    }

    override hash_t getHash(scope const void* p) nothrow @trusted const
    {
        return _aaGetHash(cast(AA*)p, this);
    }

    // BUG: need to add the rest of the functions

    override @property size_t tsize() nothrow pure const
    {
        return (char[int]).sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. (char[int]).sizeof];
    }

    override @property inout(TypeInfo) next() nothrow pure inout { return value; }
    override @property uint flags() nothrow pure const { return 1; }

    // TypeInfo entry is generated from the type of this template to help rt/aaA.d
    static struct Entry(K, V)
    {
        K key;
        V value;
    }

    TypeInfo value;
    TypeInfo key;
    TypeInfo entry;

    override @property size_t talign() nothrow pure const
    {
        return (char[int]).alignof;
    }

    version (WithArgTypes) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        arg1 = typeid(void*);
        return 0;
    }
}

class TypeInfo_Vector : TypeInfo
{
    override string toString() const { return "__vector(" ~ base.toString() ~ ")"; }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_Vector)o;
        return c && this.base == c.base;
    }

    override size_t getHash(scope const void* p) const { return base.getHash(p); }
    override bool equals(in void* p1, in void* p2) const { return base.equals(p1, p2); }
    override int compare(in void* p1, in void* p2) const { return base.compare(p1, p2); }
    override @property size_t tsize() nothrow pure const { return base.tsize; }
    override void swap(void* p1, void* p2) const { return base.swap(p1, p2); }

    override @property inout(TypeInfo) next() nothrow pure inout { return base.next; }
    override @property uint flags() nothrow pure const { return 2; /* passed in SIMD register */ }

    override const(void)[] initializer() nothrow pure const
    {
        return base.initializer();
    }

    override @property size_t talign() nothrow pure const { return 16; }

    version (WithArgTypes) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        return base.argTypes(arg1, arg2);
    }

    TypeInfo base;
}

class TypeInfo_Function : TypeInfo
{
    override string toString() const pure @trusted
    {
        import core.demangle : demangleType;

        alias SafeDemangleFunctionType = char[] function (const(char)[] buf, char[] dst = null) @safe nothrow pure;
        SafeDemangleFunctionType demangle = cast(SafeDemangleFunctionType) &demangleType;

        return cast(string) demangle(deco);
    }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_Function)o;
        return c && this.deco == c.deco;
    }

    // BUG: need to add the rest of the functions

    override @property size_t tsize() nothrow pure const
    {
        return 0;       // no size for functions
    }

    override const(void)[] initializer() const @safe
    {
        return null;
    }

    override @property immutable(void)* rtInfo() nothrow pure const @safe { return rtinfoNoPointers; }

    TypeInfo next;

    /**
    * Mangled function type string
    */
    string deco;
}

@safe unittest
{
    abstract class C
    {
       void func();
       void func(int a);
       int func(int a, int b);
    }

    alias functionTypes = typeof(__traits(getVirtualMethods, C, "func"));
    assert(typeid(functionTypes[0]).toString() == "void function()");
    assert(typeid(functionTypes[1]).toString() == "void function(int)");
    assert(typeid(functionTypes[2]).toString() == "int function(int, int)");
}

@system unittest
{
    abstract class C
    {
       void func();
       void func(int a);
    }

    alias functionTypes = typeof(__traits(getVirtualMethods, C, "func"));

    Object obj = typeid(functionTypes[0]);
    assert(obj.opEquals(typeid(functionTypes[0])));
    assert(typeid(functionTypes[0]) == typeid(functionTypes[0]));
    assert(typeid(functionTypes[0]) != typeid(functionTypes[1]));

    assert(typeid(functionTypes[0]).tsize() == 0);
    assert(typeid(functionTypes[0]).initializer() is null);
    assert(typeid(functionTypes[0]).rtInfo() is null);
}

class TypeInfo_Delegate : TypeInfo
{
    override string toString() const pure @trusted
    {
        import core.demangle : demangleType;

        alias SafeDemangleFunctionType = char[] function (const(char)[] buf, char[] dst = null) @safe nothrow pure;
        SafeDemangleFunctionType demangle = cast(SafeDemangleFunctionType) &demangleType;

        return cast(string) demangle(deco);
    }

    @safe unittest
    {
        double sqr(double x) { return x * x; }
        sqr(double.init); // for coverage completeness

        auto delegate_str = "double delegate(double) pure nothrow @nogc @safe";

        assert(typeid(typeof(&sqr)).toString() == delegate_str);
        assert(delegate_str.hashOf() == typeid(typeof(&sqr)).hashOf());
        assert(typeid(typeof(&sqr)).toHash() == typeid(typeof(&sqr)).hashOf());

        int g;

        alias delegate_type = typeof((int a, int b) => a + b + g);
        delegate_str = "int delegate(int, int) pure nothrow @nogc @safe";

        assert(typeid(delegate_type).toString() == delegate_str);
        assert(delegate_str.hashOf() == typeid(delegate_type).hashOf());
        assert(typeid(delegate_type).toHash() == typeid(delegate_type).hashOf());
    }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_Delegate)o;
        return c && this.deco == c.deco;
    }

    @system unittest
    {
        double sqr(double x) { return x * x; }
        int dbl(int x) { return x + x; }
        sqr(double.init); // for coverage completeness
        dbl(int.init); // for coverage completeness

        Object obj = typeid(typeof(&sqr));
        assert(obj.opEquals(typeid(typeof(&sqr))));
        assert(typeid(typeof(&sqr)) == typeid(typeof(&sqr)));
        assert(typeid(typeof(&dbl)) != typeid(typeof(&sqr)));
    }

    override size_t getHash(scope const void* p) @trusted const
    {
        return hashOf(*cast(const void delegate() *)p);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        auto dg1 = *cast(void delegate()*)p1;
        auto dg2 = *cast(void delegate()*)p2;
        return dg1 == dg2;
    }

    override int compare(in void* p1, in void* p2) const
    {
        auto dg1 = *cast(void delegate()*)p1;
        auto dg2 = *cast(void delegate()*)p2;

        if (dg1 < dg2)
            return -1;
        else if (dg1 > dg2)
            return 1;
        else
            return 0;
    }

    override @property size_t tsize() nothrow pure const
    {
        alias dg = int delegate();
        return dg.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. (int delegate()).sizeof];
    }

    override @property uint flags() nothrow pure const { return 1; }

    TypeInfo next;
    string deco;

    override @property size_t talign() nothrow pure const
    {
        alias dg = int delegate();
        return dg.alignof;
    }

    version (WithArgTypes) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        arg1 = typeid(void*);
        arg2 = typeid(void*);
        return 0;
    }

    override @property immutable(void)* rtInfo() nothrow pure const @safe { return RTInfo!(int delegate()); }
}

private extern (C) Object _d_newclass(const TypeInfo_Class ci);
private extern (C) int _d_isbaseof(scope TypeInfo_Class child,
    scope const TypeInfo_Class parent) @nogc nothrow pure @safe; // rt.cast_

/**
 * Runtime type information about a class.
 * Can be retrieved from an object instance by using the
 * $(DDSUBLINK spec/expression,typeid_expressions,typeid expression).
 */
class TypeInfo_Class : TypeInfo
{
    override string toString() const pure { return name; }

    override bool opEquals(const TypeInfo o) const
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_Class)o;
        return c && this.name == c.name;
    }

    override size_t getHash(scope const void* p) @trusted const
    {
        auto o = *cast(Object*)p;
        return o ? o.toHash() : 0;
    }

    override bool equals(in void* p1, in void* p2) const
    {
        Object o1 = *cast(Object*)p1;
        Object o2 = *cast(Object*)p2;

        return (o1 is o2) || (o1 && o1.opEquals(o2));
    }

    override int compare(in void* p1, in void* p2) const
    {
        Object o1 = *cast(Object*)p1;
        Object o2 = *cast(Object*)p2;
        int c = 0;

        // Regard null references as always being "less than"
        if (o1 !is o2)
        {
            if (o1)
            {
                if (!o2)
                    c = 1;
                else
                    c = o1.opCmp(o2);
            }
            else
                c = -1;
        }
        return c;
    }

    override @property size_t tsize() nothrow pure const
    {
        return Object.sizeof;
    }

    override const(void)[] initializer() nothrow pure const @safe
    {
        return m_init;
    }

    override @property uint flags() nothrow pure const { return 1; }

    override @property const(OffsetTypeInfo)[] offTi() nothrow pure const
    {
        return m_offTi;
    }

    final @property auto info() @safe @nogc nothrow pure const return { return this; }
    final @property auto typeinfo() @safe @nogc nothrow pure const return { return this; }

    byte[]      m_init;         /** class static initializer
                                 * (init.length gives size in bytes of class)
                                 */
    string      name;           /// class name
    void*[]     vtbl;           /// virtual function pointer table
    Interface[] interfaces;     /// interfaces this class implements
    TypeInfo_Class   base;      /// base class
    void*       destructor;
    void function(Object) classInvariant;
    enum ClassFlags : ushort
    {
        isCOMclass = 0x1,
        noPointers = 0x2,
        hasOffTi = 0x4,
        hasCtor = 0x8,
        hasGetMembers = 0x10,
        hasTypeInfo = 0x20,
        isAbstract = 0x40,
        isCPPclass = 0x80,
        hasDtor = 0x100,
        hasNameSig = 0x200,
    }
    ClassFlags m_flags;
    ushort     depth;           /// inheritance distance from Object
    void*      deallocator;
    OffsetTypeInfo[] m_offTi;
    void function(Object) defaultConstructor;   // default Constructor

    immutable(void)* m_RTInfo;        // data for precise GC
    override @property immutable(void)* rtInfo() const { return m_RTInfo; }

    uint[4] nameSig;            /// unique signature for `name`

    /**
     * Search all modules for TypeInfo_Class corresponding to classname.
     * Returns: null if not found
     */
    static const(TypeInfo_Class) find(const scope char[] classname)
    {
        foreach (m; ModuleInfo)
        {
            if (m)
            {
                //writefln("module %s, %d", m.name, m.localClasses.length);
                foreach (c; m.localClasses)
                {
                    if (c is null)
                        continue;
                    //writefln("\tclass %s", c.name);
                    if (c.name == classname)
                        return c;
                }
            }
        }
        return null;
    }

    /**
     * Create instance of Object represented by 'this'.
     */
    Object create() const
    {
        if (m_flags & 8 && !defaultConstructor)
            return null;
        if (m_flags & 64) // abstract
            return null;
        Object o = _d_newclass(this);
        if (m_flags & 8 && defaultConstructor)
        {
            defaultConstructor(o);
        }
        return o;
    }

   /**
    * Returns true if the class described by `child` derives from or is
    * the class described by this `TypeInfo_Class`. Always returns false
    * if the argument is null.
    *
    * Params:
    *  child = TypeInfo for some class
    * Returns:
    *  true if the class described by `child` derives from or is the
    *  class described by this `TypeInfo_Class`.
    */
    final bool isBaseOf(scope const TypeInfo_Class child) const @nogc nothrow pure @trusted
    {
        if (m_init.length)
        {
            // If this TypeInfo_Class represents an actual class we only need
            // to check the child and its direct ancestors.
            for (auto ti = cast() child; ti !is null; ti = ti.base)
                if (ti is this)
                    return true;
            return false;
        }
        else
        {
            // If this TypeInfo_Class is the .info field of a TypeInfo_Interface
            // we also need to recursively check the child's interfaces.
            return child !is null && _d_isbaseof(cast() child, this);
        }
    }
}

alias ClassInfo = TypeInfo_Class;

@safe unittest
{
    // Bugzilla 14401
    static class X
    {
        int a;
    }

    assert(typeid(X).initializer is typeid(X).m_init);
    assert(typeid(X).initializer.length == typeid(const(X)).initializer.length);
    assert(typeid(X).initializer.length == typeid(shared(X)).initializer.length);
    assert(typeid(X).initializer.length == typeid(immutable(X)).initializer.length);
}

class TypeInfo_Interface : TypeInfo
{
    override string toString() const pure { return info.name; }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto c = cast(const TypeInfo_Interface)o;
        return c && this.info.name == typeid(c).name;
    }

    override size_t getHash(scope const void* p) @trusted const
    {
        if (!*cast(void**)p)
        {
            return 0;
        }
        Interface* pi = **cast(Interface ***)*cast(void**)p;
        Object o = cast(Object)(*cast(void**)p - pi.offset);
        assert(o);
        return o.toHash();
    }

    override bool equals(in void* p1, in void* p2) const
    {
        Interface* pi = **cast(Interface ***)*cast(void**)p1;
        Object o1 = cast(Object)(*cast(void**)p1 - pi.offset);
        pi = **cast(Interface ***)*cast(void**)p2;
        Object o2 = cast(Object)(*cast(void**)p2 - pi.offset);

        return o1 == o2 || (o1 && o1.opCmp(o2) == 0);
    }

    override int compare(in void* p1, in void* p2) const
    {
        Interface* pi = **cast(Interface ***)*cast(void**)p1;
        Object o1 = cast(Object)(*cast(void**)p1 - pi.offset);
        pi = **cast(Interface ***)*cast(void**)p2;
        Object o2 = cast(Object)(*cast(void**)p2 - pi.offset);
        int c = 0;

        // Regard null references as always being "less than"
        if (o1 != o2)
        {
            if (o1)
            {
                if (!o2)
                    c = 1;
                else
                    c = o1.opCmp(o2);
            }
            else
                c = -1;
        }
        return c;
    }

    override @property size_t tsize() nothrow pure const
    {
        return Object.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. Object.sizeof];
    }

    override @property uint flags() nothrow pure const { return 1; }

    TypeInfo_Class info;

   /**
    * Returns true if the class described by `child` derives from the
    * interface described by this `TypeInfo_Interface`. Always returns
    * false if the argument is null.
    *
    * Params:
    *  child = TypeInfo for some class
    * Returns:
    *  true if the class described by `child` derives from the
    *  interface described by this `TypeInfo_Interface`.
    */
    final bool isBaseOf(scope const TypeInfo_Class child) const @nogc nothrow pure @trusted
    {
        return child !is null && _d_isbaseof(cast() child, this.info);
    }

   /**
    * Returns true if the interface described by `child` derives from
    * or is the interface described by this `TypeInfo_Interface`.
    * Always returns false if the argument is null.
    *
    * Params:
    *  child = TypeInfo for some interface
    * Returns:
    *  true if the interface described by `child` derives from or is
    *  the interface described by this `TypeInfo_Interface`.
    */
    final bool isBaseOf(scope const TypeInfo_Interface child) const @nogc nothrow pure @trusted
    {
        return child !is null && _d_isbaseof(cast() child.info, this.info);
    }
}

@safe unittest
{
    enum unittest_sym_name = __traits(identifier, __traits(parent, (){}));
    enum fqn_unittest = "object." ~ unittest_sym_name; // object.__unittest_LX_CY

    interface I {}

    assert(fqn_unittest ~ ".I" == typeid(I).info.name);
    assert((fqn_unittest ~ ".I").hashOf() == typeid(I).hashOf());
    assert(typeid(I).toHash() == typeid(I).hashOf());
}

class TypeInfo_Struct : TypeInfo
{
    override string toString() const { return name; }

    override size_t toHash() const
    {
        return hashOf(this.mangledName);
    }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;
        auto s = cast(const TypeInfo_Struct)o;
        return s && this.mangledName == s.mangledName;
    }

    override size_t getHash(scope const void* p) @trusted pure nothrow const
    {
        assert(p);
        if (xtoHash)
        {
            return (*xtoHash)(p);
        }
        else
        {
            return hashOf(p[0 .. initializer().length]);
        }
    }

    override bool equals(in void* p1, in void* p2) @trusted pure nothrow const
    {
        import core.stdc.string : memcmp;

        if (!p1 || !p2)
            return false;
        else if (xopEquals)
        {
            const dg = _memberFunc(p1, xopEquals);
            return dg.xopEquals(p2);
        }
        else if (p1 == p2)
            return true;
        else
            // BUG: relies on the GC not moving objects
            return memcmp(p1, p2, initializer().length) == 0;
    }

    override int compare(in void* p1, in void* p2) @trusted pure nothrow const
    {
        import core.stdc.string : memcmp;

        // Regard null references as always being "less than"
        if (p1 != p2)
        {
            if (p1)
            {
                if (!p2)
                    return true;
                else if (xopCmp)
                {
                    const dg = _memberFunc(p1, xopCmp);
                    return dg.xopCmp(p2);
                }
                else
                    // BUG: relies on the GC not moving objects
                    return memcmp(p1, p2, initializer().length);
            }
            else
                return -1;
        }
        return 0;
    }

    override @property size_t tsize() nothrow pure const
    {
        return initializer().length;
    }

    override const(void)[] initializer() nothrow pure const @safe
    {
        return m_init;
    }

    override @property uint flags() nothrow pure const { return m_flags; }

    override @property size_t talign() nothrow pure const { return m_align; }

    final override void destroy(void* p) const
    {
        if (xdtor)
        {
            if (m_flags & StructFlags.isDynamicType)
                (*xdtorti)(p, this);
            else
                (*xdtor)(p);
        }
    }

    override void postblit(void* p) const
    {
        if (xpostblit)
            (*xpostblit)(p);
    }

    string mangledName;

    final @property string name() nothrow const @trusted
    {
        import core.demangle : demangleType;

        if (mangledName is null) // e.g., opaque structs
            return null;

        const key = cast(const void*) this; // faster lookup than TypeInfo_Struct, at the cost of potential duplicates per binary
        static string[typeof(key)] demangledNamesCache; // per thread

        // not nothrow:
        //return demangledNamesCache.require(key, cast(string) demangleType(mangledName));

        if (auto pDemangled = key in demangledNamesCache)
            return *pDemangled;

        const demangled = cast(string) demangleType(mangledName);
        demangledNamesCache[key] = demangled;
        return demangled;
    }

    void[] m_init;      // initializer; m_init.ptr == null if 0 initialize

    @safe pure nothrow
    {
        size_t   function(in void*)           xtoHash;
        bool     function(in void*, in void*) xopEquals;
        int      function(in void*, in void*) xopCmp;
        string   function(in void*)           xtoString;

        enum StructFlags : uint
        {
            hasPointers = 0x1,
            isDynamicType = 0x2, // built at runtime, needs type info in xdtor
        }
        StructFlags m_flags;
    }
    union
    {
        void function(void*)                xdtor;
        void function(void*, const TypeInfo_Struct ti) xdtorti;
    }
    void function(void*)                    xpostblit;

    uint m_align;

    override @property immutable(void)* rtInfo() nothrow pure const @safe { return m_RTInfo; }

    version (WithArgTypes)
    {
        override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
        {
            arg1 = m_arg1;
            arg2 = m_arg2;
            return 0;
        }
        TypeInfo m_arg1;
        TypeInfo m_arg2;
    }
    immutable(void)* m_RTInfo;                // data for precise GC

    // The xopEquals and xopCmp members are function pointers to member
    // functions, which is not guaranteed to share the same ABI, as it is not
    // known whether the `this` parameter is the first or second argument.
    // This wrapper is to convert it to a delegate which will always pass the
    // `this` parameter in the correct way.
    private struct _memberFunc
    {
        union
        {
            struct // delegate
            {
                const void* ptr;
                const void* funcptr;
            }
            @safe pure nothrow
            {
                bool delegate(in void*) xopEquals;
                int delegate(in void*) xopCmp;
            }
        }
    }
}

@system unittest
{
    struct S
    {
        bool opEquals(ref const S rhs) const
        {
            return false;
        }
    }
    S s;
    assert(!typeid(S).equals(&s, &s));
}

class TypeInfo_Tuple : TypeInfo
{
    TypeInfo[] elements;

    override string toString() const
    {
        string s = "(";
        foreach (i, element; elements)
        {
            if (i)
                s ~= ',';
            s ~= element.toString();
        }
        s ~= ")";
        return s;
    }

    override bool opEquals(Object o)
    {
        if (this is o)
            return true;

        auto t = cast(const TypeInfo_Tuple)o;
        if (t && elements.length == t.elements.length)
        {
            for (size_t i = 0; i < elements.length; i++)
            {
                if (elements[i] != t.elements[i])
                    return false;
            }
            return true;
        }
        return false;
    }

    override size_t getHash(scope const void* p) const
    {
        assert(0);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        assert(0);
    }

    override int compare(in void* p1, in void* p2) const
    {
        assert(0);
    }

    override @property size_t tsize() nothrow pure const
    {
        assert(0);
    }

    override const(void)[] initializer() const @trusted
    {
        assert(0);
    }

    override void swap(void* p1, void* p2) const
    {
        assert(0);
    }

    override void destroy(void* p) const
    {
        assert(0);
    }

    override void postblit(void* p) const
    {
        assert(0);
    }

    override @property size_t talign() nothrow pure const
    {
        assert(0);
    }

    version (WithArgTypes) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        assert(0);
    }
}

class TypeInfo_Const : TypeInfo
{
    override string toString() const
    {
        return cast(string) ("const(" ~ base.toString() ~ ")");
    }

    //override bool opEquals(Object o) { return base.opEquals(o); }
    override bool opEquals(Object o)
    {
        if (this is o)
            return true;

        if (typeid(this) != typeid(o))
            return false;

        auto t = cast(TypeInfo_Const)o;
        return base.opEquals(t.base);
    }

    override size_t getHash(scope const void *p) const { return base.getHash(p); }
    override bool equals(in void *p1, in void *p2) const { return base.equals(p1, p2); }
    override int compare(in void *p1, in void *p2) const { return base.compare(p1, p2); }
    override @property size_t tsize() nothrow pure const { return base.tsize; }
    override void swap(void *p1, void *p2) const { return base.swap(p1, p2); }

    override @property inout(TypeInfo) next() nothrow pure inout { return base.next; }
    override @property uint flags() nothrow pure const { return base.flags; }

    override const(void)[] initializer() nothrow pure const
    {
        return base.initializer();
    }

    override @property size_t talign() nothrow pure const { return base.talign; }

    version (WithArgTypes) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        return base.argTypes(arg1, arg2);
    }

    TypeInfo base;
}

class TypeInfo_Invariant : TypeInfo_Const
{
    override string toString() const
    {
        return cast(string) ("immutable(" ~ base.toString() ~ ")");
    }
}

class TypeInfo_Shared : TypeInfo_Const
{
    override string toString() const
    {
        return cast(string) ("shared(" ~ base.toString() ~ ")");
    }
}

class TypeInfo_Inout : TypeInfo_Const
{
    override string toString() const
    {
        return cast(string) ("inout(" ~ base.toString() ~ ")");
    }
}

// Contents of Moduleinfo._flags
enum
{
    MIctorstart  = 0x1,   // we've started constructing it
    MIctordone   = 0x2,   // finished construction
    MIstandalone = 0x4,   // module ctor does not depend on other module
                        // ctors being done first
    MItlsctor    = 8,
    MItlsdtor    = 0x10,
    MIctor       = 0x20,
    MIdtor       = 0x40,
    MIxgetMembers = 0x80,
    MIictor      = 0x100,
    MIunitTest   = 0x200,
    MIimportedModules = 0x400,
    MIlocalClasses = 0x800,
    MIname       = 0x1000,
}

/*****************************************
 * An instance of ModuleInfo is generated into the object file for each compiled module.
 *
 * It provides access to various aspects of the module.
 * It is not generated for betterC.
 */
struct ModuleInfo
{
    uint _flags; // MIxxxx
    uint _index; // index into _moduleinfo_array[]

    version (all)
    {
        deprecated("ModuleInfo cannot be copy-assigned because it is a variable-sized struct.")
        void opAssign(const scope ModuleInfo m) { _flags = m._flags; _index = m._index; }
    }
    else
    {
        @disable this();
    }

const:
    private void* addrOf(int flag) return nothrow pure @nogc
    in
    {
        assert(flag >= MItlsctor && flag <= MIname);
        assert(!(flag & (flag - 1)) && !(flag & ~(flag - 1) << 1));
    }
    do
    {
        import core.stdc.string : strlen;

        void* p = cast(void*)&this + ModuleInfo.sizeof;

        if (flags & MItlsctor)
        {
            if (flag == MItlsctor) return p;
            p += typeof(tlsctor).sizeof;
        }
        if (flags & MItlsdtor)
        {
            if (flag == MItlsdtor) return p;
            p += typeof(tlsdtor).sizeof;
        }
        if (flags & MIctor)
        {
            if (flag == MIctor) return p;
            p += typeof(ctor).sizeof;
        }
        if (flags & MIdtor)
        {
            if (flag == MIdtor) return p;
            p += typeof(dtor).sizeof;
        }
        if (flags & MIxgetMembers)
        {
            if (flag == MIxgetMembers) return p;
            p += typeof(xgetMembers).sizeof;
        }
        if (flags & MIictor)
        {
            if (flag == MIictor) return p;
            p += typeof(ictor).sizeof;
        }
        if (flags & MIunitTest)
        {
            if (flag == MIunitTest) return p;
            p += typeof(unitTest).sizeof;
        }
        if (flags & MIimportedModules)
        {
            if (flag == MIimportedModules) return p;
            p += size_t.sizeof + *cast(size_t*)p * typeof(importedModules[0]).sizeof;
        }
        if (flags & MIlocalClasses)
        {
            if (flag == MIlocalClasses) return p;
            p += size_t.sizeof + *cast(size_t*)p * typeof(localClasses[0]).sizeof;
        }
        if (true || flags & MIname) // always available for now
        {
            if (flag == MIname) return p;
            p += strlen(cast(immutable char*)p);
        }
        assert(0);
    }

    @property uint index() nothrow pure @nogc { return _index; }

    @property uint flags() nothrow pure @nogc { return _flags; }

    /************************
     * Returns:
     *  module constructor for thread locals, `null` if there isn't one
     */
    @property void function() tlsctor() nothrow pure @nogc
    {
        return flags & MItlsctor ? *cast(typeof(return)*)addrOf(MItlsctor) : null;
    }

    /************************
     * Returns:
     *  module destructor for thread locals, `null` if there isn't one
     */
    @property void function() tlsdtor() nothrow pure @nogc
    {
        return flags & MItlsdtor ? *cast(typeof(return)*)addrOf(MItlsdtor) : null;
    }

    /*****************************
     * Returns:
     *  address of a module's `const(MemberInfo)[] getMembers(string)` function, `null` if there isn't one
     */
    @property void* xgetMembers() nothrow pure @nogc
    {
        return flags & MIxgetMembers ? *cast(typeof(return)*)addrOf(MIxgetMembers) : null;
    }

    /************************
     * Returns:
     *  module constructor, `null` if there isn't one
     */
    @property void function() ctor() nothrow pure @nogc
    {
        return flags & MIctor ? *cast(typeof(return)*)addrOf(MIctor) : null;
    }

    /************************
     * Returns:
     *  module destructor, `null` if there isn't one
     */
    @property void function() dtor() nothrow pure @nogc
    {
        return flags & MIdtor ? *cast(typeof(return)*)addrOf(MIdtor) : null;
    }

    /************************
     * Returns:
     *  module order independent constructor, `null` if there isn't one
     */
    @property void function() ictor() nothrow pure @nogc
    {
        return flags & MIictor ? *cast(typeof(return)*)addrOf(MIictor) : null;
    }

    /*************
     * Returns:
     *  address of function that runs the module's unittests, `null` if there isn't one
     */
    @property void function() unitTest() nothrow pure @nogc
    {
        return flags & MIunitTest ? *cast(typeof(return)*)addrOf(MIunitTest) : null;
    }

    /****************
     * Returns:
     *  array of pointers to the ModuleInfo's of modules imported by this one
     */
    @property immutable(ModuleInfo*)[] importedModules() return nothrow pure @nogc
    {
        if (flags & MIimportedModules)
        {
            auto p = cast(size_t*)addrOf(MIimportedModules);
            return (cast(immutable(ModuleInfo*)*)(p + 1))[0 .. *p];
        }
        return null;
    }

    /****************
     * Returns:
     *  array of TypeInfo_Class references for classes defined in this module
     */
    @property TypeInfo_Class[] localClasses() return nothrow pure @nogc
    {
        if (flags & MIlocalClasses)
        {
            auto p = cast(size_t*)addrOf(MIlocalClasses);
            return (cast(TypeInfo_Class*)(p + 1))[0 .. *p];
        }
        return null;
    }

    /********************
     * Returns:
     *  name of module, `null` if no name
     */
    @property string name() return nothrow pure @nogc
    {
        import core.stdc.string : strlen;

        auto p = cast(immutable char*) addrOf(MIname);
        return p[0 .. strlen(p)];
    }

    static int opApply(scope int delegate(ModuleInfo*) dg)
    {
        import core.internal.traits : externDFunc;
        alias moduleinfos_apply = externDFunc!("rt.minfo.moduleinfos_apply",
                                              int function(scope int delegate(immutable(ModuleInfo*))));
        // Bugzilla 13084 - enforcing immutable ModuleInfo would break client code
        return moduleinfos_apply(
            (immutable(ModuleInfo*)m) => dg(cast(ModuleInfo*)m));
    }
}

@system unittest
{
    ModuleInfo* m1;
    foreach (m; ModuleInfo)
    {
        m1 = m;
    }
}

///////////////////////////////////////////////////////////////////////////////
// Throwable
///////////////////////////////////////////////////////////////////////////////


/**
 * The base class of all thrown objects.
 *
 * All thrown objects must inherit from Throwable. Class $(D Exception), which
 * derives from this class, represents the category of thrown objects that are
 * safe to catch and handle. In principle, one should not catch Throwable
 * objects that are not derived from $(D Exception), as they represent
 * unrecoverable runtime errors. Certain runtime guarantees may fail to hold
 * when these errors are thrown, making it unsafe to continue execution after
 * catching them.
 */
class Throwable : Object
{
    interface TraceInfo
    {
        int opApply(scope int delegate(ref const(char[]))) const;
        int opApply(scope int delegate(ref size_t, ref const(char[]))) const;
        string toString() const;
    }

    alias TraceDeallocator = void function(TraceInfo) nothrow;

    string      msg;    /// A message describing the error.

    /**
     * The _file name of the D source code corresponding with
     * where the error was thrown from.
     */
    string      file;
    /**
     * The _line number of the D source code corresponding with
     * where the error was thrown from.
     */
    size_t      line;

    /**
     * The stack trace of where the error happened. This is an opaque object
     * that can either be converted to $(D string), or iterated over with $(D
     * foreach) to extract the items in the stack trace (as strings).
     */
    TraceInfo   info;

    /**
     * If set, this is used to deallocate the TraceInfo on destruction.
     */
    TraceDeallocator infoDeallocator;


    /**
     * A reference to the _next error in the list. This is used when a new
     * $(D Throwable) is thrown from inside a $(D catch) block. The originally
     * caught $(D Exception) will be chained to the new $(D Throwable) via this
     * field.
     */
    private Throwable   nextInChain;

    private uint _refcount;     // 0 : allocated by GC
                                // 1 : allocated by _d_newThrowable()
                                // 2.. : reference count + 1

    /**
     * Returns:
     * A reference to the _next error in the list. This is used when a new
     * $(D Throwable) is thrown from inside a $(D catch) block. The originally
     * caught $(D Exception) will be chained to the new $(D Throwable) via this
     * field.
     */
    @property inout(Throwable) next() @safe inout return scope pure nothrow @nogc { return nextInChain; }

    /**
     * Replace next in chain with `tail`.
     * Use `chainTogether` instead if at all possible.
     */
    @property void next(Throwable tail) @safe scope pure nothrow @nogc
    {
        if (tail && tail._refcount)
            ++tail._refcount;           // increment the replacement *first*

        auto n = nextInChain;
        nextInChain = null;             // sever the tail before deleting it

        if (n && n._refcount)
            _d_delThrowable(n);         // now delete the old tail

        nextInChain = tail;             // and set the new tail
    }

    /**
     * Returns:
     *  mutable reference to the reference count, which is
     *  0 - allocated by the GC, 1 - allocated by _d_newThrowable(),
     *  and >=2 which is the reference count + 1
     * Note:
     *  Marked as `@system` to discourage casual use of it.
     */
    @system @nogc final pure nothrow ref uint refcount() return { return _refcount; }

    /**
     * Loop over the chain of Throwables.
     */
    int opApply(scope int delegate(Throwable) dg)
    {
        int result = 0;
        for (Throwable t = this; t; t = t.nextInChain)
        {
            result = dg(t);
            if (result)
                break;
        }
        return result;
    }

    /**
     * Append `e2` to chain of exceptions that starts with `e1`.
     * Params:
     *  e1 = start of chain (can be null)
     *  e2 = second part of chain (can be null)
     * Returns:
     *  Throwable that is at the start of the chain; null if both `e1` and `e2` are null
     */
    static @__future @system @nogc pure nothrow Throwable chainTogether(return scope Throwable e1, return scope Throwable e2)
    {
        if (!e1)
            return e2;
        if (!e2)
            return e1;
        if (e2.refcount())
            ++e2.refcount();

        for (auto e = e1; 1; e = e.nextInChain)
        {
            if (!e.nextInChain)
            {
                e.nextInChain = e2;
                break;
            }
        }
        return e1;
    }

    @nogc @safe pure nothrow this(string msg, Throwable nextInChain = null)
    {
        this.msg = msg;
        this.nextInChain = nextInChain;
        if (nextInChain && nextInChain._refcount)
            ++nextInChain._refcount;
        //this.info = _d_traceContext();
    }

    @nogc @safe pure nothrow this(string msg, string file, size_t line, Throwable nextInChain = null)
    {
        this(msg, nextInChain);
        this.file = file;
        this.line = line;
        //this.info = _d_traceContext();
    }

    @trusted nothrow ~this()
    {
        if (nextInChain && nextInChain._refcount)
            _d_delThrowable(nextInChain);
        // handle owned traceinfo
        if (infoDeallocator !is null)
        {
            infoDeallocator(info);
            info = null; // avoid any kind of dangling pointers if we can help
                         // it.
        }
    }

    /**
     * Overrides $(D Object.toString) and returns the error message.
     * Internally this forwards to the $(D toString) overload that
     * takes a $(D_PARAM sink) delegate.
     */
    override string toString()
    {
        string s;
        toString((in buf) { s ~= buf; });
        return s;
    }

    /**
     * The Throwable hierarchy uses a toString overload that takes a
     * $(D_PARAM _sink) delegate to avoid GC allocations, which cannot be
     * performed in certain error situations.  Override this $(D
     * toString) method to customize the error message.
     */
    void toString(scope void delegate(in char[]) sink) const
    {
        import core.internal.string : unsignedToTempString;

        char[20] tmpBuff = void;

        sink(typeid(this).name);
        sink("@"); sink(file);
        sink("("); sink(unsignedToTempString(line, tmpBuff)); sink(")");

        if (msg.length)
        {
            sink(": "); sink(msg);
        }
        if (info)
        {
            try
            {
                sink("\n----------------");
                foreach (t; info)
                {
                    sink("\n"); sink(t);
                }
            }
            catch (Throwable)
            {
                // ignore more errors
            }
        }
    }

    /**
     * Get the message describing the error.
     *
     * This getter is an alternative way to access the Exception's message,
     * with the added advantage of being override-able in subclasses.
     * Subclasses are hence free to do their own memory managements without
     * being tied to the requirement of providing a `string` in a field.
     *
     * The default behavior is to return the `Throwable.msg` field.
     *
     * Returns:
     *  A message representing the cause of the `Throwable`
     */
    @__future const(char)[] message() const @safe nothrow
    {
        return this.msg;
    }
}


/**
 * The base class of all errors that are safe to catch and handle.
 *
 * In principle, only thrown objects derived from this class are safe to catch
 * inside a $(D catch) block. Thrown objects not derived from Exception
 * represent runtime errors that should not be caught, as certain runtime
 * guarantees may not hold, making it unsafe to continue program execution.
 */
class Exception : Throwable
{

    /**
     * Creates a new instance of Exception. The nextInChain parameter is used
     * internally and should always be $(D null) when passed by user code.
     * This constructor does not automatically throw the newly-created
     * Exception; the $(D throw) expression should be used for that purpose.
     */
    @nogc @safe pure nothrow this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null)
    {
        super(msg, file, line, nextInChain);
    }

    @nogc @safe pure nothrow this(string msg, Throwable nextInChain, string file = __FILE__, size_t line = __LINE__)
    {
        super(msg, file, line, nextInChain);
    }
}

///
@safe unittest
{
    bool gotCaught;
    try
    {
        throw new Exception("msg");
    }
    catch (Exception e)
    {
        gotCaught = true;
        assert(e.msg == "msg");
    }
    assert(gotCaught);
}

@system unittest
{
    {
        auto e = new Exception("msg");
        assert(e.file == __FILE__);
        assert(e.line == __LINE__ - 2);
        assert(e.nextInChain is null);
        assert(e.msg == "msg");
    }

    {
        auto e = new Exception("msg", new Exception("It's an Exception!"), "hello", 42);
        assert(e.file == "hello");
        assert(e.line == 42);
        assert(e.nextInChain !is null);
        assert(e.msg == "msg");
    }

    {
        auto e = new Exception("msg", "hello", 42, new Exception("It's an Exception!"));
        assert(e.file == "hello");
        assert(e.line == 42);
        assert(e.nextInChain !is null);
        assert(e.msg == "msg");
    }

    {
        auto e = new Exception("message");
        assert(e.message == "message");
    }
}


/**
 * The base class of all unrecoverable runtime errors.
 *
 * This represents the category of $(D Throwable) objects that are $(B not)
 * safe to catch and handle. In principle, one should not catch Error
 * objects, as they represent unrecoverable runtime errors.
 * Certain runtime guarantees may fail to hold when these errors are
 * thrown, making it unsafe to continue execution after catching them.
 */
class Error : Throwable
{
    /**
     * Creates a new instance of Error. The nextInChain parameter is used
     * internally and should always be $(D null) when passed by user code.
     * This constructor does not automatically throw the newly-created
     * Error; the $(D throw) statement should be used for that purpose.
     */
    @nogc @safe pure nothrow this(string msg, Throwable nextInChain = null)
    {
        super(msg, nextInChain);
        bypassedException = null;
    }

    @nogc @safe pure nothrow this(string msg, string file, size_t line, Throwable nextInChain = null)
    {
        super(msg, file, line, nextInChain);
        bypassedException = null;
    }

    /** The first $(D Exception) which was bypassed when this Error was thrown,
    or $(D null) if no $(D Exception)s were pending. */
    Throwable   bypassedException;
}

///
@system unittest
{
    bool gotCaught;
    try
    {
        throw new Error("msg");
    }
    catch (Error e)
    {
        gotCaught = true;
        assert(e.msg == "msg");
    }
    assert(gotCaught);
}

@safe unittest
{
    {
        auto e = new Error("msg");
        assert(e.file is null);
        assert(e.line == 0);
        assert(e.nextInChain is null);
        assert(e.msg == "msg");
        assert(e.bypassedException is null);
    }

    {
        auto e = new Error("msg", new Exception("It's an Exception!"));
        assert(e.file is null);
        assert(e.line == 0);
        assert(e.nextInChain !is null);
        assert(e.msg == "msg");
        assert(e.bypassedException is null);
    }

    {
        auto e = new Error("msg", "hello", 42, new Exception("It's an Exception!"));
        assert(e.file == "hello");
        assert(e.line == 42);
        assert(e.nextInChain !is null);
        assert(e.msg == "msg");
        assert(e.bypassedException is null);
    }
}

extern (C)
{
    // from druntime/src/rt/aaA.d

    private struct AA { void* impl; }
    // size_t _aaLen(in AA aa) pure nothrow @nogc;
    private void* _aaGetY(scope AA* paa, const TypeInfo_AssociativeArray ti, const size_t valsz, const scope void* pkey) pure nothrow;
    private void* _aaGetX(scope AA* paa, const TypeInfo_AssociativeArray ti, const size_t valsz, const scope void* pkey, out bool found) pure nothrow;
    // inout(void)* _aaGetRvalueX(inout AA aa, in TypeInfo keyti, in size_t valsz, in void* pkey);
    inout(void[]) _aaValues(inout AA aa, const size_t keysz, const size_t valsz, const TypeInfo tiValueArray) pure nothrow;
    inout(void[]) _aaKeys(inout AA aa, const size_t keysz, const TypeInfo tiKeyArray) pure nothrow;
    void* _aaRehash(AA* paa, const scope TypeInfo keyti) pure nothrow;
    void _aaClear(AA aa) pure nothrow;

    // alias _dg_t = extern(D) int delegate(void*);
    // int _aaApply(AA aa, size_t keysize, _dg_t dg);

    // alias _dg2_t = extern(D) int delegate(void*, void*);
    // int _aaApply2(AA aa, size_t keysize, _dg2_t dg);

    private struct AARange { AA impl; size_t idx; }
    AARange _aaRange(AA aa) pure nothrow @nogc @safe;
    bool _aaRangeEmpty(AARange r) pure nothrow @nogc @safe;
    void* _aaRangeFrontKey(AARange r) pure nothrow @nogc @safe;
    void* _aaRangeFrontValue(AARange r) pure nothrow @nogc @safe;
    void _aaRangePopFront(ref AARange r) pure nothrow @nogc @safe;

    int _aaEqual(scope const TypeInfo tiRaw, scope const AA aa1, scope const AA aa2);
    hash_t _aaGetHash(scope const AA* aa, scope const TypeInfo tiRaw) nothrow;

    /*
        _d_assocarrayliteralTX marked as pure, because aaLiteral can be called from pure code.
        This is a typesystem hole, however this is existing hole.
        Early compiler didn't check purity of toHash or postblit functions, if key is a UDT thus
        copiler allowed to create AA literal with keys, which have impure unsafe toHash methods.
    */
    void* _d_assocarrayliteralTX(const TypeInfo_AssociativeArray ti, void[] keys, void[] values) pure;
}

void* aaLiteral(Key, Value)(Key[] keys, Value[] values) @trusted pure
{
    return _d_assocarrayliteralTX(typeid(Value[Key]), *cast(void[]*)&keys, *cast(void[]*)&values);
}

// Lower an Associative Array to a newaa struct for static initialization.
auto _aaAsStruct(K, V)(V[K] aa) @safe
{
    import core.internal.newaa : makeAA;
    assert(__ctfe);
    return makeAA!(K, V)(aa);
}

alias AssociativeArray(Key, Value) = Value[Key];

/***********************************
 * Removes all remaining keys and values from an associative array.
 * Params:
 *      aa =     The associative array.
 */
void clear(Value, Key)(Value[Key] aa) @trusted
{
    _aaClear(*cast(AA *) &aa);
}

/** ditto */
void clear(Value, Key)(Value[Key]* aa) @trusted
{
    _aaClear(*cast(AA *) aa);
}

///
@safe unittest
{
    auto aa = ["k1": 2];
    aa.clear;
    assert("k1" !in aa);
}

// Issue 20559
@system unittest
{
    static class Foo
    {
        int[string] aa;
        alias aa this;
    }

    auto v = new Foo();
    v["Hello World"] = 42;
    v.clear;
    assert("Hello World" !in v);

    // Test for T*
    static assert(!__traits(compiles, (&v).clear));
    static assert( __traits(compiles, (*(&v)).clear));
}

/***********************************
 * Reorganizes the associative array in place so that lookups are more
 * efficient.
 * Params:
 *      aa =     The associative array.
 * Returns:
 *      The rehashed associative array.
 */
T rehash(T : Value[Key], Value, Key)(T aa)
{
    _aaRehash(cast(AA*)&aa, typeid(Value[Key]));
    return aa;
}

/** ditto */
T rehash(T : Value[Key], Value, Key)(T* aa)
{
    _aaRehash(cast(AA*)aa, typeid(Value[Key]));
    return *aa;
}

/** ditto */
T rehash(T : shared Value[Key], Value, Key)(T aa)
{
    _aaRehash(cast(AA*)&aa, typeid(Value[Key]));
    return aa;
}

/** ditto */
T rehash(T : shared Value[Key], Value, Key)(T* aa)
{
    _aaRehash(cast(AA*)aa, typeid(Value[Key]));
    return *aa;
}

/***********************************
 * Creates a new associative array of the same size and copies the contents of
 * the associative array into it.
 * Params:
 *      aa =     The associative array.
 */
V[K] dup(T : V[K], K, V)(T aa)
{
    //pragma(msg, "K = ", K, ", V = ", V);

    // Bug10720 - check whether V is copyable
    static assert(is(typeof({ V v = aa[K.init]; })),
        "cannot call " ~ T.stringof ~ ".dup because " ~ V.stringof ~ " is not copyable");

    V[K] result;

    //foreach (k, ref v; aa)
    //    result[k] = v;  // Bug13701 - won't work if V is not mutable

    ref V duplicateElem(ref K k, ref const V v) @trusted pure nothrow
    {
        import core.stdc.string : memcpy;

        void* pv = _aaGetY(cast(AA*)&result, typeid(V[K]), V.sizeof, &k);
        memcpy(pv, &v, V.sizeof);
        return *cast(V*)pv;
    }

    foreach (k, ref v; aa)
    {
        static if (!__traits(hasPostblit, V))
            duplicateElem(k, v);
        else static if (__traits(isStaticArray, V))
            _doPostblit(duplicateElem(k, v)[]);
        else static if (!is(typeof(v.__xpostblit())) && is(immutable V == immutable UV, UV))
            (() @trusted => *cast(UV*) &duplicateElem(k, v))().__xpostblit();
        else
            duplicateElem(k, v).__xpostblit();
    }

    return result;
}

/** ditto */
V[K] dup(T : V[K], K, V)(T* aa)
{
    return (*aa).dup;
}

///
@safe unittest
{
    auto aa = ["k1": 2];
    auto a2 = aa.dup;
    aa["k2"] = 3;
    assert("k2" !in a2);
}

// this should never be made public.
private AARange _aaToRange(T: V[K], K, V)(ref T aa) pure nothrow @nogc @safe
{
    // ensure we are dealing with a genuine AA.
    static if (is(const(V[K]) == const(T)))
        alias realAA = aa;
    else
        const(V[K]) realAA = aa;
    return _aaRange(() @trusted { return *cast(AA*)&realAA; } ());
}

/***********************************
 * Returns a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
 * which will iterate over the keys of the associative array. The keys are
 * returned by reference.
 *
 * If structural changes are made to the array (removing or adding keys), all
 * ranges previously obtained through this function are invalidated. The
 * following example program will dereference a null pointer:
 *
 *---
 * import std.stdio : writeln;
 *
 * auto dict = ["k1": 1, "k2": 2];
 * auto keyRange = dict.byKey;
 * dict.clear;
 * writeln(keyRange.front);    // Segmentation fault
 *---
 *
 * Params:
 *      aa =     The associative array.
 * Returns:
 *      A forward range referencing the keys of the associative array.
 */
auto byKey(T : V[K], K, V)(T aa) pure nothrow @nogc @safe
{
    import core.internal.traits : substInout;

    static struct Result
    {
        AARange r;

    pure nothrow @nogc:
        @property bool empty()  @safe { return _aaRangeEmpty(r); }
        @property ref front() @trusted
        {
            return *cast(substInout!K*) _aaRangeFrontKey(r);
        }
        void popFront() @safe { _aaRangePopFront(r); }
        @property Result save() { return this; }
    }

    return Result(_aaToRange(aa));
}

/** ditto */
auto byKey(T : V[K], K, V)(T* aa) pure nothrow @nogc
{
    return (*aa).byKey();
}

///
@safe unittest
{
    auto dict = [1: "v1", 2: "v2"];
    int sum;
    foreach (v; dict.byKey)
        sum += v;

    assert(sum == 3);
}

/***********************************
 * Returns a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
 * which will iterate over the values of the associative array. The values are
 * returned by reference.
 *
 * If structural changes are made to the array (removing or adding keys), all
 * ranges previously obtained through this function are invalidated. The
 * following example program will dereference a null pointer:
 *
 *---
 * import std.stdio : writeln;
 *
 * auto dict = ["k1": 1, "k2": 2];
 * auto valueRange = dict.byValue;
 * dict.clear;
 * writeln(valueRange.front);    // Segmentation fault
 *---
 *
 * Params:
 *      aa =     The associative array.
 * Returns:
 *      A forward range referencing the values of the associative array.
 */
auto byValue(T : V[K], K, V)(T aa) pure nothrow @nogc @safe
{
    import core.internal.traits : substInout;

    static struct Result
    {
        AARange r;

    pure nothrow @nogc:
        @property bool empty() @safe { return _aaRangeEmpty(r); }
        @property ref front() @trusted
        {
            return *cast(substInout!V*) _aaRangeFrontValue(r);
        }
        void popFront() @safe { _aaRangePopFront(r); }
        @property Result save() { return this; }
    }

    return Result(_aaToRange(aa));
}

/** ditto */
auto byValue(T : V[K], K, V)(T* aa) pure nothrow @nogc
{
    return (*aa).byValue();
}

///
@safe unittest
{
    auto dict = ["k1": 1, "k2": 2];
    int sum;
    foreach (v; dict.byValue)
        sum += v;

    assert(sum == 3);

    foreach (ref v; dict.byValue)
        v++;
    assert(dict == ["k1": 2, "k2": 3]);
}

/***********************************
 * Returns a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
 * which will iterate over the key-value pairs of the associative array. The
 * returned pairs are represented by an opaque type with `.key` and `.value`
 * properties for accessing references to the key and value of the pair,
 * respectively.
 *
 * If structural changes are made to the array (removing or adding keys), all
 * ranges previously obtained through this function are invalidated. The
 * following example program will dereference a null pointer:
 *
 *---
 * import std.stdio : writeln;
 *
 * auto dict = ["k1": 1, "k2": 2];
 * auto kvRange = dict.byKeyValue;
 * dict.clear;
 * writeln(kvRange.front.key, ": ", kvRange.front.value);    // Segmentation fault
 *---
 *
 * Note that this is a low-level interface to iterating over the associative
 * array and is not compatible with the
 * $(LINK2 $(ROOT_DIR)phobos/std_typecons.html#.Tuple,`Tuple`) type in Phobos.
 * For compatibility with `Tuple`, use
 * $(LINK2 $(ROOT_DIR)phobos/std_array.html#.byPair,std.array.byPair) instead.
 *
 * Params:
 *      aa =     The associative array.
 * Returns:
 *      A forward range referencing the pairs of the associative array.
 */
auto byKeyValue(T : V[K], K, V)(T aa) pure nothrow @nogc @safe
{
    import core.internal.traits : substInout;

    static struct Result
    {
        AARange r;

    pure nothrow @nogc:
        @property bool empty() @safe { return _aaRangeEmpty(r); }
        @property auto front()
        {
            static struct Pair
            {
                // We save the pointers here so that the Pair we return
                // won't mutate when Result.popFront is called afterwards.
                private void* keyp;
                private void* valp;

                @property ref key() inout @trusted
                {
                    return *cast(substInout!K*) keyp;
                }
                @property ref value() inout @trusted
                {
                    return *cast(substInout!V*) valp;
                }
            }
            return Pair(_aaRangeFrontKey(r),
                        _aaRangeFrontValue(r));
        }
        void popFront() @safe { return _aaRangePopFront(r); }
        @property Result save() { return this; }
    }

    return Result(_aaToRange(aa));
}

/** ditto */
auto byKeyValue(T : V[K], K, V)(T* aa) pure nothrow @nogc
{
    return (*aa).byKeyValue();
}

///
@safe unittest
{
    auto dict = ["k1": 1, "k2": 2];
    int sum;
    foreach (e; dict.byKeyValue)
    {
        assert(e.key[1] == e.value + '0');
        sum += e.value;
    }
    assert(sum == 3);

    foreach (e; dict.byKeyValue)
        e.value++;
    assert(dict == ["k1": 2, "k2": 3]);
}

/***********************************
 * Returns a newly allocated dynamic array containing a copy of the keys from
 * the associative array.
 * Params:
 *      aa =     The associative array.
 * Returns:
 *      A dynamic array containing a copy of the keys.
 */
Key[] keys(T : Value[Key], Value, Key)(T aa) @property
{
    // ensure we are dealing with a genuine AA.
    static if (is(const(Value[Key]) == const(T)))
        alias realAA = aa;
    else
        const(Value[Key]) realAA = aa;
    auto res = () @trusted {
        auto a = cast(void[])_aaKeys(*cast(inout(AA)*)&realAA, Key.sizeof, typeid(Key[]));
        return *cast(Key[]*)&a;
    }();
    static if (__traits(hasPostblit, Key))
        _doPostblit(res);
    return res;
}

/** ditto */
Key[] keys(T : Value[Key], Value, Key)(T *aa) @property
{
    return (*aa).keys;
}

///
@safe unittest
{
    auto aa = [1: "v1", 2: "v2"];
    int sum;
    foreach (k; aa.keys)
        sum += k;

    assert(sum == 3);
}

@safe unittest
{
    static struct S
    {
        string str;
        void[][string] dict;
        alias dict this;
    }

    auto s = S("a");
    assert(s.keys.length == 0);
}

@safe unittest
{
    @safe static struct Key
    {
         string str;
         this(this) @safe {}
    }
    string[Key] aa;
    static assert(__traits(compiles, {
                void test() @safe {
                    const _ = aa.keys;
                }
            }));
}

@safe unittest
{
    static struct Key
    {
        string str;
        this(this) @system {}
    }
    string[Key] aa;
    static assert(!__traits(compiles, {
                void test() @safe {
                    const _ = aa.keys;
                }
            }));
}

/***********************************
 * Returns a newly allocated dynamic array containing a copy of the values from
 * the associative array.
 * Params:
 *      aa =     The associative array.
 * Returns:
 *      A dynamic array containing a copy of the values.
 */
Value[] values(T : Value[Key], Value, Key)(T aa) @property
{
    // ensure we are dealing with a genuine AA.
    static if (is(const(Value[Key]) == const(T)))
        alias realAA = aa;
    else
        const(Value[Key]) realAA = aa;
    auto res = () @trusted {
        auto a = cast(void[])_aaValues(*cast(inout(AA)*)&realAA, Key.sizeof, Value.sizeof, typeid(Value[]));
        return *cast(Value[]*)&a;
    }();
    static if (__traits(hasPostblit, Value))
        _doPostblit(res);
    return res;
}

/** ditto */
Value[] values(T : Value[Key], Value, Key)(T *aa) @property
{
    return (*aa).values;
}

///
@safe unittest
{
    auto aa = ["k1": 1, "k2": 2];
    int sum;
    foreach (e; aa.values)
        sum += e;

    assert(sum == 3);
}

@safe unittest
{
    static struct S
    {
        string str;
        void[][string] dict;
        alias dict this;
    }

    auto s = S("a");
    assert(s.values.length == 0);
}

@safe unittest
{
    @safe static struct Value
    {
        string str;
        this(this) @safe {}
    }
    Value[string] aa;
    static assert(__traits(compiles, {
                void test() @safe {
                    const _ = aa.values;
                }
            }));
}

@safe unittest
{
    static struct Value
    {
        string str;
        this(this) @system {}
    }
    Value[string] aa;
    static assert(!__traits(compiles, {
                void test() @safe {
                    const _ = aa.values;
                }
            }));
}

/***********************************
 * If `key` is in `aa`, returns corresponding value; otherwise it evaluates and
 * returns `defaultValue`.
 * Params:
 *      aa =     The associative array.
 *      key =    The key.
 *      defaultValue = The default value.
 * Returns:
 *      The value.
 */
inout(V) get(K, V)(inout(V[K]) aa, K key, lazy inout(V) defaultValue)
{
    auto p = key in aa;
    return p ? *p : defaultValue;
}

/** ditto */
inout(V) get(K, V)(inout(V[K])* aa, K key, lazy inout(V) defaultValue)
{
    return (*aa).get(key, defaultValue);
}

///
@safe unittest
{
    auto aa = ["k1": 1];
    assert(aa.get("k1", 0) == 1);
    assert(aa.get("k2", 0) == 0);
}

/***********************************
 * If `key` is in `aa`, returns corresponding value; otherwise it evaluates
 * `value`, adds it to the associative array and returns it.
 * Params:
 *      aa =     The associative array.
 *      key =    The key.
 *      value =  The required value.
 * Returns:
 *      The value.
 */
ref V require(K, V)(ref V[K] aa, K key, lazy V value = V.init)
{
    bool found;
    // if key is @safe-ly copyable, `require` can infer @safe
    static if (isSafeCopyable!K)
    {
        auto p = () @trusted
        {
            return cast(V*) _aaGetX(cast(AA*) &aa, typeid(V[K]), V.sizeof, &key, found);
        } ();
    }
    else
    {
        auto p = cast(V*) _aaGetX(cast(AA*) &aa, typeid(V[K]), V.sizeof, &key, found);
    }
    if (found)
        return *p;
    else
    {
        *p = value; // Not `return (*p = value)` since if `=` is overloaded
        return *p;  // this might not return a ref to the left-hand side.
    }
}

///
@safe unittest
{
    auto aa = ["k1": 1];
    assert(aa.require("k1", 0) == 1);
    assert(aa.require("k2", 0) == 0);
    assert(aa["k2"] == 0);
}

// Tests whether T can be @safe-ly copied. Use a union to exclude destructor from the test.
private enum bool isSafeCopyable(T) = is(typeof(() @safe { union U { T x; } T *x; auto u = U(*x); }));

/***********************************
 * Calls `create` if `key` doesn't exist in the associative array,
 * otherwise calls `update`.
 * `create` returns a corresponding value for `key`.
 * `update` accepts a key parameter. If it returns a value, the value is
 * set for `key`.
 * Params:
 *      aa =     The associative array.
 *      key =    The key.
 *      create = The callable to create a value for `key`.
 *               Must return V.
 *      update = The callable to call if `key` exists.
 *               Takes a K argument, returns a V or void.
 */
void update(K, V, C, U)(ref V[K] aa, K key, scope C create, scope U update)
if (is(typeof(create()) : V) && (is(typeof(update(aa[K.init])) : V) || is(typeof(update(aa[K.init])) == void)))
{
    bool found;
    // if key is @safe-ly copyable, `update` may infer @safe
    static if (isSafeCopyable!K)
    {
        auto p = () @trusted
        {
            return cast(V*) _aaGetX(cast(AA*) &aa, typeid(V[K]), V.sizeof, &key, found);
        } ();
    }
    else
    {
        auto p = cast(V*) _aaGetX(cast(AA*) &aa, typeid(V[K]), V.sizeof, &key, found);
    }
    if (!found)
        *p = create();
    else
    {
        static if (is(typeof(update(*p)) == void))
            update(*p);
        else
            *p = update(*p);
    }
}

///
@safe unittest
{
    int[string] aa;

    // create
    aa.update("key",
        () => 1,
        (int) {} // not executed
        );
    assert(aa["key"] == 1);

    // update value by ref
    aa.update("key",
        () => 0, // not executed
        (ref int v) {
            v += 1;
        });
    assert(aa["key"] == 2);

    // update from return value
    aa.update("key",
        () => 0, // not executed
        (int v) => v * 2
        );
    assert(aa["key"] == 4);

    // 'update' without changing value
    aa.update("key",
        () => 0, // not executed
        (int) {
            // do something else
        });
    assert(aa["key"] == 4);
}

@safe unittest
{
    static struct S
    {
        int x;
    @nogc nothrow pure:
        this(this) @system {}

    @safe const:
        // stubs
        bool opEquals(S rhs) { assert(0); }
        size_t toHash() const { assert(0); }
    }

    int[string] aai;
    static assert(is(typeof(() @safe { aai.require("a", 1234); })));
    static assert(is(typeof(() @safe { aai.update("a", { return 1234; }, (ref int x) { x++; return x; }); })));

    S[string] aas;
    static assert(is(typeof(() { aas.require("a", S(1234)); })));
    static assert(is(typeof(() { aas.update("a", { return S(1234); }, (ref S s) { s.x++; return s; }); })));
    static assert(!is(typeof(() @safe { aas.update("a", { return S(1234); }, (ref S s) { s.x++; return s; }); })));

    int[S] aais;
    static assert(is(typeof(() { aais.require(S(1234), 1234); })));
    static assert(is(typeof(() { aais.update(S(1234), { return 1234; }, (ref int x) { x++; return x; }); })));
    static assert(!is(typeof(() @safe { aais.require(S(1234), 1234); })));
    static assert(!is(typeof(() @safe { aais.update(S(1234), { return 1234; }, (ref int x) { x++; return x; }); })));
}

@safe unittest
{
    struct S0
    {
        int opCall(ref int v)
        {
            return v + 1;
        }
    }

    struct S1
    {
        int opCall()()
        {
            return -2;
        }

        T opCall(T)(ref T v)
        {
            return v + 1;
        }
    }

    int[string] a = ["2" : 1];
    a.update("2", () => -1, S0.init);
    assert(a["2"] == 2);
    a.update("0", () => -1, S0.init);
    assert(a["0"] == -1);
    a.update("2", S1.init, S1.init);
    assert(a["2"] == 3);
    a.update("1", S1.init, S1.init);
    assert(a["1"] == -2);
}

@system unittest
{
    int[string] aa;

    foreach (n; 0 .. 2)
        aa.update("k1", {
            return 7;
        }, (ref int v) {
            return v + 3;
        });
    assert(aa["k1"] == 10);
}

version (CoreDdoc)
{
    // This lets DDoc produce better documentation.

    /**
    Calculates the hash value of `arg` with an optional `seed` initial value.
    The result might not be equal to `typeid(T).getHash(&arg)`.

    Params:
        arg = argument to calculate the hash value of
        seed = optional `seed` value (may be used for hash chaining)

    Return: calculated hash value of `arg`
    */
    size_t hashOf(T)(auto ref T arg, size_t seed)
    {
        static import core.internal.hash;
        return core.internal.hash.hashOf(arg, seed);
    }
    /// ditto
    size_t hashOf(T)(auto ref T arg)
    {
        static import core.internal.hash;
        return core.internal.hash.hashOf(arg);
    }

    @safe unittest
    {
        auto h1 = "my.string".hashOf;
        assert(h1 == "my.string".hashOf);
    }
}
else
{
    public import core.internal.hash : hashOf;
}

///
@system unittest
{
    class MyObject
    {
        size_t myMegaHash() const @safe pure nothrow
        {
            return 42;
        }
    }
    struct Test
    {
        int a;
        string b;
        MyObject c;
        size_t toHash() const pure nothrow
        {
            size_t hash = a.hashOf();
            hash = b.hashOf(hash);
            size_t h1 = c.myMegaHash();
            hash = h1.hashOf(hash); //Mix two hash values
            return hash;
        }
    }
}

bool _xopEquals(in void*, in void*)
{
    throw new Error("TypeInfo.equals is not implemented");
}

bool _xopCmp(in void*, in void*)
{
    throw new Error("TypeInfo.compare is not implemented");
}

/******************************************
 * Create RTInfo for type T
 */

template RTInfoImpl(size_t[] pointerBitmap)
{
    immutable size_t[pointerBitmap.length] RTInfoImpl = pointerBitmap[];
}

template RTInfo(T)
{
    enum pointerBitmap = __traits(getPointerBitmap, T);
    static if (pointerBitmap[1 .. $] == size_t[pointerBitmap.length - 1].init)
        enum RTInfo = rtinfoNoPointers;
    else
        enum RTInfo = RTInfoImpl!(pointerBitmap).ptr;
}

/**
* shortcuts for the precise GC, also generated by the compiler
* used instead of the actual pointer bitmap
*/
enum immutable(void)* rtinfoNoPointers  = null;
enum immutable(void)* rtinfoHasPointers = cast(void*)1;

// Helper functions

private inout(TypeInfo) getElement(return scope inout TypeInfo value) @trusted pure nothrow
{
    TypeInfo element = cast() value;
    for (;;)
    {
        if (auto qualified = cast(TypeInfo_Const) element)
            element = qualified.base;
        else if (auto redefined = cast(TypeInfo_Enum) element)
            element = redefined.base;
        else if (auto staticArray = cast(TypeInfo_StaticArray) element)
            element = staticArray.value;
        else if (auto vector = cast(TypeInfo_Vector) element)
            element = vector.base;
        else
            break;
    }
    return cast(inout) element;
}

private size_t getArrayHash(const scope TypeInfo element, const scope void* ptr, const size_t count) @trusted nothrow
{
    if (!count)
        return 0;

    const size_t elementSize = element.tsize;
    if (!elementSize)
        return 0;

    static bool hasCustomToHash(const scope TypeInfo value) @trusted pure nothrow
    {
        const element = getElement(value);

        if (const struct_ = cast(const TypeInfo_Struct) element)
            return !!struct_.xtoHash;

        return cast(const TypeInfo_Array) element
            || cast(const TypeInfo_AssociativeArray) element
            || cast(const ClassInfo) element
            || cast(const TypeInfo_Interface) element;
    }

    if (!hasCustomToHash(element))
        return hashOf(ptr[0 .. elementSize * count]);

    size_t hash = 0;
    foreach (size_t i; 0 .. count)
        hash = hashOf(element.getHash(ptr + i * elementSize), hash);
    return hash;
}

/// Provide the .dup array property.
@property auto dup(T)(T[] a)
    if (!is(const(T) : T))
{
    import core.internal.traits : Unconst;
    import core.internal.array.duplication : _dup;
    static assert(is(T : Unconst!T), "Cannot implicitly convert type "~T.stringof~
                  " to "~Unconst!T.stringof~" in dup.");

    return _dup!(T, Unconst!T)(a);
}

///
@safe unittest
{
    auto arr = [1, 2];
    auto arr2 = arr.dup;
    arr[0] = 0;
    assert(arr == [0, 2]);
    assert(arr2 == [1, 2]);
}

/// ditto
// const overload to support implicit conversion to immutable (unique result, see DIP29)
@property T[] dup(T)(const(T)[] a)
    if (is(const(T) : T))
{
    import core.internal.array.duplication : _dup;
    return _dup!(const(T), T)(a);
}


/// Provide the .idup array property.
@property immutable(T)[] idup(T)(T[] a)
{
    import core.internal.array.duplication : _dup;
    static assert(is(T : immutable(T)), "Cannot implicitly convert type "~T.stringof~
                  " to immutable in idup.");
    return _dup!(T, immutable(T))(a);
}

/// ditto
@property immutable(T)[] idup(T:void)(const(T)[] a)
{
    return a.dup;
}

///
@safe unittest
{
    char[] arr = ['a', 'b', 'c'];
    string s = arr.idup;
    arr[0] = '.';
    assert(s == "abc");
}

// HACK:  This is a lie.  `_d_arraysetcapacity` is neither `nothrow` nor `pure`, but this lie is
// necessary for now to prevent breaking code.
private extern (C) size_t _d_arraysetcapacity(const TypeInfo ti, size_t newcapacity, void[]* arrptr) pure nothrow;

/**
(Property) Gets the current _capacity of a slice. The _capacity is the size
that the slice can grow to before the underlying array must be
reallocated or extended.

If an append must reallocate a slice with no possibility of extension, then
`0` is returned. This happens when the slice references a static array, or
if another slice references elements past the end of the current slice.

Note: The _capacity of a slice may be impacted by operations on other slices.
*/
@property size_t capacity(T)(T[] arr) pure nothrow @trusted
{
    return _d_arraysetcapacity(typeid(T[]), 0, cast(void[]*)&arr);
}

///
@safe unittest
{
    //Static array slice: no capacity
    int[4] sarray = [1, 2, 3, 4];
    int[]  slice  = sarray[];
    assert(sarray.capacity == 0);
    //Appending to slice will reallocate to a new array
    slice ~= 5;
    assert(slice.capacity >= 5);

    //Dynamic array slices
    int[] a = [1, 2, 3, 4];
    int[] b = a[1 .. $];
    int[] c = a[1 .. $ - 1];
    debug(SENTINEL) {} else // non-zero capacity very much depends on the array and GC implementation
    {
        assert(a.capacity != 0);
        assert(a.capacity == b.capacity + 1); //both a and b share the same tail
    }
    assert(c.capacity == 0);              //an append to c must relocate c.
}

/**
Reserves capacity for a slice. The capacity is the size
that the slice can grow to before the underlying array must be
reallocated or extended.

Returns: The new capacity of the array (which may be larger than
the requested capacity).
*/
size_t reserve(T)(ref T[] arr, size_t newcapacity) pure nothrow @trusted
{
    if (__ctfe)
        return newcapacity;
    else
        return _d_arraysetcapacity(typeid(T[]), newcapacity, cast(void[]*)&arr);
}

///
@safe unittest
{
    //Static array slice: no capacity. Reserve relocates.
    int[4] sarray = [1, 2, 3, 4];
    int[]  slice  = sarray[];
    auto u = slice.reserve(8);
    assert(u >= 8);
    assert(&sarray[0] !is &slice[0]);
    assert(slice.capacity == u);

    //Dynamic array slices
    int[] a = [1, 2, 3, 4];
    a.reserve(8); //prepare a for appending 4 more items
    auto p = &a[0];
    u = a.capacity;
    a ~= [5, 6, 7, 8];
    assert(p == &a[0]);      //a should not have been reallocated
    assert(u == a.capacity); //a should not have been extended
}

// https://issues.dlang.org/show_bug.cgi?id=12330, reserve() at CTFE time
@safe unittest
{
    int[] foo() {
        int[] result;
        auto a = result.reserve = 5;
        assert(a == 5);
        return result;
    }
    enum r = foo();
}

// Issue 6646: should be possible to use array.reserve from SafeD.
@safe unittest
{
    int[] a;
    a.reserve(10);
}

// HACK:  This is a lie.  `_d_arrayshrinkfit` is not `nothrow`, but this lie is necessary
// for now to prevent breaking code.
private extern (C) void _d_arrayshrinkfit(const TypeInfo ti, void[] arr) nothrow;

/**
Assume that it is safe to append to this array. Appends made to this array
after calling this function may append in place, even if the array was a
slice of a larger array to begin with.

Use this only when it is certain there are no elements in use beyond the
array in the memory block.  If there are, those elements will be
overwritten by appending to this array.

Warning: Calling this function, and then using references to data located after the
given array results in undefined behavior.

Returns:
  The input is returned.
*/
auto ref inout(T[]) assumeSafeAppend(T)(auto ref inout(T[]) arr) nothrow @system
{
    _d_arrayshrinkfit(typeid(T[]), *(cast(void[]*)&arr));
    return arr;
}

///
@system unittest
{
    int[] a = [1, 2, 3, 4];

    // Without assumeSafeAppend. Appending relocates.
    int[] b = a [0 .. 3];
    b ~= 5;
    assert(a.ptr != b.ptr);

    debug(SENTINEL) {} else
    {
        // With assumeSafeAppend. Appending overwrites.
        int[] c = a [0 .. 3];
        c.assumeSafeAppend() ~= 5;
        assert(a.ptr == c.ptr);
    }
}

@system unittest
{
    int[] arr;
    auto newcap = arr.reserve(2000);
    assert(newcap >= 2000);
    assert(newcap == arr.capacity);
    auto ptr = arr.ptr;
    foreach (i; 0..2000)
        arr ~= i;
    assert(ptr == arr.ptr);
    arr = arr[0..1];
    arr.assumeSafeAppend();
    arr ~= 5;
    assert(ptr == arr.ptr);
}

@system unittest
{
    int[] arr = [1, 2, 3];
    void foo(ref int[] i)
    {
        i ~= 5;
    }
    arr = arr[0 .. 2];
    foo(assumeSafeAppend(arr)); //pass by ref
    assert(arr[]==[1, 2, 5]);
    arr = arr[0 .. 1].assumeSafeAppend(); //pass by value
}

// https://issues.dlang.org/show_bug.cgi?id=10574
@system unittest
{
    int[] a;
    immutable(int[]) b;
    auto a2 = &assumeSafeAppend(a);
    auto b2 = &assumeSafeAppend(b);
    auto a3 = assumeSafeAppend(a[]);
    auto b3 = assumeSafeAppend(b[]);
    assert(is(typeof(*a2) == int[]));
    assert(is(typeof(*b2) == immutable(int[])));
    assert(is(typeof(a3) == int[]));
    assert(is(typeof(b3) == immutable(int[])));
}

private void _doPostblit(T)(T[] arr)
{
    // infer static postblit type, run postblit if any
    static if (__traits(hasPostblit, T))
    {
        static if (__traits(isStaticArray, T) && is(T : E[], E))
            _doPostblit(cast(E[]) arr);
        else static if (!is(typeof(arr[0].__xpostblit())) && is(immutable T == immutable U, U))
            foreach (ref elem; (() @trusted => cast(U[]) arr)())
                elem.__xpostblit();
        else
            foreach (ref elem; arr)
                elem.__xpostblit();
    }
}

/**
Destroys the given object and optionally resets to initial state. It's used to
_destroy an object, calling its destructor or finalizer so it no longer
references any other objects. It does $(I not) initiate a GC cycle or free
any GC memory.
If `initialize` is supplied `false`, the object is considered invalid after
destruction, and should not be referenced.
*/
void destroy(bool initialize = true, T)(ref T obj) if (is(T == struct))
{
    import core.internal.destruction : destructRecurse;

    destructRecurse(obj);

    static if (initialize)
    {
        import core.internal.lifetime : emplaceInitializer;
        emplaceInitializer(obj); // emplace T.init
    }
}

@safe unittest
{
    struct A { string s = "A";  }
    A a = {s: "B"};
    assert(a.s == "B");
    a.destroy;
    assert(a.s == "A");
}

nothrow @safe @nogc unittest
{
    {
        struct A { string s = "A";  }
        A a;
        a.s = "asd";
        destroy!false(a);
        assert(a.s == "asd");
        destroy(a);
        assert(a.s == "A");
    }
    {
        static int destroyed = 0;
        struct C
        {
            string s = "C";
            ~this() nothrow @safe @nogc
            {
                destroyed ++;
            }
        }

        struct B
        {
            C c;
            string s = "B";
            ~this() nothrow @safe @nogc
            {
                destroyed ++;
            }
        }
        B a;
        a.s = "asd";
        a.c.s = "jkl";
        destroy!false(a);
        assert(destroyed == 2);
        assert(a.s == "asd");
        assert(a.c.s == "jkl" );
        destroy(a);
        assert(destroyed == 4);
        assert(a.s == "B");
        assert(a.c.s == "C" );
    }
}

private extern (C) void rt_finalize2(void* p, bool det = true, bool resetMemory = true) nothrow;

/// ditto
void destroy(bool initialize = true, T)(T obj) if (is(T == class))
{
    static if (__traits(getLinkage, T) == "C++")
    {
        static if (__traits(hasMember, T, "__xdtor"))
            obj.__xdtor();

        static if (initialize)
        {
            const initializer = __traits(initSymbol, T);
            (cast(void*)obj)[0 .. initializer.length] = initializer[];
        }
    }
    else
    {
        // Bypass overloaded opCast
        auto ptr = (() @trusted => *cast(void**) &obj)();
        rt_finalize2(ptr, true, initialize);
    }
}

/// ditto
void destroy(bool initialize = true, T)(T obj) if (is(T == interface))
{
    static assert(__traits(getLinkage, T) == "D", "Invalid call to destroy() on extern(" ~ __traits(getLinkage, T) ~ ") interface");

    destroy!initialize(cast(Object)obj);
}

/// Reference type demonstration
@system unittest
{
    class C
    {
        struct Agg
        {
            static int dtorCount;

            int x = 10;
            ~this() { dtorCount++; }
        }

        static int dtorCount;

        string s = "S";
        Agg a;
        ~this() { dtorCount++; }
    }

    C c = new C();
    assert(c.dtorCount == 0);   // destructor not yet called
    assert(c.s == "S");         // initial state `c.s` is `"S"`
    assert(c.a.dtorCount == 0); // destructor not yet called
    assert(c.a.x == 10);        // initial state `c.a.x` is `10`
    c.s = "T";
    c.a.x = 30;
    assert(c.s == "T");         // `c.s` is `"T"`
    destroy(c);
    assert(c.dtorCount == 1);   // `c`'s destructor was called
    assert(c.s == "S");         // `c.s` is back to its inital state, `"S"`
    assert(c.a.dtorCount == 1); // `c.a`'s destructor was called
    assert(c.a.x == 10);        // `c.a.x` is back to its inital state, `10`
}

/// C++ classes work too
@system unittest
{
    extern (C++) class CPP
    {
        struct Agg
        {
            __gshared int dtorCount;

            int x = 10;
            ~this() { dtorCount++; }
        }

        __gshared int dtorCount;

        string s = "S";
        Agg a;
        ~this() { dtorCount++; }
    }

    CPP cpp = new CPP();
    assert(cpp.dtorCount == 0);   // destructor not yet called
    assert(cpp.s == "S");         // initial state `cpp.s` is `"S"`
    assert(cpp.a.dtorCount == 0); // destructor not yet called
    assert(cpp.a.x == 10);        // initial state `cpp.a.x` is `10`
    cpp.s = "T";
    cpp.a.x = 30;
    assert(cpp.s == "T");         // `cpp.s` is `"T"`
    destroy!false(cpp);           // destroy without initialization
    assert(cpp.dtorCount == 1);   // `cpp`'s destructor was called
    assert(cpp.s == "T");         // `cpp.s` is not initialized
    assert(cpp.a.dtorCount == 1); // `cpp.a`'s destructor was called
    assert(cpp.a.x == 30);        // `cpp.a.x` is not initialized
    destroy(cpp);
    assert(cpp.dtorCount == 2);   // `cpp`'s destructor was called again
    assert(cpp.s == "S");         // `cpp.s` is back to its inital state, `"S"`
    assert(cpp.a.dtorCount == 2); // `cpp.a`'s destructor was called again
    assert(cpp.a.x == 10);        // `cpp.a.x` is back to its inital state, `10`
}

/// Value type demonstration
@safe unittest
{
    int i;
    assert(i == 0);           // `i`'s initial state is `0`
    i = 1;
    assert(i == 1);           // `i` changed to `1`
    destroy!false(i);
    assert(i == 1);           // `i` was not initialized
    destroy(i);
    assert(i == 0);           // `i` is back to its initial state `0`
}

/// Nested struct type
@system unittest
{
    int dtorCount;
    struct A
    {
        int i;
        ~this()
        {
            dtorCount++; // capture local variable
        }
    }
    A a = A(5);
    destroy!false(a);
    assert(dtorCount == 1);
    assert(a.i == 5);

    destroy(a);
    assert(dtorCount == 2);
    assert(a.i == 0);

    // the context pointer is now null
    // restore it so the dtor can run
    import core.lifetime : emplace;
    emplace(&a, A(0));
    // dtor also called here
}

@system unittest
{
    extern(C++)
    static class C
    {
        void* ptr;
        this() {}
    }

    destroy!false(new C());
    destroy!true(new C());
}

@system unittest
{
    // class with an `alias this`
    class A
    {
        static int dtorCount;
        ~this()
        {
            dtorCount++;
        }
    }

    class B
    {
        A a;
        alias a this;
        this()
        {
            a = new A;
        }
        static int dtorCount;
        ~this()
        {
            dtorCount++;
        }
    }
    auto b = new B;
    assert(A.dtorCount == 0);
    assert(B.dtorCount == 0);
    destroy(b);
    assert(A.dtorCount == 0);
    assert(B.dtorCount == 1);

    auto a = new A;
    destroy(a);
    assert(A.dtorCount == 1);
}

@system unittest
{
    interface I { }
    {
        class A: I { string s = "A"; this() {} }
        auto a = new A, b = new A;
        a.s = b.s = "asd";
        destroy(a);
        assert(a.s == "A");

        I i = b;
        destroy(i);
        assert(b.s == "A");
    }
    {
        static bool destroyed = false;
        class B: I
        {
            string s = "B";
            this() {}
            ~this()
            {
                destroyed = true;
            }
        }
        auto a = new B, b = new B;
        a.s = b.s = "asd";
        destroy(a);
        assert(destroyed);
        assert(a.s == "B");

        destroyed = false;
        I i = b;
        destroy(i);
        assert(destroyed);
        assert(b.s == "B");
    }
    // this test is invalid now that the default ctor is not run after clearing
    version (none)
    {
        class C
        {
            string s;
            this()
            {
                s = "C";
            }
        }
        auto a = new C;
        a.s = "asd";
        destroy(a);
        assert(a.s == "C");
    }
}

nothrow @safe @nogc unittest
{
    {
        struct A { string s = "A";  }
        A a;
        a.s = "asd";
        destroy!false(a);
        assert(a.s == "asd");
        destroy(a);
        assert(a.s == "A");
    }
    {
        static int destroyed = 0;
        struct C
        {
            string s = "C";
            ~this() nothrow @safe @nogc
            {
                destroyed ++;
            }
        }

        struct B
        {
            C c;
            string s = "B";
            ~this() nothrow @safe @nogc
            {
                destroyed ++;
            }
        }
        B a;
        a.s = "asd";
        a.c.s = "jkl";
        destroy!false(a);
        assert(destroyed == 2);
        assert(a.s == "asd");
        assert(a.c.s == "jkl" );
        destroy(a);
        assert(destroyed == 4);
        assert(a.s == "B");
        assert(a.c.s == "C" );
    }
}

nothrow unittest
{
    // Bugzilla 20049: Test to ensure proper behavior of `nothrow` destructors
    class C
    {
        static int dtorCount = 0;
        this() nothrow {}
        ~this() nothrow { dtorCount++; }
    }

    auto c = new C;
    destroy(c);
    assert(C.dtorCount == 1);
}

// https://issues.dlang.org/show_bug.cgi?id=22832
nothrow unittest
{
    static struct A {}
    static class B
    {
        A opCast(T : A)() { return A(); }
    }

    destroy(B.init);
}

// make sure destroy!false skips re-initialization
unittest
{
    static struct S { int x; }
    static class C { int x; }
    static extern(C++) class Cpp { int x; }

    static void test(T)(T inst)
    {
        inst.x = 123;
        destroy!false(inst);
        assert(inst.x == 123, T.stringof);
    }

    test(S());
    test(new C());
    test(new Cpp());
}

/// ditto
void destroy(bool initialize = true, T)(ref T obj)
if (__traits(isStaticArray, T))
{
    foreach_reverse (ref e; obj[])
        destroy!initialize(e);
}

@safe unittest
{
    int[2] a;
    a[0] = 1;
    a[1] = 2;
    destroy!false(a);
    assert(a == [ 1, 2 ]);
    destroy(a);
    assert(a == [ 0, 0 ]);
}

@safe unittest
{
    static struct vec2f {
        float[2] values;
        alias values this;
    }

    vec2f v;
    destroy!(true, vec2f)(v);
}

@system unittest
{
    // Bugzilla 15009
    static string op;
    static struct S
    {
        int x;
        this(int x) { op ~= "C" ~ cast(char)('0'+x); this.x = x; }
        this(this)  { op ~= "P" ~ cast(char)('0'+x); }
        ~this()     { op ~= "D" ~ cast(char)('0'+x); }
    }

    {
        S[2] a1 = [S(1), S(2)];
        op = "";
    }
    assert(op == "D2D1");   // built-in scope destruction
    {
        S[2] a1 = [S(1), S(2)];
        op = "";
        destroy(a1);
        assert(op == "D2D1");   // consistent with built-in behavior
    }

    {
        S[2][2] a2 = [[S(1), S(2)], [S(3), S(4)]];
        op = "";
    }
    assert(op == "D4D3D2D1");
    {
        S[2][2] a2 = [[S(1), S(2)], [S(3), S(4)]];
        op = "";
        destroy(a2);
        assert(op == "D4D3D2D1", op);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=19218
@system unittest
{
    static struct S
    {
        static dtorCount = 0;
        ~this() { ++dtorCount; }
    }

    static interface I
    {
        ref S[3] getArray();
        alias getArray this;
    }

    static class C : I
    {
        static dtorCount = 0;
        ~this() { ++dtorCount; }

        S[3] a;
        alias a this;

        ref S[3] getArray() { return a; }
    }

    C c = new C();
    destroy(c);
    assert(S.dtorCount == 3);
    assert(C.dtorCount == 1);

    I i = new C();
    destroy(i);
    assert(S.dtorCount == 6);
    assert(C.dtorCount == 2);
}

/// ditto
void destroy(bool initialize = true, T)(ref T obj)
    if (!is(T == struct) && !is(T == interface) && !is(T == class) && !__traits(isStaticArray, T))
{
    static if (initialize)
        obj = T.init;
}

@safe unittest
{
    {
        int a = 42;
        destroy!false(a);
        assert(a == 42);
        destroy(a);
        assert(a == 0);
    }
    {
        float a = 42;
        destroy!false(a);
        assert(a == 42);
        destroy(a);
        assert(a != a); // isnan
    }
}

@safe unittest
{
    // Bugzilla 14746
    static struct HasDtor
    {
        ~this() { assert(0); }
    }
    static struct Owner
    {
        HasDtor* ptr;
        alias ptr this;
    }

    Owner o;
    assert(o.ptr is null);
    destroy(o);     // must not reach in HasDtor.__dtor()
}

/* ************************************************************************
                           COMPILER SUPPORT
The compiler lowers certain expressions to instantiations of the following
templates.  They must be implicitly imported, which is why they are here
in this file. They must also be `public` as they must be visible from the
scope in which they are instantiated.  They are explicitly undocumented as
they are only intended to be instantiated by the compiler, not the user.
**************************************************************************/

public import core.internal.entrypoint : _d_cmain;

public import core.internal.array.appending : _d_arrayappendT;
version (D_ProfileGC)
{
    public import core.internal.array.appending : _d_arrayappendTTrace;
    public import core.internal.array.appending : _d_arrayappendcTXTrace;
    public import core.internal.array.concatenation : _d_arraycatnTXTrace;
    public import core.lifetime : _d_newitemTTrace;
    public import core.internal.array.construction : _d_newarrayTTrace;
    public import core.internal.array.construction : _d_newarraymTXTrace;
}
public import core.internal.array.appending : _d_arrayappendcTX;
public import core.internal.array.comparison : __cmp;
public import core.internal.array.equality : __equals;
public import core.internal.array.casting: __ArrayCast;
public import core.internal.array.concatenation : _d_arraycatnTX;
public import core.internal.array.construction : _d_arrayctor;
public import core.internal.array.construction : _d_arraysetctor;
public import core.internal.array.construction : _d_newarrayT;
public import core.internal.array.construction : _d_newarraymTX;
public import core.internal.array.arrayassign : _d_arrayassign_l;
public import core.internal.array.arrayassign : _d_arrayassign_r;
public import core.internal.array.arrayassign : _d_arraysetassign;
public import core.internal.array.capacity: _d_arraysetlengthTImpl;

public import core.internal.dassert: _d_assert_fail;

public import core.internal.destruction: __ArrayDtor;

public import core.internal.moving: __move_post_blt;

public import core.internal.postblit: __ArrayPostblit;

public import core.internal.switch_: __switch;
public import core.internal.switch_: __switch_error;

public import core.lifetime : _d_delstructImpl;
public import core.lifetime : _d_newThrowable;
public import core.lifetime : _d_newclassT;
public import core.lifetime : _d_newclassTTrace;
public import core.lifetime : _d_newitemT;

public @trusted @nogc nothrow pure extern (C) void _d_delThrowable(scope Throwable);

// Compare class and interface objects for ordering.
int __cmp(C1, C2)(C1 lhs, C2 rhs)
if ((is(C1 : const(Object)) || (is(C1 == interface) && (__traits(getLinkage, C1) == "D"))) &&
    (is(C2 : const(Object)) || (is(C2 == interface) && (__traits(getLinkage, C2) == "D"))))
{
    static if (is(C1 == typeof(null)) && is(C2 == typeof(null)))
    {
        return 0;
    }
    else static if (is(C1 == typeof(null)))
    {
        // Regard null references as always being "less than"
        return -1;
    }
    else static if (is(C2 == typeof(null)))
    {
        return 1;
    }
    else
    {
        if (lhs is rhs)
            return 0;
        if (lhs is null)
            return -1;
        if (rhs is null)
            return 1;
        return lhs.opCmp(rhs);
    }
}

// objects
@safe unittest
{
    class C
    {
        int i;
        this(int i) { this.i = i; }

        override int opCmp(Object c) const @safe
        {
            return i - (cast(C)c).i;
        }
    }

    auto c1 = new C(1);
    auto c2 = new C(2);
    assert(__cmp(c1, null) > 0);
    assert(__cmp(null, c1) < 0);
    assert(__cmp(c1, c1) == 0);
    assert(__cmp(c1, c2) < 0);
    assert(__cmp(c2, c1) > 0);

    assert(__cmp([c1, c1][], [c2, c2][]) < 0);
    assert(__cmp([c2, c2], [c1, c1]) > 0);
}

// structs
@safe unittest
{
    struct C
    {
        ubyte i;
        this(ubyte i) { this.i = i; }
    }

    auto c1 = C(1);
    auto c2 = C(2);

    assert(__cmp([c1, c1][], [c2, c2][]) < 0);
    assert(__cmp([c2, c2], [c1, c1]) > 0);
    assert(__cmp([c2, c2], [c2, c1]) > 0);
}

@safe unittest
{
    auto a = "hello"c;

    assert(a >  "hel");
    assert(a >= "hel");
    assert(a <  "helloo");
    assert(a <= "helloo");
    assert(a >  "betty");
    assert(a >= "betty");
    assert(a == "hello");
    assert(a <= "hello");
    assert(a >= "hello");
    assert(a <  "я");
}

// Used in Exception Handling LSDA tables to 'wrap' C++ type info
// so it can be distinguished from D TypeInfo
class __cpp_type_info_ptr
{
    void* ptr;          // opaque pointer to C++ RTTI type info
}

// Compiler hook into the runtime implementation of array (vector) operations.
template _arrayOp(Args...)
{
    import core.internal.array.operations;
    alias _arrayOp = arrayOp!Args;
}

public import core.builtins : __ctfeWrite;

/**

Provides an "inline import", i.e. an `import` that is only available for a
limited lookup. For example:

---
void fun(imported!"std.stdio".File input)
{
    ... use File from std.stdio normally ...
}
---

There is no need to import `std.stdio` at top level, so `fun` carries its own
dependencies. The same approach can be used for template constraints:

---
void fun(T)(imported!"std.stdio".File input, T value)
if (imported!"std.traits".isIntegral!T)
{
    ...
}
---

An inline import may be used in conjunction with the `with` statement as well.
Inside the scope controlled by `with`, all symbols in the imported module are
made available:

---
void fun()
{
    with (imported!"std.datetime")
    with (imported!"std.stdio")
    {
        Clock.currTime.writeln;
    }
}
---

The advantages of inline imports over top-level uses of the `import` declaration
are the following:

$(UL
$(LI The `imported` template specifies dependencies at declaration level, not at
module level. This allows reasoning about the dependency cost of declarations in
separation instead of aggregated at module level.)
$(LI Declarations using `imported` are easier to move around because they don't
require top-level context, making for simpler and quicker refactorings.)
$(LI Declarations using `imported` scale better with templates. This is because
templates that are not instantiated do not have their parameters and constraints
instantiated, so additional modules are not imported without necessity. This
makes the cost of unused templates negligible. Dependencies are pulled on a need
basis depending on the declarations used by client code.)
)

The use of `imported` also has drawbacks:

$(UL
$(LI If most declarations in a module need the same imports, then factoring them
at top level, outside the declarations, is simpler than repeating them.)
$(LI Traditional dependency-tracking tools such as make and other build systems
assume file-level dependencies and need special tooling (such as rdmd) in order
to work efficiently.)
$(LI Dependencies at the top of a module are easier to inspect quickly than
dependencies spread throughout the module.)
)

See_Also: The $(HTTP forum.dlang.org/post/tzqzmqhankrkbrfsrmbo@forum.dlang.org,
forum discussion) that led to the creation of the `imported` facility. Credit is
due to Daniel Nielsen and Dominikus Dittes Scherkl.

*/
template imported(string moduleName)
{
    mixin("import imported = " ~ moduleName ~ ";");
}
