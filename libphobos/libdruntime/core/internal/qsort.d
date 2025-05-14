/**
 * This is a public domain version of qsort.d.  All it does is call C's
 * qsort().
 *
 * Copyright: Copyright Digital Mars 2000 - 2010.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Martin Nowak
 */
module core.internal.qsort;

//debug=qsort;

debug (qsort) import core.stdc.stdio : printf;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

// qsort_r was added in glibc in 2.8. https://gcc.gnu.org/bugzilla/show_bug.cgi?id=88127
version (CRuntime_Glibc)
{
    version (GNU)
    {
        import gcc.config : Have_Qsort_R;
        enum Glibc_Qsort_R = Have_Qsort_R;
    }
    else
    {
        enum Glibc_Qsort_R = true;
    }
}
else
{
    enum Glibc_Qsort_R = false;
}

static if (Glibc_Qsort_R)
{
    alias extern (C) int function(scope const void *, scope const void *, scope void *) Cmp;
    extern (C) void qsort_r(scope void *base, size_t nmemb, size_t size, Cmp cmp, scope void *arg);

    extern (C) void[] _adSort(return scope void[] a, TypeInfo ti)
    {
        extern (C) int cmp(scope const void* p1, scope const void* p2, scope void* ti)
        {
            return (cast(TypeInfo)ti).compare(p1, p2);
        }
        qsort_r(a.ptr, a.length, ti.tsize, &cmp, cast(void*)ti);
        return a;
    }
}
else version (FreeBSD)
{
    import core.sys.freebsd.config : __FreeBSD_version;

    static if (__FreeBSD_version >= 1400000)
    {
        alias extern (C) int function(scope const void*, scope const void*, scope void*) Cmp;
        extern (C) void qsort_r(scope void* base, size_t nmemb, size_t size, Cmp cmp, scope void* thunk);

        // https://cgit.freebsd.org/src/tree/include/stdlib.h?h=stable/14#n350
        pragma(mangle, "qsort_r@FBSD_1.0")
        private extern (C) void __qsort_r_compat(scope void* base, size_t nmemb, size_t size, scope void* thunk, OldCmp cmp);
        alias extern (C) int function(scope void*, scope const void*, scope const void*) OldCmp;

        deprecated("In FreeBSD 14, qsort_r's signature was fixed to match POSIX. This extern(D) overload has been " ~
                   "provided to avoid breaking code, but code should be updated to use the POSIX version.")
        extern (D) void qsort_r(scope void* base, size_t nmemb, size_t size, scope void* thunk, OldCmp cmp)
        {
            __qsort_r_compat(base, nmemb, size, thunk, cmp);
        }

        extern (C) void[] _adSort(return scope void[] a, TypeInfo ti)
        {
            extern (C) int cmp(scope const void* p1, scope const void* p2, scope void* ti)
            {
                return (cast(TypeInfo)ti).compare(p1, p2);
            }
            qsort_r(a.ptr, a.length, ti.tsize, &cmp, cast(void*)ti);
            return a;
        }
    }
    else
    {
        alias extern (C) int function(scope void *, scope const void *, scope const void *) Cmp;
        extern (C) void qsort_r(scope void* base, size_t nmemb, size_t size, scope void* thunk, Cmp cmp);

        extern (C) void[] _adSort(return scope void[] a, TypeInfo ti)
        {
            extern (C) int cmp(scope void* ti, scope const void* p1, scope const void* p2)
            {
                return (cast(TypeInfo)ti).compare(p1, p2);
            }
            qsort_r(a.ptr, a.length, ti.tsize, cast(void*)ti, &cmp);
            return a;
        }
    }
}
else version (DragonFlyBSD)
{
    alias extern (C) int function(scope void *, scope const void *, scope const void *) Cmp;
    extern (C) void qsort_r(scope void *base, size_t nmemb, size_t size, scope void *thunk, Cmp cmp);

    extern (C) void[] _adSort(return scope void[] a, TypeInfo ti)
    {
        extern (C) int cmp(scope void* ti, scope const void* p1, scope const void* p2)
        {
            return (cast(TypeInfo)ti).compare(p1, p2);
        }
        qsort_r(a.ptr, a.length, ti.tsize, cast(void*)ti, &cmp);
        return a;
    }
}
else version (Darwin)
{
    alias extern (C) int function(scope void *, scope const void *, scope const void *) Cmp;
    extern (C) void qsort_r(scope void *base, size_t nmemb, size_t size, scope void *thunk, Cmp cmp);

    extern (C) void[] _adSort(return scope void[] a, TypeInfo ti)
    {
        extern (C) int cmp(scope void* ti, scope const void* p1, scope const void* p2)
        {
            return (cast(TypeInfo)ti).compare(p1, p2);
        }
        qsort_r(a.ptr, a.length, ti.tsize, cast(void*)ti, &cmp);
        return a;
    }
}
else version (CRuntime_UClibc)
{
    alias extern (C) int function(scope const void *, scope const void *, scope void *) __compar_d_fn_t;
    extern (C) void qsort_r(scope void *base, size_t nmemb, size_t size, __compar_d_fn_t cmp, scope void *arg);

    extern (C) void[] _adSort(return scope void[] a, TypeInfo ti)
    {
        extern (C) int cmp(scope const void* p1, scope const void* p2, scope void* ti)
        {
            return (cast(TypeInfo)ti).compare(p1, p2);
        }
        qsort_r(a.ptr, a.length, ti.tsize, &cmp, cast(void*)ti);
        return a;
    }
}
else
{
    import core.stdc.stdlib : qsort;

    private TypeInfo tiglobal;

    extern (C) void[] _adSort(return scope void[] a, TypeInfo ti)
    {
        extern (C) int cmp(scope const void* p1, scope const void* p2)
        {
            return tiglobal.compare(p1, p2);
        }
        tiglobal = ti;
        qsort(a.ptr, a.length, ti.tsize, &cmp);
        return a;
    }
}

unittest
{
    debug(qsort) printf("array.sort.unittest()\n");

    int[] a = new int[10];

    a[0] = 23;
    a[1] = 1;
    a[2] = 64;
    a[3] = 5;
    a[4] = 6;
    a[5] = 5;
    a[6] = 17;
    a[7] = 3;
    a[8] = 0;
    a[9] = -1;

    _adSort(*cast(void[]*)&a, typeid(a[0]));

    for (int i = 0; i < a.length - 1; i++)
    {
        debug(qsort) printf("i = %d", i);
        debug(qsort) printf(" %d %d\n", a[i], a[i + 1]);
        assert(a[i] <= a[i + 1]);
    }
}

unittest
{
    debug(qsort) printf("struct.sort.unittest()\n");

    static struct TestStruct
    {
        int value;

        int opCmp(const TestStruct rhs) const
        {
            return value <= rhs.value ?
                value < rhs.value ? -1 : 0 : 1;
        }
    }

    TestStruct[] a = new TestStruct[10];

    a[0] = TestStruct(23);
    a[1] = TestStruct(1);
    a[2] = TestStruct(64);
    a[3] = TestStruct(5);
    a[4] = TestStruct(6);
    a[5] = TestStruct(5);
    a[6] = TestStruct(17);
    a[7] = TestStruct(3);
    a[8] = TestStruct(0);
    a[9] = TestStruct(-1);

    _adSort(*cast(void[]*)&a, typeid(TestStruct));

    for (int i = 0; i < a.length - 1; i++)
    {
        //printf("i = %d", i);
        //printf(" %d %d\n", a[i], a[i + 1]);
        assert(a[i] <= a[i + 1]);
    }
}
