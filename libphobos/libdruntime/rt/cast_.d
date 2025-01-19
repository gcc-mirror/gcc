/**
 * Implementation of array assignment support routines.
 *
 * Copyright: Copyright Digital Mars 2004 - 2010.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly
 * Source: $(DRUNTIMESRC rt/_cast_.d)
 */

/*          Copyright Digital Mars 2004 - 2010.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module rt.cast_;

debug(cast_) import core.stdc.stdio : printf;

extern (C):
@nogc:
nothrow:
pure:

// Needed because ClassInfo.opEquals(Object) does a dynamic cast,
// but we are trying to implement dynamic cast.
extern (D) private bool areClassInfosEqual(scope const ClassInfo a, scope const ClassInfo b) @safe
{
    // same class if signatures match, works with potential duplicates across binaries
    if (a is b)
        return true;

    // new fast way
    if (a.m_flags & TypeInfo_Class.ClassFlags.hasNameSig)
        return a.nameSig[0] == b.nameSig[0]
            && a.nameSig[1] == b.nameSig[1]
            && a.nameSig[2] == b.nameSig[2]
            && a.nameSig[3] == b.nameSig[3];

    // old slow way for temporary binary compatibility
    return a.name == b.name;
}

/******************************************
 * Given a pointer:
 *      If it is an Object, return that Object.
 *      If it is an interface, return the Object implementing the interface.
 *      If it is null, return null.
 *      Else, undefined crash
 */
Object _d_toObject(return scope void* p)
{
    if (!p)
        return null;

    Object o = cast(Object) p;
    ClassInfo oc = typeid(o);
    Interface* pi = **cast(Interface***) p;

    /* Interface.offset lines up with ClassInfo.name.ptr,
     * so we rely on pointers never being less than 64K,
     * and Objects never being greater.
     */
    if (pi.offset < 0x10000)
    {
        debug(cast_) printf("\tpi.offset = %zd\n", pi.offset);
        return cast(Object)(p - pi.offset);
    }
    return o;
}

/*************************************
 * Attempts to cast interface Object o to class c.
 * Returns o if successful, null if not.
 */
void* _d_interface_cast(void* p, ClassInfo c)
{
    debug(cast_) printf("_d_interface_cast(p = %p, c = '%.*s')\n", p, cast(int) c.name.length, c.name.ptr);
    if (!p)
        return null;

    Interface* pi = **cast(Interface***) p;

    debug(cast_) printf("\tpi.offset = %zd\n", pi.offset);
    Object o2 = cast(Object)(p - pi.offset);
    void* res = null;
    size_t offset = 0;
    if (o2 && _d_isbaseof2(typeid(o2), c, offset))
    {
        debug(cast_) printf("\toffset = %zd\n", offset);
        res = cast(void*) o2 + offset;
    }
    debug(cast_) printf("\tresult = %p\n", res);
    return res;
}

/*****
 * Dynamic cast from a class object `o` to class or interface `c`, where `c` is a subtype of `o`.
 * Params:
 *      o = instance of class
 *      c = a subclass of o
 * Returns:
 *      null if o is null or c is not a subclass of o. Otherwise, return o.
 */
void* _d_dynamic_cast(Object o, ClassInfo c)
{
    debug(cast_) printf("_d_dynamic_cast(o = %p, c = '%.*s')\n", o, cast(int) c.name.length, c.name.ptr);

    void* res = null;
    size_t offset = 0;
    if (o && _d_isbaseof2(typeid(o), c, offset))
    {
        debug(cast_) printf("\toffset = %zd\n", offset);
        res = cast(void*) o + offset;
    }
    debug(cast_) printf("\tresult = %p\n", res);
    return res;
}

/*****
 * Dynamic cast from a class object o to class c, where c is a subclass of o.
 * Params:
 *      o = instance of class
 *      c = a subclass of o
 * Returns:
 *      null if o is null or c is not a subclass of o. Otherwise, return o.
 */
void* _d_class_cast(Object o, ClassInfo c)
{
    debug(cast_) printf("_d_cast_cast(o = %p, c = '%.*s')\n", o, cast(int) c.name.length, c.name.ptr);

    if (!o)
        return null;

    ClassInfo oc = typeid(o);
    int delta = oc.depth;

    if (delta && c.depth)
    {
        delta -= c.depth;
        if (delta < 0)
            return null;

        while (delta--)
            oc = oc.base;
        if (areClassInfosEqual(oc, c))
            return cast(void*)o;
        return null;
    }

    // no depth data - support the old way
    do
    {
        if (areClassInfosEqual(oc, c))
            return cast(void*)o;
        oc = oc.base;
    } while (oc);
    return null;
}

/**
 * Dynamic cast `o` to final class `c` only one level down
 * Params:
 *      o = object that is instance of a class
 *      c = class to cast it to
 * Returns:
 *      o if it succeeds, null if it fails
 */
void* _d_paint_cast(Object o, ClassInfo c)
{
    /* If o is really an instance of c, just do a paint
     */
    auto p = o && cast(void*)(areClassInfosEqual(typeid(o), c)) ? o : null;
    debug assert(cast(void*)p is cast(void*)_d_dynamic_cast(o, c));
    return cast(void*)p;
}

int _d_isbaseof2(scope ClassInfo oc, scope const ClassInfo c, scope ref size_t offset) @safe
{
    if (areClassInfosEqual(oc, c))
        return true;

    do
    {
        if (oc.base && areClassInfosEqual(oc.base, c))
            return true;

        // Bugzilla 2013: Use depth-first search to calculate offset
        // from the derived (oc) to the base (c).
        foreach (iface; oc.interfaces)
        {
            if (areClassInfosEqual(iface.classinfo, c) || _d_isbaseof2(iface.classinfo, c, offset))
            {
                offset += iface.offset;
                return true;
            }
        }

        oc = oc.base;
    } while (oc);

    return false;
}

int _d_isbaseof(scope ClassInfo oc, scope const ClassInfo c) @safe
{
    size_t offset = 0;
    return _d_isbaseof2(oc, c, offset);
}
