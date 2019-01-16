/**
 * Implementation of array assignment support routines.
 *
 * Copyright: Copyright Digital Mars 2004 - 2010.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly
 */

/*          Copyright Digital Mars 2004 - 2010.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module rt.cast_;

extern (C):

/******************************************
 * Given a pointer:
 *      If it is an Object, return that Object.
 *      If it is an interface, return the Object implementing the interface.
 *      If it is null, return null.
 *      Else, undefined crash
 */
Object _d_toObject(void* p)
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
        debug(cast_) printf("\tpi.offset = %d\n", pi.offset);
        return cast(Object)(p - pi.offset);
    }
    return o;
}

/*************************************
 * Attempts to cast Object o to class c.
 * Returns o if successful, null if not.
 */
void* _d_interface_cast(void* p, ClassInfo c)
{
    debug(cast_) printf("_d_interface_cast(p = %p, c = '%.*s')\n", p, c.name);
    if (!p)
        return null;

    Interface* pi = **cast(Interface***) p;

    debug(cast_) printf("\tpi.offset = %d\n", pi.offset);
    return _d_dynamic_cast(cast(Object)(p - pi.offset), c);
}

void* _d_dynamic_cast(Object o, ClassInfo c)
{
    debug(cast_) printf("_d_dynamic_cast(o = %p, c = '%.*s')\n", o, c.name);

    void* res = null;
    size_t offset = 0;
    if (o && _d_isbaseof2(typeid(o), c, offset))
    {
        debug(cast_) printf("\toffset = %d\n", offset);
        res = cast(void*) o + offset;
    }
    debug(cast_) printf("\tresult = %p\n", res);
    return res;
}

int _d_isbaseof2(ClassInfo oc, ClassInfo c, ref size_t offset)
{
    if (oc is c)
        return true;

    do
    {
        if (oc.base is c)
            return true;

        // Bugzilla 2013: Use depth-first search to calculate offset
        // from the derived (oc) to the base (c).
        foreach (iface; oc.interfaces)
        {
            if (iface.classinfo is c || _d_isbaseof2(iface.classinfo, c, offset))
            {
                offset += iface.offset;
                return true;
            }
        }

        oc = oc.base;
    } while (oc);

    return false;
}

int _d_isbaseof(ClassInfo oc, ClassInfo c)
{
    if (oc is c)
        return true;

    do
    {
        if (oc.base is c)
            return true;

        foreach (iface; oc.interfaces)
        {
            if (iface.classinfo is c || _d_isbaseof(iface.classinfo, c))
                return true;
        }

        oc = oc.base;
    } while (oc);

    return false;
}

/*********************************
 * Find the vtbl[] associated with Interface ic.
 */
void* _d_interface_vtbl(ClassInfo ic, Object o)
{
    debug(cast_) printf("__d_interface_vtbl(o = %p, ic = %p)\n", o, ic);

    assert(o);

    foreach (iface; typeid(o).interfaces)
    {
        if (iface.classinfo is ic)
            return cast(void*) iface.vtbl;
    }
    assert(0);
}
