/**
Array utilities.

Copyright: Denis Shelomovskij 2013
License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Denis Shelomovskij
Source: $(DRUNTIMESRC src/rt/util/_array.d)
*/
module rt.util.array;


import core.internal.string;
import core.stdc.stdint;


@safe /* pure dmd @@@BUG11461@@@ */ nothrow:

void enforceTypedArraysConformable(T)(const char[] action,
    const T[] a1, const T[] a2, in bool allowOverlap = false)
{
    _enforceSameLength(action, a1.length, a2.length);
    if (!allowOverlap)
        _enforceNoOverlap(action, arrayToPtr(a1), arrayToPtr(a2), T.sizeof * a1.length);
}

void enforceRawArraysConformable(const char[] action, in size_t elementSize,
    const void[] a1, const void[] a2, in bool allowOverlap = false)
{
    _enforceSameLength(action, a1.length, a2.length);
    if (!allowOverlap)
        _enforceNoOverlap(action, arrayToPtr(a1), arrayToPtr(a2), elementSize * a1.length);
}

private void _enforceSameLength(const char[] action,
    in size_t length1, in size_t length2)
{
    if (length1 == length2)
        return;

    UnsignedStringBuf tmpBuff = void;
    string msg = "Array lengths don't match for ";
    msg ~= action;
    msg ~= ": ";
    msg ~= length1.unsignedToTempString(tmpBuff, 10);
    msg ~= " != ";
    msg ~= length2.unsignedToTempString(tmpBuff, 10);
    throw new Error(msg);
}

private void _enforceNoOverlap(const char[] action,
    uintptr_t ptr1, uintptr_t ptr2, in size_t bytes)
{
    const d = ptr1 > ptr2 ? ptr1 - ptr2 : ptr2 - ptr1;
    if (d >= bytes)
        return;
    const overlappedBytes = bytes - d;

    UnsignedStringBuf tmpBuff = void;
    string msg = "Overlapping arrays in ";
    msg ~= action;
    msg ~= ": ";
    msg ~= overlappedBytes.unsignedToTempString(tmpBuff, 10);
    msg ~= " byte(s) overlap of ";
    msg ~= bytes.unsignedToTempString(tmpBuff, 10);
    throw new Error(msg);
}

private uintptr_t arrayToPtr(const void[] array) @trusted
{
    // Ok because the user will never dereference the pointer
    return cast(uintptr_t)array.ptr;
}
