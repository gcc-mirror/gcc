// Exception handling and frame unwind runtime interface routines.
// Copyright (C) 2011-2020 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// extern(C) interface for the GNU/GCC pointer encoding library.
// This corresponds to unwind-pe.h

module gcc.unwind.pe;

import gcc.unwind;
import gcc.builtins;

@nogc:

// Pointer encodings, from dwarf2.h.
enum
{
    DW_EH_PE_absptr   = 0x00,
    DW_EH_PE_omit     = 0xff,

    DW_EH_PE_uleb128  = 0x01,
    DW_EH_PE_udata2   = 0x02,
    DW_EH_PE_udata4   = 0x03,
    DW_EH_PE_udata8   = 0x04,
    DW_EH_PE_sleb128  = 0x09,
    DW_EH_PE_sdata2   = 0x0A,
    DW_EH_PE_sdata4   = 0x0B,
    DW_EH_PE_sdata8   = 0x0C,
    DW_EH_PE_signed   = 0x08,

    DW_EH_PE_pcrel    = 0x10,
    DW_EH_PE_textrel  = 0x20,
    DW_EH_PE_datarel  = 0x30,
    DW_EH_PE_funcrel  = 0x40,
    DW_EH_PE_aligned  = 0x50,

    DW_EH_PE_indirect = 0x80
}

// Given an encoding, return the number of bytes the format occupies.
// This is only defined for fixed-size encodings, and so does not
// include leb128.
uint size_of_encoded_value(ubyte encoding)
{
    if (encoding == DW_EH_PE_omit)
        return 0;

    final switch (encoding & 0x07)
    {
        case DW_EH_PE_absptr:
            return (void*).sizeof;
        case DW_EH_PE_udata2:
            return 2;
        case DW_EH_PE_udata4:
            return 4;
        case DW_EH_PE_udata8:
            return 8;
    }
    assert(0);
}

// Given an encoding and an _Unwind_Context, return the base to which
// the encoding is relative.  This base may then be passed to
// read_encoded_value_with_base for use when the _Unwind_Context is
// not available.
_Unwind_Ptr base_of_encoded_value(ubyte encoding, _Unwind_Context* context)
{
    if (encoding == DW_EH_PE_omit)
        return cast(_Unwind_Ptr) 0;

    final switch (encoding & 0x70)
    {
        case DW_EH_PE_absptr:
        case DW_EH_PE_pcrel:
        case DW_EH_PE_aligned:
            return cast(_Unwind_Ptr) 0;

        case DW_EH_PE_textrel:
            return _Unwind_GetTextRelBase(context);
        case DW_EH_PE_datarel:
            return _Unwind_GetDataRelBase(context);
        case DW_EH_PE_funcrel:
            return _Unwind_GetRegionStart(context);
    }
    assert(0);
}

// Read an unsigned leb128 value from P, *P is incremented past the value.
// We assume that a word is large enough to hold any value so encoded;
// if it is smaller than a pointer on some target, pointers should not be
// leb128 encoded on that target.
_uleb128_t read_uleb128(const(ubyte)** p)
{
    auto q = *p;
    _uleb128_t result = 0;
    uint shift = 0;

    while (1)
    {
        ubyte b = *q++;
        result |= cast(_uleb128_t)(b & 0x7F) << shift;
        if ((b & 0x80) == 0)
            break;
        shift += 7;
    }

    *p = q;
    return result;
}

// Similar, but read a signed leb128 value.
_sleb128_t read_sleb128(const(ubyte)** p)
{
    auto q = *p;
    _sleb128_t result = 0;
    uint shift = 0;
    ubyte b = void;

    while (1)
    {
        b = *q++;
        result |= cast(_sleb128_t)(b & 0x7F) << shift;
        shift += 7;
        if ((b & 0x80) == 0)
            break;
    }

    // Sign-extend a negative value.
    if (shift < result.sizeof * 8 && (b & 0x40))
        result |= -(cast(_sleb128_t)1 << shift);

    *p = q;
    return result;
}

// Load an encoded value from memory at P.  The value is returned in VAL;
// The function returns P incremented past the value.  BASE is as given
// by base_of_encoded_value for this encoding in the appropriate context.
_Unwind_Ptr read_encoded_value_with_base(ubyte encoding, _Unwind_Ptr base,
                                         const(ubyte)** p)
{
    auto q = *p;
    _Unwind_Internal_Ptr result;

    if (encoding == DW_EH_PE_aligned)
    {
        _Unwind_Internal_Ptr a = cast(_Unwind_Internal_Ptr)q;
        a = cast(_Unwind_Internal_Ptr)((a + (void*).sizeof - 1) & - (void*).sizeof);
        result = *cast(_Unwind_Internal_Ptr*)a;
        q = cast(ubyte*) cast(_Unwind_Internal_Ptr)(a + (void*).sizeof);
    }
    else
    {
        switch (encoding & 0x0f)
        {
            case DW_EH_PE_uleb128:
                result = cast(_Unwind_Internal_Ptr)read_uleb128(&q);
                break;

            case DW_EH_PE_sleb128:
                result = cast(_Unwind_Internal_Ptr)read_sleb128(&q);
                break;

            case DW_EH_PE_udata2:
                result = cast(_Unwind_Internal_Ptr) *cast(ushort*)q;
                q += 2;
                break;
            case DW_EH_PE_udata4:
                result = cast(_Unwind_Internal_Ptr) *cast(uint*)q;
                q += 4;
                break;
            case DW_EH_PE_udata8:
                result = cast(_Unwind_Internal_Ptr) *cast(ulong*)q;
                q += 8;
                break;

            case DW_EH_PE_sdata2:
                result = cast(_Unwind_Internal_Ptr) *cast(short*)q;
                q += 2;
                break;
            case DW_EH_PE_sdata4:
                result = cast(_Unwind_Internal_Ptr) *cast(int*)q;
                q += 4;
                break;
            case DW_EH_PE_sdata8:
                result = cast(_Unwind_Internal_Ptr) *cast(long*)q;
                q += 8;
                break;

            case DW_EH_PE_absptr:
                if (size_t.sizeof == 8)
                    goto case DW_EH_PE_udata8;
                else
                    goto case DW_EH_PE_udata4;

            default:
                __builtin_abort();
        }

        if (result != 0)
        {
            result += ((encoding & 0x70) == DW_EH_PE_pcrel
                       ? cast(_Unwind_Internal_Ptr)*p : base);
            if (encoding & DW_EH_PE_indirect)
                result = *cast(_Unwind_Internal_Ptr*)result;
        }
    }

    *p = q;
    return result;
}

// Like read_encoded_value_with_base, but get the base from the context
// rather than providing it directly.
_Unwind_Ptr read_encoded_value(_Unwind_Context* context, ubyte encoding,
                               const(ubyte)** p)
{
    auto base = base_of_encoded_value(encoding, context);
    return read_encoded_value_with_base(encoding, base, p);
}
