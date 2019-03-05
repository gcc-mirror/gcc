// Written in the D programming language.

/**
 * Support UTF-8 on Windows 95, 98 and ME systems.
 *
 * Copyright: Copyright Digital Mars 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright)
 */
/*          Copyright Digital Mars 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.windows.charset;

version (StdDdoc)
{
    /******************************************
     * Converts the UTF-8 string s into a null-terminated string in a Windows
     * 8-bit character set.
     *
     * Params:
     * s = UTF-8 string to convert.
     * codePage = is the number of the target codepage, or
     *   0 - ANSI,
     *   1 - OEM,
     *   2 - Mac
     *
     * Authors:
     *      yaneurao, Walter Bright, Stewart Gordon
     */
    const(char)* toMBSz(in char[] s, uint codePage = 0);

    /**********************************************
     * Converts the null-terminated string s from a Windows 8-bit character set
     * into a UTF-8 char array.
     *
     * Params:
     * s = UTF-8 string to convert.
     * codePage = is the number of the source codepage, or
     *   0 - ANSI,
     *   1 - OEM,
     *   2 - Mac
     * Authors: Stewart Gordon, Walter Bright
     */
    string fromMBSz(immutable(char)* s, int codePage = 0);
}
else:

version (Windows):

import core.sys.windows.windows;
import std.conv;
import std.string;
import std.windows.syserror;

import std.internal.cstring;

const(char)* toMBSz(in char[] s, uint codePage = 0)
{
    // Only need to do this if any chars have the high bit set
    foreach (char c; s)
    {
        if (c >= 0x80)
        {
            char[] result;
            int readLen;
            auto wsTmp = s.tempCStringW();
            result.length = WideCharToMultiByte(codePage, 0, wsTmp, -1, null, 0,
                    null, null);

            if (result.length)
            {
                readLen = WideCharToMultiByte(codePage, 0, wsTmp, -1, result.ptr,
                        to!int(result.length), null, null);
            }

            if (!readLen || readLen != result.length)
            {
                throw new Exception("Couldn't convert string: " ~
                        sysErrorString(GetLastError()));
            }

            return result.ptr;
        }
    }
    return std.string.toStringz(s);
}

string fromMBSz(immutable(char)* s, int codePage = 0)
{
    const(char)* c;

    for (c = s; *c != 0; c++)
    {
        if (*c >= 0x80)
        {
            wchar[] result;
            int readLen;

            result.length = MultiByteToWideChar(codePage, 0, s, -1, null, 0);

            if (result.length)
            {
                readLen = MultiByteToWideChar(codePage, 0, s, -1, result.ptr,
                        to!int(result.length));
            }

            if (!readLen || readLen != result.length)
            {
                throw new Exception("Couldn't convert string: " ~
                    sysErrorString(GetLastError()));
            }

            return result[0 .. result.length-1].to!string; // omit trailing null
        }
    }
    return s[0 .. c-s];         // string is ASCII, no conversion necessary
}


