// Written in the D programming language.

/**
 * Convert Win32 error code to string.
 *
 * Copyright: Copyright The D Language Foundation" 2006 - 2013.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright)
 * Credits:   Based on code written by Regan Heath
 */
/*          Copyright The D Language Foundation" 2006 - 2013.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.windows.syserror;
import std.traits : isSomeString;

version (StdDdoc)
{
    private
    {
        alias DWORD = uint;
        enum LANG_NEUTRAL = 0, SUBLANG_DEFAULT = 1;
    }

    /** Query the text for a Windows error code, as returned by
        $(LINK2 http://msdn.microsoft.com/en-us/library/windows/desktop/ms679360.aspx,
        `GetLastError`), as a D string.
     */
    string sysErrorString(
        DWORD errCode,
        // MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT) is the user's default language
        int langId = LANG_NEUTRAL,
        int subLangId = SUBLANG_DEFAULT) @trusted;

    /*********************
       Thrown if errors that set
       $(LINK2 http://msdn.microsoft.com/en-us/library/windows/desktop/ms679360.aspx,
       `GetLastError`) occur.
     */
    class WindowsException : Exception
    {
        private alias DWORD = int;
        final @property DWORD code(); /// `GetLastError`'s return value.
        this(DWORD code, string str=null, string file = null, size_t line = 0) nothrow @trusted;
    }

    /++
        If `!!value` is true, `value` is returned. Otherwise,
        $(D new WindowsException(GetLastError(), msg)) is thrown.
        `WindowsException` assumes that the last operation set
        `GetLastError()` appropriately.

        Example:
        --------------------
        wenforce(DeleteFileA("junk.tmp"), "DeleteFile failed");
        --------------------
     +/
    T wenforce(T, S)(T value, lazy S msg = null,
        string file = __FILE__, size_t line = __LINE__) @safe
    if (isSomeString!S);
}
else:

version (Windows):

import core.sys.windows.winbase, core.sys.windows.winnt;
import std.array : appender, Appender;
import std.conv : to, toTextRange, text;
import std.exception;
import std.windows.charset;

string sysErrorString(
    DWORD errCode,
    // MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT) is the user's default language
    int langId = LANG_NEUTRAL,
    int subLangId = SUBLANG_DEFAULT) @trusted
{
    auto buf = appender!string();

    wenforce(
        // Ignore unlikely UTF decoding errors, always report the actual error (`errCode`)
        putSysError(errCode, buf, MAKELANGID(langId, subLangId)).ifThrown(false),
        text("Could not fetch error string for WinAPI code ", errCode)
    );

    return buf.data;
}

@safe unittest
{
    import std.algorithm.searching;

    assert(sysErrorString(ERROR_PATH_NOT_FOUND) !is null);

    const msg = collectExceptionMsg!WindowsException(sysErrorString(DWORD.max));
    assert(msg.startsWith(`Could not fetch error string for WinAPI code 4294967295: `));
}

bool putSysError(Writer)(DWORD code, Writer w, /*WORD*/int langId = 0)
{
    wchar *lpMsgBuf = null;
    auto res = FormatMessageW(
        FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        null,
        code,
        langId,
        cast(LPWSTR)&lpMsgBuf,
        0,
        null);
    scope(exit) if (lpMsgBuf) LocalFree(lpMsgBuf);

    if (lpMsgBuf)
    {
        import std.string : strip;
        w.put(lpMsgBuf[0 .. res].strip());
        return true;
    }
    else
        return false;
}

class WindowsException : Exception
{
    import core.sys.windows.windef : DWORD;

    final @property DWORD code() { return _code; } /// `GetLastError`'s return value.
    private DWORD _code;

    this(DWORD code, string str=null, string file = null, size_t line = 0) nothrow @trusted
    {
        _code = code;

        auto buf = appender!(char[]);

        if (str != null)
        {
            buf.put(str);
            if (code)
                buf.put(": ");
        }

        if (code && writeErrorMessage(code, buf))
        {
            buf.put(" (error ");
            toTextRange(code, buf);
            buf.put(')');
        }

        super(cast(immutable) buf.data, file, line);
    }
}

/// Writes the error string associated to `code` into `buf`.
/// Writes `Error <code>` when the error message lookup fails
private bool writeErrorMessage(DWORD code, ref Appender!(char[]) buf) nothrow
{
    bool success;
    try
    {
        // Reset the buffer to undo partial changes
        const len = buf[].length;
        scope (failure) buf.shrinkTo(len);

        success = putSysError(code, buf);
    }
    catch (Exception) {}

    // Write the error code instead if we couldn't find the string
    if (!success)
    {
        buf.put("Error ");
        toTextRange(code, buf);
    }

    return success;
}

T wenforce(T, S = string)(T value, lazy S msg = null,
string file = __FILE__, size_t line = __LINE__)
if (isSomeString!S)
{
    if (!value)
        throw new WindowsException(GetLastError(), to!string(msg), file, line);
    return value;
}

T wenforce(T)(T condition, const(char)[] name, const(wchar)* namez, string file = __FILE__, size_t line = __LINE__)
{
    if (condition)
        return condition;
    string names;
    if (!name)
    {
        static string trustedToString(const(wchar)* stringz) @trusted
        {
            import core.stdc.wchar_ : wcslen;
            import std.conv : to;
            auto len = wcslen(stringz);
            return to!string(stringz[0 .. len]);
        }

        names = trustedToString(namez);
    }
    else
        names = to!string(name);
    throw new WindowsException(GetLastError(), names, file, line);
}

@system unittest
{
    import std.algorithm.searching : startsWith, endsWith;
    import std.string;

    auto e = collectException!WindowsException(
        DeleteFileA("unexisting.txt").wenforce("DeleteFile")
    );
    assert(e.code == ERROR_FILE_NOT_FOUND);
    assert(e.msg.startsWith("DeleteFile: "));
    // can't test the entire message, as it depends on Windows locale
    assert(e.msg.endsWith(" (error 2)"));

    // Test code zero
    e = new WindowsException(0);
    assert(e.msg == "");

    e = new WindowsException(0, "Test");
    assert(e.msg == "Test");
}

@safe nothrow unittest
{
    import std.algorithm.searching : endsWith;

    auto e = new WindowsException(ERROR_FILE_NOT_FOUND);
    assert(e.msg.endsWith("(error 2)"));

    e = new WindowsException(DWORD.max);
    assert(e.msg == "Error 4294967295");
}

/// Tries to translate an error code from the Windows API to the corresponding
/// error message. Returns `Error <code>` on failure
package (std) string generateSysErrorMsg(DWORD errCode = GetLastError()) nothrow @trusted
{
    auto buf = appender!(char[]);
    cast(void) writeErrorMessage(errCode, buf);
    return cast(immutable) buf[];
}

nothrow @safe unittest
{
    assert(generateSysErrorMsg(ERROR_PATH_NOT_FOUND) !is null);
    assert(generateSysErrorMsg(DWORD.max) == "Error 4294967295");
}
