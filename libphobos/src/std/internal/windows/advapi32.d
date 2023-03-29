// Written in the D programming language.

/**
 * The only purpose of this module is to do the static construction for
 * std.windows.registry, to eliminate cyclic construction errors.
 *
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Kenji Hara
 * Source:    $(PHOBOSSRC std/internal/windows/advapi32.d)
 */
module std.internal.windows.advapi32;

version (Windows):

import core.sys.windows.winbase, core.sys.windows.winnt, core.sys.windows.winreg;

pragma(lib, "advapi32.lib");

@property bool isWow64()
{
    // WOW64 is the x86 emulator that allows 32-bit Windows-based applications to run seamlessly on 64-bit Windows
    // IsWow64Process Function - Minimum supported client - Windows Vista, Windows XP with SP2
    static int result = -1; // <0 if uninitialized, >0 if yes, ==0 if no
    if (result >= 0)
        return result > 0;  // short path
    // Will do this work once per thread to avoid importing std.concurrency
    // or doing gnarly initonce work.
    alias fptr_t = extern(Windows) BOOL function(HANDLE, PBOOL);
    auto hKernel = GetModuleHandleA("kernel32");
    auto IsWow64Process = cast(fptr_t) GetProcAddress(hKernel, "IsWow64Process");
    BOOL bIsWow64;
    result = IsWow64Process && IsWow64Process(GetCurrentProcess(), &bIsWow64) && bIsWow64;
    return result > 0;
}

HMODULE hAdvapi32 = null;
extern (Windows)
{
    LONG function(
        scope const HKEY hkey, scope const LPCWSTR lpSubKey,
        scope const REGSAM samDesired, scope const DWORD reserved) pRegDeleteKeyExW;
}

void loadAdvapi32()
{
    if (!hAdvapi32)
    {
        hAdvapi32 = LoadLibraryA("Advapi32.dll");
        if (!hAdvapi32)
            throw new Exception(`LoadLibraryA("Advapi32.dll")`);

        pRegDeleteKeyExW = cast(typeof(pRegDeleteKeyExW)) GetProcAddress(hAdvapi32 , "RegDeleteKeyExW");
        if (!pRegDeleteKeyExW)
            throw new Exception(`GetProcAddress(hAdvapi32 , "RegDeleteKeyExW")`);
    }
}

// It will free Advapi32.dll, which may be loaded for RegDeleteKeyEx function
private void freeAdvapi32()
{
    if (hAdvapi32)
    {
        if (!FreeLibrary(hAdvapi32))
            throw new Exception(`FreeLibrary("Advapi32.dll")`);
        hAdvapi32 = null;

        pRegDeleteKeyExW = null;
    }
}

static ~this()
{
    freeAdvapi32();
}
